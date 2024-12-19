f_par_toelatingen_rap_jaar <-
  readRDS("data/gbm_toelating_werking.rds") %>% 
    mutate(toegelaten_rap_jaar = ifelse(year(expiratiedatum) < rap_jaar, "Niet toegelaten", "Toegelaten")) %>% 
    select(parnr, toegelaten_rap_jaar) %>% 
    maak_opzoeker()
    

mspaf_mp <-
  data_gbm %>%
  filter(jaar == rap_jaar) %>%
  mutate(paf_acuut = paf_gbm(f_aquopar(parnr),
                             concentratie = waarde,
                             detectiegrens = detectiegrens,
                             ssd_data = toxiciteit,
                             type_paf = "acuut"),
         paf_chronisch = paf_gbm(f_aquopar(parnr),
                                 concentratie = waarde,
                                 detectiegrens = detectiegrens,
                                 ssd_data = toxiciteit,
                                 type_paf = "chronisch")) %>% # View("paf_basis_info")
  # afhandelen detectiegrenswaarden
  mutate(paf_acuut = ifelse(is.na(detectiegrens), paf_acuut, 0),
         paf_chronisch = ifelse(is.na(detectiegrens), paf_chronisch, 0)) %>%
  # group_by(mp) %>%
  # mutate(aantal_monsters = n_distinct(datum)) %>%
  group_by(mp, parnr) %>%
  # alleen de hoogste waarde telt mee in de msPAF
  filter(waarde == max(waarde)) %>% 
  mutate(toelating = f_par_toelatingen_rap_jaar(parnr)) %>% 
  replace_na(list(toelating = "Niet toegelaten")) %>% 
  group_by(mp, toelating) %>%
  summarise(deel_mspaf_acuut = mspaf(paf_acuut),
            deel_mspaf_chronisch = mspaf(paf_chronisch)) %>% 
  group_by(mp) %>% 
  # correcte toedeling toxiciteit aan toegelaten vs niet toegelaten - complex doordat mspaf een product is en geen som.
  mutate(aandeel_acuut = deel_mspaf_acuut / sum(deel_mspaf_acuut),
         aandeel_chronisch = deel_mspaf_chronisch / sum(deel_mspaf_chronisch)) %>% 
  replace_na(list(aandeel_acuut = 0, aandeel_chronisch = 0)) %>% 
  mutate(# mspaf_acuut = mspaf(deel_mspaf_acuut), 
         # mspaf_chronisch = mspaf(deel_mspaf_chronisch),
         `Acute effecten` = mspaf(deel_mspaf_acuut) * aandeel_acuut, 
         `Chronische effecten` = mspaf(deel_mspaf_chronisch) * aandeel_chronisch) %>% 
  # summarise(`Acute effecten` = mspaf(mspaf_acuut),
  #           `Chronische effecten` = mspaf(mspaf_chronisch),
  #           
  #           ) %>% 
  ungroup() %>%
  
  mutate(landgebruik = ifelse(mp == "S_0609", "Glastuinbouw", str_to_sentence(f_landgebruik(mp)))) %>%
  mutate(mp2 = glue("{landgebruik} -- {mp}")) %>%
  mutate(mp2 = fct_reorder(mp2, `Acute effecten`)) %>%
  pivot_longer(cols = c(`Acute effecten`, `Chronische effecten`), names_to = "type", values_to = "mspaf" ) %>%
  mutate(landgebruik = fct_reorder(landgebruik, mspaf, .desc = TRUE))
 
# library(ggpattern)  
   
# plot_mspaf_mp <-
  mspaf_mp %>%
  # filter(aantal_monsters > 2) %>% # locaties met 2 monsters en 1 pakket eruit alleen in 2023!!!
  ggplot() +
  geom_col(aes(mspaf, mp2, fill = toelating), 
                   colour = "grey60", linewidth = 0.2) +
  geom_vline(xintercept = 0.005, colour = oranje, linetype = "dashed", linewidth = 0.8) + 
  scale_x_continuous(expand = expansion(c(0, 0.05)), labels = function(.x) scales::percent(.x, accuracy = 1)) +
  scale_fill_manual(values = c("Toegelaten" = blauw_m, "Niet toegelaten" = oranje_m)) +
  # scale_pattern_manual(values=c('stripe', 'none')) +
  # scale_pattern_type_manual(values=c('stripe', 'none')) +
  # # scale_pattern_type_manual(values = c("Toegelaten" = "none", "Niet toegelaten" = "crosshatch")) +
    # scale_pattern_type_manual(values = c(Cool = 'hexagons', But = 'crosshatch',
    #                                      Use = 'right45', Less = 'checkerboard'))
  facet_wrap(~type, scales = "free_x") +
  labs(title = "Hoe schadelijk zijn de gewasbeschermingsmiddelen?",
       subtitle = glue("per locatie in {rap_jaar}"),
       x = "Percentage aangetaste soorten",
       y = "Meetlocatie",
       fill = "") +
  thema_hor_bar +
  theme(legend.position = "bottom",
        plot.subtitle = element_text(face = "italic"),
        panel.grid.major.x = element_line(linewidth = 0.8),
        axis.title.y.left = element_text(margin = margin(r = 10)),
        panel.spacing = unit(2, "lines"),
        axis.text.y = element_text(hjust = 0))
  