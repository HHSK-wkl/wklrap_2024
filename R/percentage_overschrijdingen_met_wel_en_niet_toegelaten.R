toelatingen <- readRDS("data/gbm_toelating_werking.rds") %>% select(parnr, expiratiedatum)
  


# plot_overschr_freq <-
  toetsing %>% 
  filter(jaar >= rap_jaar - 10) %>% 
  mutate(landgebruik = str_to_sentence(f_landgebruik(mp))) %>%
  mutate(landgebruik = case_when(
    mp == "S_0504" ~ "Akkerbouw",
    mp == "S_0609" ~ "Glastuinbouw", # S_0609 is lastig toe te delen - lijkt deels op glas
    TRUE ~ landgebruik
  )) %>%
  left_join(toelatingen, by = "parnr") %>% 
  mutate(toelating = ifelse(year(expiratiedatum) < jaar, "Niet toegelaten", "Toegelaten")) %>% 
  replace_na(list(toelating = "Niet toegelaten")) %>%   
  group_by(jaar, landgebruik) %>%
  mutate(n_toetsingen = n()) %>% 
    
  group_by(jaar, landgebruik, toelating) %>%
  summarise(n_overschrijdingen = sum(normoverschrijding),
            fractie = n_overschrijdingen / first(n_toetsingen)) %>% 
  ungroup() %>% 
  mutate(landgebruik = fct_reorder(landgebruik, fractie, .desc = TRUE)) %>%
  filter(!is.na(landgebruik), landgebruik != "Stedelijk") %>% 
  ggplot(aes(jaar, fractie, fill = toelating)) +
  # geom_smooth(se = FALSE, method = "lm", linetype = "dashed", colour = blauw_l) +
  # geom_line(colour = blauw, linewidth = 1) +
  # geom_point(shape = 19, size = 2, colour = blauw) +
  geom_area(position = "stack") + 
  facet_wrap(~landgebruik, scales = "free", ncol = 2) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 0.035), labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(limits = c(rap_jaar - 10, rap_jaar), breaks = scales::breaks_pretty(6)) +
  scale_fill_manual(values = c(oranje_m, blauw_m)) +
  labs(title = "Hoe vaak wordt de norm overschreden?",
       subtitle = "per type landgebruik",
       x = "",
       y = "% normoverschrijdingen") +
  thema_line_facet +
  theme(panel.spacing = unit(25, "points"), 
        legend.position = "none")
  
  

# Origineel ---------------------------------------------------------------

  # plot_overschr_freq <-
    toetsing %>% 
    filter(jaar >= rap_jaar - 10) %>% 
    mutate(landgebruik = str_to_sentence(f_landgebruik(mp))) %>%
    mutate(landgebruik = case_when(
      mp == "S_0504" ~ "Akkerbouw",
      mp == "S_0609" ~ "Glastuinbouw", # S_0609 is lastig toe te delen - lijkt deels op glas
      TRUE ~ landgebruik
    )) %>%
    group_by(jaar, landgebruik) %>%
    summarise(n_toetsingen = n(),
              n_overschrijdingen = sum(normoverschrijding),
              fractie = n_overschrijdingen / n_toetsingen) %>%
    mutate(landgebruik = fct_reorder(landgebruik, fractie, .desc = TRUE)) %>%
    filter(!is.na(landgebruik), landgebruik != "Stedelijk") %>% View("data_orig")
    ggplot(aes(jaar, fractie)) +
    geom_smooth(se = FALSE, method = "lm", linetype = "dashed", colour = blauw_l) +
    geom_line(colour = blauw, linewidth = 1) +
    geom_point(shape = 19, size = 2, colour = blauw) +
    facet_wrap(~landgebruik, scales = "free", ncol = 2) +
    scale_y_continuous(expand = c(0,0), limits = c(0, 0.035), labels = scales::percent_format(accuracy = 1)) +
    scale_x_continuous(limits = c(rap_jaar - 10, rap_jaar), breaks = scales::breaks_pretty(6)) +
    labs(title = "Hoe vaak wordt de norm overschreden?",
         subtitle = "per type landgebruik",
         x = "",
         y = "% normoverschrijdingen") +
    thema_line_facet +
    theme(panel.spacing = unit(25, "points"))
  