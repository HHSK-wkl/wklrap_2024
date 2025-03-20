library(tidyverse)
library(readxl)
theme_set(HHSKwkl::hhskthema())
library(HHSKwkl)
library(glue)

doelen <-
  readxl::read_excel("data/waterlichamen_ekrs_en_doelen_2024.xlsx") %>% 
  mutate(naam = ifelse(naam == "'t Weegje", "t Weegje", naam)) %>% 
  select(type, naam, doelen, groep)


ekrs <- 
  read_excel("data/overzicht ekr nieuwe toetsing 2024 v02-09-2024.xlsx") %>% 
  pivot_longer(cols = starts_with("20"), names_to = "jaar", values_to = "ekr", values_drop_na = TRUE) %>% 
  rename_all(str_to_lower) %>%
  mutate(jaar = as.numeric(jaar)) %>% 
  arrange(type, nr, naam, jaar) %>% 
  group_by(type, nr, naam) %>% 
  mutate(ekr3 = slider::slide_dbl(ekr, ~mean(.x), .before = 2),
         ekr3_jaren = slider::slide_chr(jaar, ~glue_collapse(.x, sep = " - "), .before = 2)) %>% 
  ungroup() %>% 
  left_join(doelen) %>% 
  mutate(type = fct_relevel(type, c("Algen", "Waterplanten", "Macrofauna", "Vis"))) %>% 
  arrange(nr, type, jaar)

ekrs %>% 
  openxlsx::write.xlsx("C:/data/krw_scores_voortschrijdend_gemiddelde.xlsx")


namen <- ekrs$naam %>% unique()

f_code <- maak_opzoeker(ekrs, naam, nr)

pdf("C:/R/plaatjes/KRW_score_ontwikkelingen.pdf", width = 8, height = 8)

for (naam_wl in namen){
  titel <- glue("{f_code(naam_wl)} {naam_wl}")
  
  
  plot <- 
    ekrs %>% 
    filter(naam == naam_wl) %>% 
    ggplot() +
    geom_point(aes(jaar, ekr), colour = "grey40", shape = 4, size = 3) + 
    # geom_line(aes(jaar, ekr), colour = "grey40") +
    geom_line(aes(jaar, ekr3), colour = blauw, linewidth = 1) +
    geom_point(aes(jaar, ekr3), colour = blauw, size = 2) +
    geom_line(aes(jaar, doel), linetype = "dashed") +
    scale_y_continuous(limits = c(0, 1), expand = expansion(c(0, 0.05)), breaks = (0:10)/10) +
    scale_x_continuous(limits = c(2009, 2023), breaks = scales::pretty_breaks(14), guide = guide_axis(n.dodge = 2)) +
    labs(title = titel,
         x = "",
         y = "EKR",
         caption = "Blauw is het gemiddelde van de laatste 3 meetjaren. De kruisjes zijn de EKR van het meetjaar zelf. De stippellijn is het huidige doel.") +
    facet_wrap(~type, scales = "free_x", ncol = 2) 
  
  print(plot)
}

dev.off()


