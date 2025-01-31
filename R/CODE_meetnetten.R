library(HHSKwkl)
library(tidyverse)
library(sf)
library(leaflet)

rap_jaar <- 2024

fys_chem <- readRDS("data/fys_chem.rds") 
  

meetpunten <- readRDS("data/meetpunten.rds")
parameters <- readRDS("data/parameters.rds")
bio <- readRDS("data/biologie.rds") %>% HHSKwkl::add_jaar()
ws_grens <- st_read("data/ws_grens.gpkg", quiet = TRUE) %>% st_transform(crs = 4326)

meetnetten <- 
  readxl::read_excel("data/tabellen_monitoringsplan_OMS_03_01_2025.xlsx", "meetnetten") %>% 
  filter(!is.na(meetnet_code)) %>% 
  select(contains("meetnet"), mp = meetpunt_code, meetjaar)


blauwe_tekst <- function(tekst, grootte = NA){
  grootte <- if (is.na(grootte)) "" else glue::glue(";font-size:{grootte}px")
  glue::glue("<span style='color:#0079c2{grootte}'>{tekst}</span>")
}

fys_chem_sel <- 
  fys_chem %>% filter(year(datum) == rap_jaar) %>% 
  filter(!parnr %in% c(35, 900:999),
         !str_detect(mp, "^H_|^ADHOC_"),
         parnr < 9000,
         !parnr %in% c(500:599))

n_locs_fc <- 
  fys_chem_sel %>% summarise(n = n_distinct(mp)) %>% pull(n)

n_monsters_fc <- fys_chem_sel %>% summarise(n = n_distinct(mp, datum)) %>% pull(n)

n_parameters_fc <- fys_chem_sel %>% summarise(n = n_distinct(parnr)) %>% pull(n)

groepen <- 
  parameters %>% 
  mutate(groep = case_when(
    parnr %in% c(1:99) ~ "Algemene stoffen",
    parnr %in% c(100:199) ~ "Veldwaarnemingen",
    parnr %in% c(200:299) ~ "Metalen",
    parnr %in% c(300:499) ~ "Zwemwatermetingen",
    parnr %in% c(2000:2399) ~ "Organische stoffen",
    .default = cluster
  )) %>% 
  select(parnr, groep, parnaamlang) 

library(reactable)
tabel_chemie_metingen <- 
  fys_chem_sel %>% 
  left_join(groepen) %>% 
  mutate(groep = fct_reorder(groep, parnr),
         parnr = fct_reorder(as.character(parnr), parnr)) %>% 
  # mutate(n = 1) %>% 
  group_by(groep, parnaamlang) %>% 
  summarise("Aantal metingen" = n(),
            "Aantal meetlocaties" = n_distinct(mp)) %>%
  ungroup() %>% 
  rename(Groep = groep, Stof = parnaamlang) %>% 
  reactable::reactable(filterable = TRUE,
                       groupBy = "Groep",
                       columns = list(
                         Stof = colDef(),
                         `Aantal metingen` = colDef(aggregate = "sum", filterable = FALSE),
                         `Aantal meetlocaties` = colDef(aggregate = "max", filterable = FALSE)
                         )
                       )
  

taxongroepen <- 
  tibble::tribble(
    ~taxatype,                          ~Soortgroep,
    "DIATM",          "Kiezelwieren (diatomeeën)",
    "FYTPT",               "Algen (fytoplankton)",
    "MACAG",               "Algen (fytoplankton)",
    "MACEV", "Waterdiertjes (macro-evertebraten)",
    "MACFT",               "Planten (macrofyten)",
    "VISSN",                             "Vissen",
    "ZOOPT",         "Zoöplankton (watervlooien)"
  )

tabel_bio_waarnemingen <- 
  bio %>% 
  select(-contains("stadium")) %>% 
  distinct() %>% 
  filter(jaar == rap_jaar - 1,
         !methode %in% c("VISNHA", "VISBM", "KRVEG")) %>% 
  filter(!naam %in% c("Geen kreeften", "Plantae", "Pisces")) %>% 
  mutate(nednaam = twn::twn_localname(naam)) %>% 
  left_join(taxongroepen) %>% 
  mutate(Soortgroep = ifelse(methode == "KR12" & taxatype == "MACEV", "Kreeften", Soortgroep)) %>% 
  group_by(Soortgroep, naam, nednaam) %>% 
  summarise("Aantal waarnemingen" = n(),
            "Aantal meetlocaties" = n_distinct(mp)) %>% 
  ungroup() %>% 
  rename(`Wetenschappelijke naam` = naam, `Nederlandse naam` = nednaam) %>% 

  reactable::reactable(filterable = TRUE,
                       groupBy = "Soortgroep",
                       columns = list(
                         `Wetenschappelijke naam` = colDef(),
                         `Nederlandse naam` = colDef(),
                         `Aantal waarnemingen` = colDef(aggregate = "sum", filterable = FALSE),
                         `Aantal meetlocaties` = colDef(aggregate = "max", filterable = FALSE)
                       )
  )









kaart_meetnet <- function(meetnet){
  meetpunten %>% 
    right_join(filter(meetnetten, meetnet_code == meetnet), by = "mp") %>% 
    sf::st_as_sf(coords = c("x", "y"), crs = 28992) %>% 
    st_transform(crs = 4326) %>% 
    basiskaart() %>% 
    addPolylines(data = ws_grens, opacity = 1, color = "grey", weight = 3, label = "waterschapsgrens") %>% 
    leaflet.extras::addFullscreenControl()
    
}
n_locs_meetnet <- function(meetnet){
  filter(meetnetten, meetnet_code == meetnet) %>% 
  pull(mp) %>% unique() %>% length()
}

n_locs_meetnet("basis")

meetnet_basis <- 
  kaart_meetnet("basis") %>% 
  addCircleMarkers(fillColor = blauw, fillOpacity = 1, radius = 8, 
                   color = "#616161", opacity = 1, weight = 0, stroke = FALSE,
                   popup = ~mp, label = ~mp)

# kaart_waterlichamen <-
#   waterlichamen %>%
#   left_join(popup_data, by = "nr") %>% #st_drop_geometry() %>%  View()
#   st_transform(crs = 4326) %>%
#   leaflet() %>%
#   leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group = "Kaart") %>%
#   leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Luchtfoto") %>%
#   leaflet::addLayersControl(baseGroups = c("Kaart", "Luchtfoto"),
#                             options = leaflet::layersControlOptions(collapsed = FALSE), position = "topleft") %>%
#   addPolylines(data = ws_grens, opacity = 1, color = "grey", weight = 2, label = "waterschapsgrens") %>%
#   addPolygons(weight = 4, color = ~pal(naam),
#               fillOpacity = 0.8, opacity = 0.8,
#               label = ~naam,
#               popup = ~popup_tekst,
#               highlightOptions = highlightOptions(color = blauw, bringToFront = TRUE, opacity = 1)) %>% 
#   leaflet.extras::addFullscreenControl()