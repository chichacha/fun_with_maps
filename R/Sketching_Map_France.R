library(tidyverse)
library(sf)
library(cowplot)
library(ggthemes)

my_path <- "shapefiles/codes_postaux_V5"

## Read Shapefile of France
france_sf <- read_sf(my_path) 
france_sf <- france_sf %>% 
  mutate(wp_url = str_glue("https://www.worldpostalcodes.org/en/france/postal-code/{ID}"))

france_sf %>%
  #filter(str_detect(ID,"^(75)")) %>%
  ggplot() +
  geom_sf(aes(fill=POP2010), color="#ffffff20") +
  #geom_sf_text(aes(label=ID), family="Roboto Condensed", color="#ffffff90",size=16) +
  scale_fill_viridis_c(option="F") +
  theme_nothing()

ggsave("output/france_postalcodes.png", width=10, height=10)

france_sf 


france_sf %>%
  #sf::st_set_crs("4326") %>%
  filter(str_detect(ID,"^(75)")) %>%
  mutate(district = str_sub(ID,4L,5L)) %>%
  ggplot() +
  geom_sf(aes(fill=district), color="#ffffff20") +
  geom_sf_text(aes(label=str_c(ID)), family="Roboto Condensed", color="#ffffff90",size=16/.pt) +
  theme_nothing() +
  scale_fill_manual(values=colorRampPalette(tableau_color_pal("Hue Circle")(19))(20))

ggsave("output/france_paris.png", width=16, height=9)


library(leaflet)


pal <- colorNumeric(
  palette = "RdPu",
  domain = (france_sf$POP2010))

pal(20)

france_sf %>%
  st_transform(crs=st_crs(4326)) %>%
  #filter(str_detect(ID,"^(75)")) %>%
  mutate(district = str_sub(ID,4L,5L)) %>%
  leaflet() %>%
  addProviderTiles(provider=providers$CartoDB.DarkMatter) %>%
  addPolygons(fill=T, fillColor=~pal(POP2010), fillOpacity=0.5, stroke=T, weight=5, color="#ffffff")

?addPolygons


library(rvest)
paris_dist <- read_html("https://www.parisdiscoveryguide.com/paris-arrondissements.html") 
paris_dist %>% rvest::html_elements("h3") %>% 
  html_text() %>% clipr::write_clip()

paris_dist_wiki <- read_html("https://en.wikipedia.org/wiki/Arrondissements_of_Paris")
paris_dist_wiki %>% html_table() %>%
  pluck(2) %>%
  as_tibble() %>%
  select(2:5) %>%
  mutate(number=row_number()) %>%
  relocate(number) %>%
  knitr::kable("markdown")


france_sf %>%
  mutate(ending2=str_sub(ID,4L,5L)) %>%
  ggplot() +
  geom_sf(aes(fill=ending2), color="#ffffff00") +
  facet_wrap(~ending2) +
  theme_map(base_family="Roboto Condensed") +
  theme(plot.background=element_rect(fill="white")) 

ggsave("output/map_france_ending.png", width=16, height=16)

library(see)

france_sf %>%
  mutate(area_2=str_sub(ID,1L,2L),
         ending2=str_sub(ID,4L,5L)) %>%
  ggplot() +
  geom_sf(aes(fill=ending2), color="#ffffff00") +
  scale_fill_material(guide="none") +
  facet_wrap(~area_2) +
  theme_map(base_family="Roboto Condensed") +
  theme(plot.background=element_rect(fill="white"))

ggsave("output/map_france_area_ending.png", width=16, height=16)


france_sf <-france_sf %>%
  mutate(area_2=str_sub(ID,1L,2L),
         ending2=str_sub(ID,4L,5L),
         ending3=str_sub(ID,3L,5L),
         mid2=str_sub(ID,3L,4L),
         area_3 = str_sub(ID,1L,3L))

area_summary <-france_sf %>% 
  as_tibble() %>%
  group_by(area_2) %>%
  arrange(desc(POP2010)) %>%
  summarise(pop=sum(POP2010),
            names=paste(unique(LIB),collapse=","),
            name1 =first(LIB)) %>%
  ungroup() %>%
  arrange(desc(pop))


area_summary3 <-france_sf %>% 
  as_tibble() %>%
  group_by(area_3) %>%
  arrange(desc(POP2010)) %>%
  summarise(pop=sum(POP2010),
            names=paste(unique(LIB),collapse=","),
            name1 =first(LIB)) %>%
  ungroup() %>%
  arrange(desc(pop))

draw_map <- function(x="48") {
  
  selected_area <- x
  
  france_sf %>%
    filter(area_2==selected_area) %>%
    ggplot() +
    geom_sf(aes(fill=ending2), color="#ffffff00") +
    geom_sf_text(aes(label=ending3), family="Roboto Condensed") +
    facet_wrap(~area_2) +
    theme_map(base_family="Roboto Condensed") +
    scale_fill_manual(values=colorRampPalette(tableau_color_pal("Hue Circle")(19))(100), guide="none") +
    labs(title=(area_summary$name1[area_summary$area_2==selected_area]))

  
}

library(patchwork)
draw_map(75) +
  draw_map(48) +
  draw_map(74) +
  draw_map(13) +
  draw_map(69) +
  draw_map(31)
  
  






