library(tidyverse)
library(sf)        
library(tmap)      
library(here)      
library(readxl)  
library(janitor)   
library(countrycode)

world_map_sf <- st_read(here("World_Countries_(Generalized).geojson")) %>%
  clean_names() 

inequality_raw <- read_csv(here("HDR25_Composite_indices_complete_time_series.csv")) %>%
  clean_names() 

world_map_clean <- world_map_sf %>%
  select(country_name_from_map = country, geometry) %>% 
  mutate(iso_code = countrycode(country_name_from_map, 'country.name', 'iso3c')) %>%
  drop_na(iso_code) %>%
  distinct(iso_code, .keep_all = TRUE)

inequality_clean <- inequality_raw %>%
  select(
    iso_code = iso3, 
    gii_2010_raw = gii_2010,
    gii_2019_raw = gii_2019
  ) %>%

  mutate(
    gii_2010 = as.numeric(gii_2010_raw),
    gii_2019 = as.numeric(gii_2019_raw)
  ) %>%
  mutate(gii_difference = gii_2019 - gii_2010) %>%
  drop_na(gii_difference)

world_data_joined <- world_map_clean %>%
  left_join(
    inequality_clean, # (我们可以连接整个表)
    by = "iso_code"
  )

tmap_mode("view") 

tm_shape(world_data_joined) +
  tm_polygons(
    "gii_difference",  
    title = "GII 2010-2019 变化",
    palette = "-RdYlGn", 
    style = "jenks", 
    popup.vars = c(
      "国家" = "country_name_from_map",
      "2010 GII" = "gii_2010",
      "2019 GII" = "gii_2019",
      "变化" = "gii_difference"
    )
  ) +
  tm_layout(
    main.title = "全球性别不平等指数变化 (2010 vs 2019)"
  )

