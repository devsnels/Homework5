library(tidyverse)
library(lubridate)
library(sf)
library(maps)

homicides <- read_csv("data/homicide-data.csv")
head(homicides, 10)

homicides <- homicides %>% 
  mutate(unsolved = disposition == "Closed without arrest" | disposition == "Open/No arrest",
         reported_date = date(ymd(reported_date))) %>%
  unite(city_name,
        city,
        state,
        sep = ", ") %>% 
  select(-uid,
         -victim_last,
         -victim_first,
         -disposition) %>% 
  filter(lon != "NA")

#us_states <- map("state", plot = FALSE, fill = TRUE) %>% 
  #st_as_sf()

#map_homicides <- st_as_sf(homicides, coords = c("lon", "lat")) %>% 
  #st_set_crs(4269)

#ggplot() +
  #geom_sf(data = us_states,
          #color = "white",
          #fill = "darkcyan",
          #alpha = 0.5) +
  #geom_sf(data = map_homicides, aes(color = victim_sex, shape = victim_race))

il_map <- map(state = "illinois",
              plot = FALSE,
              fill = TRUE) %>% 
  st_as_sf()
