library(tidyverse)
library(lubridate)
library(sf)
library(maps)
library(ggthemes)
library(viridis)

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
         -disposition)

head(homicides)

homicides %>% 
  filter(unsolved == TRUE) %>% 
  ggplot() +
  geom_bar(aes(x = victim_race,
               fill = victim_sex)) +
  scale_x_discrete(limits = c("Black", "Hispanic", "White", "Asian", "Other", "Unknown")) +
  labs(x = "Race",
         y = "Homicide counts",
         title = "Number of unsolved homicides by race and gender") +
  theme_tufte() +
  scale_fill_viridis_d(name = "Gender",
                       breaks = c("Male", "Female", "Unknown"))
