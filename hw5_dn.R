library(dplyr)
library(stringr)
library(tidyselect)
library(ggplot2)
library(rmarkdown)
library(RColorBrewer)
library(knitr)
library(forcats)
library(readr)
library(tidyr)
library(broom)
library(purrr)
library(scales)
library(lubridate)
library(viridis)
library(RColorBrewer)
library(scales)
library(tidyverse)

homicide <- read_csv('Data/homicide-data.csv') 

homicide <- homicide %>% 
  unite(cityname, city, state, sep = ", ") %>% 
  select(reported_date, cityname, victim_last, victim_first) %>% 
  filter(cityname == "Baltimore, MD") %>% 
  mutate(reported_date = ymd(reported_date)) %>% 
  mutate(year = year(reported_date),
         month = month(reported_date)) %>% 
  mutate(season = recode(month, 
                         '5' = "summer",
                         '6' = 'summer',
                         '7' = 'summer',
                         '8' = 'summer',
                         '9' = 'summer',
                         '10' = 'summer',
                         '11' = 'winter',
                         '12' = 'winter',
                         '1' = 'winter',
                         '2' = 'winter',
                         '3' = 'winter',
                         '4' = 'winter')) %>% 
  group_by(year, month, season) %>% 
  count() %>% 
  ungroup () %>% 
  mutate(yearcopy = year,
         monthcopy = month) %>% 
  unite(monthyear, monthcopy, yearcopy) %>% 
  rename("homicide_count" = 'n') %>% 
  arrange(year, month)

?arrange

homicide

homicideplot <- homicide %>% 
  ggplot() +
  geom_col(mapping = aes(x = reorder(monthyear, year, month), y = homicide_count, fill = season), color = "darkgrey") +
  theme_dark() +
  scale_fill_manual(values=c("lightgrey", "lightblue")) +
  labs(x = "Date", y = "Monthly Homicides") +
  ggtitle("Homicides in Baltimore, MD") +
  scale_x_discrete(breaks=c("1_2008", "1_2010", "1_2012", "1_2014", "1_2016", "1_2018"),
                   labels=c("2008", "2010", "2012", "2014", "2016", "2018")) 

  
homicideplot



