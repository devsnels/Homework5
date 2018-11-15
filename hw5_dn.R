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
  mutate(reported_date = floor_date(reported_date, unit = "month")) %>% 
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
  group_by(reported_date, year, month, season) %>% 
  count() %>% 
  ungroup () %>% 
  rename("homicide_count" = 'n') %>% 
  arrange(year, month) %>% 
  mutate(date_numeric = as.numeric(reported_date))

homicideplot <- homicide %>% 
  ggplot() +
  geom_col(mapping = aes(x = reported_date, y = homicide_count, fill = season), color = "darkgrey") +
  geom_smooth(aes(x = reported_date, y = homicide_count), method = "loess", se = FALSE, span = 0.1) +
  theme_dark() +
  scale_fill_manual(values=c("lightgrey", "lightblue")) +
  labs(x = "Date", y = "Monthly Homicides") +
  ggtitle("Homicides in Baltimore, MD") +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  geom_vline(xintercept = 16526, colour = "red", linetype = "dashed", size = 1) +
  geom_text(x = 16400, y = 35, label = "Arrest of \nFreddie Gray", color = "lightgrey", size = 4) +
  theme(legend.position='bottom')


homicideplot

?geom_smooth
