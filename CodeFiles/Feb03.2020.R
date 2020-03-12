## Tidy Tuesday Feb 3, 2020



attendance <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/attendance.csv')
standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/standings.csv')
games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/games.csv')


theme_set(theme_bw())

# libraries 
library(tidyverse)
library(ggplot2)
library(dplyr)

rm(tuesdata)

## view dataset 
attendance %>% glimpse()
standings %>% glimpse()
games %>% glimpse()



attendance %>% 
  ggplot(aes(x=year,y=home)) +
  geom_line() +
  facet_wrap(~team_name)


?distinct()
