# Tidy Tuesday Feb 11, 2020
# something about hotels

# load packages
library(tidyverse)


# upload data
hotels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv')


hotels %>% glimpse()

hotels %>% 
  mutate(date = glue::glue("{arrival_date_year}-{arrival_date_month}-{arrival_date_day_of_month}"),
         date = parse_date(date, format = "%Y-%B-%d")) %>%
  ggplot(aes(x=date,group=hotel,color=hotel)) +
  geom_point(stat="count") 


hotels %>%
  mutate(date = glue::glue("{arrival_date_year}-{arrival_date_month}-{arrival_date_day_of_month}"),
         date = parse_date(date, format = "%Y-%B-%d")) %>%
  group_by(hotel,date) %>%
  summarise(n=n()) %>%
  ggplot(aes(x=date,y=n,color=hotel)) +
  geom_path()+
  geom_smooth(span=.2) +
  theme_classic()


library(ggridges)

hotels <- 
hotels %>% 
  mutate(date = glue::glue("{arrival_date_year}-{arrival_date_month}-{arrival_date_day_of_month}"),
         date = parse_date(date, format = "%Y-%B-%d")) %>%
  mutate(date2 = glue::glue("{arrival_date_year}-{arrival_date_month}"),
         date2 = parse_date(date2, format = "%Y-%B")) %>%
# mutate(date3 = glue::glue("{arrival_date_month} {arrival_date_day_of_month}"),
#        date3 = parse_date(date3, format = "%B %d"))  %>%
  mutate(arrival_date_month = factor(arrival_date_month, levels=c("January","February","March","April",
                                        "May","June","July","August",
                                        "September","October","November","December")))
?parse_date

hotels %>%
  group_by(arrival_date_year) %>%
  ggplot(aes(x=arrival_date_month,fill=hotel,y=arrival_date_year,group=interaction(arrival_date_year,hotel)))+
  geom_density_ridges(alpha=.5)+
  theme_ridges()

  
hotels %>% 
  ggplot(aes(x=as.numeric(arrival_date_year),))

