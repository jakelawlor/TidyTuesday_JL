# tidy tuesday 2019, week 2
# Australian Fire data

theme_set(theme_minimal(base_size = 12))



library(tidyverse)
library(lubridate)
library(ggforce)


rainfall <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/rainfall.csv')
temperature <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/temperature.csv')


rainfall %>% glimpse()

head(rainfall)
rainfall %>% range(rainfall)
range(rainfall$rainfall,na.rm = T)


rainfall %>% distinct()

library(sf)

#downloaded data from here "https://datasets.seed.nsw.gov.au/dataset/1d05e145-80cb-4275-af9b-327a1536798d/resource/9de6386e-6115-4af4-bfc4-e0baa1b62381/download/firenpwsfirehistory.zip","FireHistory.shp"))
fires <- read_sf(file.choose())

fires  %>% as.data.frame() %>%  select(-geometry)%>%  separate(Label,into = c("Start_Year","End_Year","Label"))  %>% distinct(Label)

# get fire data through time
firesclean <- fires %>%
  #filter(grepl("Wildfire",Label)) %>%
  select(FireName,Label,AreaHa) %>%
  separate(Label,into = c("Start_Year","End_Year","Label")) %>%
  mutate("Start_Year" = as.numeric(Start_Year)) %>%
  mutate("End_Year" = Start_Year + 1)

firesclean <- as.data.frame(firesclean) %>% select(FireName,Start_Year,End_Year,Label,AreaHa)

# plot fire area by year
nwsfires <- firesclean %>% group_by(Start_Year,Label) %>%
  #filter(Start_Year > 1970) %>%
  summarise(Total_Ha = sum(AreaHa)/1000000) %>%
  ggplot(aes(x=Start_Year,y=Total_Ha,fill=Label)) +
  geom_bar(stat="identity")+
  geom_hline(yintercept = 1,linetype="dashed",size=.5)+
  theme(axis.text.x = element_text(angle = 45))+
  theme_classic()+
  scale_y_continuous(limits = c(0,3.75),expand = c(0,0))


rainfall %>% glimpse()

SydRain <- rainfall %>% 
  filter(city_name %in% c("Sydney")) %>% #pick only nsw cities
  filter(!is.na(rainfall)) %>%
  filter(year > 1901 & year < 2020) %>%
 # mutate(rainfall = is.numeric(rainfall)) %>%
  group_by(city_name,year) %>% 
  summarise(total = sum(rainfall)) %>%
  arrange(year) %>%
  mutate(diff = total - 1198) %>%
#  summarise(mean=mean(total,na.rm = T)) %>%
  ggplot(aes(x=year,y=diff))+
  geom_bar(stat = "identity")+
  scale_x_continuous(limits = c(1892,2030))


 rainfall %>% 
  filter(city_name %in% c("Sydney","Canberra")) %>% #pick only nsw cities
  filter(!is.na(rainfall)) %>%
  filter(year > 1901 & year < 2020) %>%
  # mutate(rainfall = is.numeric(rainfall)) %>%
  group_by(city_name,year) %>% 
  summarise(total = sum(rainfall)) %>%
  ungroup() %>%
  group_by(year,city_name) %>%
  summarise(total=mean(total)) %>%
  arrange(year) %>%
  #mutate(diff = total - 1198) %>%
  #  summarise(mean=mean(total,na.rm = T)) %>%
  ggplot(aes(x=year,y=total))+
  geom_bar(stat = "identity")+
  scale_x_continuous(limits = c(1892,2030))+
  facet_grid(~city_name)



yearstring <- c(1990:2019)


temperature %>% 
  #filter(!city_name %in% c("PORT","KENT")) %>%
  group_by(date,city_name) %>%
  spread(key=temp_type,value = temperature) %>%
  mutate(meantemp = (max+min)/2) %>%
  mutate(year = as.numeric(substring(date,1,4))) %>%
  #filter(year > 1990) %>%
  ungroup() %>%
  group_by(year,city_name) %>%
  summarise(mean = mean(meantemp,na.rm = T),
            max=mean(max,na.rm=T),
            min=mean(min,na.rm = T)) %>%
  ggplot(aes(x=year,y=mean,color=city_name)) +
  geom_line(alpha=.5) +  
  geom_smooth(se=F)+
  scale_y_continuous(limits = c(0,32))+
  theme_classic()

  
rainfall %>% 
  filter(!city_name %in% c("Adelaide")) %>%
#  filter(year > 1920) %>%
  group_by(year,city_name) %>%
  summarise(Total = sum(rainfall,na.rm = T)) %>%
  ggplot(aes(x=year,y=Total,color=city_name)) +
  geom_line(alpha=.5) +
  geom_smooth()+
#  facet_wrap(~city_name,nrow=2,as.table=F)+
  theme_minimal()

?geom_smooth


#### from jake knaupp
rainfall %>% 
  group_by(year, month) %>% 
  summarize(rainfall = mean(rainfall, na.rm = TRUE)) %>% 
  group_by(year) %>% 
  summarize(rainfall = sum(rainfall, na.rm = TRUE)) %>% 
  filter(year < 2020) %>%
  ggplot( aes(x = year, y = rainfall)) +
  geom_point(color = "#afc3cc") +
  geom_path(color = "#afc3cc") +
  ggforce::geom_mark_circle(aes(filter = year == 2019, label = glue("Total Rainfall: {round(rainfall,1)} mm"), description = "Annual rainfall at a 60 year low."), label.family = c("Oswald", "Lora"), label.buffer = unit(6, "cm"), label.fontsize = 10) +
  labs(x = NULL,
       y = NULL) +
  scale_x_continuous(limits = c(1900, 2020), breaks = seq(1900, 2020, 10)) +
  scale_y_continuous(limits = c(10, 50), breaks = seq(10, 50, 10), label = label_number_si(unit = "mm")) +
  theme_jk(grid = "XY")


rain <- ggplot(yearly_rain, aes(x = year, y = rainfall)) +
  geom_point(color = "#afc3cc") +
  geom_path(color = "#afc3cc") +
  geom_mark_circle(aes(filter = year == 2019, label = glue("Total Rainfall: {round(rainfall,1)} mm"), description = "Annual rainfall at a 60 year low."), label.family = c("Oswald", "Lora"), label.buffer = unit(6, "cm"), label.fontsize = 10) +
  labs(x = NULL,
       y = NULL) +
  scale_x_continuous(limits = c(1900, 2020), breaks = seq(1900, 2020, 10)) +
  scale_y_continuous(limits = c(10, 50), breaks = seq(10, 50, 10), label = label_number_si(unit = "mm")) +
  theme_jk(grid = "XY")


