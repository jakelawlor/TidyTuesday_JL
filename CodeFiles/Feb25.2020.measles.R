# Tidy Tuesday Feb 25 2020
# something about measles


# libraries
library(ggplot2)
library(dplyr)


# data
measles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-25/measles.csv')



measles %>% glimpse()

min(measles$overall)



# see which cities have highest vaccine rates 
measles %>% group_by(county,state) %>%
  filter(mmr > 0) %>%
  #filter(state %in% c("Washington")) %>%
  summarise(countymean=mean(mmr)) %>%
  ungroup() %>% group_by(state) %>%
  summarise(statemean = mean(countymean)) %>%
 # filter(mean > 5 & mean < 80) %>%
  arrange((statemean))


theme_set(theme_classic())

measles %>% group_by(state,county) %>%
  summarise(countymean = mean(mmr)) %>%
  ungroup() %>% group_by(state) %>%
  ggplot(aes(x=state,y=countymean)) + 
  geom_violin()

measles %>% group_by(state) %>%
  ggplot(aes(x=state,y=mmr)) +
  geom_violin(scale="count")

install.packages("ggbeeswarm")
library(ggbees)


measles %>% distinct(state)
  filter(mmr > 0 & overall > 0) %>%
  group_by(state) %>%
  summarise(meanmmr = mean(mmr),
            meanvac = mean(overall)) %>%
  tidyr::pivot_longer(c(meanmmr,meanvac),names_to = "vac",values_to = "rate") %>%
  arrange(state)

  
  
measles %>% filter(!is.na(type)) %>%
  ggplot(aes(x=xrel,y=xper,group=type,color=type)) +
  geom_point()


library(ggbeeswarm)
measles %>% filter(mmr > 0 ) %>%
  group_by(state,city) %>%
  summarise(citymean = mean(mmr))  %>%
  ggplot(aes(x=state,y=citymean)) +
  geom_beeswarm() +
  theme(axis.text.x = element_blank())


measles %>% filter(!is.na(type)) %>%
  group_by(type) %>%
  ggplot(aes(x=type,y=mmr)) + 
  geom_violin()



stateorder <- measles %>% filter(mmr>0) %>%
  mutate(herd = case_when(mmr >= 93 ~"yes",
                          mmr < 93 ~ "no")) %>%
  select(city,state,type,county,mmr,overall,herd) %>%
  count(state,herd) %>%
  group_by(state) %>%
  mutate(n2 = n/sum(n) * 100) %>%
  filter(herd =="yes") %>%
  arrange(desc(n2)) %>% select(state)
  

measles %>% #filter(mmr>0) %>% 
  filter(state %in% c("California")) %>%
  mutate(herd = case_when(mmr >= 95 ~"yes",
                          mmr < 95 ~ "no")) %>%
  select(city,state,type,county,mmr,overall,herd) %>%
  count(county,herd) %>%
  group_by(county) %>%
  mutate(n2 = n/sum(n) * 100) %>%
  ggplot(aes(x=county,y=n2,fill = herd)) +
  geom_bar(position="stack", stat="identity") 


measles %>% filter(state %in% c("Idaho","Florida"))

df <- data.frame(group = rep(1:4,times=c(20,10,17,8)),
                 outcome = rep(c("yes","yes","no","yes","no"),times = 11))


measles %>% distinct(year)



df %>% count(group, outcome) %>% group_by(group) %>% mutate(n = n/sum(n) * 100)





