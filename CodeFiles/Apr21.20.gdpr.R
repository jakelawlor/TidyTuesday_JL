
gdpr_violations <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_violations.tsv')
gdpr_text <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_text.tsv')



library(ggplot2)
library(dplyr)
library(ggimage)
library(rsvg)

theme_set(theme_classic())

gdpr_violations %>% glimpse()
gdpr_violations %>% distinct(controller)
1??geom_image



gdpr_violations %>% 
  group_by(name,picture) %>%
  summarise(price=sum(price))  %>%
  ggplot(aes(x= reorder(name,-price), y = price))+
  geom_bar(stat = "identity")+
  geom_image(aes(y=price + 2500000, image=picture))+
  theme(axis.text.x=element_text(angle=45,hjust=1))



gdpr_violations %>% arrange(desc(price)) %>% select(price, controller, name)


gdpr_violations  %>% select(-picture) %>% head()

 gdpr_violations %>% group_by(controller) %>%
  filter(name =="France") %>%
  summarise(number = n(),
            price = mean(price)) %>%
  arrange(desc(number)) %>%
   filter(grepl("facebook", tolower(controller) ) )
   
 gdpr_violations %>%
   filter(grepl("telecom", tolower(controller))) %>%
   select(article_violated)

gdpr_text %>% glimpse()


gdpr_violations %>%
  mutate(art = substring(article_violated,1,7)) %>%
  select(art)
  




