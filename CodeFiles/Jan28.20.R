# trees of San Francisco

library(dplyr)
library(ggplot2)
install.packages("sf")
sf_trees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv')



sf_trees %>% glimpse()



sf_trees %>%
  ggplot(aes(x=caretaker)) +
  geom_bar(stat="count") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45,hjust=1))


top10 <- sf_trees %>%
group_by(species) %>%
  summarise(n=n(),
            meandbh = mean(dbh,na.rm = T)) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  slice(2:11)


tree1 <- sf_trees %>%
  filter(species %in% top10$species[1]) %>%
  filter(dbh < 1000) %>% 
  filter(complete.cases(.))


pack_circles(sf_trees$d)



sf_trees %>%
  filter(species %in% top10$species) %>%
  ggplot(aes(x=species)) +
  geom_bar(stat="count") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45))




# trees of San Francisco

library(dplyr)
library(ggplot2)
library(ggraph)
library(ggforce)

sf_trees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv')



sf_trees %>% glimpse()

sites <- unique(sf_trees$site_info)
sites2 <- data.frame(sites=sites,num=1:length(sites))
sites2$sites <- as.character(sites2$sites)


sites2 %>%
  filter(grepl("^sidewalk", tolower(sites)))




sf_trees %>%
  filter(grepl("^sidewalk",tolower(site_info))) %>%
  distinct(spec)

sites2 %>% glim

sites %like% "Sidewalk"

sf_trees %>%
  ggplot(aes(x=caretaker)) +
  geom_bar(stat="count") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45,hjust=1))

unique(sf_trees$legal_status)


sigtrees <- 
  sf_trees %>%
  filter(legal_status == "Significant Tree") 


unique(sigtrees$species)


library(ggridges)

sf_trees %>%
  filter(!is.na(date)) %>%
  filter(!is.na(dbh)) %>%
  filter(dbh < 40) %>%
  group_by("species") %>%
  filter(species %in% top10$species) %>%
  ggplot() +
  geom_density_ridges(aes(x=dbh,y=species,fill=species,height=..density..),show.legend = F,stat = "density")+
  theme_ridges()

biggest10 <- sf_trees %>%
  filter(!is.na(dbh)) %>%
  arrange(desc(dbh)) %>%
  slice(1:10)

biggest10$address

top10 <- sf_trees %>%
  filter(grepl("^sidewalk",tolower(site_info))) %>%
  filter(!is.na(dbh)) %>%
  group_by(species) %>%
  summarise(n=n(),
            meandbh = mean(dbh,na.rm = T)) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  slice(1:10)

bottom10 <- 
  sf_trees %>%
  filter(grepl("^sidewalk",tolower(site_info))) %>%
  filter(!is.na(dbh)) %>%
  group_by(species) %>%
  summarise(n=n(),
            meandbh = mean(dbh,na.rm = T)) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  slice(2:10)

sidewalktrees <- sf_trees %>%
  filter(grepl("^sidewalk",tolower(site_info))) %>%
  filter(!is.na(dbh))


test <- sidewalktrees %>% 
  filter(species %in% top10$species[1]) %>%
  slice(1:500)

tree1 <- sf_trees %>%
  filter(species %in% top10$species[1]) %>%
  filter(dbh < 1000) %>% 
  filter(!is.na(dbh))


unique(tree1$species)

position <- pack_circles(test$dbh)


data <- data.frame(x=position[,1],y=position[,2],r=sqrt(test$dbh/pi))

ggplot() +
  geom_circle(aes(x0 = x, y0 = y, r = r), data = data, fill = 'steelblue') +
  coord_equal() +
  theme_void()



sf_trees %>%
  filter(species %in% top10$species) %>%
  ggplot(aes(x=species)) +
  geom_bar(stat="count") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45))


