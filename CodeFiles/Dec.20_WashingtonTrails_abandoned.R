# washington trails - Dec 2020

library(ggraph)
library(tidyverse)
library(tidytext)
library(ggtext)
library(igraph)
library(ggforce)

data <- readr::read_rds(url('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-24/hike_data.rds'))
data %>% glimpse()


# lets do some text analysis
descriptions <- hike_data %>% select(name, region, rating, description)

# find bigrams of good reviews
#===================================
descriptions %>%  
  
  # filter only good hikes
  filter(rating >4) %>%
  
  # get column for just 11 regions
  unnest_tokens(bigram, description,token="ngrams",n=3) %>%
  tidyr::separate(bigram,c("word1","word2","word3"),sep=" ") %>%
  
  # cut out meaningless
  anti_join(stop_words,by=c("word1"="word")) %>%
  anti_join(stop_words,by=c("word2"="word")) %>%
  anti_join(stop_words,by=c("word3"="word")) %>%
  filter(!is.na(word1) & !is.na(word2) & !is.na(word3)) %>%
  
  
  # count and sort
  count(word1,word2,word3,sort=T) %>%
  
  # find only bigrams used > once   *** or change this number if you want more/fewer words
  filter(n>1) %>%
  
  # format
  graph_from_data_frame() %>%
  
  # ggraph
  ggraph(layout = "fr") +
  
  # add line with arrow
  geom_edge_link(aes(edge_alpha = log(n)), show.legend = F,
                 arrow=grid::arrow(type="closed",length=unit(.15,"inches")),
                 end_cap=circle(.07,"inches")) +
  
  # add point
  geom_node_point(color="lightblue",size=5) +
  
  # add label
  geom_node_text(aes(label=name),hjust=1,vjust=1,
                 size=4
  ) 
#----------
  

# find bigrams of bad reviews
#===================================
descriptions %>%  
  
  # filter only good hikes
  filter(rating <1) %>%
  
  # get column for just 11 regions
  unnest_tokens(bigram, description,token="ngrams",n=3) %>%
  tidyr::separate(bigram,c("word1","word2","word3"),sep=" ") %>%
  
  # cut out meaningless
  anti_join(stop_words,by=c("word1"="word")) %>%
  anti_join(stop_words,by=c("word2"="word")) %>%
  anti_join(stop_words,by=c("word3"="word")) %>%
  filter(!is.na(word1) & !is.na(word2) & !is.na(word3)) %>%
  
  
  # count and sort
  count(word1,word2,word3,sort=T) %>%
  
  # find only bigrams used > once   *** or change this number if you want more/fewer words
  filter(n>1) %>%
  
  # format
  graph_from_data_frame() %>%
  
  # ggraph
  ggraph(layout = "fr") +
  
  # add line with arrow
  geom_edge_link(aes(edge_alpha = log(n)), show.legend = F,
                 arrow=grid::arrow(type="closed",length=unit(.15,"inches")),
                 end_cap=circle(.07,"inches")) +
  
  # add point
  geom_node_point(color="lightblue",size=5) +
  
  # add label
  geom_node_text(aes(label=name),hjust=1,vjust=1,
                 size=4
  ) 
#----------


# find bigrams of bad reviews -- bigrams only
#===================================
descriptions %>%  
  
  # filter only good hikes
  filter(rating <1) %>%
  
  # get column for just 11 regions
  unnest_tokens(bigram, description,token="ngrams",n=2) %>%
  tidyr::separate(bigram,c("word1","word2"),sep=" ") %>%
  
  # cut out meaningless
  anti_join(stop_words_filtered,by=c("word1"="word")) %>%
  anti_join(stop_words_filtered,by=c("word2"="word")) %>%
  filter(!is.na(word1) & !is.na(word2)) %>%
  filter(!word1 %in% name_words) %>%
  filter(!word2 %in% name_words3) %>%
  
  
  # count and sort
  count(word1,word2,sort=T) %>%
  
  # find only bigrams used > once   *** or change this number if you want more/fewer words
  filter(n>3) %>%
  
  # format
  graph_from_data_frame() %>%
  
  # ggraph
  ggraph(layout = "fr") +
  
  # add line with arrow
  geom_edge_link(aes(edge_alpha = log(n)), show.legend = F,
                 arrow=grid::arrow(type="closed",length=unit(.15,"inches")),
                 end_cap=circle(.07,"inches")) +
  
  # add point
  geom_node_point(color="salmon",size=5) +
  
  # add label
  geom_node_text(aes(label=name),hjust=1,vjust=1,
                 size=4
  ) +
  labs(title="Common words when reviews < 1")+
  theme(plot.background = element_blank(),
        panel.background = element_blank())
#----------

# find bigrams of good reviews -- bigrams only
#===================================
descriptions %>%  
  
  # filter only good hikes
  filter(rating >4) %>%
  
  # get column for just 11 regions
  unnest_tokens(bigram, description,token="ngrams",n=2) %>%
  tidyr::separate(bigram,c("word1","word2"),sep=" ") %>%
  
  # cut out meaningless
  anti_join(stop_words_filtered,by=c("word1"="word")) %>%
  anti_join(stop_words_filtered,by=c("word2"="word")) %>%
  filter(!is.na(word1) & !is.na(word2)) %>%
  filter(!word1 %in% name_words3) %>%
  filter(!word2 %in% name_words3) %>%

  
  
  # count and sort
  count(word1,word2,sort=T) %>%
  
  # find only bigrams used > once   *** or change this number if you want more/fewer words
  filter(n>4) %>%
  
  # format
  graph_from_data_frame() %>%
  
  # ggraph
  ggraph(layout = "fr") +
  
  # add line with arrow
  geom_edge_link(aes(edge_alpha = log(n)), show.legend = F,
                 arrow=grid::arrow(type="closed",length=unit(.15,"inches")),
                 end_cap=circle(.07,"inches")) +
  
  # add point
  geom_node_point(color="lightblue",size=5) +
  
  # add label
  geom_node_label(aes(label=str_to_title(name)),hjust=1,vjust=1,
                 size=4, alpha=.5
  ) 
#----------

"of" %in% stop_words$word



# to remove from stop_words
# old, new, see, seeing, between, all, 
# back, anyone, anywhere, everywhere, 
# full, great, group, groups, long, longer, 
# need, over, problem, problems,
# small, big, large, too, many
exclude_words <- c("old","new","seeing", "between", "all",
                    "back", "anyone", "anywhere", "everywhere", 
                    "full", "great", "group", "groups", "long", "longer", 
                    "need", "over", "problem", "problems",
                    "small", "big", "large", "too", "many","allow")

stop_words_filtered <- stop_words %>% filter(!word %in% exclude_words)
"juan" %in% stop_words$word
"too" %in% stop_words_filtered$word


name_words<-hike_data %>%
  select(specific) %>% 
  unnest_tokens(word, specific,token="words") %>%
  distinct(word) %>% pull(word)

name_words2 <- hike_data %>%
  select(specific) %>% 
  unnest_tokens(word, specific,token="words") %>%
  distinct(word) %>%
  filter(!word %in% c("islands","valley","mountain","peninsula","pacific",
                      "range","island","juan","coast","pass","cascades",
                      "beach","loop","range","sunrise")) %>%
  pull(word)



name_words3<-hike_data %>%
  select(name) %>% 
  unnest_tokens(word, name,token="words") %>%
  distinct(word) 


descriptions %>%
  
  # get column for just 11 regions
  unnest_tokens(bigram, description,token="ngrams",n=3) %>%
  tidyr::separate(bigram,c("word1","word2","word3"),sep=" ") %>%
  filter(word2 %in% c("degree")) 




# which kinds of views are best? 

descriptions %>%
  
  # get column for just 11 regions
  unnest_tokens(bigram, description,token="ngrams",n=2) %>%
  tidyr::separate(bigram,c("word1","word2"),sep=" ") %>%
  
  # cut out meaningless
  filter(word2 %in% c("view","views"))%>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  
  # add views
  mutate(word1 = str_replace(word1, "degree", "360 degree"),
         views=str_c(word1,"views",sep=" "),
         views = str_to_title(views)
  ) %>%
  filter(!word1=="enjoy")%>%
  
  # take most common view types
  group_by(word1) %>%
  add_count() %>%
  
  filter(n>=9) %>% # those with over 10 reviews
  
  filter(!views=="Enjoy Views") %>%
  
  group_by(views) %>%
  mutate(mean_rating = mean(rating),
         med_rating = median(rating)) %>%
  ungroup() %>%
  
  mutate(views = fct_reorder(views,mean_rating)) %>%
  
  
  
  
  ggplot(aes(x=rating,y=views))+
  #ggridges::geom_density_ridges(bandwidth=.07)
  
  stat_halfeye(aes(fill = mean_rating,
                   fill = after_scale(colorspace::darken(fill, .25))
  ),
  trim = F,
  adjust=.25,
  height=.6,
  .width=0)+
  
 #stat_summary(
 #  aes(xend = Inf, yend = views),
 #  orientation = "y",
 #  fun = mean,
 #  geom = "segment",
 #  color = "green",
 #  size = 1.3
 #) +
 ### colored bar for lollipop
 #stat_summary(
 #  aes(xend = -Inf, yend = views),
 #  orientation = "y",
 #  fun = median,
 #  geom = "segment",
 #  size = 1.3
 #) +
  geom_text(
    aes(x = mean_rating, 
        label = format(round(mean_rating, 2))),
    stat = "unique",
    family = "Avenir Next Condensed",
    fontface = "bold",
    size = 4,
    nudge_x = .05,
    nudge_y = -.2,
    hjust = 0
  ) +
  
  
  ggthemes::theme_economist()+
  theme(legend.position = "none")

?geom_density_ridges
  
  ?stat_halfeye
