# TidyTuesday on the office 


# libraries
library(ggplot2)
library(dplyr)
library(schrute)
library(waffle)
library(ggtext)
library(cowplot)
library(patchwork)




# get data
office_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv')


mydata <- schrute::theoffice
# change season to numbers
mydata <- mydata %>%
  mutate(season=as.numeric(season),
         episode = as.numeric(episode),
         title = episode_name)
# remove weird characters
mydata$character <- gsub("\"","",mydata$character)


# can I find "that's waht she said" ?
shesaid <- mydata %>% 
  filter(grepl("that's what she said", tolower(text) ) ) %>%
  select(index, season, episode, episode_name, character, text, title) %>%
  mutate(category = "shesaid") %>%
  mutate(language = "english")

hesaid <- mydata %>%  
  filter(grepl("that's what he said", tolower(text) ) ) %>%
  select(index, season, episode, episode_name, character, text, title) %>%
  mutate(category = "hesaid") %>%
  mutate(language = "english")

hesaid2 <-  mydata %>%  
  filter(grepl("eso es lo que dice", tolower(text) ) ) %>%
  select(index, season, episode, episode_name, character, text, title) %>%
  mutate(category = "hesaid") %>%
  mutate(language = "spanish")


theysaid <- rbind(shesaid,hesaid,hesaid2) %>% rbind(c(NA,8,NA,NA,NA,NA,NA,NA,NA))

# some theme stuff
bgcol <- "#f7f4ea"
titlefont <- "Chalkboard Bold"
subtitlefont <- "Chalkboard"
legendfont <- "Helvetica Neue"
theme_set(theme_classic())
theme_update(
  plot.background=element_rect(fill=bgcol,color=bgcol),
  panel.background=element_blank()
)


# twss bar graph by character
#=== === === === === === === === === ===
characterspal <- c(
                   "#b0ba77", # creed olive
                   "#ebe7df", # david beige
                   "#dec787", # dwight mustard
                   "#fff3b5", # holly blonde
                   "#854e82", # jan purple
                   "#c7d0eb", # Jim light blue
                   "#484959", # michael grey
                   "#eba0b0" # pam rose,
                  )

bychar <- theysaid %>%
  filter(complete.cases(.)) %>%
  group_by(character) %>%
  summarise(count=n()) %>%
  mutate(character = case_when(character == "David" ~ "David Brent",
                               TRUE ~ .$character))%>%
  ggplot()+
  geom_bar(aes(x=character,y=count,fill=character),
           stat="identity",color="grey20",size=.25)+
  scale_x_discrete(limits=rev(c("Michael","Dwight","Jim","Pam","Creed","David Brent","Holly","Jan")))+
  scale_fill_manual(values=(characterspal))+
  geom_text(aes(label=character,x=character,y=count+.2),
            hjust=0,family="American Typewriter")+
  guides(fill=F)+
  coord_flip(ylim = c(0,31),xlim=c(0.25,8.75),expand = F)+
  labs(title='TWSS Jokes by Character',
       subtitle="Michael Scott makes more TWSS jokes than all other characters combined")+
  theme(text = element_text(family=titlefont),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title=element_blank(),
        axis.text=element_text(family=titlefont),
        panel.background = element_blank(),
        plot.margin = margin(10,10,10,10),
        plot.title = element_text(size=12,family=titlefont),
        plot.subtitle = element_text(size=10,family=subtitlefont))

bychar


# ok, now separate by gender
#=== === === === === === === === === ===
# add column for gender
# jokes by gender 
donutpal <- c("#38818c","#aaa4b0")

donut <- 
theysaid %>% 
  filter(complete.cases(.)) %>%
  mutate(gender = case_when(character %in% c("Michael","Dwight","Jim","Creed","David") ~ "male",
                            character %in% c("Pam","Holly","Jan") ~ "female")) %>%
  group_by(gender) %>%
  summarise(count=n()) %>%
  mutate(percent = round(count/nrow(theysaid %>%filter(complete.cases(.)))*100),
         lab.pos = cumsum(percent)-.5*percent,
         gender= factor(gender,levels=c("male","female"))
        ) %>%
  
  ggplot(aes(x=2,y=percent))+
  geom_bar(aes(x=2,fill=gender),stat="identity",show.legend = F) +  
  coord_polar("y",start=-250)+
  geom_text(aes(y=lab.pos, label = paste(percent,"%", sep = "")), col = c("white","white")) +
  xlim(-.15,2.5)+
  theme_void()+
  labs(title="Gender of Joke Maker",
       subtitle = "Male characters tell almost 90% of TWSS jokes")+
  scale_fill_manual(values = donutpal) +
  theme(plot.title=element_text(family=titlefont,size=12),
        plot.subtitle = element_text(family = subtitlefont,size=10),
        panel.background = element_blank(),
        plot.margin = margin(10,10,10,10),
        plot.background = element_rect(fill=bgcol,color=bgcol))+
  geom_richtext(aes(label="<span style = 'color:#38818c'> Male </span><br><span style = 'color:#aaa4b0'> Female </span>",
                    x=-.15,y=0),
                fill=NA, label.color=NA,
                family="American Typewriter Bold",
                size=6)
  
donut



# they said waffle plot
#=== === === === === === === === === ===
waffle <- theysaid %>% group_by(season,category,language,character.drop=F) %>%
  summarise(count=n()) %>%
  ungroup() %>%
  mutate(category = case_when(category=="shesaid" ~ "That's What *She* Said",
                              category=="hesaid" ~ "That's What *He* Said"),
         category = factor(category,levels=c("That's What *She* Said","That's What *He* Said"))) %>%
  mutate(season = paste("Season",season)) %>%
  arrange(category) %>%
  ggplot()+
  geom_waffle(aes(fill=category,values=count,size=language),n_rows = 2,flip = T) + 
  guides(color=F) +
  scale_fill_manual(values=c("#7ecca3","#f9f6f4"),name=NULL)+
  scale_size_manual(values=c(.25,.75))+
  facet_wrap(~season,nrow=1,strip.position = "bottom")+
  coord_cartesian(ylim=c(.4,5.7),expand = F) +
  theme_void()+
  labs(title="That's What They Said",
       subtitle="'She Said' and 'He Said' Jokes by Season") +
  theme(plot.title=element_text(family=titlefont,size=12),
        plot.subtitle = element_text(family=subtitlefont,size=10),
        legend.position = "bottom",
        legend.key = element_rect(color="black"),
        legend.text = element_markdown(size=10),
        panel.background = element_blank(),
        plot.margin = margin(5,5,5,5),
        plot.background = element_rect(color=bgcol,fill=bgcol)) 

# add some examples
waffle2<-ggdraw(waffle) +
  geom_text(
    aes(x=.47, y=.88, label = "Lawyer: And you were\ndirectly under her the entire time?"),
    hjust = 0, vjust = 0.5, size = 9/.pt,
    color = "black",
    lineheight=.75,
    inherit.aes = FALSE,
    family="American Typewriter",
    angle=7
  )+
  geom_curve(
    aes(x=.465, y=.88,xend=.395,yend=.81),
    curvature = .15,
    color = "black",
    inherit.aes = FALSE,
  )+
  geom_text(
    aes(x=.57, y=.67, label = "Kelly: Dwight,\nget out of my nook!"),
    hjust = 0, vjust = 0.5, size = 9/.pt,
    color = "black",
    lineheight=.75,
    inherit.aes = FALSE,
    family="American Typewriter",
    angle=7
  )+
  geom_curve(
    aes(x=.565, y=.67,xend=.52,yend=.44),
    curvature = .15,
    color = "black",
    inherit.aes = FALSE,
  )+
  geom_text(
    aes(x=.98, y=.51, label = "Clark: There's no way you guys\nare making that magic\nwith just your mouths!"),
    hjust = 1, vjust = 0.5, size = 9/.pt,
    color = "black",
    lineheight=.75,
    inherit.aes = FALSE,
    family="American Typewriter",
    angle=-7
  )+
  geom_curve(
    aes(x=.87, y=.47,xend=.92,yend=.305),
    curvature = .30,
    color = "black",
    inherit.aes = FALSE,
  )

  

# find correlation over seasons
#=== === === === === === === === === ===
theysaid_seasons <- theysaid %>%
  filter(complete.cases(.)) %>%
  group_by(season) %>%
  summarise(count=n())

ratings_seasons <- office_ratings %>%
  group_by(season) %>%
  summarise(score = mean(imdb_rating),
            sd = sd(imdb_rating))

corr <- theysaid_seasons %>% right_join(ratings_seasons) %>% tidyr::replace_na(list(count=0))

corrplot <- corr %>%
  ggplot(aes(x=count,y=score)) +
  geom_smooth(method="lm",fill="#cedbde",color="black",size=.5) + 
  geom_point() +
  ggrepel::geom_text_repel(aes(label=paste("Season",season)),family="American Typewriter") +
  scale_x_continuous(breaks=c(0,3,6,9), name = "Number of 'That's What She Said's")+
  scale_y_continuous(name="Average rating") +
  labs(title="The Joke Makes the Show?",
       subtitle = "Seasons with more TWSS jokes had higher mean IMDb ratings!")+
  theme(text = element_text(family="American Typewriter"),
        axis.title  = element_text(family = subtitlefont),
        plot.title = element_text(family=titlefont,size=12),
        plot.subtitle = element_text(family = subtitlefont,size=10),
        panel.background = element_blank(),
        plot.margin = margin(10,10,10,15)) 

corrplot


# combine into one
#=== === === === === === === === === ===
row1 <- bychar + donut + plot_layout(widths = c(.7,.3))
row2 <- plot_grid(waffle2,corrplot,rel_widths = c(.6,.4))

fullplot <- row1 / row2 + plot_layout(heights = c(1,1.25))


fullplot2<-fullplot+
  plot_annotation(title="That's What She Said!",
                  subtitle = "Tracking occurrences of The Office's most famous joke",
                  caption = "Data: schrute R packge & IMDb | Vis: @Jake_Lawlor1",
                  theme = theme(text=element_text(family="American Typewriter"),
                                plot.background = element_rect(fill=bgcol,color=bgcol),
                                plot.title = element_text(size=30,hjust=.5),
                                plot.subtitle = element_markdown(size=18,hjust=.5, family="Gill Sans Light"),
                                plot.caption = element_text(hjust=.5),
                                plot.margin = margin(20,15,15,15)) 
                  )




ggsave(fullplot2,
       filename = here::here("output","Mar17.20.office","officeplot2.png"),
       dpi = 300,
       width=12,
       height=8)



