## Dec 23, 2019
#
# Weekly Top Christmas Songs over time


# Set up
library(tidyverse)
library(cartography)
library(cowplot)

# Load Data
tuesdata <- tidytuesdayR::tt_load("2019-12-24")
songs <- tuesdata$christmas_songs
rm(tuesdata)


# make vector for total years
dlist <- c(1958:2017)
N <- length(dlist)
dlist[10] #call number in years vector


## function to make tree plots by year
#--- --- --- --- --- --- --- --- --- --- --- --- --- ---
tree.plot <- function(i=10,list=dlist) {
# make color pal
pal <- carto.pal("green.pal",n1=nrow(songs %>% filter(year %in% dlist[i]) %>% distinct(songid)) )
# ended up not actually using the palette, but still leaving the code for the "if" statement below

 # make plot
if(length(pal) > 0){
songs %>% select(song,weeks_on_chart,year,songid) %>% 
  group_by(year) %>%
  filter(year %in% c(dlist[i])) %>%
  mutate(weeks_on_chart_cent = weeks_on_chart/2 ) %>%
  mutate(weeks_on_chart_cent_neg = -weeks_on_chart_cent) %>%
  gather_(key="original",value_col = "weeks",gather_cols=c("weeks_on_chart_cent","weeks_on_chart_cent_neg")) %>%
  distinct() %>%
  #ggplot(aes(x=reorder(songid,-weeks_on_chart),y=weeks,fill=reorder(songid,weeks_on_chart)  )) + 
  ggplot(aes(x=reorder(songid,-weeks_on_chart),y=weeks )) + 
  geom_bar(stat="identity",fill="#2BAE66FF", width=0.85) +
  coord_flip(xlim = c(0,15),ylim = c(-11,11) ) +
  theme_classic()+
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = "#FEFCFC",colour = "#FEFCFC"),
        panel.background = element_blank(),
        axis.line = element_line(color = "#89ABE3FF"))+
  annotate(geom="text",y=-10,x=14,hjust=0,label=paste(dlist[i]),color="#101820FF") +
  geom_point(inherit.aes = F, aes(y=0,x=length(pal)+1),shape=8,stroke=.5,color="#FFD662FF",size=1)+ #star
  geom_polygon(inherit.aes=F,data=data.frame(x = c(-1,-1, .5, .5),y = c(-.8, .8, .8, -.8)),aes(x = x, y = y), #treetrunk
               fill = "#6E4C1EFF")
    } else { # if no songs made the top 100 that year, just make a blank graph
  ggplot()+
    coord_flip(xlim = c(0,15),ylim = c(-11,11) ) +
    theme_classic()+
    theme(legend.position = "none",
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          plot.background = element_rect(fill = "#FEFCFC",color = "#FEFCFC"),
          panel.background = element_blank(),
          axis.line = element_line(color = "#89ABE3FF"))+
    annotate(geom="text",y=-10,x=14,hjust=0,label=paste(dlist[i]),color="#101820FF")
      }
}

# test it
tree.plot(10) # year with no top 100 xmas songs
tree.plot(3) # year with lots of top 100 xmas songs


# print all plots in a list
treelist <- list()
for(i in 1:N){
  treelist[[i+8]] <-   tree.plot(i) # start at i+8 to arrange by decade (skipping 1950-1957)
}


# ggarrange the list - this leaves background color weird
trees.arranged <- ggpubr::ggarrange(plotlist = treelist,ncol=10,nrow=7) 

# add title and bg color
cowplot::ggdraw(trees.arranged)+
theme(plot.background = element_rect(fill="#FEFCFC",color="#FEFCFC")) +
draw_label("Christmas Song Trees, 1958 - 2017", 
           color = "#89ABE3FF", size = 32,
           fontfamily = "Georgia", fontface = "bold",
           x=.01,y=.99,
           hjust=0,vjust=1)+
  draw_label("Branches represent individual Christmas Songs in the Billboard Top 100. \nBranch widths represent number of weeks on the charts.", 
             color = "#101820FF", size = 18,
             fontfamily = "Helvetica Neue Light",
             x=.01,y=.93,
             hjust=0,vjust=1)

ggsave(here::here("output","Dec23.19","treeplot.png"),
       height = 7, width = 10.75,dpi=300)






