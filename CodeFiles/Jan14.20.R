
# 
library(tidyverse)
library(ggtext)
library(gtools)
library(extrafont)
library(sysfonts)
library(showtext)
library(ggforce)



# imported extra data files from Knowledge is Beaituful blog, where the original TidyTuesday came from
# https://docs.google.com/spreadsheets/d/1cz7TDhm0ebVpySqbTvrHrD3WpxeyE4hLZtifWSnoNTQ/edit#gid=12
combos <- read.csv(here::here("data files","Jan14.20","pw_combos.csv"))
strengths <- read.csv(here::here("data files","Jan14.20","pw_strengths.csv")) %>% filter(complete.cases(.))



# set my graphing themes
theme_set(theme_classic())
theme_update(
  rect=element_rect(fill="grey20",color="grey20"),
  line=element_line(color="grey80"),
  panel.background = element_rect(fill="grey20",color="grey20"),
  axis.line = element_line(color=alpha("grey80",.75)),
  axis.text.x = element_markdown(family = "Gill Sans MT",color=alpha("grey80",.7)),
  axis.title.x = element_markdown(family = "Gill Sans MT",color=alpha("grey80",.7)),
  axis.text.y = element_markdown(family = "Gill Sans MT",color=alpha("grey80",.7)),
  axis.title.y = element_markdown(family = "Gill Sans MT",color=alpha("grey80",.7)),
  plot.margin = margin(20, 30, 5, 15),
 plot.title = element_markdown(family = "Gills Sans MT",color=alpha("grey80",.7),size=30,hjust = .5),
 plot.subtitle = element_markdown(family="Gill Sans MT",color=alpha("grey80",.7),hjust = .5),
 plot.caption = element_markdown(family="Gill Sans MT",color =alpha("grey80",.7),size=6 )
)



# open quartz because my RStudio doesn't like fonts
quartz()

# plot the backgroun (shaded rectanges)
bgplot <- ggplot() +
  annotate("rect", 
           xmin = 4, xmax = 22, 
           ymin = log(strengths$start[1]), ymax = log(strengths$end[4]), 
           alpha = .25,fill="grey30")+
  annotate("text",
           x=20,y=14.6,
           label="Weak",
           hjust=1,
           vjust=0,
           color="grey80",alpha=.4)+
  annotate("rect", 
           xmin = 4, xmax = 22, 
           ymin = log(strengths$start[5]), ymax = log(strengths$end[8]), 
           alpha = .25,fill="grey40")+
  annotate("text",
           x=20,y=log(strengths$start[5])+.6,
           label="Medium",
           hjust=1,
           vjust=0,
           color="grey80",alpha=.4)+
  annotate("rect", 
           xmin = 4, xmax = 22, 
           ymin = log(strengths$start[9]), ymax = log(strengths$end[11]), 
           alpha = .25,fill="grey30")+
    annotate("text",
             x=20,y=log(strengths$start[9])+.6,
             label="Strong",
             hjust=1,
             vjust=0,
             color="grey80",alpha=.4)+
    annotate("rect", 
           xmin = 4, xmax = 22, 
           ymin = log(strengths$start[12]), ymax = 95, 
           alpha = .25, fill="grey40")+
    annotate("text",
             x=20,y=log(strengths$start[12])+.6,
             label="Nigh on \nUncrackable",
             hjust=1,
             vjust=0,
             color="grey80",alpha=.4)

# change giant numbers to words
logbreaks <- c(log(10000000),log(10000000000),log(10000000000000000),log(10000000000000000000000),log(10000000000000000000000000000),log(10000000000000000000000000000000000))
loglabs <- c("10 million","10 billion","10 quadrillion","10 sextillion", "10 octillion","10 decillion")

# coordinates for noted points
arrows <-
  tibble(
    xend = c(8.1,    9.9, 15.1, 17.9),
    yend = c(log(2.088271e+11), log(1.445551e+17),  log(7.689097e+26), log( 3.972143e+35)),
    x = c(9.5,    8, 16.7, 16.5),
    y = c(26,    42, 63, 83)
  )

# example passwords
examples <- 
  tibble(
    label = c("password","HoRsELoVeR","M1sterW0rldWid3","Al3xAnder.Hamilton"),
    x= c(9.6, 7.9, 16.8, 16.4),
    y= c(25.9,42,63.1,83),
    hjust = c(0,1,0,1),
    color=pal
  )

# highlight a point
marks <- combos %>% 
  gather(keyboard,combos,Lower,upper,numbers,full) %>%
  filter((Length ==11 & keyboard == "full" ))

# add stuff to background plot
fullplot <- bgplot +
  geom_point(data = (combos %>%
                       gather(keyboard,combos,Lower,upper,numbers,full)),
             aes(x=Length,y=log(combos),color=factor(keyboard,levels = c("Lower","upper","numbers","full"))))+
  geom_line(data = (combos %>%
                      gather(keyboard,combos,Lower,upper,numbers,full)),
            aes(x=Length,y=log(combos),color=keyboard)) +
  guides(color=FALSE)+
  coord_cartesian(xlim = c(4,20.15),ylim=c(14,90),expand=F) +
  scale_x_continuous(name="**Password Length**")+
  scale_y_continuous(name=NULL,
                     breaks = logbreaks,
                     labels = loglabs)+
  labs(
       title = "Building Better Passwords",
       subtitle = "Potential character combinations of passwords cumulatively using  <br> **<span style=color:#F8766D>one case</span>**, 
                                                                        **<span style=color:#7CAE00>TwO cAsEs</span>**, 
                                                                        **<span style = color:#00BFC4> AlphaNum3r1c </span>**, 
                                                                    and **<span style=color:#C77CFF> Symb0!$ </span>**",
       caption = "<br>Visualization by Jake Lawlor | Data by Knowledge is beautiful"
       ) +
  geom_curve(data = arrows,
             aes(x, y, xend = xend, yend = yend),
             curvature = 0.15,
             size = 0.35,
             colour=pal,alpha=.7,size=2)+
  geom_text( data=examples,
             aes(x=x,y=y,label=label,hjust=hjust),
             color=c("#F8766D","#7CAE00","#00BFC4","#C77CFF"),
             size=4
            )+
  geom_mark_circle(data=marks,
                   aes(filter=Length==11,x=Length,y=log(combos),
                   label="Ch@ract3rs M@tt3r",
                   description = "when using the full keyboard, it only takes 11 characters to build an uncrackable password"),
                   label.family = "Gill Sans MT",
                   label.fontsize = 8,
                   position = "identity",
                   label.fill = alpha("gray78",.2)
                   ) 
  
quartz()
fullplot

#save plot
ggsave(fullplot,filename = here::here("output","Jan14.20","pwplot.png"),
       dpi=300,
       height = 6,
       width = 8,
       units = c("in"))
warnings()
