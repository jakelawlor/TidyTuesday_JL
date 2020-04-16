## tidy tuesday from Apr 7, 2020
## tour de france winners


# load data
tuesdata <- tidytuesdayR::tt_load('2020-04-07')
tdf_winners <- tuesdata$tdf_winners
rm(tuesdata)

# libraries
library(ggplot2) #
library(dplyr) #
library(magik)
library(png)#
library(gridGraphics) #
library(ggimage) #
library(ggflags) #
library(countrycode) #



# theme stuff
bgcol <- '#5995ab'
textcol <- "grey95"
titlefont <- "Montserrat Medium"
subfont <- "Montserrat"
subfont2 <- "Montserrat Light"

theme_set(theme_classic())
theme_update(plot.background = element_rect(color=bgcol,fill=bgcol),
              panel.background = element_blank(),
              axis.line = element_line(color=textcol),
              text = element_text(color=textcol),
              axis.ticks = element_line(color=textcol),
              axis.text = element_text(color=textcol,family=subfont2),
             axis.title = element_text(family=subfont),
              plot.title = element_text(family=titlefont),
             plot.subtitle = element_text(family=subfont),
             plot.caption = element_text(family=subfont2)
)



# bar plot of winners by nationality
# will it be possible to fill with country flag?
quartz()
wins <- tdf_winners %>% group_by(nationality) %>%
  summarise(count=n()) %>%
  mutate(code = tolower(countrycode(nationality,
                origin = 'country.name', destination = 'iso2c'))) %>%
  ggplot(aes(x=reorder(nationality,-count),y=count))+
  geom_bar(stat="identity",color=NA,fill=textcol,width = .8) +
  scale_y_continuous(name="Number of Wins",limits = c(0,39),expand=c(0,0))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = margin(5,5,20,5)) +
  geom_flag(aes(country=code,y=(count+1.6),x=reorder(nationality,count)),
            size=6) + 
  labs(title="France Holds the Crown")

wins

# closest races plot
closest <- tdf_winners %>% arrange(time_margin) %>%
  slice(1:10)

closestraces <- closest %>% 
  mutate(second = time_overall-time_margin) %>%
  select(c(1:4,6:7,20)) %>%
  mutate(second_rezero = (0-time_margin)*3600,
         image="data files/April7.20.tourdefrance/cyclist.png",
         year = substring(start_date,1,4)) %>%

  ggplot(aes(x=reorder(year,second_rezero),y=second_rezero)) +
  geom_bar(stat="identity",width = .15, fill = "#293036") +
  coord_flip() +
  theme(axis.title.y = element_blank())+
  #geom_text(aes(y=0.0005,label=winner_name),
  #          hjust=0,size=2.5,vjust=0,color=textcol,
  #          nudge_x = .55,angle=7)+
  #geom_image(data=df,aes(x=x,y=y,image=image),size=.03)+
  geom_image(aes(x=reorder(year,second_rezero),
                 y=second_rezero,image=image),
             nudge_x = .3,nudge_y=1.5)+
  geom_image(aes(x=reorder(year,second_rezero),
                 y=-0.0002,image=image),
             nudge_x = .3,nudge_y=-1.5)+
  labs(title="10 Closest Races")+
  scale_y_continuous(breaks=c(-60,-45,-30,-15,0),
                     labels = c("1 Minute","45 Seconds","30 Seconds","15 Seconds",0),
                     name="Time to Second Place")
    

closestraces





# winning margin over time
margins <- tdf_winners %>% tidyr::drop_na(time_margin) %>%
  mutate( year = as.numeric(substring(start_date,1,4))) %>%
  ggplot(aes(x=year,y=time_margin)) +
  geom_point(color="white") +
  geom_smooth( formula = y~x,method="loess",
               color="grey30",fill="grey80")  +
  scale_y_continuous(breaks = c(0,0.16,1,2,3),
                     labels = c(0,"10 Min","1 Hr","2 Hrs","3 Hrs"),
                     name="Winning Margin")+
  scale_x_continuous(name=NULL) +
  coord_cartesian(ylim=c(-.1,3.1),
                  xlim=c(1899,2020),expand=F) + 
  labs(title = "Closer Races Through Time")

margins

library(patchwork)

fullplot <- 
closestraces + (wins /margins) +
  plot_layout(widths = c(1.3, 1))+
  plot_annotation(title = 'Le Tour de France',
                  subtitle = "Analysis of wins through the years",
                  caption = "Data: Alastair Rushworth | Vis: @Jake_Lawlor1",
                 theme= theme(plot.title = element_text(size=28,hjust=.5,family="Montserrat Black"),
                        plot.subtitle = element_text(size=18,hjust=.5),
                        plot.caption = element_text(size=10,hjust=.5)))

fullplot

ggsave(filename = here::here("output","April7.20.tourdefrance","tdf.png"))


  
  
  