# Tidytuesday week 9 2021 - employment status




# Load data and Libraries -------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2021, week = 9)
employed <- tuesdata$employed
earn <- tuesdata$earn
rm(tuesdata)

library(tidyverse)
library(extrafont)
library(cowplot)
font_import()
loadfonts()




# Plot 1 without Hispanic / Latino ----------------------------------------
earn %>% group_by(race,ethnic_origin) %>% count()
# the race / ethnicity categories are a bit confusing in this dataset
# it shows Asian, Black, and White, and "All Races" as distinct,
# then within "All races", there is a subset where ethnic origin is Hispanic / Latino.
# All races / all origins seems to be a combination of all groups, 
# whereas all races / hispanic and latino seems to be all hispanic and latino people,
# no matter their racial identity. Not sure if I should use this as a distinct group or not. 
# see next plot below. 


pal = c("#6da4bc","#EC111A","#116aa0","#4d8fb8")
p <- earn %>% 
  
  group_by(race,year) %>%
  summarise(pop = mean(n_persons),
            earn = mean(median_weekly_earn)) %>%
  ggplot(aes(x=year,y=earn,color=race)) +
  geom_line(size=1,show.legend = F)+
  
  geom_text(data = data.frame(race = c("All Races","Asian","Black or African American","White"),
                              label = c("All","Asian","Black","White"),
                              x = c(2019, 2019, 2018, 2018.5),
                              y = c(790, 1170, 650, 935)),
            aes(x=x,y=y,label=label),
            family = "Open Sans Extrabold",
            fontface = "bold",
            size=5.5,
            show.legend = F)+
  
  
  scale_y_continuous(labels=scales::dollar_format(),
                     position = "right")+
  scale_x_continuous(labels=c(2010,2012,2014,2016,2018,2020),
                     breaks = c(2010,2012,2014,2016,2018,2020))+
  scale_color_manual(values = pal)+

  labs(x=NULL,y=NULL,
       title ="American wages are increasing,\nbut Asian workers are seeing biggest gains",
       subtitle  = "Average weeky income of American workers",
       caption = c("Source: Bureau of Labor Statistics","@Jake_Lawlor1"))+
  
  ggthemes::theme_few()+
  theme(panel.border  = element_blank(),
        panel.grid.major.y =  element_line(linetype = "dashed", color = "grey70",size=.25),
        plot.title = element_text(color="#EC111A", family = "Open Sans Condensed Bold",size=25),
        plot.subtitle = element_text(size=14,margin = margin(t=30)),
        text = element_text(family="Open Sans"),
        axis.text = element_text(size=14, family="Open Sans"),
        plot.caption = element_text(hjust=c(0, 1),color="#B6B6B6",size=14,margin = margin(t=10)),
        plot.caption.position = "plot",
        plot.margin = margin(40,20,20,20,"pt"),
        axis.line.x = element_line(size=.5),
        plot.background = element_blank())

p




# Plot 2 ------------------------------------------------------------------
p2<- earn %>% 
  filter(!(race %in% c("All Races") & ethnic_origin %in% c("All Origins"))) %>% 
  mutate(race = case_when(race == "Black or African American" ~ "Black",
                          race == "All Races" & ethnic_origin == "Hispanic or Latino" ~ "Hispanic\nor Latino",
                          TRUE ~ race)) %>% 
  mutate(race = factor(race, levels = c("Black","Hispanic\nor Latino","White","Asian"))) %>%
  group_by(race,sex,year) %>%
  summarize(earn = mean(median_weekly_earn)) %>%
  filter(!sex == "Both Sexes") %>%
  pivot_wider(names_from = sex, values_from =earn) %>%
  mutate(diff= Men-Women) %>%

  # start plot
  ggplot(aes(x=year,y=diff,fill=race,color=race))+
  
  # add geoms
  geom_line(show.legend = F,size=1)+
  geom_area(alpha=.5,show.legend = F)+
  #geom_smooth(method="lm",aes(color=race),
  #            se=F,linetype="dashed")+
  coord_cartesian(ylim = c(0,300), expand = F) +
  
  # scales
  scale_fill_manual(values = alpha(pal[c(2,3,4,1)],.3)) +
  scale_color_manual(values = pal[c(2,3,4,1)]) +
  scale_y_continuous(position = "right",
                     breaks=  c(0,100,200,300),
                     #labels = c("No Gap", "$100","$200","$300"),
                     labels = scales::dollar_format()) +
  scale_x_continuous(breaks = c(2010,2015,2020),
                     labels = c(2010,2015,2020))+
  
  # add text
  geom_text(data = data.frame(race = c("Black","White","Hispanic\nor Latino", "Asian"),
                              label = c("Black","White","Hispanic\nor Latino", "Asian"),
                              x = 2015,
                              y = c( 80, 200, 120, 270 )) %>%
              mutate(race = factor(race, levels = c("Black","Hispanic\nor Latino","White","Asian"))),
            aes(x=x,y=y,label=label,color=race),
            family = "Open Sans Extrabold",
            fontface = "bold",
            size=5.5,
            lineheight=.7,
            show.legend = F)+
  
  # add labels
  labs(x=NULL,y=NULL,
       title = "However, Asian women are getting left behind",
       subtitle = "Average weekly additional earnings of male workers\ncompared to women of the same racial or ethnic group",
       caption = c("Source: Bureau of Labor Statistics","@Jake_Lawlor1"))+
  
  # theme stuff
  ggthemes::theme_few()+
  theme(panel.border  = element_blank(),
        panel.grid.major.y =  element_line(linetype = "dashed", color = "grey70",size=.25),
        plot.title = element_text(color="#EC111A", family = "Open Sans Condensed Bold",size=25),
        plot.subtitle = element_text(size=14,margin = margin(t=30,b=-20)),
        text = element_text(family="Open Sans"),
       # axis.title.y=element_text(position="top"),
        axis.text.x = element_text(size=9, hjust = c(0.1,.5,.9)),
        axis.text.y = element_text(size=10, hjust = c(0.1,.5,.9)),
       
        axis.text = element_text(size=14, family="Open Sans"),
        plot.caption = element_text(hjust=c(0, 1),color="#B6B6B6",size=14,margin = margin(t=10)),
        plot.caption.position = "plot",
        plot.margin = margin(40,20,20,20,"pt"),
        axis.line.x = element_line(size=.5),
        plot.background = element_blank(),
        strip.text = element_blank())+
  facet_wrap(~race,ncol=4)

p2


# Plot 1 with Hispanic/Latino ---------------------------------------------
pal = c("#EC111A","#6da4bc","#116aa0","#4d8fb8")

p <- earn %>% 
  filter(!(race %in% c("All Races") & ethnic_origin %in% c("All Origins"))) %>% 
  mutate(race = case_when(race == "Black or African American" ~ "Black",
                          race == "All Races" & ethnic_origin == "Hispanic or Latino" ~ "Hispanic\nor Latino",
                          TRUE ~ race)) %>% 
  group_by(race,year) %>%
  summarise(pop = mean(n_persons),
            earn = mean(median_weekly_earn)) %>%
  ggplot(aes(x=year,y=earn,color=race)) +
  geom_line(size=1,show.legend = F)+
  
  geom_text(data = data.frame(race = c("Hispanic\nor Latino",   "Asian", "Black", "White"),
                              label = c("Hispanic\nor Latino",  "Asian", "Black", "White"),
                              x =    c(2018.5,                     2019,    2019.25,  2018.5),
                              y =    c(620,                      1170,    790,     935)),
            aes(x=x,y=y,label=label),
            family = "Open Sans Extrabold",
            fontface = "bold",
            size=c(5,5.5,5.5,5.5),
            show.legend = F,
            lineheight=.7)+
  
  
  scale_y_continuous(labels=scales::dollar_format(),
                     position = "right")+
  scale_x_continuous(labels=c(2010,2012,2014,2016,2018,2020),
                     breaks = c(2010,2012,2014,2016,2018,2020))+
  scale_color_manual(values = pal)+
  
  labs(x=NULL,y=NULL,
       title ="American wages are increasing,\nbut Asian workers are seeing biggest gains",
       subtitle  = "Average weeky income of American workers",
       caption = c("Source: Bureau of Labor Statistics","@Jake_Lawlor1"))+
  
  ggthemes::theme_few()+
  theme(panel.border  = element_blank(),
        panel.grid.major.y =  element_line(linetype = "dashed", color = "grey70",size=.25),
        plot.title = element_text(color="#EC111A", family = "Open Sans Condensed Bold",size=25),
        plot.subtitle = element_text(size=14,margin = margin(t=35), family = "Open Sans Semibold"),
        text = element_text(family="Open Sans"),
        axis.text = element_text(size=14, family="Open Sans"),
        plot.caption = element_text(hjust=c(0, 1),color="#B6B6B6",size=14,margin = margin(t=10)),
        plot.caption.position = "plot",
        plot.margin = margin(40,20,20,20,"pt"),
        axis.line.x = element_line(size=.5),
        plot.background = element_blank())

p



# Make economist theme ----------------------------------------------------
topline <- 
  ggplot()+
  geom_segment(aes(x=0,xend=10,
                   y=0,yend=0,),
               color="#EC111A") +
  geom_rect(aes(xmin=0, xmax=1.,
                ymin=0,ymax=.2),
            color="#EC111A",
            fill="#EC111A") +
  coord_cartesian(xlim = c(0,10),expand = F,
                  ylim = c(-10,.3))+
  theme(plot.margin = margin(20,20,0,20,"pt"))+
  scale_y_continuous(position = "right")+
  theme_void() + 
  theme(plot.margin = margin(c(15,20,20,20)))



# Combine and save  -------------------------------------------------------
# Plot 1
fullplot1<-ggdraw(topline) +
  draw_plot(p, 
            x=0,
            y=0, 
            width=1, 
            height=1,
            hjust = 0, 
            vjust = 0) 


ggsave(fullplot1,
       filename = here::here("output","Feb23.21.workers","plot1.2.png"),
       height = 7,
       width = 7,
       units = "in",
       dpi=300)

# plot 2
fullplot2 <- ggdraw(topline) +
  draw_plot(p2, 
            x=0,
            y=0, 
            width=1, 
            height=1,
            hjust = 0, 
            vjust = 0) +
  annotate(geom = "segment",
           x=.50, xend = .50,
           y=.12, yend = .455,
           arrow = arrow(length=unit(0.2,"cm"), ends="both", type = "closed",),
           alpha=.5)+
  annotate(geom = "text",
           x=.51,
           y=.13,
           label = "Female Wage",
           family = "Open Sans",
           hjust=0,vjust=0,
           size=3)+
annotate(geom = "text",
         x=.51,
         y=.435,
         label = "Male Wage",
         family = "Open Sans",
         hjust=0,vjust=0,
         size=3)

ggsave(fullplot2,
       filename = here::here("output","Feb23.21.workers","plot2.png"),
       height = 7,
       width = 7,
       units = "in",
       dpi=300)







# Other random plots ------------------------------------------------------
# industry by percent race
employed %>% 
  filter(!industry %in% c("Asian","Black or African American","White","Men","Women")) %>%
  filter(!race_gender %in% c("Men","Women","TOTAL")) %>%
  group_by(industry,race_gender,year) %>% 
  summarise(total=unique(industry_total)) %>% 
  ungroup() %>%  
  pivot_wider(names_from   = race_gender,
              values_from=total) %>%
  rename("Black" = 'Black or African American') %>%
  mutate(total = Asian + Black + White,
         percent_asian = Asian/total*100,
         percent_black = Black/total*100,
         percent_white = White/total*100) %>%
  filter(year == 2020) %>%
  pivot_longer(cols = c(Asian, Black, White), names_to = "race") %>%
  select(-percent_asian,-percent_black,percent_white) %>%
  mutate(percent = value/total*100) %>%
  #pivot_longer(cols = c(percent_asian,percent_black,percent_white),names_to = "percent",values_to = "value2")
  ggplot(aes(x=reorder(industry,-percent_white),y=percent,color,fill=race))+
  geom_col() +
  coord_flip()




earn %>%
  #filter(!age %in% c("16 years and over","25 years and over","55 years and over","65 years and over","25 to 54 years")) %>% 
  filter(age %in% c( "16 years and over" ,"16 to 24 years" ,   "25 years and over", "25 to 54 years" , "55 years and over")) %>% 
  
  # mutate(age = case_when(age %in% c("16 to 24 years","16 to 19 years","20 to 24 years") ~ "16-24",
  #                        age %in% c("25 to 34 years") ~ "25-34",
  #                        age %in% c("35 to 44 years") ~ "35-44",
  #                        age %in% c("45 to 54 years") ~ "45-54",
  #                        age %in% c("55 to 64 years") ~ "55-64",
  #                        TRUE ~ age)) %>%
  
  mutate(age_numeric = case_when(age == "16 to 24 years" ~ 0,
                                 age ==  "16 years and over" ~ 1,
                                 age ==  "25 years and over" ~ 2,
                                 age == "25 to 54 years" ~ 0,
                                 age ==  "55 years and over" ~ 3
  )) %>%
  filter(age_numeric > 0) %>%
  group_by(age,age_numeric,race,ethnic_origin,year) %>%
  summarise(mean = mean(median_weekly_earn) ) %>%
  ungroup() %>%
  ggplot(aes(x=age_numeric,y=mean,color=interaction(race,ethnic_origin)))+
  geom_point() +
  geom_line()+
  facet_wrap(~year,nrow=1) +
  theme(legend.position = "top")



# wage growth my sex and race
earn %>% #filter(race == "Black or African American") %>%
  group_by(year,sex,race) %>%
  summarise(earn = mean(median_weekly_earn)) %>% 
  ungroup() %>% 
  filter(sex %in% c("Men","Women")) %>%
  ggplot(aes(x=year,y=earn,color=sex)) +
  geom_line() +
  facet_wrap(~race) +
  ggthemes::theme_economist()


# gender gap change through time

# ok lets try for the gap
earn %>% 
  filter(!age %in% c("16 years and over", "16 to 19 years")) %>%
  filter(year %in% c(2010,2020),
         sex %in% c("Men","Women")) %>%
  group_by(sex,race,year) %>%
  summarize(earn = mean(median_weekly_earn)) %>%
  ungroup() %>%
  pivot_wider(names_from = sex,values_from = earn) %>%
  mutate(percent = Women/Men*100) %>%
  ggplot(aes(x=year,y=percent,color=race))+
  geom_path()+
  geom_point()+
  scale_x_continuous(breaks = c(2010,2020), limits = c(2010, 2020)) +
  ggthemes::theme_few()+
  theme(panel.border  = element_blank(),
        panel.grid.major =  element_line(linetype = "dashed", color = "grey70",size=.25))

