# March 10, 2020
# tidy tuesday, school diversity or something


library(dplyr)
library(ggplot2)
library(plotly)
library(ggrepel)
library(tidyr)
library(ggtext)
library(ggpubr)
library(PNWColors)
library(here)


# set theme
theme_set(theme_classic())

# load data
cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_cost.csv')
income <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_income.csv') 
salary <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/salary_potential.csv')
hist_tuition <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/historical_tuition.csv')
diversity <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/diversity_school.csv')





# find the 50 most expensive in-state price universities
# ended up having to find 57 bc some don't match below
priciest57 <- cost %>% arrange(desc(in_state_total))  %>% slice(1:57) %>% pull( name)



df2 <- diversity %>% 
  filter(category %in% c("White",
                         "Black",
                         "Hispanic",
                         "Asian",
                         "American Indian / Alaska Native",
                         "Native Hawaiian / Pacific Islander")) %>%
  filter(name %in% priciest57) %>%
  mutate(name = factor(name, levels = rev(priciest57)))





# so now lets do this proportionally
# (recalculating bc there were loads of "unknowns" in the real data)
df3 <- df2 %>% select(-total_enrollment) %>%
  pivot_wider(names_from = category, values_from = enrollment) %>%
  mutate(sum = rowSums(.[3:8])) %>%
  pivot_longer(names_to = "Category",cols = c(3:8)) %>%
  mutate(prop = value/sum*100) %>%
  mutate(Category = factor(Category,
                           levels=rev(c("White","Asian","Hispanic","Black","Native Hawaiian / Pacific Islander","American Indian / Alaska Native")))) %>%
  mutate(Category = case_when(
    Category == "Native Hawaiian / Pacific Islander" ~ "Native Hawaiian/\nPacific Islander",
    Category == "American Indian / Alaska Native" ~ "American Indian/\nAlaska Native",
    TRUE ~ as.character(Category)
  ))

# order by % white
pricy_order <- df3 %>% 
  filter(Category %in% c("White")) %>%
  arrange(prop) %>%
  pull(name)



pricy <- df3 %>% 
  ggplot(aes(x=prop,y=factor(name,levels=pricy_order),fill=Category))+
  geom_bar(position = "stack",stat="identity",color="grey20",size=.1)+
  theme(axis.title = element_blank())+
  coord_cartesian(xlim = c(0,100),expand = F)



# ok, can we do the same with the 50 cheapest schools?
# again, it took 57 to get 50 matches
cheap57 <- cost %>% 
  arrange((in_state_total))  %>% 
  slice(1:57) %>% pull( name)

# pull out diversity categories
df2_cheap <- diversity %>% 
  filter(category %in% c("White",
                         "Black",
                         "Hispanic",
                         "Asian",
                         "American Indian / Alaska Native",
                         "Native Hawaiian / Pacific Islander")) %>%
  filter(name %in% cheap57) %>%
  mutate(name = factor(name, levels = cheap57))


# make into proportions 
df3_cheap <- df2_cheap %>% 
  select(-total_enrollment) %>%
  pivot_wider(names_from = category, values_from = enrollment) %>%
  mutate(sum = rowSums(.[3:8])) %>%
  pivot_longer(names_to = "Category",cols = c(3:8)) %>%
  mutate(prop = value/sum*100) %>%
  mutate(Category = factor(Category,
                           levels=rev(c("White","Asian","Hispanic","Black","Native Hawaiian / Pacific Islander","American Indian / Alaska Native")))) %>%
  mutate(Category = case_when(
    Category == "Native Hawaiian / Pacific Islander" ~ "Native Hawaiian/\nPacific Islander",
    Category == "American Indian / Alaska Native" ~ "American Indian/\nAlaska Native",
    TRUE ~ as.character(Category)
  ))



cheap_order <- df3_cheap %>% 
  filter(Category %in% c("White")) %>%
  arrange(prop) %>%
  pull(name)


cheap <- df3_cheap %>% 
  ggplot(aes(x=prop,y=factor(name,levels=cheap_order),fill=Category))+
  geom_bar(position = "stack",stat="identity",color="grey20",size=.1) + 
  theme(axis.title = element_blank()) +
  coord_cartesian(xlim = c(0,100),expand = F)


# merge and fix aesthetics
#=== === === === === === === === === ===
pal <- pnw_palette("Winter",6)
bgcol <- "white"
family <- "Tw Cen MT Std Semi Medium"
familytitles <- "Gill Sans MT Bold"
fontcol <- "#0b060f"

pricy2 <-pricy +
  labs(title = "Highest Tuition Schools",
       subtitle = "Ordered by % white students")+
  scale_fill_manual(values=pal)+
  scale_x_continuous(breaks = c(0,25,50,75,100),
                     labels = c("0%","25%","50%","75%","100%"))+
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.key.size = unit(.5,"cm"),
        legend.text = element_text(size=9,family=family),
        plot.background = element_rect(fill=bgcol,color=bgcol),
        panel.background = element_rect(fill=bgcol,color=bgcol),
        plot.margin = margin(0, 10, 0, 5),
        axis.text = element_text(family=family,color=fontcol,size=8),
        panel.grid = element_blank(),
        plot.title = element_text(family = familytitles,
                                     hjust=.5,color=fontcol,
                                     size=12),
        plot.subtitle = element_text(family=family,
                                     hjust=.5,color = fontcol,
                                     size=8))+
  guides(
    fill = guide_legend(
      nrow = 1,
      reverse = T )
  ) 
  

cheap2<-cheap +
  labs(title = "Lowest Tuition Schools",
       subtitle = "Ordered by % white students")+
  scale_fill_manual(values=pal)+
  scale_x_continuous(breaks = c(0,25,50,75,100),
                     labels = c("0%","25%","50%","75%","100%"))+
  scale_y_discrete(position="right")+
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.key.size = unit(.5,"cm"),
        legend.text = element_text(family = family,size=9),
        plot.background = element_rect(fill=bgcol,color=bgcol),
        panel.background = element_rect(fill=bgcol,color=bgcol),
        plot.margin = margin(0, 5, 0, 10),
        axis.text = element_text(family=family,color=fontcol,size=8),
        panel.grid = element_blank(),
        plot.title = element_text(family = familytitles,
                                     hjust=.5,color=fontcol,
                                     size=12),
        plot.subtitle = element_text(family=family,
                                     hjust=.5,color = fontcol,
                                     size=8))+
  guides(
    fill = guide_legend(
      nrow = 1,
      reverse = T
    )
  ) 

# merge two plots into one
plots <- ggpubr::ggarrange(pricy2,cheap2,nrow=1,common.legend = T)


# make title in separate plot
titleplot <- ggplot(data.frame(x = 1, y = 1)) +
  labs(x = NULL, y = NULL,
       title = "Mind the Gap",
       #subtitle = "Relative proportion of students identifying as <br>**<b style='color:#ECECEC'>White</b>**, **<b style='color:#96B6B9'>Asian</b>**, **<b style='color:#658892'>Hispanic</b>**, **<b style='color:#46606C'>Black</b>**, **<b style='color:#313F46'>Pacific Islander</b>**, and **<b style='color:#2D2926'>American  Indian</b><br>** in the 50 highest and 50 lowest tuition colleges in the US<br>"
       subtitle = "Proportional representation of racial groups in student populations\nat the 50 highest total-price and 50 lowest total-price colleges in the United States") +
#  theme_void() + 
  theme(plot.title = element_text(family = familytitles, size = 18, 
                                      lineheight = 1.1, 
                                      hjust = .5, 
                                      ),
        plot.subtitle = element_text(size = 10, 
                                         color = fontcol, 
                                         hjust = .5,
                                     family=family
                                        ),
        plot.margin = margin(20, 50, 0, 50),
        plot.background = element_rect(fill=bgcol,color=bgcol),
        panel.background = element_rect(fill=bgcol,color=bgcol))


# make caption in separate plot
captionplot <- ggplot(data.frame(x = 1, y = 1)) +
  labs(x = NULL, y = NULL,
       caption = "Data from Chronical of Higher Ed | Visualization by @Jake_Lawlor1"
       ) +
  #  theme_void() + 
  theme(plot.caption = element_text(size = 8, 
                                   color = fontcol, 
                                   hjust = .5, 
                                   # lineheight = 1.4,
                                   family= family
  ),
  plot.margin = margin(0,0, 8, 0),
  plot.background = element_rect(fill=bgcol,color=bgcol),
  panel.background = element_rect(fill=bgcol,color=bgcol))


# stitch all together
fullplot<- plot_grid(titleplot,plots,captionplot,ncol = 1,rel_heights = c(.14,.85,.05))

#view and size
quartz()
fullplot

#save
ggsave(filename = here("output","Mar10.20.Diversity","diverse.png"),
       dpi=300)
