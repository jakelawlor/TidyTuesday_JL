# Dec 2020 - Washington Trails

# ------------ WARNING: ----------------
# something about this script must be intensive 
# my R sessions aborted a few times during...
# Not sure what the problem is exactly.
# I have been having problems with my graphics devices
# lately, so it could be that... 

library(tidyverse)
library(ggdist)
library(tidytext)
library(here)
library(ggimage)


image <- here("data files","images","hiker.svg")
image2 <- here("data files","images","hiker.png")


#--------



# 1. Prepare Data
#---------------------------------------
hike_data <- readr::read_rds(url('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-24/hike_data.rds'))


# make edited data frams
data <- hike_data %>% 
  
  separate(location, sep = " -- ", into=c("region","specific")) %>%
  select(name, region, rating, description) %>%
  
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
         views = str_to_title(views),
         rating=as.numeric(rating)
  ) %>%
  filter(!word1=="enjoy")%>%
  
  # take most common view types
  group_by(word1) %>%
  add_count() %>%
  
  filter(n>=9) %>% # those with over 10 reviews
  
  group_by(views) %>%
  mutate(mean_rating = mean(rating),
         med_rating = median(rating)) %>%
  ungroup() %>%
  
  mutate(views = fct_reorder(views,mean_rating)) 
#------------


# 1.5 custom functions to find top of curve
#-------------------------------------------------------
# Function to get height of density curve at mean value
dens_at_mean = function(x) { 
  d = density(x,adjust=.25) # adding this adjustment bc that's what I'm using in the plot
  mean.x = mean(x)
  data.frame(mean.x = mean.x,
             max.y = max(d$y),
             mean.y = approx(d$x, d$y, xout=mean.x)$y)
}

# Function to return data frame with properly scaled heights 
#  to plot mean points
get_mean_coords = function(data, value.var, group.var, height) {
  
  data %>% 
    group_by({{group.var}}) %>% 
    summarise(vals = list(dens_at_mean({{value.var}}))) %>% 
    ungroup %>% 
    unnest_wider(vals) %>% 
    # Scale y-value to work properly with stat_halfeye
    mutate(mean.y = (mean.y/max(max.y) * 0.9 * height + 1:n())) %>% # adding height var bc don't want full height plot
    select(-max.y)
}

# Function to get height of density curve at median value
dens_at_med = function(x) { 
  d = density(x,adjust=.25)
  med.x = median(x)
  data.frame(med.x = med.x,
             max.y = max(d$y),
             mean.y = approx(d$x, d$y, xout=med.x)$y)
}

# Function to return data frame with properly scaled heights 
#  to plot mean points
get_med_coords = function(data, value.var, group.var,height) {
  
  data %>% 
    group_by({{group.var}}) %>% 
    summarise(vals = list(dens_at_med({{value.var}}))) %>% 
    ungroup %>% 
    unnest_wider(vals) %>% 
    # Scale y-value to work properly with stat_halfeye
    mutate(mean.y = (mean.y/max(max.y) * 0.9 * height + 1:n())) %>% #adding height var bc I don't want full height plot
    select(-max.y)
}

#-----------------------------


# 2. Make Plot
#-------------------------------------------------------
plot <- data %>% 
  
  ggplot(aes(x=rating,y=views))+

  # ----------- add density ridges -----------
  stat_halfeye(aes(fill = mean_rating),
               geom = "slab",  trim = F, adjust=.25, show.legend=F, height=.8)+
  

  # ----------- add hiker at mean -------------
  ggimage::geom_image(data=get_mean_coords(data, rating, views, 0.8) %>% 
                        mutate(image = image),
                      aes(x=mean.x, y=mean.y,image=image),
                      size=.025, nudge_y = .1) +
  
  # --------  add ratings at means  -----------
  geom_text(data=get_mean_coords(data, rating, views, 0.8), 
             aes(x=mean.x, y=mean.y, label=format(round(mean.x,2))),
            family = "Montserrat Medium",
            size = 2.25,
            nudge_x = -.065,
            nudge_y = .2,
            hjust = 1) +

  
  # ---------  add median points ----------------
  geom_point(data=get_med_coords(data, rating, views, 0.8), 
             aes(x=med.x, y=mean.y + .025),
             color="black", size=.3) +
  
 
  # --------- add ratings at medians -------------
  geom_text(data=get_med_coords(data, rating, views, 0.8), 
            aes(x=med.x, y=mean.y, label=format(round(med.x,2))),
            family = "Montserrat Medium",
            size = 2.25,
            nudge_x = .03,
            nudge_y = .12,
            hjust = 0) +
  
  
  #----------- add segment between -----------------
  geom_segment(data = data.frame(starts=get_mean_coords(data,rating,views, 0.8),
                                 ends = get_med_coords(data,rating,views, 0.8)),
               aes(x = starts.mean.x + .03,
                   y = starts.mean.y +.2,
                   xend = ends.med.x,
                   yend = ends.mean.y + .025 ),
               size=.25)+
  

  # ---------- add horizontal lines -----------------
  geom_segment( data = data %>% group_by(views) %>% slice(1),
    aes(xend = -Inf, yend = views,x = Inf,
        color= mean_rating),
    size = .5,
    show.legend = F
  ) +
  
  # ------------- scales -------------------
  scale_y_discrete(labels = function(views) str_wrap(views, width = 10))+
  scale_x_continuous(breaks = c(0,1,2,3,4,5))+
  scale_fill_gradient(low = "#0c1e27", high = "#5a8ea3") +
  scale_color_gradient(low = "#0c1e27", high = "#5a8ea3") +
  coord_cartesian(xlim=c(-.1,5.15))+

  # ------------- labs ------------------
  labs(title = "Which Washington Trail Views are Best?",
       subtitle = "Washington Trails Association online trail ratings by the most frequently mentioned types of view.",
       y = NULL,
       x = "Trail Rating",
       caption = "Data: WTA | Visualization: @Jake_Lawlor1")+
  
  # ---------- theme stuff ----------------
  ggthemes::theme_hc()+
  theme(panel.background = element_blank(),
        plot.background = element_rect(color="#d3d9db",
                                       fill="#d3d9db"),
        axis.line.x = element_line(size=.25),
        text = element_text(family="Montserrat Light"),
        plot.title.position = "plot",
        plot.title = element_text(size=20, 
                                  family = "Noteworthy Bold",
                                  margin = margin(t=5,b=5)),
        plot.subtitle = element_text(size=8),
        axis.text = element_text(family = "Montserrat SemiBold",
                                 size=8),
        axis.title.x = element_text(size=8),
        plot.caption = element_text(size=6)
        )+
  
  
  # ------------makeshift legend ----------------
  # icon
  geom_image(data=data.frame(x=0.5,y=10.25,image=image2),
             aes(x=x,y=y,image=image))+
  # mean label
  geom_text(data=data.frame(x=0.5,y=10.25,label = "Mean\nRating"),
            aes(x=x,y=y,label=label),
            nudge_x = -.13, nudge_y = .15,
            size=3.25, hjust=1,
            lineheight=.7,
            family= "Montserrat Medium")+
  # add segment
  geom_segment(data = data.frame(x=0.55,xend=1.25,
               y=10.46,yend=10.4),
               aes(x=x,xend=xend,
                   y=y,yend=yend),
               size=.5) +
  # add end point
  geom_point(data = data.frame(x=1.25,y=10.4),
             aes(x=x,y=y),size=1.2) +
  # add median label
  geom_text(data=data.frame(x=1.25,y=10.25,label = "Median\nRating"),
            aes(x=x,y=y,label=label),
            lineheight=.7,
            nudge_x = .05,
            nudge_y = .2,
            hjust=0, vjust=.5, size=3.25,
            family="Montserrat Medium") 
#--------- end plot ----------------


quartz()
plot


ggsave(filename = here("output","Dec.20_WashingtonHikes","hikes4.png"),
       #device=cairo_pdf,
       dpi=300)




