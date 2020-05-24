# May somethingth
# volcanoes


library(ggplot2)
library(dplyr)
library(plotly)
library(ggimage)
library(ggrepel)
library(gridGraphics)
library(cowplot)



# load data
volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')
eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')

# view datasets
volcano %>% glimpse()


eruptions %>% glimpse()




# recenter coordinates to center around ring of fire
#=== === === === === === === === === === === === ===
rof2 <- volcano %>% 
  # change lat/lon numbers by 180 to recenter coordinates
  mutate(longfixed = case_when(longitude < 0 ~ 180 + longitude,
                               longitude > 0 ~ longitude - 180)) %>%
  # consolidate volcano types
  mutate(type = case_when(grepl("Caldera",primary_volcano_type) ~ "Caldera",
                          grepl("Strato",primary_volcano_type) ~ "Stratovolcano",
                          grepl("Shield",primary_volcano_type) ~ "Shield",
                          TRUE ~ "Other"))   %>%
  # filter only volcanoes in the pacific / ring of fire
  filter(longfixed > -65 & longfixed < 115 &
           latitude > -75 & latitude < 70) %>%
  select(-contains('rock')) # remove all rock columns
rof2 %>% glimpse()


# find number of explosions 
#=== === === === === === === === === ===
eruptionslist <- eruptions %>%
  group_by(volcano_name,volcano_number) %>%
  summarise(eruptions = n()) %>%
  tidyr::drop_na(eruptions) 
  

# merge rof and eruptions
#=== === === === === === === === === === === ===
joined <- rof2 %>% left_join(eruptionslist,by=c("volcano_name","volcano_number")) %>% 
  tidyr::drop_na(volcano_name) %>%
  filter(longfixed > -65 & longfixed < 115) %>%
  tidyr::drop_na(eruptions) %>%
  mutate(colorbin = case_when(eruptions <= 10 ~ "<10",
                              eruptions > 10 & eruptions <= 50 ~ "10-50",
                              eruptions > 50 & eruptions <= 100 ~ "50-100",
                              eruptions > 100 ~ "100+"),
         colorbin = factor(colorbin, levels=c("<10","10-50","50-100","100+")))

joined %>% group_by(colorbin) %>%
  summarise(count=n())

range(eruptionslist$eruptions)

joined %>% glimpse()



library(sp)
library(rgeos)
library(data.table)

# world country layer
sppolys <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sp")
wgs84 <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# =========================================================================
# Split sPolygonsDF object by "split line"
# =========================================================================
# shift central/prime meridian towards west - positive values only
shift <- 180 

# create "split line" to split worldmap (split at Prime Meridian)
split.line    <- 
  sp::SpatialLines(list
                   (Lines
                     (list
                       (Line(cbind(180 - shift, c(-90, 90)))), 
                       ID="line")
                     ), 
                   proj4string = wgs84)

sppolys <- gBuffer(sppolys, byid=TRUE, width=0)

# intersecting line with world polys 
line.gInt     <- gIntersection(split.line, sppolys)

# create a very thin polygon (buffer) out of the intersecting "split line"
bf            <- gBuffer(line.gInt, byid=TRUE, width=0.000001)  

# split world polys using intersecting thin polygon (buffer)
sppolys.split <- gDifference(sppolys, bf, byid=TRUE)

#==========================================================
## plot 
#==========================================================

countries <- data.table(map_data(as(sppolys.split, 
                                    "SpatialPolygonsDataFrame")))

# Shift coordinates to fall correctly on shifted map
countries$long.shift <- countries$long + shift
countries$long.shift <- ifelse(countries$long.shift > 180, 
                               countries$long.shift - 360, countries$long.shift)

# plot shifted map

#make colors
bluecol = "#354154"
whitecol = "#FCFFF9"
blackcol = "#000000"
redcol = "#9d2222"
orangecol = "#E56800"

# make map
map <- ggplot() + 
  geom_polygon(data=countries, 
               aes(x=long.shift, y=lat, group=group), 
               size = 0.1,
               color = blackcol,
               fill = whitecol) +
  coord_equal(xlim=c(-65,115),
              ylim=c(-75,70))+  
  theme_void()+
  theme(panel.background = element_rect(fill=bluecol,color=orangecol,size=1.5),
        plot.background = element_rect(fill=bluecol),
        plot.margin = margin(10,20,20,20))

# view map
map

# upload png images for volcano types
joined <- joined %>% 
  #mutate(image=here::here("data files","shield.svg"))
  mutate(image = case_when(type == "Caldera" ~ here::here("data files","images","Caldera.png"),
                           type == "Shield" ~ here::here("data files","images","Shield.png"),
                           type == "Other" ~ here::here("data files","images","Other.png"),
                           type == "Stratovolcano" ~ here::here("data files","images","Strato.png")))

 
# make a list of 10 with most eruptions
top10 <- joined %>% arrange(desc(eruptions)) %>%head(10) %>% pull(volcano_name)



# add volcanoes to map
rofmap <- map +
  # all volcanoes
   geom_point(data=joined ,
              inherit.aes = F,
              aes(x=longfixed,
                  y=latitude),
              size=1,
              color=orangecol,
              alpha=.85)+

  #label top 10
  geom_label_repel(data=(joined %>% filter(volcano_name %in% top10)),
                  inherit.aes = F,
                  aes(x=longfixed,
                      y=latitude,
                      label=volcano_name),
                  size=2.5,
                  force = 6,
                  alpha=.95,
                  fill = whitecol,
                  color=blackcol,
                  family="Montserrat Bold")+
  # icon top 10
  geom_image(data=(joined %>% filter(volcano_name %in% top10)),
             inherit.aes = F,
             aes(x=longfixed,
                 y=latitude,
                 image=image),
             size=.045) +
  # labs
  labs(title="The Ring of Fire",
       subtitle = "Pacfic Rim volcanoes with documented eruptions\nIcons denote 10 most active volcanoes",
       caption = "Data: The Smithsonian Institution | Visualization: @Jake_Lawlor1")+
  # theme stuff
  theme(plot.title = element_text(family="Montserrat Black",
                                      color=orangecol,
                                      size=40,
                                      hjust = 0,
                                  margin = margin(5,0,9,0)),
        plot.subtitle = element_text(family="Montserrat",
                                         color=orangecol,
                                         size=16,
                                         hjust=0,
                                     margin = margin(0,0,8,0)),
        plot.caption = element_text(family="Montserrat",
                                     color=whitecol,size=10))

#view map
quartz()
rofmap

ggsave(here::here("output","May11.Volcano","map.png"),
       dpi=600)
# upload ong volcano
img1 <- png::readPNG(here::here("data files","images","Strato_spew.png"))
fig1 <- rasterGrob(img1)


# sort by type
# cal, other, shield, strato
spouts <- joined %>%
  group_by(type,image,type) %>%
  summarise(num = n()) %>%
  ggplot(aes(x=reorder(type,num),y=num)) +
  scale_y_continuous(breaks=c(0,100,200,300),
                     labels = c("0","100","200","300"))+
  coord_cartesian(ylim=c(-38,300),
                  xlim=c(.5,4.5),
                  expand = F)+
  geom_image(inherit.aes=F,aes(x=reorder(type,num),
                               y=c(-16.5,-17,-22.5,-16.5),
                               image=image),
             size=c(.17,.15,.17,.16)) +
  #geom_hline(yintercept = -34)
  geom_segment(aes(x=type,xend=type,y=c(1,2,-8,4),yend=num),
               color=blackcol,size=1.5,
               lineend="round")+
  geom_segment(aes(x=type,xend=type,y=c(1,2,-8,4),yend=num),
               color=redcol,size=1.25,
               lineend="round")+
  geom_text(aes(label=type,x=type,y=num),nudge_y = 16,
            color=whitecol,
             alpha=.8,
            family="Montserrat SemiBold",
            size=2.75)+
  theme_classic()  +
  theme(axis.ticks.x = element_blank(),
        axis.line.x = element_line(color=whitecol),
        axis.title=element_blank(),
        axis.text.x = element_blank(),
        plot.background = element_rect(fill="transparent",color="transparent"),
        panel.background = element_rect(fill="transparent",color="transparent",size=.3),
        axis.line.y=element_line(color=whitecol),
        axis.text.y = element_text(color=whitecol),
        axis.ticks.y = element_line(color=whitecol))+
  labs(title = "Volcanoes by Type") +
  theme(plot.title = element_text(family="Montserrat SemiBold",color=orangecol,size=14))

#view
spouts 




# add things onto map
quartz()
fullplot<-ggdraw() +
  draw_plot(rofmap) +
  draw_plot(spouts, x = 0.45, y = .1, width = .38, height = .37)+
  draw_grob(fig1,x=.7,y=.87,width=.12,height=.1)+
  draw_grob(fig1,x=.76,y=.87,width=.1,height=.08)
fullplot
# sizing of this is weird using quartz.. 
# I had to save with white space on edge, then crop in preview

ggsave(filename = here::here("output","May11.Volcano","ROFmap3.png"),
       dpi=500)
