### Feb 18 Tidy Tuesday
## CO2 emissions and food?
library(dplyr)
library(ggplot2)
library(ggtext)


food_consumption <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')


food_consumption %>% glimpse()




## going to make a polar bar plot. First have to make labels
#--- --- --- --- --- --- --- --- ---
label_data2 <- food_consumption %>% group_by(country) %>%
  summarise(sum=sum(co2_emmission)) %>%
  arrange(desc(sum))
label_data2$id <- c(1:nrow(label_data2))


# calculate the ANGLE of the labels
number_of_bar <- nrow(label_data2)
angle <-  90 - 360 * ( label_data2$id-.5 )  /number_of_bar    

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
label_data2$hjust<-ifelse( angle < -90, 1, 0)

# flip angle to make them readable
label_data2$angle<-ifelse(angle < -90, angle+180, angle)

# find order of countries to use for scale_x, turn into factor
order <- label_data2$country
order <- c(order)
order <- factor(order, levels=order)





# Create small df to add special annotation using geom_richtext (ggtext)
# this took some trial and error to get the position right
df <- data.frame(
  label = c(
    "Carbon emissions released due to<br> <span style = 'color:#e76a6a'>**Meat**</span>, <span style = 'color:#efefe8'>**Dairy & Eggs**,</span> & <span style = 'color:#6dac4f'>**Nuts & Grains**</span><br>per person per year in 130 countries"  ),
  x = c(65),
  y = c(-1100),
  hjust = c(0.5),
  vjust = c(1),
  angle = c(0),
  color = c("grey90")
)



# make color palette. Going to arrange into Meat, Dairy, & Plants,
# so I chose a nice pink, milky white, and green
pal <- c("#e76a6a","#efefe8","#6dac4f")


quartz()

# Make the Plot
#--- --- --- --- --- --- --- --- --- ---
food_consumption %>%
  group_by(country) %>%
  
  #group into category types & order levels
  mutate(food_category2 = case_when(food_category %in% c("Beef","Fish","Lamb & Goat","Pork","Poultry") ~ "Meat",
                                    food_category %in% c("Eggs","Milk - inc. cheese") ~ "Eggs & Dairy",
                                    food_category %in% c("Nuts inc. Peanut Butter","Rice","Soybeans","Wheat and Wheat Products") ~ "Nuts & Grains"
                                    )) %>%
  mutate(food_category2 = factor(food_category2,levels = c("Meat","Eggs & Dairy","Nuts & Grains"))) %>%
  
  # start ggplot
  ggplot( aes(fill=food_category2, y=co2_emmission, x=country)) + 
  
  #add bars, order, color
  geom_bar(position="stack", stat="identity") +
  scale_x_discrete(limits=order)+
  scale_fill_manual(values=pal,guide=F)+
  
  # add ylim to keep empty circle in middle
  ylim(-1500,2250) +
  
  # remove axes and turn polar
  theme_void() +
  coord_polar(direction = 1,
              clip = "off") +
  
  # add country labels
  geom_text(data=label_data2, aes(x=id, y=sum+50, label=country, hjust=hjust), 
            color="grey90", fontface="bold",alpha=0.6, size=2.5, 
            angle= label_data2$angle, inherit.aes = FALSE )  +
  
  # add annotation in middle with colored words
  geom_richtext(inherit.aes = F, data=df,
                aes(x,y,label=label,hjust=hjust),
                fill=NA, label.color=NA,size=2.75,
                family="Helvetica Neue",color="grey90")+
  
  # annotate title in middle
  annotate(geom = "text",
           x=0,y=-850,
           hjust=.5, vjust=1,
           label="How Carbon Intensive \nis Your Diet?",
           size=5.5, lineheight=.8,
           family="Staatliches Regular",
           color="grey90")+
  
  # annotate caption (real caption was too low)
  annotate(geom = "text",
           x=65,y=2200,
           vjust=1,
           hjust=.5,
           label = "Data: FAO | Visualization: @Jake_Lawlor1",
           size=2.5,
           color="grey90")+
  
  # annotate one scale value. 
  annotate(geom="text",
           x=0.3,y=2000,
           label="2000 Kg CO2-",
           hjust=1,
           size=1.5,
           color="grey90",
           alpha=.7)+
  
  #background fill
  theme(plot.background = element_rect(fill="#516869")) 
  



#save
ggsave(filename = here::here("output","Feb18.20","food_white.png"),dpi=300)


dev.off()



