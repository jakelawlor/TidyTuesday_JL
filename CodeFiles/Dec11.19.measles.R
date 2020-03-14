## this week's lesson was that you can make almost any existing plot in R. 
## instead, I wanted to make one visualization in a new way. 



# Step 1. import and clean data
#=== === === === === === === === === === === ===
devtools::install_github("thebioengineer/tidytuesdayR")
tuesdata <- tidytuesdayR::tt_load("2019-12-10")

# there are 4 datasets in here, but I'm only using the disease set for today
diseases <- tuesdata$diseases
rm(tuesdata)

# I want to make a map of measles rates through time
# first, cut data to only measles
library(dplyr)
df <- diseases %>% filter(disease == "Measles")

# add "rate" column (counts per 100,000 people)
df <- df %>% mutate(rate = count / population * 100000 * 52/weeks_reporting)


# Step 2. plot for one year only to see how it will look
#=== === === === === === === === === === === === === ===
library(usmap)
library(ggplot2)
devtools::install_github("jakelawlor/PNWColors")

df28 <- df %>% filter(year == "1928")
plot_usmap(data = df28, values = "rate")+
  scale_fill_gradientn(name = "Measles Rate",colors = PNWColors::pnw_palette("Bay",100))
# great, we have a plot. However, I want the palette to be a little less blue.

# change palette and try again - I just want to make it less blue
pal <- PNWColors::pnw_palette("Bay",100)[20:100] # cuts off the first 20 blue hues
plot_usmap(data = df28, values = "rate")+
  scale_fill_gradientn(name = "Measles Rate",colors = pal)
# there we go.  


# Step 3. Make .pngs for every year
#=== === === === === === === === === === === === === ===
# since I don't know how to add a progress bar to gganimate, 
# I'm going to make a map and a progress bar separately, 
# then attach them with cowplot into .pngs, 
# then stitch into a gif. 


# pull out year info
dlist<-unique(df$year) # list of years
N<-length(dlist) # total number of years


# 3.1 make a map plotting function
#--- --- --- --- --- --- --- --- --- ---
g.map<- function(i=10,d.list=dlist){
  plot_usmap(data=df[df$year %in% c(dlist[i]),],values="rate")+
    scale_fill_gradientn(name="Measles Cases    \nper 100,000",colors = pal,limits=c(0,max(df$rate,na.rm = T)))+
    theme_void()+
    theme(plot.background = element_rect(color="#f5f3f3",fill="#f5f3f3"),
          legend.text  = element_text(family="Helvetica Neue Light"),
          legend.title = element_text(family="Helvetica Neue Light"))
}
# and test it
g.map(45)
# sweet. 

extrafont
# 3.2 make a progress bar function
#--- --- --- --- --- --- --- --- --- ---
g.progress<- function(i=10,maxi=N){
  ggplot(data=data.frame(x="progress",y=i/maxi),aes(x=x,y=y))+
    geom_bar(stat="identity",color=NA,fill="gray47",alpha=0.82)+
    geom_bar(stat="identity", data=data.frame(x="progress",y=1),
             color="black",fill=NA)+
    ggtitle("1928-2003")+
    theme(plot.background = element_rect(color="#f5f3f3",fill="#f5f3f3"),
          panel.background = element_blank(),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_text(hjust = 0,size = 12),
          plot.title = element_text(size =16,family = "Helvetica Neue Light"),
          axis.text.y = element_blank(),
          axis.title = element_blank(),
          legend.background = element_blank(),
          axis.text.x = element_text(family="Helvetica Neue"))+
    geom_hline(yintercept = 36/maxi)+ # add a line when vaccine is introduced (info from example plot https://simplystatistics.org/2019/08/28/you-can-replicate-almost-any-plot-with-ggplot2/)
    scale_y_continuous(limits=c(0,1),breaks = 36/maxi,labels = c("Vaccine Introduced (1963)"))+ # label the line
    coord_flip()
}
# and test it 
g.progress(15)
# great, we got a progress bar ~ 
?plot_grid

# 3.3 Attach into one plot
#--- --- --- --- --- --- --- --- ---
library(cowplot)
plot_grid(g.map(10),g.progress(10), rel_heights=c(5,1),rel_widths = c(1,1),ncol=1)%>%
  ggdraw() +
  theme(plot.background = element_rect(fill="#f5f3f3",color="#f5f3f3")) 
  
# looks great! 

warnings()

# 3.4 make a function to do this for any year
#--- --- --- --- --- --- --- --- ---
plotf<- function(i=10){plot_grid(g.map(i),g.progress(i), rel_heights=c(5,1),ncol=1,rel_widths = c(1,1)) %>%
    ggdraw() +
    theme(plot.background = element_rect(fill="#f5f3f3",color="#f5f3f3"))
          }
# test it
plotf(37)

# 3.5 function to save out all plots
#--- --- --- --- --- --- --- --- ---
plot.save<-function(i=10){
  # add 5000 to index so images are in the right order (10 comes after 9)
  file_path = paste0(mydir, "/plot-",5000+i ,".png")
  ggsave(filename=file_path, plotf(i),
         width = 8, height = 6 , units = "cm",scale=1.85)
  
}

getwd()


# save them out into a directory 
library(purrr)
mydir<-("output/Dec11.19")
map(1:N, plot.save)
# now we have like 76 separate pngs saved that will be each frame of a .gif


getwd()

# Part 4. Convert to .gif
#=== === === === === === === === === ===
# I think you need to have ImageMagick installed to do this, if using on mac. 
# Basically this code is just accessing the ImageMagick app through system(), as far as I understand..
# Installation instructions here: https://imagemagick.org/script/download.php
# I had never used ImageMagick before. Needs to be installed through Terminal.
# Kind of a pain in the butt. 
setwd("output/Dec11.19")
system(command = "convert -delay 20 *.png measlesmap.gif")
# this takes all the .pngs you just created, and stitches them into a gif. 
# change the delay number (20 above) for a slower or faster gif. 

# check in the  folder for example_1.gif. 
# Open in a browser because preview will show it static.  



# Part 5. Delete all the .pngs
#--- --- --- --- --- --- --- --- --- ---
here::here()
for (i in 1:N){
  unlink(here::here("output","Dec11.19",paste0("plot-",5000+i,".png")))
}









