# Shelter Dog data from Sep 20, 2019
# #TidyTuesday Dec 17, 2019

# Step 0. Load Libraries
#--- --- --- --- --- --- --- --- --- ---
library(tidyverse)
devtools::install_github("clauswilke/ggtext")
library(cowplot)
devtools::install_github("wmurphyrd/fiftystater")
library(fiftystater) # states
devtools::install_github("jakelawlor/PNWColors")
library(PNWColors) # colors


# Step 1. Load Data
#--- --- --- --- --- --- --- --- --- ---
# moves <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_moves.csv')
# travel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_travel.csv')
descriptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_descriptions.csv')


# Step 2. Set Theme
#--- --- --- --- --- --- --- --- --- ---
theme_set(theme_void(base_size = 12))
theme_update(plot.background   =element_rect(fill = "grey15"),
                     panel.background = element_blank(),
                     plot.title = element_text(size = rel(2.3), 
                                               family = "Helvetica Neue Thin",
                                               hjust = 0.5,
                                               color="grey75"),
                    plot.subtitle = element_text(size = rel(1), 
                                              family = "Helvetica Neue Thin", 
                                              color = "grey75", 
                                              lineheight = 1.4,
                                              hjust = 0.5),
                     plot.caption = element_text(size = rel(.75), 
                                                 family = "Bitter", 
                                                 color = "grey50"),
             legend.position = "bottom",
             legend.justification = "center",
             legend.text = element_text(size = rel(1),
                                        family = "Helvetica Neue Thin",
                                        color="grey75")
)


# Step 3. Build tools
#--- --- --- --- --- --- --- --- --- ---
names(state.abb) <- state.name # connect state abb and state name. will use later.
names(state.name) <- state.abb # connect state abb and state name. will use later.
Mode <- function(x) { #create "mode" function
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
pal <- pnw_palette("Winter",6)


# Step 4. Clean and plot
#--- --- --- --- --- --- --- --- --- ---
descriptions %>% 
  filter(contact_state %in% state.abb) %>% # remove those with atypical state codes
  group_by(contact_state) %>% 
  summarise( total = n(),  # number per state
             breed = Mode(breed_primary)) %>%  #mode of breed
  rename(state = contact_state) %>% 
  filter(!is.na(breed)) %>%  #remove no breed dogs
  mutate(id = tolower(state.name[state])) %>% # match state abb to state name and change to lower case
  left_join(fifty_states,by="id") %>% # 
  ggplot(aes(x = long, y = lat,
             group = group, fill = breed))+
  geom_polygon(color="black")+
  scale_fill_manual(values = (pal),name=NULL)+
  coord_map(projection = "albers",lat0=41,lat1=45,ylim = c(23.5,48))+
  guides(fill = guide_legend(nrow=1))+
  labs(x = NULL, y = NULL,
       title = "Adoptable Dog Breeds",
       subtitle = "Most common primary breed of shelter dogs per state on Sep 20, 2019")+
  theme(plot.margin = unit(c(5,0,5,0), "points"))



# Step 5. save
#--- --- --- --- --- --- --- --- --- ---
ggsave(here::here("output","Dec17.19","breedmap.png"), height = 5,width = 8.2)





# PLOT 2 -- interstate export circle diagram
#==========================================================

library(tidyverse)
library(countrycode)
library(circlize)


#=== === === === == === === === === === === === === === === === === === ===
# Most of this cleaning code is from conkline on GitHub. 
# I saw their nice #TidyTuesday on twitter and wanted to make a similar one.
# https://github.com/conkline/TidyTuesdayScripts/blob/master/tidytuesday_121719_dogs.R
#=== === === === == === === === === === === === === === === === === === ===


dog_travel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_travel.csv')
dog_descriptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_descriptions.csv')

#maybe look at what kinds of dog are imported/exported?
#join the two datasets

dogs_total <- dog_travel %>%
  select(-c(contact_city, contact_state, description)) %>% 
  left_join(dog_descriptions, by="id")
rm(dog_travel,dog_descriptions)

#messy - lots of repeats/missing data - try to clean up
dogs_cleaned <- dogs_total %>% distinct() %>% #keep only distinct rows
  group_by(id) %>% 
  mutate(id_count = n()) %>% #count # of times this ID shows up
  ungroup() %>%
  filter(id_count == 1 | (id_count >= 2 & !is.na(manual))) %>% #if we have repeats, keep only those manually entered
  select(-c(id_count, color_tertiary, house_trained,
            declawed, shots_current, tags, photo,
            status, accessed, type, stateQ, 
            breed_unknown, still_there)) %>% #we don't need id_count anymore, also remove other variables we don't need
  filter(!grepl("foster", tolower(name))) %>% #some are ads for foster homes needed - remove
  arrange(id) #order by #ID
rm(dogs_total)

#final step of filtering - if multiple id matches, keep only the first
lastid <- 0
idx_to_remove <- c()

for (i in 1:nrow(dogs_cleaned)){
  
  thisid <- dogs_cleaned$id[i]
  if (thisid == lastid){
    idx_to_remove <- c(idx_to_remove, i)
  }
  
  lastid <- thisid
}

dogs_cleaned <- dogs_cleaned[-idx_to_remove,]#remove these repeats
rm(i,lastid,idx_to_remove)


#clean up place of origin - recognizable state names/abbrevations or country names
dogs_cleaned <- dogs_cleaned %>%
  filter(found %in% state.name | found %in% state.abb |
           manual %in% state.name | manual %in% state.abb |
           found %in% codelist$country.name.en |
           manual %in% codelist$country.name.en | 
           found == "DC" |
           manual == "DC")

#consolidate origin into one column
dogs_cleaned$origin <- dogs_cleaned$manual
dogs_cleaned[which(is.na(dogs_cleaned$manual)),]$origin <- dogs_cleaned[which(is.na(dogs_cleaned$manual)),]$found
dogs_cleaned <- dogs_cleaned %>% select(-c(manual, found))

#convert abbreviations to state names
dogs_cleaned$rescue_location <- state.name[match(dogs_cleaned$contact_state, state.abb)]
dogs_cleaned[which(dogs_cleaned$contact_state == "DC"),]$rescue_location <- "Washington DC"
dogs_cleaned[which(dogs_cleaned$contact_country == "KY"),]$rescue_location <- "Kentucky"

for (i in 1:nrow(dogs_cleaned)){
  if (dogs_cleaned$origin[i] %in% state.abb){
    dogs_cleaned$origin[i] <- state.name[match(dogs_cleaned$origin[i], state.abb)]
  }
  if (dogs_cleaned$origin[i] == "DC"){
    dogs_cleaned$origin[i] = "Washington DC"
  }
}

#=== === === === == === === === === === === === === === === === === === ===
# End conkline code.
#=== === === === == === === === === === === === === === === === === === ===
rm(dog_descriptions,dog_travel,i,idx_to_remove,thisid,lastid)

dogs_cleaned <- dogs_cleaned %>% 
  select(c(id,breed_primary,size,origin,contact_country,rescue_location)) %>%
  filter(origin %in% state.name) # pull only those originating from US states

exports <- data.frame(table(dogs_cleaned$origin, dogs_cleaned$rescue_location))
# this makes a df of moves from state to state, we will use it to make a circular export graph

# Circle Graph Making
#=== === === === === === === === === === === === === === === === === === ===

## Separate states into regions
#--- --- --- --- --- --- --- --- --- --- ---
{
  northeast <- c("Connecticut","Maine","Massachusetts","New Hampshire","Rhode Island","Vermont","New Jersey","New York","Pennsylvania")
  midwest <- c( "Indiana", "Illinois", "Michigan", "Ohio", "Wisconsin", "Iowa","Nebraska", 
                "Kansas", "North Dakota", "Minnesota", "South Dakota", "Missouri")
  southatlantic <- c("Maryland","Delaware","Washington DC","West Virginia","Virginia","North Carolina","South Carolina","Georgia","Florida")
  southgulf <- c("Kentucky","Tennessee","Alabama","Mississippi","Arkansas","Louisiana","Oklahoma","Texas")
  west <- c("Arizona","Colorado","Idaho","New Mexico","Montana",
            "Utah","Nevada","Wyoming","Alaska","California","Hawaii","Oregon","Washington")
}
statelist <- c(northeast,southatlantic,southgulf,midwest,west) # order them this way, rather than alphabetically

# color tools 
#--- --- --- --- --- --- --- --- --- --- --- ---
pal <- pnw_palette("Starfish",5) # build a palette
totalsegs <- unique(abind::abind(unique(exports$Var1),unique(exports$Var2)))
grid.col <- c( # make a vector colors of each state (colored by region). Not all states listed show up in segments, so this was kind of weird
  rep( pal[1], times = length(totalsegs[totalsegs %in% northeast])),
  rep( pal[2], times = length(totalsegs[totalsegs %in% southatlantic])),
  rep( pal[3], times = length(totalsegs[totalsegs %in% southgulf])),
  rep( pal[4], times = length(totalsegs[totalsegs %in% midwest])),
  rep( pal[5], times = length(totalsegs[totalsegs %in% west]))
)

circos.clear()
# graph patameters 
#--- --- --- --- --- --- --- --- --- ---
par(
  mar = c(1, 0, 3, 0),    # Margin around chart
  bg = c("grey15"),     # background color
  family="Helvetica Neue Light"
) 

# chord diagram
#--- --- --- --- --- --- --- --- --- ---
chordDiagram(x=exports, order=statelist,
             directional = 1,
             #  direction.type = "arrows",
             diffHeight = F,
             preAllocateTracks = list(list(track.height=  uh(3,"mm")   ), # outside track for names
                                      list(track.height=  uh(10,"mm")    )), # middle track for regions
             self.link = 1,
             annotationTrack = "grid",
             grid.col = grid.col
)

#highlight regions in track 2
#--- --- --- --- --- --- --- --- --- --- --- ---
highlight.sector(northeast, track.index = 1, col = pal[1],
                 text = "Northeast", cex = 0.7, text.col = "white", niceFacing = TRUE)
highlight.sector(southatlantic, track.index = 1, col = pal[2],
                 text = "South Atlantic", cex = 0.7, text.col = "white", niceFacing = TRUE)
highlight.sector(southgulf, track.index = 1, col = pal[3],
                 text = "Gulf States", cex = 0.7, text.col = "white", niceFacing = TRUE)
highlight.sector(midwest[midwest %in% totalsegs], track.index = 1, col = pal[4],
                 text = "Midwest", cex = 0.7, text.col = "white", niceFacing = TRUE)
highlight.sector(west[west %in% totalsegs], track.index = 1, col = pal[5],
                 text = "West", cex = 0.7, text.col = "white", niceFacing = TRUE)

# add state names
#--- --- --- --- --- --- --- --- --- --- --- ---
circos.track(track.index = 2, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex=.7,col="grey75")
}, bg.border = NA) # add state labels

# add title
#--- --- --- --- --- --- --- --- --- --- --- ---
#par(family="Helvetica Thin",col.main="grey75")
title(main = list("Shelter Dog Trade Network",
                  cex=2.4,
                  col="grey75"))
            

# I just exported this one using the export pulldown on the plots tab.
# That was the easiest way to control for sizing. 


            

