# Tidy Tuesday Jan 21, 2020
# Spotify metadata




install.packages('spotifyr')
devtools::install_github('charlie86/spotifyr')

library("spotifyr")
library("tidyverse")
library("knitr")
library("lubridate")
library("ggtext")
library(ggiraphExtra)
library(ggplot2)



Sys.setenv(SPOTIFY_CLIENT_ID = '7fe745bee36d4c2dafb2b62356d64466')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '51d09d9720f542d2ab2273bf4e89c084')

access_token <- get_spotify_access_token()




bbq <- get_playlist_audio_features(username= "1225135908", 
                                    playlist_uris="0todj9r1lBkQrLz94nYkf5",
                            authorization = get_spotify_access_token())

study <- get_playlist_audio_features(username= "1225135908", 
                                     playlist_uris="68CN2MdFKElnlFteLeO0lc",
                                     authorization = get_spotify_access_token())


mellow <- get_playlist_audio_features(username= "1225135908", 
                                   playlist_uris="4T7oMhA245ASFqm5bptNKm",
                                   authorization = get_spotify_access_token())

groove <- get_playlist_audio_features(username= "1225135908", 
                                      playlist_uris="07PQMNamojYOayPeyzxK1A",
                                      authorization = get_spotify_access_token())







num <- 25

bbqradar <- bbq  %>%
  dplyr::select(playlist_name,
                track.name,
                danceability,
                energy,
                loudness,
               # mode,
                speechiness,
                acousticness,
                instrumentalness,
                liveness,
                valence,
                tempo
               # track.id
               ) %>%
  sample_n(num)


studyradar <- study %>%
  dplyr::select(playlist_name,
                track.name,
                danceability,
                energy,
                loudness,
                # mode,
                speechiness,
                acousticness,
                instrumentalness,
                liveness,
                valence,
                tempo
                # track.id
  ) %>%
  sample_n(num)
  


mellowradar <- mellow %>% 
  dplyr::select(playlist_name,
                track.name,
                danceability,
                energy,
                loudness,
                # mode,
                speechiness,
                acousticness,
                instrumentalness,
                liveness,
                valence,
                tempo
                # track.id
  ) %>%
  sample_n(num)



grooveradar <- groove %>% 
  dplyr::select(playlist_name,
                track.name,
                danceability,
                energy,
                loudness,
                # mode,
                speechiness,
                acousticness,
                instrumentalness,
                liveness,
                valence,
                tempo
                # track.id
  ) %>%
  sample_n(num-1)





radardf <- rbind(bbqradar,
                 studyradar,
                 mellowradar
                 )


order <- c("acousticness", 
           "instrumentalness", 
           "tempo", 
           "loudness",
           "valence",
           "energy",
           "danceability"
           #"liveness",
           #"speechiness"
) 


plot.df <- radardf %>%
  mutate(danceability = scale(danceability,center=F,scale=max(danceability,na.rm = T)/100),
         energy = scale(energy,center=F,scale=max(energy,na.rm = T)/100),
         loudness = scale((loudness+40),center=F,scale=max((loudness+40),na.rm = T)/100),
         speechiness = scale(speechiness,center=F,scale=max(speechiness,na.rm = T)/100),
         acousticness = scale(acousticness,center=F,scale=max(acousticness,na.rm = T)/100),
         valence = scale(valence,center=F,scale=max(valence,na.rm = T)/100),
         tempo = scale(tempo,center=F,scale=max(tempo,na.rm = T)/100),
         liveness =scale(liveness, center=F,scale = max(liveness, na.rm=T)/100),
         instrumentalness = scale(instrumentalness,center=F,scale=max(instrumentalness,na.rm = T)/100)
         ) %>%
  gather(key = metric,c(danceability,
                        energy,
                       # speechiness,
                        acousticness,
                        instrumentalness,
                        #liveness,
                        valence,
                        loudness,
                        tempo
                       ),
         value=value) %>%
  group_by(playlist_name) %>%
#  distinct(playlist_name)
 # filter(playlist_name %in% c("Groove")) %>%
  #arrange(metric) %>%
  arrange(factor(metric, levels = order))

plot.df2 <- plot.df %>% select(-speechiness,-liveness)

save(plot.df2,file=here::here("data files","Jan21.20","playlists.Rdata"))


str(plot.df2)

library(PNWColors)
pnw_palette("Spring",3)


pal <- pnw_palette("Spring",3)
main <-  plot.df %>%
ggplot(aes(x=metric,y=value,group=interaction(playlist_name,metric),fill=playlist_name,color=playlist_name)) +
  geom_polygon(aes(group=track.name),alpha=.01,size=.2,fill=NA,show.legend = F)+ 
  scale_x_discrete(limits = order,
                   labels=c("Acoustic","Instrumental","Fast","Loud","Happy","Energetic","Danceable"))+
  scale_color_manual(values = pal)+
  scale_fill_manual(values = pal)+
  coord_polar(clip = "off")+
  theme_minimal() + 
  labs(title= "The Flavors of 3 Playlists",
       subtitle = "<span style='color:#d8aedd'>Backyard BBQ</span>  <span style='color:#cb74ad'>Mellow Jams</span>    <span style='color:#ffc3a3'>Study Songs</span>")+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_markdown(family="Montserrat",
                                      hjust=.5,
                                      size=24),
        plot.subtitle = element_markdown(family="Gill Sans",
                                         size=16,
                                         hjust=.5),
        text = element_markdown(family="Gill Sans Light",
                                size=16),
        #plot.background = element_rect(color="grey20",fill="transparent")
        )+
  ylim(0,100)+
   geom_polygon(data = . %>%  group_by(playlist_name,metric) %>% 
                  summarise_at(c("value"),mean) %>%
                  arrange(factor(metric, levels = order)) %>%
                  ungroup(),
                aes(x=metric,y=value,group=playlist_name,color=playlist_name,fill=playlist_name),alpha=.4,size=1.5,show.legend = F
                 ) 



facet <- plot.df %>%
  filter(playlist_name != "Groove") %>%
  ggplot(aes(x=metric,y=value,group=interaction(playlist_name,metric),fill=playlist_name,color=playlist_name)) +
  geom_polygon(aes(group=track.name),size=.2,alpha=.03,show.legend = F)+ 
  scale_x_discrete(limits = order,
                   labels=c("Acoustic","Instrumental","Fast","Loud","Happy","Energetic","Danceable"))+
  scale_color_manual(values = pal)+
  scale_fill_manual(values = pal)+
  coord_polar(clip="off")+
  theme_minimal() +
  labs(caption = "Data from Spotify & SpotifyR | Visualization by @Jake_Lawlor1")+
  theme(strip.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.caption = element_markdown(family="Montserrat",
                                        hjust=0.5,
                                        size=10))+
  ylim(0,100) +
#  geom_polygon(data = . %>%  group_by(playlist_name,metric) %>% 
#                 summarise_at(c("value"),mean) %>%
#                 arrange(factor(metric, levels = order)) %>%
#                 ungroup(),
#               aes(x=metric,y=value,group=playlist_name,color=playlist_name,fill=playlist_name),alpha=.4,size=1.5,show.legend = F
#                ) +
  facet_grid(~playlist_name)


library(patchwork)

(panel <- (main + facet) + plot_layout(ncol = 1, heights = c(1 , 0.28)))

ggsave(here::here("output", "Jan21.20", "PlaylistPlot.png"), plot = panel,
       width = 6, height = 8, limitsize = F)
  

here::here()
summary %>%
  ggplot(aes(x=metric,y=value,group=playlist_name,color=playlist_name)) +
  geom_polygon(fill=NA,aes(group=playlist_name)) +
  scale_x_discrete(limits = order ) +
  coord_polar() +
  ylim(0,100)

  
  ggplot(aes(x=metric,y=value,group=interaction(playlist_name,metric),fill=playlist_name,color=playlist_name)) +
  geom_polygon(aes(group=playlist_name),alpha=.01,size=.2)+ 
  #geom_point(position="jitter") +
  #geom_boxplot() +
  scale_x_discrete(limits = order )+
  #  scale_fill_manual(values=c("blue","red","yellow")) +
  coord_polar()+
  theme_minimal() + 
  labs(title= "Qualities of Songs per Playlist")+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_markdown(family="Georgia",
                                      hjust=.5),
        text = element_markdown(family="Georgia"))
  

plot.df %>%
  filter(playlist_name %in% c("Studystudy")) %>%
  filter(metric %in% c("valence")) %>%
  summarise(mean=mean(value))

library(extrafont)
font <- "SimSun"
fonts <- fonts()
i <- 13
warnings()
fonts <- names(pdfFonts())
fonts[i]

radardf %>%
  mutate(danceability = scale(danceability,center=F,scale=max(danceability,na.rm = T)/100),
         energy = scale(energy,center=F,scale=max(energy,na.rm = T)/100),
         loudness = scale((loudness+40),center=F,scale=max((loudness+40),na.rm = T)/100),
         speechiness = scale(speechiness,center=F,scale=max(speechiness,na.rm = T)/100),
         acousticness = scale(acousticness,center=F,scale=max(acousticness,na.rm = T)/100),
         valence = scale(valence,center=F,scale=max(valence,na.rm = T)/100),
         tempo = scale(tempo,center=F,scale=max(tempo,na.rm = T)/100),
         liveness =scale(liveness, center=F,scale = max(liveness, na.rm=T)/100),
         instrumentalness = scale(instrumentalness,center=F,scale=max(instrumentalness,na.rm = T)/100)) %>%
  gather(key = metric,c(danceability,
                        energy,
                        # speechiness,
                        acousticness,
                        instrumentalness,
                        #liveness,
                        valence,
                        loudness,
                        tempo),value=value) %>%


?scale_fill_manual


?scale_x_discrete
  ggplotly(p)


dat$a <- scale(dat$a, center = FALSE, scale = max(dat$a, na.rm = TRUE)/100)



bbqradar %>%
  ggradar(plot.legend = F,
          group.colours = "blue")

?ggradar

library(ggradar)
colnames(top)

top

  
get_my_top_artists_or_tracks()










