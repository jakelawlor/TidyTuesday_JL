# March 10, 2020
# tidy tuesday, school diversity or something


devtools::install_github("thebioengineer/tidytuesdayR")
tuesdata <- tidytuesdayR::tt_load(2020, week = 11)

library(dplyr)
library(ggplot2)
library(plotly)
library(ggrepel)
library(tidyr)

# set theme
theme_set(theme_classic())

# load data
cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_cost.csv')
income <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_income.csv') 
salary <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/salary_potential.csv')
hist_tuition <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/historical_tuition.csv')
diversity <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/diversity_school.csv')


cost %>% glimpse()
income %>% glimpse()


salary %>% glimpse()
# ok, lets take school name, mid_career_pay, and make_world_better_percent

cost %>% glimpse()
# lets take name, in_state_total(), out_of_state_total()

salary_cut <- salary %>% select(name, mid_career_pay, make_world_better_percent)
nrow(salary_cut)
# 935 rows

cost_cut <- cost %>% select(name, in_state_total, out_of_state_total,type)
nrow(cost_cut)
# 2938 rows

# now merge those two
df <- salary_cut %>% left_join(cost_cut)
df %>% glimpse()

df_full <- df %>% filter(complete.cases(.))
df_full %>% glimpse()

ggplotly(
df_full %>% 
  ggplot(aes(y=mid_career_pay,x=make_world_better_percent,label=name,color=type))+
  geom_point(position = "jitter",alpha=.5,aes(size=in_state_total))
)

ggplotly(
df %>% 
  ggplot(aes(y=mid_career_pay,x=in_state_total,label=name))+
  geom_point()
)



priciest50 <- c()

priciest50 <- cost %>% arrange(desc(in_state_total))  %>% slice(1:50) %>% pull( name)


priciest50[]

diversity %>% slice(1:15)

df2 <- diversity %>% 
  filter(category %in% c("White",
                         "Black",
                         "Hispanic",
                         "Asian",
                         "American Indian / Alaska Native",
                         "Native Hawaiian / Pacific Islander")) %>%
  filter(name %in% priciest50) %>%
  mutate(name = factor(name, levels = rev(priciest50)))

unique(diversity$category)


df2 %>% 
  ggplot(aes(x=enrollment,y=name))+
  geom_bar(position = "stack",stat="identity")





# so now lets do this proportionally
df3 <- df2 %>% select(-total_enrollment) %>%
  pivot_wider(names_from = category, values_from = enrollment) %>%
  mutate(sum = rowSums(.[3:8])) %>%
  pivot_longer(names_to = "Category",cols = c(3:8)) %>%
  mutate(prop = value/sum*100) 


df3 %>% 
  ggplot(aes(x=prop,y=name,fill=Category))+
  geom_bar(position = "stack",stat="identity")



# ok, can we do the same with the 50 cheapest schools? 
cheap50 <- cost %>% arrange((in_state_total))  %>% slice(1:50) %>% pull( name)

df2_cheap <- diversity %>% 
  filter(category %in% c("White",
                         "Black",
                         "Hispanic",
                         "Asian",
                         "American Indian / Alaska Native",
                         "Native Hawaiian / Pacific Islander")) %>%
  filter(name %in% cheap50) %>%
  mutate(name = factor(name, levels = cheap50))

length(unique(df2_cheap$name)) # only 44 show up


df3_cheap <- df2_cheap %>% select(-total_enrollment) %>%
  pivot_wider(names_from = category, values_from = enrollment) %>%
  mutate(sum = rowSums(.[3:8])) %>%
  pivot_longer(names_to = "Category",cols = c(3:8)) %>%
  mutate(prop = value/sum*100) 



df3_cheap %>% 
  ggplot(aes(x=prop,y=name,fill=Category))+
  geom_bar(position = "stack",stat="identity")



# can I make a scatterplot of expensive and proportion white?

white <- diversity %>% 
  filter(category %in% c("White")) %>%
  mutate(propwhite = enrollment/total_enrollment)


price <- cost %>% 
  filter(degree_length %in% c("4 Year")) %>%
  arrange(desc(in_state_total)) %>%
  select( name,in_state_total,type)


whiteprice <- price %>% left_join(white) %>% arrange(desc(in_state_total))


whiteprice %>%
  ggplot(aes(x=in_state_total,y=propwhite))+
  geom_point(aes(color=type)) + 
  geom_smooth()


# see what colleges are most expensive for rich/poor

income %>% glimpse()


income %>% filter(year %in% c(2018)) %>% filter(name %in% c(priciest50,cheap50)) %>%
  mutate(group = case_when(name %in% priciest50 ~ "pricy",
                           name %in% cheap50 ~ "cheap")) %>%
  ggplot(aes(x=income_lvl,y=net_cost,color=group)) +
  geom_line(aes(group=name),alpha=.3) +
  geom_point() 

income %>% filter(name %in% c("Harvard University"))



cost %>% glimpse()

cost %>%
  ggplot(aes(x=in_state_tuition,y=room_and_board,color=degree_length)) +
  geom_point()


