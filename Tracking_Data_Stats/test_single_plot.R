## Packages
library(reticulate)
library(purrr)
library(data.table)
library(dplyr)
library(tidyverse)
library(tweenr)
source('my_pitch_plot.R')
library(gganimate)

# para geom_tile_voronoi libreria de github
# devtools::install_github("thomasp85/ggforce")

## Load data
#(https://stackoverflow.com/questions/35121192/reading-a-pickle-file-pandas-python-data-frame-in-r)


source_python("script.py")
data <- read_pickle_file("test_data_1.pkl")  # filename from Stats dataset

## Get dataframe with all data
dt_list <- map(data, as.data.frame)
dt_data <- rbindlist(dt_list, fill=TRUE, idcol=T)
data <- dt_data %>%
      rename(sequ=.id) %>%
      arrange(sequ)


# write.csv(data, "test_1.csv", row.names=F)
# write.csv(data, "data_train.csv", row.names=F)


# Filter a single sequence
data <- data %>% filter(sequ == "sequence_2")

# Remove first column
data <- data %>% select(-sequ) 



# change names for dataframe columns
names(data) <- c(paste('team A_', rep(1:11, each = 2), '_', c('x', 'y'), sep = ''), 
                 paste('team B_', rep(1:11, each = 2), '_', c('x', 'y'), sep = ''), 
                 paste('ball_0_', c('x', 'y'), sep = ''))





data <- data %>% 
  mutate(time = row_number()) %>% 
  gather(key, value, -time) %>% 
  extract(key, into = c('team', 'player', 'coordinate'), regex = '(.+)_(.+)_([xy])') %>% 
  spread(coordinate, value)



# Filter a single time frame
data <- data %>% filter(time == 1)



## map(.x, .f, ...)


data_hull_temp <- data %>%
  filter(player != 1) %>% 
  group_by(team, time) %>%
  nest() %>%
  mutate(
    hull = map(data, ~ with(.x, chull(x, y))),
    out = map2(data, hull, ~ .x[.y,,drop=FALSE])
  ) 




# calculating convex hull for each team and .frame. I exclude the goalkeepers as the convex hull with
# only outfield players gives a nice visualisation of defence line disposure and therefore is more informative

data_hull <- data %>%
  filter(player != 1) %>% 
  group_by(team, time) %>%
  nest() %>%
  mutate(
    hull = map(data, ~ with(.x, chull(x, y))),
    out = map2(data, hull, ~ .x[.y,,drop=FALSE])
  ) %>%
  select(-data) %>%
  unnest()


############ AREA INITIAL #################
## area team A

data_hull_team_A <- data_hull %>%
              filter(team == 'team A')%>% 
              select(x,y)


# load library("sp")
chull.tA <- Polygon(data_hull_team_A, hole=F)
chull.ta.Area <- chull.tA@area
chull.ta.Area

# team A REd area 587,148


# team b
data_hull_team_B <- data_hull %>%
  filter(team == 'team B')%>% 
  select(x,y)



chull.tB <- Polygon(data_hull_team_B, hole=F)
chull.tb.Area <- chull.tB@area
chull.tb.Area

# team B blue area 812,2341

############ AREA END #################



### trial
data_hull_team_xxxxx <- data_hull %>%
  filter(team == 'team B')

Polygon(data_hull_team_xxxxx %>% select(x,y), hole=F)@area






#########################################################################
## good test ##

  pitch_plot(68, 105) +
  geom_polygon(data = filter(data_hull, team == 'team A'), aes(x, y), fill = 'red', alpha = 0.4) +
  geom_polygon(data = filter(data_hull, team == 'team B'), aes(x, y), fill = 'blue', alpha = 0.4) +
  geom_point(data = filter(data, str_detect(team, 'team')), aes(x, y, group = player, fill = team), shape = 21, size = 6, stroke = 2) +
  geom_point(data = filter(data, team == 'ball'), aes(x, y), shape = 21, fill = 'dark orange', size = 4) +
  # transition_time(time) +
  scale_fill_manual(values = c('team A' = 'red', 'team B' = 'blue')) +
  geom_text(data = filter(data, str_detect(team, 'team')), aes(x, y, label = player), color = 'white') +
  guides(fill = FALSE)+ 
  annotate("text", label = round(Polygon(filter(data_hull, team == 'team B')%>% select(x,y), hole=F)@area), x = -20, y = -37, size = 5, colour = "blue")+
  annotate("text", label = round(Polygon(filter(data_hull, team == 'team A')%>% select(x,y), hole=F)@area), x = +20, y = -37, size = 5, colour = "red")+
  labs( title = paste("Time",as.character(data$time),
                      "- Blue",
                      round(Polygon(filter(data_hull, team == 'team B')%>% select(x,y), hole=F)@area),
                       "- Red",
                       round(Polygon(filter(data_hull, team == 'team A')%>% select(x,y), hole=F)@area))  
      )





# animate(p,width = 1024, nframes=max(data$time), height = 768,fps = 10)
# anim_save("sequence2_without_interpolation_10fps.gif") 
# 20fps
# animate(p,width = 1024, nframes=max(data$time), height = 768,fps = 20)
# anim_save("sequence2_without_interpolation_20fps.gif") 
