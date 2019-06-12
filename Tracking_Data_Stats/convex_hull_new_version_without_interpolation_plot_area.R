## Packages
library(reticulate)
library(purrr)
library(data.table)
library(dplyr)
library(tidyverse)
library(tweenr)
source('my_pitch_plot.R')
library(gganimate)
library(sp)

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





# Add new lines to dataframe
# coordinates are recorded every 0.1 second. To ensure we have a smooth animation we can interpolate the
# coordinates with tweenr package

### TRIAL WITHOUT INTERPOLATION

# data <- data %>% 
#   arrange(time, team, player) %>% 
#   unite(group, team, player) %>% 
#   mutate(ease = 'linear') %>%
#   tween_elements('time', 'group', 'ease', nframes = 2 * max(.$time)) %>%
#   separate(col = .group, into = c('team', 'player'), sep = '_')



#############################################
# save(data, file = "mydata.rda")
#############################################
# LOAD data dataframe
#############################################

# load(file = "mydata.rda")

############################################
# time  x      y      .frame  team player
# 1.0   -8.19   18.57   0    ball  0
# 1.0 46.30 1.31    0 team A    1
############################################


# calculating convex hull for each team and .frame. I exclude the goalkeepers as the convex hull with
# only outfield players gives a nice visualisation of defence line disposure and therefore is more informative

# OLD  without AREA
# data_hull <- data %>%
#   filter(player != 1) %>% 
#   group_by(team, time) %>%
#   nest() %>%
#   mutate(
#     hull = map(data, ~ with(.x, chull(x, y))),
#     out = map2(data, hull, ~ .x[.y,,drop=FALSE])
#   ) %>%
#   select(-data) %>%
#   unnest()

# AREA FOR EACH POLYGON convex hull

data_hull <- data %>% 
  filter(player != 1) %>% 
  group_by(team, time) %>% 
  nest() %>% 
  mutate( hull = map(data, ~ with(.x, chull(x, y))), 
          out = map2(data, hull, ~ .x[.y,,drop=FALSE]), 
          polygon = map(out, ~.x %>% select(x, y) %>% 
                      as.matrix() %>% sp::Polygon(hole = FALSE)), area = map_dbl(polygon, ~.x@area) ) %>% 
  select(-data, -polygon) %>% 
  unnest()





# temporary filters
#   data <- data %>%  filter(time < 10)
#  data_hull <- data_hull %>%  filter(time < 10)

#########################################################################


p <- pitch_plot(68, 105) +
  geom_polygon(data = filter(data_hull, team == 'team A'), aes(x, y), fill = 'red', alpha = 0.4) +
  geom_polygon(data = filter(data_hull, team == 'team B'), aes(x, y), fill = 'blue', alpha = 0.4) +
  geom_point(data = filter(data, str_detect(team, 'team')), aes(x, y, group = player, fill = team), shape = 21, size = 6, stroke = 2) +
  geom_point(data = filter(data, team == 'ball'), aes(x, y), shape = 21, fill = 'dark orange', size = 4) +
  transition_time(time) +
  scale_fill_manual(values = c('team A' = 'red', 'team B' = 'blue')) +
  geom_text(data = filter(data, str_detect(team, 'team')), aes(x, y, label = player), color = 'white') +
  # team B blue
  geom_text(data = filter(data_hull,team =='team B'),aes(x=-20, y=-36, label = paste("Blue Area: ",as.character(round(area))) ),size=5,color='blue') +
  # team A red
  geom_text(data = filter(data_hull,team =='team A'),aes(x=+20, y=-36, label = paste("Red Area: ",as.character(round(area))) ) ,size=5,color='red') +
 theme(legend.position = "none")



animate(p,width = 735, nframes=max(data$time), height = 476,fps = 5)


anim_save("sequence2_without_int_area_10fps.gif") 


# 20fps
animate(p,width = 1024, nframes=max(data$time), height = 768,fps = 20)

anim_save("sequence2_without_interpolation_20fps.gif") 
