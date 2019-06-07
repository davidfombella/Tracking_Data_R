library(tidyverse)
library(gganimate)
library(tweenr)
source('my_pitch_plot.R')


'''
tween_elements
From tweenr v0.1.5 by Thomas Pedersen
Create frames based on individual element states
This function creates tweens for each observation individually, in cases where the data doesnt pass through collective states but consists of fully independent transitions. Each observation is identified by an id and each state must have a time associated with it.

Usage

tween_elements(data, time, group, ease, timerange, nframes)

Arguments

data

    A data.frame consisting at least of a column giving the observation id, a column giving timepoints for each state and a column giving the easing to apply when transitioning away from the state.
time

    The name of the column holding timepoints
group

    The name of the column holding the observation id
ease

    The name of the column holding the easing function name
timerange

    The range of time to span. If missing it will default to range(data[[time]])
nframes

    The number of frames to generate. If missing it will default to ceiling(diff(timerange) + 1) (At least one frame for each individual timepoint)

Value A data.frame with the same columns as data except for the group and ease columns, but replicated nframes times. Two additional columns called .frame and .group will be added giving the frame number and observation id for each row.


Other data.frame.tween: tween_appear, tween_states
'''

#  Examples
#
#  NOT RUN {
# data <- data.frame(
#  x = c(1, 2, 2, 1, 2, 2),
#  y = c(1, 2, 2, 2, 1, 1),
#  time = c(1, 4, 10, 4, 8, 10),
#  group = c(1, 1, 1, 2, 2, 2),
#  ease = rep('cubic-in-out', 6)
# )
# data <- tween_elements(data, 'time', 'group', 'ease', nframes = 100)
# }







data <- read_csv('csv_files\\sequence_2.csv') 
data <- data %>% select(-X1)
names(data) <- c(paste('team A_', rep(1:11, each = 2), '_', c('x', 'y'), sep = ''), 
                 paste('team B_', rep(1:11, each = 2), '_', c('x', 'y'), sep = ''), 
                 paste('ball_0_', c('x', 'y'), sep = ''))

data <- data %>% 
  mutate(time = row_number()) %>% 
  gather(key, value, -time) %>% 
  extract(key, into = c('team', 'player', 'coordinate'), regex = '(.+)_(.+)_([xy])') %>% 
  spread(coordinate, value)

# coordinates are recorded every 0.1 second. To ensure we have a smooth animation we can interpolate the
# coordinates with tweenr package
data <- data %>% 
  arrange(time, team, player) %>% 
  unite(group, team, player) %>% 
  mutate(ease = 'linear') %>%
  tween_elements('time', 'group', 'ease', nframes = 2 * max(.$time)) %>%
  separate(col = .group, into = c('team', 'player'), sep = '_')

# calculating convex hull for each team and .frame. I exclude the goalkeepers as the convex hull with
# only outfield players gives a nice visualisation of defence line disposure and therefore is more informative
data_hull <- data %>%
  filter(player != 1) %>% 
  group_by(team, .frame) %>%
  nest() %>%
  mutate(
    hull = map(data, ~ with(.x, chull(x, y))),
    out = map2(data, hull, ~ .x[.y,,drop=FALSE])
  ) %>%
  select(-data) %>%
  unnest()

p <- pitch_plot(68, 105) +
  geom_polygon(data = filter(data_hull, team == 'team A'), aes(x, y, frame = .frame), fill = 'red', alpha = 0.4) +
  geom_polygon(data = filter(data_hull, team == 'team B'), aes(x, y, frame = .frame), fill = 'blue', alpha = 0.4) +
  geom_point(data = filter(data, str_detect(team, 'team')), aes(x, y, group = player, fill = team, frame = .frame), shape = 21, size = 6, stroke = 2) +
  geom_point(data = filter(data, team == 'ball'), aes(x, y, frame = .frame), shape = 21, fill = 'dark orange', size = 4) +
  scale_fill_manual(values = c('team A' = 'red', 'team B' = 'blue')) +
  geom_text(data = filter(data, str_detect(team, 'team')), aes(x, y, label = player, frame = .frame), color = 'white') +
  guides(fill = FALSE)

animation::ani.options(ani.width = 735, ani.height = 476)
gganimate(p, 'sequence_2.gif', interval = 0.05, title_frame = FALSE)