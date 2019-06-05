## Packages
library(reticulate)
library(purrr)
library(data.table)
library(dplyr)
# para geom_tile_voronoi libreria de github
# devtools::install_github("thomasp85/ggforce")

## Load data
#(https://stackoverflow.com/questions/35121192/reading-a-pickle-file-pandas-python-data-frame-in-r)
source_python("script.py")
data <- read_pickle_file("train_data.pkl")  # filename from Stats dataset

## Get dataframe with all data
dt_list <- map(data, as.data.frame)
dt_data <- rbindlist(dt_list, fill=TRUE, idcol=T)
df <- dt_data %>%
      rename(sequ=.id) %>%
      arrange(sequ)

#write.csv(df, "data_train.csv", row.names=F)

## Animation
source("soccerAnimate.R")

### Voronoi
soccerAnimate(df, "sequence_1001", method="voronoi")  #sequence name from Stats dataset (train)
anim_save("output_woronoi_seq_1001.gif") 

## convexhull

soccerAnimate(df, "sequence_1001", method="convexhull")

anim_save("output_convexhull_seq_1001.gif") 


## clean

soccerAnimate(df, "sequence_1001", method="clean")

anim_save("output_clean_seq_1001.gif") 
