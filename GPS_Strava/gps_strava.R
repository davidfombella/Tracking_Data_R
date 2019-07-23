# FROM
# https://rcrastinate.blogspot.com/2018/05/get-your-tracks-from-strava-api-and.html
# Get your tracks from the Strava API and plot them on Leaflet maps
# Here is some updated R code from my previous post. 
# It doesn't throw any warnings when importing tracks with and without heart rate information. 
# Also, it is easier to distinguish types of tracks now (e.g., when you want to plot runs and rides separately).
#  Another thing I changed: You get very basic information on the track when you click on it (currently the name of the track and the total length).




options(stringsAsFactors = F)

rm(list=ls())

library(httr)
library(rjson)
library(leaflet)
library(dplyr)

token <- "<your Strava API token>"


# Functions ---------------------------------------------------------------

get.coord.df.from.stream <- function (stream.obj) {
  data.frame(lat = sapply(stream.obj[[1]]$data, USE.NAMES = F, FUN = function (x) x[[1]]),
             lon = sapply(stream.obj[[1]]$data, USE.NAMES = F, FUN = function (x) x[[2]]))
}


get.stream.from.activity <- function (act.id, token) {
  stream <- GET("https://www.strava.com/",
                path = paste0("api/v3/activities/", act.id, "/streams/latlng"),
                query = list(access_token = token))
  content(stream)
}


get.activities2 <- function (token) {
  activities <- GET("https://www.strava.com/", path = "api/v3/activities",
                    query = list(access_token = token, per_page = 200))
  activities <- content(activities, "text")
  activities <- fromJSON(activities)
  res.df <- data.frame()
  for (a in activities) {
    values <- sapply(c("name", "distance", "moving_time", "elapsed_time", "total_elevation_gain",
                       "type", "id", "start_date_local",
                       "location_country", "average_speed", "max_speed", "has_heartrate", "elev_high",
                       "elev_low", "average_heartrate", "max_heartrate"), FUN = function (x) {
                         if (is.null(a[[x]])) {
                           NA } else { a[[x]] }
                       })
    res.df <- rbind(res.df, values)
  }
  names(res.df) <- c("name", "distance", "moving_time", "elapsed_time", "total_elevation_gain",
                     "type", "id", "start_date_local",
                     "location_country", "average_speed", "max_speed", "has_heartrate", "elev_high",
                     "elev_low", "average_heartrate", "max_heartrate")
  res.df
}


get.multiple.streams <- function (act.ids, token) {
  res.list <- list()
  for (act.id.i in 1:length(act.ids)) {
    if (act.id.i %% 5 == 0) cat("Actitivy no.", act.id.i, "of", length(act.ids), "\n")
    stream <- get.stream.from.activity(act.ids[act.id.i], token)
    coord.df <- get.coord.df.from.stream(stream)
    res.list[[length(res.list) + 1]] <- list(act.id = act.ids[act.id.i],
                                             coords = coord.df)
  }
  res.list
}

activities <- get.activities2(token)

stream.list <- get.multiple.streams(activities$id, token)


# Leaflet -----------------------------------------------------------------

lons.range <- c(9.156572, 9.237580)
lats.range <- c(48.74085, 48.82079)

map <- leaflet() %>%
  addProviderTiles("OpenMapSurfer.Grayscale", # nice: CartoDB.Positron, OpenMapSurfer.Grayscale, CartoDB.DarkMatterNoLabels
                   options = providerTileOptions(noWrap = T)) %>%
  fitBounds(lng1 = min(lons.range), lat1 = max(lats.range), lng2 <- max(lons.range), lat2 = min(lats.range))

add.run <- function (act.id, color, act.name, act.dist, strlist = stream.list) {
  act.ind <- sapply(stream.list, USE.NAMES = F, FUN = function (x) {
    x$act.id == act.id
  })
  act.from.list <- strlist[act.ind][[1]]
  map <<- addPolylines(map, lng = act.from.list$coords$lon,
               lat = act.from.list$coords$lat,
               color = color, opacity = 1/3, weight = 2,
               popup = paste0(act.name, ", ", round(as.numeric(act.dist) / 1000, 2), " km"))
}


# plot all
for (i in 1:nrow(activities)) {
  add.run(activities[i, "id"], ifelse(activities[i, "type"] == "Run", "red",
                                      ifelse(activities[i, "type"] == "Ride", "blue", "black")),
          activities[i, "name"], activities[i, "distance"])
}

map 