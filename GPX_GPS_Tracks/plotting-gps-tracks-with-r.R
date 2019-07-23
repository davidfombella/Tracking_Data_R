# Many GPS devices and apps have the capability to track your current position via GPS. 
# If you go walking, running, cycling, flying or driving, you can take a look at your exact route and your average speed.

# Some of these devices or apps also allow you to export your routes in various formats, 
#  e.g., the popular XML-based GPX format.

# I want to show you my attempts to
# - read in a GPX file using R and its XML package,
# - calculate distances and speeds between points,
# - plot elevation and speed,
# - plot a track,
# - plot a track on a map.

library(XML)
library(OpenStreetMap)
library(lubridate)
library(raster)


# Function that shifts vectors conviniently (we gonna need this later):
shift.vec <- function (vec, shift) {
  if(length(vec) <= abs(shift)) {
    rep(NA ,length(vec))
  }else{
    if (shift >= 0) {
      c(rep(NA, shift), vec[1:(length(vec)-shift)]) }
    else {
      c(vec[(abs(shift)+1):length(vec)], rep(NA, abs(shift))) } } }
	  

# Now, we're reading in the GPX file. 
# If you want help on parsing XML files, check out this (German) tutorial I made a while ago.

# Parse the GPX file
pfile <- htmlTreeParse("run.gpx", error = function (...) {}, useInternalNodes = T)

# Get all elevations, times and coordinates via the respective xpath
elevations <- as.numeric(xpathSApply(pfile, path = "//trkpt/ele", xmlValue))
times <- xpathSApply(pfile, path = "//trkpt/time", xmlValue)
coords <- xpathSApply(pfile, path = "//trkpt", xmlAttrs)

# Extract latitude and longitude from the coordinates
lats <- as.numeric(coords["lat",])
lons <- as.numeric(coords["lon",])

# Put everything in a dataframe and get rid of old variables
geodf <- data.frame(lat = lats, lon = lons, ele = elevations, time = times)
rm(list=c("elevations", "lats", "lons", "pfile", "times", "coords"))

head(geodf)




# We already have nice dataframe now with all the information available in the GPX file for each position. 
# Each position is defined by the latitude and longitude and we also have the elevation (altitude) available.
# Note, that the altitude ist quite noisy with GPS, we will see this in a minute.



# Now, let's calculate the distances between successive positions and the respective speed in this segment.




# Shift vectors for lat and lon so that each row also contains the next position.
geodf$lat.p1 <- shift.vec(geodf$lat, -1)
geodf$lon.p1 <- shift.vec(geodf$lon, -1)

# Calculate distances (in metres) using the function pointDistance from the 'raster' package.
# Parameter 'lonlat' has to be TRUE!
geodf$dist.to.prev <- apply(geodf, 1, FUN = function (row) {
  raster::pointDistance(c(as.numeric(row["lat.p1"]),
  as.numeric(row["lon.p1"])),
                c(as.numeric(row["lat"]), as.numeric(row["lon"])),
                lonlat = T)
})

# Transform the column 'time' so that R knows how to interpret it.
geodf$time <- strptime(geodf$time, format = "%Y-%m-%dT%H:%M:%OS")

# Shift the time vector, too.
geodf$time.p1 <- shift.vec(geodf$time, -1)

# Calculate the number of seconds between two positions.
geodf$time.diff.to.prev <- as.numeric(difftime(geodf$time.p1, geodf$time))

# Calculate metres per seconds, kilometres per hour and two LOWESS smoothers to get rid of some noise.
geodf$speed.m.per.sec <- geodf$dist.to.prev / geodf$time.diff.to.prev
geodf$speed.km.per.h <- geodf$speed.m.per.sec * 3.6
geodf$speed.km.per.h <- ifelse(is.na(geodf$speed.km.per.h), 0, geodf$speed.km.per.h)
geodf$lowess.speed <- lowess(geodf$speed.km.per.h, f = 0.2)$y
geodf$lowess.ele <- lowess(geodf$ele, f = 0.2)$y





head(geodf)


#Now, lets plot all the stuff!

# Plot elevations and smoother
plot(geodf$ele, type = "l", bty = "n", xaxt = "n", ylab = "Elevation", xlab = "", col = "grey40")
lines(geodf$lowess.ele, col = "red", lwd = 3)
legend(x="bottomright", legend = c("GPS elevation", "LOWESS elevation"),col = c("grey40", "red"), lwd = c(1,3), bty = "n")

# Plot speeds and smoother
plot(geodf$speed.km.per.h, type = "l", bty = "n", xaxt = "n", ylab = "Speed (km/h)", xlab = "", col = "grey40")
lines(geodf$lowess.speed, col = "blue", lwd = 3)
legend(x="bottom", legend = c("GPS speed", "LOWESS speed"), col = c("grey40", "blue"), lwd = c(1,3), bty = "n")
abline(h = mean(geodf$speed.km.per.h), lty = 2, col = "blue")

# Plot the track without any map, the shape of the track is already visible.
plot(rev(geodf$lon), rev(geodf$lat), type = "l", col = "red", lwd = 3, bty = "n", ylab = "Latitude", xlab = "Longitude")

# We will now use the OpenStreetMap package to get some maps from the internet and use them as a background for the track we just converted.
#  There are several blocks for each map type.
#  Check the comments in the first block what the different calls do.


####################
# MAP 1 OSM
####################

# First, we need to get the specific map. The 'type' argument controls which type of map we get.

map <- openmap(as.numeric(c(max(geodf$lat), min(geodf$lon))),
               as.numeric(c(min(geodf$lat), max(geodf$lon))), type = "osm")

# This next step is important and it took me a while to figure this out.
# We need to convert the maps we got with the function openmap() to a projection that fits our longitude-latitude format of positions.
# This will distort the maps in the plots a little. But we need this step because the track has to fit the map!

transmap <- openproj(map, projection = "+proj=longlat")

# Now for plotting...
png("map1.png", width = 1000, height = 800, res = 100)
par(mar = rep(0,4))
plot(transmap, raster=T)
lines(geodf$lon, geodf$lat, type = "l", col = scales::alpha("red", .5), lwd = 4)
dev.off()

####################
# MAP 2 BING
####################
map <- openmap(as.numeric(c(max(geodf$lat), min(geodf$lon))),
               as.numeric(c(min(geodf$lat), max(geodf$lon))), type = "bing")

transmap <- openproj(map, projection = "+proj=longlat")
png("map2.png", width = 1000, height = 800, res = 100)
par(mar = rep(0,4))
plot(transmap, raster=T)
lines(geodf$lon, geodf$lat, type = "l", col = scales::alpha("yellow", .5), lwd = 4)
dev.off()


####################
# MAP 3 MAPQUEST falla mapquest use waze
####################
map <- openmap(as.numeric(c(max(geodf$lat), min(geodf$lon))),
               as.numeric(c(min(geodf$lat), max(geodf$lon))), type = "waze")

transmap <- openproj(map, projection = "+proj=longlat")
png("map3.png", width = 1000, height = 800, res = 100)
par(mar = rep(0,4))
plot(transmap, raster=T)
lines(geodf$lon, geodf$lat, type = "l", col = scales::alpha("yellow", .5), lwd = 4)
dev.off()


####################
# MAP 4 skobbler
####################

map <- openmap(as.numeric(c(max(geodf$lat), min(geodf$lon))),
               as.numeric(c(min(geodf$lat), max(geodf$lon))), type = "skobbler")

transmap <- openproj(map, projection = "+proj=longlat")
png("map4.png", width = 1000, height = 800, res = 100)
par(mar = rep(0,4))
plot(transmap, raster=T)
lines(geodf$lon, geodf$lat, type = "l", col = scales::alpha("blue", .5), lwd = 4)
dev.off()


####################
# MAP 5 esri-topo
####################

map <- openmap(as.numeric(c(max(geodf$lat), min(geodf$lon))),
               as.numeric(c(min(geodf$lat), max(geodf$lon))), type = "esri-topo")

transmap <- openproj(map, projection = "+proj=longlat")
png("map5.png", width = 1000, height = 800, res = 100)
par(mar = rep(0,4))
plot(transmap, raster=T)
lines(geodf$lon, geodf$lat, type = "l", col = scales::alpha("blue", .5), lwd = 4)
dev.off()




