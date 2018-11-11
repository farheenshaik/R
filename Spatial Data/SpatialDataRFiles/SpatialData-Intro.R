# Attach 'sp' and 'raster' packages first
# Let's start with some basic spatial points

longitude <- c(-116.7, -120.4, -116.7, -113.5, -115.5, -120.8, -119.5, -113.7, -113.7, -110.7)
latitude <- c(45.3, 42.6, 38.9, 42.1, 35.7, 38.9, 36.2, 39, 41.6, 36.9)
lonlat <- cbind(longitude, latitude)

# Now we create a SpatialPoints object and see what's inside ...
pts <- SpatialPoints(lonlat)
showDefault(pts)

# This gives a 'bounding box', but also a coordinate reference
# system (here set to NA).We can set the CRS as follows ...
crdref <- CRS('+proj=longlat +datum=WGS84')
pts <- SpatialPoints(lonlat, proj4string=crdref)
pts

# Here we combine the SpatialPoints object with a data frame
df <- data.frame(ID=1:nrow(lonlat), precip=(latitude-30)^3)
ptsdf <- SpatialPointsDataFrame(pts, data=df)
ptsdf
str(ptsdf)
# Note we simply created a variable 'precip' to provide
# some data for each data point
showDefault(ptsdf)

# Now let's create some lines and polygons ...
lon <- c(-116.8, -114.2, -112.9, -111.9, -114.2, -115.4, -117.7)
lat <- c(41.3, 42.9, 42.4, 39.8, 37.6, 38.3, 37.6)
lonlat <- cbind(lon, lat)
lns <- spLines(lonlat, crs=crdref)
lns
pols <- spPolygons(lonlat, crs=crdref)
pols
# Note the structure of 'pols' is complex ...
str(pols)

# Let's produce a very crude map of the data:
plot(pols, axes=TRUE, las=1)
plot(pols, border='blue', col='yellow', lwd=3, add=TRUE)
points(pts, col='red', pch=20, cex=3)