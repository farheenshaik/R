# We will now load in shape file that is contained in 
# the raster package. Make sure the package 'rgdal'
# is installed.

f <- system.file("external/lux.shp", package="raster")
library(raster)
p <- shapefile(f)
p

par(mai=c(0,0,0,0))
plot(p)

# We turn the object into a data frame and inspect ...
d <- data.frame(p)
head(d)
names(p)
p$NAME_2

# Here we get a single SpatialPolygonsDataFrame 
p[, 'NAME_2']

# Add a new variable ...
p$new <- sample(letters, length(p))
p
# We can of course also use this command to assign new
# values to an existing variable. We can also remove a 
# variable:
p$new = NULL

# Here is how we can merge a data frame with our Spatial object
dfr <- data.frame(District=p$NAME_1, Canton=p$NAME_2, Value=round(runif(length(p), 100, 1000)))
head(dfr)

pm <- merge(p, dfr, by.x=c('NAME_1', 'NAME_2'), by.y=c('District', 'Canton'))
pm

# it is possible to select particular records ...
i <- which(p$NAME_1 == 'Grevenmacher')
g <- p[i,]
g

# Here is how we can append a spatial object to another 
# spatial object ...
z <- raster(p, nrow=2, ncol=2, vals=1:4)
names(z) <- 'Zone'
# z is a raster object, which is consists of a 2x2 grid. 
# Here we coerce it to a SpatialPolygonsDataFrame
z <- as(z, 'SpatialPolygonsDataFrame')
z
plot(p)
plot(z, add=TRUE, border='blue', lwd=5)

# To append two spatial objects use the 'bind' command ...
b <- bind(p, z)
head(b)
tail(b)

# Here we aggregate a spatial object using a particular
# variable. Install and attach 'rgeos' first ...
pa <- aggregate(p, by='NAME_1')
plot(pa, add=TRUE, col=rainbow(3), lwd=3, border='black')

# Finally, here is how we can do spatial queries
pts <- matrix(c(6, 6.1, 5.9, 5.7, 6.4, 50, 49.9, 49.8, 49.7, 49.5), ncol=2)
spts <- SpatialPoints(pts, proj4string=crs(p))
plot(z, col='light blue', lwd=2)
points(spts, col='light gray', pch=20, cex=6)
text(spts, 1:nrow(pts), col='red', font=2, cex=1.5)

# The 'over' command can be used for queries between
# spatial objects ...

over(spts, p)
over(spts, z)