# We look at crime reports in Portland. Make sure the 
# 'rgdal' package is loaded.

port <- readOGR(dsn = "Data", layer = "Police_Districts_Portland")
crime <- readOGR(dsn = "Data", layer = "NIJ_Nov2016_Crime")

# Let's look at he plots
par(mfrow=c(1,2))
plot(port); title(main = list("Portland Police Districts (Polygon Object)", cex=0.8))
plot(crime); title(main = list("November 2016 Crime Reports (Points Object)", cex=0.8))

summary(crime)

# Here is the bounding box
bbox(crime)

# Crime is a SpatialPointDataFrame - here is a look
# at the first few rows of data:
head(crime@data)

# Here we count the number of crimes in each polygon
# defined in 'port'

crime_agg <- aggregate(x=crime["CATEGORY"],by=port,FUN=length) # Aggregate by district
summary(crime_agg)

# Here are the crime oounts in each of the 60 
# districts defined in 'port':
print(crime_agg$CATEGORY)

# Attach the 'leaflet' package for the following
crime_agg <- spTransform(crime_agg, CRS("+init=epsg:4326")) # Reproject coordinates
qpal <- colorBin("Reds", crime_agg$CATEGORY, bins=5)

# This is a fairly complicated command that produces 
# a nice graphic of our aggregate crime data:
leaflet(crime_agg) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(CATEGORY),weight = 1) %>%
  addLegend(values=~CATEGORY,pal=qpal,title="Crime Count")

# Now we will add a grid and work with our data 
# in this grid setting ... attach 'raster'
e <- extent(bbox(port))                  # define boundaries of object
r <- raster(e)                           # create raster object 
dim(r) <- c(40, 40)                      # specify number of cells
projection(r) <- CRS(proj4string(port))  # give it the same projection as port
g <- as(r, 'SpatialPolygonsDataFrame')   # convert into polygon
summary(g)

# We will do the same aggregation function using our
# new grid structure:
p <- g[port,]
crime_agg1 <- aggregate(x=crime["CATEGORY"],by=p,FUN=length)

# Since the grid cells are smaller than the police districts,
# they may not have any crime reports in them (they would be
# NAs). We set the NAs to '0'

crime_agg1$CATEGORY[is.na(crime_agg1$CATEGORY)] <- 0
# Note crime_agg1$CATEGORY is a dataframe living inside
# a spatial object

# Let's do a plot:
crime_agg1 <- spTransform(crime_agg1, CRS("+init=epsg:4326")) # reproject
qpal <- colorBin("Reds", crime_agg1$CATEGORY, bins=5)       # define color bins
leaflet(crime_agg1) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(CATEGORY),weight = 0.5) %>%
  addLegend(values=~CATEGORY,pal=qpal,title="Crime Count")

# We can crop this plot and overlay it on a Portland
# shape as follows: (make sure 'rgeos' is attached)

# Melt Portland districts into one polygon
portland = gUnaryUnion(port, port$dummy)
# Take the intersection of the grid and the Portland map
map <- gIntersection(p,portland,byid = TRUE,drop_lower_td = TRUE) 

# Now redo the calculations:
crime_agg2 <- aggregate(x=crime["CATEGORY"],by=map,FUN=length)
crime_agg2$CATEGORY[is.na(crime_agg2$CATEGORY)] <- 0
crime_agg2 <- spTransform(crime_agg2, CRS("+init=epsg:4326"))
qpal <- colorBin("Reds", crime_agg2$CATEGORY, bins=5)
plot3 <- leaflet(crime_agg2) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(CATEGORY),weight = 0.5) %>%
  addLegend(values=~CATEGORY,pal=qpal,title="Crime Count")
