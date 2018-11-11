# First we read in the precipitation data and the counties 
# Attach 'sp' first

cts <- readRDS('data/counties.rds')
p <- read.csv('data/precipitation.csv')
head(p)

# p is a standard data file, cts gives the shpaes for California 
# counties. 

par(mai=c(0,0,0,0))
plot(cts)
points(p[,c('LONG', 'LAT')], col='red', pch=20)

# Standard approach: We will compute an average (annual) 
# rainfall for each location, and then use the LAT/LONG/ALT
# values to build a global regression model:

p$pan <- rowSums(p[,6:17])
lr.mod = lm(pan ~ ALT+LAT+LONG, data=p)
summary(lr.mod)

# We will do Geographically Weighted Regression (GWR).
# Attach the 'spgwr' package ...

# These commands set up a CRS for the problem ... make sure
# 'raster' and 'rgdal' are attached.
alb <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
sp <- p
coordinates(sp) = ~ LONG + LAT
crs(sp) <- "+proj=longlat +datum=NAD83"

spt <- spTransform(sp, alb)
ctst <- spTransform(cts, alb)

# We need to find the optimal bandwidth for the problem ...
bw <- gwr.sel(pan ~ ALT+LAT+LONG, data=spt)

# These are new points we will use for prediction ...
r <- raster(ctst, res=10000)
r <- rasterize(ctst, r)
newpts <- rasterToPoints(r)

# Here we run the gwr function to make the predictions ..
g <- gwr(pan ~ ALT+LAT+LONG, data=spt, bandwidth=bw, fit.points=newpts[, 1:2])
g

# We plot the results, considering only the ALT variable ...
slope <- r
intercept <- r
slope[!is.na(slope)] <- g$SDF$ALT
intercept[!is.na(intercept)] <- g$SDF$'(Intercept)'
s <- stack(intercept, slope)
names(s) <- c('intercept', 'slope')
plot(s)