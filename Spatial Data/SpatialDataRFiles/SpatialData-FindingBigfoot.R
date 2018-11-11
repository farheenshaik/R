# First, read in the Bigfoot data ...
dim(bigfoot)
head(bigfoot)

# next we plot the locations ... We will use a nice map
# of North America. Make sure 'sp','rgeos', and 'maptools'
# are attached.

par(mfrow = c(1,1))
plot(bigfoot[,1:2], cex=0.5, col='red')
data(wrld_simpl)
plot(wrld_simpl, add=TRUE)

# we will try to use climate data for our predictor information.
# As is often the case, we need to derive this information
# from another data source ...
# see: http://www.worldclim.org/bioclim
# Attach the 'raster' library

wc <- getData('worldclim', res=10, var='bio')
plot(wc[[c(1, 12)]], nr=2)

# Next we extract climatic data related to the locations
# of Bigfoot sightings ...
bfc <- extract(wc, bigfoot[,1:2])
head(bfc)

# There are a couple of sightings with no associated 
# climate data ...
i <- which(is.na(bfc[,1]))
i
plot(bigfoot[,1:2], cex=0.5, col='blue')
plot(wrld_simpl, add=TRUE)
points(bigfoot[i, ], pch=20, cex=3, col='red')

# Here we will add the random-absence data points ...
# Make sure 'dismo' is attached.
e <- extent(SpatialPoints(bigfoot[, 1:2]))
e
bg <- sampleRandom(wc, 5000, ext=e)
dim(bg)
head(bg)

# Now we combine the data sets to create our data frame
d <- rbind(cbind(pa=1, bfc), cbind(pa=0, bg))
d <- data.frame(d)
dim(d)

# Let's build a logisitic regession model and try to 
# predict sightings based upon bioclimatic predictors ...
set.seed(1)
rain = sample(8092,7500)
lr.model = glm(pa~., data=d, subset=train, family="binomial")
lr.pred = predict(lr.model,newdata = d[-train,-1], type="response")
pred = rep(0,592)
pred[lr.pred > .5] = 1
table(d[-train,]$pa,pred)
mean(d[-train,]$pa == pred)

# Let's see the probability of finding Bigfoot at UTD ...
# The long/lat coordinates of UTD are (-96.75,32.98).

BFUTDData = data.frame("lon" = -96.75, "lat"=32.98)
BFUTDPred = extract(wc, BFUTDData)
predict(lr.model,newdata = as.data.frame(BFUTDPred), type="response")

# Relatively low ....