##########################################################################
# this code is from some delightful human on the internet who figured out 
# how to source from GitHub; run this function in order to source in the 
# script with all the Pepperwood Functions 
if(!require(RCurl)){install.packages("RCurl");library(RCurl)}
url <- "https://raw.githubusercontent.com/dackerly/PepperwoodVegPlots/master/source_files.r"
eval(parse(text = getURL(url, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))), envir = .GlobalEnv)
##########################################################################

# Put the GIS folders in a "bigdata" folder in the project root, which will be pushed to github
setwd('../bigdata')

library(raster)
library(rgeos)
library(rgdal)

# Load GIS layers for extracting
dem.lidar <- raster("lidar/Pepperwood_HYDROFLATTENED_BARE_EARTH_UTM1.tif") * 0.3048
veg.height <- raster("lidar/Pepperwood_VEGETATION_HEIGHT_UTM1.tif") * 0.3048
canopy.den <- raster("lidar/Pepperwood_CANOPY_DENSITY_UTM1.tif")
geology <- readOGR("geology/PepperwoodGeologyQuadShapefile.shp", layer="PepperwoodGeologyQuadShapefile", stringsAsFactors=FALSE)
geology <- spTransform(geology, crs(dem.lidar))


# Download tree data from the hectar surveys (including trees >20cm from the original plots)
trees <- getHectareTrees()
trees <- applySubplots(trees, size=20, centroid=TRUE, fill=TRUE)
str(trees)

deciduous <- c("QUEGAR","QUEDOU","AESCAL","QUEKEL","QUELOB","QUEdec","FRALAT","ACEMAC")
evergreen <- c("ARCMAN","UMBCAL","QUEAGR","PSEMEN","ARBMEN","NOTDEN")

# Make a list of the 20x20m subplots
subplots <- aggregate(list(Center.UTM.E=trees$Center.UTM.E, Center.UTM.N=trees$Center.UTM.N), list(Subplot=trees$Subplot_20m, Plot=trees$Plot), unique)

# Get the basal area of deciduous species in each subplot
decid.ba <- aggregate(list(Deciduous.Basal.Area=trees[trees$Species %in% deciduous,'Basal.Area']), list(Subplot=trees[trees$Species %in% deciduous,'Subplot_20m'], Plot=trees[trees$Species %in% deciduous,'Plot']), sum, na.rm=TRUE)
subplots <- merge(subplots, decid.ba, by=c("Plot","Subplot"), all=TRUE)
# Get the basal area of evergreen species in each subplot
everg.ba <- aggregate(list(Evergreen.Basal.Area=trees[trees$Species %in% evergreen,'Basal.Area']), list(Subplot=trees[trees$Species %in% evergreen,'Subplot_20m'], Plot=trees[trees$Species %in% evergreen,'Plot']), sum, na.rm=TRUE)
subplots <- merge(subplots, everg.ba, by=c("Plot","Subplot"), all=TRUE)
# Get the total basal area of all trees in each subplot
trees.ba <- aggregate(list(Total.Basal.Area=trees$Basal.Area), list(Subplot=trees$Subplot_20m, Plot=trees$Plot), sum, na.rm=TRUE)
subplots <- merge(subplots, trees.ba, by=c("Plot","Subplot"),all=TRUE)
# Replace the NAs (from merging) with zeros
subplots[is.na(subplots)] <- 0

# Make a SpatialPointsDF for extracting
subs<-subplots
coordinates(subs) <- 3:4
crs(subs) <- crs(dem.lidar)

# Also set up a SpatialPolygonsDFwith 20m square buffers
subs.buffer <- gBuffer(subs, byid=TRUE, width=10, quadsegs=1, capStyle="SQUARE")


# Extract from the geology layer using the subplot centroids
subplots$Geology <- extract(geology, subs, fun=unique)[,8]

# Extract from the LIDAR data using the square buffers
subplots$Mean.Elevation.LIDAR <- extract(dem.lidar, subs.buffer, fun=mean)[,1]
subplots$Mean.Slope.LIDAR <- extract(terrain(dem.lidar, opt="slope", unit="degrees"), subs.buffer, fun=mean)[,1]
subplots$Mean.Aspect.LIDAR <- extract(terrain(dem.lidar, opt="aspect", unit="degrees"), subs.buffer, fun=mean)[,1]
subplots$Mean.Veg.Height.LIDAR <- extract(veg.height, subs.buffer, fun=mean)[,1]
subplots$Max.Veg.Height.LIDAR <- extract(veg.height, subs.buffer, fun=max)[,1]
subplots$Canopy.Density.LIDAR <- extract(canopy.den, subs.buffer, fun=max)[,1]

str(subplots)

# Just checking...
subplots[,c(1:2,7,12)]
plot(Total.Basal.Area ~ Mean.Veg.Height.LIDAR, data=subplots)

boxplot(Total.Basal.Area ~ Geology, data=subplots)
boxplot(Evergreen.Basal.Area ~ Geology, data=subplots)
boxplot(Deciduous.Basal.Area ~ Geology, data=subplots)
