### Collated ADraper Files - Classifier
## Casey Samagalsky

setwd("C:/Users/cassa/Google Drive/MARSH project/Landsat ARD Data/Data Files")

### From file: remote_sensing.R
rm(list=ls())

library(raster)
library(rgdal)
library(sp)
library(maps)
library(ggplot2)
library(ggmap)
library(magrittr)
library(randomForest)
library(gridExtra)
#devtools::install_github('skinner927/reprtree')
library(reprtree)

## Importing & manipulating band files
bands_20150716 <- read.csv("C:/Users/cassa/Google Drive/MARSH project/Landsat ARD Data/Data Files/rtk20150817.csv")

utm.bands <- SpatialPoints(cbind(bands_20150716$easting, bands_20150716$northing), 
                           proj4string=CRS("+proj=utm +zone=18"))
long.lat.bands <- as.data.frame(spTransform(utm.bands,CRS("+proj=longlat")))

bands.map <- data.frame(subset(bands_20150716, select=c(Band1, Band2, Band3, Band4, 
                                                        Band5, Band6, Band7, easting, northing)),
                        lat = long.lat.bands$coords.x1,
                        lon = long.lat.bands$coords.x2)

band1 <- subset(bands.map, select=c(lat, lon, Band1))
band2 <- subset(bands.map, select=c(lat, lon, Band2))
band3 <- subset(bands.map, select=c(lat, lon, Band3))
band4 <- subset(bands.map, select=c(lat, lon, Band4))
band5 <- subset(bands.map, select=c(lat, lon, Band5))
band6 <- subset(bands.map, select=c(lat, lon, Band6))
band7 <- subset(bands.map, select=c(lat, lon, Band7))

band.map.subset <- subset(bands.map, select=c(easting, northing, lat, lon))


## Importing dominant species dataset
species <- read.csv("C:/Users/cassa/Google Drive/MARSH project/Landsat ARD Data/Data Files/DominantSpPerPlot.csv")
species <- species[!is.na(species$easting),]
species <- species[!is.na(species$northing),]

# convert UTM to lat, long
utm.coor.serc <- SpatialPoints(cbind(species$easting,species$northing), 
                               proj4string=CRS("+proj=utm +zone=18"))
# Convert to lat/long
long.lat.coor.serc <- as.data.frame(spTransform(utm.coor.serc, CRS("+proj=longlat")))

species.map <- data.frame(species = species$simplifiedClass,
                          lat = long.lat.coor.serc$coords.x1,
                          lon = long.lat.coor.serc$coords.x2,
                          easting = utm.coor.serc$coords.x1,
                          northing = utm.coor.serc$coords.x2)
species.map[which(species.map$species == ""),1] <- "no species"

# Saving species.map to csv
write.csv(species.map, "C:/Users/cassa/Google Drive/MARSH project/Landsat ARD Data/CSamagalsky/species.map.rtk20150817.csv", row.names = TRUE)

## Calculating NDVI
# reflection of vegetation (using NDVI)
ndvi <- (band5[,3]-band4[,3])/(band5[,3]+band4[,3])

# Workaround to get correct coordinate values
ndvi.points <- data.frame(layer=ndvi, lat=long.lat.bands$coords.x1, lon=long.lat.bands$coords.x2, 
                          easting = utm.bands$coords.x1, northing = utm.bands$coords.x2)

# constrain the ndvi points to the plot region
ndvi.points <- ndvi.points[which(ndvi.points$lat > -76.555 & ndvi.points$lat < -76.537),]
ndvi.points <- ndvi.points[which(ndvi.points$lon > 38.8725 & ndvi.points$lon < 38.879),]

## Plots
# SERC plots on top of Landat NDVI scores
ggplot() + 
  geom_point(data=ndvi.points, aes(x=lat, y=lon, color=layer), size=2.25) +
  geom_point(data = species.map[,3:2], aes(x=lat, y=lon), shape=1, color="darkslategray", size =1.5) +
  scale_color_gradient2(midpoint=median(ndvi.points$layer),low="lightblue", mid="bisque", high="darkolivegreen4") +
  labs(x="Latitude", y="Longitude")

# Plotting Species within the NDVI
ggplot() + 
  geom_rect(data=species.map, aes(xmin=easting, xmax=easting + 20, ymin=northing, ymax=northing + 20, fill = species), color=NA) + 
  geom_rect(data=ndvi.points, aes(xmin=easting, xmax=easting + 30, ymin=northing, ymax=northing + 30), color="black", fill = NA)

# Edited xmin to match both rectangles, now matches
ggplot() + 
  geom_rect(data=species.map, aes(xmin=easting, xmax=easting +30, ymin=northing, ymax=northing + 30, fill = species), color=NA) + 
  geom_rect(data=ndvi.points, aes(xmin=easting, xmax=easting + 30, ymin=northing, ymax=northing + 30), color="black", fill = NA)



## Creating Training data and full.data from band data & species data
# create our crop region layer
e <- as(extent(365375, 366400, 4303600, 4304800), 'SpatialPolygons')
crs(e) <- "+proj=utm +zone=18"

# covariates for model and dataset construction
evi.value <- 2.5 * ((band5[,3] - band4[,3]) / (((band4[,3] * 6) + band5[,3]) - ((7.5 * band2[,3]) + 1)))
ndvi.value <- ((band5[,3] - band4[,3]) / (band5[,3] + band4[,3]))
ndwi.value <- (band3[,3] - band5[,3]) / (band3[,3] + band5[,3])
savi.value <- (1.5 * ((band5[,3] - band4[,3]) / (band5[,3] + band4[,3] + 1.5)))

full.data <- data.frame(x = band2[,1], 
                        y = band2[,2], 
                        easting = bands.map[,8],
                        northing = bands.map[,9],
                        band2 = band2[,3],
                        band3 = band3[,3],
                        band4 = band4[,3],
                        band5 = band5[,3],
                        ndvi = ndvi.value,
                        evi = evi.value,
                        ndwi = ndwi.value,
                        savi = savi.value)

ggplot(data = full.data, aes(x=x, y=y)) + geom_point(data=full.data, aes(color = ndvi))
# ggsave("plots/ndvi_test.png")

# Saving full.data to csv
write.csv(full.data, "C:/Users/cassa/Google Drive/MARSH project/Landsat ARD Data/CSamagalsky/full.data.rtk20150817.csv", row.names = TRUE)

# Creating long species data
species.map.long <- data.frame(species[2:9],
                          lat = long.lat.coor.serc$coords.x1,
                          lon = long.lat.coor.serc$coords.x2,
                          easting = utm.coor.serc$coords.x1,
                          northing = utm.coor.serc$coords.x2)

# Saving species.map.long to csv
write.csv(species.map.long, "C:/Users/cassa/Google Drive/MARSH project/Landsat ARD Data/CSamagalsky/species.map.long.rtk20150817.csv", row.names = TRUE)

# Creating empty training data frame
training <- data.frame(plot.id = numeric(561), easting = numeric(561), northing = numeric(561),
                       scam = numeric(561), ivfr = numeric(561), c4 = numeric(561), phau = numeric(561), 
                       spcy = numeric(561), tyla = numeric(561), dead= numeric(561), bare_water = numeric(561),
                       band2 = numeric(561), band3 = numeric(561), band4 = numeric(561), band5 = numeric(561), 
                       ndvi = numeric(561), evi = numeric(561), ndwi = numeric(561), savi = numeric(561))

count <- 0
for(i in 1:nrow(full.data)){
  for (j in 1:nrow(species.map.long)){
    if (species.map.long[j,11] - full.data[i,3] < 30 & species.map.long[j,11] - full.data[i,3] > 0 &
        species.map.long[j,12] - full.data[i,4] < 30 & species.map.long[j,12] - full.data[i,4] > 0){
      count = count + 1
      training$plot.id[count] <- i
      training[count,-c(1)] <- c(full.data[i,3:4],species.map.long[j,1:8], full.data[i,5:12])
    }
  }
}

paste("Yes count:", count)

# Saving training to csv
write.csv(training, "C:/Users/cassa/Google Drive/MARSH project/Landsat ARD Data/CSamagalsky/training.rtk20150817.csv", row.names = TRUE)
