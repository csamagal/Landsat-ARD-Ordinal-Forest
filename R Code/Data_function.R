## Creating data to fun through random forest models
# Casey Samagalsky
setwd("C:/Users/cassa/Google Drive/MARSH project/Landsat ARD Data/Data Files")
rm(list=ls())

library(raster)
library(rgdal)
library(dplyr)
library(sp)
library(maps)
library(ggplot2)
library(ggmap)
library(magrittr)
library(randomForest)
library(gridExtra)
#devtools::install_github('skinner927/reprtree')
library(reprtree)

# Import
bands.filepath <- ("C:/Users/cassa/Google Drive/MARSH project/Landsat ARD Data/Data Files/20150918.csv")
species.RTK <- read.csv("C:/Users/cassa/Google Drive/MARSH project/Landsat ARD Data/Data Files/DominantSpPerPlot.csv")

# Create data function for Marine Geo data
create.data.mg<- function(bands.filepath, years){
  
  # Creating band data
  bands <- read.csv(bands.filepath)
  
  # Link gps points to band data
  gps.points <- read.csv("C:/Users/cassa/Google Drive/MARSH project/Landsat ARD Data/Data Files/LANDSAT GPS points.csv")[c(2,4,5)]
  bands <- bands %>% left_join(gps.points, by="Name")
  
  # Cont Band Data
  utm.bands <- SpatialPoints(cbind(bands$easting, bands$northing), proj4string=CRS("+proj=utm +zone=18"))
  long.lat.bands <- as.data.frame(spTransform(utm.bands,CRS("+proj=longlat")))
  bands.map <- data.frame(subset(bands, select=c(Band1, Band2, Band3, Band4, 
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
  
  year <- as.numeric(str_sub(bands.filepath,-12,-9))
  
  # Getting species data
  species <- read.csv(glue::glue("C:/Users/cassa/Google Drive/MARSH project/Landsat ARD Data/Data Files/mg_cover_{year}.csv"))
  
  ## Creating Species Data
  species <- species[!is.na(species$latitude),]
  species <- species[!is.na(species$longitude),]
  
  # convert lat, long to UTM
  long.lat.coor.serc <- SpatialPoints(cbind(species$longitude, species$latitude),
                                      proj4string=CRS("+proj=longlat"))
  utm.coor.serc <- as.data.frame(spTransform(long.lat.coor.serc, CRS("+proj=utm +zone=18 +datum=WGS84")))

  ## ndvi
  # reflection of vegetation (using NDVI)
  ndvi <- (band5[,3]-band4[,3])/(band5[,3]+band4[,3])
  
  # Workaround to get correct coordinate values
  ndvi.points <- data.frame(layer=ndvi, lat=long.lat.bands$coords.x1, lon=long.lat.bands$coords.x2, 
                            easting = utm.bands$coords.x1, northing = utm.bands$coords.x2)
  
  # constrain the ndvi points to the plot region
  ndvi.points <- ndvi.points[which(ndvi.points$lat > -76.555 & ndvi.points$lat < -76.537),]
  ndvi.points <- ndvi.points[which(ndvi.points$lon > 38.8725 & ndvi.points$lon < 38.879),]
  
  
  ### Create Training Data
  # create our crop region layer
  e <- as(extent(365375, 366400, 4303600, 4304800), 'SpatialPolygons')
  crs(e) <- "+proj=utm +zone=18"
  
  # covariates for model and dataset construction
  evi.value <- 2.5 * ((band5[,3] - band4[,3]) / (((band4[,3] * 6) + band5[,3]) - ((7.5 * band2[,3]) + 1)))
  ndvi.value <- ((band5[,3] - band4[,3]) / (band5[,3] + band4[,3]))
  ndwi.value <- (band3[,3] - band5[,3]) / (band3[,3] + band5[,3])
  savi.value <- (1.5 * ((band5[,3] - band4[,3]) / (band5[,3] + band4[,3] + 1.5)))
  
  full.data <- data.frame(x = band2[,1], y = band2[,2], easting = bands.map[,8], northing = bands.map[,9], band2 = band2[,3],
                          band3 = band3[,3], band4 = band4[,3], band5 = band5[,3], ndvi = ndvi.value, evi = evi.value, ndwi = ndwi.value,
                          savi = savi.value)
  
  # Creating long species data
  species.map.long <- data.frame(species[7:11], lat = species$latitude, lon = species$longitude,
                                 easting = utm.coor.serc$coords.x1, northing = utm.coor.serc$coords.x2)
  
  # Creating empty training data frame
  training <- data.frame(plot.id = numeric(127), easting = numeric(127), northing = numeric(127),
                         disp = numeric(127), ivfr = numeric(127), phau = numeric(127), scam = numeric(127), 
                         sppa = numeric(127), band2 = numeric(127), band3 = numeric(127), band4 = numeric(127), 
                         band5 = numeric(127), ndvi = numeric(127), evi = numeric(127), ndwi = numeric(127), 
                         savi = numeric(127))
  
  count <- 0
  for(i in 1:nrow(full.data)){
    for (j in 1:nrow(species.map.long)){
      if (species.map.long[j,8] - full.data[i,3] < 30 & species.map.long[j,8] - full.data[i,3] > 0 &
          species.map.long[j,9] - full.data[i,4] < 30 & species.map.long[j,9] - full.data[i,4] > 0){
        count = count + 1
        training$plot.id[count] <- i
        training[count,-c(1)] <- c(full.data[i,3:4],species.map.long[j,1:5], full.data[i,5:12])
      }
    }
  }
  
  return(training)
}

create.data.rtk <- function(bands.filepath, species){
  
  # Creating band data
  bands <- read.csv(bands.filepath)
  
  # Cont Band Data
  utm.bands <- SpatialPoints(cbind(bands$easting, bands$northing), proj4string=CRS("+proj=utm +zone=18"))
  long.lat.bands <- as.data.frame(spTransform(utm.bands,CRS("+proj=longlat")))
  bands.map <- data.frame(subset(bands, select=c(Band1, Band2, Band3, Band4, 
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
  
  
  ## Creating Species Data
  species <- species[!is.na(species$easting),]
  species <- species[!is.na(species$northing),]
  
  # convert UTM to lat, long
  utm.coor.serc <- SpatialPoints(cbind(species$easting,species$northing), proj4string=CRS("+proj=utm +zone=18"))
  # Convert to lat/long
  long.lat.coor.serc <- as.data.frame(spTransform(utm.coor.serc, CRS("+proj=longlat")))
  
  species.map <- data.frame(species = species$simplifiedClass, lat = long.lat.coor.serc$coords.x1, lon = long.lat.coor.serc$coords.x2, 
                            easting = utm.coor.serc$coords.x1, northing = utm.coor.serc$coords.x2)
  species.map[which(species.map$species == ""),1] <- "no species"
  
  ## ndvi
  # reflection of vegetation (using NDVI)
  ndvi <- (band5[,3]-band4[,3])/(band5[,3]+band4[,3])
  
  # Workaround to get correct coordinate values
  ndvi.points <- data.frame(layer=ndvi, lat=long.lat.bands$coords.x1, lon=long.lat.bands$coords.x2, 
                            easting = utm.bands$coords.x1, northing = utm.bands$coords.x2)
  
  # constrain the ndvi points to the plot region
  ndvi.points <- ndvi.points[which(ndvi.points$lat > -76.555 & ndvi.points$lat < -76.537),]
  ndvi.points <- ndvi.points[which(ndvi.points$lon > 38.8725 & ndvi.points$lon < 38.879),]
  
  
  ### Create Training Data
  # create our crop region layer
  e <- as(extent(365375, 366400, 4303600, 4304800), 'SpatialPolygons')
  crs(e) <- "+proj=utm +zone=18"
  
  # covariates for model and dataset construction
  evi.value <- 2.5 * ((band5[,3] - band4[,3]) / (((band4[,3] * 6) + band5[,3]) - ((7.5 * band2[,3]) + 1)))
  ndvi.value <- ((band5[,3] - band4[,3]) / (band5[,3] + band4[,3]))
  ndwi.value <- (band3[,3] - band5[,3]) / (band3[,3] + band5[,3])
  savi.value <- (1.5 * ((band5[,3] - band4[,3]) / (band5[,3] + band4[,3] + 1.5)))
  
  full.data <- data.frame(x = band2[,1], y = band2[,2], easting = bands.map[,8], northing = bands.map[,9], band2 = band2[,3],
                          band3 = band3[,3], band4 = band4[,3], band5 = band5[,3], ndvi = ndvi.value, evi = evi.value, ndwi = ndwi.value,
                          savi = savi.value)
  
  # Creating long species data
  species.map.long <- data.frame(species[2:9], lat = long.lat.coor.serc$coords.x1, lon = long.lat.coor.serc$coords.x2,
                                 easting = utm.coor.serc$coords.x1, northing = utm.coor.serc$coords.x2)
  
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
  
  # Recode RTK data
  for (z in 4:11){
    training[which(training[,z] == 5),z] <- 6
    training[which(training[,z] == 4),z] <- 5
    training[which(training[,z] == 3),z] <- 4
    training[which(training[,z] == 2),z] <- 3
    training[which(training[,z] == 1),z] <- 2
    training[which(training[,z] == 0),z] <- 1
    training[which(is.na(training[,z])),z] <- 0
  }
  
  return(training)
}


training <- create.data.rtk(bands.filepath, species.RTK)
write.csv(training, "C:/Users/cassa/Google Drive/MARSH project/Landsat ARD Data/CSamagalsky/training.rtk20150817.csv", row.names = TRUE)


# Adding all RTK data to one training file
dir<-"C:/Users/cassa/Google Drive/MARSH project/Landsat ARD Data/Data Files/"
files<-list.files(path=dir, pattern='rtk2019', full.names = TRUE)
files
training.rtk2019 <- NULL

for (file in files){
  bands.filepath<-file
  band.data <- create.data(bands.filepath, species)
  training.rtk2019 <- rbind(training.rtk2019, band.data)
  print(bands.filepath)
}
training.rtk2019
write.csv(training.rtk2019, "C:/Users/cassa/Google Drive/MARSH project/Landsat ARD Data/CSamagalsky/training.rtk2019", row.names = TRUE)




# Creating non-RTK data
bands.filepath <- "C:/Users/cassa/Google Drive/MARSH project/Landsat ARD Data/Data Files/20150716.csv"
#gps.points <- read.csv("C:/Users/cassa/Google Drive/MARSH project/Landsat ARD Data/LANDSAT GPS points.csv")[c(2,4,5)]
#bands.merge <- bands %>% left_join(gps.points, by="Name")

training.n <- create.data(bands.filepath, species, RTK=FALSE)

# In a loop:
dir<-"C:/Users/cassa/Google Drive/MARSH project/Landsat ARD Data/Data Files/"
files<-list.files(path=dir, pattern='^2016', full.names = TRUE)
files
training.2016 <- NULL

for (file in files){
  bands.filepath<-file
  band.data <- create.data(bands.filepath, species, RTK = FALSE)
  training.2016 <- rbind(training.2016, band.data)
  print(bands.filepath)
}
training.2016 <- create.data(files, species, RTK = FALSE)
write.csv(training.2016, "C:/Users/cassa/Google Drive/MARSH project/Landsat ARD Data/CSamagalsky/training.2016", row.names = TRUE)
