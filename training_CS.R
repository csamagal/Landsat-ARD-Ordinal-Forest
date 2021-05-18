# Test of Model - CS

# Aidan Draper 
# R script to create full dataframe covariates for Random Forest classification
# June 13, 2018

rm(list=ls())

library(raster)
library(magrittr)
library(rgdal)
library(ggplot2)

# NEED TO ADD REFLECTANCE CONVERSION SECTION BEFORE TESTING IMAGES!!!
setwd("G:/My Drive/MARSH project/Landsat ARD Data/Data files")

# create our crop region layer
e <- as(extent(365375, 366400, 4303600, 4304800), 'SpatialPolygons')
crs(e) <- "+proj=utm +zone=18"

# import bands, crop them to the SERC region and raster to a data frame
bands_20150716 <- read.csv("G:/My Drive/MARSH project/Landsat ARD Data/Data Files/rtk20150716.csv")
band2 <- bands_20150716$Band2
band3 <- bands_20150716$Band3
band4 <- bands_20150716$Band4
band5 <- bands_20150716$Band5

# covariates for model and dataset construction
evi.value <- 2.5 * ((band5 - band4) / (((band4 * 6) + band5) - ((7.5 * band2) + 1)))
ndvi.value <- ((band5 - band4) / (band5 + band4))
ndwi.value <- (band3 - band5) / (band3 + band5)
savi.value <- (1.5 * ((band5 - band4) / (band5 + band4 + 1.5)))

full.data <- data.frame(x = bands_20150716$northing, 
                        y = bands_20150716$easting, 
                        band2 = band2,
                        band3 = band3,
                        band4 = band4,
                        band5 = band5,
                        ndvi = ndvi.value,
                        evi = evi.value,
                        ndwi = ndwi.value,
                        savi = savi.value)

ggplot(data = full.data, aes(x=x, y=y)) + geom_point(aes(color = ndvi))
# ggsave("plots/ndvi_test.png")

# compile the full dataframe to run

# Species Map Import 
#species <- read.csv("~/Documents/Junior_Year/DISC_REU/DISC_chesapeake/DominantSpPerPlot.csv")
species <- read.csv("G:/My Drive/MARSH project/Landsat ARD Data/Data Files/DominantSpPerPlot.csv")

# remove plots with no location, if any
species <- species[!is.na(species$easting),]
species <- species[!is.na(species$northing),]

# convert UTM to lat, long
utm.coor.serc <- SpatialPoints(cbind(species$easting,species$northing), 
                               proj4string=CRS("+proj=utm +zone=18"))

# Convert to lat/long
long.lat.coor.serc <- as.data.frame(spTransform(utm.coor.serc,CRS("+proj=longlat")))

species.map <- data.frame(species[2:9],
                          lat = long.lat.coor.serc$coords.x1,
                          lon = long.lat.coor.serc$coords.x2,
                          easting = utm.coor.serc$coords.x1,
                          northing = utm.coor.serc$coords.x2)

rm(species, long.lat.coor.serc, utm.coor.serc)


# Proper way to train dataset

training <- data.frame(plot.id = numeric(521), easting = numeric(521), northing = numeric(521),
                       scam = numeric(521), ivfr = numeric(521), c4 = numeric(521), phau = numeric(521), 
                       spcy = numeric(521), tyla = numeric(521), dead= numeric(521), bare_water = numeric(521),
                       band2 = numeric(521), band3 = numeric(521), band4 = numeric(521), band5 = numeric(521), 
                       ndvi = numeric(521), evi = numeric(521), ndwi = numeric(521), savi = numeric(521))

count <- 0
for(i in 1:nrow(full.data)){
  for (j in 1:nrow(species.map)){
    if (species.map[j,11] - full.data[i,1] < 30 & species.map[j,11] - full.data[i,1] > 0 &
        species.map[j,12] - full.data[i,2] < 30 & species.map[j,12] - full.data[i,2] > 0){
      count = count + 1
      training$plot.id[count] <- i
      training[count,-c(1)] <- c(full.data[i,1:2],species.map[j,1:8], full.data[i,3:10])
    }
  }
}
paste("Yes count:", count)
nrow(training)


#training <- data.frame(plot.id = numeric(1457), easting = numeric(1457), northing = numeric(1457), overlap = numeric(1457),
#                       scam = numeric(1457), ivfr = numeric(1457), c4 = numeric(1457), phau = numeric(1457), 
#                       spcy = numeric(1457), tyla = numeric(1457), dead= numeric(1457), bare_water = numeric(1457),
#                       band2 = numeric(1457), band3 = numeric(1457), band4 = numeric(1457), band5 = numeric(1457), 
#                       ndvi = numeric(1457), evi = numeric(1457), ndwi = numeric(1457), savi = numeric(1457))


# INCORRECT - way to calculate plot overlap 

#count <- 0
#for(i in 1:nrow(full.data)){
#  for (j in 1:nrow(species.map)){
#    if (species.map[j,11] - full.data[i,1] < 30 & species.map[j,11] - full.data[i,1] > -20 & 
#                  species.map[j,12] - full.data[i,2] < 30 & species.map[j,12] - full.data[i,2] > -20){
#      count = count + 1
#      training$plot.id[count] <- i 
#      training[count,-c(1,4)] <- c(full.data[i,1:2],species.map[j,1:8], full.data[i,3:10])
#      if(species.map[j,11] - full.data[i,1] < 0){
#        # starting x point is outside landast
#        x.overlap <- (species.map[j,11] + 20) - full.data[i,1]
#      } else {
#        x.overlap <- (full.data[i,1] + 30) - species.map[j,11]
#      }
#      if(species.map[j,12] - full.data[i,2] < 0){
#        # starting y point is outside landast
#        y.overlap <- (species.map[j,12] + 20) - full.data[i,2]
#      } else {
#        y.overlap <- (full.data[i,2] + 30) - species.map[j,12]
#      }
#      #cat("Overlap Area: ", (x.overlap * y.overlap) / 900, "\n")
#      training$overlap[count] <- (x.overlap * y.overlap) / (30*30) # SERC plot overlap area / Landsat area
#    }
#  }
#}
#paste("Yes count:", count)
#nrow(training)

# reassign ordinal values words
for (z in 4:11){
  training[which(is.na(training[,z])),z] <- "0%"
  training[which(training[,z] == 0),z] <- "less than 1%"
  training[which(training[,z] == 1),z] <- "1 - 5%"
  training[which(training[,z] == 2),z] <- "6% - 25%"
  training[which(training[,z] == 3),z] <- "26 - 50%"
  training[which(training[,z] == 4),z] <- "51 - 75%"
  training[which(training[,z] == 5),z] <- "76% - 100%"
  training[,z] <- as.factor(training[,z])
}

#save(training, file = '~/Documents/Junior_Year/DISC_REU/DISC_chesapeake/training_set.rda')

#graph current species under landsat plots
xmin <- (species.map$easting - min(training$easting))/30
xmax <- (species.map$easting - min(training$easting) + 10)/30
ymin <- (species.map$northing - min(training$northing))/30
ymax <- (species.map$northing - min(training$northing) + 10)/30


ggplot() + 
  geom_rect(data=species.map, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill = as.factor(unlist(species.map[4]))), color=NA) +
  labs(title=paste("Population Abundance"), x="X (30m increment)", y="Y (30m increment)", fill = "Cover") #+ 
  geom_rect(data=training, aes(xmin=(easting - min(training$easting))/30, xmax=(easting - min(training$easting) + 30)/30, ymin=(northing-min(training$northing))/30, ymax=(northing + 30 - min(training$northing))/30), color="black", fill = NA)

for(sp in 1:8){
  species.map.long[which(is.na(species.map.long[,sp])),sp] <- "0%"
  species.map.long[which(species.map.long[,sp] == 0),sp] <- "less than 1%"
  species.map.long[which(species.map.long[,sp] == 1),sp] <- "1 - 5%"
  species.map.long[which(species.map.long[,sp] == 2),sp] <- "6% - 25%"
  species.map.long[which(species.map.long[,sp] == 3),sp] <- "26 - 50%"
  species.map.long[which(species.map.long[,sp] == 4),sp] <- "51 - 75%"
  species.map.long[which(species.map.long[,sp] == 5),sp] <- "76% - 100%"
  species.map.long[,sp] <- as.factor(species.map.long[,sp])
}
# for validation after model run

species.map$scam <- factor(species.map$scam, levels = c("0%", "less than 1%", "1 - 5%", "6% - 25%", "26 - 50%", "51 - 75%", "76% - 100%"))
#cols = c("0%"="#ceb467", "less than 1%" = "#ace5b2", "1 - 5%" = "#7aef87", "6% - 25%" = "#57d165", "26 - 50%" = "#21a31d", "51 - 75%" = "#197f24", "76% - 100%" = "#115118")

# red scale for Phrag
#cols = c("0%"="#ceb467", "less than 1%" = "#fce0e0", "1 - 5%" = "#ffa8a8", "6% - 25%" = "#f26a6a", "26 - 50%" = "#e83e3e", "51 - 75%" = "#c12020", "76% - 100%" = "#891818")

# blue scale for scam
cols = c("0%"="#ceb467", "less than 1%" = "#d1ddfc", "1 - 5%" = "#adc0f4", "6% - 25%" = "#8ba6f4", "26 - 50%" = "#5e82ed", "51 - 75%" = "#2c2ccc", "76% - 100%" = "#231572")



serc.plots <- ggplot() + 
  geom_rect(data=species.map, aes(xmin=easting, xmax=easting + 10, ymin=northing, ymax=northing + 10, fill = as.factor(unlist(species.map[1]))), color=NA) +
  labs(title=paste("scam Population Abundance in SERC Plots"), x="Easting", y="Northing", fill = "Cover") + 
  geom_rect(data=training, aes(xmin=easting, xmax=easting + 30, ymin=northing, ymax=northing + 30), color="black", fill = NA) +
  scale_x_continuous(limits = c(365430, 366280)) +
  scale_y_continuous(limits = c(4303800, 4304470)) +
  scale_fill_manual(values = cols) +
  theme(plot.background = element_rect(fill = 'white'),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

serc.plots

#ggsave("plots/phau_prediction_validation.png")

