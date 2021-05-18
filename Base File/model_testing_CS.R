# Test of Model - CS

# Aidan Draper
# August 16, 2018
# Testing Classifiers on Different Images

rm(list=ls())

library(raster)
library(magrittr)
library(randomForest)


### NEED TO ADD REFLECTANCE CONVERSION SECTION BEFORE TESTING IMAGES!!!


###GLOBAL PARAMETERS
image.date <- "07-16-2015" # try 07-02-2016 or 07-18-2016, 20150716
band.path <- "20150716" #find this line after the date in the .TIF band file names
landsat.path <- "G:/My Drive/MARSH project/Landsat ARD Data/Data Files"

z <- c(3,1,2)
date.format <- paste(strsplit(image.date,"-")[[1]][z], collapse="")

# import models to run on processed images later
setwd("G:/My Drive/MARSH project/Landsat ARD Data/Data Files")
load(file = "Classifiers/phau_model.rda")
phau_model <- model
rm(model)

load(file = "Classifiers/scam_model.rda")
scam_model <- model
rm(model)

load(file = "Classifiers/c4_model.rda")
c4_model <- model
rm(model)

load(file="serc_data.rdata")

# create our crop region layer
e <- as(extent(365375, 366400, 4303600, 4304800), 'SpatialPolygons')
crs(e) <- "+proj=utm +zone=18"

# IMAGE PROCESSING
# import bands, crop them to the SERC region and raster to a data frame
band2 <- paste(c(landsat.path,image.date,"/LC08_L1TP_015033_",date.format,"_",band.path,"_01_T1_B2.TIF"),collapse="") %>% raster() %>% crop(y = e) %>% rasterToPoints()
band3 <- paste(c(landsat.path,image.date,"/LC08_L1TP_015033_",date.format,"_",band.path,"_01_T1_B3.TIF"),collapse="") %>% raster() %>% crop(y = e) %>% rasterToPoints()
band4 <- paste(c(landsat.path,image.date,"/LC08_L1TP_015033_",date.format,"_",band.path,"_01_T1_B4.TIF"),collapse="") %>% raster() %>% crop(y = e) %>% rasterToPoints()
band5 <- paste(c(landsat.path,image.date,"/LC08_L1TP_015033_",date.format,"_",band.path,"_01_T1_B5.TIF"),collapse="") %>% raster() %>% crop(y = e) %>% rasterToPoints()
  
# covariates for model and dataset construction
evi.value <- 2.5 * ((band5[1,3] - band4[1,3]) / (((band4[1,3] * 6) + band5[1,3]) - ((7.5 * band2[1,3]) + 1)))
ndvi.value <- ((band5[,3] - band4[,3]) / (band5[,3] + band4[,3]))
ndwi.value <- (band3[,3] - band5[,3]) / (band3[,3] + band5[,3])
savi.value <- (1.5 * ((band5[,3] - band4[,3]) / (band5[,3] + band4[,3] + 1.5)))

full.data <- data.frame(x = band2[,1], 
                        y = band2[,2], 
                        band2 = band2[,3],
                        band3 = band3[,3],
                        band4 = band4[,3],
                        band5 = band5[,3],
                        ndvi = ndvi.value,
                        evi = evi.value,
                        ndwi = ndwi.value,
                        savi = savi.value)

# constrain data to our plot region
cut.data <- data.frame(plot.id = numeric(521), easting = numeric(521), northing = numeric(521),
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
      cut.data$plot.id[count] <- i
      cut.data[count,-c(1)] <- c(full.data[i,1:2],species.map[j,1:8], full.data[i,3:10])
    }
  }
}
paste("Yes count:", count) # is it 521?
nrow(cut.data) 


# predict the spatial resolutions
scam.pred <- predict(scam_model, newdata = cut.data)
table(scam.pred)

phau.pred <- predict(phau_model, newdata = cut.data)
table(phau.pred)

c4.pred <- predict(c4_model, newdata = cut.data)
table(c4.pred)

results <- data.frame(plot.id=cut.data$plot.id,easting=cut.data$easting,northing=cut.data$northing,phau.pred=as.vector(phau.pred),scam.pred=as.vector(scam.pred), c4.pred=as.vector(c4.pred))

save(results,file=paste(c("2016_Results/",image.date,"_results.rdata"),collapse=""))

