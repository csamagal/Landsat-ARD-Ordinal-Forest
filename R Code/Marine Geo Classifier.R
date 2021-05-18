# Classifying the data
# CSamagalsky
setwd("C:/Users/cassa/Google Drive/MARSH project/Landsat ARD Data/Data Files")
rm(list=ls())

# Functions needed:
# create.data() -> Data_function.R
# create.ord.model() -> Ordinal_forest_func.R
# run.ord.classifier() -> Ordinal_forest_func.R

# Libraries
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
library(dplyr)
library(ordinalForest)
library(stringr)

## Create Training Data
# Using create_data() function
bands.filepath <- "C:/Users/cassa/Google Drive/MARSH project/Landsat ARD Data/Data Files/20160702.csv"

# Grouped Marine Geo Files
years = 2015:2018 # Marine Geo only goes up to 2018
dir<-"C:/Users/cassa/Google Drive/MARSH project/Landsat ARD Data/Data Files/"

for(i in years){
  files<-list.files(path=dir, pattern=glue::glue('^{i}'), full.names = TRUE)
  training <- NULL
  for (file in files){
    bands.filepath<-file
    band.data <- create.data.mg(bands.filepath, years)
    training <- rbind(training, band.data)
    print(bands.filepath)
  }
  training
  form = paste('training', i, sep = '')
  write.csv(training, glue::glue("C:/Users/cassa/Google Drive/MARSH project/Landsat ARD Data/CSamagalsky/training data/{form}.csv"), row.names = TRUE)
}


## Run the model
# List of all Data files:
dir<-"C:/Users/cassa/Google Drive/MARSH project/Landsat ARD Data/CSamagalsky/training data"
training.files <- grep(list.files(path=dir,full.names = TRUE),  pattern='rtk', value=TRUE, invert=TRUE)
training.names <- c("training2015", "training2016","training2017", "training2018")

# Inputs:
seed.num <- 1000
curr.species <- "disp" # "ivfr" "phau" "sppa" "scam"
train.size = .8
ord_training <- read.csv(training.files[1])
cols.in.training = c(which(names(ord_training)==curr.species), 10:17)
perff.func <- "equal" # "proportional" "probability" "oneclass" "custom"
set.seed(seed.num)

# Classify all data for all species types
species.list <- c("disp", "ivfr", "phau", "sppa", "scam")

for(j in 1:length(species.list)){
  curr.species <- species.list[j]
  cols.in.training = c(which(names(ord_training)==curr.species), 10:17)
  
  for(i in 1:length(training.files)){
    # Factorizing training data
    ord_training <- read.csv(training.files[i])
    print(i)
    for (z in 5:9){
      ord_training[,z] <- factor(ord_training[,z], levels = c(0, 1, 2, 3, 4, 5, 6))
      ord_training[,z] <- as.ordered(ord_training[,z])
    }
    
    # Creating model and classifying
    ord_model <- create.ord.model(ord_training, curr.species, train.size, cols.in.training, perff.func, seed.num)
    print("Ord Model Completed")
    predict.model <- run.ord.classifier(ord_model, perff.func, ord_training, train.size, seed.num)
    print("Predictions Completed")
    
    # Saving model with each species in its own folder
    setwd(glue::glue("C:/Users/cassa/Google Drive/MARSH project/Landsat ARD Data/CSamagalsky/marine_geo/{curr.species}"))
    
    modelname <- paste(paste(curr.species, "model", training.names[i], sep="_"), ".RData", sep = "")
    save(ord_model, file = modelname)
    
    predictname <- paste(paste(curr.species, "predict", training.names[i], sep="_"), ".RData", sep = "")
    save(predict.model, file = predictname)
    
    print("Completed and Saved!")
  }
  print("And another species done")
}  


