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

# Grouped RTK Files
  years <- 2015:2019
  species.RTK <- read.csv("C:/Users/cassa/Google Drive/MARSH project/Landsat ARD Data/Data Files/DominantSpPerPlot.csv")
  dir<-"C:/Users/cassa/Google Drive/MARSH project/Landsat ARD Data/Data Files/"
  
  for(i in years){
    files<-list.files(path=dir, pattern=(glue::glue('rtk{i}')), full.names = TRUE)
    training.rtk <- NULL
    for (file in files){
      bands.filepath<-file
      band.data <- create.data.rtk(bands.filepath, species.RTK)
      training.rtk <- rbind(training.rtk, band.data)
      print(bands.filepath)
    }
    training.rtk
    form = paste('training', i, 'rtk', sep = '')
    write.csv(training.rtk, glue::glue("C:/Users/cassa/Google Drive/MARSH project/Landsat ARD Data/CSamagalsky/training data/{form}.csv"), row.names = TRUE)
  }
  


## Run the model
# List of all Data files:
  dir<-"C:/Users/cassa/Google Drive/MARSH project/Landsat ARD Data/CSamagalsky/training data"
  training.files <- list.files(path=dir, pattern='rtk', full.names = TRUE)
  training.names <- c("training2015rtk", "training2016rtk", "training2017rtk", "training2018rtk", "training2019rtk")
  
# Inputs:
  seed.num <- 1000
  curr.species <- "c4" # "ivfr" "c4" "phau" "spcy" "tyla" "scam"
  train.size = .8
  ord_training <- read.csv(training.files[1])
  cols.in.training = c(which(names(ord_training)==curr.species), 13:20)
  perff.func <- "equal" # "proportional" "probability" "oneclass" "custom"
  set.seed(seed.num)
  

# Classify all data for all species types
  species.list <- c("phau", "spcy", "tyla", "c4", "scam", "ivfr")
  
for(j in 1:length(species.list)){
  curr.species <- species.list[j]
  cols.in.training = c(which(names(ord_training)==curr.species), 12:19)
  
  for(i in 1:length(training.files)){
  # Factorizing training data
    ord_training <- read.csv(training.files[i])
    print(i)
    for (z in 5:12){
      ord_training[,z] <- factor(ord_training[,z], levels = c(0, 1, 2, 3, 4, 5, 6))
      ord_training[,z] <- as.ordered(ord_training[,z])
    }
  
  # Creating model and classifying
    ord_model <- create.ord.model(ord_training, curr.species, train.size, cols.in.training, perff.func, seed.num)
    print("Ord Model Completed")
    predict.model <- run.ord.classifier(ord_model, perff.func, ord_training, train.size, seed.num)
    print("Predictions Completed")
    
  # Saving model with each species in its own folder
    setwd(glue::glue("C:/Users/cassa/Google Drive/MARSH project/Landsat ARD Data/CSamagalsky/rtk/{curr.species}"))
    
    modelname <- paste(paste(curr.species, "model", training.names[i], sep="_"), ".RData", sep = "")
    save(ord_model, file = modelname)
    
    predictname <- paste(paste(curr.species, "predict", training.names[i], sep="_"), ".RData", sep = "")
    save(predict.model, file = predictname)
    
    print("Completed and Saved!")
  }
  print("And another species done")
}  
