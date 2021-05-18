### Spectral Signal Model - Mixing Model
## Casey Samagalsky

# Goal: Obtain a spectral signal for each species
# Data required: full.data, species.map

library(randomForest)
library(dplyr)

full.data <- read.csv()
species.map <- read.csv()

train.size = .8
cols.in.training <- c(4:12)
seed.num = 2000
set.seed(seed.num)

# Create species data from full.data and species.map (which has column species)
training.species <- data.frame(plot.id = numeric(561), easting = numeric(561), northing = numeric(561),
                               species = numeric(561), band2 = numeric(561), band3 = numeric(561), band4 = numeric(561), band5 = numeric(561), 
                               ndvi = numeric(561), evi = numeric(561), ndwi = numeric(561), savi = numeric(561))

count <- 0
for(i in 1:nrow(full.data)){
  for (j in 1:nrow(species.map)){
    if (species.map[j,4] - full.data[i,3] < 30 & species.map[j,4] - full.data[i,3] > 0 &
        species.map[j,5] - full.data[i,4] < 30 & species.map[j,5] - full.data[i,4] > 0){
      count = count + 1
      training.species$plot.id[count] <- i
      training.species[count,-c(1)] <- c(full.data[i,3:4], species.map[j,1], full.data[i,5:12])
    }
  }
}
paste("Yes count:", count)

training.species$species <- factor(training.species$species)

# Creating Majority Species Factor
species <- c("c4", "scam", "ivfr", "phau", "spcy", "tyla", "no species")
training.species$majority <- numeric(nrow(training.species))
training.species$majority[training.species$species %in% species] <-"1"
training.species$majority[!(training.species$species %in% species)] <-"0"

# Create Test and Training Data - training on majority plots, testing on mixed plots
train.sp <- training.species %>% dplyr::filter(majority == 1)
test.sp <- training.species %>% dplyr::filter(majority == 0)
train.sp$species <- factor(train.sp$species)


# Run random forest model
model_species <- randomForest(train.sp$species~., mtry = 4, ntree = 100, data = train.sp[,cols.in.training], keep.forest=TRUE)

plot(model_species, main="", cex.main=2, cex.lab=1.4, cex.axis=1.4)
varImpPlot(model_species, main="", cex.main=2, cex.lab=1.4, cex.axis=1.4)

# Predict the Model
pred.sp <- predict(model_species, newdata = test.sp)
table(pred.sp, test.sp[,4])


# Species Accuracy 
# Avg oob
mean(model_species$err.rate[,1]) #0.5295024

# Model accuracy over a number of trials
seed.range <- as.integer(sample(100:10000, 100))
sum.of.oob <- 0

for (x in seed.range){
  set.seed(x)
  train.sp <- training.species %>% dplyr::filter(majority == 1)
  test.sp <- training.species %>% dplyr::filter(majority == 0)
  train.sp$species <- factor(train.sp$species)
  
  model_species <- randomForest(train.sp$species~., data = train.sp[,cols.in.training], keep.forest=TRUE)
  sum.of.oob = sum.of.oob + mean(model_species$err.rate[,1])
  if (x != seed.range[100]){
    rm(model_species)
  }
}

avg.oob <- sum.of.oob/length(seed.range)
avg.oob #0.5208506


# Accuracy based on species type
true.pred.species <- data.frame(table(true_values=test.sp$species, predictions=pred.sp))

levels <- as.factor(levels(training.species$species))
species.error <- data.frame(species = numeric(length(levels)), correct = numeric(length(levels)), total = numeric(length(levels)), error = numeric(length(levels)))
species.error$species <- levels

for(i in 1:nrow(true.pred.species)){
  # Correctness
  if(true.pred.species[i,2]==true.pred.species[i,1]){
    species.error[ as.character(species.error[,1]) == as.character(true.pred.species[i, 2]), 2] <- 
      species.error[ as.character(species.error[,1]) == as.character(true.pred.species[i, 2]), 2] + true.pred.species[i,3]
  }
  # Totals
  species.error[ as.character(species.error[,1]) == as.character(true.pred.species[i, 2]), 3] <- 
    species.error[ as.character(species.error[,1]) == as.character(true.pred.species[i, 2]), 3] + true.pred.species[i,3]
}


# Calculating Error
for(i in 1:nrow(species.error)){
  if(species.error[i,3] != 0){
    species.error[i,4] <- (species.error[i,3] - species.error[i,2])/species.error[i,3]
  }else{
    species.error[i,4] <- NA
  }
}

# Best predicted species
species.error[order(species.error$error),] # scam, phau, c4 scam, c4


