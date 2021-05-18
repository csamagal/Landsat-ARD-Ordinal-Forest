### Spectral Signal Models
## Casey Samagalsky

# Goal: Obtain a spectral signal for each species
# Data required: full.data, species.map

setwd("C:/Users/cassa/Google Drive/MARSH project/Landsat ARD Data/Data Files")

library(randomForest)
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

# Saving training.species to csv
write.csv(training, "C:/Users/cassa/Google Drive/MARSH project/Landsat ARD Data/CSamagalsky/training.species.rtk20150817.csv", row.names = TRUE)


# Create Test and Training Data
samp.sp <- sample(nrow(training.species), train.size * nrow(training.species))
train.sp <- droplevels(training.species[samp.sp,])
test.sp <- droplevels(training.species[-samp.sp,])

# Run random forest model
model_species <- randomForest(train.sp$species~., data = train.sp[,cols.in.training], keep.forest=TRUE)

plot(model_species, main="", cex.main=2, cex.lab=1.4, cex.axis=1.4)
varImpPlot(model_species, main="", cex.main=2, cex.lab=1.4, cex.axis=1.4)

# Predict the Model
pred.sp <- predict(model_species, newdata = test.sp)
table(pred.sp, test.sp[,4])


# Species Accuracy 
# Avg oob
mean(model_species$err.rate[,1]) #0.5891945

# Model accuracy over a number of trials
seed.range <- as.integer(sample(100:10000, 100))
sum.of.oob <- 0

for (x in seed.range){
  set.seed(x)
  samp.sp <- sample(nrow(training.species), train.size * nrow(training.species))
  train.sp <- droplevels(training.species[samp.sp,])
  test.sp <- droplevels(training.species[-samp.sp,])
  
  model_species <- randomForest(train.sp$species~., data = train.sp[,cols.in.training], keep.forest=TRUE)
  sum.of.oob = sum.of.oob + mean(model_species$err.rate[,1])
  if (x != seed.range[100]){
    rm(model_species)
  }
}

avg.oob <- sum.of.oob/length(seed.range)
avg.oob #0.6198219


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



## Spectral Signal Groupings
# C3/C4 Spectral Signal Model (Group 1)

# Data
training.sp.1 <- training.species
training.sp.1$pathway <- numeric(nrow(training.species))

# Creating Pathway variable from species
c4 <- c("c4", "c4 ivfr", "c4 ivfr scam", "ivfr", "ivfr phau", "ivfr scam", "ivfr spcy", "phau", "spcy")
c3 <- c("scam", "tyla", "scam tyla")
mix <- c("scam spcy", "c4 scam")

training.sp.1$pathway[training.sp.1$species %in% c3] <-"c3"
training.sp.1$pathway[training.sp.1$species %in% mix] <-"mix"
training.sp.1$pathway[training.sp.1$species %in% c4] <-"c4"
training.sp.1$pathway[training.sp.1$species == "no species"] <-"no species"

training.sp.1$pathway <- as.factor(training.sp.1$pathway)
training.sp.1 <- training.sp.1[-4]

# Create Test and Training Data
samp.sp.1 <- sample(nrow(training.sp.1), train.size * nrow(training.sp.1))
train.sp.1 <- droplevels(training.sp.1[samp.sp.1,])
test.sp.1 <- droplevels(training.sp.1[-samp.sp.1,])

# Run random forest model
model_species.1 <- randomForest(train.sp.1$pathway~., data = train.sp.1[,cols.in.training], keep.forest=TRUE)

plot(model_species.1, main="", cex.main=2, cex.lab=1.4, cex.axis=1.4)
varImpPlot(model_species.1, main="", cex.main=2, cex.lab=1.4, cex.axis=1.4)

# Predict the Model
pred.sp.1 <- predict(model_species.1, newdata = test.sp.1)
table(pred.sp.1, test.sp.1[,12])


# Model Accuracy 
# Avg oob
mean(model_species.1$err.rate[,1]) #0.2405985

# Model accuracy over a number of trials
seed.range <- as.integer(sample(100:10000, 100))
sum.of.oob <- 0

for (x in seed.range){
  set.seed(x)
  samp.sp.1 <- sample(nrow(training.sp.1), train.size * nrow(training.sp.1))
  train.sp.1 <- droplevels(training.sp.1[samp.sp.1,])
  test.sp.1 <- droplevels(training.sp.1[-samp.sp.1,])
  
  model_species.1 <- randomForest(train.sp.1$pathway~., data = train.sp.1[,cols.in.training], keep.forest=TRUE)
  sum.of.oob = sum.of.oob + mean(model_species.1$err.rate[,1])
  if (x != seed.range[100]){
    rm(model_species.1)
  }
}

avg.oob <- sum.of.oob/length(seed.range)
avg.oob #0.2319895


# Accuracy based on pathway
true.pred.1 <- data.frame(table(true_values=test.sp.1$pathway, predictions=pred.sp.1))

levels.1 <- as.factor(levels(training.sp.1$pathway))
species.error.1 <- data.frame(pathway = numeric(length(levels.1)), correct = numeric(length(levels.1)), total = numeric(length(levels.1)), error = numeric(length(levels.1)))
species.error.1$pathway <- levels.1

for(i in 1:nrow(true.pred.1)){
  # Correctness
  if(true.pred.1[i,2]==true.pred.1[i,1]){
    species.error.1[ as.character(species.error.1[,1]) == as.character(true.pred.1[i, 2]), 2] <- 
      species.error.1[ as.character(species.error.1[,1]) == as.character(true.pred.1[i, 2]), 2] + true.pred.1[i,3]
  }
  # Totals
  species.error.1[ as.character(species.error.1[,1]) == as.character(true.pred.1[i, 2]), 3] <- 
    species.error.1[ as.character(species.error.1[,1]) == as.character(true.pred.1[i, 2]), 3] + true.pred.1[i,3]
}


# Calculating Error
for(i in 1:nrow(species.error.1)){
  if(species.error.1[i,3] != 0){
    species.error.1[i,4] <- (species.error.1[i,3] - species.error.1[i,2])/species.error.1[i,3]
  }else{
    species.error.1[i,4] <- NA
  }
}

# Best predicted pathway
species.error.1[order(species.error.1$error),] # c4, c3, mix, no species






# C3/C4 Spectral Signal Model (Group 2)

# Data
training.sp.2 <- training.species
training.sp.2$pathway <- numeric(nrow(training.species))

# Creating Pathway variable from species
c4.2 <- c("c4",  "ivfr phau",  "phau", "spcy")
c3.2 <- c("scam", "tyla", "scam tyla", "c4 ivfr scam", "ivfr", "ivfr scam", "ivfr spcy")
mix.2 <- c("scam spcy", "c4 scam", "c4 ivfr")

training.sp.2$pathway[training.sp.2$species %in% c3.2] <-"c3"
training.sp.2$pathway[training.sp.2$species %in% mix.2] <-"mix"
training.sp.2$pathway[training.sp.2$species %in% c4.2] <-"c4"
training.sp.2$pathway[training.sp.2$species == "no species"] <-"no species"

training.sp.2$pathway <- as.factor(training.sp.2$pathway)
training.sp.2 <- training.sp.2[-4]

# Create Test and Training Data
samp.sp.2 <- sample(nrow(training.sp.2), train.size * nrow(training.sp.2))
train.sp.2 <- droplevels(training.sp.2[samp.sp.2,])
test.sp.2 <- droplevels(training.sp.2[-samp.sp.2,])

# Run random forest model
model_species.2 <- randomForest(train.sp.2$pathway~., data = train.sp.2[,cols.in.training], keep.forest=TRUE)

plot(model_species.2, main="", cex.main=2, cex.lab=1.4, cex.axis=1.4)
varImpPlot(model_species.2, main="", cex.main=2, cex.lab=1.4, cex.axis=1.4)

# Predict the Model
pred.sp.2 <- predict(model_species.2, newdata = test.sp.2)
table(pred.sp.2, test.sp.2[,12])


# Model Accuracy 
# Avg oob
mean(model_species.2$err.rate[,1]) #0.4764575

# Model accuracy over a number of trials
seed.range <- as.integer(sample(100:10000, 100))
sum.of.oob <- 0

for (x in seed.range){
  set.seed(x)
  samp.sp.2 <- sample(nrow(training.sp.2), train.size * nrow(training.sp.2))
  train.sp.2 <- droplevels(training.sp.2[samp.sp.2,])
  test.sp.2 <- droplevels(training.sp.2[-samp.sp.2,])
  
  model_species.2 <- randomForest(train.sp.2$pathway~., data = train.sp.2[,cols.in.training], keep.forest=TRUE)
  sum.of.oob = sum.of.oob + mean(model_species.2$err.rate[,1])
  if (x != seed.range[100]){
    rm(model_species.2)
  }
}

avg.oob <- sum.of.oob/length(seed.range)
avg.oob #0.4735272


# Accuracy based on pathway
true.pred.2 <- data.frame(table(true_values=test.sp.2$pathway, predictions=pred.sp.2))

levels.2 <- as.factor(levels(training.sp.2$pathway))
species.error.2 <- data.frame(pathway = numeric(length(levels.2)), correct = numeric(length(levels.2)), total = numeric(length(levels.2)), error = numeric(length(levels.2)))
species.error.2$pathway <- levels.2

for(i in 1:nrow(true.pred.2)){
  # Correctness
  if(true.pred.2[i,2]==true.pred.2[i,1]){
    species.error.2[ as.character(species.error.2[,1]) == as.character(true.pred.2[i, 2]), 2] <- 
      species.error.2[ as.character(species.error.2[,1]) == as.character(true.pred.2[i, 2]), 2] + true.pred.2[i,3]
  }
  # Totals
  species.error.2[ as.character(species.error.2[,1]) == as.character(true.pred.2[i, 2]), 3] <- 
    species.error.2[ as.character(species.error.2[,1]) == as.character(true.pred.2[i, 2]), 3] + true.pred.2[i,3]
}


# Calculating Error
for(i in 1:nrow(species.error.2)){
  if(species.error.2[i,3] != 0){
    species.error.2[i,4] <- (species.error.2[i,3] - species.error.2[i,2])/species.error.2[i,3]
  }else{
    species.error.2[i,4] <- NA
  }
}

# Best predicted pathway
species.error.2[order(species.error.2$error),] # c4, c3, mix, no species
