### Ordinal Model Function
library(ordinalForest)

# Inputs:
seed.num <- 1000
curr.species <- "scam" # "ivfr" "c4" "phau" "spcy" "tyla" 
train.size = .8
cols.in.training = c(which(names(ord_training)==curr.species), 13:20)
perff.func <- "equal" # "proportional" "probability" "oneclass" "custom"
set.seed(seed.num)

# Data:
ord_training <- training
for (z in 5:12){
  ord_training[which(is.na(ord_training[,z])),z] <- 0
  ord_training[,z] <- as.ordered(ord_training[,z])
}

# Creating the model with new parameters
model <- create.ord.model(ord_training, curr.species, train.size, cols.in.training, perff.func)

# Classifying data with new model
create.ord.model <- function(ord_training, curr.species, train.size, cols.in.training, perff.func, seed.num){
  set.seed(seed.num)
  
  # Creating test and training data
  current_ord <- ord_training[,which(names(ord_training)==curr.species)]
  samp_ord <- sample(nrow(ord_training), train.size * nrow(ord_training))
  train_ord <- droplevels(ord_training[samp_ord,])
  test_ord <- droplevels(ord_training[-samp_ord,])
  
  # Running Ordinal Forest model
  if(perff.func == "custom"){
    weight.list <- rexp(100)
    level.number <- nlevels(train_ord$scam)
    weights <- sort(sample(weight.list, level.number), decreasing=TRUE)
    
    ord_model <- ordfor(depvar=curr.species, nsets=1000, data=na.omit(train_ord[,cols.in.training]), perffunction = "custom", classweights = weights)
    
  }else{
    ord_model <- ordfor(depvar=curr.species, nsets=1000, data=na.omit(train_ord[,cols.in.training]), perffunction = perff.func)
  }

  return(ord_model)
}

run.ord.classifier <- function(ord_model, perff.func, ord_training, train.size, seed.num){
  set.seed(seed.num)
  
  # Recreating train/test data
  current_ord <- ord_training[,which(names(ord_training)==curr.species)]
  samp_ord <- sample(nrow(ord_training), train.size * nrow(ord_training))
  train_ord <- droplevels(ord_training[samp_ord,])
  test_ord <- droplevels(ord_training[-samp_ord,])
  
  # Prediction
  test_ord[,which(names(test_ord)==curr.species)] <- as.ordered(factor(test_ord[,which(names(test_ord)==curr.species)], levels=c(0, 1, 2, 3, 4, 5, 6)))
  pred2 <- predict(ord_model, newdata = test_ord)
  pred2$ypred <- as.ordered(factor(pred2$ypred, levels=c(0, 1, 2, 3, 4, 5, 6)))
  
  # Performance Functions to get Errors
  # if(perff.func == "custom"){
  #   error <-perff_custom(ytest=test_ord[,which(names(test_ord)==curr.species)], ytestpred=pred2$ypred, classweights = weights)
  # }else if(perff.func == "equal"){
  #   error <- perff_equal(ytest=test_ord[,which(names(test_ord)==curr.species)], ytestpred=pred2$ypred) 
  # }else if(perff.func == "proportional"){
  #   error <-perff_proportional(ytest=test_ord[,which(names(test_ord)==curr.species)], ytestpred=pred2$ypred)
  # }else{
  #   error <- perff_oneclass(ytest=test_ord[,which(names(test_ord)==curr.species)], ytestpred=pred2$ypred, categ = "0")  
  # }
  
  return.list <- list("ordforpred" = pred2) #, "error" = error
  return(return.list)
}
