# Finding the best # of trees for mixed model
mtry <- seq(2,6,1)
ntree <- seq(100,1000,100)
error <- data.frame(matrix(nrow = 10, ncol = 5))
colnames(error) <- mtry
rownames(error) <- ntree

for(i in 1:length(ntree)){
  for(j in 1:length(mtry)){
    model_species <- randomForest(train.sp$species~., ntree = ntree[i], mtry = mtry[j],
                                  data = train.sp[,cols.in.training], keep.forest=TRUE)
    pred.sp <- predict(model_species, newdata = test.sp)
    error[i, j] <- mean(model_species$err.rate[,1])
  }
}

# Column name with minimum value
col <- names(error)[which.min(apply(error,MARGIN=2,min))]
# Row name with minimum value
row <- rownames(error)[which.min(apply(error,MARGIN=1,min))]

col #6
row #100


## Ordinal Forest Ntrees
# Unable to be done due to the amount of trees
library(ordinalForest)
mtry <- seq(2,6,1)
ntree <- seq(100,1000,100)
ord_error <- data.frame(matrix(nrow = 10, ncol = 5))
colnames(ord_error) <- mtry
rownames(ord_error) <- ntree
seed.range <- as.integer(sample(100:10000, 5))
sum.of.oob <- 0

#Running in parallel
for(i in 1:length(ntree)){
  for(j in 1:length(mtry)){
    for (x in seed.range){
      set.seed(x)
      samp_ord <- sample(nrow(ord_training), train.size * nrow(ord_training))
      train_ord <- droplevels(ord_training[samp_ord,])
      test_ord <- droplevels(ord_training[-samp_ord,])
      
      ord_model <- ordfor(depvar='scam', data=na.omit(train_ord[,cols.in.training]), perffunction = "equal")
      preds <- predict(ord_model, newdata = test_ord)
      preds$ypred <- ordered(preds$ypred, levels=c(0, 1, 2, 3, 4, 5, 6))
      test_ord$scam <- ordered(test_ord$scam, levels=c(0, 1, 2, 3, 4, 5, 6))
      
      sum.of.oob = sum.of.oob +  perff_equal(ytest=test_ord$scam, ytestpred=preds$ypred)
      if (x != seed.range[5]){
        rm(model_species)
      }
    }
    ord_error[i, j] <- sum.of.oob/length(seed.range)
  }
}

library(doParallel)
library(foreach)
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

t1 <- proc.time()
rf <- foreach(x=seed.range, .packages=c('ordinalForest')) %dopar%
  {for(i in 1:length(ntree)){
      for(j in 1:length(mtry)){
          set.seed(x)
          samp_ord <- sample(nrow(ord_training), train.size * nrow(ord_training))
          train_ord <- droplevels(ord_training[samp_ord,])
          test_ord <- droplevels(ord_training[-samp_ord,])
          
          ord_model <- ordfor(depvar='scam', data=na.omit(train_ord[,cols.in.training]), perffunction = "equal")
          preds <- predict(ord_model, newdata = test_ord)
          preds$ypred <- ordered(preds$ypred, levels=c(0, 1, 2, 3, 4, 5, 6))
          test_ord$scam <- ordered(test_ord$scam, levels=c(0, 1, 2, 3, 4, 5, 6))
          
          sum.of.oob = sum.of.oob +  perff_equal(ytest=test_ord$scam, ytestpred=preds$ypred)
          if (x != seed.range[5]){
            rm(model_species)
          }
        }
        ord_error[i, j] <- sum.of.oob/length(seed.range)
      }
}
proc.time()-t1
