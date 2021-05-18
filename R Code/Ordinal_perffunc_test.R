# Testing performance functions for ordinal model

# Equal
ord_model <- ordfor(depvar='scam', data=na.omit(train_ord[,cols.in.training]), perffunction = "equal")
preds <- predict(ord_model, newdata = test_ord)
preds$ypred <- ordered(preds$ypred, levels=c(0, 1, 2, 3, 4, 5, 6))
test_ord$scam <- ordered(test_ord$scam, levels=c(0, 1, 2, 3, 4, 5, 6))
perff_equal(ytest=test_ord$scam, ytestpred=preds$ypred) 
# 0.2165

seed.range <- as.integer(sample(100:10000, 5))
sum.of.oob <- 0

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

avg.oob <- sum.of.oob/length(seed.range)
avg.oob
# 0.2353745

# Proportional
ord_model <- ordfor(depvar='scam', data=na.omit(train_ord[,cols.in.training]), perffunction = "proportional")
preds <- predict(ord_model, newdata = test_ord)
preds$ypred <- ordered(preds$ypred, levels=c(0, 1, 2, 3, 4, 5, 6))
test_ord$scam <- ordered(test_ord$scam, levels=c(0, 1, 2, 3, 4, 5, 6))
perff_proportional(ytest=test_ord$scam, ytestpred=preds$ypred)

seed.range <- as.integer(sample(100:10000, 5))
sum.of.oob <- 0

for (x in seed.range){
  set.seed(x)
  samp_ord <- sample(nrow(ord_training), train.size * nrow(ord_training))
  train_ord <- droplevels(ord_training[samp_ord,])
  test_ord <- droplevels(ord_training[-samp_ord,])
  
  ord_model <- ordfor(depvar='scam', data=na.omit(train_ord[,cols.in.training]), perffunction = "proportional")
  preds <- predict(ord_model, newdata = test_ord)
  preds$ypred <- ordered(preds$ypred, levels=c(0, 1, 2, 3, 4, 5, 6))
  test_ord$scam <- ordered(test_ord$scam, levels=c(0, 1, 2, 3, 4, 5, 6))
  
  sum.of.oob = sum.of.oob +  perff_proportional(ytest=test_ord$scam, ytestpred=preds$ypred) 
  if (x != seed.range[5]){
    rm(model_species)
  }
}

avg.oob <- sum.of.oob/length(seed.range)
avg.oob
# 0.3159961

# One Class
ord_model <- ordfor(depvar='scam', data=na.omit(train_ord[,cols.in.training]), perffunction = "oneclass", classimp="0")
preds <- predict(ord_model, newdata = test_ord)
preds$ypred <- ordered(preds$ypred, levels=c(0, 1, 2, 3, 4, 5, 6))
test_ord$scam <- ordered(test_ord$scam, levels=c(0, 1, 2, 3, 4, 5, 6))
perff_oneclass(ytest=test_ord$scam, ytestpred=preds$ypred, categ = "0")  
# -0.1184296

seed.range <- as.integer(sample(100:10000, 5))
sum.of.oob <- 0

for (x in seed.range){
  set.seed(x)
  samp_ord <- sample(nrow(ord_training), train.size * nrow(ord_training))
  train_ord <- droplevels(ord_training[samp_ord,])
  test_ord <- droplevels(ord_training[-samp_ord,])
  
  ord_model <- ordfor(depvar='scam', data=na.omit(train_ord[,cols.in.training]), perffunction = "oneclass", classimp="0")
  preds <- predict(ord_model, newdata = test_ord)
  preds$ypred <- ordered(preds$ypred, levels=c(0, 1, 2, 3, 4, 5, 6))
  test_ord$scam <- ordered(test_ord$scam, levels=c(0, 1, 2, 3, 4, 5, 6))
  
  sum.of.oob = sum.of.oob +  perff_oneclass(ytest=test_ord$scam, ytestpred=preds$ypred, categ = "0") 
  if (x != seed.range[5]){
    rm(model_species)
  }
}

avg.oob <- sum.of.oob/length(seed.range)
avg.oob
# -0.151471

# Custom
# Creating Exponential weights for levels of species (0) being the most important
set.seed(seed.num)
weight.list <- rexp(100)
level.number <- nlevels(train_ord$scam)
weights <- sort(sample(weight.list, level.number), decreasing=TRUE)

ord_model <- ordfor(depvar='scam', data=na.omit(train_ord[,cols.in.training]), perffunction = "custom", classweights = weights)
preds <- predict(ord_model, newdata = test_ord)
preds$ypred <- ordered(preds$ypred, levels=c(0, 1, 2, 3, 4, 5, 6))
test_ord$scam <- ordered(test_ord$scam, levels=c(0, 1, 2, 3, 4, 5, 6))
perff_custom(ytest=test_ord$scam, ytestpred=preds$ypred, classweights = weights)
# 0.2225222

seed.range <- as.integer(sample(100:10000, 5))
sum.of.oob <- 0

for (x in seed.range){
  set.seed(x)
  samp_ord <- sample(nrow(ord_training), train.size * nrow(ord_training))
  train_ord <- droplevels(ord_training[samp_ord,])
  test_ord <- droplevels(ord_training[-samp_ord,])
  
  ord_model <- ordfor(depvar='scam', data=na.omit(train_ord[,cols.in.training]), perffunction = "custom", classweights = weights)
  preds <- predict(ord_model, newdata = test_ord)
  preds$ypred <- ordered(preds$ypred, levels=c(0, 1, 2, 3, 4, 5, 6))
  test_ord$scam <- ordered(test_ord$scam, levels=c(0, 1, 2, 3, 4, 5, 6))
  
  sum.of.oob = sum.of.oob +  perff_custom(ytest=test_ord$scam, ytestpred=preds$ypred, classweights = weights)
  if (x != seed.range[5]){
    rm(model_species)
  }
}

avg.oob <- sum.of.oob/length(seed.range)
avg.oob
# 0.2793972