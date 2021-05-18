### Ordinal Forest Model
## Casey Samagalsky

#install.packages("ordinalForest")
library(ordinalForest)

# Inputs
ord_training <- training #training or total.training
curr.species = 'scam'
train.size = .8
cols.in.training = c(4, 12:19)
seed.num = 2000

set.seed(seed.num)

# Reassign Codes for Coverage and Order
for (z in 4:11){
  ord_training[which(ord_training[,z] == 5),z] <- 6
  ord_training[which(ord_training[,z] == 4),z] <- 5
  ord_training[which(ord_training[,z] == 3),z] <- 4
  ord_training[which(ord_training[,z] == 2),z] <- 3
  ord_training[which(ord_training[,z] == 1),z] <- 2
  ord_training[which(ord_training[,z] == 0),z] <- 1
  ord_training[which(is.na(ord_training[,z])),z] <- 0
  ord_training[,z] <- as.ordered(ord_training[,z])
}

# Saving ordinal training data to csv
write.csv(ord_training, "C:/Users/cassa/Google Drive/MARSH project/Landsat ARD Data/CSamagalsky/", row.names = TRUE)


# Create Test and Training data
current_ord <- ord_training[,which(names(ord_training)==curr.species)]
samp_ord <- sample(nrow(ord_training), train.size * nrow(ord_training))
train_ord <- droplevels(ord_training[samp_ord,])
test_ord <- droplevels(ord_training[-samp_ord,])

# Run Ordinal Model
str(train_ord)
ord_model <- ordfor(depvar='scam', data=na.omit(train_ord[,cols.in.training]), perffunction = "equal")

# Study variable importance values:
sort(ord_model$varimp, decreasing=TRUE)

# Take a closer look at the top variables:
boxplot(train_ord$band5 ~ train_ord$scam, horizontal=TRUE)

# Predict on Test Data
preds <- predict(ord_model, newdata = test_ord)
table(data.frame(true_values=test_ord$scam, predictions=preds$ypred))

cols = c("NA"="#ceb467", "0" = "#ace5b2", "1" = "#7aef87", "2" = "#57d165", "3" = "#21a31d", "4" = "#197f24", "5" = "#115118")

pred.graph <- ggplot() +
  geom_rect(data=test_ord, aes(xmin=easting, xmax=easting + 30, ymin=northing, ymax=northing + 30, fill = preds$ypred), color="black") + 
  labs(title=paste(paste(curr.species,"Pop. Abundance Prediction on 40% of Plots")), x="Easting", y="Northing", fill="Cover") +
  scale_x_continuous(limits = c(365430, 366280)) +
  scale_y_continuous(limits = c(4303800, 4304470)) +
  scale_fill_manual(values = cols)

pred.graph
rm(ord_model)


## Ordinal Model Accuracy 
preds$ypred <- ordered(preds$ypred, levels=c(0, 1, 2, 3, 4, 5, 6))
test_ord$scam <- ordered(test_ord$scam, levels=c(0, 1, 2, 3, 4, 5, 6))
perff_equal(ytest=test_ord$scam, ytestpred=preds$ypred) 

# Model accuracy over 5 trials
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



