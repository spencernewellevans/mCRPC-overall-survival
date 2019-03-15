#Script for testing accuracy of LDA model for death event prediciton
setwd("~/BME800/OG07_mcrpc_survival_pred")
library(MASS)
library(caret)
pcdata <- read.csv("data/prodata.csv")
pcdata2 <- read.csv("data/prodata.csv")
set.seed(289)

#0 imputation for NA
pcdata[is.na(pcdata)] <- 0

#Select variables to use for prediction
# pcdata <- pcdata[,3:ncol(pcdata)]
pcdata <- pcdata[,c(
    11,14,16,18,27,21,3,35,36,37,38,39,40,41
    ,4,5
    ,6,10
    ,15,17
    ,22
    ,26
    ,30,31,32,34
)]
  
#   3,4,5,6,7,8,9,10,
#   11,12,13,14,15,16,17,18,19,20,
#   21,22,23,24,25,26,27,28,29,30,
#   31,32,33,34,35,36,37,38,39,40,41
# )]

#Perform 20 iterations of bootstrap validation to estimate out of sample accuracy
BSmeans <- rep(NA, 20)
for (i in 1:20){
  #Create training and testing data sets
  samples <- createDataPartition(pcdata$DEATH, p=0.9, list=FALSE)
  train_data <- pcdata[samples,]
  test_data <- pcdata[-samples,]
  # Train LDA model
  fit <- lda(formula = DEATH ~.,
               data = train_data)
  #Make predictions on testing data and calculate accuracy
  predictions <- predict(fit, test_data)
  BSmeans[i] <- mean(predictions$class == test_data$DEATH)
}

#10-fold cross-validation using caret package
pcdata$DEATH <- as.factor(pcdata$DEATH)
train_control = trainControl(method = 'cv', number = 10)
fitC <- train(DEATH~., data = pcdata, trControl = train_control, method = 'lda')

#Final bootstrap accuracy
mean(BSmeans)

#Final CV accuracy
# mean(fitC$resample$Accuracy)
saveRDS(fit, file = 'models/event_model.rds')