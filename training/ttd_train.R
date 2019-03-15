#Script for evaluating the accuracy of coxph thresholding using RMSE
setwd("C:/Users/spenc/OneDrive/Documents/BME800/OG07_mcrpc_survival_pred")
pcdata <- read.csv('data/prodata.csv')

#Mean imputation for NAs in CA 
pcdata$CA[which(is.na(pcdata$CA))] <- mean(pcdata$CA, na.rm = TRUE)

#Create training and testing data sets
library(caret)
set.seed(1000)
samples <- createDataPartition(pcdata$DEATH, p=0.7, list=FALSE)
train_data <- pcdata[samples,]
test_data <- pcdata[-samples,]

#Fit Cox proportional hazard model to training data
library(survival)
coxfit <- coxph(Surv(time = train_data$LKADT_P, event = train_data$DEATH) ~ 
                  LIVER + ANALGESICS + PLEURA + MI + GONADOTROPIN,
                data = train_data)
# coxfit <- coxph(Surv(time = train_data$LKADT_P, event = train_data$DEATH) ~ CA + LYMPH_NODES + LIVER + PLEURA + ANALGESICS + GONADOTROPIN + TURP + MI + GIBLEED + COPD, data = train_data)

#Make predictions on testing data set
predicted <- rep(NA, nrow(test_data))
for(i in 1:nrow(test_data)){
  sfit <- survfit(coxfit, test_data[i,])
  #Predict time to death as time when survival probability reaches 75%
  predicted[i] <- sfit$time[which.min(abs(sfit$surv - 0.75))]
}

#Evaluate accuracy of model using RMSE
timeRMSE <- RMSE(test_data$LKADT_P, predicted)
timeRMSE

#Save trained model
typeof(train_data)
str(train_data)
saveRDS(coxfit, file = 'models/ttd_model.rds')
saveRDS(train_data, file = 'models/train_data.rds')
# write.csv(train_data, file = "models/train_data.csv")
