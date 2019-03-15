#Script for estimating overall prediciton of new mCRPC data
#Required values:

setwd("~/BME800/OG07_mcrpc_survival_pred")

new_data <- read.csv('data/new_data_demo_1.csv')

event_fit <- readRDS('models/event_model.rds')
ttd_fit <- readRDS('models/ttd_model.rds')
# train_data <- readRDS('models/train_data.rds')

#predict death event
library(caret)
library(MASS)
death_pred <- predict(event_fit, new_data)

#Predict ttd only if death is predicted
library(survival)
ttd_pred <- NA
if (death_pred$class == 1){
  # ttd_data <- new_data[,c(1,14,16,18,27)]
  sfit <- survfit(ttd_fit, new_data)
  ttd_pred <- sfit$time[which.min(abs(sfit$surv - 0.75))]
}

death_pred$class
ttd_pred
