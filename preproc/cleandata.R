library(mlbench)
library(caret)
library(dplyr)

setwd("~/BME800/OG07_mcrpc_survival_pred")
#import all DREAM data
data1 = as.matrix(read.table("data/CoreTable_training.csv",sep=",",header=T,na.strings=c("NA","",".")))
data1[,"REGION_C"] = as.numeric(data1[,"REGION_C"])
data2 = as.matrix(read.table("data/CoreTable_leaderboard.csv",sep=",",header=T,na.strings=c("NA","",".")))
data2[,"REGION_C"] = as.numeric(data2[,"REGION_C"])
data3 = as.matrix(read.table("data/CoreTable_validation.csv",sep=",",header=T,na.strings=c("NA","",".")))
data3[,"REGION_C"] = as.numeric(data3[,"REGION_C"])
data = rbind(data1,data2,data3)


#Combine datasets into a data.frame
numid = c("LKADT_P","WEIGHTBL","ALP","AST","CA","HB","LDH","PSA","TBILI") # ,"CREACL","ALB","TPRO","RBC","BUN","GLU"
multid = c("AGEGRP2","RACE_C")
bnid = c("DEATH","PROSTATE","LYMPH_NODES","LIVER","KIDNEYS","PLEURA","CORTICOSTEROID","ANALGESICS","GLUCOCORTICOID","ANTI_ANDROGENS","GONADOTROPIN","LYMPHADENECTOMY","SPINAL_CORD_SURGERY","HEAD_AND_NECK","TURP","GIBLEED","MI","COPD","DVT","MHSOCIAL","MHSURG","MHNEOPLA","MHMUSCLE","MHNERV")  
n = nrow(data)

A = matrix(data=NA,nrow=n,ncol=length(numid))
colnames(A) = numid
A = apply(data[,numid],2,function(x){as.numeric(x)})
#Replace NAs with column mean in numerical data
for (i in 1:ncol(A)){
  A[is.na(A[,i]), i] <- mean(A[,i], na.rm=TRUE)
}

# pcdata$CA[which(is.na(pcdata$CA))] <- mean(pcdata$CA, na.rm = TRUE)

B = matrix(data=NA,nrow=n,ncol=length(multid))
colnames(B) = multid
B = apply(data[,multid],2,function(x){as.factor(x)})

C = matrix(data=NA,nrow=n,ncol=length(bnid))
colnames(C) = bnid
C =  apply(data[,bnid],2,function(x){as.factor(x)})
#Change 'YES' -> 1 and NA -> 0
C[!is.na(C)]=1
C[is.na(C)]=0
C =  apply(C,2,function(x){as.numeric(x)})

alldata=data.frame(A,B,C)
#alldata$DEATH = as.numeric(alldata$DEATH)-1


#Transform categorical variables for LDA
Age1 =  (alldata[,"AGEGRP2"]=="18-64")*1
Age2 =  (alldata[,"AGEGRP2"]=="65-74")*1
Age3 =  (alldata[,"AGEGRP2"]==">=75")*1
Asian = (alldata[,"RACE_C"]=="Asian")*1
Black = (alldata[,"RACE_C"]=="Black")*1
White = (alldata[,"RACE_C"]=="White")*1
OtherRace = (alldata[,"RACE_C"]=="Other")*1
alldata = cbind(alldata,Age1,Age2,Age3,Asian,Black,White,OtherRace)


#Remove Categorical Variables
alldata$AGEGRP2 <- NULL
alldata$RACE_C <- NULL


write.csv(alldata,file = "data/prodata.csv")