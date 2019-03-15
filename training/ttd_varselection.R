#Script for selecting most impactful variables on survival proabability for time to death prediction
setwd("C:/Users/spenc/OneDrive/Documents/BME800/OG07_mcrpc_survival_pred")
pcdata <- read.csv("data/prodata.csv")
#Set NA values to 0
pcdata[is.na(pcdata)] <- 0
#Create survival object
library(survival)
library(survminer)
survival_obj = Surv(time = pcdata$LKADT_P, event = pcdata$DEATH)
#Use Cox proportional hazards to determine hazard ratios for all covariates
fit.coxph <- coxph(survival_obj ~ ALP + AST + CA + HB + LDH + PSA + TBILI + PROSTATE + LYMPH_NODES + LIVER +
                     KIDNEYS + PLEURA + CORTICOSTEROID + ANALGESICS + GLUCOCORTICOID + ANTI_ANDROGENS + GONADOTROPIN +
                     LYMPHADENECTOMY + SPINAL_CORD_SURGERY + HEAD_AND_NECK + TURP + GIBLEED + MI + COPD + DVT + MHSOCIAL +
                     MHSURG + MHNEOPLA + MHMUSCLE + MHNERV + Age1 + Age2 + Age3 + Asian + Black + White + OtherRace,
                   data = pcdata)
ggforest(fit.coxph, data = pcdata)

#Fit survival curves and plot Kaplan-Meier curves and p values for covariates with greatest hazard ratios
#Convert numerical data to 2 level factors
pcdata <- pcdata %>% mutate(CAsplit = ifelse(CA >= 1.5, 'CA_high', 'CA_low'))
fitCA <- survfit(survival_obj ~ CAsplit, data = pcdata)
ggsurvplot(fitCA, data = pcdata, pval=TRUE)

fitLYMPH <- survfit(survival_obj ~ LYMPH_NODES, data = pcdata)
ggsurvplot(fitLYMPH, data = pcdata, pval=TRUE)

fitLIVER <- survfit(survival_obj ~ LIVER, data = pcdata)
ggsurvplot(fitLIVER, data = pcdata, pval=TRUE)

fitPL <- survfit(survival_obj ~ PLEURA, data = pcdata)
ggsurvplot(fitPL, data = pcdata, pval=TRUE)

fitANALGESICS <- survfit(survival_obj ~ ANALGESICS, data = pcdata)
ggsurvplot(fitANALGESICS, data = pcdata, pval=TRUE)

fitGONA <- survfit(survival_obj ~ GONADOTROPIN, data = pcdata)
ggsurvplot(fitGONA, data = pcdata, pval=TRUE)

fitTURP <- survfit(survival_obj ~ TURP, data = pcdata)
ggsurvplot(fitTURP, data = pcdata, pval=TRUE)

fitGIBLEED <- survfit(survival_obj ~ GIBLEED, data = pcdata)
ggsurvplot(fitGIBLEED, data = pcdata, pval=TRUE)

fitMI <- survfit(survival_obj ~ MI, data = pcdata)
ggsurvplot(fitMI, data = pcdata, pval=TRUE)

fitCOPD <- survfit(survival_obj ~ COPD, data = pcdata)
ggsurvplot(fitCOPD, data = pcdata, pval=TRUE)





