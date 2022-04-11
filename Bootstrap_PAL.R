# Libraries ---------
library(geepack)
library(multgee)
library(tidyverse)
library(caret)
library(boot)

# Read in Data ---------
airleak_final <-readxl::read_excel("/Users/mayakashani/Dropbox (Partners HealthCare)/Maya_Dessi_Florian_shared/Deidentified_data_2.xlsx")

# Apply Data Dictionary -------
airleak_data_dictionary <- readr::read_csv('/Users/mayakashani/Dropbox (Partners HealthCare)/Maya_Dessi_Florian_shared/airleak_data_dictionary.csv')
airleak_cleaned <- basecamb::apply_data_dictionary(airleak_final, airleak_data_dictionary, print_coerced_NA = F)

#create subset for included sessions
airleak_cleaned_included <-subset(airleak_cleaned, airleak_cleaned$included_sessions==1)

#make sure dataset is sorted by mrn and then date
airleak_cleaned_included$date <- as.Date(airleak_cleaned_included$date, format = "%m/%d/%Y")
airleak_cleaned_included <- airleak_cleaned_included[order(airleak_cleaned_included$mrn,airleak_cleaned_included$date),]

#preprocess variables
# airleak_cleaned_included$composite_outcome is already numeric
airleak_cleaned_included$composite_outcome = as.numeric(as.character(airleak_cleaned_included$composite_outcome))
airleak_cleaned_included$composite_outcome = as.numeric(as.character(airleak_cleaned_included$composite_outcome))
airleak_cleaned_included$mrn <- as.factor(airleak_cleaned_included$mrn)
airleak_cleaned_included$sex <- as.factor(airleak_cleaned_included$sex)
airleak_cleaned_included$ipsilateral_lung_resection <- as.factor(airleak_cleaned_included$ipsilateral_lung_resection)
airleak_cleaned_included$ventilation <- as.factor(airleak_cleaned_included$ventilation)
airleak_cleaned_included$modality <- as.factor(airleak_cleaned_included$modality)
airleak_cleaned_included$chemo <- as.factor(airleak_cleaned_included$chemo)

#BASE CASE GEE MODELS
summary(geeglm(composite_outcome~number_of_lesions_per_session, id=mrn, data=airleak_cleaned_included, family="binomial"))
summary(geeglm(composite_outcome~puncture_site_number, id=mrn, data=airleak_cleaned_included, family="binomial"))
summary(geeglm(composite_outcome~sex, id=mrn, data=airleak_cleaned_included, family="binomial"))
summary(geeglm(composite_outcome~ipsilateral_lung_resection, id=mrn, data=airleak_cleaned_included, family="binomial"))
summary(geeglm(composite_outcome~total_distance_traversed, id=mrn, data=airleak_cleaned_included, family="binomial"))
summary(geeglm(composite_outcome~ventilation, id=mrn, data=airleak_cleaned_included, family="binomial"))
summary(geeglm(composite_outcome~age, id=mrn, data=airleak_cleaned_included, family="binomial"))
summary(geeglm(composite_outcome~pleural_punctures_session, id=mrn, data=airleak_cleaned_included, family="binomial"))
summary(geeglm(composite_outcome~modality, id=mrn, data=airleak_cleaned_included, family="binomial"))
summary(geeglm(composite_outcome~total_pleural_area, id=mrn, data=airleak_cleaned_included, family="binomial"))
summary(geeglm(composite_outcome~chemo, id=mrn, data=airleak_cleaned_included, family="binomial"))


#BOOTSTRAP

#bootstrap for coefficient and p-value of first variable in gee model
fnVar1 <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  d <- d[order(d$mrn, d$date),]
  fit <- geeglm(formula, data=d, id=mrn, family="binomial")
  resultsVec <- c(summary(fit)$coef[2,1],summary(fit)$coef[2,4])
  return(resultsVec) #coefficient and p-value of first variable in gee model
}

fnPower <- function(data,cutoff) {
  #note: data needs to be a vector of values
  d <- rep(0, length(data))
  d[data <= cutoff] <- 1
  power <- mean(d)
  return(power)
}

#fix seed and rng
RNGkind(sample.kind = "Rejection")
set.seed(5)

#MODALITY

#shows bootstrapped confidence intervals for coefficient
boot.ci(boot.out = bootMod, type = c("norm", "basic", "perc"))
fnPower(bootMod$t[,2],0.01) 
fnPower(bootMod$t[,2],0.05) 
fnPower(bootMod$t[,2],0.10) 

#NUMBER OF LESIONS PER SESSION
bootNumLes <- boot(data=airleak_cleaned_included, statistic=fnVar1,
                   R=10000, formula=composite_outcome~number_of_lesions_per_session)
#shows bootstrapped confidence intervals for coefficient
boot.ci(boot.out = bootNumLes, type = c("norm", "basic", "perc"))
fnPower(bootNumLes$t[,2],0.01) 
fnPower(bootNumLes$t[,2],0.05) 
fnPower(bootNumLes$t[,2],0.10) 

#bootstrap for remaining single-variable models
bootPunc <- boot(data=airleak_cleaned_included, statistic=fnVar1,
                 R=10000, formula=composite_outcome~puncture_site_number_total)
bootSex <- boot(data=airleak_cleaned_included, statistic=fnVar1,
                R=10000, formula=composite_outcome~sex)
bootIpsi <- boot(data=airleak_cleaned_included, statistic=fnVar1,
                 R=10000, formula=composite_outcome~ipsilateral_lung_resection)
bootTotDist <- boot(data=airleak_cleaned_included, statistic=fnVar1,
                    R=10000, formula=composite_outcome~total_distance_traversed)
#bootVent <- boot(data=airleak_cleaned_included, statistic=fnVar1,
#                 R=10000, formula=composite_outcome~ventilation)
bootAge <- boot(data=airleak_cleaned_included, statistic=fnVar1,
                R=10000, formula=composite_outcome~age)
bootPleuPunc <- boot(data=airleak_cleaned_included, statistic=fnVar1,
                     R=10000, formula=composite_outcome~pleural_punctures_session)
bootPleuArea <- boot(data=airleak_cleaned_included, statistic=fnVar1,
                     R=10000, formula=composite_outcome~total_pleural_area)
bootChemo <- boot(data=airleak_cleaned_included, statistic=fnVar1,
                  R=10000, formula=composite_outcome~chemo)

#print CIs for coefficients of remaining single-variable models
boot.ci(boot.out = bootPunc, type = c("norm", "basic", "perc"))
boot.ci(boot.out = bootSex, type = c("norm", "basic", "perc"))
boot.ci(boot.out = bootIpsi, type = c("norm", "basic", "perc"))
boot.ci(boot.out = bootTotDist, type = c("norm", "basic", "perc"))
#boot.ci(boot.out = bootVent, type = c("norm", "basic", "perc"))
boot.ci(boot.out = bootAge, type = c("norm", "basic", "perc"))
boot.ci(boot.out = bootPleuPunc, type = c("norm", "basic", "perc"))
boot.ci(boot.out = bootPleuArea, type = c("norm", "basic", "perc"))
boot.ci(boot.out = bootChemo, type = c("norm", "basic", "perc"))

#power analysis (bootstrap results for p-values of remaining single-variable models)
fnPower(bootPunc$t[,2],0.01) 
fnPower(bootPunc$t[,2],0.05) 
fnPower(bootPunc$t[,2],0.10)

fnPower(bootSex$t[,2],0.01) 
fnPower(bootSex$t[,2],0.05) 
fnPower(bootSex$t[,2],0.10)

fnPower(bootIpsi$t[,2],0.01) 
fnPower(bootIpsi$t[,2],0.05) 
fnPower(bootIpsi$t[,2],0.10)

fnPower(bootTotDist$t[,2],0.01) 
fnPower(bootTotDist$t[,2],0.05) 
fnPower(bootTotDist$t[,2],0.10)

#fnPower(bootVent$t[,2],0.01) 
#fnPower(bootVent$t[,2],0.05) 
#fnPower(bootVent$t[,2],0.10)

fnPower(bootAge$t[,2],0.01) 
fnPower(bootAge$t[,2],0.05) 
fnPower(bootAge$t[,2],0.10)

fnPower(bootPleuPunc$t[,2],0.01) 
fnPower(bootPleuPunc$t[,2],0.05) 
fnPower(bootPleuPunc$t[,2],0.10)

fnPower(bootPleuArea$t[,2],0.01) 
fnPower(bootPleuArea$t[,2],0.05) 
fnPower(bootPleuArea$t[,2],0.10)

fnPower(bootChemo$t[,2],0.01) 
fnPower(bootChemo$t[,2],0.05) 
fnPower(bootChemo$t[,2],0.10)

#Two-variable model with Modality and Number of Lesions Per Session

#base case gee model
summary(geeglm(composite_outcome~modality+number_of_lesions_per_session, id=mrn, data=airleak_cleaned_included, family="binomial"))

#bootstrap for coefficients and p-values of first two variables in gee model
#for analysis of final model with two explanatory variables
fnVar12 <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  d <- d[order(d$mrn, d$date),]
  fit <- geeglm(formula, data=d, id=mrn, family="binomial")
  resultsVec <- c(summary(fit)$coef[2,1],summary(fit)$coef[3,1],summary(fit)$coef[2,4],summary(fit)$coef[3,4])
  return(resultsVec) 
}

fnPower2Var <- function(data,cutoff) {
  #note: data needs to be a data frame with first two columns 
  #containing the p-values for the two variables 
  d1 <- rep(0, nrow(data))
  d2 <- rep(0, nrow(data))
  d12 <- rep(0, nrow(data))
  d1[data[,1] <= cutoff] <- 1
  d2[data[,2] <= cutoff] <- 1
  d12[(data[,1] <= cutoff) & (data[,2] <= cutoff)] <- 1
  power1 <- mean(d1)
  power2 <- mean(d2)
  power12 <- mean(d12)
  #output power for each variable and also for the two together
  resultsVec <- c(power1,power2,power12)
  return(resultsVec)
}

bootModNumLes <- boot(data=airleak_cleaned_included, statistic=fnVar12,
                      R=10000, formula=composite_outcome~modality+number_of_lesions_per_session)

#95% CI for coefficient of modality in two-variable model
boot.ci(boot.out = bootModNumLes, index = 1, type = c("norm", "basic", "perc"))
#95% CI for coefficient of number of lesions per session in two-variable model
boot.ci(boot.out = bootModNumLes, index = 2, type = c("norm", "basic", "perc"))

pValueDF <- as.data.frame(bootModNumLes$t[,3:4])

fnPower2Var(pValueDF,0.01) 
fnPower2Var(pValueDF,0.05) 
fnPower2Var(pValueDF,0.10) 

bootVent3 <- boot(data=airleak_cleaned_included, statistic=fnVar12,
                  R=10000, formula=composite_outcome~ventilation)
#95% CI for coefficient of ventilation1
boot.ci(boot.out = bootVent3, index = 1, type = c("norm", "basic", "perc"))
#95% CI for coefficient of number of lesions per session in two-variable model
boot.ci(boot.out = bootVent3, index = 2, type = c("norm", "basic", "perc"))

pValueVentDF <- as.data.frame(bootVent3$t[,3:4])
fnPower2Var(pValueVentDF,0.01) 
fnPower2Var(pValueVentDF,0.05) 
fnPower2Var(pValueVentDF,0.10) 
