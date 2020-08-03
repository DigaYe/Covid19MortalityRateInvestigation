knitr::opts_chunk$set(echo = FALSE, tidy = TRUE,tidy.opts=list(width.cutoff= 70), fig.pos = "!h")

# Input Data and library
library(tidyverse)
library("here")
library(formattable)
library(leaps)
library(car)
library(MPV)
library(leaps)
library(MASS)
library(knitr)
library(kableExtra)
alldata <- read.csv(here::here("data","counties.csv"), header=T) 
attach(alldata)
# Rename the variable 
Physician_rate <- Active.Physicians.per.100000.Population.2018..AAMC.
Specialist <- All.Specialties..AAMC.
ElderlyRate <- Pop_Above_65
Poverty <- POVALL_2018
Migration <- INTERNATIONAL_MIG_2018
Nurse <- Total.nurse.practitioners..2019.
Covid19 <- data.frame(Mortality, ElderlyRate, Region, Specialist, population, Physician_rate, Unemployment_rate_2018, Poverty, Migration, Nurse)

# Summary of fitting individual model with its corresponding p-value
d1=data.frame(Variable= c("ElderlyRate", "Region", "Specialist", "population", "Physician_rate", "Unemployment_rate_2018","Poverty", "Migration","Nurse"), p_value = c(7.072*10^(-7), 0.02471,0.02745, 0.01440, 0.04606, 0.04841, 0.02501, 0.00275, 0.01404))
kable(d1,align = "l",booktabs = T, caption = "P-values for Selected Explanatory Variables",linesep = "")%>%
  kable_styling(position = "center", full_width = FALSE,latex_options = "HOLD_position")%>%
  column_spec(1, width = "20em")

# Find the correlation matrix and put it in the table 
x<-data.frame(ElderlyRate, Specialist, Physician_rate, Unemployment_rate_2018, Poverty, Migration, Nurse)
cor_matrix <- cor(x)
output_matrix <- round(cor_matrix,3)
kable(output_matrix,align = "l",booktabs = T, caption = "Correlation Matrix",
      col.names = c("Elderly","Specilist","Physician","Unemployment", "Poverty","Migration", "Nurse"))%>%
  kable_styling(position = "center", full_width = TRUE, latex_options = "HOLD_position",font_size = 10)%>%
  column_spec(1, width = "12em")%>%
  column_spec(5, width = "6em")%>%
  column_spec(7, width = "5em")%>%
  column_spec(4, width = "4em")
# Fit the model with all the variable we found its statistically significant 
fullmodel<-lm(Mortality ~ ElderlyRate + factor(Region) + Specialist + factor(population) + Physician_rate + Unemployment_rate_2018 + Poverty + Migration + Nurse)
# Compute full model's corresponding VIF value and put it in the table
vif_value <- vif(fullmodel)
kable(vif_value,align = "l",booktabs = T, caption = "VIF Values")%>%
  kable_styling(position = "center", full_width = FALSE, latex_options = "HOLD_position")%>%
  column_spec(1, width = "15em")

# Stepwise Automatic Model Selection Method
step.model <- stepAIC(fullmodel, direction = "both",  trace = FALSE)
summary(step.model)
step.model$anova

# Form a Table of Details about Models Used
f <- regsubsets(Mortality~., data=Covid19,nbest=2, nvmax=11)
e <- summary(f)
attach(e)
explain1 <- data.frame(Num = c("0","1","2","3"), Variable = c("Intercept","ElderlyRate", "RegionNorthEast","RegionSouth"),Num = c("4","5", "6","7"), Variable = c("RegionWest","Specialist", "populationSmall","PhysicianRate"), Num = c("8","9","10","11"), Variable = c("Unemployment_Rate_2018","Poverty","Migration","Nurse"))
kable(explain1,align = "l",booktabs = T, caption = "Variable Reference")%>%
  kable_styling(position = "center", full_width = FALSE,latex_options = "HOLD_position")%>%
  column_spec(1, width = "5em")

# All Possible Combinations by Automatic Model Selection Process
combination <- cbind(which,cp,adjr2)
kable(combination,align = "l",booktabs = T, caption = "Model Selection with CP values",
      col.names = c("0", "1","2","3","4","5","6","7","8","9","10","11", "cp","adjr2"))%>%
  kable_styling(position = "center", full_width = FALSE, latex_options = "HOLD_position")%>%
  column_spec(1, width = "5em")

# Suitable Models from Above Model Selection Process
model1 <- lm(Mortality~ElderlyRate + Specialist + Migration + factor(Region))
model2 <- lm(Mortality~ElderlyRate + Nurse + Migration + factor(Region))
model3 <- lm(Mortality~ElderlyRate + Specialist + Migration + Unemployment_rate_2018 + factor(Region))
model4 <- lm(Mortality~ElderlyRate + Nurse + Migration + Unemployment_rate_2018 + factor(Region))
model5 <- lm(Mortality~ElderlyRate + Specialist + Migration + Unemployment_rate_2018 + factor(Region) + factor(population))
model6 <- lm(Mortality~ElderlyRate + Specialist + Migration + Unemployment_rate_2018 + Physician_rate +  factor(Region))
model7 <- lm(Mortality~ElderlyRate + Nurse + Migration + Unemployment_rate_2018 + Physician_rate + factor(Region))
model8 <- lm(Mortality~ElderlyRate + Specialist + Migration + Unemployment_rate_2018 + Physician_rate + factor(Region) + factor(population))
model9 <- lm(Mortality~ElderlyRate + Nurse + Migration + Unemployment_rate_2018 + Physician_rate + factor(Region) + factor(population))
model10 <- lm(Mortality~ElderlyRate + Specialist + Nurse + Migration + Unemployment_rate_2018 + Physician_rate + factor(Region) + factor(population))
model11 <- lm(Mortality~ElderlyRate + Specialist + Migration + Unemployment_rate_2018 + Physician_rate + Poverty + factor(Region) + factor(population))
model12 <- lm(Mortality~ElderlyRate + Specialist + Nurse + Migration + Unemployment_rate_2018 + Physician_rate + Poverty + factor(Region) + factor(population))

# AIC Values for Above Models
A <- AIC(model1,model2,model3,model4,model5,model6,model7,model8,model9,model10,model11,model12, k=2)

# BIC Values for Above Models
B <- BIC(model1,model2,model3,model4,model5,model6,model7,model8,model9,model10,model11,model12)

# Prepare a Table for Available Models
d2=data.frame(Model= c(1, 2, 3, 4, 5, 6,7, 8,9,10,11,12), Regression_Equation = c("Log of Mortality Rate vs. ElderlyRate + Specialist + Migration + Region","Log of Mortality Rate vs. ElderlyRate + Nurse + Migration + Region", "Log of Mortality Rate vs. ElderlyRate + Specialist + Migration + Unemployment_rate_2018 + Region","Log of Mortality Rate vs. ElderlyRate + Nurse + Migration + Unemployment_rate_2018 + Region","Log of Mortality Rate vs. ElderlyRate + Specialist + Migration + Unemployment_rate_2018 + Region + population","Log of Mortality Rate vs. ElderlyRate + Specialist + Migration + Unemployment_rate_2018 + Physician_rate +  Region","Log of Mortality Rate vs. ElderlyRate + Nurse + Migration + Unemployment_rate_2018 + Physician_rate + Region","Log of Mortality Rate vs. ElderlyRate + Specialist + Migration + Unemployment_rate_2018 + Physician_rate + Region + population","Log of Mortality Rate vs. ElderlyRate + Nurse + Migration + Unemployment_rate_2018 + Physician_rate + Region + population","Log of Mortality Rate vs. ElderlyRate + Specialist + Nurse + Migration + Unemployment_rate_2018 + Physician_rate + Region + population","Log of Mortality Rate vs. ElderlyRate + Specialist + Migration + Unemployment_rate_2018 + Physician_rate + Poverty + Region + population","Log of Mortality Rate vs. ElderlyRate + Specialist + Nurse + Migration + Unemployment_rate_2018 + Physician_rate + Poverty + Region + population"))

# Generate Table including Available Models
kable(d2,align = "cl",booktabs = T, caption = "Automatic Selection Model Summary")%>%
  kable_styling(position = "center", full_width = TRUE,latex_options = "HOLD_position")%>%
  column_spec(1, width = "5em")%>%
  row_spec(1:12, hline_after = TRUE)


# Generate Table including AIC & BIC values
output_table <- data.frame(A, B[2]) 
kable(output_table,align = "l",booktabs = T, caption = "AIC and BIC values")%>%
  kable_styling(position = "center", full_width = FALSE,latex_options = "HOLD_position")%>%
  column_spec(1, width = "5em")

# Model 3: Log of Mortality Rate vs. ElderlyRate + Specialist + Migration + Unemployment_rate_2018 + Region
summary(model3)

# Model 1: Log of Mortality Rate vs. ElderlyRate + Specialist + Migration + Region
summary(model1)

# Model 2: Log of Mortality Rate vs. ElderlyRate + Nurse + Migration + Region
summary(model2)

# Model 4: Log of Mortality Rate vs. ElderlyRate + Nurse + Migration + Unemployment_rate_2018 + Region
summary(model4)

# Calculating p-value for Unemployment_rate_2018
(pval4 <- pf(1.66634,1, 1723, lower.tail = FALSE))

# Model with Interaction term
model_int <- lm(Mortality~ElderlyRate + Specialist + Migration + factor(Region) + Specialist * Migration)
summary(model_int)
anova(model_int)

# Plot the 2x2 diagnosis plot for modeling checking
par(mfrow = c(2, 2))
plot(model1,cex.lab=1.1, cex.axis=1.1, cex.main=1, cex.sub=0.8)

# Single Model Fitting
modela <- lm(Mortality~ ElderlyRate)
modelb <- lm(Mortality~ Region)
modelc <- lm(Mortality~ Specialist)
modeld <- lm(Mortality~ population)
modele <- lm(Mortality~ Physician_rate)
modelf <- lm(Mortality~ Unemployment_rate_2018)
modelg <- lm(Mortality~ Poverty)
modelh <- lm(Mortality~ Migration)
modeli <- lm(Mortality~ Nurse)

summary(modela)
summary(modelb)
summary(modelc)
summary(modeld)
summary(modele)
summary(modelf)
summary(modelg)
summary(modelh)
summary(modeli)

## library(knitr)
## purl("~/Desktop/STAT 371/Final Project Report/STAT371/Final Report.Rmd", output = "test2.R", documentation = 3)
## 
## 
