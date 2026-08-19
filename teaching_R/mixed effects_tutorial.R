# Logistic regression - odds of increase or decrease 

rm(list=ls())
library("readxl")
library("dplyr")
library("tidyr")
library("lme4") #this package is used for the mixed effects models. To install lme4, go to Tools-> Install Packages->search lme4

#set working directory - put file path of data set
setwd("")

#load data set 
import<-read_xlsx("")

#select variables and filter for complete cases
data<-import%>%
  dplyr::select(hh,idexrtfediff,cumvol,hpcrt_bin)%>% #input the names of the variables you're interested in
  filter(complete.cases(.))


#This code runs a FIXED EFFECTS LOGISTIC REGRESIION (no control for clustering of households/locations)
glm1 <- glm(dependent_var ~ independent_var1 + independent_var2,
            data=data,
            family=binomial)

# the dependent / outcome variable must be a binary variable for logistic regression
summary(glm1)


#This code runs a MIXED EFFECTS LOGISTIC REGRESIION (control for clustering of samples in households)
glm2<-glmer(dependent_var ~ independent_var1 + independent_var2 + (1 | location),
                   control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)), #leave this, this optimizes the function
                   family=binomial, #leave this
                   data=data) #tell it where the data is

summary(glm2) #summarize the coefficients and p-values for each predictor variable
ci<-confint(glm2) #calculates a 95% confidence interval around each predictor variable coefficient
or<-exp(ci) #calculates the 95% confidence interval around each odds ratio
exp(coef(glm2)) #calculates the odds ratio for each predictor

