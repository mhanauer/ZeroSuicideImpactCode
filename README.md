---
---
title: "ITS"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Library packages
```{r}
library(psych)
library(dplyr)
library(caret)
library(ggplot2)
library(AER)
library(pscl)
library(TSA)
library(TTR)
library(prettyR)
library(multcomp)
library(descr)
```
Load the data
```{r}
head(ITSTest)
```
Cleaning data
```{r}
### use the gsub function to break off -02 part, then get rid of -, then you have the year
ITSTest$Year = gsub("\\D", "", ITSTest$Month)
ITSTest$Year = as.numeric(ITSTest$Year)

head(ITSTest)
### use the gsub function to break off -02 part, then get rid of -, then you have the year
ITSTest$MonthNum =  gsub("\\d", "", ITSTest$Month)
### Get rid of -0x part 
ITSTest$MonthNum = substr(ITSTest$MonthNum, start = 1, stop= 3)
ITSTest$Month = NULL
### Add a time variable that is 1:length of data set see Bernal article
ITSTest$Time= 1:dim(ITSTest)[1]
dim(ITSTest)
head(ITSTest)
ITSTest[142:150,]

#Start Jan 2014 intervention starts
# 1 equals Jan 2014 to Dec 2015 and Jan 2016 is when meds only problem discovered
#2 equals when meds only was being dealt with Jan 2016 to May 2016
# 3 is June 2016 to March 2018 (end of data)
Intervention= c(rep(0,143), rep(1,167-143), rep(2,172-167), rep(3,194-172))
length(Intervention)

ITSTest$Intervention = Intervention
head(ITSTest)
ITSTest[143:145,]
ITSTest[144:168,]
ITSTest[168:175,]
dim(ITSTest)


### Changing the month names to numbers so we can plot
ITSTest$MonthNum = ifelse(ITSTest$MonthNum == "Jan", 1, ifelse(ITSTest$MonthNum == "Feb", 2, ifelse(ITSTest$MonthNum == "Mar", 3, ifelse(ITSTest$MonthNum=="Apr", 4, ifelse(ITSTest$MonthNum == "May", 5, ifelse(ITSTest$MonthNum == "Jun", 6, ifelse(ITSTest$MonthNum == "Jul", 7, ifelse(ITSTest$MonthNum == "Aug", 8, ifelse(ITSTest$MonthNum == "Sep", 9, ifelse(ITSTest$MonthNum == "Oct", 10, ifelse(ITSTest$MonthNum == "Nov", 11, ifelse(ITSTest$MonthNum == "Dec", 12, ITSTest$MonthNum))))))))))))

ITSTest$Quarter = ifelse(ITSTest$MonthNum <= 3, 1, ifelse(ITSTest$MonthNum >= 4 & ITSTest$MonthNum <= 6, 2, ifelse(ITSTest$MonthNum >= 7 & ITSTest$MonthNum <= 9, 3, ifelse(ITSTest$MonthNum >= 10, 4, ITSTest$MonthNum))))

```
Just look at descirptives
```{r}
describe(ITSTest)
ITSTest$MonthNum
compmeans(ITSTest$Suicides, ITSTest$Intervention)
```
The tests we want to run are:
Differences between Intervention 1 and Intervention 0 where intervention 1 is when we think things were going well and zero was when we were implementing.

Differences between 3 and 0 where 3 is when we fixed the problem and 3 and 2 (probably not enough data), but is this when we have a problem (2) and 3 when we fixed it.


Previous modeling with the data showed the hurdle with poisson with count and quarter and linear regression with moving average only intervention are the best fits for the data.

Model: Count
```{r}
modelH_q= hurdle(Suicides ~ factor(Intervention) + factor(Quarter), dist = "poisson", zero.dist = "binomial", data = ITSTest) 
summary(modelH_q)

modelH_harm= hurdle(Suicides ~ factor(Intervention) + factor(Quarter) + harmonic(MonthNum, 2, 12), dist = "poisson", zero.dist = "binomial", data = ITSTest) 
summary(modelH_harm)
AIC(modelH_q)
AIC(modelH_harm)

modelH_q_sum = summary(modelH_q)
round(exp(modelH_q_sum$coefficients$count[,1]),3)
round(exp(modelH_q_sum$coefficients$count[,2]),3)

#### Set up contrasts
K = matrix(c(0,0,-1,1,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,-1,1,0,0), ncol = 12, nrow = 2, byrow = TRUE)
t = glht(modelH_q, linfct = K)
t_sum = summary(t)
t_sum
```
Checking assumptions of the model (kpss tests are the same as previous analysis, because they are based on the raw data) 
```{r}
#Checking autocorrelation
residModelH = residuals(modelH_q)
plot(ITSTest$Time, residModelH)
acf(residModelH)
pacf(residModelH)
```
Model 2: Moving average model with difference scores
Cleaning data
```{r}
## Moving average starting at December 2004
Suicides_ma = SMA(ITSTest$Suicides, n = 12)
ITSTest$Suicides_ma = Suicides_ma
ITS_ma = na.omit(ITSTest)
## Now difference the data, high autocorrelation
Suicides_ma_diff = diff(ITS_ma$Suicides_ma)
## Now drop first row so we can rbind 
dim(ITS_ma)
ITS_ma = ITS_ma[-1,] 
dim(ITS_ma)
## Now put back together the data sets
ITS_ma$Suicides_ma_diff = Suicides_ma_diff
dim(ITS_ma)
head(ITS_ma)
```
Model 2: Moving average model
Get plots and descriptives with new moving average
```{r}
describe(ITS_ma)
compmeans(ITS_ma$Suicides_ma_diff, ITS_ma$Intervention) 
```
Model 2: Moving average with differencing
```{r}
model_lm = lm(Suicides_ma_diff ~ factor(Intervention), data = ITS_ma) 
summary(model_lm)

model_lm_q = lm(Suicides_ma_diff ~ factor(Intervention) + factor(Quarter), data = ITS_ma)  

model_lm_harm = lm(Suicides_ma_diff ~ factor(Intervention) + factor(Quarter) + harmonic(MonthNum, 2, 12), data = ITS_ma)  

anova(model_lm, model_lm_q, model_lm_harm)

## Contrasts
K = matrix(c(0,0,-1,1), ncol = 4, nrow = 1, byrow = TRUE)
t = glht(model_lm, linfct = K)
t_sum = summary(t)
t_sum
```
Checking assumptions not great for autocorrelation.  
```{r}
#Checking autocorrelation
residmodel_lm= residuals(model_lm)
plot(ITS_ma$Time, residmodel_lm)
acf(residmodel_lm)
pacf(residmodel_lm)

### compare hurdle to linear
AIC(modelH_q)
AIC(model_lm)
```



