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
model_p= glm(Suicides ~ factor(Intervention) + factor(Quarter), family = "poisson", data = ITSTest)
summary(model_p)

model_p_harm= glm(Suicides ~ factor(Intervention) + factor(Quarter) + harmonic(MonthNum, 2, 12), family = "poisson", data = ITSTest) 


AIC(model_p)
BIC(model_p)

AIC(model_p_harm)
BIC(model_p_harm)

lrtest(model_p, model_p_harm)
```
Final Model
```{r}
summary(model_p)
sum_model_p = summary(model_p)
exp(sum_model_p$coefficients[,1])
```


Checking assumptions of the model (kpss tests are the same as previous analysis, because they are based on the raw data) 
```{r}
#Checking autocorrelation
residModelH = residuals(model_p)
plot(ITSTest$Time, residModelH)
acf(residModelH)
pacf(residModelH)
```



