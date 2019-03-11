---
---
title: "ITS"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Library packages

Good time series information: https://www.itl.nist.gov/div898/handbook/pmc/section4/pmc4431.htm

More good time series information: https://otexts.org/fpp2/stationarity.html

Good time series: https://datascienceplus.com/time-series-analysis-in-r-part-1-the-time-series-object/

Good information on time series in R: https://datascienceplus.com/time-series-analysis-in-r-part-2-time-series-transformations/
```{r}
library(lavaan)
library(psych)
library(semTools)
library(dplyr)
library(ltm)
library(prettyR)
library(semTools)
library(GPArotation)
library(lavaan)
library(psych)
library(semTools)
library(dplyr)
library(ltm)
library(lordif)
library(Amelia)
library(plyr)
library(paran)
library(caret)
library(ggplot2)
library(pracma)
library(AER)
library(pscl)
library(TSA)
library(TTR)
library(smooth)
library(descr)
library(urca)
```
Load the data
```{r}
head(ITSTest)
```
Cleaning data
```{r}
### use the gsub function to break off -02 part, then get rid of -, then you have the year
ITSTest$MonthNum =  gsub("\\d", "", ITSTest$Month)
### Get rid of -0x part 
ITSTest$MonthNum = substr(ITSTest$MonthNum, start = 1, stop= 3)

ITSTest$Year = gsub("\\D", "", ITSTest$Month)

ITSTest$Year = as.numeric(ITSTest$Year)
ITSTest$Month = NULL
head(ITSTest)

### Add a time variable that is 1:length of data set see Bernal article
ITSTest$Time= 1:dim(ITSTest)[1]
dim(ITSTest)
head(ITSTest)
ITSTest
ITSTest[142:150,]

#Start Jan 2014 intervention starts
# 1 equals Jan 2014 to Jan 2016 and Jan 2016 is when meds only problem discovered
#2 equals when meds only was being dealt with Jan 2016 to May 2016
# 3 is May 2016 to March 2018 (end of data)
Intervention= c(rep(0,143), rep(1,167-143), rep(2,172-167), rep(3,194-172))
length(Intervention)

ITSTest$Intervention = Intervention
head(ITSTest)
ITSTest[143:145,]
ITSTest[144:172,]
ITSTest[144:168,]
dim(ITSTest)


### Changing the month names to numbers so we can plot
ITSTest$MonthNum = ifelse(ITSTest$MonthNum == "Jan", 1, ifelse(ITSTest$MonthNum == "Feb", 2, ifelse(ITSTest$MonthNum == "Mar", 3, ifelse(ITSTest$MonthNum=="Apr", 4, ifelse(ITSTest$MonthNum == "May", 5, ifelse(ITSTest$MonthNum == "Jun", 6, ifelse(ITSTest$MonthNum == "Jul", 7, ifelse(ITSTest$MonthNum == "Aug", 8, ifelse(ITSTest$MonthNum == "Sep", 9, ifelse(ITSTest$MonthNum == "Oct", 10, ifelse(ITSTest$MonthNum == "Nov", 11, ifelse(ITSTest$MonthNum == "Dec", 12, ITSTest$MonthNum))))))))))))

```
Just look at descirptives
```{r}
describe(ITSTest)
```


Get counts by month and year 
We are missing some months?  Why?

Getting the total number of suicide deaths by month, then year
```{r}
sucByYear = aggregate(Suicides ~ Suicides + Year, data = ITSTest, sum)
sucByMonth = aggregate(Suicides ~ Suicides + MonthNum, data = ITSTest, sum)

sucByYear

sucByMonth

plot(sucByYear$Year, sucByYear$Suicides)
plot(sucByMonth$Month, sucByMonth$Suicides)

```
Evaluate the counts if those are different

The tests we want to run are:
Differences between Intervention 1 and Intervention 0 where intervention is when we think things were going well and zero was when we were implementing

Differences bewteen 3 and 0 where three is when we fixed the problem and 3 and 2 (probably not enough data)
```{r}
compmeans(ITSTest$Suicides, ITSTest$Intervention)
```
We want to subset the data so we can get the differences that we want
0 versus 1 and 0 versus 3

Model: Count
```{r}

ITS_0_1 = subset(ITSTest, Intervention == 1 | Intervention == 0)
ITS_0_3 = subset(ITSTest, Intervention == 0 | Intervention == 3)


modelH_0_1= hurdle(Suicides ~ Time*Intervention, dist = "poisson", zero.dist = "binomial", data = ITS_0_1)  
summary(modelH_0_1)
modelH_0_1_neg = hurdle(Suicides ~ Time*Intervention, dist = "negbin", zero.dist = "binomial", data = ITS_0_1)

## Probably just need a poisson model
summary(modelH_0_1_neg)

library(jtools)
interact_plot(modelH_0_1, pred = "Time", modx = "Intervention")

### Now model 0 versus 3
modelH_0_3= hurdle(Suicides ~ Time*Intervention, dist = "poisson", zero.dist = "binomial", data = ITS_0_3)  
summary(modelH_0_3)
modelH_0_3_neg = hurdle(Suicides ~ Time*Intervention, dist = "negbin", zero.dist = "binomial", data = ITS_0_3)

## Probably just need a poisson model
summary(modelH_0_3_neg)

library(jtools)
interact_plot(modelH_0_3, pred = "Time", modx = "Intervention")



## Not trying cubic spline, not sure what is happening
# When I model time as cubed as not raw then ok 
modelHC = hurdle(Suicides ~ poly(Time, 3, raw = FALSE)*Intervention, dist = "poisson", zero.dist = "binomial", data = ITSTest)  
summary(modelHC)

#Checking autocorrelation
residModelH = residuals(modelH_0_1)
plot(ITS_0_1$Time, residModelH)
acf(residModelH)
pacf(residModelH)

### Use other test
mean_station_short =  ur.kpss(ITSTest$Suicides, type="mu", lags="short")
summary(mean_station_short)

trend_station_short =  ur.kpss(ITSTest$Suicides, type="tau", lags="short")
summary(trend_station_short)

mean_station_long =  ur.kpss(ITSTest$Suicides, type="mu", lags="long")
summary(mean_station_long)

trend_station_long =  ur.kpss(ITSTest$Suicides, type="tau", lags="long")
summary(trend_station_long)

```



Model: First difference log with constant
Cleaning data for this analysis
```{r}

# You can exp the log to get the original value, then you can subtract the constant from before
t1 = 10

t1_log = log(t1)

t1_exp = exp(t1_log); t1_exp

t1_exp-5

ITS_diff_log = ITSTest

## Now add some constant to subtract later
Suicides_c = 6+ITS_diff_log$Suicides
length(Suicides_c)
# Now get rid first observation to rbind later
ITS_diff_log = ITS_diff_log[-1,] 
dim(ITS_diff_log)
## Make sure the numbers are the same (need to comment the above to work)
# Use 12, because that is seasonal differencing
# Seasonal differencing makes the numbers not line, so maybe try that later
ITS_diff_log$Suicides_diff_log =  diff(exp(log(Suicides_c))-6)
head(ITS_diff_log)
hist(ITS_diff_log$Suicides_diff_log)
```
Just get mean differences
```{r}
compmeans(ITS_diff_log$Suicides_diff_log, ITS_diff_log$Intervention)

ITS_diff_log_3 = subset(ITS_diff_log, Intervention == 3)
dim(ITS_diff_log_3)
t.test(ITS_diff_log_3$Suicides_diff_log, mu = .2, alternative = "less")
```


Model: First difference log with constant 
Checking assumptions 

ur.kpps test assumes the data are stationary and if the test statistic is larger than those at different levels listed in the output, then you reject the null hypothesis.  If the data are stationary then, we don't have to worry about seasonality or autocorrelation
```{r}

interaction.plot(x.factor = ITS_diff_log$Time, trace.factor = ITS_diff_log$Intervention, response = ITS_diff_log$Suicides_diff_log)

mean_station =  ur.kpss(ITS_diff_log$Suicides_diff_log, type="mu", lags="short")
summary(mean_station)

trend_station =  ur.kpss(ITS_diff_log$Suicides_diff_log, type="tau", lags="short")
summary(trend_station)

season_station = nsdiffs(ITS_diff_log$Suicides_diff_log)

```
Model: First difference log with constant 
Now look at the interaction plot and compare means
```{r}
interaction.plot(x.factor = ITS_diff_log$Time, trace.factor = ITS_diff_log$Intervention, response = ITS_diff_log$Suicides_diff_log)
library(descr)
compmeans(ITS_diff_log$Suicides_diff_log, ITS_diff_log$Intervention)
```
Model: First difference log with constant 
Now try robust linear regression
```{r}
model_diff_log_lm_nointer = lm(Suicides_diff_log ~ Time + Intervention, data = ITS_diff_log)
summary(model_diff_log_lm_nointer)

model_diff_log_lm_inter = lm(Suicides_diff_log ~ Time*Intervention, data = ITS_diff_log)
summary(model_diff_log_lm_inter)

#Checking autocorrelation
residModelH = residuals(model_diff_log_lm_nointer)
plot(ITS_diff_log$Time, residModelH)
acf(residModelH)
pacf(residModelH)

```
Model 2: Moving average model with difference scores
Cleaning data
```{r}
## Moving average starting at December 2004
library(TTR)
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
interaction.plot(x.factor = ITS_ma$Time, trace.factor = ITS_ma$Intervention, response = ITS_ma$Suicides_ma_diff)
mean(Suicides_ma_diff)
sd(Suicides_ma_diff)
compmeans(ITS_ma$Suicides_ma, ITS_ma$Intervention) 

compmeans(ITS_ma$Suicides_ma_diff, ITS_ma$Intervention) 

```
Model 2: Moving average model
Check assumptions
```{r}
mean_station =  ur.kpss(ITS_ma$Suicides_ma_diff, type="mu", lags="short")
summary(mean_station)

trend_station =  ur.kpss(ITS_ma$Suicides_ma, type="tau", lags="short")
summary(trend_station)

```
Model 2: Moving average model

Running the model and checking for autocorrelation

Seems like differing is needed there is autocorrelation
```{r}
model_lm = lm(Suicides_ma ~Time*Intervention, data = ITS_ma)  
summary(model_lm)
library(jtools)


interact_plot(model_lm, pred = "Time", modx = "Intervention", data = ITS_ma)

interaction.plot(x.factor = ITS_diff_log$Time, trace.factor = ITS_diff_log$Intervention, response = ITS_diff_log$Suicides_diff_log)

#Checking autocorrelation
residmodel_lm= residuals(model_lm)
plot(ITS_ma$Time, residmodel_lm)
acf(residmodel_lm)
pacf(residmodel_lm)


### Not significant may be too much going on
#model_poly = lm(Suicides_ma ~ poly(Time, 3, raw = FALSE)*Intervention, data = ITS_ma)  
#summary(model_poly)

```
Model 3: Moving average plus differencing (use data set generated before)
```{r}
describe(ITS_ma)
interaction.plot(x.factor = ITS_ma$Time, trace.factor = ITS_ma$Intervention, response = ITS_ma$Suicides_ma_diff)
hist(Suicides_ma_diff)
qqnorm(Suicides_ma_diff)
compmeans(ITS_ma$Suicides_ma_diff, ITS_ma$Intervention) 

```
Now compare means for the three versus meds only
```{r}
ITS_ma_3 = subset(ITS_ma, Intervention == 3)
dim(ITS_ma_3)

t.test(ITS_ma$Suicides_ma, mu = 1.92, alternative = "less")

acf(ITS_ma_3$Suicides_ma_diff)

```


Model 3: Moving average and differencing
Checking if data is stationary
```{r}
mean_station =  ur.kpss(ITS_ma$Suicides_ma_diff, type="mu", lags="short")
summary(mean_station)

trend_station =  ur.kpss(ITS_ma$Suicides_ma_diff, type="tau", lags="short")
summary(trend_station)


```
Model 3: Moving average and differencing
Running linear and robust models
```{r}
model_lm = lm(ITS_ma$Suicides_ma_diff ~ Time*Intervention, data = ITS_ma)  
summary(model_lm)


#Checking autocorrelation
residmodel_lm= residuals(model_lm)
plot(ITS_ma$Time, residmodel_lm)
acf(residmodel_lm)
pacf(residmodel_lm)
```
Model 4: Poisson and hurdle with counts
Testing assumptions
```{r}
mean_station =  ur.kpss(ITSTest$Suicides, type="mu", lags="short")
summary(mean_station)

trend_station =  ur.kpss(ITSTest$Suicides, type="tau", lags="short")
summary(trend_station)

interaction.plot(x.factor = ITSTest$Time, trace.factor = ITSTest$Intervention, response = ITSTest$Suicides)

compmeans(ITSTest$Suicides, ITSTest$Intervention) 
```
Model 4: Poisson and hurdle with counts
Running models

Testing for seasonality

Testing overdispersion and autocorrelation

Dispersion not significantly different from one by test and visual inspection of value 


Autocorrelation good link: https://www.ibm.com/support/knowledgecenter/en/SS3RA7_15.0.0/com.ibm.spss.modeler.help/timeseries_acf_pacf.htm

Shows the correlation on the y-axis between different time orders.  For example, time one 
```{r}
modelP = glm(Suicides ~ Time+ Intervention, family = "poisson", data = ITSTest)  
modelQP = glm(Suicides ~ Time*Intervention, family = "quasipoisson", data = ITSTest)  
dispersiontest(modelP, alternative = "two.sided")
mean(ITSTest$Suicides)
sd(ITSTest$Suicides)

summary(modelP)

### Testing autocorrelation

residModelP = residuals(modelP)
plot(residModelP, ITSTest$time)
acf(residModelP)
pacf(residModelP)
```

Try the hurdle model and test for auto
```{r}
modelH= hurdle(Suicides ~ Time*Intervention, dist = "poisson", zero.dist = "binomial", data = ITSTest)  
summary(modelH)
# Needs to be a ts function and doesn't work like the example says
modelH = hurdle(Suicides ~ Time*Intervention, dist = "poisson", zero.dist = "binomial", data = ITSTest)  

## Not trying cubic spline, not sure what is happening
# When I model time as cubed as not raw then ok 
modelHC = hurdle(Suicides ~ poly(Time, 3, raw = FALSE)*Intervention, dist = "poisson", zero.dist = "binomial", data = ITSTest)  
summary(modelHC)

#Checking autocorrelation
residModelH = residuals(modelH)
plot(ITSTest$Time, residModelH)
acf(residModelH)
pacf(residModelH)
```
Model 5: CRI's rolling sum
Load the data
```{r}
setwd("C:/Users/Matthew.Hanauer/Desktop")
ITSRolling = read.csv("ZeroSuicideRollingSum.csv", header = TRUE)
head(ITSRolling)
```
Model 5: CRI's rolling sum
Cleaning data
```{r}
### use the gsub function to break off -02 part, then get rid of -, then you have the year
ITSRolling$MonthNum =  gsub("\\d", "", ITSRolling$Date)
### Get rid of -0x part 
ITSRolling$MonthNum = substr(ITSRolling$MonthNum, start = 2, stop= 4)

ITSRolling$Year = gsub("\\D", "", ITSRolling$Date)

ITSRolling$Year = as.numeric(ITSRolling$Year)

ITSRolling$Date = NULL
head(ITSRolling)

### Add a time variable that is 1:length of data set see Bernal article
ITSRolling$Time= 1:dim(ITSRolling)[1]
dim(ITSRolling)
head(ITSRolling)
dim(ITSRolling)
ITSRolling[144:145,]


Intervention = c(rep(0, 144), rep(1, 168-144), rep(2, 172-168), rep(3, 195-172))
length(Intervention)

ITSRolling$Intervention = Intervention
head(ITSRolling)
ITSRolling[144:145,]
ITSRolling[144:169,]
ITSRolling[169:175,]



### Changing the month names to numbers so we can plot
ITSRolling$MonthNum = ifelse(ITSRolling$MonthNum == "Jan", 1, ifelse(ITSRolling$MonthNum == "Feb", 2, ifelse(ITSRolling$MonthNum == "Mar", 3, ifelse(ITSRolling$MonthNum=="Apr", 4, ifelse(ITSRolling$MonthNum == "May", 5, ifelse(ITSRolling$MonthNum == "Jun", 6, ifelse(ITSRolling$MonthNum == "Jul", 7, ifelse(ITSRolling$MonthNum == "Aug", 8, ifelse(ITSRolling$MonthNum == "Sep", 9, ifelse(ITSRolling$MonthNum == "Oct", 10, ifelse(ITSRolling$MonthNum == "Nov", 11, ifelse(ITSRolling$MonthNum == "Dec", 12, ITSRolling$MonthNum))))))))))))

write.csv(ITSRolling, "ITSRolling.csv", row.names = FALSE)

ITSRolling = read.csv("ITSRolling.csv", header = TRUE)
```
Model 5: CRI's rolling sum

Just look at descirptives
```{r}
describe(ITSRolling)
```
Model 5: CRI's rolling sum
If test statistic is below the critical levels, then we reject the null hypothesis of being stationary (this is bad)

```{r}
mean_station =  ur.kpss(ITSRolling$RollingSumTN, type="mu", lags="short")
summary(mean_station)

trend_station =  ur.kpss(ITSRolling$RollingSumTN, type="tau", lags="short")
summary(trend_station)

interaction.plot(x.factor = ITSRolling$Time, trace.factor = ITSRolling$Intervention, response = ITSRolling$RollingSumTN)

compmeans(ITSRolling$RollingSumTN, ITSRolling$Intervention) 

(15-23)/23
(12-23)/23
```
Now try t-test a one-sample t-test bewteen the average of meds only policy 
See if there were differences between earlier pre and intervention 


Testing if post med only is different from meds only
```{r}

ITSRolling_3 = subset(ITSRolling, Intervention == 3)
dim(ITSRolling_3)
t.test(ITSRolling_3$RollingSumTN, mu = 23.25, alternative = "less")

```



Model 5: CRI's rolling sum
```{r}
library(jtools)
model_lm = lm(RollingSumTN ~ Intervention*Time, data = ITSRolling)  
summary(model_lm)

interact_plot(model_lm, pred = "Time", modx = "Intervention")

### Testing autocorrelation
residModelP = residuals(model_lm)
plot(ITSRolling$Time, residModelP)
acf(residModelP)
pacf(residModelP)
hist(predict.lm(model_lm))


```
Can you compare two models with an intervention variable where one group gets the intervention and the other does not and they each have their own outcome variables? Sure do some model comparison?? 
Won't work because the outcome variable is different.

Maybe we can compare parameter estimates.  The values should be the same.  If we have the parameter estimate and the standard error, then we compare them via a t test??? The outcome is different numerically, but the same thing.  Think about if differences in degrees of freedom will make a difference
```{r}
ITSRollingComp = na.omit(ITSRolling)

dim(ITSRollingComp)

model_TN = lm(RollingSumTN ~ Time*Intervention, data = ITSRolling)
summary(model_TN)
model_IN = lm(RollingSumIN ~ Time*Intervention, data = ITSRolling)
summary(model_IN)

```
Do t-tests and demonstrate how to do this

(x1=x2)/sqrt(s1^2/n1 + s2^2/n2)
```{r}

```
Try GLS model
```{r}
head(ITSRolling)
library(nlme)
ITSRollingGLS = data.frame(RollingSumTN = ITSRolling$RollingSumTN, Intervention = ITSRolling$Intervention)
gls_rollingTN = gls(RollingSumTN ~ Intervention, correlation  = corAR1(), data = ITSRolling)
summary(gls_rollingTN)
```

