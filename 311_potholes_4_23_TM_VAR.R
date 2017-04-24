# 311 requests

### other data to consider

# possibly use rainfall data
# https://data.boston.gov/dataset/rainfall-data
# pothol FAQ http://www.apwa.net/library/About/PotHoleFactSheet.pdf
# pothole economics http://www.pothole.info/the-facts/
# pothole factors https://www.earthmagazine.org/article/geologic-column-impact-factor-potholes
# how pothole forms http://www.summitengineer.net/resources/learning/52-potholes
# public works active works zone: metal patches
# building and property violations

### questions
# how much will it cost to fill potholes?
# what are the factors that affect a pothole getting filled ontime?
# what can we do about it?
# how long does it takes to fill a pothole, by source?
# what goes into an issue of creating a pothole? # build regression

### goal: predict pothole locations across the city of boston (by city, neighborhood, ward, etc)

#### Load packages ####

library(readr) # read data into R
library(ggplot2) # data viz
library(tseries)
library(forecast)
library(dplyr) # manipulate data
library(ggmap) # to get this, you need to use install.packages("ggmap", type = "source")

library(fGarch) # don't load until needed
library(FinTS)

library(zoo) # might need this for time series manipulation
library(lubridate) # might need this to play with date/time

#### Read data into R ####
# read data in R change the directory to point to where your data is
t11 <- read_csv("/Users/tylermiguel/Desktop/311 Service Requests/mayors24hourhotline.csv")

t11 %>%
  dplyr::filter(CASE_TITLE == 'Request for Pothole Repair') %>%
  {.} -> public_works_ph

#### Data cleaning ####
str(public_works_ph) # 39987 observations, 33 variables
paste(min(public_works_ph$open_dt),"to",max(public_works_ph$open_dt)) # date from 2011/7 to 2017/2

## Remove duplicate cases
public_works_ph %>%
  dplyr::filter(grepl('dup|Dup', CLOSURE_REASON)) %>%
  summarise(count = n()) # there are 1255 records that are duplicates

# search each row and return rows where the CLOSURE_REASON does not contain "dup" or "Dup" (indicating Duplicate, therefore it was removed)
public_works_ph %>%
  dplyr::filter(!grepl('dup|Dup', CLOSURE_REASON)) %>%
  {.} -> pwph_clean

## Remove noted cases
pwph_clean %>%
  dplyr::filter(grepl('Noted', CLOSURE_REASON)) %>%
  summarise(count = n())  # there are 1748 cases "Noted" after removing duplicates
# case noted usually is a case that is not completed (see data dictionary)

pwph_clean %>%
  dplyr::filter(!grepl('Noted', CLOSURE_REASON)) %>% # remove close reason = noted
  {.} -> pwph_clean

# only use data with an open and closed date
pwph_clean$closed_dt %>%
  is.na() %>%
  sum() # there are 1896 cases where there is not a closed date

# filter data frame and return only records where the "closed_dt" is not NA
pwph_clean %>%
  dplyr::filter(!is.na(closed_dt)) %>%
  {.} -> pwph_clean

# create new variables (days open)
pwph_clean %>%
  mutate(days_open = closed_dt - open_dt) %>%
  {.} -> pwph_clean

pwph_clean %>%
  mutate(days_open_numeric_hours = as.numeric(days_open/60/60)) %>%
  {.} -> pwph_clean

pwph_clean %>%
  mutate(days_open_numeric_days = as.numeric(days_open/60/60/24)) %>%
  {.} -> pwph_clean

str(pwph_clean)

# this will write a csv of the clean data to your current working directory. 
# write_csv(pwph_clean, "potholes_clean.csv")

#### Data exploration (choosen data) ####
# data from 2011 to 2017 March 
# we should only explore our training data which is 2011 to september 2016
# here we can go into general descriptive statitistics for the presentation, we should also combine this with the data cleaning stage to talk about how we explored our data and then had to clean it.
dim(pwph_clean) #35088 records

# what percent of data is NA
pwph_clean %>%
  is.na() %>%
  sum() %>%
  {.}/(nrow(pwph_clean)*ncol(pwph_clean)) #about 6%

# requests by pwd_district
pwph_clean %>%
  group_by(pwd_district) %>%
  summarise(n_requests = n())

# 3 maps of potholes in boston colored by pwd_district
qmap('Boston', zoom = 12) +
  geom_point(aes(x=Longitude, 
                 y=Latitude, 
                 color = pwd_district), 
             data=pwph_clean,
             alpha=0.03)

qmap('Boston', zoom = 12, maptype = "hybrid") +
  geom_point(aes(x=Longitude, 
                 y=Latitude, 
                 color = pwd_district), 
             data=pwph_clean,
             alpha=0.03)

qmap('Boston', zoom = 12, maptype = "toner-lite") +
  geom_point(aes(x=Longitude, 
                 y=Latitude, 
                 color = pwd_district), 
             data=pwph_clean,
             alpha=0.03)

# requests by zipcode
pwph_clean %>%
  group_by(LOCATION_ZIPCODE) %>%
  summarise(n_requests = n())

# requests by source of request
pwph_clean %>%
  group_by(Source) %>%
  summarise(n_requests = n())

# average number of days to fill a pothole
pwph_clean %>%
  select(Source, days_open_numeric_days) %>%
  group_by(Source) %>%
  summarise(mean_days_to_fil = mean(days_open_numeric_days, na.rm=T),
            median_days_to_fil = median(days_open_numeric_days, na.rm=T),
            count_of_potholes = n())

# we can see the numbers look much better when you look only at the city worker app

# general time series of potholes #
# convert time to YYYY-MM
g_pw_ph <- pwph_clean[,2] # extract time (when request is made)
g_pw_ph <- format(g_pw_ph$open_dt, "%Y-%m") # turns POSIXct YYYY-MM-DD HH:MM:SS to character vector YYYY-MM format
g_pw_ph <- data.frame(date = g_pw_ph) # create a dataframe

g_pw_ph %>%
  group_by(date) %>%
  summarise(count = n()) %>%
  {.} -> g_pw_ph # just counting how pot hole requests we have per month


#### Create a time series ####
# build the series
# check stationarity
# settle on data model to move forward with
monthly_ph_ts <- ts(g_pw_ph$count, start = c(2011, 7), frequency = 12)

monthly_ts_train <- window(monthly_ph_ts, start = c(2011, 7), end = c(2016,9) , frequency = 12) # take a window on time from July 2011 to December 2016.

# we do a similar step below
monthly_ts_test <- window(monthly_ph_ts, start = c(2016, 10), end = c(2017,2) , frequency = 12)

ndiffs(monthly_ts_train)

plot(monthly_ts_train) # don't look stationary given mean is not constant and variance is not constant
plot(diff(monthly_ts_train)) # mean is constant but variance is not constant
plot(diff(monthly_ts_train+monthly_ts_train^2)) # 2015 could be a problem. need to figure out how to get this down
acf(monthly_ts_train) # we can see seasonality here 
pacf(monthly_ts_train)

# testing
plot(diff(log(monthly_ts_train))) # tried to normalize variance first, then remove trend
plot(diff(log(monthly_ts_train), lag=12)) # remove 12 month seasonal effects

arima = auto.arima(monthly_ts_train, trace = T, stationary = FALSE, ic="aic") # Best model: ARIMA(0,0,1)(1,0,0)[12] with non-zero mean #AIC=892.99

plot(arima$residuals) #missing differencing, need to force it 
tsdiag(arima) 

Box.test(arima$residuals^2, type = "Ljung")
ArchTest(arima$residuals^2)

############### FORCING SARIMA Models ################
#Need to figure if d, manually
arima2 = arima(monthly_ts_train, order=c(0,1,0),seasonal=list(order=c(0,1,0),period=12))
arima2 #aic = 751.85

arima3 = arima(monthly_ts_train, order=c(2,1,2),seasonal=list(order=c(1,1,1),period=12))
arima3 #aic = 720.71

arima4 = arima(monthly_ts_train, order=c(1,1,0),seasonal=list(order=c(1,1,0),period=12))
arima4 #aic = 732.79

arima5 = arima(monthly_ts_train, order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12))
arima5 #aic = 724.68

arima6 = arima(monthly_ts_train, order=c(1,1,0),seasonal=list(order=c(0,1,1),period=12))
arima6 #aic = 726.87

arima7 = arima(monthly_ts_train, order=c(1,1,1),seasonal=list(order=c(1,1,1),period=12))
arima7 #aic = 719.08 <--- Lowest

arima8 = arima(monthly_ts_train, order=c(0,0,1),seasonal=list(order=c(0,1,1),period=12))
arima8 #aic = 723.87

arima9 = arima(monthly_ts_train, order=c(1,1,0),seasonal=list(order=c(0,1,0),period=12))
arima9 #aic = 752.35

# can't run this one
arima10 = arima(monthly_ts_train, order=c(1,2,1),seasonal=list(order=c(1,2,1),period=12))
arima10

# ar1 model
ar1 <- arima(monthly_ts_train, order=c(1,0,0))

predict(ar1, 5)
monthly_ts_test

#arima7 is best : (1,1,1)(1,1,1)[12] from lowest aic
#pulled auto arima diagnostics down for comparison
arima = auto.arima(monthly_ts_train, trace = T, stationary = FALSE, ic="aic") # Best model: ARIMA(0,0,1)(0,0,1)[12] with non-zero mean #AIC=892.99
plot(arima$residuals) #missing differencing, need to force it 
tsdiag(arima) 

Box.test(arima$residuals^2, type = "Ljung") #p-value 0.4462
ArchTest(arima$residuals^2) #L-jung p-values decreasing overtime

plot(arima7$residuals) #flattens out 2012, some difference from auto.arima
tsdiag(arima7) #smaller lags than arima, but not by much, L-jung p-values increasing 

Box.test(arima7$residuals^2, type = "Ljung") #higher p-value 0.8364
ArchTest(arima7$residuals^2)  # p-val 1

#comparing auto.arima with arima7 (forced seasonality), arima7 has smaller AIC
#but l-jung is smaller for autoarima v arima7;
## SAR(1) coef. is not significant in the arima7 model.
#see tsdiag to compare 
#which is better to use?

# one more model (TM)
arima11 = auto.arima(monthly_ts_train, trace = T, ic="aic", stepwise = F, approximation = F) # even without approximation and stepwise we got an (0,0,1)(1,0,0)[12]

############### Linear Models ##############

pottime=time(monthly_ts_train)
potseason=cycle(monthly_ts_train)
potdata=coredata(monthly_ts_train)

#first order model
potreg=lm(log(potdata) ~ 0 + pottime + factor(potseason)) 
summary(potreg) #all variables are significant at 10%
RMSE1=(sqrt(mean((monthly_ts_train-exp(potreg$fitted.values))^2)))
plot(potreg) #variance looks constant, normality looks violated, no influencial obs.
library(car)
durbinWatsonTest(potreg)  #there is significant autocorrelation
res <- ts(resid(potreg))
Acf(res, lag.max = 35) #several bands are large and there is an obvious pattern
pacf(res, lag.max = 35)
# the ACF/PACF seems to only have 1 spike, might be helpful to model an arima(1,0,0) to this model

#second order model
potreg2=lm(log(potdata) ~ 0+pottime+I(pottime^2)+factor(potseason))
summary(potreg2) #all variables are significant at .1
plot(potreg2) #looks similar to first order model
durbinWatsonTest(potreg2) #there is significant autocorrelation
RMSE2=sqrt(mean((monthly_ts_train-exp(potreg2$fitted.values) )^2))
res2 <- ts(resid(potreg2))
Acf(res2, lag.max = 35) #even worse than that of first order model
pacf(res2, lag.max = 35) #even worse than that of first order model
# the ACF/PACF seems to only have 2-4 spikes 

#third order model
potreg3 = lm(log(potdata) ~ 0+pottime+I(pottime^2)+I(pottime^3) + factor(potseason))
summary(potreg3) #many variables insignificant, and also signficant at 5% or lower
plot(potreg3) #looks similar to first and second order models
durbinWatsonTest(potreg3) #there is significant autocorrelation
RMSE3=sqrt(mean((monthly_ts_train-exp(potreg3$fitted.values) )^2))
res3 <- ts(resid(potreg3))
Acf(res3, lag.max = 35) #looks like that of second order model
pacf(res3, lag.max = 35) #looks like that of second order model

#compare RMSEs
RMSE1 #237.5238
RMSE2 # 215.1226
RMSE3 #215.1122

#third order has smallest RMSE, but insignificant variables, so we won't consider it.
#second order has next smallest RMSE, but the Acf of the residuals shows worse
#autocorrelation problem than the frist order model, so this may explain underestimated
#RMSE. First order model is simplest and has comparable RMSE. I will test both first and 
#second order models.

#Test first order model
predtime=seq((2016 + (10/12)), (2017 + (2/12)),by=(1/12))
predtime

predseas=c(10, 11, 12, 1, 2)
predseas

newdata=data.frame(pottime=predtime, potseason=predseas)
newdata

# preddata and predictions are the same. reason for this? (TM)
preddata=predict.lm(potreg,newdata,se.fit=TRUE)
preddata 
predictions = predict(potreg, newdata,interval="predict")
predictions #predictions with confidence intervals before undoing log tranformation
predvalues=preddata$fit
predvalues #predictions without confidence intervals before undoing log tranformation
transform = (exp(1)^predvalues) # you can also just do exp(predvalues) because it assumes a base=1
transform #predictions without confidence intervals after undoing log transformation
monthly_ts_test #compare actual observations
MSE = mean((transform - monthly_ts_test)^2) # this was sum() and I changed it to mean() (TM)
MSE #calculate MSE
sqrt(MSE) # 521

# test second order model
preddata2=predict.lm(potreg2,newdata,se.fit=TRUE)
preddata2
predictions = predict(potreg2, newdata,interval="predict")
predictions #predictions with confidence intervals before undoing log tranformation
predvalues2=preddata2$fit
predvalues2 #predictions without confidence intervals before undoing log tranformation
transform2 = (exp(1)^predvalues2)
transform2 #predictions without confidence intervals after undoing log transformation
monthly_ts_test #compare actual observations
MSE2 = mean((transform2 - monthly_ts_test)^2)
MSE2#calculate MSE
sqrt(MSE2) #675

#first order test has lower MSE. however, it is still very high. obviously linear regression
#model sucks

##########################GARCH MODELS###########################

potgarch = garch(arima$residuals, order= c(0 , 1), trace=FALSE)
summary(potgarch)
potgarch.res = potgarch$resid[-1]
Acf(potgarch.res)
Acf(potgarch.res^2)
AIC1 = 2 - 2*logLik(potgarch) # this is also equivelant to AIC(potgarch) (TM)

potgarch2 = garch(arima$residuals, order= c(1 , 1), trace=FALSE)
summary(potgarch2)
potgarch2.res = resid(potgarch2)[-1]
Acf(potgarch2.res)
Acf(potgarch2.res^2)
AIC2 = 4 - 2*logLik(potgarch2)

potgarch3 = garch(arima$residuals, order= c(1 , 0), trace=FALSE)
summary(potgarch3)
potgarch3.res = resid(potgarch3)[-1]
AIC3 = 2 - 2*logLik(potgarch3)

potgarch4 = garch(arima$residuals, order= c(0 , 2), trace=FALSE)
potgarch4.res = resid(potgarch4)[-1]
Acf(potgarch4.res, na.action = na.omit)
Acf(potgarch4.res^2, na.action = na.omit)
AIC4 = 4 - 2*logLik(potgarch4)

AIC1
AIC2
AIC3
AIC4

##looking at ACFs, garch(0,2) looks best. It also has the lowest AIC.

###### data with only constituents #####

pwph_clean %>%
  dplyr::filter(Source == "Citizens Connect App" |
           Source == "Constituent Call" |
           Source == "Twitter") %>%
           {.} -> constituents_only
  
# general time series of potholes #
# convert time to YYYY-MM
co_pw_ph <- constituents_only[,2] # extract time (when request is made)
co_pw_ph <- format(co_pw_ph$open_dt, "%Y-%m") # turns POSIXct YYYY-MM-DD HH:MM:SS to character vector YYYY-MM format
co_pw_ph <- data.frame(date = co_pw_ph) # create a dataframe

co_pw_ph %>%
  group_by(date) %>%
  summarise(count = n()) %>%
  {.} -> co_pw_ph # just counting how pot hole requests we have per month

# build the series
# check stationarity
# settle on data model to move forward with
co_monthly_ph_ts <- ts(co_pw_ph$count, start = c(2011, 7), frequency = 12)

co_monthly_ts_train <- window(co_monthly_ph_ts, start = c(2011, 7), end = c(2016,9) , frequency = 12) # take a window on time from July 2011 to December 2016.

# we do a similar step below
co_monthly_ts_test <- window(co_monthly_ph_ts, start = c(2016, 10), end = c(2017,2), frequency = 12)


##### need to vet through this process #####
plot(co_monthly_ts_train)
acf(co_monthly_ts_train)
pacf(co_monthly_ts_train)


co_arima <- auto.arima(co_monthly_ts_train, stepwise = F, approximation = F, ic = "aic", trace = T)
co_arima1 <- auto.arima(co_monthly_ts_train, ic = "aic", trace = T)
acf(co_arima$residuals)
pacf(co_arima$residuals)
acf(co_arima$residuals^2)
ArchTest(co_arima$residuals^2) # no ARCH effect
Box.test(co_arima$residuals, type = "Ljung")

pred_co_arima <- predict(co_arima, 5)
sqrt(mean((co_monthly_ts_test - pred_co_arima$pred)^2))

pred_co_arima_lm <- lm(log(co_monthly_ts_train) ~ 0+co_trend+factor(co_time))


# AR(2)
co_ar2 <- arima(co_monthly_ts_train, order = c(2,0,0))
acf(co_ar2$residuals)
pacf(co_ar2$residuals)

pred_ar2 <- predict(co_ar2, 5)
sqrt(mean((co_monthly_ts_test - pred_ar2$pred)^2))


#Test first order model
predtime=seq((2016 + (10/12)), (2017 + (2/12)),by=(1/12))
predtime

predseas=c(10, 11, 12, 1, 2)
predseas

newdata=data.frame(pottime=predtime, potseason=predseas)
newdata

# preddata and predictions are the same. reason for this? (TM)
preddata=predict.lm(potreg,newdata,se.fit=TRUE)
preddata 
predictions = predict(potreg, newdata,interval="predict")
predictions #predictions with confidence intervals before undoing log tranformation
predvalues=preddata$fit
predvalues #predictions without confidence intervals before undoing log tranformation
transform = (exp(1)^predvalues) # you can also just do exp(predvalues) because it assumes a base=1
transform #predictions without confidence intervals after undoing log transformation
monthly_ts_test #compare actual observations
MSE = mean((transform - monthly_ts_test)^2) # this was sum() and I changed it to mean() (TM)
MSE #calculate MSE
sqrt(MSE) # 521

##### build regression model connect to weather API #####
# connect to API #
devtools::install_github("ALShum/rwunderground") # download the package
library(rwunderground)
api_key <- set_api_key("bd6ee504abb96cbc")# set API key
location <-  set_location(territory = "Massachusetts", city = "Boston")
history(location, date = 20170228) # will pull one day
history_daily(location, date = 20170228) # will pull one day

library(stringr)

# not unit tested
range_dates <- function(from_date, to_date) {
  seq_day <- seq.Date(from = as.Date(from_date), 
                      to = as.Date(to_date), "day")
  ch_seq_day <- as.character(seq_day)
  numeric_day <- str_replace_all(ch_seq_day, "-", "")
  return(numeric_day)
}

# not unit tested
history_daily_range <- function(location, from_date, to_date) {
  numeric_range <- range_dates(from_date, to_date)
  
  df <- NULL
  for (date in numeric_range) {
    # for each day in date_range, run the history_daily
    new_day <- history_daily(location, date = date)
    df <- bind_rows(df, new_day)
    print(Sys.time())
    Sys.sleep(7)
  }
  return(df)
}


test_range_dates <- history_daily_range(location, 
                                         from_date = "2015-09-16",
                                         to_date = "2017-01-27")

as.Date("2017-01-28") - 500

weather <- bind_rows(test_range_dates, weather)

write_csv(weather, "weather.csv")


##### Create mutli weather model #####

# transform weather data structure
new_weather <- cbind(weather) # makes a copy (different pointers)
new_weather$date <- format(new_weather$date, "%Y-%m")
weather_prepared <- aggregate(new_weather, list(new_weather$date), mean)
weather_prep_final <- weather_prepared[c("Group.1",
                                         "fog",
                                         "rain",
                                         "snow",
                                         "thunder",
                                         "mean_temp",
                                         "mean_dewp",
                                         "mean_pressure",
                                         "mean_wind_spd",
                                         "mean_visib",
                                         "max_temp",
                                         "min_temp")]

reg_data <- merge(weather_prep_final, g_pw_ph[51:68,], 
                  by.x = "Group.1", by.y = "date")

# split data
weather_prep_train <- reg_data[1:13,]
weather_prep_test <- reg_data[14:18,-13]
weather_prep_test_lab <- reg_data[14:18,13]

#build stepwise model
ols_model_weather <- lm(count ~ . - Group.1,  weather_prep_train)
summary(ols_model_weather)
library(MASS)
stepwise <- stepAIC(ols_model_weather, direction = "both")
summary(stepwise)
str(weather_prep_train)

# make predictions
predictions_reg_weather <-predict(stepwise,weather_prep_test )
sqrt(mean((predictions_reg_weather - weather_prep_test_lab)^2)) # 682, it sucked

##### try the VAR model #####

# make min tempature a ts
new_weather <- cbind(weather) # makes a copy (different pointers)
new_weather$date <- format(new_weather$date, "%Y-%m")
weather_prepared <- aggregate(new_weather, list(new_weather$date), mean)
min_temp <- weather_prepared[c("Group.1","mean_temp")]
min_temp_ts <- ts(min_temp[,2], start = c(2015,9), frequency = 12)
min_temp_train <- window(min_temp_ts, start = c(2015,9), end = c(2016,9))
min_temp_test <- window(min_temp_ts, start = c(2016,10), end = c(2017,2))
plot(min_temp_train)
plot(min_temp_test)

# make window on potholes
monthly_ts_train_VAR <- window(monthly_ph_ts, start = c(2015, 9), end = c(2016,9) , frequency = 12) # take a window on time from July 2011 to December 2016.

# we do a similar step below
monthly_ts_test_VAR <- window(monthly_ph_ts, start = c(2016, 10), end = c(2017,2) , frequency = 12)

# create first VAR model #
library(vars)
library(astsa)
par(mfrow = c(2,1))
plot(monthly_ts_train_VAR)
plot(min_temp_train)

X <- cbind(monthly_ts_train_VAR,min_temp_train)

plot.ts(X, main = "", xlab = "")
VAR_model <- VAR(X, p=1, type="both")
summary(VAR_model)
VAR_model$varresult$monthly_ts_train_VAR
pred_VAR = predict(VAR_model,n.ahead = 5)
sqrt(mean((monthly_ts_test-pred_VAR$fcst$monthly_ts_train_VAR[,"fcst"])^2))

# check correlation sof the data
library(corrplot)
weather_prepared_2 <- cbind(weather_prep_final)
weather_prepared_2[is.na(weather_prepared_2)] <- 0
cordata <- cbind(weather_prepared_2[,2:12], g_pw_ph[51:68,2])
par(mfrow = c(1,1))
cor_of_cordata <- cor(cordata)
count_cors <- cor_of_cordata[,12]
count_cors[count_cors > .5 | count_cors < -.5]
corrplot(cor_of_cordata, method="circle")

# create VAR of snow, mean_pressure, mean_wind_spd ,   mean_visib
last_var <- weather_prepared[c("Group.1","snow","mean_pressure","mean_wind_spd", "mean_visib")]
snow_var_ts <- ts(last_var[,2], start = c(2015,9), frequency = 12)
mean_pressure_var_ts <- ts(last_var[,3], start = c(2015,9), frequency = 12)
mean_wind_spd_ts <- ts(last_var[,4], start = c(2015,9), frequency = 12)
mean_visib_ts <- ts(last_var[,5], start = c(2015,9), frequency = 12)

snow_var_train <- window(snow_var_ts, start = c(2015,9), end = c(2016,9))
mean_pressure_var_train <- window(mean_pressure_var_ts, start = c(2015,9), end = c(2016,9))
mean_wind_spd_train <- window(mean_wind_spd_ts, start = c(2015,9), end = c(2016,9))
mean_visib_train <- window(mean_visib_ts, start = c(2015,9), end = c(2016,9))

snow_var_test <- window(snow_var_ts, start = c(2016,10), end = c(2017,2))
mean_pressure_var_test <- window(mean_pressure_var_ts, start = c(2016,10), end = c(2017,2))
mean_wind_spd_test <- window(mean_wind_spd_ts, start = c(2016,10), end = c(2017,2))
mean_visib_test <- window(mean_visib_ts, start = c(2016,10), end = c(2017,2))

# make window on potholes
monthly_ts_train_VAR <- window(monthly_ph_ts, start = c(2015, 9), end = c(2016,9) , frequency = 12) # take a window on time from July 2011 to December 2016.

# we do a similar step below
monthly_ts_test_VAR <- window(monthly_ph_ts, start = c(2016, 10), end = c(2017,2) , frequency = 12)

# create VAR model with many vars#
par(mfrow = c(2,1))
plot(monthly_ts_train_VAR)
plot(min_temp_train)

X_2 <- cbind(monthly_ts_train_VAR,snow_var_train, mean_pressure_var_train,
             mean_wind_spd_train,mean_visib_train)

plot.ts(X_2, main = "", xlab = "")
VAR_model_2 <- VAR(X_2, p=1, type="both")
summary(VAR_model_2)
VAR_model_2$varresult$monthly_ts_train_VAR
pred_VAR_2 = predict(VAR_model_2,n.ahead = 5)
sqrt(mean((monthly_ts_test-pred_VAR_2$fcst$monthly_ts_train_VAR[,"fcst"])^2)) # 280.87 RMSE predictions look much better!

plot(as.numeric(monthly_ts_test), type="l")
lines(pred_VAR_2$fcst$monthly_ts_train_VAR[,1], type='l', col="blue")
lines(pred_VAR_2$fcst$monthly_ts_train_VAR[,3], type='l', col="red", lty = 2)
lines(pred_VAR_2$fcst$monthly_ts_train_VAR[,2], type='l', col="red", lty=2)

pred_VAR_2$fcst$monthly_ts_train_VAR[,2]

