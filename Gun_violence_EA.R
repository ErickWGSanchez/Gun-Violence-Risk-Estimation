#############################
#Erick Guevara
#EDA: Historic Gun Vio 
#Capstone Project 
#############################

library(tidyverse)
library(ggplot2)
library(maps)
library(forecast)

gun_vio <- read.csv('C:/Users/justd/Desktop/IPI - PRSA/MainDatasets/all_incidents_rev.csv', header=T)

gun_vio$date <- as.Date(gun_vio$date, format = "%m/%d/%Y")

states <- map_data("state")

gun_vio2 <- gun_vio %>% arrange(region)


#replacing column names 
colnames(demo)[colnames(demo) == "state"] <- 'state_abr'
colnames(gun_vio)[colnames(gun_vio) == "city"] <- 'county'
colnames(gun_vio)[colnames(gun_vio) == "region"] <- 'state'






######################TIME SERIES FORECASTING#####################################

#1. visualization 

plot(gun_vio$year, gun_vio$n_killed, type="l", xlab='Year', ylab='Number of Killed')
plot(gun_vio$year, gun_vio$n_killed, xlab='Year', ylab='Number of Killed')

ggplot(data=gun_vio, aes(x=day, y=n_killed)) + 
  geom_point() + 
  labs(x ='Day of Month', title='Most Amount killed at Each Day')


df_state <- gun_vio %>% 
  group_by(state) %>%
  summarize(count = sum(n_killed))

ggplot(df_state, aes(x=state, y=count)) +
  geom_bar(stat = 'identity') + 
  labs(x='State', y="total County", title='Total Count by State') +
  theme(axis.text.x = element_text(angle= 90, hjust = 1))



#Checking for Stationary & Autocorrelation
# Creating Time series Model ARIMA 

nkilled_ts = ts(gun_vio$n_killed,start =c(2018, 1),
                end = c(2022, 5), frequency= 7)

plot(nkilled_ts)




avg_model = Arima(nkilled_ts, c(0,0,0))


#Checks for usual lag patterns in the auto correlation plot
#Shows the structure in terms of auto correlations 
ac <- acf(nkilled_ts)
plot(ac)




#5. Testing a Forecasts /Plotting Forecast

avg_forecast = forecast(avg_model)

avg_forecast = forecast(avg_model, 24, level = c(50, 95))

#Plotting the long term average
windows()
plot(nkilled_ts)
lines(avg_forecast$mean, col='red')

#Shows historical and future uncertain points
#50% prediction interval is in the blue box 
#95% prediction internval is in the grey box 

plot(avg_forecast)

#auto regressive non-seasonal model

#Forecasts one step into the future at a time then reverts back to the mean 

auto_Arima = auto.arima(nkilled_ts, seasonal = FALSE)
auto_forecast = forecast(auto_Arima)
plot(auto_forecast)


#auto regressive seasonal model 
seasonal_auto_model = auto.arima(nkilled_ts)
seasonal_auto_forecast = forecast(seasonal_auto_model, h=24, level= c(50,95))
plot(seasonal_auto_forecast)


#splitting the data into training and testing 
ts_train <- window(nkilled_ts, start=c(2018,1), end= c(2020,12))
ts_test <- window(nkilled_ts, start = c(2021, 1))

#Building the main auto Arima forecasting model
model <- auto.arima(ts_train)
gvd_forecast <- forecast(model, h=length(ts_test))

#Validating the timeseries model by showing the error table set 

accuracy(gvd_forecast, ts_test)

#Output:
#ME      RMSE       MAE  MPE MAPE      MASE        ACF1 Theil's U
#Training set -0.01868109 0.5250583 0.4631214  NaN  Inf 0.6285219 -0.01256615        NA
#Test set     -0.10453850 0.5039770 0.4662002 -Inf  Inf 0.6327003  0.34454548  


plot(gvd_forecast)







