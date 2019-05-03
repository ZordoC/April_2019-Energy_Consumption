library(RMySQL)
library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape2)
library(ggfortify)
library(forecast)
library(tseries)
library(opera)
library(forecastHybrid)



monthly_data <-  granularity(FullYears,Global_active_power,year,month)

monthly_data_ts <-  ts(monthly_data$Total_Cost,frequency=12,start = c(2007,1))


sets <- train_test_sets(monthly_data_ts,c(2007,1),c(2009,12),c(2010,1),c(2010,11))


models <- c("holtwinters","autoarima","ets")

sets$my_train


l <- all_forecasts(models,sets$my_train,11)


