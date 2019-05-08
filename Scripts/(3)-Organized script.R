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
install.packages('pacman')



##### $$$$ #####

monthly_data <-  granularity(FullYears,Global_active_power,year,month)

monthly_money_ts <- ts(monthly_data$Total_Cost,frequency = 12,start = c(2007,1))

sets <- train_test_sets(monthly_money_ts,c(2007,1),c(2009,12),c(2010,1),c(2010,11))

models <- c("holtwinters","ets","autoarima")

l <- all_forecasts(models,sets$my_train,11)

forecasts_mean_together <-cbind(l[[1]]$mean,l[[2]]$mean,l[[3]]$mean)

colnames(forecasts_mean_together) <- c("Holt-Winters","ETS","Auto-Arima")

a <- accuracy_function(l,sets$my_test)

b <- c()

for( i in 1:length(models))
{
  b <- cbind(b,a[[i]]$MAPE)  
  
} 
rownames(b) <-  c("Train","Test")

colnames(b) <- c("HoltWinters","ETS","Auto-Arima")

b <- as.data.frame(b)


##### Combining all models ####

df <- cbind(sets$my_test, forecasts_mean_together)

colnames(df) <- c("Data","HoltWinters","ETS","Auto-Arima")


MLpol0 <- mixture(model = "MLpol", loss.type = "square")

weights <- predict(MLpol0, forecasts_mean_together , sets$my_test, type='weights')

head(weights)

z <- ts(predict(MLpol0, forecasts_mean_together, sets$my_test, type='response'), freq=12,start =  c(2010,1),end=c(2010,11))

df <- cbind(z,sets$my_test)

colnames(df) <- c("Data","Mixture")

# autoplot(df) +
#   xlab("Year") + ylab("Power Consumption")

f <- as.data.frame(accuracy(z,sets$my_test))

f_mape <- as.data.frame(f$MAPE) 

for (i in 1:3) 
{
  
tests[i] <-  b[2,i]

}

tests <- as.data.frame(tests)

rownames(tests) <-  c("Holt-Winters","ETS","Auto-Arima")

rownames(f_mape) <- c("Mixture")

bind_rows(tests,f_mape)

##### ForecastHybrid Method

library(forecastHybrid)


fit1 <- hybridModel(train, weights="equal")

fit2 <- hybridModel(train, weights="insample")



