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
#### Testing / Experiments ####


Test1 <- FullYears %>% group_by(year,month) %>% summarise(TotalMonthConsumption = sum(Global_active_power)
                                                          )
Test1 <-  granularity(FullYears,Global_active_power,year,month)







Test2 <- FullYears %>% group_by(quarter,week) %>% summarise(TotalMonthConsumption=sum(Global_active_power))


  



time_series_test_1 <- ts(Test1$TotalMonthConsumption,frequency = 12,start = c(2007,1))

autoplot(time_series_test_1)

plot.ts(time_series_test_1)

model <- tslm(time_series_test_1 ~ trend + season)

forecastfitSM3 <- forecast(model, h=6)

plot(forecastfitSM3)

component_ts <- decompose(time_series_test_1)

plot(component_ts)

summary(component_ts)

ts_adjusted <- time_series_test_1 - component_ts$seasonal

autoplot(ts_adjusted)

plot(decompose(ts_adjusted))

ts_hw <- HoltWinters(ts_adjusted,beta = FALSE,gamma=FALSE)

plot(ts_hw,ylim=c(500,1000))

ts_hw_for <- forecast(ts_hw,h=12)

plot(ts_hw_for,ylim=c(500,1000),start(4))



time_series_test_2 <- ts(Test1$TotalMonthConsumption,frequency = 12, start =  c(2007,1))


L <- train_test_sets(time_series_test_2,c(2007,1),c(2009,12),c(2010,1),c(2010,11))



my_train <- window(time_series_test_2, c(2007,1) , c(2009,12))

my_test <- window(time_series_test_2,start = c(2010,1))


acf(my_train,lag.max = 20)
pacf(my_train,lag.max = 20)




diff_train <- diff(L$my_train)

diff_train2 <- diff(diff_train)

diff_train3 <- diff(diff_train2)

adf.test(my_train)
adf.test(diff_train)
adf.test(diff_train2)
adf.test(diff_train3)

#### Comparing different Models ####


compare_diff_models <- function(train,h,model1,model2,model3,model4)
{
M1 <- train %>% 
  model1() %>%
  forecast(h)

M2 <- train %>% 
  model2() %>%
    forecast(h)

M3 <- train %>% 
  model3(h)

M4 <- train %>% 
      model4() %>%
        forecast(h)


}

model_training_forecasting <-  function(train,h,model) 
{
  if(model == 'holtwinters') 
  {
    m <- hw(train,seasonal = "additive",h = h)  
  }
  else if(model == "autoarima")
  {
    m <- auto.arima(train) %>%
      forecast(h)
  }
  else if (model == "ets")
  {
    m <-ets(train) %>%
      forecast(h)
  } 
  m
}




hw(my_train,seasonal = 'additive',h = 11 )

hw(my_train,seasonal = 'additive')



autoplot(my_train)

HW <- L$

  m <- HoltWinters(L$my_train,seasonal = "additive")

stl1 <- my_train %>%  
      stlf(method = 'arima',12)

three_model_forecasts <- cbind(ETS=ETS$mean, ARIMA=ARIMA$mean, STL=STL$mean)

df <- cbind(my_test, X)

colnames(df) <- c("Data","ETS","ARIMA","STL")

# autoplot(time_series_test_2)+
#   autolayer(three_model_forecasts)+
#   xlab('Year') + ylab('Power Consumption')  
# 
# autoplot(df) +
#   xlab("Year") + ylab(expression("Power Consumprion"))
# 


MLpol0 <- mixture(model = "MLpol", loss.type = "square")

weights <- predict(MLpol0, X , my_train, type='weights')

head(weights)




z <- ts(predict(MLpol0, three_model_forecasts, my_test, type='response'), freq=12,start =  c(2010,1),end=c(2010,11))

my_test

df <- cbind(z,my_test)

colnames(df) <- c("Data","Mixture")

autoplot(df) +
  xlab("Year") + ylab("Power Consumption")

accuracy(z,my_test)


accuracy(z,my_test)
accuracy(ETS,my_test)
accuracy(ARIMA,my_test)
accuracy(hw_forecast,my_test)
accuracy(stl1,my_test)
hw_model <- HoltWinters(my_train)

  hw_forecast <- forecast:::forecast.HoltWinters(hw_model,h=h)

autoplot(time_series_test_2) +
  autolayer(hw_forecast$mean)+
  autolayer(ARIMA$mean)+
  autolayer(ETS$mean)+
  autolayer(z)
  

fit1 <- hybridModel(my_train,weights = "equal")


fit2 <- hybridModel(my_train,weights = 'insample')


checkresiduals(fc2)


fc1 <- forecast(fit1,h=12)

fc2 <- forecast(fit2,h=12)

autoplot(fc1)

autoplot(fc2)

corrected <- cbind(Data= time_series_test_2,Hybrid1=fc1$mean,Hybrid2 = fc2$mean) %>% 
              autoplot()
  


accuracy(fc1,my_test)

accuracy(fc2,my_test)
  
# #### Energy ####
# total_energy_ammount_month <-FullYears %>% group_by(month) %>% summarise(Globalenergry = sum(Global_active_power))
# 
# total_energy_ammount_weeks <- FullYears %>% group_by(weekdays(FullYears$DateTime)) %>% summarise(Globalenergry = sum(Global_active_power))
# 
# total_energy_kitchen <- FullYears %>% group_by(month) %>% summarise(Kitchen.KwH = sum(Kitchen/(1000))) 
# 
# total_energy_laundry <- FullYears %>% group_by(month) %>% summarise(Kitchen.KwH = sum(LaundryRoom/(1000))) 
# 
# 
# #### Money ####
# total_money_hour <- FullYears %>% group_by(year) %>% summarise(Total)
# 
# total_money_month <- FullYears   %>% group_by(month) %>% summarise(TotalMoney = sum(Price*Global_active_power/(60) ))
# 
# total_money_week <- FullYears  %>% group_by(week) %>% summarise(TotalMoney = sum(Price*Global_active_power/60) )
# 
# total_money_hour <- FullYears %>%  group_by(hour) %>% summarise(TotalMoney = sum(Price*Global_active_power/60))
# 
# average_money_month <- FullYears   %>% group_by(month) %>% summarise(TotalMoney = sum(Price*Global_active_power/(60*3) ))
# 
# total_money_weekday <- FullYears %>% group_by(weekdays(FullYears$DateTime)) %>% summarise(MoneySpentOnAverage = sum(Price*Global_active_power/(60*53*3)))
# 
# 
# 
