#### Important Functions ####


pre_function <-  function(data,hi = 6, mi = 30 ,hf = 22,mf=30,Pday = 0.1580,Pnight = 0.1230)
  
{
  data$year <- year(data$DateTime)
  data$quarter <- quarter(data$DateTime)
  data$month <- month(data$DateTime)
  data$week <- week(data$DateTime)
  data$day <- day(data$DateTime)
  data$hour <- hour(data$DateTime)
  data$minute <- minute(data$DateTime)
  data <- data  %>% mutate(Price = ifelse(hour >= hi + 1  & hour <= hf - 1, Pday,
                                          ifelse(hour == hi & minute >= mi  | hour == hf & minute <= mf 
                                                 ,Pday,Pnight)))  %>% 
    rename( Kitchen = Sub_metering_1, LaundryRoom = Sub_metering_2, Heat = Sub_metering_3) %>%
    conversion_function()  
  data <- data[ ,c("DateTime","Global_active_power","Kitchen","LaundryRoom","Heat","year","quarter","month","week","day","hour","minute","Price","Cost","residuals") ]
  data
}


conversion_function <- function(data)
{
  
  data$Kitchen <- data$Kitchen / 1000
  data$LaundryRoom <- data$LaundryRoom / 1000
  data$Heat <- data$Heat / 1000
  data$Global_active_power <- data$Global_active_power / 60 
  data$Rest_of_the_house <- data$Global_active_power - data$Kitchen - data$LaundryRoom - data$Heat
  data$Cost <- data$Global_active_power * data$Price
  data
}

granularity <- function(df, meter, var1, var2)
{
  meter <-  enquo(meter)
  var1 <-  enquo(var1)
  var2 <-  enquo(var2)
  df %>%
    group_by(!!var1,!!var2) %>%
    summarise(Total_Power = sum(!!meter ),
              Total_Cost = sum(Cost))
}

train_test_sets <- function(ts,start_train,end_train,start_test,end_test)
{
  train <- window(ts,start_train,end_train)
  test <- window(ts,start_test,end_test)
  sets <- list(my_train = train,my_test = test)
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



all_forecasts <- function(models,train,h) 
{
  l <- list()
  for(i in 1:length(models))
  {
    l[[i]] <- model_training_forecasting(train,h,models[i])
  }
  l
}

accuracy_function <-  function(forecasts,test)
{
  c <- list()
  for (i in 1:length(forecasts))
  {
    c[[i]] <- as.data.frame(accuracy(forecasts[[i]],test))
  }
  
  c
  
}  



error_function <- function(acclist,metric,models =  c("holtwinters","ets","autoarima"))
{
  b <- c()
  
  for( i in 1:3)
  {
    b <- cbind(b,acclist[[i]]$metric) 
  }
  b
}


##### Optional/Fun functions ####



long_format <- function(data) 
{
  data %>% gather(mMeter,KwH,`Kitchen`,`LaundryRoom`, `Heat`) %>%  
    data$Meter <- factor(data$Meter)
}


change_names <- function(data)
{
  data %>%   rename( Kitchen = Sub_metering_1, LaundryRoom = Sub_metering_2, Heat = Sub_metering_3)
  data 
}





metrics_of_total_power <- function(df, var1, var2)
  
{
  var1 <-  enquo(var1)
  var2 <-  enquo(var2)
  df %>%
    group_by(!!var1) %>%
    summarise(Total = sum(!!var2),
              mean = mean(!!var2),
              sd = sd(!!var2))
}


AllDivisionsTotalSum <-  function(data,gvar1,gvar2,var1,var2,var3) 
{
  varg1 <-  enquo(gvar1);
  varg2 <-  enquo(gvar2);
  var1 <- enquo(var1);
  var2 <- enquo(var2);
  var3 <- enquo(var3) ;
  Total <-  data %>% group_by(!!varg1,!!varg2) %>% summarise(Kitchen =sum(!!var1),
                                                             Laundry = sum(!!var2),
                                                             Heating =sum(!!var3))
  Total
}


time_series_creator <- function(data,f)
{
  time_series <- ts(data,frequency = f)
  components <- decompose(time_series)
  plot(components)
  results <- list(ts = time_series ,comp = components)
}

