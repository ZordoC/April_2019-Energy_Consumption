
AddYearsAndPriceFunction <-  function(data,hi = 6, mi = 30 ,hf = 22,mf=30,Pday = 0.1580,Pnight = 0.1230)
{
  data$year <- year(data$DateTime)
  data$quarter <- quarter(data$DateTime)
  data$month <- month(data$DateTime)
  data$week <- week(data$DateTime)
  data$day <- day(data$DateTime)
  data$hour <- hour(data$DateTime)
  data$minute <- minute(data$DateTime)
  data <- data  %>% mutate(Price = ifelse(hour >= hi + 1  & hour <= hf - 1,Pday,
                                                ifelse(hour == hi & minute >= mi  | hour == hf & minute <= mf 
                                                       ,Pday,Pnight)))  %>% 
    rename( Kitchen = Sub_metering_1, LaundryRoom = Sub_metering_2, Heat = Sub_metering_3)
  
  data <- ConversionFunction(data)  
 
    data
}


ConversionFunction <- function(data)
{
  data$Kitchen <- data$Kitchen / 1000
  data$LaundryRoom <- data$LaundryRoom / 1000
  data$Heat <- data$Heat / 1000
  data$Global_active_power <- data$Global_active_power / 60 
  data$residuals <- data$Global_active_power - data$Kitchen - data$LaundryRoom - data$Heat
  
  data
  
}


  

MetricsOfTotalPower = function(df, var1, var2)
{
  var1 <-  enquo(var1); var2 <-  enquo(var2);
  df %>%
    group_by(!!var1) %>%
    summarise(Total = sum(!!var2),
              mean = mean(!!var2),
              sd = sd(!!var2))
  
}

TotalFor



AllDivisionsTotalSum <-  function(data,gvar1,gvar2,var1,var2,var3) 
  {
  varg1 <-  enquo(gvar1); varg2 <-  enquo(gvar2);
  
  var1 <- enquo(var1); var2 <- enquo(var2); var3 <- enquo(var3) ;
    
Total <-  data %>% group_by(!!varg1,!!varg2) %>% summarise(Kitchen =sum(!!var1),
                                                 Laundry = sum(!!var2),
                                                 Heating =sum(!!var3))
                                                
    Total
    }



  
 EnergySum <- AllDivisionsTotalSum(FullYears,quarter,month,Kitchen,LaundryRoom,Heat)

  
  

# Function = function(data,tframe,values)
# {
#   
#   L <- data %>% group_by_(.dots = lazyeval::lazy(tframe)) %>% summarise_( Total =  values) 
#   
#   L
# }
