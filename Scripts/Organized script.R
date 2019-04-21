library(RMySQL)
library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape2)




Gfunction(FullYears,year,Global_active_power)






#### Energy ####
Total.Energy.Ammount.Month <-FullYears %>% group_by(month) %>% summarise(Globalenergry = sum(Global_active_power))

Total.Energy.Ammount.Weeks <- FullYears %>% group_by(weekdays(FullYears$DateTime)) %>% summarise(Globalenergry = sum(Global_active_power))

TotalE.kitchen <- FullYears %>% group_by(month) %>% summarise(Kitchen.KwH = sum(Kitchen/(1000))) 

TotalE.Laundry <- FullYears %>% group_by(month) %>% summarise(Kitchen.KwH = sum(LaundryRoom/(1000))) 


#### Money ####
Total.Money.Hour <- FullYears %>% group_by(year) %>% summarise(Total)

Total.Money.Month <- FullYears   %>% group_by(month) %>% summarise(TotalMoney = sum(Price*Global_active_power/(60) ))

Total.Money.Week <- FullYears  %>% group_by(week) %>% summarise(TotalMoney = sum(Price*Global_active_power/60) )

Total.Money.Hour <- FullYears %>%  group_by(hour) %>% summarise(TotalMoney = sum(Price*Global_active_power/60))



Average.Money.Month <- FullYears   %>% group_by(month) %>% summarise(TotalMoney = sum(Price*Global_active_power/(60*3) ))

Total.Money.WeekDay <- FullYears %>% group_by(weekdays(FullYears$DateTime)) %>% summarise(MoneySpentOnAverage = sum(Price*Global_active_power/(60*53*3)))



ggplot(Total.Money.Month,aes(x=))
