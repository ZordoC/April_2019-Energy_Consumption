library(RMySQL)
library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape2)
##### Query Data ####

con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!', dbname='dataanalytics2018', 
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')



Year06<- dbGetQuery(con, "Select * FROM yr_2006")

Year07 <- dbGetQuery(con,"SELECT * FROM yr_2007")

Year08 <- dbGetQuery(con,"SELECT * FROM yr_2008")

Year09 <- dbGetQuery(con,"SELECT * FROM yr_2009")

Year10 <- dbGetQuery(con,"SELECT * FROM yr_2010")


#### Pre-Process ####

FullYears <- bind_rows(Year07,Year08,Year09)

FullYears <-cbind(FullYears,paste(FullYears$Date,FullYears$Time), stringsAsFactors=FALSE)

colnames(FullYears)[11] <-"DateTime"

FullYears <- FullYears[,c(ncol(FullYears), 1:(ncol(FullYears)-1))]

FullYears$DateTime <- as.POSIXct(FullYears$DateTime, "%Y/%m/%d %H:%M:%S")

attr(FullYears$DateTime, "tzone") <- "Europe/Paris"


FullYears <- AddYearsAndPriceFunction(FullYears)



# FullYears <-ConversionFunction(FullYears)


#FullYears$residuals <- FullYears$Global_active_power - FullYears$Kitchen - FullYears$LaundryRoom - FullYears$HeatingAirdcondioner

# MonthM1 <- FullYears %>% select(Sub_metering_1,month) %>% group_by(month) %>% summarise( TotalWatts = sum(Sub_metering_1))
# 
# MonthM2 <- FullYears %>% select(Sub_metering_2,month) %>% group_by(month) %>% summarise( TotalWatts = sum(Sub_metering_2))
# 
# MonthM3 <- FullYears %>% select(Sub_metering_3,month) %>% group_by(month) %>% summarise( TotalWatts = sum(Sub_metering_3))
# 
# YearM1 <- FullYears %>% select(Sub_metering_1,year) %>% group_by(year) %>% summarise( Total_Watts = sum(Sub_metering_1))
# 
# YearM2 <- FullYears %>% select(Sub_metering_2,year) %>% group_by(year) %>% summarise( Total_Watts = sum(Sub_metering_2))
# 
# YearM3 <- FullYears %>% select(Sub_metering_3,year) %>% group_by(year) %>% summarise( Total_Watts = sum(Sub_metering_3))
# 
# WeekM1 <- FullYears %>% select(Sub_metering_1,week.month) %>% group_by(week.month) %>% summarise( Total_Watts = sum(Sub_metering_1))
# 
# WeekM2 <- FullYears  %>% select(Sub_metering_2,week.month) %>% group_by(week.month) %>% summarise( Total_Watts = sum(Sub_metering_2))
# 
# WeekM3 <- FullYears  %>% select(Sub_metering_3,week.month) %>% group_by(week.month) %>% summarise( Total_Watts = sum(Sub_metering_3))

