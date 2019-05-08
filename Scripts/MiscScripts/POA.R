library(RMySQL)
library(dplyr)

library(lubridate)
con = dbConnect(MySQL(), user = 'deepAnalytics',password = 'Sqltask1234!',
                dbname= 'dataanalytics2018',
                host = 'data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

dbListTables(con)

# dbListFields(con,'iris')
# 
# 
# irisALL <- dbGetQuery(con,"SELECT * FROM iris")
# 
# 
# irisSELECT <- dbGetQuery(con, "SELECT SepalLengthCm,SepalWidthCm FROM iris")
# 
# 
# weekdays(as.Date('1990-03-10'))

dbListFields(con,'yr_2007')




Years <- c("yr_2006","yr_2007","yr_2008","yr_2009","yr_2010")





Y2006 <- dbGetQuery(con,"SELECT * from yr_2006")

Y2007 <- dbGetQuery(con,"SELECT * from yr_2007")

Y2008 <-dbGetQuery(con,"SELECT * from yr_2008") 

Y2009 <- dbGetQuery(con,"SELECT * from yr_2009")

Y2010 <-   dbGetQuery(con,"SELECT * from yr_2010")


str(Y2006)
str(Y2007)
str(Y2008)
str(Y2009)
str(Y2010)

summary(Y2010)

head(Y2010$Date)

Data3FullYears <-  bind_rows(Y2007,Y2008,Y2009,Y2010)


Data3FullYears <- cbind(Data3FullYears,paste(Data3FullYears$Date,Data3FullYears$Time),stringsAsFactors = FALSE )


summary(Data3FullYears)

Data3FullYears$`paste(Data3FullYears$Date, Data3FullYears$Time)`[1:30]



colnames(Data3FullYears)[11] <- "DateTime"

Data3FullYears <-  Data3FullYears[,c(ncol(Data3FullYears),1:(ncol(Data3FullYears)-1))]


head(Data3FullYears)


## Convert DateTime from POSIXlt to POSIXct 

Data3FullYears$DateTime <- as.POSIXct(Data3FullYears$DateTime,"%Y/%m/%d %H:%M:%S")

attr(Data3FullYears$DateTime, "tzone") <- "Europe/Paris"
head(Data3FullYears$DateTime)

Data3FullYears$year <- year(Data3FullYears$DateTime)


Data3FullYears$quarter <- quarter(Data3FullYears$DateTime)

Data3FullYears$month <- month(Data3FullYears$DateTime)

Data3FullYears$week <- week(Data3FullYears$DateTime)

Data3FullYears$day <- day(Data3FullYears$DateTime)

Data3FullYears$hour <- hour(Data3FullYears$DateTime)

Data3FullYears$minutes <- minute(Data3FullYears$DateTime)

