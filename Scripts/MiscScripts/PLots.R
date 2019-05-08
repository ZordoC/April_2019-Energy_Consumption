

subset_1 <- FullYears[,c("DateTime", "Kitchen")]
colnames(Subset_1) <- c("Time", "Data")
subset_2 <- FullYears[,c("DateTime", "LaundryRoom")]
colnames(subset_2) <- c("Time", "Data")
subset_3 <- FullYears[,c("DateTime", "Heat")]
colnames(subset_3) <- c("Time", "Data")


ggplot(subset_1, aes(x = Time, y = Data)) +
  geom_smooth() +
  geom_smooth(data = subset_2, aes(x = Time, y = Data), col = "red") +
  geom_smooth(data = subset_3, aes(x = Time, y = Data), col = "green")




Y <- FullYears %>% group_by(month) %>% summarise( Kitchen = sum(Sub_metering_1), Laundry = sum(Sub_metering_2),
                  WaterHeater_AirConditioner =sum(Sub_metering_3))  %>% melt(id='month')


mymonths <- c("Jan","Feb","Mar",
                             "Apr","May","Jun",
                             "Jul","Aug","Sep",
                             "Oct","Nov","Dec")
              
FullYears_tidy$month <- mymonths[FullYears_tidy$month]

colnames(Y)[3] <-"Total Watts"




ggplot(Y,aes(x=month,y=`Total Watts`,fill=variable)) + geom_bar(stat ="identity",position=position_stack(reverse = TRUE)) + 
  xlab("Months") 




  plot(FullYears$Global_active_power ~ FullYears$DateTime, ylab = "Global Active Power (kilowatts)", xlab = "", type = "l")


  FullYears_tidy <- FullYears %>%
    gather(Meter, Watt_hr, `Kitchen`, `LaundryRoom`, `Heat`)  

  
  #### Year ####
  FullYears_tidy %>%
    group_by(year, Meter) %>%
    summarise(sum=sum(KWatt_hr * Price)) %>%
    ggplot(aes(x=factor(year), sum, group=Meter,fill=Meter)) +
    labs(x='Year', y='Proportion of Energy Usage') +
    ggtitle('Proportion of Total Yearly Energy Consumption') +
    geom_bar(stat='identity', position=position_stack(reverse = TRUE), color='black') +
    theme(panel.border=element_rect(colour='black', fill=NA)) +
    theme(text = element_text(size = 14))

  ##### Month #####
  
  FullYears_tidy %>%
    filter(month(DateTime)==11 & year(DateTime) == 2008) %>%
    mutate(Day=lubridate::day(DateTime)) %>%
    group_by(month,Day, Meter) %>%
    summarise(sum=sum(KWatt_hr*Price)) %>%
    ggplot(aes(x=factor(Day), y=sum)) +
    labs(x='Day of the Month ', y='Total Spent $$') +
    ggtitle('Money spent') +
    geom_bar(stat='identity', aes(fill = Meter), colour='black', position = position_stack(reverse = TRUE)) +
    theme(panel.border=element_rect(colour='black', fill=NA)) +
    theme(text = element_text(size = 14))
  
  
#### BY the hour #### 
  
  FullYears_tidy %>%
    filter(month(DateTime) == c(1,2,11,12)) %>%
    group_by(hour(DateTime), Meter) %>%
    summarise(sum=sum(KWatt_hr)) %>%
    ggplot(aes(x=factor(`hour(DateTime)`), y=sum)) +
    labs(x='Hour of the Day', y='kWh') +
    ggtitle('Total Energy Usage by Hour of the Day') +
    geom_bar(stat='identity', aes(fill = Meter), position = position_stack(reverse = TRUE), colour='black') +
    theme(panel.border=element_rect(colour='black', fill=NA)) +
    theme(text = element_text(size = 14))
  
  
#### Part 2 ####
  
 #Missing Data
  
  
  
    aggr(FullYears, col=c('navyblue','red'),
       numbers=TRUE, 
       sortVars=TRUE, 
       labels=names(FullYears),
       cex.axis=.7, 
       gap=3, 
       ylab=c("Histogram of missing data","Pattern"), 
       digits=2)

 
  
   
  
 plt <-  FullYears %>% filter(year == 2008, week == 2 ) 

 plot(plt$Kitchen)    
    
    


