

Subset_1 <- FullYears[,c("DateTime", "Sub_metering_1")]
colnames(Subset_1) <- c("Time", "Data")
Subset_2 <- FullYears[,c("DateTime", "Sub_metering_2")]
colnames(Subset_2) <- c("Time", "Data")
Subset_3 <- FullYears[,c("DateTime", "Sub_metering_3")]
colnames(Subset_3) <- c("Time", "Data")


ggplot(Subset_1, aes(x = Time, y = Data)) +
  geom_smooth() +
  geom_smooth(data = Subset_2, aes(x = Time, y = Data), col = "red") +
  geom_smooth(data = Subset_3, aes(x = Time, y = Data), col = "green")


# 
# 
# ggplot(MonthM1,aes(x=month,y=TotalWatts)) + geom_bar(stat = "identity")
# 
# 
# mymonths <- c("Jan","Feb","Mar",
#               "Apr","May","Jun",
#               "Jul","Aug","Sep",
#               "Oct","Nov","Dec")
# 
# MonthM1$month <- mymonths[ MonthM1$month ]
# 
# MonthM2$month <- mymonths [ MonthM2$month]
# 
# MonthM3$month <- mymonths [ MonthM3$month]
# 
# MonthM1$month <- factor(MonthM1$month,levels = c("Jan","Feb","Mar",
#                                         "Apr","May","Jun",
#                                         "Jul","Aug","Sep",
#                                         "Oct","Nov","Dec"))
# 
# MonthM2$month <- factor(MonthM2$month,levels = c("Jan","Feb","Mar",
#                                                  "Apr","May","Jun",
#                                                  "Jul","Aug","Sep",
#                                                  "Oct","Nov","Dec"))
# 
# 
# 
# MonthM3$month <- factor(MonthM3$month,levels = c("Jan","Feb","Mar",
#                                                  "Apr","May","Jun",
#                                                  "Jul","Aug","Sep",
#                                                  "Oct","Nov","Dec"))
# 
# 
# 
# 
# ggplot(MonthM1,aes(x=month,y=TotalWatts)) + geom_bar(stat = "identity",fill='yellow') + geom_bar(data=MonthM2,stat = 'identity',fill='red')
# 
# 




Y <- FullYears %>% group_by(month) %>% summarise( Kitchen = sum(Sub_metering_1), Laundry = sum(Sub_metering_2),
                  WaterHeater_AirConditioner =sum(Sub_metering_3))  %>% melt(id='month')

Y$month <- mymonths[Y$month]

colnames(Y)[3] <-"Total Watts"



Y$month <- factor(Y$month,levels = c("Jan","Feb","Mar",
                                                 "Apr","May","Jun",
                                                 "Jul","Aug","Sep",
                                                 "Oct","Nov","Dec"))



ggplot(Y,aes(x=month,y=`Total Watts`,fill=variable)) + geom_bar(stat ="identity",position=position_dodge()) + 
  xlab("Months") 



















