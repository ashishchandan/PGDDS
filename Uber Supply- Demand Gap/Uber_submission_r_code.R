#load required pacakages
library(dplyr)
library(ggplot2)
library(stringr)

#Import Data
uber_master <- read.csv("Uber Request Data.csv", stringsAsFactors = F)
uber <- uber_master

#str(uber)
#head(uber)


#Data cleaning
#Request.timestamp & Drop.timestamp cointain time in two different format
#convert into same format

uber$Request.timestamp <-str_replace_all(uber$Request.timestamp,"/", "-")
uber$Drop.timestamp <-str_replace_all(uber$Drop.timestamp,"/", "-")

#convert the two fields into timestamp class

uber$Request.timestamp <- as.POSIXct(uber$Request.timestamp, format = "%d-%m-%Y %H:%M")
uber$Drop.timestamp <- as.POSIXct(uber$Drop.timestamp, format = "%d-%m-%Y %H:%M")

#str(uber)

#extract hour field from two timestamp field
uber$Request.hour <- as.numeric(format(uber$Request.timestamp , "%H"))
uber$Drop.hour <- as.numeric(format(uber$Drop.timestamp , "%H"))

#uber$not.competed <- ifelse(uber$Status== "Trip Completed",0,1)

#Dividing day into day slots
#[0-5) -- Early_Morning
#[5-10) -- Morning
#[10-17) -- Day
#[17-22) -- Evening
#[22-0) -- Night

uber$Request_day_slot <- ifelse(uber$Request.hour <= 4,"Early_Morning",
                                ifelse(uber$Request.hour <= 9,"Morning",
                                       ifelse(uber$Request.hour <= 16,"Day",
                                              ifelse(uber$Request.hour <= 21,"Evening",
                                                     ifelse(uber$Request.hour <=23 , "Night")))))


#plot to show variation of number of requests with hours

p <- ggplot(uber,aes(x=Request.hour))
p1 <- p + geom_line( stat = "count") +
  labs(x="Request Hour", title="Request Count Over Hours of a Day",y="Number of Request") +
  scale_x_discrete(limits=c(0:23))



#print p1
p1

# plot P1 shows that during morning hours (5 - 9 AM) and evning (5 - 9 PM) , demand is quite high

# further breaking down p1 (total demand variation) into pickup point

p2 <- p + geom_line(aes(colour=Pickup.point),stat="count") +geom_point(aes(colour=Pickup.point),stat="count") +
  labs(x="Request Hour", title="Number Of Requests Over Hours of a Day",y="Number of Request") +
  scale_x_discrete(limits=c(0:23))


#print p2  
p2

#plot the same graphs(geom_bar) using day slot 
ggplot(uber) + geom_bar(aes(x=factor(uber$Request_day_slot,levels = c("Early_Morning", "Morning" , "Day","Evening","Night" )),fill=Pickup.point)) + theme_bw()
#line plot is looking better that bar plot to continuing with line plot

# plot P2 shows that during morning hours (5 - 9 AM) demands are high in city for airport
# and evning (5 - 9 PM)  demands are high at airport for city

# breaking the plot p1 on the basis of status ( Trip Completed, No Cab Available and Canclled) at Airport and City

p3 <- p+ geom_line(aes(colour=Status),stat="count") +geom_point(aes(colour=Status),stat="count") +
  facet_grid(Pickup.point~.) + scale_x_discrete(limits=c(0:23)) +
  labs(x="Request Hour", title="Request Status Over Hour of a Day",y="Number of Request")
  
#print p3
p3

#p3 show two pressing issues :
# 1) number of cancellation in city for airport during morning hours (5 - 9 AM) is quite high
# 2) number of request with status "No Cars Available" at airport during evening hours (5 - 9 PM) is quite high


# percentage of request cancelled in city for airport during morning hours
request_cancelled_city_mng <- uber %>% filter(Request.hour >=5,Request.hour<=9,Status=="Cancelled",Pickup.point=="City") %>% nrow()

request_city_mng <- uber %>% filter(Request.hour >=5,Request.hour<=9,Pickup.point=="City") %>% nrow()

request_cancelled_city_mng*100/request_city_mng # 49%

# percentage of request with No Cars Available at Airport during evening hours

request_no_cars_airport_evng <- uber %>% filter(Request.hour >=17,Request.hour<=21,Status=="No Cars Available",Pickup.point=="Airport") %>% nrow()

request_airport_evng <- uber %>% filter(Request.hour >=17,Request.hour<=21,Pickup.point=="Airport") %>% nrow()

request_no_cars_airport_evng*100/request_airport_evng # 73%

p3 + labs(subtitle="Percentage of request cancelled during morning hours(5-9AM) is around 50%
Percentage of request with No Cars Available at Airport during evening hours(5-9PM) is around 75%")

#############################################Part 2###########################################
#Supply/demand 


#Dataframe cointaining number of requests in each hour of day
Demand_df <- uber %>% group_by(Request.hour) %>% summarise(Demand=n()) %>% as.data.frame()

#Dataframe cointaining number of requests in each hour of day with status "completed"
Demand_completed_df <- uber %>% filter(Status=='Trip Completed') %>% group_by(Request.hour) %>% summarise(Demand_completed=n()) %>% as.data.frame()

#Dataframe cointaining number of requests in each hour of day (total + completed)
Demand_completed_df2 <- merge(x=Demand_df,y=Demand_completed_df, by="Request.hour") %>% as.data.frame()

#Ratio to determine number of requests completed
Demand_completed_df2$Ratio_demand_completed <- Demand_completed_df2$Demand_completed/Demand_completed_df2$Demand

Demand_completed_df2$Request.hour <- as.numeric(Demand_completed_df2$Request.hour)

#percentage of request completed over hours of day
p4 <- ggplot(Demand_completed_df2) + geom_line(aes(x=Request.hour, y=Ratio_demand_completed*100)) + 
  geom_point(aes(x=Request.hour, y=Ratio_demand_completed*100)) +
  labs(title="Percentage of Demand Fulfilled for Requests", x="Request Hour",y="% of Demand Completed") +
  scale_x_discrete(limits=c(0:23))

#print p4
p4

#From the plots, we can say that during morning and evening hours, percentage of request completed is
# quite low i.e. most supply-demand gap exists during these hours.



#Supply/demand in City for Airport


#Dataframe cointaining number of requests in each hour of day
Demand_in_city_df <- uber %>% filter(Pickup.point=='City') %>% group_by(Request.hour) %>% summarise(Demand_in_city=n()) %>% as.data.frame()

#Dataframe cointaining number of requests in each hour of day with status "completed"
Demand_completed_in_city_df <- uber %>% filter(Pickup.point=='City',Status=='Trip Completed') %>% group_by(Request.hour) %>% summarise(Demand_completed_in_city=n()) %>% as.data.frame()

#Dataframe cointaining number of requests in each hour of day (total + completed)
Demand_completed_in_city_df2 <- merge(x=Demand_in_city_df,y=Demand_completed_in_city_df, by="Request.hour") %>% as.data.frame()

#Ratio to determine number of requests completed
Demand_completed_in_city_df2$Ratio_demand_completed_in_city <- Demand_completed_in_city_df2$Demand_completed_in_city/Demand_completed_in_city_df2$Demand_in_city

Demand_completed_in_city_df2$Request.hour <- as.numeric(Demand_completed_in_city_df2$Request.hour)

#percentage of request completed over hours of day
p5 <- ggplot(Demand_completed_in_city_df2) + geom_line(aes(x=Request.hour, y=Ratio_demand_completed_in_city*100)) + 
  geom_point(aes(x=Request.hour, y=Ratio_demand_completed_in_city*100)) +
  labs(title="Percentage of Demand Fulfilled for Requests in City for Airport", x="Request Hour",y="% of Demand Completed") +
  scale_x_discrete(limits=c(0:23))

#print p5
p5

#From the plots, we can say that during morning hours, there is very high demand of cars for airport 
# in city however number of request completed is very low results in high gap between demand and supply.

#percentage of request completed in morning hours in city for airport
Demand_completed_in_city_df2 %>% filter(Request.hour %in% 5:9) %>% summarise(sum(Demand_completed_in_city)/sum(Demand_in_city))
#28%
 

p5 +labs(subtitle="During morning hours(5-9 AM), only less than 30% of total requests got completed")


#Supply/demand at Airport


Demand_at_airport_df <- uber %>% filter(Pickup.point=='Airport') %>% group_by(Request.hour) %>% summarise(Demand_at_airport=n()) %>% as.data.frame()


Demand_completed_at_airport_df <- uber %>% filter(Pickup.point=='Airport',Status=='Trip Completed') %>% group_by(Request.hour) %>% summarise(Demand_completed_at_airport=n()) %>% as.data.frame()

Demand_completed_at_airport_df2 <- merge(x=Demand_at_airport_df,y=Demand_completed_at_airport_df, by="Request.hour") %>% as.data.frame()


Demand_completed_at_airport_df2$Ratio_demand_completed_at_airport <- Demand_completed_at_airport_df2$Demand_completed_at_airport/Demand_completed_at_airport_df2$Demand_at_airport


#demand fulfilled

p6 <- ggplot(Demand_completed_at_airport_df2) + geom_line(aes(x=as.numeric(Request.hour), y=Ratio_demand_completed_at_airport*100)) +
  geom_point(aes(x=as.numeric(Request.hour), y=Ratio_demand_completed_at_airport*100)) +
  labs(title="Percentage of Demand Fulfilled for Requests at Airport", x="Request Hour",y="% of Demand Completed") +
  scale_x_discrete(limits=c(0:23))

#print p6
p6


#From the plots, we can say that during evening hours, there is very high demand of cars at airport 
# however number of request completed is very low results in high gap between demand and supply.


#percentage of request completed in evening hours at airport
Demand_completed_at_airport_df2 %>% filter(Request.hour %in% 17:21) %>% summarise(sum(Demand_completed_at_airport)/sum(Demand_at_airport))
#21%

p6 + labs(subtitle="During evening hours(5-9 PM), only around 20% of requests have got complete")