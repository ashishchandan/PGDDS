##################################################
########### Ecommerce Capstone Project ###########
#                                                #
# 0.1. Problem Statement                         #
# 1.1. Data Understanding                        #
# 1.2. Data Preparation and EDA                  #
# 2.   Model Building                            #
#                                                #
##################################################


# 0.1 Problem Statement: 

#####################################################################################
#                                                                                   #
# ElecKart is an e-commerce firm specialising in electronic products. Over the      #
# last one year, they had spent a significant amount of money in marketing.         #
# We need to develop a market mix model to observe the actual impact of different   #
# marketing variables over the last year. Using our understanding of the model, we  #
# have to recommend the optimal budget allocation for different marketing levers -  #
# for the next year.                                                                # 
#                                                                                   #
#####################################################################################


# 1.1. Data Understanding: 

#######################################################################################
#                                                                                     #
# Order level data:                                                                   #
#                                                                                     #
#                                                                                     #
# Number of Instances in Complete Dataset : 4,75,984                                  #
# Number of Attributes : 3                                                            #
#                                                                                     #
# Data Dictionary                                                                     #
# ____________________________________________________________________________________#
# Attribute_Name    | Description                                                     #
# __________________ _________________________________________________________________#
# FSN ID 	          : The unique identification of each SKU                           #
# Order Date        : Date on which the order was placed                              #
# Order item ID     : Suppose you order 2 different products under the same order,    #
#                     it generates 2 different order Item IDs under the same order ID;# 
#                     orders are tracked by the Order Item ID.                        #
# GMV               : Gross Merchandise Value or Revenue                              #  
# Units             : Number of units of the specific product sold                    #
# Order payment type: How the order was paid - prepaid or cash on delivery            #
# SLA               : Number of days it typically takes to deliver the product        #
# Cust id           : Unique identification of a customer                             #
# Product MRP       : Maximum retail price of the product                             #
# Product procurement SLA: Time typically taken to procure the product                #
#                                                                                     #
# Other Available Information:                                                        #
# 1. Monthly spends on various advertising channels                                   #
# 2. Days when there was any special sale                                             #
# 3. Monthly NPS score - this may work as a proxy to 'voice of customer'              #
#                                                                                     #
#######################################################################################


# Importing required packages
library(dplyr)
library(plyr)
library(lubridate)
library(MASS)
library(car)
library(DataCombine)
library(zoo)
library(glmnet)
library(DAAG)
library(ggplot2)

# Loading dataset  ConsumerElectronics.csv
ConsumerElectronics_master <- read.csv("ConsumerElectronics.csv", stringsAsFactors = F)

# modifying the column names 
colnames(ConsumerElectronics_master) <- c("FSN_ID","Order_date","Year","Month","Order_id","Order_item_id","gmv","Units","deliverybdays","deliverycdays", "payment_mode","SLA","cust_id","pincode","P_super_category","P_analytic_category","P_sub_category","P_analytic_vertical","MRP","Procurement_SLA")

# structure of dataset 
str(ConsumerElectronics_master)
summary(ConsumerElectronics_master)


# Subseting the data only for three categories.

ConsumerElectronics <- filter(ConsumerElectronics_master, P_sub_category %in% c("CameraAccessory","GamingAccessory","HomeAudio" ))

nrow(ConsumerElectronics)

str(ConsumerElectronics)


unique(ConsumerElectronics$deliverybdays)
unique(ConsumerElectronics$deliverycdays)
#Removing the fileds which are not required for analysis
ConsumerElectronics <- ConsumerElectronics[,!(names(ConsumerElectronics) %in% c("deliverybdays","deliverycdays","cust_id","pincode"))]


# data sanity Check : Data should be from july-2015 to June -2016,Let's remove other data from ecom dataset

ConsumerElectronics$Order_date <- as.Date(ConsumerElectronics$Order_date, "%Y-%m-%d")

ConsumerElectronics <- ConsumerElectronics[ConsumerElectronics$Order_date %in% as.Date(as.Date('2015-07-01'):as.Date('2016-06-30'),origin="1970-01-01"),]


## Convert the dataset into weekly levels. Week 1-53 starting from 2015-06-30

ConsumerElectronics$Order_week <-  (as.numeric(difftime(ConsumerElectronics$Order_date,as.Date('2015-06-30'), unit="day")) %/% 7)+1  


summary(ConsumerElectronics$MRP)

# checking for NA values
sum(is.na(ConsumerElectronics))
sapply(ConsumerElectronics, function(x) { sum(is.na(x))})
#only gmv has NAs
# for gmv whhich has NA values, we will consider that the item has been sold with no-profit no-loss

ConsumerElectronics$gmv[is.na(ConsumerElectronics$gmv)] <- ConsumerElectronics$MRP[is.na(ConsumerElectronics$gmv)] * ConsumerElectronics$Units[is.na(ConsumerElectronics$gmv)]

sum(is.na(ConsumerElectronics$gmv)) # 0


# gmv should not be more than MRP*units as product cannot be sold higher than MRP
# we will remove all the records where the product GMV > MRP*Units
ConsumerElectronics <- subset(ConsumerElectronics, (MRP*Units)>=gmv)

## Let's divide the dataset into 3 dataframes based on category.


GamingAccessory <-ConsumerElectronics[ConsumerElectronics$P_sub_category=="GamingAccessory" ,] 

CameraAccessory<-ConsumerElectronics[ConsumerElectronics$P_sub_category=="CameraAccessory",]

HomeAudio<-ConsumerElectronics[ConsumerElectronics$P_sub_category=="HomeAudio",]


# Note that we havent removed or replaced the records having gmp=0 as we have used this to build a KIP
# all the records having MRP=0 but GMV >0 have already droped as part of subseting on condition (MRP*Units)>=gmv(line number 77)


# reading nps details
library(readxl)
nps<-read_excel("Media data and other information.xlsx", sheet="Monthly NPS Score")
Month <- c(7:12,1:6)
Year <- c(rep(2015,6),rep(2016,6))
nps <- as.numeric(nps[1,-1])

nps <- cbind(Month=Month,Year=Year,NPS=nps)

# note that we have considered same nps score for entire month


##############################################################
## Lets start building KPIs ##
##############################################################

KPI_Build <- function(dataset){
  
  #1. KPI - sell price for all the products
  dataset$sell_price <- dataset$gmv/dataset$Units
  
  #2. KPI - Promotional Offer for all the products
  dataset$promotional_discount <- (dataset$MRP-dataset$sell_price)/dataset$MRP
  
  #
  
  #3. Clustering
  ## Creating a new KPI (though not used in the final model)
  ## It divides the products into three categories based on MRP and num units sold - 
  ## mass market, medium market and premium product
  
  dataset$P_analytic_vertical <- factor(dataset$P_analytic_vertical)
  
  cluster<- aggregate(cbind(Units,sell_price,MRP)~P_analytic_vertical,dataset,mean)
  
  
  if(nrow(cluster)<=3){
    
    cluster$p_tag <-NA
    
    
    # Assuming premium product:- 
    
    cluster$P_tag[which(cluster$MRP>=mean(cluster$MRP))] <- "Middle_p"
    cluster$P_tag[-which(cluster$MRP>=mean(cluster$MRP))] <- "Mass_p"
    
    cluster <- cluster[,-c(2:4)]
    
    dataset <-merge(dataset,cluster,by="P_analytic_vertical")
    
  } else {
    
    #str(cluster) 
    
    cluster$sell_price_1 <- scale(cluster$sell_price)
    cluster$MRP_1<- scale(cluster$MRP)
    cluster$Units_1 <- scale(cluster$Units)
    
    #str(cluster)
    
    k1 <- cluster[,-c(2:4)]
    
    
    clust <- kmeans(k1[,-1], centers = 3,iter.max = 50,nstart = 50)
    
    cluster$P_tag <- as.factor(clust$cluster)
    
    cluster <- cluster[,c(1,8)]
    
    # Add extra column in dataset with 
    
    dataset <-merge(dataset,cluster,by=c("P_analytic_vertical"),all.x=TRUE)
    
    
    library("plyr")
    library("dplyr")
    
    k2 <- table(dataset$P_tag)
    
    levels(dataset$P_tag)[which(k2==max(table(dataset$P_tag)))] <- "Mass_p"
    levels(dataset$P_tag)[which(k2==min(table(dataset$P_tag)))] <- "Premium_p"
    levels(dataset$P_tag)[which(k2!=max(table(dataset$P_tag))& k2!=min(table(dataset$P_tag)))] <- "Middle_p"
    
    
  }
  
  #3. Payment model indicator
  dataset$order_pay_ind<-ifelse(dataset$payment_mode=="Prepaid",1,0)
  
  #4.Add (Online_order/Total Order) KPI
  
  # Total Order placed
  # total_order <-aggregate(order_pay_ind ~ Order_week,data=dataset,FUN=NROW)
  total_order <- group_by(dataset,Order_week) %>% dplyr::summarise(number_of_order=n())
  
  # Total prepaid order
  #Online_order<-aggregate(order_pay_ind ~ Order_week,data=dataset,FUN=sum)
  prepaid_order <- group_by(dataset,Order_week) %>% dplyr::summarise(number_of_prepaid_order=sum(order_pay_ind))
  # Merge the both the file 
  merged <-merge(total_order,prepaid_order,by=c("Order_week"),all.x=TRUE)
  
  # Create new column of percentage online
  merged$percentage_of_prepaid_order <- merged$number_of_prepaid_order*100/merged$number_of_order
  
  # Remove other variables from 
  # Merge with dataset file. 
  
  merged <- merged[,-c(2,3)]
  
  # add "percentage_of_prepaid_order" column in dataset 
  
  dataset<- merge(dataset,merged,by=c("Order_week"),all.x=TRUE)
  
  dataset$P_sub_category <- NULL
  
  dataset$P_super_category <- NULL
  
  dataset$P_analytic_category <- NULL
  
  
  #5.NPS score
  dataset<-merge(dataset,nps,by=c("Month","Year"),all.x=TRUE)
  
  
  
  #6. freebies
  
  dataset$freebies <- ifelse(dataset$gmv==0,1,0)
  
  freebies_count <- group_by(dataset,Order_week) %>% dplyr::summarise(freebies_count=sum(freebies))
  
  dataset <- merge(dataset,freebies_count,by=c("Order_week"),all.x=TRUE)
  
  
  #7.Holiday_column
  
  holiday_list<-c("2015-07-18","2015-07-19","2015-08-15",
                  "2015-08-16","2015-08-17","2015-08-28",
                  "2015-08-29","2015-08-30","2015-10-15",
                  "2015-10-16","2015-10-17","2015-11-07","2015-11-08","2015-11-09","2015-11-10",
                  "2015-10-11","2015-10-12","2015-11-13","2015-11-14","2015-12-25","2015-12-26",
                  "2015-12-27","2015-12-28","2015-12-29","2015-12-30","2016-01-01","2016-01-02",
                  "2016-01-03","2016-01-20","2016-01-21","2016-01-22","2016-02-01","2016-02-02",
                  "2016-02-20","2016-02-21","2016-02-14","2016-02-15","2016-03-07","2016-03-08",
                  "2016-03-09","2016-05-25","2016-05-26","2016-05-27")
  
  
  holiday_list <- as.Date(holiday_list)
  
  Order_week <- (as.numeric(difftime(holiday_list,as.Date('2015-06-30'), unit="day")) %/% 7)+1
  
  year <- year(holiday_list)
  
  holiday <- data.frame(Order_week)
  
  #holiday$Order_week<- ifelse(holiday$Order_week<=26 & holiday$year==2016,holiday$Order_week+53,holiday$Order_week)
  
  #holiday$year <-NULL
  #holiday$holiday_freq <- 1
  
  holiday <- group_by(holiday,Order_week) %>% dplyr::summarise(holiday_freq=n())
  
  
  products <- as.data.frame.matrix(t(table(dataset$P_tag,dataset$Order_week)))
  
  
  products$Order_week <- row.names(products)
  
  dataset_1 <- aggregate(gmv~Order_week,dataset,sum)
  
  dataset<- aggregate(cbind(sell_price,MRP,Units,SLA,promotional_discount,Procurement_SLA,percentage_of_prepaid_order,freebies_count,NPS)~ Order_week,data=dataset,FUN = mean)
  
  
  dataset <- merge(dataset,products,by="Order_week",all.x=TRUE)
  
  dataset <- merge(dataset,holiday,by="Order_week",all.x=TRUE)
  dataset$holiday_freq[is.na(dataset$holiday_freq)] <-0
  
  dataset <- merge(dataset,dataset_1,by="Order_week",all.x=TRUE)
  
  ##################################################################################################
  
  return(dataset) 
  
} 
# Datasets

GamingAccessory<- KPI_Build(GamingAccessory)

HomeAudio <-KPI_Build(HomeAudio)

CameraAccessory <- KPI_Build(CameraAccessory)


###############################################################################################
# Create Adstock 
###############################################################################################

library(readxl)

Media_Investment <- read_excel("Media data and other information.xlsx", sheet="Media Investment")

names(Media_Investment) <- Media_Investment[1,]
Media_Investment <- Media_Investment[-1,]
Media_Investment <- as.data.frame(lapply(Media_Investment,function(x) { as.numeric(x)}))

Media_Investment$Month <- month.abb[Media_Investment$Month]


date <- as.Date(as.Date('2015-07-01'):as.Date('2016-06-30'),origin="1970-01-01")

media_investment_date <- as.data.frame(date)

require(Hmisc)

media_investment_date$month_days <- monthDays(media_investment_date$date)


media_investment_date$Month <- as.POSIXlt(media_investment_date$date)$mon +1


media_investment_date$Month <- month.abb[media_investment_date$Month]

#media_investment_date$week_number <- format(as.POSIXct(media_investment_date$date), "%U")


media_investment_date$week_number <-  (as.numeric(difftime(media_investment_date$date,as.Date('2015-06-30'), unit="day")) %/% 7)+1


media_investment_date_merged <- merge(media_investment_date,Media_Investment ,by="Month")



media_investment_date_merged[,6:15] <- sapply(media_investment_date_merged[,6:15], function(x) { x/media_investment_date_merged$month_days})

library(dplyr)


media_investment_weekly <-group_by(media_investment_date_merged,week_number) %>% dplyr::summarise(Total.Investment=sum(Total.Investment), TV=sum(TV), Digital=sum(Digital), Sponsorship=sum(Sponsorship), Content.Marketing=sum(Content.Marketing), Online.marketing=sum(Online.marketing), Affiliates=sum(Affiliates), SEM=sum(SEM), Radio=sum(Radio), Other=sum(Other))

media_investment_weekly$Radio[is.na(media_investment_weekly$Radio)] <- 0

media_investment_weekly$Other[is.na(media_investment_weekly$Other)] <- 0

media_investment_weekly_carryover <- media_investment_weekly

# for creating ad-stock , we are considering ad effect of 0.6,0.4,0.2 for subsequent week when the adv is shown/published.

media_investment_weekly_carryover[,2:11] <- sapply(media_investment_weekly[,2:11], function(x) {x + 0.6*lag(x,1,default=0) + 0.4*lag(x,2,default=0) + 0.2*lag(x,3,default=0)})

Adstock <- media_investment_weekly_carryover



#################################################################################################

## Combining ad-stock (weekly) with the other KPIs 

GamingAccessory <- merge(GamingAccessory , Adstock,by.x = "Order_week", by.y="week_number")

HomeAudio <- merge(HomeAudio,Adstock,by.x = "Order_week", by.y="week_number")

CameraAccessory <- merge(CameraAccessory,Adstock,by.x = "Order_week", by.y="week_number")



######################## EDA ###########################

# Visualization for ad-stock vs GMV : Gaming Accessory

dataset <- GamingAccessory
name <- "Gaming Accessory"

plot1_game <- ggplot(dataset,aes(TV,gmv))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(name) + labs(x = "TV AdStock", y = "GMV")
plot1_game


plot2_game <- ggplot(dataset,aes(Digital,gmv))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(name) + labs(x = "Digital AdStock", y = "GMV")
plot2_game

plot3_game <- ggplot(dataset,aes(Sponsorship,gmv))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(name) + labs(x = "Sponsorship AdStock", y = "GMV")
plot3_game

plot4_game <- ggplot(dataset,aes(Content.Marketing,gmv))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(name) + labs(x = "Content Marketing AdStock", y = "GMV")
plot4_game

plot5_game <- ggplot(dataset,aes(Online.marketing,gmv))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(name) + labs(x = "Online Marketing AdStock", y = "GMV")
plot5_game

plot6_game <- ggplot(dataset,aes(Affiliates,gmv))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(name) + labs(x = "Affiliates AdStock", y = "GMV")
plot6_game

plot7_game <- ggplot(dataset,aes(SEM,gmv))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(name) + labs(x = "SEM AdStock", y = "GMV")
plot7_game

plot8_game <- ggplot(dataset,aes(dataset$Order_week,dataset$gmv, fill = as.factor(ifelse(dataset$holiday_freq>0,1,0))))+geom_bar(stat="identity") + labs(fill = "Holiday_flag", x = "Week", y = "GMV") + ggtitle(name)
plot8_game



# Visualization for ad-stock vs GMV : Camera Accessory

dataset <- CameraAccessory
name <- "Camera Accessory"

plot1_camera <- ggplot(dataset,aes(TV,gmv))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(name) + labs(x = "TV AdStock", y = "GMV")
plot1_camera


plot2_camera <- ggplot(dataset,aes(Digital,gmv))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(name) + labs(x = "Digital AdStock", y = "GMV")
plot2_camera

plot3_camera <- ggplot(dataset,aes(Sponsorship,gmv))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(name) + labs(x = "Sponsorship AdStock", y = "GMV")
plot3_camera

plot4_camera <- ggplot(dataset,aes(Content.Marketing,gmv))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(name) + labs(x = "Content Marketing AdStock", y = "GMV")
plot4_camera

plot5_camera <- ggplot(dataset,aes(Online.marketing,gmv))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(name) + labs(x = "Online Marketing AdStock", y = "GMV")
plot5_camera

plot6_camera <- ggplot(dataset,aes(Affiliates,gmv))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(name) + labs(x = "Affiliates AdStock", y = "GMV")
plot6_camera

plot7_camera <- ggplot(dataset,aes(SEM,gmv))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(name) + labs(x = "SEM AdStock", y = "GMV")
plot7_camera

plot8_camera <- ggplot(dataset,aes(dataset$Order_week,dataset$gmv, fill = as.factor(ifelse(dataset$holiday_freq>0,1,0))))+geom_bar(stat="identity") + labs(fill = "Holiday_flag", x = "Week", y = "GMV") + ggtitle(name)
plot8_camera


# Visualization for ad-stock vs GMV : Home Audio

dataset <- HomeAudio
name <- "Home Audio"

plot1_HomeAudio <- ggplot(dataset,aes(TV,gmv))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(name) + labs(x = "TV AdStock", y = "GMV")
plot1_HomeAudio


plot2_HomeAudio <- ggplot(dataset,aes(Digital,gmv))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(name) + labs(x = "Digital AdStock", y = "GMV")
plot2_HomeAudio

plot3_HomeAudio <- ggplot(dataset,aes(Sponsorship,gmv))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(name) + labs(x = "Sponsorship AdStock", y = "GMV")
plot3_HomeAudio

plot4_HomeAudio <- ggplot(dataset,aes(Content.Marketing,gmv))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(name) + labs(x = "Content Marketing AdStock", y = "GMV")
plot4_HomeAudio

plot5_HomeAudio <- ggplot(dataset,aes(Online.marketing,gmv))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(name) + labs(x = "Online Marketing AdStock", y = "GMV")
plot5_HomeAudio

plot6_HomeAudio <- ggplot(dataset,aes(Affiliates,gmv))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(name) + labs(x = "Affiliates AdStock", y = "GMV")
plot6_HomeAudio

plot7_HomeAudio <- ggplot(dataset,aes(SEM,gmv))+geom_point()+ geom_smooth(aes(method="lm"))+ggtitle(name) + labs(x = "SEM AdStock", y = "GMV")
plot7_HomeAudio

plot8_HomeAudio <- ggplot(dataset,aes(dataset$Order_week,dataset$gmv, fill = as.factor(ifelse(dataset$holiday_freq>0,1,0))))+geom_bar(stat="identity") + labs(fill = "Holiday_flag", x = "Week", y = "GMV") + ggtitle(name)
plot8_HomeAudio


## ------------------------Creating moving average variables------------------------------------

moving_average_kpi <- function(dataset)
{
  library(dplyr)
  library(zoo)
  
  myfun1 = function(x) rollmean(x, k = 2, fill = NA, align = "right")
  myfun2 = function(x) rollmean(x, k = 3, fill = NA, align = "right")
  myfun3 = function(x) rollmean(x, k = 4, fill = NA, align = "right")
  
  #dataset1<-arrange(dataset1,P_analytic_vertical,Year,Order_week)
  
  x=dataset[,c("Order_week","sell_price","promotional_discount")]
  
  
  x1<-x %>% mutate_each(funs(myfun1),sell_price,promotional_discount) %>% data.frame()
  
  x2<-x %>% mutate_each(funs(myfun2),sell_price,promotional_discount) %>% data.frame()
  
  x3<-x %>% mutate_each(funs(myfun3),sell_price,promotional_discount) %>% data.frame()
  
  
  x1$LP_MA1<-(x1$sell_price)
  x1$PO_MA1<-(x1$promotional_discount)
  
  x2$LP_MA2<-(x2$sell_price)
  x2$PO_MA2<-(x2$promotional_discount)
  
  x3$LP_MA3<-(x3$sell_price)
  x3$PO_MA3<-(x3$promotional_discount)
  
  x4=cbind(x1[,-c(2:3)],x2[,-c(1:3)],x3[,-c(1:3)])
  
  
  
  dataset<-merge(dataset,x4,by="Order_week")
  
  
  
  dataset$inc_LP_MA1<-(dataset$sell_price - dataset$LP_MA1)/dataset$LP_MA1
  dataset$inc_LP_MA2<-(dataset$sell_price - dataset$LP_MA2)/dataset$LP_MA2
  dataset$inc_LP_MA3<-(dataset$sell_price - dataset$LP_MA3)/dataset$LP_MA3
  
  dataset$inc_PO_MA1<-(dataset$promotional_discount - dataset$PO_MA1)/dataset$PO_MA1
  dataset$inc_PO_MA2<-(dataset$promotional_discount - dataset$PO_MA2)/dataset$PO_MA2
  dataset$inc_PO_MA3<-(dataset$promotional_discount - dataset$PO_MA3)/dataset$PO_MA3
  
  #Deleting some columns
  
  dataset$LP_MA1<-NULL
  dataset$LP_MA2<-NULL
  dataset$LP_MA3<-NULL
  
  dataset$PO_MA1<-NULL
  dataset$PO_MA2<-NULL
  dataset$PO_MA3<-NULL
  
  names(dataset)[22:27]<-c("inc_LP_MA1","inc_LP_MA2","inc_LP_MA3","inc_PO_MA1","inc_PO_MA2",
                           "inc_PO_MA3")
  
  
  
  #------1) Lag of List price by 1 week,2 week, 3 week
  #------2) Lag of discount(promo_off) by 1 week,2 week, 3 week
  #------3) Incremental Lag of List price & promotions/discounts by 1 week,2 week, 3 week
  
  #-----------------Lag the data after aggregating by week----#
  
  #8.	Lag List price (different period lags)
  
  library(DataCombine)
  
  #List of list price by 1,2,3 dates (Date values are ordered)
  #Previous List price
  data_dum <- slide(dataset, Var = "sell_price",slideBy = -1)
  
  data_dum <- slide(data_dum, Var = "sell_price",slideBy = -2)
  
  data_dum <- slide(data_dum, Var = "sell_price", slideBy = -3)
  
  #9.lag the promotion variables
  
  data_dum <- slide(data_dum, Var = "promotional_discount", slideBy = -1)
  
  data_dum <- slide(data_dum, Var = "promotional_discount", slideBy = -2)
  
  data_dum <- slide(data_dum, Var = "promotional_discount", slideBy = -3)
  
  data_dum <- slide(data_dum, Var = "NPS", slideBy = -1)
  
  data_dum <- slide(data_dum, Var = "NPS", slideBy = -2)
  
  data_dum <- slide(data_dum, Var = "NPS", slideBy = -3)
  
  
  data_dum <- slide(data_dum, Var = "holiday_freq", slideBy = -1)
  
  data_dum <- slide(data_dum, Var = "holiday_freq", slideBy = -2)
  
  data_dum <- slide(data_dum, Var = "holiday_freq", slideBy = -3)
  
  
  dataset <- na.omit(data_dum) 
  
  
  return(dataset)
}

Game_final <- moving_average_kpi(GamingAccessory)
Home_final <- moving_average_kpi(HomeAudio)
Camera_final <- moving_average_kpi(CameraAccessory)


write.csv(Game_final,"Game_final.csv", row.names = FALSE)
write.csv(Home_final,"Home_final.csv", row.names = FALSE)
write.csv(Camera_final,"Camera_final.csv", row.names = FALSE)