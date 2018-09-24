#####################################################################################################
############################# Model on Home Audio Dataset#######################################
#####################################################################################################

# importing required packages

library(plyr)
library(dplyr)
library(lubridate)
library(MASS)
library(car)
library(DataCombine)
library(zoo)
library(glmnet)
library(DAAG)
library(ggplot2)

Cross_home<- rep(0,5) 

# Build the Basic Linear regression model
Home_final <- read.csv("Home_final.csv", stringsAsFactors = F)

Home_lm_ds <- Home_final

colnames(Home_lm_ds)

column_list <- c("SLA", "promotional_discount", "Procurement_SLA", "percentage_of_prepaid_order", "freebies_count", "NPS", "Premium_p", "Mass_p", "Middle_p", "holiday_freq", "gmv", "TV", "Digital", "Sponsorship", "Content.Marketing", "Online.marketing", "inc_LP_MA1", "inc_LP_MA2", "inc_LP_MA3", "inc_PO_MA1", "inc_PO_MA2", "inc_PO_MA3")

Home_lm_ds <- Home_lm_ds[,column_list]

Home_lm_ds <- scale(Home_lm_ds)

Home_lm_ds <-data.frame(Home_lm_ds)

model_1 <- lm(gmv~.,Home_lm_ds)


################################################################################################

# Summary of Linear Model 
summary(model_1)


library(car)      # vif
library(MASS)
library(DAAG)
library(magrittr) # for function "%>%"
library(carData)  
library(dplyr)  # for function "arrange"
library(ggplot2)


model_2 <- stepAIC(model_1,direction = "both") 

summary(model_2) 

data.frame(var=names(vif(model_2)),vif=vif(model_2)) %>% arrange(desc(vif))

# As we see all the attributes have significant p-value, 
#let's see the corelation of attribute having hightes VIF

train_1 <- Home_lm_ds[,c("inc_LP_MA1", "Content.Marketing", "inc_LP_MA2", "Sponsorship", "TV", "Mass_p", "SLA", "inc_PO_MA3", "Procurement_SLA")]

cor(Home_lm_ds$Online.marketing,train_1)

# removing Online.marketing
lm(formula = gmv ~ SLA + Procurement_SLA + Mass_p + TV + Sponsorship + 
     Content.Marketing + inc_LP_MA1 + inc_LP_MA2 + 
     inc_PO_MA3, data = Home_lm_ds) %>% summary()
#Adjusted R-squared:  0.996

# removing inc_LP_MA1
lm(formula = gmv ~ SLA + Procurement_SLA + Mass_p + TV + Sponsorship + 
     Content.Marketing + Online.marketing + inc_LP_MA2 + 
     inc_PO_MA3, data = Home_lm_ds) %>% summary()
#Adjusted R-squared:  0.9959

#So removing Online.marketing and creating a new model
model_3 <- lm(formula = gmv ~ SLA + Procurement_SLA + Mass_p + TV + Sponsorship + 
                Content.Marketing + inc_LP_MA1 + inc_LP_MA2 + 
                inc_PO_MA3, data = Home_lm_ds)
summary(model_3) 

data.frame(var=names(vif(model_3)),vif=vif(model_3)) %>% arrange(desc(vif))

# Removing inc_LP_MA1 having vif 3.1145 and p-value 0.153457
model_4 <- lm(formula = gmv ~ SLA + Procurement_SLA + Mass_p + TV + Sponsorship + 
                Content.Marketing + inc_LP_MA2 + inc_PO_MA3, data = Home_lm_ds)
summary(model_4) 

data.frame(var=names(vif(model_4)),vif=vif(model_4)) %>% arrange(desc(vif))

train_4 <- Home_lm_ds[,c("inc_LP_MA2", "Content.Marketing", "Sponsorship", "Mass_p", "SLA", "inc_PO_MA3", "Procurement_SLA")]

cor(Home_lm_ds$TV, train_4)

# Removing TV
lm(formula = gmv ~ SLA + Procurement_SLA + Mass_p + Sponsorship + 
     Content.Marketing + inc_LP_MA2 + inc_PO_MA3, data = Home_lm_ds) %>% summary()
# Adjusted R-squared:  0.996

# Removing Sponsorship
lm(formula = gmv ~ SLA + Procurement_SLA + Mass_p + TV + 
     Content.Marketing + inc_LP_MA2 + inc_PO_MA3, data = Home_lm_ds) %>% summary()
# Adjusted R-squared:  0.992

#So removing TV and creating a new model
model_5 <- lm(formula = gmv ~ SLA + Procurement_SLA + Mass_p + Sponsorship + 
     Content.Marketing + inc_LP_MA2 + inc_PO_MA3, data = Home_lm_ds)

summary(model_5) 

data.frame(var=names(vif(model_5)),vif=vif(model_5)) %>% arrange(desc(vif))

train_5 <- Home_lm_ds[,c("Content.Marketing", "Sponsorship", "Mass_p", "SLA", "inc_PO_MA3", "Procurement_SLA")]

cor(Home_lm_ds$inc_LP_MA2, train_5)

#Removing inc_LP_MA2
lm(formula = gmv ~ SLA + Procurement_SLA + Mass_p + Sponsorship + 
     Content.Marketing + inc_PO_MA3, data = Home_lm_ds) %>% summary()
#Adjusted R-squared:  0.9928

#Removing Content.Marketing
lm(formula = gmv ~ SLA + Procurement_SLA + Mass_p + Sponsorship + 
     inc_LP_MA2 + inc_PO_MA3, data = Home_lm_ds) %>% summary()
#Adjusted R-squared:  0.9948

#So removing Content.Marketing and creating a new model
model_6 <- lm(formula = gmv ~ SLA + Procurement_SLA + Mass_p + Sponsorship + 
                inc_LP_MA2 + inc_PO_MA3, data = Home_lm_ds)

summary(model_6)

data.frame(var=names(vif(model_6)),vif=vif(model_6)) %>% arrange(desc(vif))

# Now all attributes have vif < 5 and SLA has p-value 0.01315
# Removing SLA

model_7 <- lm(formula = gmv ~ Procurement_SLA + Mass_p + Sponsorship + 
     inc_LP_MA2 + inc_PO_MA3, data = Home_lm_ds)

summary(model_7)

data.frame(var=names(vif(model_7)), vif=vif(model_7)) %>% arrange(desc(vif))

# We see all the variables are significant, so it's the final model

##Final Model 
Linear_Final_model_home <- model_7

# Adjusted R-squared:  0.994  with 5 variables

temp_crossval <- cv.lm(data = Home_lm_ds, form.lm = formula(gmv ~ Procurement_SLA + Mass_p + Sponsorship + 
                                                              inc_LP_MA2 + inc_PO_MA3),m = 10)

#Cross_home[1] <- attr(temp_crossval, "ms") #Error

################################################################################

# Elasticity Analysis

train <- Home_lm_ds


grlm <- Linear_Final_model_home


# estimating the elasticity coefficients

elasticity <- function(var){
  
  elax1 <-as.numeric(Linear_Final_model_home$coefficients[var]*mean(train[,var])/mean(train$gmv))
  
  return(elax1)
} 

var_list <- list()

for(i in 2:length(Linear_Final_model_home$coefficients)){
  
  var_list[i-1] <-elasticity(names(Linear_Final_model_home$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(Linear_Final_model_home$coefficients[2:length(Linear_Final_model_home$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")


ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Home Audio - Linear Model") +xlab("Variables")

#####################################################################################################################
#####################################################################################################################
########################################## Multiplicative Model: Home Audio #####################################################

Home_final <- read.csv("Home_final.csv")

Home_multiplicative_ds <- Home_final

column_list <- c("SLA", "promotional_discount", "Procurement_SLA", "percentage_of_prepaid_order", "freebies_count", "NPS", "Premium_p", "Mass_p", "Middle_p", "holiday_freq", "gmv", "TV", "Digital", "Sponsorship", "Content.Marketing", "Online.marketing", "inc_LP_MA1", "inc_LP_MA2", "inc_LP_MA3", "inc_PO_MA1", "inc_PO_MA2", "inc_PO_MA3")

Home_multiplicative_ds <- Home_multiplicative_ds[,column_list]

# Checking number of zeros or less in each variable

sapply(Home_multiplicative_ds, function(x) { sum(x <=0)})

# Result:
#SLA                       promotional_discount  Procurement_SLA             percentage_of_prepaid_order 
#0                         0                     0                           0 
#freebies_count            NPS                   Premium_p                   Mass_p 
#46                        0                     45                          0 
#Middle_p                  holiday_freq                         gmv          TV 
#3                         32                    0                           0 
#Digital                   Sponsorship           Content.Marketing           Online.marketing 
#0                         0                     0                           0 
#inc_LP_MA1                inc_LP_MA2            inc_LP_MA3                  inc_PO_MA1 
#0                         0                     27                          27 
#inc_PO_MA2                inc_PO_MA3 
#23                        21 

# lets remove the variables having more than 10 zeros and for the variables having less than 10, will substitute zeros with 
# a small value(0.01)

# Treatment

Home_multiplicative_ds$Middle_p[which(Home_multiplicative_ds$Middle_p==0)] <- 0.01

Home_multiplicative_ds <- Home_multiplicative_ds[,!(names(Home_multiplicative_ds) %in% c("freebies_count","Premium_p","holiday_freq","inc_LP_MA3","inc_PO_MA1","inc_PO_MA2","inc_PO_MA3"))]

Home_multiplicative_ds_log <- log(Home_multiplicative_ds)

################################################
#################################################

## Build the First model ##

multi_model <- lm(gmv~.,Home_multiplicative_ds_log)

summary(multi_model)

library(dplyr)
library(car)
library(MASS)

multi_model_2 <- stepAIC(multi_model,direction = "both")

#Adjusted R-squared:  0.998

summary(multi_model_2)

data.frame(var=names(vif(multi_model_2)),vif=vif(multi_model_2)) %>% arrange(desc(vif))

#TV (VIF=25.85, p-value=0.16243 )
# Removing TV as it's insignificant

multi_model_3 <- lm(formula = gmv ~ SLA + promotional_discount + Mass_p + Middle_p + 
                      Digital + Sponsorship + Content.Marketing + Online.marketing + 
                      inc_LP_MA1, data = Home_multiplicative_ds_log)

summary(multi_model_3) 

data.frame(var=names(vif(multi_model_3)),vif=vif(multi_model_3)) %>% arrange(desc(vif))

multi_train_3 <- Home_multiplicative_ds[,c("inc_LP_MA1", "Content.Marketing", "Mass_p", "Sponsorship", "Digital", "promotional_discount", "SLA", "Middle_p")]

cor(Home_multiplicative_ds$Online.marketing,multi_train_3)

# Removing Online.marketing
lm(formula = gmv ~ SLA + promotional_discount + Mass_p + Middle_p + 
     Digital + Sponsorship + Content.Marketing +  
     inc_LP_MA1, data = Home_multiplicative_ds_log) %>% summary()
#Adjusted R-squared:  0.996

# Removing inc_LP_MA1
lm(formula = gmv ~ SLA + promotional_discount + Mass_p + Middle_p + 
     Digital + Sponsorship + Content.Marketing + Online.marketing, 
     data = Home_multiplicative_ds_log) %>% summary()
#Adjusted R-squared:  0.996

# So, creating new model by removing inc_LP_MA1
multi_model_4 <- lm(formula = gmv ~ SLA + promotional_discount + Mass_p + Middle_p + 
                      Digital + Sponsorship + Content.Marketing + Online.marketing, 
                    data = Home_multiplicative_ds_log)

summary(multi_model_4) 

data.frame(var=names(vif(multi_model_4)),vif=vif(multi_model_4)) %>% arrange(desc(vif))

#Content.Marketing vif = 19.24, p-value = 0.0841
# So, creating new model by removing Content.Marketing

multi_model_5 <- lm(formula = gmv ~ SLA + promotional_discount + Mass_p + Middle_p + 
                      Digital + Sponsorship + Online.marketing, 
                    data = Home_multiplicative_ds_log)

summary(multi_model_5) 

data.frame(var=names(vif(multi_model_5)),vif=vif(multi_model_5)) %>% arrange(desc(vif))

# promotional_discount vif = 3.72, p-value = 0.388
# So, creating new model by removing promotional_discount

multi_model_6 <- lm(formula = gmv ~ SLA + Mass_p + Middle_p + 
                      Digital + Sponsorship + Online.marketing, 
                    data = Home_multiplicative_ds_log)

summary(multi_model_6) 

data.frame(var=names(vif(multi_model_6)),vif=vif(multi_model_6)) %>% arrange(desc(vif))

# Online.marketing vif = 3.08, p-value = 0.76652
# So, creating new model by removing Online.marketing

multi_model_7 <- lm(formula = gmv ~ SLA + Mass_p + Middle_p + 
                      Digital + Sponsorship, data = Home_multiplicative_ds_log)

summary(multi_model_7) 

data.frame(var=names(vif(multi_model_7)),vif=vif(multi_model_7)) %>% arrange(desc(vif))

# Sponsorship vif = 2.33, p-value = 0.54509
# So, creating new model by removing it

multi_model_8 <- lm(formula = gmv ~ SLA + Mass_p + Middle_p + 
                      Digital, data = Home_multiplicative_ds_log)

summary(multi_model_8) 

data.frame(var=names(vif(multi_model_8)),vif=vif(multi_model_8)) %>% arrange(desc(vif))

# Middle_p vif = 1.63, p-value = 0.16012
# So, creating new model by removing it

multi_model_9 <- lm(formula = gmv ~ SLA + Mass_p +  
                      Digital, data = Home_multiplicative_ds_log)

summary(multi_model_9) 

data.frame(var=names(vif(multi_model_9)),vif=vif(multi_model_9)) %>% arrange(desc(vif))

#Call:
#  lm(formula = gmv ~ SLA + Mass_p + Digital, data = Home_multiplicative_ds_log)
#
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-0.11272 -0.03125  0.00143  0.02702  0.11499 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  8.17257    0.11798   69.27  < 2e-16 ***
#  SLA         -0.26520    0.06592   -4.02  0.00023 ***
#  Mass_p       0.99621    0.00955  104.29  < 2e-16 ***
#  Digital      0.02310    0.01131    2.04  0.04733 *  
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 0.0514 on 43 degrees of freedom
#Multiple R-squared:  0.997,	Adjusted R-squared:  0.996 
#F-statistic: 4.09e+03 on 3 and 43 DF,  p-value: <2e-16

### 
# Multiplicative : Final Model Model-7

multi_final_model_home <- multi_model_9

#Adjusted R-squared:  0.9966  with 3 significant variables 

temp_crossval <- cv.lm(data = Home_multiplicative_ds, form.lm = formula(gmv ~ SLA + Mass_p + Digital),m = 10)
#Cross_home[2] <- attr(temp_crossval, "ms") # ms=1.38e+12 #Error

##########################################################################################
# Elasticity Analysis

train <- Home_multiplicative_ds


grlm <- multi_final_model_home


# estimating the elasticity coefficients

elasticity <- function(var){
  
  elax1 <-as.numeric(multi_final_model_home$coefficients[var]*mean(train[,var])/mean(train$gmv))
  
  return(elax1)
} 

var_list <- list()

for(i in 2:length(multi_final_model_home$coefficients)){
  
  var_list[i-1] <-elasticity(names(multi_final_model_home$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(multi_final_model_home$coefficients[2:length(multi_final_model_home$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")


ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Home Audio - Multiplicative Model") +xlab("Variables")


#######################################################################################################################
#######################################################################################################################
############################################## Koyck models #################################################

Home_koyck<- Home_final[,c("SLA", "promotional_discount", "Procurement_SLA", "percentage_of_prepaid_order", "freebies_count", "NPS", "Premium_p", "Mass_p", "Middle_p", "holiday_freq", "gmv", "Total.Investment", "TV", "Digital", "Sponsorship", "Content.Marketing", "Online.marketing", "inc_LP_MA1", "inc_LP_MA2", "inc_LP_MA3", "inc_PO_MA1", "inc_PO_MA2", "inc_PO_MA3", "inc_LP_MA3.1", "inc_PO_MA1.1", "inc_PO_MA2.1", "inc_PO_MA3.1", "sell_price.1", "sell_price.2", "sell_price.3", "promotional_discount.1", "promotional_discount.2", "promotional_discount.3", "NPS.1", "NPS.2", "NPS.3", "holiday_freq.1", "holiday_freq.2", "holiday_freq.3")]

library(DataCombine)

# gmv 1 lag
Home_koyck <- slide(Home_koyck, Var = "gmv",slideBy = 1)


Home_koyck <- na.omit(Home_koyck)

Home_koyck <- scale(Home_koyck)

Home_koyck <- data.frame(Home_koyck)

# Build first model
koyck_model <- lm(gmv1~.,Home_koyck)

summary(koyck_model)

koyck_model_2 <- stepAIC(koyck_model,direction = "both")

summary(koyck_model_2)  

data.frame(var=names(vif(koyck_model_2)),vif=vif(koyck_model_2)) %>% arrange(desc(vif))

#Removing inc_PO_MA3.1 (vif = 3691.70, p-value = 0.13480)
koyck_model_3 <- lm(formula = gmv1 ~ SLA + Procurement_SLA + percentage_of_prepaid_order + 
     freebies_count + Premium_p + Mass_p + holiday_freq + gmv + 
     Total.Investment + TV + Digital + Sponsorship + Content.Marketing + 
     Online.marketing + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
     inc_PO_MA2 + inc_PO_MA3 + inc_PO_MA1.1 + inc_PO_MA2.1 +  
     sell_price.1 + sell_price.2 + promotional_discount.1 + promotional_discount.2 + 
     promotional_discount.3 + NPS.1 + NPS.3 + holiday_freq.1 + 
     holiday_freq.2 + holiday_freq.3, data = Home_koyck)

summary(koyck_model_3)  

data.frame(var=names(vif(koyck_model_3)),vif=vif(koyck_model_3)) %>% arrange(desc(vif))

#Removing Total.Investment vif = 3048.40, p-value = 0.06224
koyck_model_4 <- lm(formula = gmv1 ~ SLA + Procurement_SLA + percentage_of_prepaid_order + 
                      freebies_count + Premium_p + Mass_p + holiday_freq + gmv + 
                      TV + Digital + Sponsorship + Content.Marketing + 
                      Online.marketing + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
                      inc_PO_MA2 + inc_PO_MA3 + inc_PO_MA1.1 + inc_PO_MA2.1 +  
                      sell_price.1 + sell_price.2 + promotional_discount.1 + promotional_discount.2 + 
                      promotional_discount.3 + NPS.1 + NPS.3 + holiday_freq.1 + 
                      holiday_freq.2 + holiday_freq.3, data = Home_koyck)

summary(koyck_model_4)  

data.frame(var=names(vif(koyck_model_4)),vif=vif(koyck_model_4)) %>% arrange(desc(vif))

#Removing inc_PO_MA1.1 with vif = 1317.50, p-value = 0.05135
koyck_model_5 <- lm(formula = gmv1 ~ SLA + Procurement_SLA + percentage_of_prepaid_order + 
                      freebies_count + Premium_p + Mass_p + holiday_freq + gmv + 
                      TV + Digital + Sponsorship + Content.Marketing + 
                      Online.marketing + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
                      inc_PO_MA2 + inc_PO_MA3 + inc_PO_MA2.1 +  
                      sell_price.1 + sell_price.2 + promotional_discount.1 + promotional_discount.2 + 
                      promotional_discount.3 + NPS.1 + NPS.3 + holiday_freq.1 + 
                      holiday_freq.2 + holiday_freq.3, data = Home_koyck)

summary(koyck_model_5)  

data.frame(var=names(vif(koyck_model_5)),vif=vif(koyck_model_5)) %>% arrange(desc(vif))

# Removing Mass_p with vif = 750.52, p-value = 0.07850

koyck_model_6 <- lm(formula = gmv1 ~ SLA + Procurement_SLA + percentage_of_prepaid_order + 
                      freebies_count + Premium_p + holiday_freq + gmv + 
                      TV + Digital + Sponsorship + Content.Marketing + 
                      Online.marketing + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
                      inc_PO_MA2 + inc_PO_MA3 + inc_PO_MA2.1 +  
                      sell_price.1 + sell_price.2 + promotional_discount.1 + promotional_discount.2 + 
                      promotional_discount.3 + NPS.1 + NPS.3 + holiday_freq.1 + 
                      holiday_freq.2 + holiday_freq.3, data = Home_koyck)

summary(koyck_model_6)  

data.frame(var=names(vif(koyck_model_6)),vif=vif(koyck_model_6)) %>% arrange(desc(vif))

# Removing Content.Marketing with vif = 495.51, p-value = 0.09528
koyck_model_7 <- lm(formula = gmv1 ~ SLA + Procurement_SLA + percentage_of_prepaid_order + 
                      freebies_count + Premium_p + holiday_freq + gmv + 
                      TV + Digital + Sponsorship + Online.marketing + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
                      inc_PO_MA2 + inc_PO_MA3 + inc_PO_MA2.1 +  
                      sell_price.1 + sell_price.2 + promotional_discount.1 + promotional_discount.2 + 
                      promotional_discount.3 + NPS.1 + NPS.3 + holiday_freq.1 + 
                      holiday_freq.2 + holiday_freq.3, data = Home_koyck)

summary(koyck_model_7)  

data.frame(var=names(vif(koyck_model_7)),vif=vif(koyck_model_7)) %>% arrange(desc(vif))

#Removing Sponsorship with vif = 19.26, p-value = 0.27669
koyck_model_8 <- lm(formula = gmv1 ~ SLA + Procurement_SLA + percentage_of_prepaid_order + 
                      freebies_count + Premium_p + holiday_freq + gmv + 
                      TV + Digital + Online.marketing + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
                      inc_PO_MA2 + inc_PO_MA3 + inc_PO_MA2.1 +  
                      sell_price.1 + sell_price.2 + promotional_discount.1 + promotional_discount.2 + 
                      promotional_discount.3 + NPS.1 + NPS.3 + holiday_freq.1 + 
                      holiday_freq.2 + holiday_freq.3, data = Home_koyck)

summary(koyck_model_8)  

data.frame(var=names(vif(koyck_model_8)),vif=vif(koyck_model_8)) %>% arrange(desc(vif))

# Removing promotional_discount.1 with vif = 7.95, p-value = 0.16930
koyck_model_9 <- lm(formula = gmv1 ~ SLA + Procurement_SLA + percentage_of_prepaid_order + 
                      freebies_count + Premium_p + holiday_freq + gmv + 
                      TV + Digital + Online.marketing + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
                      inc_PO_MA2 + inc_PO_MA3 + inc_PO_MA2.1 +  
                      sell_price.1 + sell_price.2 + promotional_discount.2 + 
                      promotional_discount.3 + NPS.1 + NPS.3 + holiday_freq.1 + 
                      holiday_freq.2 + holiday_freq.3, data = Home_koyck)

summary(koyck_model_9)  

data.frame(var=names(vif(koyck_model_9)),vif=vif(koyck_model_9)) %>% arrange(desc(vif))

koyck_train_9 <- Home_koyck[,c("inc_PO_MA3", "inc_PO_MA1", "inc_LP_MA3", "inc_LP_MA2", "Digital", "sell_price.1", "sell_price.2", "TV", "Online.marketing", "percentage_of_prepaid_order", "NPS.1", "promotional_discount.2", "gmv", "SLA", "NPS.3", "inc_PO_MA2.1", "Procurement_SLA", "promotional_discount.3", "holiday_freq.2", "Premium_p", "holiday_freq", "holiday_freq.1", "holiday_freq.3", "freebies_count")]

cor(Home_koyck$inc_PO_MA2,koyck_train_9)

# Removing inc_PO_MA2
lm(formula = gmv1 ~ SLA + Procurement_SLA + percentage_of_prepaid_order + 
     freebies_count + Premium_p + holiday_freq + gmv + 
     TV + Digital + Online.marketing + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
     inc_PO_MA3 + inc_PO_MA2.1 +  
     sell_price.1 + sell_price.2 + promotional_discount.2 + 
     promotional_discount.3 + NPS.1 + NPS.3 + holiday_freq.1 + 
     holiday_freq.2 + holiday_freq.3, data = Home_koyck) %>% summary()
#Adjusted R-squared:  0.484

# Removing inc_PO_MA3
lm(formula = gmv1 ~ SLA + Procurement_SLA + percentage_of_prepaid_order + 
     freebies_count + Premium_p + holiday_freq + gmv + 
     TV + Digital + Online.marketing + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
     inc_PO_MA2 + inc_PO_MA2.1 +  
     sell_price.1 + sell_price.2 + promotional_discount.2 + 
     promotional_discount.3 + NPS.1 + NPS.3 + holiday_freq.1 + 
     holiday_freq.2 + holiday_freq.3, data = Home_koyck) %>% summary()
#Adjusted R-squared:  0.487

# As R-squared changed drastically so removing promotional_discount.2 with vif = 4.18, and p-value = 0.36624

koyck_model_10 <- lm(formula = gmv1 ~ SLA + Procurement_SLA + percentage_of_prepaid_order + 
     freebies_count + Premium_p + holiday_freq + gmv + 
     TV + Digital + Online.marketing + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
     inc_PO_MA2 + inc_PO_MA3 + inc_PO_MA2.1 +  
     sell_price.1 + sell_price.2 + 
     promotional_discount.3 + NPS.1 + NPS.3 + holiday_freq.1 + 
     holiday_freq.2 + holiday_freq.3, data = Home_koyck)

summary(koyck_model_10)  

data.frame(var=names(vif(koyck_model_10)),vif=vif(koyck_model_10)) %>% arrange(desc(vif))

#Removing percentage_of_prepaid_order with vif = 10.11, p-value = 0.0516
koyck_model_11 <- lm(formula = gmv1 ~ SLA + Procurement_SLA +  
                       freebies_count + Premium_p + holiday_freq + gmv + 
                       TV + Digital + Online.marketing + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
                       inc_PO_MA2 + inc_PO_MA3 + inc_PO_MA2.1 +  
                       sell_price.1 + sell_price.2 + 
                       promotional_discount.3 + NPS.1 + NPS.3 + holiday_freq.1 + 
                       holiday_freq.2 + holiday_freq.3, data = Home_koyck)

summary(koyck_model_11)  

data.frame(var=names(vif(koyck_model_11)),vif=vif(koyck_model_11)) %>% arrange(desc(vif))

#Removing SLA with vif = 3.12, p-value = 0.17097
koyck_model_12 <- lm(formula = gmv1 ~ Procurement_SLA +  
                       freebies_count + Premium_p + holiday_freq + gmv + 
                       TV + Digital + Online.marketing + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
                       inc_PO_MA2 + inc_PO_MA3 + inc_PO_MA2.1 +  
                       sell_price.1 + sell_price.2 + 
                       promotional_discount.3 + NPS.1 + NPS.3 + holiday_freq.1 + 
                       holiday_freq.2 + holiday_freq.3, data = Home_koyck)

summary(koyck_model_12)  

data.frame(var=names(vif(koyck_model_12)),vif=vif(koyck_model_12)) %>% arrange(desc(vif))

#Removing NPS.3 with vif = 3.01, p-value = 0.34227
koyck_model_13 <- lm(formula = gmv1 ~ Procurement_SLA +  
                       freebies_count + Premium_p + holiday_freq + gmv + 
                       TV + Digital + Online.marketing + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
                       inc_PO_MA2 + inc_PO_MA3 + inc_PO_MA2.1 +  
                       sell_price.1 + sell_price.2 + 
                       promotional_discount.3 + NPS.1 + holiday_freq.1 + 
                       holiday_freq.2 + holiday_freq.3, data = Home_koyck)

summary(koyck_model_13)  

data.frame(var=names(vif(koyck_model_13)),vif=vif(koyck_model_13)) %>% arrange(desc(vif))

koyck_train_13 <- Home_koyck[,c("Procurement_SLA", "freebies_count","Premium_p","holiday_freq","gmv","TV","Digital","Online.marketing","inc_LP_MA2","inc_LP_MA3","inc_PO_MA1","inc_PO_MA3","inc_PO_MA2.1","sell_price.1","sell_price.2","promotional_discount.3","NPS.1","holiday_freq.1","holiday_freq.2","holiday_freq.3")]

cor(Home_koyck$inc_PO_MA2,koyck_train_13)

# Removing inc_PO_MA2
lm(formula = gmv1 ~ Procurement_SLA +  
     freebies_count + Premium_p + holiday_freq + gmv + 
     TV + Digital + Online.marketing + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
     inc_PO_MA3 + inc_PO_MA2.1 +  
     sell_price.1 + sell_price.2 + 
     promotional_discount.3 + NPS.1 + holiday_freq.1 + 
     holiday_freq.2 + holiday_freq.3, data = Home_koyck) %>% summary()
#Adjusted R-squared:  0.385

# Removing inc_PO_MA3
lm(formula = gmv1 ~ Procurement_SLA +  
     freebies_count + Premium_p + holiday_freq + gmv + 
     TV + Digital + Online.marketing + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
     inc_PO_MA2 + inc_PO_MA2.1 +  
     sell_price.1 + sell_price.2 + 
     promotional_discount.3 + NPS.1 + holiday_freq.1 + 
     holiday_freq.2 + holiday_freq.3, data = Home_koyck) %>% summary()
#Adjusted R-squared:  0.387

#So, Removing holiday_freq.2 with vif = 2.27, p-value = 0.1987
koyck_model_14 <- lm(formula = gmv1 ~ Procurement_SLA +  
                       freebies_count + Premium_p + holiday_freq + gmv + 
                       TV + Digital + Online.marketing + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
                       inc_PO_MA2 + inc_PO_MA3 + inc_PO_MA2.1 +  
                       sell_price.1 + sell_price.2 + 
                       promotional_discount.3 + NPS.1 + holiday_freq.1 + 
                       holiday_freq.3, data = Home_koyck)

summary(koyck_model_14)  

data.frame(var=names(vif(koyck_model_14)),vif=vif(koyck_model_14)) %>% arrange(desc(vif))

#Removing inc_PO_MA2.1 with vif = 2.36, p-value = 0.6608
koyck_model_15 <- lm(formula = gmv1 ~ Procurement_SLA +  
                       freebies_count + Premium_p + holiday_freq + gmv + 
                       TV + Digital + Online.marketing + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
                       inc_PO_MA2 + inc_PO_MA3 +   
                       sell_price.1 + sell_price.2 + 
                       promotional_discount.3 + NPS.1 + holiday_freq.1 + 
                       holiday_freq.3, data = Home_koyck)

summary(koyck_model_15)  

data.frame(var=names(vif(koyck_model_15)),vif=vif(koyck_model_15)) %>% arrange(desc(vif))

#Checking correlation
koyck_train_15 <- Home_koyck[,c("inc_PO_MA3", "sell_price.1", "sell_price.2", "inc_LP_MA2", "Digital", "inc_PO_MA1", "inc_LP_MA3", "TV", "Online.marketing", "NPS.1", "gmv", "promotional_discount.3", "holiday_freq.3", "holiday_freq.1", "Premium_p", "Procurement_SLA", "holiday_freq", "freebies_count")]

cor(Home_koyck$inc_PO_MA2,koyck_train_15)

#Removing inc_PO_MA2
lm(formula = gmv1 ~ Procurement_SLA +  
     freebies_count + Premium_p + holiday_freq + gmv + 
     TV + Digital + Online.marketing + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
     inc_PO_MA3 + sell_price.1 + sell_price.2 + 
     promotional_discount.3 + NPS.1 + holiday_freq.1 + 
     holiday_freq.3, data = Home_koyck) %>% summary()
#Adjusted R-squared:  0.392

#Removing inc_PO_MA3
lm(formula = gmv1 ~ Procurement_SLA +  
     freebies_count + Premium_p + holiday_freq + gmv + 
     TV + Digital + Online.marketing + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
     inc_PO_MA2 + sell_price.1 + sell_price.2 + 
     promotional_discount.3 + NPS.1 + holiday_freq.1 + 
     holiday_freq.3, data = Home_koyck) %>% summary()
#Adjusted R-squared:  0.394 

#Removing sell_price.1
lm(formula = gmv1 ~ Procurement_SLA +  
     freebies_count + Premium_p + holiday_freq + gmv + 
     TV + Digital + Online.marketing + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
     inc_PO_MA2 + inc_PO_MA3 + sell_price.2 + 
     promotional_discount.3 + NPS.1 + holiday_freq.1 + 
     holiday_freq.3, data = Home_koyck) %>% summary()
#Adjusted R-squared:  0.392

#Removing sell_price.2
lm(formula = gmv1 ~ Procurement_SLA +  
     freebies_count + Premium_p + holiday_freq + gmv + 
     TV + Digital + Online.marketing + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
     inc_PO_MA2 + inc_PO_MA3 + sell_price.1 + 
     promotional_discount.3 + NPS.1 + holiday_freq.1 + 
     holiday_freq.3, data = Home_koyck) %>% summary()
#Adjusted R-squared:  0.404

#Removing Digital
lm(formula = gmv1 ~ Procurement_SLA +  
     freebies_count + Premium_p + holiday_freq + gmv + 
     TV + Online.marketing + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
     inc_PO_MA2 + inc_PO_MA3 + sell_price.1 + sell_price.2 + 
     promotional_discount.3 + NPS.1 + holiday_freq.1 + 
     holiday_freq.3, data = Home_koyck) %>% summary()
#Adjusted R-squared:  0.23

#Removing gmv
lm(formula = gmv1 ~ Procurement_SLA +  
     freebies_count + Premium_p + holiday_freq + 
     TV + Digital + Online.marketing + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
     inc_PO_MA2 + inc_PO_MA3 + sell_price.1 + sell_price.2 + 
     promotional_discount.3 + NPS.1 + holiday_freq.1 + 
     holiday_freq.3, data = Home_koyck) %>% summary()
#Adjusted R-squared:  0.467

#So, Removing promotional_discount.3 with vif = 2.09, p-value = 0.07906
koyck_model_16 <- lm(formula = gmv1 ~ Procurement_SLA +  
                       freebies_count + Premium_p + holiday_freq + gmv + 
                       TV + Digital + Online.marketing + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
                       inc_PO_MA2 + inc_PO_MA3 +   
                       sell_price.1 + sell_price.2 + 
                       NPS.1 + holiday_freq.1 + 
                       holiday_freq.3, data = Home_koyck)

summary(koyck_model_16)  

data.frame(var=names(vif(koyck_model_16)),vif=vif(koyck_model_16)) %>% arrange(desc(vif))

#Removing holiday_freq.1 with p-value = 0.59018 
koyck_model_17 <- lm(formula = gmv1 ~ Procurement_SLA +  
                       freebies_count + Premium_p + holiday_freq + gmv + 
                       TV + Digital + Online.marketing + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
                       inc_PO_MA2 + inc_PO_MA3 +   
                       sell_price.1 + sell_price.2 + 
                       NPS.1 + holiday_freq.3, data = Home_koyck)

summary(koyck_model_17)  

data.frame(var=names(vif(koyck_model_17)),vif=vif(koyck_model_17)) %>% arrange(desc(vif))

#Removing holiday_freq with p-value = 0.3529 
koyck_model_18 <- lm(formula = gmv1 ~ Procurement_SLA +  
                       freebies_count + Premium_p + gmv + 
                       TV + Digital + Online.marketing + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
                       inc_PO_MA2 + inc_PO_MA3 +   
                       sell_price.1 + sell_price.2 + 
                       NPS.1 + holiday_freq.3, data = Home_koyck)

summary(koyck_model_18)  

data.frame(var=names(vif(koyck_model_18)),vif=vif(koyck_model_18)) %>% arrange(desc(vif))

#Removing freebies_count with p-value = 0.26454
koyck_model_19 <- lm(formula = gmv1 ~ Procurement_SLA +  
                       Premium_p + gmv + 
                       TV + Digital + Online.marketing + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
                       inc_PO_MA2 + inc_PO_MA3 +   
                       sell_price.1 + sell_price.2 + 
                       NPS.1 + holiday_freq.3, data = Home_koyck)

summary(koyck_model_19)  

data.frame(var=names(vif(koyck_model_19)),vif=vif(koyck_model_19)) %>% arrange(desc(vif))

#Removing holiday_freq.3 with p-value = 0.19172
koyck_model_20 <- lm(formula = gmv1 ~ Procurement_SLA +  
                       Premium_p + gmv + 
                       TV + Digital + Online.marketing + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
                       inc_PO_MA2 + inc_PO_MA3 +   
                       sell_price.1 + sell_price.2 + 
                       NPS.1, data = Home_koyck)

summary(koyck_model_20)  

data.frame(var=names(vif(koyck_model_20)),vif=vif(koyck_model_20)) %>% arrange(desc(vif))

#Removing Procurement_SLA with p-value = 0.05945
koyck_model_21 <- lm(formula = gmv1 ~ Premium_p + gmv + 
                       TV + Digital + Online.marketing + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
                       inc_PO_MA2 + inc_PO_MA3 +   
                       sell_price.1 + sell_price.2 + 
                       NPS.1, data = Home_koyck)

summary(koyck_model_21)  #Adjusted R-squared:  0.502

data.frame(var=names(vif(koyck_model_21)),vif=vif(koyck_model_21)) %>% arrange(desc(vif))

#Checking correlation
koyck_train_21 <- Home_koyck[,c("inc_PO_MA3", "sell_price.1", "sell_price.2", "inc_LP_MA2", "Digital", "inc_PO_MA1", "inc_LP_MA3", "TV", "Online.marketing", "NPS.1", "gmv", "Premium_p")]

cor(Home_koyck$inc_PO_MA2,koyck_train_21)

#Removing inc_PO_MA2
lm(formula = gmv1 ~ Premium_p + gmv + 
     TV + Digital + Online.marketing + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
     inc_PO_MA3 + sell_price.1 + sell_price.2 + 
     NPS.1, data = Home_koyck) %>% summary()
#Adjusted R-squared:  0.343

#Removing inc_PO_MA3
lm(formula = gmv1 ~ Premium_p + gmv + 
     TV + Digital + Online.marketing + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
     inc_PO_MA2 + sell_price.1 + sell_price.2 + 
     NPS.1, data = Home_koyck) %>% summary()
#Adjusted R-squared:  0.345 

koyck_model_22 <- lm(formula = gmv1 ~ Premium_p + gmv + 
                       TV + Digital + Online.marketing + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
                       inc_PO_MA2 + sell_price.1 + sell_price.2 + 
                       NPS.1, data = Home_koyck)

summary(koyck_model_22)  #Adjusted R-squared:  0.345

data.frame(var=names(vif(koyck_model_22)),vif=vif(koyck_model_22)) %>% arrange(desc(vif))

#Removing sell_price.1 with vif = 2.16, p-value = 0.4023
koyck_model_23 <- lm(formula = gmv1 ~ Premium_p + gmv + 
                       TV + Digital + Online.marketing + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
                       inc_PO_MA2 + sell_price.2 + 
                       NPS.1, data = Home_koyck)

summary(koyck_model_23)  #Adjusted R-squared:  0.351

data.frame(var=names(vif(koyck_model_23)),vif=vif(koyck_model_23)) %>% arrange(desc(vif))

#Checking correlation
koyck_train_23 <- Home_koyck[,c("Digital", "inc_PO_MA1", "inc_LP_MA3", "TV", "Online.marketing", "NPS.1", "gmv", "inc_PO_MA2", "Premium_p", "sell_price.2")]

cor(Home_koyck$inc_LP_MA2,koyck_train_23)

#Removing Digital
lm(formula = gmv1 ~ Premium_p + gmv + 
     TV + Online.marketing + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
     inc_PO_MA2 + sell_price.2 + 
     NPS.1, data = Home_koyck) %>% summary()
#Adjusted R-squared:  0.126

#Removing inc_LP_MA2
lm(formula = gmv1 ~ Premium_p + gmv + 
     TV + Digital + Online.marketing + inc_LP_MA3 + inc_PO_MA1 + 
     inc_PO_MA2 + sell_price.2 + 
     NPS.1, data = Home_koyck) %>% summary()
#Adjusted R-squared:  0.0983

#Removing gmv
lm(formula = gmv1 ~ Premium_p +  
     TV + Digital + Online.marketing + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
     inc_PO_MA2 + sell_price.2 + 
     NPS.1, data = Home_koyck) %>% summary()
#Adjusted R-squared:  0.325

#So, Removing gmv with p-value = 0.13049
koyck_model_24 <- lm(formula = gmv1 ~ Premium_p +  
                       TV + Digital + Online.marketing + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
                       inc_PO_MA2 + sell_price.2 + 
                       NPS.1, data = Home_koyck)

summary(koyck_model_24)  #Adjusted R-squared:  0.325

data.frame(var=names(vif(koyck_model_24)),vif=vif(koyck_model_24)) %>% arrange(desc(vif))

#Removing NPS.1 with p-value = 0.06346
koyck_model_25 <- lm(formula = gmv1 ~ Premium_p +  
                       TV + Digital + Online.marketing + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
                       inc_PO_MA2 + sell_price.2, data = Home_koyck)

summary(koyck_model_25)  #Adjusted R-squared:  0.275

data.frame(var=names(vif(koyck_model_25)),vif=vif(koyck_model_25)) %>% arrange(desc(vif))

#Removing Online.marketing with p-value = 0.1110
koyck_model_26 <- lm(formula = gmv1 ~ Premium_p +  
                       TV + Digital + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
                       inc_PO_MA2 + sell_price.2, data = Home_koyck)

summary(koyck_model_26)  #Adjusted R-squared:  0.242

data.frame(var=names(vif(koyck_model_26)),vif=vif(koyck_model_26)) %>% arrange(desc(vif))

#Checking correlation
koyck_train_26 <- Home_koyck[,c("Digital", "inc_PO_MA1", "inc_LP_MA3", "TV", "Premium_p", "sell_price.2","inc_PO_MA2")]

cor(Home_koyck$inc_LP_MA2,koyck_train_26)

#Removing Digital
lm(formula = gmv1 ~ Premium_p +  
     TV + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
     inc_PO_MA2 + sell_price.2, data = Home_koyck) %>% summary()
#Adjusted R-squared:  0.137

#Removing inc_LP_MA2
lm(formula = gmv1 ~ Premium_p +  
     TV + Digital + inc_LP_MA3 + inc_PO_MA1 + 
     inc_PO_MA2 + sell_price.2, data = Home_koyck) %>% summary()
#Adjusted R-squared:  0.11 

#Removing TV
lm(formula = gmv1 ~ Premium_p +  
     Digital + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
     inc_PO_MA2 + sell_price.2, data = Home_koyck) %>% summary()
#Adjusted R-squared:  0.14

#Removing inc_PO_MA2
lm(formula = gmv1 ~ Premium_p +  
     TV + Digital + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
     sell_price.2, data = Home_koyck) %>% summary()
#Adjusted R-squared:  0.237


#Removing inc_PO_MA2 with p-value = 0.272
koyck_model_27 <- lm(formula = gmv1 ~ Premium_p +  
                       TV + Digital + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1 + 
                       sell_price.2, data = Home_koyck)

summary(koyck_model_27)  #Adjusted R-squared:  0.237

data.frame(var=names(vif(koyck_model_27)),vif=vif(koyck_model_27)) %>% arrange(desc(vif))


#Removing sell_price.2 with p-value = 0.2731
koyck_model_28 <- lm(formula = gmv1 ~ Premium_p +  
                       TV + Digital + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1, 
                       data = Home_koyck)

summary(koyck_model_28)  #Adjusted R-squared:  0.233

data.frame(var=names(vif(koyck_model_28)),vif=vif(koyck_model_28)) %>% arrange(desc(vif))


##Final Model 
koyck_Final_Model_home <- koyck_model_28 

# Adjusted R-squared:  0.233 with 5 significant variable 

temp_crossval <- cv.lm(data = Home_koyck, form.lm = formula(gmv1 ~ Premium_p + TV + Digital + inc_LP_MA2 + inc_LP_MA3 + inc_PO_MA1),m = 10)

#Cross_home[3] <- attr(temp_crossval, "ms") # 0.67 # Error

#######################################################################################

# Elasticity Analysis

train <- Home_koyck


grlm <- koyck_Final_Model_home


# estimating the elasticity coefficients

elasticity <- function(var){
  
  elax1 <-as.numeric(grlm$coefficients[var]*mean(train[,var])/mean(train$gmv))
  
  return(elax1)
} 

var_list <- list()

for(i in 2:length(grlm$coefficients)){
  
  var_list[i-1] <-elasticity(names(grlm$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(grlm$coefficients[2:length(grlm$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")


ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Home Audio - Koyck Model") +xlab("Variables")



#######################################################################################################################
#######################################################################################################################
############################################## Distributed lag models #################################################

column_list <- c("SLA", "promotional_discount", "Procurement_SLA", "percentage_of_prepaid_order", "freebies_count", "NPS", "Premium_p", "Mass_p", "Middle_p", "holiday_freq", "gmv", "TV", "Digital", "Sponsorship", "Content.Marketing", "Online.marketing", "inc_LP_MA1", "inc_LP_MA2", "inc_LP_MA3", "inc_PO_MA1", "inc_PO_MA2", "inc_PO_MA3")


Dist_Model_ds <- Home_final[,column_list]

Dist_Model_ds_1 <- slide(Dist_Model_ds, Var = "gmv",slideBy = -1)

Dist_Model_dsl_1 <- slide(Dist_Model_ds_1, Var = "gmv",slideBy = -2)

Dist_Model_ds_1 <- slide(Dist_Model_ds_1, Var = "gmv",slideBy = -3)

Dist_Model_ds <- na.omit(Dist_Model_ds_1)

Dist_Model_ds <- scale(Dist_Model_ds)
Dist_Model_ds <- data.frame(Dist_Model_ds)

# Lag distribution model: 


dist_model <- lm(gmv~.,Dist_Model_ds)

summary(dist_model)

dist_model_2 <- stepAIC(dist_model,direction = "both")

summary(dist_model_2) #Adjusted R-squared:  0.999

lm(formula = gmv ~ Procurement_SLA + percentage_of_prepaid_order + NPS + Mass_p + 
     holiday_freq + Digital + Sponsorship + Content.Marketing + 
     Online.marketing + inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 + 
     inc_PO_MA1 + inc_PO_MA2 + inc_PO_MA3 + gmv.3, data = Dist_Model_ds) %>% summary()
#Adjusted R-squared:  0.999

data.frame(var=names(vif(dist_model_2)),vif=vif(dist_model_2)) %>% arrange(desc(vif))

col_list_3=c("Online.marketing", "inc_LP_MA1", "Digital", "inc_LP_MA3", "inc_PO_MA1", "Content.Marketing", "Sponsorship", "NPS", "inc_PO_MA3", "inc_PO_MA2", "percentage_of_prepaid_order", "Mass_p", "holiday_freq", "gmv.3", "Procurement_SLA")

cor(Dist_Model_ds[,"inc_LP_MA2"],Dist_Model_ds[,col_list_3] )

#Removing inc_LP_MA2
lm(formula = gmv ~ Procurement_SLA + percentage_of_prepaid_order + NPS + Mass_p + 
  holiday_freq + Digital + Sponsorship + Content.Marketing + 
  Online.marketing + inc_LP_MA1 + inc_LP_MA3 + 
  inc_PO_MA1 + inc_PO_MA2 + inc_PO_MA3 + gmv.3, data = Dist_Model_ds) %>% summary()
#Adjusted R-squared:  0.994

#Removing Digital
lm(formula = gmv ~ Procurement_SLA + percentage_of_prepaid_order + NPS + Mass_p + 
  holiday_freq + Sponsorship + Content.Marketing + 
  Online.marketing + inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 + 
  inc_PO_MA1 + inc_PO_MA2 + inc_PO_MA3 + gmv.3, data = Dist_Model_ds) %>% summary()
#Adjusted R-squared:  0.997

#Creating a new model by removing inc_LP_MA2
dist_model_3 <- lm(formula = gmv ~ Procurement_SLA + percentage_of_prepaid_order + NPS + Mass_p + 
                     holiday_freq + Digital + Sponsorship + Content.Marketing + 
                     Online.marketing + inc_LP_MA1 + inc_LP_MA3 + 
                     inc_PO_MA1 + inc_PO_MA2 + inc_PO_MA3 + gmv.3, data = Dist_Model_ds)

summary(dist_model_3)

data.frame(var=names(vif(dist_model_3)),vif=vif(dist_model_3)) %>% arrange(desc(vif))

#Removing Online.marketing (vif = 476.55, p-value = 0.08064)
dist_model_4 <- lm(formula = gmv ~ Procurement_SLA + percentage_of_prepaid_order + NPS + Mass_p + 
                     holiday_freq + Digital + Sponsorship + Content.Marketing + 
                     inc_LP_MA1 + inc_LP_MA3 + 
                     inc_PO_MA1 + inc_PO_MA2 + inc_PO_MA3 + gmv.3, data = Dist_Model_ds)

summary(dist_model_4)

data.frame(var=names(vif(dist_model_4)),vif=vif(dist_model_4)) %>% arrange(desc(vif))

#Removing Sponsorship (vif = 5.82, p-value = 0.1338)
dist_model_5 <- lm(formula = gmv ~ Procurement_SLA + percentage_of_prepaid_order + NPS + Mass_p + 
                     holiday_freq + Digital + Content.Marketing + 
                     inc_LP_MA1 + inc_LP_MA3 + 
                     inc_PO_MA1 + inc_PO_MA2 + inc_PO_MA3 + gmv.3, data = Dist_Model_ds)

summary(dist_model_5)

data.frame(var=names(vif(dist_model_5)),vif=vif(dist_model_5)) %>% arrange(desc(vif))

#Removing inc_PO_MA2 (vif = 5.23, p-value = 0.62380)
dist_model_6 <- lm(formula = gmv ~ Procurement_SLA + percentage_of_prepaid_order + NPS + Mass_p + 
                     holiday_freq + Digital + Content.Marketing + 
                     inc_LP_MA1 + inc_LP_MA3 + 
                     inc_PO_MA1 + inc_PO_MA3 + gmv.3, data = Dist_Model_ds)

summary(dist_model_6)

data.frame(var=names(vif(dist_model_6)),vif=vif(dist_model_6)) %>% arrange(desc(vif))

#Removing NPS (vif = 3.04, p-value = 0.09458)
dist_model_7 <- lm(formula = gmv ~ Procurement_SLA + percentage_of_prepaid_order + Mass_p + 
                     holiday_freq + Digital + Content.Marketing + 
                     inc_LP_MA1 + inc_LP_MA3 + 
                     inc_PO_MA1 + inc_PO_MA3 + gmv.3, data = Dist_Model_ds)

summary(dist_model_7)

data.frame(var=names(vif(dist_model_7)),vif=vif(dist_model_7)) %>% arrange(desc(vif))

col_list_7=c("Content.Marketing", "Digital", "inc_LP_MA3", "inc_PO_MA1", "Mass_p", "holiday_freq", "percentage_of_prepaid_order", "gmv.3", "Procurement_SLA", "inc_PO_MA3")

cor(Dist_Model_ds[,"inc_LP_MA1"],Dist_Model_ds[,col_list_7] )

#Removing Content.Marketing
lm(formula = gmv ~ Procurement_SLA + percentage_of_prepaid_order + Mass_p + 
     holiday_freq + Digital + inc_LP_MA1 + inc_LP_MA3 + 
     inc_PO_MA1 + inc_PO_MA3 + gmv.3, data = Dist_Model_ds) %>% summary()
#Adjusted R-squared:  0.991

#Removing inc_LP_MA1
lm(formula = gmv ~ Procurement_SLA + percentage_of_prepaid_order + Mass_p + 
     holiday_freq + Digital + Content.Marketing + 
     inc_LP_MA3 + inc_PO_MA1 + inc_PO_MA3 + gmv.3, data = Dist_Model_ds) %>% summary()
#Adjusted R-squared:  0.993

#Removing inc_LP_MA1 (vif = 46.53, p-value = 0.00047)

dist_model_8 <- lm(formula = gmv ~ Procurement_SLA + percentage_of_prepaid_order + Mass_p + 
                     holiday_freq + Digital + Content.Marketing + 
                     inc_LP_MA3 + inc_PO_MA1 + inc_PO_MA3 + gmv.3, data = Dist_Model_ds)

summary(dist_model_8)

data.frame(var=names(vif(dist_model_8)),vif=vif(dist_model_8)) %>% arrange(desc(vif))

column_list_8 <- c("Content.Marketing", "inc_LP_MA3", "inc_PO_MA1", "Mass_p", "holiday_freq", "percentage_of_prepaid_order", "inc_PO_MA3", "gmv.3", "Procurement_SLA")
cor(Dist_Model_ds[,"Digital"],Dist_Model_ds[,column_list_8])

#Content.Marketing inc_LP_MA3 inc_PO_MA1    Mass_p holiday_freq percentage_of_prepaid_order inc_PO_MA3    gmv.3
#[1,]         0.9318776 -0.3361954 -0.2830569 0.4105442    0.2032539                  -0.1493821  0.2302468 0.308653
#Procurement_SLA
#[1,]      -0.1687261



#Content.Marketing
lm(formula = gmv ~ Procurement_SLA + percentage_of_prepaid_order + Mass_p + 
     holiday_freq + Digital + inc_LP_MA3 + inc_PO_MA1 + inc_PO_MA3 + gmv.3, data = Dist_Model_ds) %>% summary()
# Adjusted R-squared:  0.9908 

# Digital
lm(formula = gmv ~ Procurement_SLA + percentage_of_prepaid_order + Mass_p + 
     holiday_freq  + Content.Marketing + 
     inc_LP_MA3 + inc_PO_MA1 + inc_PO_MA3 + gmv.3, data = Dist_Model_ds) %>% summary()
# Adjusted R-squared:  0.9902

# Content.Marketing


dist_model_9 <- lm(formula = gmv ~ Procurement_SLA + percentage_of_prepaid_order + Mass_p + 
                     holiday_freq + Digital + inc_LP_MA3 + inc_PO_MA1 + inc_PO_MA3 + gmv.3, data = Dist_Model_ds)

summary(dist_model_9)

data.frame(var=names(vif(dist_model_9)),vif=vif(dist_model_9)) %>% arrange(desc(vif))

# inc_LP_MA3 (VIF=8.0978, p-value=0.6157)

dist_model_10 <- lm(formula = gmv ~ Procurement_SLA + percentage_of_prepaid_order + Mass_p + 
                     holiday_freq + Digital +  inc_PO_MA1 + inc_PO_MA3 + gmv.3, data = Dist_Model_ds)

summary(dist_model_10)

data.frame(var=names(vif(dist_model_10)),vif=vif(dist_model_10)) %>% arrange(desc(vif))

# all VIFs are below 5, so will start eliminating on the basis of p-value.

# inc_PO_MA1 (p-value=0.556)

dist_model_11 <- lm(formula = gmv ~ Procurement_SLA + percentage_of_prepaid_order + Mass_p + 
                      holiday_freq + Digital +  inc_PO_MA3 + gmv.3, data = Dist_Model_ds)

summary(dist_model_11)

# gmv.3 (p-value=0.602)

dist_model_12 <- lm(formula = gmv ~ Procurement_SLA + percentage_of_prepaid_order + Mass_p + 
                      holiday_freq + Digital +  inc_PO_MA3 , data = Dist_Model_ds)

summary(dist_model_12)

# percentage_of_prepaid_order (p-value=0.315)

dist_model_13 <- lm(formula = gmv ~ Procurement_SLA + Mass_p + 
                      holiday_freq + Digital +  inc_PO_MA3 , data = Dist_Model_ds)

summary(dist_model_13)

# holiday_freq (p-value=0.183)

dist_model_14 <- lm(formula = gmv ~ Procurement_SLA + Mass_p + 
                       Digital +  inc_PO_MA3 , data = Dist_Model_ds)

summary(dist_model_14)

# Digital (p-value=0.1034)

dist_model_15 <- lm(formula = gmv ~ Procurement_SLA + Mass_p + 
                        inc_PO_MA3 , data = Dist_Model_ds)

summary(dist_model_15)

##Final Model 

Dist_Final_Model_home <- dist_model_15

# Adjusted R-squared:  0.991 with 3 significant variables

temp_crossval <- cv.lm(data = Dist_Model_ds, form.lm = formula(gmv ~ Procurement_SLA + Mass_p + inc_PO_MA3),m = 10)

Cross_home[4] <- attr(temp_crossval, "ms") # ms=0.0162


#################################################################
# Elasticity Analysis

#train <- Dis_model


grlm <- Dist_Final_Model_home


# estimating the elasticity coefficients

elasticity <- function(var){
  
  elax1 <-as.numeric(Dist_Final_Model_home$coefficients[var]*mean(Dist_Model_ds[,var])/mean(Dist_Model_ds$gmv))
  
  return(elax1)
} 

var_list <- list()

for(i in 2:length(Dist_Final_Model_home$coefficients)){
  
  var_list[i-1] <-elasticity(names(Dist_Final_Model_home$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(Dist_Final_Model_home$coefficients[2:length(Dist_Final_Model_home$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")


ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Home Accessory - Distributed Lag Model") +xlab("Variables")


############################################################
# Multiplicative + distributed model: 

column_list <- c("SLA", "promotional_discount", "Procurement_SLA", "percentage_of_prepaid_order", "freebies_count", "NPS", "Premium_p", "Mass_p", "Middle_p", "holiday_freq", "gmv", "TV", "Digital", "Sponsorship", "Content.Marketing", "Online.marketing", "inc_LP_MA1", "inc_LP_MA2", "inc_LP_MA3", "inc_PO_MA1", "inc_PO_MA2", "inc_PO_MA3")

Multi_Dist_ds <- Home_final[,column_list]

# gmv

Multi_Dist_ds_1 <- slide(Multi_Dist_ds, Var = "gmv",slideBy = -1)

Multi_Dist_ds_1 <- slide(Multi_Dist_ds_1, Var = "gmv",slideBy = -2)

Multi_Dist_ds_1 <- slide(Multi_Dist_ds_1, Var = "gmv",slideBy = -3)

Multi_Dist_ds <- na.omit(Multi_Dist_ds_1)



# Checking number of zeros or less in each variable

sapply(Multi_Dist_ds, function(x) { sum(x <=0)})




#SLA                    promotional_discount  Procurement_SLA     percentage_of_prepaid_order 
#0                        0                     0                   0 
#freebies_count         NPS                   Premium_p           Mass_p 
#43                       0                     42                  0 
#Middle_p               holiday_freq          gmv                 TV 
#2                        30                    0                   0 
#Digital                Sponsorship           Content.Marketing   Online.marketing 
#0                        0                     0                   0 
#inc_LP_MA1             inc_LP_MA2            inc_LP_MA3          inc_PO_MA1 
#0                        0                     24                  24 
#inc_PO_MA2             inc_PO_MA3            gmv-1               gmv-2 
#21                       20                    0                   0 
#gmv-3 
#0 

# lets remove the variables having more than 10 zeros and for the variables having less than 10, will substitute zeros with 
# a small value(0.01)

# Treatment

Multi_Dist_ds$Middle_p[which(Multi_Dist_ds$Middle_p==0)] <- 0.01 

#c("freebies_count","Premium_p","holiday_freq","inc_LP_MA3","inc_PO_MA1","inc_PO_MA3","inc_PO_MA2")


Multi_Dist_ds <- Multi_Dist_ds[,!(names(Multi_Dist_ds) %in% c("freebies_count","Premium_p","holiday_freq","inc_LP_MA3","inc_PO_MA1","inc_PO_MA3","inc_PO_MA2"))]


########################################################

Multi_dist_log <- log(Multi_Dist_ds)

multi_dist_model_1 <- lm(gmv~., Multi_dist_log)

summary(multi_dist_model_1)

multi_dist_model_2 <- stepAIC(multi_dist_model_1,direction = "both")

summary(multi_dist_model_2) 

data.frame(var=names(vif(multi_dist_model_2)),vif=vif(multi_dist_model_2)) %>% arrange(desc(vif))

# Removing Online.marketing with vif = 134.32, p-value=0.1639

multi_dist_model_3 <- lm(formula = gmv ~ SLA + Mass_p + Sponsorship + Content.Marketing +  
                           inc_LP_MA1 + inc_LP_MA2, data = Multi_dist_log)

summary(multi_dist_model_3)

data.frame(var=names(vif(multi_dist_model_3)),vif=vif(multi_dist_model_3)) %>% arrange(desc(vif))

# inc_LP_MA1 with p-value = 0.9995
multi_dist_model_4 <- lm(formula = gmv ~ SLA + Mass_p + Sponsorship + Content.Marketing +  
                           inc_LP_MA2, data = Multi_dist_log)

summary(multi_dist_model_4)

data.frame(var=names(vif(multi_dist_model_4)),vif=vif(multi_dist_model_4)) %>% arrange(desc(vif))

# Adjusted R-squared:  0.992  with 5 significant variables

Multi_Dist_Final_Model_home <- multi_dist_model_4

temp_crossval <- cv.lm(data = Multi_dist_log, form.lm = formula(gmv ~ SLA + Mass_p + Sponsorship + Content.Marketing + inc_LP_MA2),m = 10)

#Cross_home[5] <- attr(temp_crossval, "ms") # 0.0198


##############################################################################################
# Elasticity Analysis

train <- Multi_dist_log


grlm <- Multi_Dist_Final_Model_home


# estimating the elasticity coefficients

elasticity <- function(var){
  
  elax1 <-as.numeric(grlm$coefficients[var]*mean(train[,var])/mean(train$gmv))
  
  return(elax1)
} 

var_list <- list()

for(i in 2:length(grlm$coefficients)){
  
  var_list[i-1] <-elasticity(names(grlm$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(grlm$coefficients[2:length(grlm$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")


ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Home Accessory - Multiplicative & Distributed Lag Model") +xlab("Variables")


##############################################################################################
##############################################################################################
