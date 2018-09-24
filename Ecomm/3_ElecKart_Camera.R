#####################################################################################################
############################# Model on Camera Accessory Dataset#######################################
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


Cross_camera<- rep(0,5)

# Build the Basic Linear regression model
Camera_final <- read.csv("Camera_final.csv", stringsAsFactors = F)

Camera_lm_ds <- Camera_final

column_list <- c("SLA", "promotional_discount", "Procurement_SLA", "percentage_of_prepaid_order", "freebies_count", "NPS", "Premium_p", "Mass_p", "Middle_p", "holiday_freq", "gmv", "TV", "Digital", "Sponsorship", "Content.Marketing", "Online.marketing", "inc_LP_MA1", "inc_LP_MA2", "inc_LP_MA3", "inc_PO_MA1", "inc_PO_MA2", "inc_PO_MA3")

Camera_lm_ds <- Camera_lm_ds[,column_list]

Camera_lm_ds <- scale(Camera_lm_ds)

Camera_lm_ds <-data.frame(Camera_lm_ds)

model_1 <- lm(gmv~.,Camera_lm_ds)


################################################################################################

# Summary of Linear Model 
summary(model_1)


library(car)
library(MASS)
library(DAAG)

model_2 <- stepAIC(model_1,direction = "both")

summary(model_2) 

data.frame(var=names(vif(model_2)),vif=vif(model_2)) %>% arrange(desc(vif))


train_1 <- Camera_lm_ds[,c("inc_PO_MA2", "Sponsorship", "TV", "Content.Marketing", "inc_LP_MA1", "Middle_p", "Mass_p", "inc_PO_MA1", "SLA", "Procurement_SLA", "holiday_freq")]

cor(Camera_lm_ds$inc_PO_MA3,train_1)

# removing inc_PO_MA3
lm(formula = gmv ~ SLA + Procurement_SLA + Mass_p + Middle_p + 
     holiday_freq + TV + Sponsorship + Content.Marketing + inc_LP_MA1 + 
     inc_PO_MA1 + inc_PO_MA2 , data = Camera_lm_ds) %>% summary()
#Adjusted R-squared:  0.9207 

# removing inc_PO_MA2
lm(formula = gmv ~ SLA + Procurement_SLA + Mass_p + Middle_p + 
     holiday_freq + TV + Sponsorship + Content.Marketing + inc_LP_MA1 + 
     inc_PO_MA1  + inc_PO_MA3, data = Camera_lm_ds) %>% summary()
#Adjusted R-squared:  0.9265 

# removing inc_PO_MA2
model_3 <- lm(formula = gmv ~ SLA + Procurement_SLA + Mass_p + Middle_p + 
                holiday_freq + TV + Sponsorship + Content.Marketing + inc_LP_MA1 + 
                inc_PO_MA1  + inc_PO_MA3, data = Camera_lm_ds)
summary(model_3) 

data.frame(var=names(vif(model_3)),vif=vif(model_3)) %>% arrange(desc(vif))

train_3 <- Camera_lm_ds[,c("TV", "Content.Marketing", "inc_LP_MA1", "Middle_p", "inc_PO_MA1", "Mass_p", "SLA", "inc_PO_MA3", "Procurement_SLA", "holiday_freq")]

cor(Camera_lm_ds$Sponsorship,train_3)

# removing Sponsorship
lm(formula = gmv ~ SLA + Procurement_SLA + Mass_p + Middle_p + 
     holiday_freq + TV + Content.Marketing + inc_LP_MA1 + 
     inc_PO_MA1 + inc_PO_MA3, data = Camera_lm_ds) %>% summary()

# Adjusted R-squared:  0.8884 

# removing Content.Marketing
lm(formula = gmv ~ SLA + Procurement_SLA + Mass_p + Middle_p + 
     holiday_freq + TV + Sponsorship  + inc_LP_MA1 + 
     inc_PO_MA1 + inc_PO_MA3, data = Camera_lm_ds) %>% summary()
# Adjusted R-squared:  0.9025 

# removing Content.Marketing
model_4 <-  lm(formula = gmv ~ SLA + Procurement_SLA + Mass_p + Middle_p + 
                 holiday_freq + TV + Sponsorship  + inc_LP_MA1 + 
                 inc_PO_MA1 + inc_PO_MA3, data = Camera_lm_ds)


summary(model_4) 

data.frame(var=names(vif(model_4)),vif=vif(model_4)) %>% arrange(desc(vif))


# renoving TV
model_5 <- lm(formula = gmv ~ SLA + Procurement_SLA + Mass_p + Middle_p + 
                holiday_freq + Sponsorship  + inc_LP_MA1 + 
                inc_PO_MA1 + inc_PO_MA3, data = Camera_lm_ds)


summary(model_5) 

data.frame(var=names(vif(model_5)),vif=vif(model_5)) %>% arrange(desc(vif))
# all the VIF are less that 2


# removing inc_PO_MA1 (p-value = 0.9519)
model_6 <- lm(formula = gmv ~ SLA + Procurement_SLA + Mass_p + Middle_p + 
                holiday_freq + Sponsorship  + inc_LP_MA1 + inc_PO_MA3, data = Camera_lm_ds)

summary(model_6) 

data.frame(var=names(vif(model_6)),vif=vif(model_6)) %>% arrange(desc(vif))

# removing inc_LP_MA1 ( p-value = 0.633)
model_7 <- lm(formula = gmv ~ SLA + Procurement_SLA + Mass_p + Middle_p + 
                holiday_freq + Sponsorship+ inc_PO_MA3, data = Camera_lm_ds)



summary(model_7) # Adjusted R-squared:  0.9012

#vif(data.frame(var=names(vif(model_7)),vif=vif(model_7)) %>% arrange(desc(vif)))

# removing SLA ( p-value= 0.3889)
model_8 <- lm(formula = gmv ~ Procurement_SLA + Mass_p + Middle_p + 
                holiday_freq + Sponsorship+ inc_PO_MA3, data = Camera_lm_ds)


summary(model_8)

# removing Middle_p ( p-value = 0.19588)

model_9 <- lm(formula = gmv ~ Procurement_SLA + Mass_p +
                holiday_freq + Sponsorship+ inc_PO_MA3, data = Camera_lm_ds)

summary(model_9) # Adjusted R-squared:  0.9001 


# removing Sponsorship ( p-value = 0.07688)

model_10 <- lm(formula = gmv ~ Procurement_SLA + Mass_p +
                 holiday_freq + inc_PO_MA3, data = Camera_lm_ds)
# Adjusted R-squared:  0.895

summary(model_10)


##Final Model 
Linear_Final_model_camera <- model_10

# Adj R square  = 0.895  with 4 variables

temp_crossval <- cv.lm(data = Camera_lm_ds, form.lm = formula(gmv ~  Procurement_SLA + Mass_p +holiday_freq + inc_PO_MA3),m = 10)

Cross_camera[1] <- attr(temp_crossval, "ms")

################################################################################

# Elasticity Analysis

train <- Camera_lm_ds


grlm <- Linear_Final_model_camera


# estimating the elasticity coefficients

elasticity <- function(var){
  
  elax1 <-as.numeric(Linear_Final_model_camera$coefficients[var]*mean(train[,var])/mean(train$gmv))
  
  return(elax1)
} 

var_list <- list()

for(i in 2:length(Linear_Final_model_camera$coefficients)){
  
  var_list[i-1] <-elasticity(names(Linear_Final_model_camera$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(Linear_Final_model_camera$coefficients[2:length(Linear_Final_model_camera$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")


ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Camera Accessory - Linear Model") +xlab("Variables")


#####################################################################################################################
#####################################################################################################################
########################################## Multiplicative Model: Camera Accessory #####################################################

Camera_final <- read.csv("Camera_final.csv")

Camera_multiplicative_ds <- Camera_final

column_list <- c("SLA", "promotional_discount", "Procurement_SLA", "percentage_of_prepaid_order", "freebies_count", "NPS", "Premium_p", "Mass_p", "Middle_p", "holiday_freq", "gmv", "TV", "Digital", "Sponsorship", "Content.Marketing", "Online.marketing", "inc_LP_MA1", "inc_LP_MA2", "inc_LP_MA3", "inc_PO_MA1", "inc_PO_MA2", "inc_PO_MA3")

Camera_multiplicative_ds <- Camera_multiplicative_ds[,column_list]

# Checking number of zeros or less in each variable

sapply(Camera_multiplicative_ds, function(x) { sum(x <=0)})




#SLA        promotional_discount             Procurement_SLA percentage_of_prepaid_order 
#0                           0                           0                           2 
#freebies_count                         NPS                   Premium_p                      Mass_p 
#32                           0                          48                           0 
#Middle_p                holiday_freq                         gmv                          TV 
#5                          33                           0                           0 
#Digital                 Sponsorship           Content.Marketing            Online.marketing 
#0                           0                           0                           0 
#inc_LP_MA1                  inc_LP_MA2                  inc_LP_MA3                  inc_PO_MA1 
#0                           0                          29                          29 
#inc_PO_MA2                  inc_PO_MA3 
#23                          21 

# lets remove the variables having more than 10 zeros and for the variables having less than 10, will substitute zeros with 
# a small value(0.01)

# Treatment

Camera_multiplicative_ds$Middle_p[which(Camera_multiplicative_ds$Middle_p==0)] <- 0.01

Camera_multiplicative_ds$percentage_of_prepaid_order[which(Camera_multiplicative_ds$percentage_of_prepaid_order==0)] <- 0.01

#c("freebies_count","Premium_p","holiday_freq","inc_LP_MA3","inc_PO_MA1","inc_PO_MA3","inc_PO_MA2")


Camera_multiplicative_ds <- Camera_multiplicative_ds[,!(names(Camera_multiplicative_ds) %in% c("freebies_count","Premium_p","holiday_freq","inc_LP_MA3","inc_PO_MA1","inc_PO_MA3","inc_PO_MA2"))]

Camera_multiplicative_ds_log <- log(Camera_multiplicative_ds)

################################################
#################################################

## Build the First model ##

multi_model <- lm(gmv~.,Camera_multiplicative_ds_log)

summary(multi_model)

library(dplyr)
library(car)
library(MASS)

multi_model_2 <- stepAIC(multi_model,direction = "both")

summary(multi_model_2)

data.frame(var=names(vif(multi_model_2)),vif=vif(multi_model_2)) %>% arrange(desc(vif))


#Online.marketing (VIF=2009.2000, p-value=0.11224 )

multi_model_3 <- lm(formula = gmv ~ promotional_discount + Procurement_SLA + NPS + 
                Mass_p + Content.Marketing  + inc_LP_MA1 + 
                inc_LP_MA2, data = Camera_multiplicative_ds_log)



summary(multi_model_3) 

data.frame(var=names(vif(multi_model_3)),vif=vif(multi_model_3)) %>% arrange(desc(vif))

# inc_LP_MA1 (VIF=17.4910, p-value=0.28001)

multi_model_4 <- lm(formula = gmv ~ promotional_discount + Procurement_SLA + NPS + 
                      Mass_p + Content.Marketing  + inc_LP_MA2, data = Camera_multiplicative_ds_log)

summary(multi_model_4)

data.frame(var=names(vif(multi_model_4)),vif=vif(multi_model_4)) %>% arrange(desc(vif))

cor(Camera_multiplicative_ds_log[,"Content.Marketing"],Camera_multiplicative_ds_log[,c("NPS", "Mass_p", "promotional_discount", "inc_LP_MA2", "Procurement_SLA")] )


#  NPS          Mass_p       promotional_discount inc_LP_MA2 Procurement_SLA
# -0.9018827 0.7819496          -0.01984199        0.6061598       0.2891018





lm(formula = gmv ~ promotional_discount + Procurement_SLA + NPS + 
     Mass_p + inc_LP_MA2, data = Camera_multiplicative_ds_log) %>% summary()

#Adjusted R-squared:  0.9968

lm(formula = gmv ~ promotional_discount + Procurement_SLA + NPS + 
     Content.Marketing  + inc_LP_MA2, data = Camera_multiplicative_ds_log) %>% summary()

# Adjusted R-squared:  0.693



multi_model_5 <- lm(formula = gmv ~ promotional_discount + Procurement_SLA + NPS + 
                      Mass_p + inc_LP_MA2, data = Camera_multiplicative_ds_log)

summary(multi_model_5)

data.frame(var=names(vif(multi_model_5)),vif=vif(multi_model_5)) %>% arrange(desc(vif))


# no VIF too high, Procurement_SLA ( p-value=0.13280)

multi_model_6 <- lm(formula = gmv ~ promotional_discount + NPS + 
                      Mass_p + inc_LP_MA2, data = Camera_multiplicative_ds_log)

summary(multi_model_6)

data.frame(var=names(vif(multi_model_6)),vif=vif(multi_model_6)) %>% arrange(desc(vif))


# inc_LP_MA2 (p-value=0.12782)

multi_model_7 <- lm(formula = gmv ~ promotional_discount + NPS + 
                     Mass_p, data = Camera_multiplicative_ds_log)

summary(multi_model_7)

#Call:
#  lm(formula = gmv ~ promotional_discount + NPS + Mass_p, data = Camera_multiplicative_ds_log)
#
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-0.23889 -0.08019 -0.00326  0.08529  0.28030 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)            8.9769     1.3761    6.52  5.2e-08 ***
#  promotional_discount  -1.5117     0.1408  -10.74  5.3e-14 ***
#  NPS                   -0.8540     0.3304   -2.59    0.013 *  
#  Mass_p                 1.0451     0.0139   75.17  < 2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 0.111 on 45 degrees of freedom
#Multiple R-squared:  0.997,	Adjusted R-squared:  0.997 
#F-statistic: 4.68e+03 on 3 and 45 DF,  p-value: <2e-16

### 
# Multiplicative : Final Model Model-7

multi_final_model_camera <- multi_model_7

#Adjusted R-squared:  0.997  with 3 significant variables 

temp_crossval <- cv.lm(data = Camera_multiplicative_ds, form.lm = formula(gmv ~ promotional_discount + NPS + Mass_p),m = 10)
Cross_camera[2] <- attr(temp_crossval, "ms") # ms=1.38e+12

##########################################################################################
# Elasticity Analysis

train <- Camera_multiplicative_ds


grlm <- multi_final_model_camera


# estimating the elasticity coefficients

elasticity <- function(var){
  
  elax1 <-as.numeric(multi_final_model_camera$coefficients[var]*mean(train[,var])/mean(train$gmv))
  
  return(elax1)
} 

var_list <- list()

for(i in 2:length(multi_final_model_camera$coefficients)){
  
  var_list[i-1] <-elasticity(names(multi_final_model_camera$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(multi_final_model_camera$coefficients[2:length(multi_final_model_camera$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")


ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Camera Accessory - Multiplicative Model") +xlab("Variables")


#######################################################################################################################
#######################################################################################################################
############################################## Koyck models #################################################

Camera_koyck<- Camera_final[,c("SLA", "promotional_discount", "Procurement_SLA", "percentage_of_prepaid_order", "freebies_count", "NPS", "Premium_p", "Mass_p", "Middle_p", "holiday_freq", "gmv", "Total.Investment", "TV", "Digital", "Sponsorship", "Content.Marketing", "Online.marketing", "inc_LP_MA1", "inc_LP_MA2", "inc_LP_MA3", "inc_PO_MA1", "inc_PO_MA2", "inc_PO_MA3", "inc_LP_MA3.1", "inc_PO_MA1.1", "inc_PO_MA2.1", "inc_PO_MA3.1", "sell_price.1", "sell_price.2", "sell_price.3", "promotional_discount.1", "promotional_discount.2", "promotional_discount.3", "NPS.1", "NPS.2", "NPS.3", "holiday_freq.1", "holiday_freq.2", "holiday_freq.3")]

library(DataCombine)

# gmv 1 lag
Camera_koyck <- slide(Camera_koyck, Var = "gmv",slideBy = 1)


Camera_koyck <- na.omit(Camera_koyck)

Camera_koyck <- scale(Camera_koyck)

Camera_koyck <- data.frame(Camera_koyck)

# Build first model
koyck_model <- lm(gmv1~.,Camera_koyck)

summary(koyck_model)

koyck_model_2 <- stepAIC(koyck_model,direction = "both")

summary(koyck_model_2)  
data.frame(var=names(vif(koyck_model_2)),vif=vif(koyck_model_2)) %>% arrange(desc(vif))


# inc_LP_MA3.1  vif=107.62, p-value= 0.16994)

koyck_model_3 <- lm(formula = gmv1 ~ SLA + promotional_discount + Procurement_SLA + 
                percentage_of_prepaid_order + freebies_count + Premium_p + 
                Mass_p + Middle_p + gmv + Sponsorship + Content.Marketing + 
                Online.marketing + inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 + 
                inc_PO_MA1 + inc_PO_MA3  + inc_PO_MA2.1 + inc_PO_MA3.1 + 
                sell_price.1 + promotional_discount.3 + NPS.1 + NPS.2 + NPS.3 + 
                holiday_freq.2, data = Camera_koyck)


summary(koyck_model_3)

data.frame(var=names(vif(koyck_model_3)),vif=vif(koyck_model_3)) %>% arrange(desc(vif))

# gmv   47.80 (p-value=0.08940)


koyck_model_4 <- lm(formula = gmv1 ~ SLA + promotional_discount + Procurement_SLA + 
                      percentage_of_prepaid_order + freebies_count + Premium_p + 
                      Mass_p + Middle_p  + Sponsorship + Content.Marketing + 
                      Online.marketing + inc_LP_MA1 + inc_LP_MA2 + inc_LP_MA3 + 
                      inc_PO_MA1 + inc_PO_MA3  + inc_PO_MA2.1 + inc_PO_MA3.1 + 
                      sell_price.1 + promotional_discount.3 + NPS.1 + NPS.2 + NPS.3 + 
                      holiday_freq.2, data = Camera_koyck)


summary(koyck_model_4)

data.frame(var=names(vif(koyck_model_4)),vif=vif(koyck_model_4)) %>% arrange(desc(vif))


# inc_LP_MA3  117.07 (p-value=0.03968)

koyck_model_5 <- lm(formula = gmv1 ~ SLA + promotional_discount + Procurement_SLA + 
                      percentage_of_prepaid_order + freebies_count + Premium_p + 
                      Mass_p + Middle_p  + Sponsorship + Content.Marketing + 
                      Online.marketing + inc_LP_MA1 + inc_LP_MA2 +  
                      inc_PO_MA1 + inc_PO_MA3  + inc_PO_MA2.1 + inc_PO_MA3.1 + 
                      sell_price.1 + promotional_discount.3 + NPS.1 + NPS.2 + NPS.3 + 
                      holiday_freq.2, data = Camera_koyck)


summary(koyck_model_5)

data.frame(var=names(vif(koyck_model_5)),vif=vif(koyck_model_5)) %>% arrange(desc(vif))

# NPS.2 (VIF=22.3230, p-value=0.232801)
  
koyck_model_6 <- lm(formula = gmv1 ~ SLA + promotional_discount + Procurement_SLA + 
                percentage_of_prepaid_order + freebies_count + Premium_p + 
                Mass_p + Middle_p + Sponsorship + Content.Marketing + Online.marketing + 
                inc_LP_MA1 + inc_LP_MA2 + inc_PO_MA1 + inc_PO_MA3 + inc_PO_MA2.1 + 
                inc_PO_MA3.1 + sell_price.1 + promotional_discount.3 + NPS.1 + 
                NPS.3 + holiday_freq.2, data = Camera_koyck)


summary(koyck_model_6)

data.frame(var=names(vif(koyck_model_6)),vif=vif(koyck_model_6)) %>% arrange(desc(vif))

# NPS.1   (15.4090, p-value=0.847725)
koyck_model_7 <- lm(formula = gmv1 ~ SLA + promotional_discount + Procurement_SLA + 
                      percentage_of_prepaid_order + freebies_count + Premium_p + 
                      Mass_p + Middle_p + Sponsorship + Content.Marketing + Online.marketing + 
                      inc_LP_MA1 + inc_LP_MA2 + inc_PO_MA1 + inc_PO_MA3 + inc_PO_MA2.1 + 
                      inc_PO_MA3.1 + sell_price.1 + promotional_discount.3 + 
                      NPS.3 + holiday_freq.2, data = Camera_koyck)


summary(koyck_model_7)

data.frame(var=names(vif(koyck_model_7)),vif=vif(koyck_model_7)) %>% arrange(desc(vif))

# Procurement_SLA    5.9775, p-value=0.275532

koyck_model_8 <- lm(formula = gmv1 ~ SLA + promotional_discount + percentage_of_prepaid_order + freebies_count + Premium_p + 
                      Mass_p + Middle_p + Sponsorship + Content.Marketing + Online.marketing + 
                      inc_LP_MA1 + inc_LP_MA2 + inc_PO_MA1 + inc_PO_MA3 + inc_PO_MA2.1 + 
                      inc_PO_MA3.1 + sell_price.1 + promotional_discount.3 + 
                      NPS.3 + holiday_freq.2, data = Camera_koyck)


summary(koyck_model_8)

data.frame(var=names(vif(koyck_model_8)),vif=vif(koyck_model_8)) %>% arrange(desc(vif))

# sell_price.1    8.0920 p-value=0.049641

koyck_model_9 <- lm(formula = gmv1 ~ SLA + promotional_discount + percentage_of_prepaid_order + freebies_count + Premium_p + 
                      Mass_p + Middle_p + Sponsorship + Content.Marketing + Online.marketing + 
                      inc_LP_MA1 + inc_LP_MA2 + inc_PO_MA1 + inc_PO_MA3 + inc_PO_MA2.1 + 
                      inc_PO_MA3.1 + promotional_discount.3 + 
                      NPS.3 + holiday_freq.2, data = Camera_koyck)


summary(koyck_model_9)

data.frame(var=names(vif(koyck_model_9)),vif=vif(koyck_model_9)) %>% arrange(desc(vif))

column_list9 <- c("inc_PO_MA2.1", "promotional_discount.3", "promotional_discount", "Online.marketing", "inc_LP_MA1", "Content.Marketing", "inc_LP_MA2", "inc_PO_MA3", "Middle_p", "Sponsorship", "NPS.3", "Mass_p", "SLA", "inc_PO_MA1", "percentage_of_prepaid_order", "holiday_freq.2", "freebies_count", "Premium_p")


cor(Camera_koyck[,"inc_PO_MA3.1"], Camera_koyck[,column_list9])
#inc_PO_MA2.1 promotional_discount.3 promotional_discount Online.marketing  inc_LP_MA1 Content.Marketing  inc_LP_MA2 inc_PO_MA3    Middle_p Sponsorship
#[1,]    0.9326001             -0.2911176             0.851122      -0.04303455 -0.03599834       -0.08276972 -0.08189801 -0.8464063 -0.02360509  -0.0635983
#NPS.3      Mass_p        SLA  inc_PO_MA1 percentage_of_prepaid_order holiday_freq.2 freebies_count   Premium_p
#[1,] -0.07455694 -0.05855881 -0.4335152 0.005355953                  -0.1647825     -0.1392268     -0.2471726 -0.05056996

# inc_PO_MA3.1
lm(formula = gmv1 ~ SLA + promotional_discount + percentage_of_prepaid_order + freebies_count + Premium_p + 
     Mass_p + Middle_p + Sponsorship + Content.Marketing + Online.marketing + 
     inc_LP_MA1 + inc_LP_MA2 + inc_PO_MA1 + inc_PO_MA3 + inc_PO_MA2.1 + 
     promotional_discount.3 + NPS.3 + holiday_freq.2, data = Camera_koyck) %>% summary()
# Adjusted R-squared:  0.6097 


# inc_PO_MA2.1
lm(formula = gmv1 ~ SLA + promotional_discount + percentage_of_prepaid_order + freebies_count + Premium_p + 
     Mass_p + Middle_p + Sponsorship + Content.Marketing + Online.marketing + 
     inc_LP_MA1 + inc_LP_MA2 + inc_PO_MA1 + inc_PO_MA3 + inc_PO_MA3.1 + promotional_discount.3 + 
     NPS.3 + holiday_freq.2, data = Camera_koyck) %>% summary()

#Adjusted R-squared:  0.618

# will drop inc_PO_MA2.1

koyck_model_10 <- lm(formula = gmv1 ~ SLA + promotional_discount + percentage_of_prepaid_order + freebies_count + Premium_p + 
                      Mass_p + Middle_p + Sponsorship + Content.Marketing + Online.marketing + 
                      inc_LP_MA1 + inc_LP_MA2 + inc_PO_MA1 + inc_PO_MA3 + inc_PO_MA3.1 + promotional_discount.3 + 
                      NPS.3 + holiday_freq.2, data = Camera_koyck)


summary(koyck_model_10)

data.frame(var=names(vif(koyck_model_10)),vif=vif(koyck_model_10)) %>% arrange(desc(vif))


# Online.marketing 364.1700 , p-value=0.23257 

koyck_model_11 <- lm(formula = gmv1 ~ SLA + promotional_discount + percentage_of_prepaid_order + freebies_count + Premium_p + 
                       Mass_p + Middle_p + Sponsorship + Content.Marketing + 
                       inc_LP_MA1 + inc_LP_MA2 + inc_PO_MA1 + inc_PO_MA3 + inc_PO_MA3.1 + promotional_discount.3 + 
                       NPS.3 + holiday_freq.2, data = Camera_koyck)


summary(koyck_model_11)

data.frame(var=names(vif(koyck_model_11)),vif=vif(koyck_model_11)) %>% arrange(desc(vif))

# inc_PO_MA3.1 21.1720 , p-value=0.06505

koyck_model_12 <- lm(formula = gmv1 ~ SLA + promotional_discount + percentage_of_prepaid_order + freebies_count + Premium_p + 
                       Mass_p + Middle_p + Sponsorship + Content.Marketing + 
                       inc_LP_MA1 + inc_LP_MA2 + inc_PO_MA1 + inc_PO_MA3  + promotional_discount.3 + 
                       NPS.3 + holiday_freq.2, data = Camera_koyck)


summary(koyck_model_12)

data.frame(var=names(vif(koyck_model_12)),vif=vif(koyck_model_12)) %>% arrange(desc(vif))

# promotional_discount  2.6612 , p-value= 0.47172
koyck_model_13 <- lm(formula = gmv1 ~ SLA + percentage_of_prepaid_order + freebies_count + Premium_p + 
                       Mass_p + Middle_p + Sponsorship + Content.Marketing + 
                       inc_LP_MA1 + inc_LP_MA2 + inc_PO_MA1 + inc_PO_MA3  + promotional_discount.3 + 
                       NPS.3 + holiday_freq.2, data = Camera_koyck)


summary(koyck_model_13)

data.frame(var=names(vif(koyck_model_13)),vif=vif(koyck_model_13)) %>% arrange(desc(vif))


# Middle_p  2.1836, p-value=0.208227
koyck_model_14 <- lm(formula = gmv1 ~ SLA + percentage_of_prepaid_order + freebies_count + Premium_p + 
                       Mass_p + Sponsorship + Content.Marketing + 
                       inc_LP_MA1 + inc_LP_MA2 + inc_PO_MA1 + inc_PO_MA3  + promotional_discount.3 + 
                       NPS.3 + holiday_freq.2, data = Camera_koyck)


summary(koyck_model_14)

data.frame(var=names(vif(koyck_model_14)),vif=vif(koyck_model_14)) %>% arrange(desc(vif))

# Content.Marketing 36.6170 , p-value= 0.010606
koyck_model_15 <- lm(formula = gmv1 ~ SLA + percentage_of_prepaid_order + freebies_count + Premium_p + 
                       Mass_p + Sponsorship + inc_LP_MA1 + inc_LP_MA2 + inc_PO_MA1 + inc_PO_MA3  + promotional_discount.3 + 
                       NPS.3 + holiday_freq.2, data = Camera_koyck)


summary(koyck_model_15)

data.frame(var=names(vif(koyck_model_15)),vif=vif(koyck_model_15)) %>% arrange(desc(vif))


# vif is below 5, will start droping as per p-value

# SLA, p-value=0.895298
koyck_model_16 <- lm(formula = gmv1 ~  percentage_of_prepaid_order + freebies_count + Premium_p + 
                       Mass_p + Sponsorship + inc_LP_MA1 + inc_LP_MA2 + inc_PO_MA1 + inc_PO_MA3  + promotional_discount.3 + 
                       NPS.3 + holiday_freq.2, data = Camera_koyck)


summary(koyck_model_16)

# promotional_discount.3, p-value=0.825529


koyck_model_17 <- lm(formula = gmv1 ~  percentage_of_prepaid_order + freebies_count + Premium_p + 
                       Mass_p + Sponsorship + inc_LP_MA1 + inc_LP_MA2 + inc_PO_MA1 + inc_PO_MA3  +  
                       NPS.3 + holiday_freq.2, data = Camera_koyck)


summary(koyck_model_17)

#Premium_p, p-value=0.610328

koyck_model_18 <- lm(formula = gmv1 ~  percentage_of_prepaid_order + freebies_count + 
                       Mass_p + Sponsorship + inc_LP_MA1 + inc_LP_MA2 + inc_PO_MA1 + inc_PO_MA3  +  
                       NPS.3 + holiday_freq.2, data = Camera_koyck)


summary(koyck_model_18)

#percentage_of_prepaid_order, p-value=0.350947

koyck_model_19 <- lm(formula = gmv1 ~   freebies_count + 
                       Mass_p + Sponsorship + inc_LP_MA1 + inc_LP_MA2 + inc_PO_MA1 + inc_PO_MA3  +  
                       NPS.3 + holiday_freq.2, data = Camera_koyck)


summary(koyck_model_19)

# freebies_count, p-value=0.318977

koyck_model_20 <- lm(formula = gmv1 ~ Mass_p + Sponsorship + inc_LP_MA1 + inc_LP_MA2 + inc_PO_MA1 + inc_PO_MA3  +  
                       NPS.3 + holiday_freq.2, data = Camera_koyck)


summary(koyck_model_20)

# inc_LP_MA2, p-value=0.396382 

koyck_model_21 <- lm(formula = gmv1 ~ Mass_p + Sponsorship + inc_LP_MA1 + inc_PO_MA1 + inc_PO_MA3  +  
                       NPS.3 + holiday_freq.2, data = Camera_koyck)


summary(koyck_model_21)

# inc_PO_MA1, p-value= 0.402383

koyck_model_22 <- lm(formula = gmv1 ~ Mass_p + Sponsorship + inc_LP_MA1 + inc_PO_MA3  +  
                       NPS.3 + holiday_freq.2, data = Camera_koyck)


summary(koyck_model_22)


# holiday_freq.2, 0.248145 
koyck_model_23 <- lm(formula = gmv1 ~ Mass_p + Sponsorship + inc_LP_MA1 + inc_PO_MA3  +  
                       NPS.3 , data = Camera_koyck)


summary(koyck_model_23)

# Mass_p, 0.033708
koyck_model_24 <- lm(formula = gmv1 ~ Sponsorship + inc_LP_MA1 + inc_PO_MA3  +  
                       NPS.3 , data = Camera_koyck)


summary(koyck_model_24)

##Final Model 
koyck_Final_Model_camera <- koyck_model_24 

# Adjusted R-squared:  0.496 with 4 significant variable 

temp_crossval <- cv.lm(data = Camera_koyck, form.lm = formula(gmv1 ~ Sponsorship + inc_LP_MA1 + inc_PO_MA3+ NPS.3 ),m = 10)

Cross_camera[3] <- attr(temp_crossval, "ms") # 0.67

#######################################################################################

# Elasticity Analysis

train <- Camera_koyck


grlm <- koyck_Final_Model_camera


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
  ggtitle("Camera Accessory - Koyck Model") +xlab("Variables")



#######################################################################################################################
#######################################################################################################################
############################################## Distributed lag models #################################################

column_list <- c("SLA", "promotional_discount", "Procurement_SLA", "percentage_of_prepaid_order", "freebies_count", "NPS", "Premium_p", "Mass_p", "Middle_p", "holiday_freq", "gmv", "TV", "Digital", "Sponsorship", "Content.Marketing", "Online.marketing", "inc_LP_MA1", "inc_LP_MA2", "inc_LP_MA3", "inc_PO_MA1", "inc_PO_MA2", "inc_PO_MA3")


Dist_Model_ds <- Camera_final[,column_list]

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

summary(dist_model_2)

data.frame(var=names(vif(dist_model_2)),vif=vif(dist_model_2)) %>% arrange(desc(vif))


# inc_PO_MA2  8.5003 , p-value=0.079713

dist_model_3 <- lm(formula = gmv ~ SLA + promotional_discount + Procurement_SLA + 
                Mass_p + Middle_p + holiday_freq + Digital + Sponsorship + 
                inc_LP_MA1  + inc_PO_MA3 + percentage_of_prepaid_order, 
              data = Dist_Model_ds)

summary(dist_model_3)


data.frame(var=names(vif(dist_model_3)),vif=vif(dist_model_3)) %>% arrange(desc(vif))

col_list_3=c("Procurement_SLA", "Sponsorship", "inc_PO_MA3", "Digital", "SLA", "Mass_p", "inc_LP_MA1", "Middle_p", "percentage_of_prepaid_order", "holiday_freq")

cor(Dist_Model_ds[,"promotional_discount"],Dist_Model_ds[,col_list_3] )

#Procurement_SLA Sponsorship inc_PO_MA3    Digital        SLA      Mass_p inc_LP_MA1  Middle_p percentage_of_prepaid_order holiday_freq
#  -0.7799825  -0.1439409 -0.7176749 -0.1311434 -0.5003768 -0.01115374 -0.2571115 0.1961788                 -0.07142823  -0.02079814


# promotional_discount
lm(formula = gmv ~ SLA  + Procurement_SLA + 
     Mass_p + Middle_p + holiday_freq + Digital + Sponsorship + 
     inc_LP_MA1  + inc_PO_MA3 + percentage_of_prepaid_order, 
   data = Dist_Model_ds) %>% summary()

# Adjusted R-squared:  0.9097

# Procurement_SLA
lm(formula = gmv ~ SLA + promotional_discount + 
     Mass_p + Middle_p + holiday_freq + Digital + Sponsorship + 
     inc_LP_MA1  + inc_PO_MA3 + percentage_of_prepaid_order, 
   data = Dist_Model_ds) %>% summary()

# Adjusted R-squared:  0.9067 

# adj R-sq are almost same, we will with droping of Procurement_SLA

dist_model_4 <- lm(formula = gmv ~ SLA + promotional_discount + 
                     Mass_p + Middle_p + holiday_freq + Digital + Sponsorship + 
                     inc_LP_MA1  + inc_PO_MA3 + percentage_of_prepaid_order, 
                   data = Dist_Model_ds)

summary(dist_model_4)


data.frame(var=names(vif(dist_model_4)),vif=vif(dist_model_4)) %>% arrange(desc(vif))



# promotional_discount 5.1419 , p-value=0.359844


dist_model_5 <- lm(formula = gmv ~ SLA + Mass_p + Middle_p + holiday_freq + Digital + Sponsorship + 
                     inc_LP_MA1  + inc_PO_MA3 + percentage_of_prepaid_order, 
                   data = Dist_Model_ds)

summary(dist_model_5)


data.frame(var=names(vif(dist_model_5)),vif=vif(dist_model_5)) %>% arrange(desc(vif))

# All VIF values are less than 5, will start eliminating on the basis of p-value now

# SLA, p-value=0.457272


dist_model_6 <- lm(formula = gmv ~ Mass_p + Middle_p + holiday_freq + Digital + Sponsorship + 
                     inc_LP_MA1  + inc_PO_MA3 + percentage_of_prepaid_order, 
                   data = Dist_Model_ds)

summary(dist_model_6)

# inc_LP_MA1 , p-value=0.265256

dist_model_7 <- lm(formula = gmv ~ Mass_p + Middle_p + holiday_freq + Digital + Sponsorship + 
                      inc_PO_MA3 + percentage_of_prepaid_order, 
                   data = Dist_Model_ds)

summary(dist_model_7)

# holiday_freq, p-value=0.080676

dist_model_8 <- lm(formula = gmv ~ Mass_p + Middle_p  + Digital + Sponsorship + 
                     inc_PO_MA3 + percentage_of_prepaid_order, 
                   data = Dist_Model_ds)

summary(dist_model_8)



##Final Model 

Dist_Final_Model_camera <- dist_model_8

# Adjusted R-squared:  0.9022 with 6 significant variables

temp_crossval <- cv.lm(data = Dist_Model_ds, form.lm = formula(gmv ~ Mass_p + Middle_p  + Digital + Sponsorship +inc_PO_MA3 + percentage_of_prepaid_order),m = 10)

Cross_camera[4] <- attr(temp_crossval, "ms") # ms=0.133


#################################################################
# Elasticity Analysis

train <- Dist_Model_ds


grlm <- Dist_Final_Model_camera


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
  ggtitle("Camera Accessory - Distributed Lag Model") +xlab("Variables")


############################################################
# Multiplicative + distributed model: 

column_list <- c("SLA", "promotional_discount", "Procurement_SLA", "percentage_of_prepaid_order", "freebies_count", "NPS", "Premium_p", "Mass_p", "Middle_p", "holiday_freq", "gmv", "TV", "Digital", "Sponsorship", "Content.Marketing", "Online.marketing", "inc_LP_MA1", "inc_LP_MA2", "inc_LP_MA3", "inc_PO_MA1", "inc_PO_MA2", "inc_PO_MA3")

Multi_Dist_ds <- Camera_final[,column_list]

# gmv

Multi_Dist_ds_1 <- slide(Multi_Dist_ds, Var = "gmv",slideBy = -1)

Multi_Dist_ds_1 <- slide(Multi_Dist_ds_1, Var = "gmv",slideBy = -2)

Multi_Dist_ds_1 <- slide(Multi_Dist_ds_1, Var = "gmv",slideBy = -3)

Multi_Dist_ds <- na.omit(Multi_Dist_ds_1)



# Checking number of zeros or less in each variable

sapply(Multi_Dist_ds, function(x) { sum(x <=0)})




#SLA        promotional_discount             Procurement_SLA percentage_of_prepaid_order              freebies_count 
#0                           0                           0                           1                          30 
#NPS                   Premium_p                      Mass_p                    Middle_p                holiday_freq 
#0                          45                           0                           2                          30 
#gmv                          TV                     Digital                 Sponsorship           Content.Marketing 
#0                           0                           0                           0                           0 
#Online.marketing                  inc_LP_MA1                  inc_LP_MA2                  inc_LP_MA3                  inc_PO_MA1 
#0                           0                           0                          26                          26 
#inc_PO_MA2                  inc_PO_MA3                       gmv-1                       gmv-2                       gmv-3 
#20                          19                           0                           0                           0     21 

# lets remove the variables having more than 10 zeros and for the variables having less than 10, will substitute zeros with 
# a small value(0.01)

# Treatment

Multi_Dist_ds$Middle_p[which(Multi_Dist_ds$Middle_p==0)] <- 0.01

Multi_Dist_ds$percentage_of_prepaid_order[which(Multi_Dist_ds$percentage_of_prepaid_order==0)] <- 0.01

#c("freebies_count","Premium_p","holiday_freq","inc_LP_MA3","inc_PO_MA1","inc_PO_MA3","inc_PO_MA2")


Multi_Dist_ds <- Multi_Dist_ds[,!(names(Multi_Dist_ds) %in% c("freebies_count","Premium_p","holiday_freq","inc_LP_MA3","inc_PO_MA1","inc_PO_MA3","inc_PO_MA2"))]


########################################################

Multi_dist_log <- log(Multi_Dist_ds)

multi_dist_model_1 <- lm(gmv~., Multi_dist_log)

summary(multi_dist_model_1)

multi_dist_model_2 <- stepAIC(multi_dist_model_1,direction = "both")

summary(multi_dist_model_2) 

data.frame(var=names(vif(multi_dist_model_2)),vif=vif(multi_dist_model_2)) %>% arrange(desc(vif))

# Online.marketing 1352.10, p-value=0.069

multi_dist_model_3 <- lm(formula = gmv ~ promotional_discount + NPS + Mass_p + Content.Marketing + 
                inc_LP_MA1 + inc_LP_MA2 + `gmv-2`, data = Multi_dist_log)

summary(multi_dist_model_3)

data.frame(var=names(vif(multi_dist_model_3)),vif=vif(multi_dist_model_3)) %>% arrange(desc(vif))

# inc_LP_MA1 19.23, p-value=0.762
multi_dist_model_4 <- lm(formula = gmv ~ promotional_discount + NPS + Mass_p + Content.Marketing + 
                            inc_LP_MA2 + `gmv-2`, data = Multi_dist_log)

summary(multi_dist_model_4)

data.frame(var=names(vif(multi_dist_model_4)),vif=vif(multi_dist_model_4)) %>% arrange(desc(vif))


col_list_4 <- c("NPS", "Mass_p", "inc_LP_MA2", "gmv-2", "promotional_discount")

cor(Multi_dist_log[,"Content.Marketing"], Multi_dist_log[,col_list_4])

#NPS         Mass_p  inc_LP_MA2   gmv-2 promotional_discount
#-0.874     0.841         0.623   0.509               -0.136

# Content.Marketing
lm(formula = gmv ~ promotional_discount + NPS + Mass_p + 
     inc_LP_MA2 + `gmv-2`, data = Multi_dist_log) %>% summary()
# Adjusted R-squared:  0.996

# NPS
lm(formula = gmv ~ promotional_discount  + Mass_p + Content.Marketing + 
     inc_LP_MA2 + `gmv-2`, data = Multi_dist_log) %>% summary()

# Adjusted R-squared:  0.994 

# Mass_p
lm(formula = gmv ~ promotional_discount + NPS  + Content.Marketing + 
     inc_LP_MA2 + `gmv-2`, data = Multi_dist_log) %>% summary()
# Adjusted R-squared:  0.81 


# Content.Marketing

multi_dist_model_5 <- lm(formula = gmv ~ promotional_discount + NPS + Mass_p + 
                inc_LP_MA2 + `gmv-2`, data = Multi_dist_log)

summary(multi_dist_model_5)

data.frame(var=names(vif(multi_dist_model_5)),vif=vif(multi_dist_model_5)) %>% arrange(desc(vif))


# elimination on the basis of p-value
# gmv-2, p-value=0.21866
multi_dist_model_6 <- lm(formula = gmv ~ promotional_discount + NPS + Mass_p + 
                           inc_LP_MA2 , data = Multi_dist_log)

summary(multi_dist_model_6)



# Adjusted R-squared:  0.996  with 4 significant variables

Multi_Dist_Final_Model_camera <- multi_dist_model_6

temp_crossval <- cv.lm(data = Multi_dist_log, form.lm = formula(gmv ~ promotional_discount + NPS + Mass_p +inc_LP_MA2 ),m = 10)

Cross_camera[5] <- attr(temp_crossval, "ms") # 0.0198


##############################################################################################
# Elasticity Analysis

train <- Multi_dist_log


grlm <- Multi_Dist_Final_Model_camera


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
  ggtitle("Camera Accessory - Multiplicative & Distributed Lag Model") +xlab("Variables")


##############################################################################################
##############################################################################################
