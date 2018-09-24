#####################################################################################################
############################# Model on Game Accessory Dataset#######################################
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

Cross_game<- rep(0,5)

# Building the Baisc Linear regression Model

Game_final <- read.csv("Game_final.csv", stringsAsFactors = F)

Linear_model <- Game_final

column_list <- c("SLA", "promotional_discount", "Procurement_SLA", "percentage_of_prepaid_order", "freebies_count", "NPS", "Premium_p", "Mass_p", "Middle_p", "holiday_freq", "gmv", "TV", "Digital", "Sponsorship", "Content.Marketing", "Online.marketing", "inc_LP_MA1", "inc_LP_MA2", "inc_LP_MA3", "inc_PO_MA1", "inc_PO_MA2", "inc_PO_MA3")

Linear_model <- Linear_model[,column_list]

Linear_model <- scale(Linear_model)

Linear_model <-data.frame(Linear_model)

model_1 <- lm(gmv~.,Linear_model)


################################################################################################

# Summary of Linear Model 
summary(model_1)


library(car)
library(MASS)

model_2 <- stepAIC(model_1,direction = "both")

summary(model_2) 

data.frame(var=names(vif(model_2)),vif=vif(model_2)) %>% arrange(desc(vif))



# removing TV (p-value=0.0839 , vif=4.48)
model_3 <- lm(formula = gmv ~ SLA + promotional_discount + Procurement_SLA + 
                percentage_of_prepaid_order + NPS + Premium_p + Mass_p + 
                Middle_p + Digital + Sponsorship + inc_PO_MA1, data = Linear_model)



summary(model_3) 

data.frame(var=names(vif(model_3)),vif=vif(model_3)) %>% arrange(desc(vif))

train_1 <- Linear_model[,c("NPS", "Digital", "Middle_p", "Mass_p", "Premium_p", "promotional_discount", "Procurement_SLA", "percentage_of_prepaid_order", "inc_PO_MA1", "SLA")]


cor(Linear_model$Sponsorship,train_1)

# removing Sponsorship

lm(formula = gmv ~ SLA + promotional_discount + Procurement_SLA + 
     percentage_of_prepaid_order + NPS + Premium_p + Mass_p + 
     Middle_p + Digital + inc_PO_MA1, data = Linear_model) %>% summary()
# Adjusted R-squared:  0.983 

# removing Digital

lm(formula = gmv ~ SLA + promotional_discount + Procurement_SLA + 
     percentage_of_prepaid_order + NPS + Premium_p + Mass_p + 
     Middle_p  + Sponsorship + inc_PO_MA1, data = Linear_model) %>% summary()
# Adjusted R-squared:  0.984


# removing Digital
model_4 <-  lm(formula = gmv ~ SLA + promotional_discount + Procurement_SLA + 
                 percentage_of_prepaid_order + NPS + Premium_p + Mass_p + 
                 Middle_p  + Sponsorship + inc_PO_MA1, data = Linear_model)


summary(model_4) 

data.frame(var=names(vif(model_4)),vif=vif(model_4)) %>% arrange(desc(vif))

# removing Sponsorship (p-value =0.07298, vif=2.69)

model_5 <- lm(formula = gmv ~ SLA + promotional_discount + Procurement_SLA + 
                percentage_of_prepaid_order + NPS + Premium_p + Mass_p + 
                Middle_p + inc_PO_MA1, data = Linear_model)



summary(model_5) 

data.frame(var=names(vif(model_5)),vif=vif(model_5)) %>% arrange(desc(vif))

# removing Procurement_SLA (p-value=0.06221, vif=2.5)

model_6 <- lm(formula = gmv ~ SLA + promotional_discount + 
                percentage_of_prepaid_order + NPS + Premium_p + Mass_p + 
                Middle_p + inc_PO_MA1, data = Linear_model)


summary(model_6) 

data.frame(var=names(vif(model_6)),vif=vif(model_6)) %>% arrange(desc(vif))

# removing NPS (p-value=0.1573, vif=2.21)

model_7 <- lm(formula = gmv ~ SLA + promotional_discount + 
                percentage_of_prepaid_order + Premium_p + Mass_p + 
                Middle_p + inc_PO_MA1, data = Linear_model)



summary(model_7)

data.frame(var=names(vif(model_7)),vif=vif(model_7)) %>% arrange(desc(vif))

train_7 <- Linear_model[,c("Mass_p", "Premium_p", "promotional_discount", "percentage_of_prepaid_order", "SLA", "inc_PO_MA1")]

cor(Linear_model$Middle_p, train_7)


#removing Middle_p
lm(formula = gmv ~ SLA + promotional_discount + 
     percentage_of_prepaid_order + Premium_p + Mass_p + 
     inc_PO_MA1, data = Linear_model) %>% summary()
#Adjusted R-squared:  0.968 

# removing Mass_p
lm(formula = gmv ~ SLA + promotional_discount + 
     percentage_of_prepaid_order + Premium_p + 
     Middle_p + inc_PO_MA1, data = Linear_model) %>% summary()
# Adjusted R-squared:  0.731 

#removing Middle_p

model_8 <- lm(formula = gmv ~ SLA + promotional_discount + 
                percentage_of_prepaid_order + Premium_p + Mass_p + 
                inc_PO_MA1, data = Linear_model)


summary(model_8)

data.frame(var=names(vif(model_8)),vif=vif(model_8)) %>% arrange(desc(vif))
# no vif below 2

# removing promotional_discount
model_9 <- lm(formula = gmv ~ SLA + percentage_of_prepaid_order + Premium_p + Mass_p + 
                inc_PO_MA1, data = Linear_model)

summary(model_9)

data.frame(var=names(vif(model_9)),vif=vif(model_9)) %>% arrange(desc(vif))

# removing percentage_of_prepaid_order( p-valu=0.619)
model_10 <- lm(formula = gmv ~ SLA + Premium_p + Mass_p + 
                 inc_PO_MA1, data = Linear_model)


summary(model_10)

# removing SLA
model_11 <- lm(formula = gmv ~ Premium_p + Mass_p + 
                 inc_PO_MA1, data = Linear_model)


summary(model_11)

##Final Model 
Linear_Final_model_game <- model_11

# Adj R square  = 0.967  with 3 variables

temp_crossval <- cv.lm(data = Linear_model, form.lm = formula(gmv ~ Premium_p + Mass_p +inc_PO_MA1),m = 10)

Cross_game[1] <- attr(temp_crossval, "ms")
# ms= 0.0349

#######################################################################################

# Elasticity Analysis

train <- Linear_model


grlm <- Linear_Final_model_game


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
  ggtitle("Game Accessory - Linear Model") +xlab("Variables")

#####################################################################################################################
#####################################################################################################################
########################################## Multiplicative Model: Game Accessory #####################################################

Cross_game<- rep(0,5)

library(ggplot2)

Game_final <- read.csv("Game_final.csv")

Game_multiplicative_ds <- Game_final

column_list <- c("SLA", "promotional_discount", "Procurement_SLA", "percentage_of_prepaid_order", "freebies_count", "NPS", "Premium_p", "Mass_p", "Middle_p", "holiday_freq", "gmv", "TV", "Digital", "Sponsorship", "Content.Marketing", "Online.marketing", "inc_LP_MA1", "inc_LP_MA2", "inc_LP_MA3", "inc_PO_MA1", "inc_PO_MA2", "inc_PO_MA3")

Game_multiplicative_ds <- Game_multiplicative_ds[,column_list]

# Checking number of zeros or less in each variable

sapply(Game_multiplicative_ds, function(x) { sum(x <=0)})

#SLA        promotional_discount             Procurement_SLA 
#0                           0                           0 
#percentage_of_prepaid_order              freebies_count                         NPS 
#2                          34                           0 
#Premium_p                      Mass_p                    Middle_p 
#4                           1                           1 
#holiday_freq                         gmv                          TV 
#34                           0                           0 
#Digital                 Sponsorship           Content.Marketing 
#0                           0                           0 
#Online.marketing                  inc_LP_MA1                  inc_LP_MA2 
#0                           0                           0 
#inc_LP_MA3                  inc_PO_MA1                  inc_PO_MA2 
#30                          30                          20 
#inc_PO_MA3 
#24 

# lets remove the variables having more than 10 zeros and for the variables having less than 10, will substitute zeros with 
# a small value(0.01)

# Treatment

Game_multiplicative_ds$percentage_of_prepaid_order[which(Game_multiplicative_ds$percentage_of_prepaid_order==0)] <- 0.01
Game_multiplicative_ds$Premium_p[which(Game_multiplicative_ds$Premium_p==0)] <- 0.01
Game_multiplicative_ds$Mass_p[which(Game_multiplicative_ds$Mass_p==0)] <- 0.01
Game_multiplicative_ds$Middle_p[which(Game_multiplicative_ds$Middle_p==0)] <- 0.01

#c("freebies_count","Premium_p","holiday_freq","inc_LP_MA3","inc_PO_MA1","inc_PO_MA3","inc_PO_MA2")


Game_multiplicative_ds <- Game_multiplicative_ds[,!(names(Game_multiplicative_ds) %in% c("freebies_count","holiday_freq","inc_LP_MA3","inc_PO_MA1","inc_PO_MA3","inc_PO_MA2"))]

Game_multiplicative_ds_log <- log(Game_multiplicative_ds)

################################################
#################################################

## Build the First model ##

multi_model <- lm(gmv~.,Game_multiplicative_ds_log)

summary(multi_model)

library(dplyr)
library(car)
library(MASS)

multi_model_2 <- stepAIC(multi_model,direction = "both")

summary(multi_model_2)

data.frame(var=names(vif(multi_model_2)),vif=vif(multi_model_2)) %>% arrange(desc(vif))

#TV (VIF=95.193627, p-value=0.125621)

multi_model_3 <- lm(formula = gmv ~ Online.marketing + inc_LP_MA1 + Content.Marketing + 
                      Mass_p + Middle_p + Premium_p + Sponsorship +
                      percentage_of_prepaid_order + Digital + inc_LP_MA2 +
                      promotional_discount + SLA, data = Game_multiplicative_ds_log)

summary(multi_model_3) 

data.frame(var=names(vif(multi_model_3)),vif=vif(multi_model_3)) %>% arrange(desc(vif))

cor(Game_multiplicative_ds_log[,"Online.marketing"],Game_multiplicative_ds_log[,c("inc_LP_MA1", "Content.Marketing", "Mass_p", "Premium_p", "Middle_p", "inc_LP_MA2", "Digital", "Sponsorship", "percentage_of_prepaid_order", "promotional_discount", "SLA")] )

lm(formula = gmv ~ inc_LP_MA1 + Content.Marketing + 
     Mass_p + Middle_p + Premium_p + Sponsorship +
     percentage_of_prepaid_order + Digital + inc_LP_MA2 +
     promotional_discount + SLA, data = Game_multiplicative_ds_log) %>% summary()

#Adjusted R-squared:  0.9966

lm(formula = gmv ~ Content.Marketing + Online.marketing +
     Mass_p + Middle_p + Premium_p + Sponsorship +
     percentage_of_prepaid_order + Digital + inc_LP_MA2 +
     promotional_discount + SLA, data = Game_multiplicative_ds_log) %>% summary()

#Adjusted R-squared:  0.9968

multi_model_4 <- lm(formula = gmv ~ Content.Marketing + Online.marketing +
                      Mass_p + Middle_p + Premium_p + Sponsorship +
                      percentage_of_prepaid_order + Digital + inc_LP_MA2 +
                      promotional_discount + SLA, data = Game_multiplicative_ds_log)

summary(multi_model_4) 

data.frame(var=names(vif(multi_model_4)),vif=vif(multi_model_4)) %>% arrange(desc(vif))

#Content.Marketing (VIF=30.008302, p-value=0.09693)

multi_model_5 <- lm(formula = gmv ~ Online.marketing +
                      Mass_p + Middle_p + Premium_p + Sponsorship +
                      percentage_of_prepaid_order + Digital + inc_LP_MA2 +
                      promotional_discount + SLA, data = Game_multiplicative_ds_log)

summary(multi_model_5)

data.frame(var=names(vif(multi_model_5)),vif=vif(multi_model_5)) %>% arrange(desc(vif))

#inc_LP_MA2 (VIF=7.812723, p-value=0.07395)

multi_model_6 <- lm(formula = gmv ~ Online.marketing +
                      Mass_p + Middle_p + Premium_p + Sponsorship +
                      percentage_of_prepaid_order + Digital +
                      promotional_discount + SLA, data = Game_multiplicative_ds_log)

summary(multi_model_6)

data.frame(var=names(vif(multi_model_6)),vif=vif(multi_model_6)) %>% arrange(desc(vif))

#Sponsorship (VIF=5.272848, p-value=0.0909)

multi_model_7 <- lm(formula = gmv ~ Online.marketing +
                      Mass_p + Middle_p + Premium_p +
                      percentage_of_prepaid_order + Digital +
                      promotional_discount + SLA, data = Game_multiplicative_ds_log)

summary(multi_model_7)

data.frame(var=names(vif(multi_model_7)),vif=vif(multi_model_7)) %>% arrange(desc(vif))

#Online.marketing (VIF=5.997154, p-value=0.1465)

multi_model_8 <- lm(formula = gmv ~ Mass_p + Middle_p + Premium_p +
                      percentage_of_prepaid_order + Digital +
                      promotional_discount + SLA, data = Game_multiplicative_ds_log)

summary(multi_model_8)

data.frame(var=names(vif(multi_model_8)),vif=vif(multi_model_8)) %>% arrange(desc(vif))

#removing on the basis of p-value
#percentage_of_prepaid_order (p-value=0.95436)

multi_model_9 <- lm(formula = gmv ~ Mass_p + Middle_p + Premium_p +
                      Digital +
                      promotional_discount + SLA, data = Game_multiplicative_ds_log)

summary(multi_model_9)

data.frame(var=names(vif(multi_model_9)),vif=vif(multi_model_9)) %>% arrange(desc(vif))

#SLA (p-value=0.315)

multi_model_10 <- lm(formula = gmv ~ Mass_p + Middle_p + Premium_p +
                      Digital + promotional_discount, data = Game_multiplicative_ds_log)

summary(multi_model_10)

data.frame(var=names(vif(multi_model_10)),vif=vif(multi_model_10)) %>% arrange(desc(vif))

#Digital (p-value=0.09669)

multi_model_11 <- lm(formula = gmv ~ Mass_p + Middle_p + Premium_p +
                      promotional_discount, data = Game_multiplicative_ds_log)

summary(multi_model_11)

data.frame(var=names(vif(multi_model_11)),vif=vif(multi_model_11)) %>% arrange(desc(vif))

# Adjusted R-squared:  0.996

multi_final_model_game <- multi_model_11

#temp_crossval <- cv.lm(data = Game_multiplicative_ds, form.lm = formula(gmv ~ Mass_p + Middle_p + Premium_p + promotional_discount),m = 10)
#Cross_game[2] <- attr(temp_crossval, "ms")

##########################################################################################
# Elasticity Analysis

train <- Game_multiplicative_ds

grlm <- multi_final_model_game

# estimating the elasticity coefficients

elasticity <- function(var){
  
  elax1 <-as.numeric(multi_final_model_game$coefficients[var]*mean(train[,var])/mean(train$gmv))
  
  return(elax1)
} 

var_list <- list()

for(i in 2:length(multi_final_model_game$coefficients)){
  
  var_list[i-1] <-elasticity(names(multi_final_model_game$coefficients)[i])
  
}

elasticity.outputs <- data.frame(names(multi_final_model_game$coefficients[2:length(multi_final_model_game$coefficients)]))
elasticity.outputs <- cbind(elasticity.outputs,do.call(rbind.data.frame, var_list))
colnames(elasticity.outputs) <- c("Variable","Elasticity")

elasticity.outputs$direction <- ifelse(elasticity.outputs$Elasticity > 0, "Positive", "Negative")

ggplot(data=elasticity.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Game - Multiplicative Model") +xlab("Variables")

#######################################################################################################################
#######################################################################################################################
############################################## Koyck models #################################################

Game_koyck<- Game_final[,c("SLA", "promotional_discount", "Procurement_SLA", "percentage_of_prepaid_order", "freebies_count", "NPS", "Premium_p", "Mass_p", "Middle_p", "holiday_freq", "gmv", "TV", "Digital", "Sponsorship", "Content.Marketing", "Online.marketing", "inc_LP_MA1", "inc_LP_MA2", "inc_LP_MA3", "inc_PO_MA1", "inc_PO_MA2", "inc_PO_MA3")]

library(DataCombine)

# gmv 1 lag
Game_koyck <- slide(Game_koyck, Var = "gmv",slideBy = 1)

Game_koyck <- na.omit(Game_koyck)

Game_koyck <- scale(Game_koyck)

Game_koyck <- data.frame(Game_koyck)

# Build first model
koyck_model <- lm(gmv1~.,Game_koyck)

summary(koyck_model)

koyck_model_2 <- stepAIC(koyck_model,direction = "both")

summary(koyck_model_2)  
data.frame(var=names(vif(koyck_model_2)),vif=vif(koyck_model_2)) %>% arrange(desc(vif))

# Mass_p  vif=37.632064, p-value= 0.05487)

koyck_model_3 <- lm(formula = gmv1 ~ SLA + promotional_discount + Procurement_SLA + 
                      percentage_of_prepaid_order + NPS  + Middle_p + gmv + 
                      Content.Marketing + Online.marketing + inc_LP_MA1 + inc_LP_MA2 + 
                      inc_LP_MA3 + inc_PO_MA1 + inc_PO_MA2 + inc_PO_MA3, data = Game_koyck)
summary(koyck_model_3)  
data.frame(var=names(vif(koyck_model_3)),vif=vif(koyck_model_3)) %>% arrange(desc(vif))

# Online.marketing  vif=448.609313, p-value= 0.1104)

koyck_model_4 <- lm(formula = gmv1 ~ SLA + promotional_discount + Procurement_SLA + 
                      percentage_of_prepaid_order + NPS  + Middle_p + gmv + 
                      Content.Marketing + inc_LP_MA1 + inc_LP_MA2 + 
                      inc_LP_MA3 + inc_PO_MA1 + inc_PO_MA2 + inc_PO_MA3, data = Game_koyck)
summary(koyck_model_4)  
data.frame(var=names(vif(koyck_model_4)),vif=vif(koyck_model_4)) %>% arrange(desc(vif))

# NPS  vif=16.658647, p-value= 0.09492)

koyck_model_5 <- lm(formula = gmv1 ~ SLA + promotional_discount + Procurement_SLA + 
                      percentage_of_prepaid_order  + Middle_p + gmv + 
                      Content.Marketing + inc_LP_MA1 + inc_LP_MA2 + 
                      inc_LP_MA3 + inc_PO_MA1 + inc_PO_MA2 + inc_PO_MA3, data = Game_koyck)
summary(koyck_model_5)  
data.frame(var=names(vif(koyck_model_5)),vif=vif(koyck_model_5)) %>% arrange(desc(vif))

# inc_LP_MA3 vif=10.389655, p-value= 0.6803

koyck_model_6 <- lm(formula = gmv1 ~ SLA + promotional_discount + Procurement_SLA + 
                      percentage_of_prepaid_order  + Middle_p + gmv + 
                      Content.Marketing + inc_LP_MA1 + inc_LP_MA2 + 
                      inc_PO_MA1 + inc_PO_MA2 + inc_PO_MA3, data = Game_koyck)
summary(koyck_model_6)  
data.frame(var=names(vif(koyck_model_6)),vif=vif(koyck_model_6)) %>% arrange(desc(vif))

# Middle_p vif=6.765733, p-value= 0.6497

koyck_model_7 <- lm(formula = gmv1 ~ SLA + promotional_discount + Procurement_SLA + 
                      percentage_of_prepaid_order + gmv + 
                      Content.Marketing + inc_LP_MA1 + inc_LP_MA2 + 
                      inc_PO_MA1 + inc_PO_MA2 + inc_PO_MA3, data = Game_koyck)
summary(koyck_model_7)  
data.frame(var=names(vif(koyck_model_7)),vif=vif(koyck_model_7)) %>% arrange(desc(vif))




# Content.Marketing and inc_LP_MA2 have high VIF and non of them are in-significant (pvalue > 0.05)

column_list_7 <- c("inc_LP_MA2", "inc_PO_MA3", "inc_PO_MA2", "inc_LP_MA1", "inc_PO_MA1", "promotional_discount", "gmv", "Procurement_SLA", "percentage_of_prepaid_order", "SLA")

cor(Game_koyck[,"Content.Marketing"], Game_koyck[,column_list_7])


# inc_LP_MA2 inc_PO_MA3  inc_PO_MA2 inc_LP_MA1  inc_PO_MA1 promotional_discount       gmv Procurement_SLA
# [1,]  0.9583354 -0.1066045 -0.07012555  0.4697853 -0.04612909          -0.08179083 0.2593742       0.2116323
# percentage_of_prepaid_order        SLA
# [1,]                  -0.2602417 0.01939725


# inc_LP_MA2
lm(formula = gmv1 ~ SLA + promotional_discount + Procurement_SLA + 
     percentage_of_prepaid_order + gmv + 
     Content.Marketing + inc_LP_MA1  + 
     inc_PO_MA1 + inc_PO_MA2 + inc_PO_MA3, data = Game_koyck) %>% summary()
# Adjusted R-squared:  0.3202 

# Content.Marketing
lm(formula = gmv1 ~ SLA + promotional_discount + Procurement_SLA + 
     percentage_of_prepaid_order + gmv + 
      inc_LP_MA1 + inc_LP_MA2 + 
     inc_PO_MA1 + inc_PO_MA2 + inc_PO_MA3, data = Game_koyck) %>% summary()
# Adjusted R-squared:  0.3204

# for both the models, adj-sq is almost same, will drop inc_LP_MA2

koyck_model_8 <- lm(formula = gmv1 ~ SLA + promotional_discount + Procurement_SLA + 
                      percentage_of_prepaid_order + gmv + 
                      Content.Marketing + inc_LP_MA1  + 
                      inc_PO_MA1 + inc_PO_MA2 + inc_PO_MA3, data = Game_koyck)
summary(koyck_model_8)  
data.frame(var=names(vif(koyck_model_8)),vif=vif(koyck_model_8)) %>% arrange(desc(vif))


# all the VIF values are below 5, so will start droping variables on the basis of p-value

# percentage_of_prepaid_order (p-value=0.9089)

koyck_model_9 <- lm(formula = gmv1 ~ SLA + promotional_discount + Procurement_SLA + gmv + 
                      Content.Marketing + inc_LP_MA1  + 
                      inc_PO_MA1 + inc_PO_MA2 + inc_PO_MA3, data = Game_koyck)
summary(koyck_model_9) 

# Content.Marketing   (p-value=0.8359)

koyck_model_10 <- lm(formula = gmv1 ~ SLA + promotional_discount + Procurement_SLA + gmv + inc_LP_MA1  + 
                      inc_PO_MA1 + inc_PO_MA2 + inc_PO_MA3, data = Game_koyck)
summary(koyck_model_10) 



# inc_PO_MA1 p-value= 0.69376  
koyck_model_11 <- lm(formula = gmv1 ~ SLA + promotional_discount + Procurement_SLA + gmv + inc_LP_MA1  + 
                       inc_PO_MA2 + inc_PO_MA3, data = Game_koyck)
summary(koyck_model_11) 


# inc_PO_MA3 (p-value=0.58209)
koyck_model_12 <- lm(formula = gmv1 ~ SLA + promotional_discount + Procurement_SLA + gmv + inc_LP_MA1  + 
                       inc_PO_MA2, data = Game_koyck)
summary(koyck_model_12)

# gmv (p-value= 0.58759)

koyck_model_13 <- lm(formula = gmv1 ~ SLA + promotional_discount + Procurement_SLA  + inc_LP_MA1  + 
                       inc_PO_MA2, data = Game_koyck)
summary(koyck_model_13)

#Procurement_SLA (p-value= 0.174203)

koyck_model_14 <- lm(formula = gmv1 ~ SLA + promotional_discount + inc_LP_MA1  + 
                       inc_PO_MA2, data = Game_koyck)
summary(koyck_model_14)

# promotional_discount (p-value 0.330019)

koyck_model_15 <- lm(formula = gmv1 ~ SLA + inc_LP_MA1  + inc_PO_MA2, data = Game_koyck)
summary(koyck_model_15)



##Final Model 
koyck_Final_Model_game <- koyck_model_15 

# Adjusted R-squared:  0.496 with 4 significant variable 

temp_crossval <- cv.lm(data = Game_koyck, form.lm = formula(gmv1 ~ SLA + inc_LP_MA1  + inc_PO_MA2 ),m = 10)

Cross_game[3] <- attr(temp_crossval, "ms") # 0.67

#######################################################################################

# Elasticity Analysis

train <- Game_koyck


grlm <- koyck_Final_Model_game


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
  ggtitle("Game Accessory - Koyck Model") +xlab("Variables")


#######################################################################################################################
#######################################################################################################################
############################################## Distributed lag models #################################################

column_list <- c("SLA", "promotional_discount", "Procurement_SLA", "percentage_of_prepaid_order", "freebies_count", "NPS", "Premium_p", "Mass_p", "Middle_p", "holiday_freq", "gmv", "TV", "Digital", "Sponsorship", "Content.Marketing", "Online.marketing", "inc_LP_MA1", "inc_LP_MA2", "inc_LP_MA3", "inc_PO_MA1", "inc_PO_MA2", "inc_PO_MA3")

Dist_Model_ds <- Game_final[,column_list]

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

# Online.marketing  414.995531 , p-value=0.059216

dist_model_3 <- lm(formula = gmv ~ SLA + promotional_discount + Procurement_SLA + 
                     NPS + Premium_p + Mass_p + Middle_p + Digital + Sponsorship + 
                    inc_LP_MA1 + inc_LP_MA3 + inc_PO_MA1 + 
                     gmv.1, data = Dist_Model_ds)


summary(dist_model_3)


data.frame(var=names(vif(dist_model_3)),vif=vif(dist_model_3)) %>% arrange(desc(vif))

# inc_LP_MA3 vif=11.480742 p-value=0.277393

dist_model_4 <- lm(formula = gmv ~ SLA + promotional_discount + Procurement_SLA + 
                     NPS + Premium_p + Mass_p + Middle_p + Digital + Sponsorship + 
                     inc_LP_MA1 + inc_PO_MA1 + gmv.1, data = Dist_Model_ds)


summary(dist_model_4)

data.frame(var=names(vif(dist_model_4)),vif=vif(dist_model_4)) %>% arrange(desc(vif))

# Sponsorship, NPS and Middle_p have high VIF and non of them are in-significant (pvalue > 0.05)

column_list_5 <- c("NPS", "Middle_p", "Digital", "inc_LP_MA1", "Mass_p", "promotional_discount", "Procurement_SLA", "Premium_p", "gmv.1", "inc_PO_MA1", "SLA")

cor(Dist_Model_ds[,"Sponsorship"], Dist_Model_ds[,column_list_5])

#NPS   Middle_p   Digital inc_LP_MA1   Mass_p promotional_discount Procurement_SLA  Premium_p     gmv.1 inc_PO_MA1
#[1,] -0.6514302 0.08617897 0.7496917  0.2822646 0.462802           -0.3077262       0.2690982 0.06319351 0.4329503 -0.2876554
#SLA
#[1,] 0.07201623

# Sponsorship
lm(formula = gmv ~ SLA + promotional_discount + Procurement_SLA + 
     NPS + Premium_p + Mass_p + Middle_p + Digital + 
     inc_LP_MA1 + inc_PO_MA1 + gmv.1, data = Dist_Model_ds) %>% summary()
# Adjusted R-squared:  0.9868 

# Middle_p
lm(formula = gmv ~ SLA + promotional_discount + Procurement_SLA + 
     NPS + Premium_p + Mass_p + Digital + Sponsorship + 
     inc_LP_MA1 + inc_PO_MA1 + gmv.1, data = Dist_Model_ds) %>% summary()
# Adjusted R-squared:  0.9765


#dropping Sponsorship

dist_model_5 <- lm(formula = gmv ~ SLA + promotional_discount + Procurement_SLA + 
                     NPS + Premium_p + Mass_p + Middle_p + Digital + 
                     inc_LP_MA1 + inc_PO_MA1 + gmv.1, data = Dist_Model_ds)

summary(dist_model_5)

data.frame(var=names(vif(dist_model_5)),vif=vif(dist_model_5)) %>% arrange(desc(vif))

# Removing on the basisof p=value
# Removing inc_PO_MA1 (p-value=0.094043)

dist_model_6 <- lm(formula = gmv ~ SLA + promotional_discount + Procurement_SLA + 
                     NPS + Premium_p + Mass_p + Middle_p + Digital + 
                     inc_LP_MA1 + gmv.1, data = Dist_Model_ds)

summary(dist_model_6)

data.frame(var=names(vif(dist_model_6)),vif=vif(dist_model_6)) %>% arrange(desc(vif))

# Removing Digital (p-value=0.094043)
dist_model_7 <- lm(formula = gmv ~ SLA + promotional_discount + Procurement_SLA + 
                     NPS + Premium_p + Mass_p + Middle_p + 
                     inc_LP_MA1 + gmv.1, data = Dist_Model_ds)

summary(dist_model_7)

data.frame(var=names(vif(dist_model_7)),vif=vif(dist_model_7)) %>% arrange(desc(vif))

# Removing inc_LP_MA1 (p-value=0.206784)
dist_model_8 <- lm(formula = gmv ~ SLA + promotional_discount + Procurement_SLA + 
                     NPS + Premium_p + Mass_p + Middle_p + 
                    gmv.1, data = Dist_Model_ds)

summary(dist_model_8)

data.frame(var=names(vif(dist_model_8)),vif=vif(dist_model_8)) %>% arrange(desc(vif))

# Removing Procurement_SLA (p-value=0.045928)
dist_model_9 <- lm(formula = gmv ~ SLA + promotional_discount + 
                     NPS + Premium_p + Mass_p + Middle_p + 
                     gmv.1, data = Dist_Model_ds)

summary(dist_model_9)

data.frame(var=names(vif(dist_model_9)),vif=vif(dist_model_9)) %>% arrange(desc(vif))

##Final Model 

Dist_Final_Model_game <- dist_model_9

# Adjusted R-squared:  0.985 with 7 significant variables

#temp_crossval <- cv.lm(data = Dist_Model_ds, form.lm = formula(gmv ~ SLA + promotional_discount + NPS + Premium_p + Mass_p + Middle_p + gmv.1),m = 10)

#Cross_game[4] <- attr(temp_crossval, "ms")


#################################################################
# Elasticity Analysis

train <- Dist_Model_ds

grlm <- Dist_Final_Model_game

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
  ggtitle("Game Accessory - Distributed Lag Model") +xlab("Variables")


############################################################
# Multiplicative + distributed model: 

column_list <- c("SLA", "promotional_discount", "Procurement_SLA", "percentage_of_prepaid_order", "freebies_count", "NPS", "Premium_p", "Mass_p", "Middle_p", "holiday_freq", "gmv", "TV", "Digital", "Sponsorship", "Content.Marketing", "Online.marketing", "inc_LP_MA1", "inc_LP_MA2", "inc_LP_MA3", "inc_PO_MA1", "inc_PO_MA2", "inc_PO_MA3")

Multi_Dist_ds <- Game_final[,column_list]

# gmv

Multi_Dist_ds_1 <- slide(Multi_Dist_ds, Var = "gmv",slideBy = -1)

Multi_Dist_ds_1 <- slide(Multi_Dist_ds_1, Var = "gmv",slideBy = -2)

Multi_Dist_ds_1 <- slide(Multi_Dist_ds_1, Var = "gmv",slideBy = -3)

Multi_Dist_ds <- na.omit(Multi_Dist_ds_1)



# Checking number of zeros or less in each variable

sapply(Multi_Dist_ds, function(x) { sum(x <=0)})

#SLA        promotional_discount             Procurement_SLA percentage_of_prepaid_order 
#0                           0                           0                           2 
#freebies_count                         NPS                   Premium_p                      Mass_p 
#31                           0                           3                           1 
#Middle_p                holiday_freq                         gmv                          TV 
#1                          31                           0                           0 
#Digital                 Sponsorship           Content.Marketing            Online.marketing 
#0                           0                           0                           0 
#inc_LP_MA1                  inc_LP_MA2                  inc_LP_MA3                  inc_PO_MA1 
#0                           0                          27                          27 
#inc_PO_MA2                  inc_PO_MA3                       gmv-1                       gmv-2 
#19                          23                           0                           0 
#gmv-3 
#0 

# lets remove the variables having more than 10 zeros and for the variables having less than 10, will substitute zeros with 
# a small value(0.01)

# Treatment

Multi_Dist_ds$percentage_of_prepaid_order[which(Multi_Dist_ds$percentage_of_prepaid_order==0)] <- 0.01
Multi_Dist_ds$Premium_p[which(Multi_Dist_ds$Premium_p==0)] <- 0.01
Multi_Dist_ds$Mass_p[which(Multi_Dist_ds$Mass_p==0)] <- 0.01
Multi_Dist_ds$Middle_p[which(Multi_Dist_ds$Middle_p==0)] <- 0.01

Multi_Dist_ds <- Multi_Dist_ds[,!(names(Multi_Dist_ds) %in% c("freebies_count","holiday_freq","inc_LP_MA3","inc_PO_MA1","inc_PO_MA3","inc_PO_MA2"))]

########################################################

Multi_dist_log <- log(Multi_Dist_ds)

multi_dist_model_1 <- lm(gmv~., Multi_dist_log)

summary(multi_dist_model_1)

multi_dist_model_2 <- stepAIC(multi_dist_model_1,direction = "both")

summary(multi_dist_model_2) 

data.frame(var=names(vif(multi_dist_model_2)),vif=vif(multi_dist_model_2)) %>% arrange(desc(vif))

#Procurement_SLA vif = 7.546099 p-value = 0.091517

multi_dist_model_3 <- lm(formula = gmv ~ promotional_discount + percentage_of_prepaid_order + 
                           Premium_p + Mass_p + Middle_p + TV + Digital + Sponsorship + 
                           Content.Marketing + Online.marketing + inc_LP_MA1 + inc_LP_MA2, 
                         data = Multi_dist_log)

summary(multi_dist_model_3)

data.frame(var=names(vif(multi_dist_model_3)),vif=vif(multi_dist_model_3)) %>% arrange(desc(vif))

#TV vif = 79.451053 p-value = 0.052503

multi_dist_model_4 <- lm(formula = gmv ~ promotional_discount + percentage_of_prepaid_order + Premium_p + Mass_p + 
                           Middle_p + Digital + Sponsorship + Content.Marketing + Online.marketing +
                           inc_LP_MA1 + inc_LP_MA2, data = Multi_dist_log)

summary(multi_dist_model_4)

data.frame(var=names(vif(multi_dist_model_4)),vif=vif(multi_dist_model_4)) %>% arrange(desc(vif))

# Most of the variables have high VIF but non of them are insignificant

column_list_5 <- c("inc_LP_MA1", "Content.Marketing", "Premium_p", "Mass_p", "inc_LP_MA2", "Digital", "Sponsorship", "Middle_p", "percentage_of_prepaid_order", "promotional_discount")

cor(Multi_dist_log[,"Online.marketing"], Multi_dist_log[,column_list_5])

#inc_LP_MA1 Content.Marketing Premium_p    Mass_p inc_LP_MA2  Digital Sponsorship  Middle_p percentage_of_prepaid_order
#[1,]  0.9987345         0.9579058 0.9094674 0.8330584  0.4933138 0.156885   0.7271124 0.9152638                   0.7111567
#promotional_discount
#[1,]           -0.3060972

# Online.marketing
lm(formula = gmv ~ promotional_discount + percentage_of_prepaid_order + Premium_p + Mass_p + 
     Middle_p + Digital + Sponsorship + Content.Marketing +
     inc_LP_MA1 + inc_LP_MA2, data = Multi_dist_log) %>% summary()
# Adjusted R-squared:  0.9986 

# inc_LP_MA1
lm(formula = gmv ~ promotional_discount + percentage_of_prepaid_order + Premium_p + Mass_p + 
     Middle_p + Digital + Sponsorship + Content.Marketing + Online.marketing +
      inc_LP_MA2, data = Multi_dist_log) %>% summary()
# Adjusted R-squared:  0.9987

# since adjusted R-squared for both are almost same, dropping Online.marketing.

multi_dist_model_5 <- lm(formula = gmv ~ promotional_discount + percentage_of_prepaid_order + Premium_p + Mass_p + 
                           Middle_p + Digital + Sponsorship + Content.Marketing +
                           inc_LP_MA1 + inc_LP_MA2, data = Multi_dist_log)

summary(multi_dist_model_5)

data.frame(var=names(vif(multi_dist_model_5)),vif=vif(multi_dist_model_5)) %>% arrange(desc(vif))

# Premium_p (VIF = 37.745230, p-value= 0.14739)

multi_dist_model_6 <- lm(formula = gmv ~ promotional_discount + percentage_of_prepaid_order + Mass_p + 
                           Middle_p + Digital + Sponsorship + Content.Marketing +
                           inc_LP_MA1 + inc_LP_MA2, data = Multi_dist_log)

summary(multi_dist_model_6)

data.frame(var=names(vif(multi_dist_model_6)),vif=vif(multi_dist_model_6)) %>% arrange(desc(vif))

# inc_LP_MA2 (VIF = 9.752870, p-value= 0.962699)

multi_dist_model_7 <- lm(formula = gmv ~ promotional_discount + percentage_of_prepaid_order + Mass_p + 
                           Middle_p + Digital + Sponsorship + Content.Marketing +
                           inc_LP_MA1, data = Multi_dist_log)

summary(multi_dist_model_7)

data.frame(var=names(vif(multi_dist_model_7)),vif=vif(multi_dist_model_7)) %>% arrange(desc(vif))

# some of the variables have high VIF but non of them are insignificant

column_list_8 <- c("Mass_p", "Content.Marketing", "Middle_p", "percentage_of_prepaid_order", "Sponsorship", "Digital", "promotional_discount")

cor(Multi_dist_log[,"inc_LP_MA1"], Multi_dist_log[,column_list_8])

#Mass_p Content.Marketing  Middle_p percentage_of_prepaid_order Sponsorship   Digital promotional_discount
#[1,] 0.8317732         0.9461042 0.9140594                   0.7088505     0.70752 0.1435988           -0.3078638


# inc_LP_MA1
lm(formula = gmv ~ promotional_discount + percentage_of_prepaid_order + Mass_p + 
     Middle_p + Digital + Sponsorship + Content.Marketing, data = Multi_dist_log) %>% summary()
# Adjusted R-squared:  0.9974 

# Content.Marketing
lm(formula = gmv ~ promotional_discount + percentage_of_prepaid_order + Mass_p + 
     Middle_p + Digital + Sponsorship + inc_LP_MA1, data = Multi_dist_log) %>% summary()
# Adjusted R-squared:  0.9978

# since adjusted R-squared for both are almost same, dropping inc_LP_MA1.

multi_dist_model_8 <- lm(formula = gmv ~ promotional_discount + percentage_of_prepaid_order + Mass_p + 
                           Middle_p + Digital + Sponsorship + Content.Marketing, data = Multi_dist_log)

summary(multi_dist_model_8)

data.frame(var=names(vif(multi_dist_model_8)),vif=vif(multi_dist_model_8)) %>% arrange(desc(vif))

# Content.Marketing (VIF = 7.935671, P-value = 0.33867)

multi_dist_model_9 <- lm(formula = gmv ~ promotional_discount + percentage_of_prepaid_order + Mass_p + 
                           Middle_p + Digital + Sponsorship, data = Multi_dist_log)

summary(multi_dist_model_9)

data.frame(var=names(vif(multi_dist_model_9)),vif=vif(multi_dist_model_9)) %>% arrange(desc(vif))

# some of the variables have high VIF but non of them are insignificant

column_list_10 <- c("Middle_p", "Sponsorship", "percentage_of_prepaid_order", "promotional_discount", "Digital")

cor(Multi_dist_log[,"Mass_p"], Multi_dist_log[,column_list_10])

#Middle_p Sponsorship percentage_of_prepaid_order promotional_discount    Digital
#[1,] 0.8404431   0.6638547                   0.3442006           -0.6259121 0.09233405

#dropping Mass_p

multi_dist_model_10 <- lm(formula = gmv ~ promotional_discount + percentage_of_prepaid_order + 
                           Middle_p + Digital + Sponsorship, data = Multi_dist_log)

summary(multi_dist_model_10)

data.frame(var=names(vif(multi_dist_model_10)),vif=vif(multi_dist_model_10)) %>% arrange(desc(vif))

# Adjusted R-squared:  0.968  with 5 significant variables

Multi_Dist_Final_Model_game <- multi_dist_model_10

temp_crossval <- cv.lm(data = Multi_dist_log, form.lm = formula(gmv ~ promotional_discount + percentage_of_prepaid_order + 
                                                                  Middle_p + Digital + Sponsorship ),m = 10)

Cross_game[5] <- attr(temp_crossval, "ms") # 0.0198


##############################################################################################
# Elasticity Analysis

train <- Multi_dist_log


grlm <- Multi_Dist_Final_Model_game


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
  ggtitle("Game Accessory - Multiplicative & Distributed Lag Model") +xlab("Variables")


##############################################################################################
##############################################################################################

