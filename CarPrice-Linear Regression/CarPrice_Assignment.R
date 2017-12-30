# loading the data
carprice_master <- read.csv("CarPrice_Assignment.csv" , stringsAsFactors = F)

carprice <- carprice_master


# loading required package
library(stringr)
library(dplyr)
library(ggplot2)
library(MASS)
library(car)


# checking Structure of the dataset
str(carprice)
head(carprice$CarName)


# we will be only considering maker (company) name of cars 
# so , extracting company from CarName 
carprice$company <- str_split_fixed(carprice$CarName," ",2)[,1]
carprice <- carprice[,!(names(carprice) %in% "CarName")]

# checking duplicates
nrow(unique(carprice[,-1])) == nrow(carprice)

# checking if there is any NA value in the dataset.
sum(is.na(carprice))


# symboling assigned insurance risk rating, A value of +3 indicates that the auto is risky, 
# -3 that it is probably pretty safe.
unique(carprice$symboling)
summary(as.factor(carprice$symboling))

# This dataset have -2,-1,0,1,2,3 values.
# dividing symboling into three groups.
# (-2,-1) -- very_safe
# (0,1) -- safe
# (2,3) -- risky
carprice$symboling <- as.factor(carprice$symboling)
levels(carprice$symboling)

levels(carprice$symboling)[1:2] <- "very_safe"
levels(carprice$symboling)
levels(carprice$symboling)[2:3] <- "safe"
levels(carprice$symboling)
levels(carprice$symboling)[3:4] <- "risky"
levels(carprice$symboling)




#####################
# in normal case , larger engine size leads to greater horse power , 
# however if smaller engine has greter horse power , implies that good technology is used , 
# thus there are good chances that will have more price.
# so lets create a valiable which will measure horse power with respect to size of engine
# ratio_hp_enginesize = horsepower/enginesize

ratio_hp_enginesize <- carprice$horsepower/carprice$enginesize

# checking the Structure
str(carprice)

View(carprice)


###
##### lets deal with outliers for numeric variables #####

### wheelbase
boxplot(carprice$wheelbase)
quantile(carprice$wheelbase,seq(0,1,0.01))
# There is a jump between 99% and 100%. So, cap all values above 115.544 (99%) to 115.544.
carprice$wheelbase[which(carprice$wheelbase > 115.544)] <- 115.544


### carlength
boxplot(carprice$carlength)
quantile(carprice$carlength,seq(0,1,0.01))
# There is a jump between 2% and 3%. So, cap all values below 155.900 (3%) to 155.900.
carprice$carlength[which(carprice$carlength < 155.900)] <- 155.900


### carwidth
boxplot(carprice$carwidth)
quantile(carprice$carwidth,seq(0,1,0.01))
# There is a jump between 0% and 1%. So, cap all values below 62.536 (1%) to 62.536.
# Also there is a jump between 96% and 97%. So, cap all values above 70.852 (96%) to 70.852.
carprice$carwidth[which(carprice$carwidth < 62.536)] <- 62.536
carprice$carwidth[which(carprice$carwidth > 70.852)] <- 70.852


### carheight
boxplot(carprice$carheight)
quantile(carprice$carheight,seq(0,1,0.01))
# There is no sudden jump.


### curbweight
boxplot(carprice$curbweight)
quantile(carprice$curbweight,seq(0,1,0.01))
# There is a jump between 0% and 1%. So, cap all values below 1819.72 (1%) to 1819.72.
# Also there is a jump between 98% and 99%. So, cap all values above 3768.40 (98%) to 3768.40.
carprice$curbweight[which(carprice$curbweight < 1819.72)] <- 1819.72
carprice$curbweight[which(carprice$curbweight > 3768.40)] <- 3768.40


### enginesize
boxplot(carprice$enginesize)
quantile(carprice$enginesize,seq(0,1,0.01))
# Also there is a jump between 96% and 97%. So, cap all values above 209.00 (96%) to 209.00.
carprice$enginesize[which(carprice$enginesize > 209.00)] <- 209.00


### boreratio
boxplot(carprice$boreratio)
quantile(carprice$boreratio,seq(0,1,0.01))
# Also there is a jump between 98% and 99%. So, cap all values above 3.7800 (98%) to 3.7800.
carprice$boreratio[which(carprice$boreratio > 3.7800)] <- 3.7800


### stroke
boxplot(carprice$stroke)
quantile(carprice$stroke,seq(0,1,0.01))
# There is a jump between 1% and 2%. So, cap all values below 2.6400 (2%) to 2.6400.
# Also there is a jump between 95% and 96%. So, cap all values above 3.6400 (95%) to 3.6400.
carprice$stroke[which(carprice$stroke < 2.6400)] <- 2.6400
carprice$stroke[which(carprice$stroke > 3.6400)] <- 3.6400



### compressionratio
boxplot(carprice$compressionratio)
quantile(carprice$compressionratio,seq(0,1,0.01))
# There is a jump between 90% and 91%. So, cap all values above 10.9400 (90%) to 10.9400.
carprice$compressionratio[which(carprice$compressionratio > 10.9400)] <- 10.9400


### horsepower
boxplot(carprice$horsepower)
quantile(carprice$horsepower,seq(0,1,0.01))
# There is a jump between 97% and 98%. So, cap all values above 184.00 (97%) to 184.00
carprice$horsepower[which(carprice$horsepower > 184.00)] <- 184.00



### peakrpm
boxplot(carprice$peakrpm)
quantile(carprice$peakrpm,seq(0,1,0.01))
# There is a jump between 99% and 100%. So, cap all values above 6000 (99%) to 6000
carprice$peakrpm[which(carprice$peakrpm > 6000)] <- 6000


### citympg
boxplot(carprice$citympg)
quantile(carprice$citympg,seq(0,1,0.01))
# There is a jump between 98% and 99%. So, cap all values above 38.00 (98%) to 38.00
carprice$citympg[which(carprice$citympg > 38.00)] <- 38.00

### highwaympg
boxplot(carprice$highwaympg)
quantile(carprice$highwaympg,seq(0,1,0.01))
# There is a jump between 98% and 99%. So, cap all values above 46.92 (98%) to 46.92
carprice$highwaympg[which(carprice$highwaympg > 46.92)] <- 46.92


### price
boxplot(carprice$price)
quantile(carprice$price,seq(0,1,0.01))
# There is no sudden jump.



########## Now lets deal with catogolical variables ###########
# we will simply set 0 and 1 to the variable having two catogories.
# for the variable having more than 2 catogories, we will use model.matrix to create dummy variables.


### symboling
unique(carprice$symboling)
summary(carprice$symboling)
dummy_1 <- data.frame(model.matrix( ~symboling -1, data = carprice))[,-1]
#check the dummy_1 data frame.
View(dummy_1)
carprice_1 <- cbind(carprice[,!(names(carprice) %in% "symboling")], dummy_1)


### fueltype
unique(carprice_1$fueltype)
carprice_1$fueltype <- as.factor(carprice_1$fueltype)
levels(carprice_1$fueltype)
levels(carprice_1$fueltype) <- c(0,1)
carprice_1$fueltype <- as.numeric(levels(carprice_1$fueltype))[carprice_1$fueltype]


### aspiration
unique(carprice_1$aspiration)
carprice_1$aspiration <- as.factor(carprice_1$aspiration)
levels(carprice_1$aspiration)
levels(carprice_1$aspiration) <- c(0,1)
carprice_1$aspiration <- as.numeric(levels(carprice_1$aspiration))[carprice_1$aspiration]


### doornumber
unique(carprice_1$doornumber)
carprice_1$doornumber <- as.factor(carprice_1$doornumber)
levels(carprice_1$doornumber)
levels(carprice_1$doornumber) <- c(1,0)
carprice_1$doornumber <- as.numeric(levels(carprice_1$doornumber))[carprice_1$doornumber]


### carbody
unique(carprice_1$carbody)
dummy_2 <- data.frame(model.matrix( ~carbody -1, data = carprice_1))
dummy_2 <- dummy_2[,-1]
carprice_2 <- cbind(carprice_1[,!(names(carprice_1) %in% "carbody")], dummy_2)


### drivewheel
unique(carprice_2$drivewheel)
dummy_3 <- data.frame(model.matrix( ~drivewheel -1, data = carprice_2))[,-1] 
carprice_3 <- cbind(carprice_2[,!(names(carprice_2) %in% "drivewheel")], dummy_3)


### enginelocation
unique(carprice_3$enginelocation)
carprice_3$enginelocation <- as.factor(carprice_3$enginelocation)
levels(carprice_3$enginelocation)
levels(carprice_3$enginelocation) <- c(1,0)
carprice_3$enginelocation <- as.numeric(levels(carprice_3$enginelocation))[carprice_3$enginelocation]


### enginetype
unique(carprice_3$enginetype)
summary(as.factor(carprice_3$enginetype))
dummy_4 <- data.frame(model.matrix( ~enginetype -1, data = carprice_3))[,-1] 
carprice_4 <- cbind(carprice_3[,!(names(carprice_3) %in% "enginetype")], dummy_4)



### fuelsystem
unique(carprice_4$fuelsystem)
summary(as.factor(carprice_4$fuelsystem))
dummy_5 <- data.frame(model.matrix( ~fuelsystem -1, data = carprice_4))[,-1]
carprice_5 <- cbind(carprice_4[,!(names(carprice_4) %in% "fuelsystem")], dummy_5)


### we will convert catogorical cylindernumber into numeric
unique(carprice_5$cylindernumber)
carprice_5$cylindernumber <- ifelse(carprice_5$cylindernumber == "two", 2,
                                    ifelse(carprice_5$cylindernumber=="three",3,
                                           ifelse(carprice_5$cylindernumber=="four",4,
                                                  ifelse(carprice_5$cylindernumber=="five",5,
                                                         ifelse(carprice_5$cylindernumber=="six", 6,
                                                                ifelse(carprice_5$cylindernumber=="eight", 8,
                                                                       ifelse(carprice_5$cylindernumber=="twelve",12,NA)))))))



summary(as.factor(carprice_5$cylindernumber))

# checking if there is any NA after the conversion
sum(is.na(carprice_5$cylindernumber))



# dealing with spelling in the makers name(company) of the data
unique(carprice_5$company)
summary(as.factor(carprice_5$company))
carprice_5$company <- tolower(carprice_5$company)
unique(carprice_5$company)
carprice_5$company[carprice_5$company=="maxda"] <- "mazda"
carprice_5$company[carprice_5$company=="porcshce"] <- "porsche"
carprice_5$company[carprice_5$company=="toyouta"] <- "toyota"
carprice_5$company[carprice_5$company %in% c("vokswagen", "vw")] <- "volkswagen"
unique(carprice_5$company)

### spliting company name into different dummy mariable and combinding back to dataset
dummy_6 <- data.frame(model.matrix( ~company -1, data = carprice_5))[,-1]
carprice_6 <- cbind(carprice_5[,!(names(carprice_5) %in% "company")], dummy_6)


### NULL check after all convirsion
sum(is.na(carprice_6))
sum(carprice_6=="")
sum(carprice_6==" ")

# duplicate check after conversion 
nrow(distinct(carprice_6)) == nrow(carprice_6)

# comparing row count after all conversion
nrow(carprice) == nrow(carprice_6)


# as first column (id) is just identifier, removing this
carprice_7 <- carprice_6[,-1]




##############

# Divide into training and test data set
#set the seed to 100, let's run it 
set.seed(100)

# randomly generate row indices for train dataset
trainindices <- sample(1:nrow(carprice_7), 0.7*nrow(carprice_7))
# generate the train data set
train <-  carprice_7[trainindices,]

#Similarly store the rest of the observations into an object "test".
test <-  carprice_7[-trainindices,]

################ lets now start building model one by one ############
# Build model 1 containing all variables
model_1 <-lm(price~.,data=train)
summary(model_1)


# Now, lets see how to use stepAIC

# In stepAIC function, we pass our first model i.e model_1 and 
# direction is ser as both, because in stepwise,  both the forward selection 
# of variables and backward elimination of variables happen simultaneously 

step <- stepAIC(model_1, direction="both")

# now we need to know our model equation so lets write the Step command here. 

step
# stepAIC makes multiple calls while checking which variables to keep
# The last call that step makes, contains only the variables it considers to be important in the model. 
# some insignifican variables have been removed. 
# Now store the last model equation of stepwise method into an object called model_2

model_2 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                carwidth + carheight + curbweight + cylindernumber + boreratio + 
                stroke + horsepower + symbolingsafe + symbolingrisky + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypedohcv + enginetypel + enginetypeohcf + enginetypeohcv + 
                enginetyperotor + companybmw + companybuick + companychevrolet + 
                companydodge + companyjaguar + companymazda + companymercury + 
                companymitsubishi + companynissan + companyplymouth + companyrenault + 
                companysaab + companytoyota + companyvolkswagen, data = train)
# Let us look at the summary of the model
summary(model_2)

## Let us check for multicollinearity 
# If the VIF is above 2 would remove 
# the variables if they are statistically insignificant
vif(model_2)
# arrange the variables on decreasing order of VIF
data.frame(var=names(vif(model_2)),vif=vif(model_2)) %>% arrange(desc(vif))

# curbweight has a VIF of 22.316230 and p value is 0.205217 which is insignificant 
# and thus, we can remove curbweight variable and run the model again
model_3 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                carwidth + carheight + cylindernumber + boreratio + 
                stroke + horsepower + symbolingsafe + symbolingrisky + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypedohcv + enginetypel + enginetypeohcf + enginetypeohcv + 
                enginetyperotor + companybmw + companybuick + companychevrolet + 
                companydodge + companyjaguar + companymazda + companymercury + 
                companymitsubishi + companynissan + companyplymouth + companyrenault + 
                companysaab + companytoyota + companyvolkswagen, data = train)
# Let us look at the summary of the model
summary(model_3)

#checking VIF
data.frame(var=names(vif(model_3)),vif=vif(model_3)) %>% arrange(desc(vif))


# symbolingrisky has the max VIF with p-value insignificant
# lets remove symbolingrisky and build 4th model
model_4 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                carwidth + carheight + cylindernumber + boreratio + 
                stroke + horsepower + symbolingsafe + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypedohcv + enginetypel + enginetypeohcf + enginetypeohcv + 
                enginetyperotor + companybmw + companybuick + companychevrolet + 
                companydodge + companyjaguar + companymazda + companymercury + 
                companymitsubishi + companynissan + companyplymouth + companyrenault + 
                companysaab + companytoyota + companyvolkswagen, data = train)
# Let us look at the summary of the model
summary(model_4)

#VIF
data.frame(var=names(vif(model_4)),vif=vif(model_4)) %>% arrange(desc(vif))


# boreratio has the max VIF with p-value insignificant
# lets remove boreratio and build 5th model
model_5 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                carwidth + carheight + cylindernumber + 
                stroke + horsepower + symbolingsafe + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypedohcv + enginetypel + enginetypeohcf + enginetypeohcv + 
                enginetyperotor + companybmw + companybuick + companychevrolet + 
                companydodge + companyjaguar + companymazda + companymercury + 
                companymitsubishi + companynissan + companyplymouth + companyrenault + 
                companysaab + companytoyota + companyvolkswagen, data = train)
# Let us look at the summary of the model
summary(model_5)
#VIF
data.frame(var=names(vif(model_5)),vif=vif(model_5)) %>% arrange(desc(vif))


# carheight has the max VIF with p-value insignificant
# lets remove carheight and build 6th model
model_6 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                carwidth + cylindernumber + stroke + horsepower + 
                symbolingsafe + carbodyhardtop + carbodyhatchback + carbodysedan + 
                carbodywagon + drivewheelrwd + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetypeohcv + enginetyperotor + companybmw + 
                companybuick + companychevrolet + companydodge + companyjaguar + companymazda + 
                companymercury + companymitsubishi + companynissan + companyplymouth + companyrenault + 
                companysaab + companytoyota + companyvolkswagen, data = train)
# Let us look at the summary of the model
summary(model_6)
#VIF
data.frame(var=names(vif(model_6)),vif=vif(model_6)) %>% arrange(desc(vif))


# enginetypel has the max VIF with p-value insignificant
# lets remove enginetypel and build 7th model
model_7 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                carwidth + cylindernumber + stroke + horsepower + 
                symbolingsafe + carbodyhardtop + carbodyhatchback + carbodysedan + 
                carbodywagon + drivewheelrwd + enginetypedohcv +
                enginetypeohcf + enginetypeohcv + enginetyperotor + companybmw + 
                companybuick + companychevrolet + companydodge + companyjaguar + companymazda + 
                companymercury + companymitsubishi + companynissan + companyplymouth + companyrenault + 
                companysaab + companytoyota + companyvolkswagen, data = train)
# Let us look at the summary of the model
summary(model_7)
# VIF
data.frame(var=names(vif(model_7)),vif=vif(model_7)) %>% arrange(desc(vif))


# companysaab has the max VIF with p-value insignificant
# lets remove companysaab and build 8th model
model_8 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                carwidth + cylindernumber + stroke + horsepower + 
                symbolingsafe + carbodyhardtop + carbodyhatchback + carbodysedan + 
                carbodywagon + drivewheelrwd + enginetypedohcv +
                enginetypeohcf + enginetypeohcv + enginetyperotor + companybmw + 
                companybuick + companychevrolet + companydodge + companyjaguar + companymazda + 
                companymercury + companymitsubishi + companynissan + companyplymouth + companyrenault + 
                companytoyota + companyvolkswagen, data = train)
# Let us look at the summary of the model
summary(model_8)
#VIF
data.frame(var=names(vif(model_8)),vif=vif(model_8)) %>% arrange(desc(vif))


# enginetypeohcv has the max VIF with p-value insignificant
# lets remove enginetypeohcv and build 9th model
model_9 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                carwidth + cylindernumber + stroke + horsepower + 
                symbolingsafe + carbodyhardtop + carbodyhatchback + carbodysedan + 
                carbodywagon + drivewheelrwd + enginetypedohcv +
                enginetypeohcf + enginetyperotor + companybmw + 
                companybuick + companychevrolet + companydodge + companyjaguar + companymazda + 
                companymercury + companymitsubishi + companynissan + companyplymouth + companyrenault + 
                companytoyota + companyvolkswagen, data = train)
# Let us look at the summary of the model
summary(model_9)
# VIF
data.frame(var=names(vif(model_9)),vif=vif(model_9)) %>% arrange(desc(vif))

# non of the variable having vif > 2 have insignificant p-value
# carbodysedan has max VIF, lets find corealtion between carbodysedan and other variables present in the model
train_1 <- train[,c("carbodysedan","carbodyhatchback","carlength","carwidth","carbodywagon","horsepower","cylindernumber","carbodyhardtop","drivewheelrwd","companybuick","enginetypeohcf","enginetyperotor","enginelocation","stroke","aspiration","companymazda","companytoyota","symbolingsafe","companyvolkswagen","enginetypedohcv","companynissan","companyjaguar","companymitsubishi","companyplymouth","companybmw","companydodge","companymercury","companyrenault","companychevrolet")]
cor(train$carbodysedan, train_1)
# we can see carbodysedan and carbodyhatchback are highly correalated , -0.6421687 (64%)
# and with regards to p-value, carbodysedan(0.000202) is more significant compare to carbodyhatchback(0.000126)
# so just drop carbodyhatchback

model_10 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                carwidth + cylindernumber + stroke + horsepower + 
                symbolingsafe + carbodyhardtop  + carbodysedan + 
                carbodywagon + drivewheelrwd + enginetypedohcv +
                enginetypeohcf + enginetyperotor + companybmw + 
                companybuick + companychevrolet + companydodge + companyjaguar + companymazda + 
                companymercury + companymitsubishi + companynissan + companyplymouth + companyrenault + 
                companytoyota + companyvolkswagen, data = train)
# Let us look at the summary of the model
summary(model_10)
#VIF
data.frame(var=names(vif(model_10)),vif=vif(model_10)) %>% arrange(desc(vif))



# drivewheelrwd has the max VIF with p-value insignificant
# lets remove drivewheelrwd and build 11th model
model_11 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                 carwidth + cylindernumber + stroke + horsepower + 
                 symbolingsafe + carbodyhardtop  + carbodysedan + 
                 carbodywagon + enginetypedohcv +
                 enginetypeohcf + enginetyperotor + companybmw + 
                 companybuick + companychevrolet + companydodge + companyjaguar + companymazda + 
                 companymercury + companymitsubishi + companynissan + companyplymouth + companyrenault + 
                 companytoyota + companyvolkswagen, data = train)
# Let us look at the summary of the model
summary(model_11)
# VIF
data.frame(var=names(vif(model_11)),vif=vif(model_11)) %>% arrange(desc(vif))



# carbodysedan has the max VIF with p-value insignificant
# lets remove carbodysedan and build 12th model
model_12 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                 carwidth + cylindernumber + stroke + horsepower + 
                 symbolingsafe + carbodyhardtop  + carbodywagon + enginetypedohcv +
                 enginetypeohcf + enginetyperotor + companybmw + 
                 companybuick + companychevrolet + companydodge + companyjaguar + companymazda + 
                 companymercury + companymitsubishi + companynissan + companyplymouth + companyrenault + 
                 companytoyota + companyvolkswagen, data = train)
# Let us look at the summary of the model
summary(model_12)
# VIF
data.frame(var=names(vif(model_12)),vif=vif(model_12)) %>% arrange(desc(vif))


# non of the variable having vif >2 have insignificant p-value
# carwidth has max VIF, lets find corealtion between horsepower and other variables present in the model
train_2 <- train[,c("carwidth","cylindernumber","horsepower","carlength","companybuick","enginetypeohcf","enginetyperotor","enginelocation","aspiration","stroke","companymazda","companytoyota","symbolingsafe","enginetypedohcv","companyvolkswagen","companynissan","carbodyhardtop","carbodywagon","companyjaguar","companymitsubishi","companyplymouth","companydodge","companymercury","companybmw","companyrenault","companychevrolet")]
View(cor(train_2))
# we can see carwidth and carlength are highly correalated , 0.84784960 (85%)
# and with regards to p-value, carwidth(0.000982) is more significant compare to carlength(0.001038)
# so just drop carlength
model_13 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + cylindernumber + stroke + horsepower + 
                 symbolingsafe + carbodyhardtop  + carbodywagon + enginetypedohcv +
                 enginetypeohcf + enginetyperotor + companybmw + 
                 companybuick + companychevrolet + companydodge + companyjaguar + companymazda + 
                 companymercury + companymitsubishi + companynissan + companyplymouth + companyrenault + 
                 companytoyota + companyvolkswagen, data = train)
# Let us look at the summary of the model
summary(model_13)

data.frame(var=names(vif(model_13)),vif=vif(model_13)) %>% arrange(desc(vif))


# aspiration has the max VIF with p-value insignificant
# lets remove aspiration and build 14th model 
model_14 <- lm(formula = price ~  enginelocation + carwidth + cylindernumber + stroke + horsepower + 
                 symbolingsafe + carbodyhardtop  + carbodywagon + enginetypedohcv +
                 enginetypeohcf + enginetyperotor + companybmw + 
                 companybuick + companychevrolet + companydodge + companyjaguar + companymazda + 
                 companymercury + companymitsubishi + companynissan + companyplymouth + companyrenault + 
                 companytoyota + companyvolkswagen, data = train)
# Let us look at the summary of the model
summary(model_14)
# VIF
data.frame(var=names(vif(model_14)),vif=vif(model_14)) %>% arrange(desc(vif))


# non of the variable having vif >2 have insignificant p-value
# horsepower has max VIF, lets find corealtion between horsepower and other variables present in the model having VIF >2
train_3 <- train[,c("horsepower","cylindernumber","carwidth","enginetypeohcf","companybuick","enginelocation","stroke")]
View(cor(train_3))
# we can see horsepower and cylindernumber are highly correalated , 0.67220812 (67%)
# and with regards to p-value, horsepower(9.23e-10) is more significant compare to cylindernumber(0.000482)
# so just drop cylindernumber
model_15 <- lm(formula = price ~  enginelocation + carwidth + stroke + horsepower + 
                 symbolingsafe + carbodyhardtop  + carbodywagon + enginetypedohcv +
                 enginetypeohcf + enginetyperotor + companybmw + 
                 companybuick + companychevrolet + companydodge + companyjaguar + companymazda + 
                 companymercury + companymitsubishi + companynissan + companyplymouth + companyrenault + 
                 companytoyota + companyvolkswagen, data = train)
# Let us look at the summary of the model
summary(model_15)
#VIF
data.frame(var=names(vif(model_15)),vif=vif(model_15)) %>% arrange(desc(vif))




# non of the variable having vif >2 have insignificant p-value
# horsepower has max VIF, lets find corealtion between horsepower and other variables present in the model having VIF >2
train_4 <- train[,c("carwidth","horsepower","enginetypeohcf","enginelocation","stroke")]
View(cor(train_4))
# we can see horsepower and carwidth are highly correalated , 0.65876400 (66%)
# and with regards to p-value, horsepower(2e-16) is more significant compare to carwidth(5.31e-13)
# so just drop carwidth
model_16 <- lm(formula = price ~  enginelocation + stroke + horsepower + 
                 symbolingsafe + carbodyhardtop  + carbodywagon + enginetypedohcv +
                 enginetypeohcf + enginetyperotor + companybmw + 
                 companybuick + companychevrolet + companydodge + companyjaguar + companymazda + 
                 companymercury + companymitsubishi + companynissan + companyplymouth + companyrenault + 
                 companytoyota + companyvolkswagen, data = train)
# Let us look at the summary of the model
summary(model_16)
# VIF
data.frame(var=names(vif(model_16)),vif=vif(model_16)) %>% arrange(desc(vif))



# stroke has the max VIF with p-value insignificant
# lets remove stroke and build 17th model
model_17 <- lm(formula = price ~  enginelocation + horsepower + 
                 symbolingsafe + carbodyhardtop  + carbodywagon + enginetypedohcv +
                 enginetypeohcf + enginetyperotor + companybmw + 
                 companybuick + companychevrolet + companydodge + companyjaguar + companymazda + 
                 companymercury + companymitsubishi + companynissan + companyplymouth + companyrenault + 
                 companytoyota + companyvolkswagen, data = train)
# Let us look at the summary of the model
summary(model_17)
# VIF
data.frame(var=names(vif(model_17)),vif=vif(model_17)) %>% arrange(desc(vif))


# now we have only one variable (horsepower) having VIF > 2
# and p-value for horsepower is significant.
# lets try and see after droping horsepower
model_18 <- lm(formula = price ~  enginelocation + symbolingsafe + carbodyhardtop  + carbodywagon + 
                 enginetypedohcv + enginetypeohcf + enginetyperotor + companybmw + 
                 companybuick + companychevrolet + companydodge + companyjaguar + companymazda + 
                 companymercury + companymitsubishi + companynissan + companyplymouth + companyrenault + 
                 companytoyota + companyvolkswagen, data = train)

# Let us look at the summary of the model
summary(model_18)

# here we can see that model Adjusted R-squared has decreased from 0.9123 to 0.7564
# so this is not comperatively good model

# lets start droping variables on the basis or p-value
summary(model_17)

# companyvolkswagen is most insignificant 
# droping companyvolkswagen
model_18 <- lm(formula = price ~  enginelocation + horsepower + 
                 symbolingsafe + carbodyhardtop  + carbodywagon + enginetypedohcv +
                 enginetypeohcf + enginetyperotor + companybmw + 
                 companybuick + companychevrolet + companydodge + companyjaguar + companymazda + 
                 companymercury + companymitsubishi + companynissan + companyplymouth + companyrenault + 
                 companytoyota , data = train)
# Let us look at the summary of the model
summary(model_18)


# symbolingsafe is most insignificant 
# droping symbolingsafe
model_19 <- lm(formula = price ~  enginelocation + horsepower + 
                 carbodyhardtop  + carbodywagon + enginetypedohcv +
                 enginetypeohcf + enginetyperotor + companybmw + 
                 companybuick + companychevrolet + companydodge + companyjaguar + companymazda + 
                 companymercury + companymitsubishi + companynissan + companyplymouth + companyrenault + 
                 companytoyota , data = train)
# Let us look at the summary of the model
summary(model_19)




# enginetyperotor is most insignificant 
# droping enginetyperotor
model_20 <- lm(formula = price ~  enginelocation + horsepower + 
                 carbodyhardtop  + carbodywagon + enginetypedohcv +
                 enginetypeohcf + companybmw + companybuick + 
                 companychevrolet + companydodge + companyjaguar + companymazda + 
                 companymercury + companymitsubishi + companynissan + companyplymouth + companyrenault + 
                 companytoyota , data = train)
# Let us look at the summary of the model
summary(model_20)


# companychevrolet is most insignificant 
# droping companychevrolet
model_21 <- lm(formula = price ~  enginelocation + horsepower + 
                 carbodyhardtop  + carbodywagon + enginetypedohcv +
                 enginetypeohcf + companybmw + companybuick + 
                 companydodge + companyjaguar + companymazda + 
                 companymercury + companymitsubishi + companynissan + companyplymouth + companyrenault + 
                 companytoyota , data = train)
# Let us look at the summary of the model
summary(model_21)



# carbodyhardtop is most insignificant 
# droping carbodyhardtop
model_22 <- lm(formula = price ~  enginelocation + horsepower + 
                 carbodywagon + enginetypedohcv + enginetypeohcf + companybmw + companybuick + 
                 companydodge + companyjaguar + companymazda + 
                 companymercury + companymitsubishi + companynissan + companyplymouth + companyrenault + 
                 companytoyota , data = train)
# Let us look at the summary of the model
summary(model_22)


# carbodywagon is most insignificant 
# droping carbodywagon
model_23 <- lm(formula = price ~  enginelocation + horsepower + 
                 enginetypedohcv + enginetypeohcf + companybmw + companybuick + 
                 companydodge + companyjaguar + companymazda + 
                 companymercury + companymitsubishi + companynissan + companyplymouth + companyrenault + 
                 companytoyota , data = train)
# Let us look at the summary of the model
summary(model_23)



# companyrenault is most insignificant 
# droping companyrenault
model_24 <- lm(formula = price ~  enginelocation + horsepower + 
                 enginetypedohcv + enginetypeohcf + companybmw + companybuick + 
                 companydodge + companyjaguar + companymazda + 
                 companymercury + companymitsubishi + companynissan + companyplymouth  + 
                 companytoyota , data = train)
# Let us look at the summary of the model
summary(model_24)



# companymazda is most insignificant 
# droping companymazda
model_25 <- lm(formula = price ~  enginelocation + horsepower + 
                 enginetypedohcv + enginetypeohcf + companybmw + companybuick + 
                 companydodge + companyjaguar + companymercury + companymitsubishi + 
                 companynissan + companyplymouth  + companytoyota , data = train)
# Let us look at the summary of the model
summary(model_25)



# companymercury is most insignificant 
# droping companymercury
model_26 <- lm(formula = price ~  enginelocation + horsepower + 
                 enginetypedohcv + enginetypeohcf + companybmw + companybuick + 
                 companydodge + companyjaguar + companymitsubishi + 
                 companynissan + companyplymouth  + companytoyota , data = train)
# Let us look at the summary of the model
summary(model_26)



# predicting the results in test dataset
Predict_1 <- predict(model_26,test[, !(names(test) %in% "price")])
test$test_price <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared  # 0.8388796
