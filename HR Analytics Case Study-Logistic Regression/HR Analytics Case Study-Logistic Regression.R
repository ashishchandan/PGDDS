#############################XYZ Company Attrition Solution###################
##############################################################################
#Business Understanding
#Data Understanding
#Data Preparation & EDA
#Model Building 
#Model Evaluation
################################################################

### Business Understanding:
#A large company named XYZ, employs, at any given point of time, around 4000 employees. However,
#every year, around 15% of its employees leave the company and need to be replaced with the 
#talent pool available in the job market. The management believes that this level of attrition 
#(employees leaving, either on their own or because they got fired) is bad for the company


## AIM:
#Required to model the probability of attrition using a logistic regression. The results thus 
#obtained will be used by the management to understand what changes they should make to their 
#workplace, in order to get most of their employees to stay.


##DATA given
#The Manager Survey Data - Collected from a company wide survey.
#The Employee Survey Data - Collected from a company wide survey.
#In-Time Data - Collected from company's attendance Log sheet/ Machine
#Out - Time Data - Collected from company's attendance Log sheet/ Machine
#General Data - General data includes employee wide their personal data  along with education 
#               and their satisfaction level for association with XYZ org, etc.  

################################################################

### Data Understanding

# Install and Load the required packages
#install.packages("MASS")
#install.packages("car")
#install.packages("e1071")
#install.packages("caret", dependencies = c("Depends", "Suggests"))
#install.packages("cowplot")
#install.packages("GGally")
#install.packages("ggplot2")
#install.packages("lattice")

library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(stringr)
library(dplyr)

# Loading 5 files
employee_survey_data<- read.csv("employee_survey_data.csv", stringsAsFactors = T)
general_data        <- read.csv("general_data.csv", stringsAsFactors = T) 
manager_survey_data <- read.csv("manager_survey_data.csv", stringsAsFactors = T)
in_time             <- read.csv("in_time.csv", stringsAsFactors = T)
out_time            <- read.csv("out_time.csv", stringsAsFactors = T)
## Used stringsAsFactors = T to identify factor variables upfront ######

str(employee_survey_data)    # 4410 obs of 4 variables 
str(general_data)            # 4410 obs of 24 variables including the target variable
str(manager_survey_data)     # 4410 obs of 3 variables
str(in_time)                 # 4410 obs of 262 variables
str(out_time)                # 4410 obs of 262 variables

#### Naming the first column name as EmployeeID for both in_time and out_time datasets
names(in_time) <- str_replace(names(in_time),"X","in")
names(out_time) <- str_replace(names(out_time),"X","out")
names(in_time)[1] <- "EmployeeID"
names(out_time)[1] <- "EmployeeID"

#### start - Data cleaning, applying derived metrics on intime and outtime datasets only ####

#### Get the average work hours from in_time and out_time dataset, which can help us to derive below
#1.Over time : Employee workimg greater than 8 hours
#2.Inadequate Work hours : Employee working less than 8 hours
#3.No of Leaves : If the employee has NA values for in time for a day which is not an assumed
#                 holiday or a weekend

# Check if NAs in both dataset are at same place, if yes , then employee was on leave on that date

sum(is.na(in_time) != is.na(out_time))
# sum is 0 , means the way on which there is no entry in in_time , there is no entry in out_time 
# which implies that the employee was on leave on that day.

ofc_hours <- data.frame(matrix(ncol = ncol(in_time), nrow = nrow(in_time)))
names(ofc_hours) <- str_replace(names(in_time),"in","hrs")
ofc_hours$EmployeeID <- in_time$EmployeeID
for(i in 2:ncol(ofc_hours)) {
  ofc_hours[,i] <- round(as.POSIXct(out_time[,i], format = "%Y-%m-%d %H:%M") - as.POSIXct(in_time[,i], 
                                                                                          format = "%Y-%m-%d %H:%M"),2)
}
ofc_hours <- sapply(ofc_hours, as.numeric)
ofc_hours <- as.data.frame(ofc_hours)

# There are 12 hodidays in a year, so to calculate leave, we have subtracted 12 holidays
ofc_hours$leave_count   <- apply(ofc_hours[,2:262],1,function(x) {sum(is.na(x))-12})
# Calculated average working hours for entire year per each employee id
ofc_hours$avg_ofc_hours <- apply(ofc_hours[,2:262],1,function(x) {mean(x,na.rm = T)})

#### END - Data cleaning, applying derived metrics on intime and outtime datasets only ####

# Collate the data together in one single file
length(unique(tolower(employee_survey_data$EmployeeID)))    # 4410, confirming EmployeeID is key 
length(unique(tolower(general_data$EmployeeID)))            # 4410, confirming EmployeeID is key
length(unique(tolower(manager_survey_data$EmployeeID)))     # 4410, confirming EmployeeID is key


setdiff(general_data$EmployeeID,employee_survey_data$EmployeeID) # Identical EmployeeID across these datasets
setdiff(general_data$EmployeeID,manager_survey_data$EmployeeID) # Identical EmployeeID across these datasets

# Merging general data, employee survey data and manager survey data in to one single dataset hr_attrition
hr_attrition  <- merge(general_data,employee_survey_data, by="EmployeeID", all = F)
hr_attrition  <- merge(hr_attrition,manager_survey_data, by="EmployeeID", all = F)

#Merging leave count and average office hours to the attrition i.e. final dataset
ofc_hours <- ofc_hours[order(ofc_hours$EmployeeID),] 
sum(hr_attrition$EmployeeID != ofc_hours$EmployeeID)        # 4410, confirming EmployeeID is key
hr_attrition$leave_count    <- ofc_hours$leave_count        # Merging leave count data to hr attrition data 
hr_attrition$avg_ofc_hours  <- ofc_hours$avg_ofc_hours      # Merging average working hours

#### We can futher segregate Average office working into two types i.e. greater than 8 hours and
#### less than 8 hours
hr_attrition$avg_ofc_hours <- ifelse(hr_attrition$avg_ofc_hours >= 8,1,0)

View(hr_attrition) #master file for further analysis 

################################################################
### Data Preparation & Exploratory Data Analysis
################################################################
# Understanding the structure of the collated file
str(hr_attrition) #4410 obs. of 31 variables;

# check for missing values
sum(is.na(hr_attrition))
# There are 111 NA values

anyNA(hr_attrition$Age)                            #No Missing values
anyNA(hr_attrition$Attrition)                      #No Missing values
anyNA(hr_attrition$BusinessTravel)                 #No Missing values
anyNA(hr_attrition$Department)                     #No Missing values  
anyNA(hr_attrition$DistanceFromHome)               #No Missing values
anyNA(hr_attrition$Education)                      #No Missing values
anyNA(hr_attrition$EducationField)                 #No Missing values  
anyNA(hr_attrition$EmployeeCount)                  #No Missing values
anyNA(hr_attrition$EmployeeNumber)                 #No Missing values
anyNA(hr_attrition$EnvironmentSatisfaction)        #HAVE Missing values
anyNA(hr_attrition$Gender)                         #No Missing values
anyNA(hr_attrition$JobInvolvement)                 #No Missing values
anyNA(hr_attrition$JobLevel)                       #No Missing values
anyNA(hr_attrition$JobRole)                        #No Missing values
anyNA(hr_attrition$JobSatisfaction)                #HAVE Missing Values
anyNA(hr_attrition$MaritalStatus)                  #No Missing values
anyNA(hr_attrition$MonthlyIncome)                  #No Missing values
anyNA(hr_attrition$NumCompaniesWorked)             #HAVE Missing values
anyNA(hr_attrition$Over18)                         #No Missing values
anyNA(hr_attrition$PercentSalaryHike)              #No Missing values
anyNA(hr_attrition$PerformanceRating)              #No Missing values
anyNA(hr_attrition$RelationshipSatisfaction)       #No Missing values
anyNA(hr_attrition$StandardHours)                  #No Missing values
anyNA(hr_attrition$StockOptionLevel)               #No Missing values
anyNA(hr_attrition$TotalWorkingYears)              #HAVE Missing values
anyNA(hr_attrition$TrainingTimesLastYear)          #No Missing values
anyNA(hr_attrition$WorkLifeBalance)                #HAVE Missing values  
anyNA(hr_attrition$YearsAtCompany)                 #No Missing values
anyNA(hr_attrition$YearsSinceLastPromotion)        #No Missing values
anyNA(hr_attrition$YearsWithCurrManager)           #No Missing values  

#cleaning NA for all variables of above by replacing with median or mean
hr_attrition$JobSatisfaction[which(is.na(hr_attrition$JobSatisfaction))]<-
  median(hr_attrition$JobSatisfaction,na.rm = TRUE)
hr_attrition$WorkLifeBalance[which(is.na(hr_attrition$WorkLifeBalance))]<-
  median(hr_attrition$WorkLifeBalance,na.rm = TRUE)
hr_attrition$NumCompaniesWorked[which(is.na(hr_attrition$NumCompaniesWorked))]<-
  mean(hr_attrition$NumCompaniesWorked,na.rm = TRUE)
hr_attrition$TotalWorkingYears[which(is.na(hr_attrition$TotalWorkingYears))]<-
  mean(hr_attrition$TotalWorkingYears,na.rm = TRUE)
hr_attrition$EnvironmentSatisfaction[which(is.na(hr_attrition$EnvironmentSatisfaction))]<-
  median(hr_attrition$EnvironmentSatisfaction,na.rm = TRUE)

# check for missing values after replacing with median
sum(is.na(hr_attrition))
# There are zero NA values now

# check duplicated
which(duplicated(hr_attrition))
# no duplicatee values

#### Barcharts for categorical features with stacked telecom information
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")

# Barcharts for Categorical variables jobrole, Marital status, Business travel and Department against
# Attrition 

plot_grid(ggplot(hr_attrition, aes(x=JobRole,fill=factor(Attrition)))+ geom_bar(position = "fill")+bar_theme1, 
          ggplot(hr_attrition, aes(x=MaritalStatus,fill=factor(Attrition)))+ geom_bar(position = "fill"),
          ggplot(hr_attrition, aes(x=BusinessTravel,fill=factor(Attrition)))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(hr_attrition, aes(x=Department,fill=factor(Attrition)))+bar_theme1+ geom_bar(position = "fill"),
          align = "h")
# Job roles with  Research Director has the higher attrition
# Marital status - Single, are leaving the company more compared to married and divorced
# Employees who had Business travel frequently seems to quit more compared to others
# Surprising, HR department got more attrition 

# Barcharts for Categorical variables Education, Education field and Job level against Attrition

plot_grid(ggplot(hr_attrition, aes(x=Education,fill=factor(Attrition)))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(hr_attrition, aes(x=EducationField,fill=factor(Attrition)))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(hr_attrition, aes(x=JobLevel,fill=factor(Attrition)))+ geom_bar(position = "fill"),
          align = "h")   
# Education 2 got higher attrition
# Max percentage of person having Education Filed as Human Resource has left the organisation 
# job level 2 has got higher attrition

# Barcharts for Categorical variables EnvironmentSatisfaction, JobSatisfaction 
# and WorkLifeBalance against Attrition

plot_grid(ggplot(hr_attrition, aes(x=EnvironmentSatisfaction,fill=factor(Attrition)))+geom_bar(position = "fill")+bar_theme1,
          ggplot(hr_attrition, aes(x=JobSatisfaction,fill=factor(Attrition)))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(hr_attrition, aes(x=WorkLifeBalance,fill=factor(Attrition)))+ geom_bar(position = "fill"),
          align = "h")   
# Environment satisfaction, Job satisfactionlevel and Work Life Balance attrition decrease with 
# increase in corrosponding index.


# Barcharts for Categorical variables JobInvolvement, PerformanceRating against Attrition

plot_grid(ggplot(hr_attrition, aes(x=JobInvolvement,fill=factor(Attrition)))+geom_bar(position = "fill")+bar_theme1,
          ggplot(hr_attrition, aes(x=PerformanceRating,fill=factor(Attrition)))+ geom_bar(position = "fill"),
          align = "h")   
# lesser the job involvement, more the attrition
# Dont look much variation in attrition due to band performance

#### Histogram and Boxplots for numeric variables 
box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

#Histograms for numeric variables and their trends 
# Age
plot_grid(ggplot(hr_attrition, aes(Age))+ geom_histogram(binwidth = 10),
          ggplot(hr_attrition, aes(x="",y=Age))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
# 30 to 45 age group employees are more in the organisation

#DistanceFromHome
plot_grid(ggplot(hr_attrition, aes(DistanceFromHome))+ geom_histogram(binwidth = 10),
          ggplot(hr_attrition, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
# Employees who stay nearby office are more in the organisation

#MonthlyIncome
plot_grid(ggplot(hr_attrition, aes(MonthlyIncome))+ geom_histogram(binwidth = 10000),
          ggplot(hr_attrition, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
# Employees whose salary is less than 60000 are more in the organisation

#NumCompaniesWorked
plot_grid(ggplot(hr_attrition, aes(NumCompaniesWorked))+ geom_histogram(binwidth = 2),
          ggplot(hr_attrition, aes(x="",y=NumCompaniesWorked))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
# Employees who worked less than 3 companies are more in the organisation

#PercentSalaryHike
plot_grid(ggplot(hr_attrition, aes(PercentSalaryHike))+ geom_histogram(binwidth = 10),
          ggplot(hr_attrition, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
# More employees get percentage salary hike less than 15%

# Leave count
plot_grid(ggplot(hr_attrition, aes(leave_count))+ geom_histogram(binwidth = 10),
          ggplot(hr_attrition, aes(x="",y=leave_count))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#TrainingTimesLastYear
plot_grid(ggplot(hr_attrition, aes(TrainingTimesLastYear))+ geom_histogram(binwidth = 2),
          ggplot(hr_attrition, aes(x="",y=TrainingTimesLastYear))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#YearsAtCompany
plot_grid(ggplot(hr_attrition, aes(YearsAtCompany))+ geom_histogram(binwidth = 10),
          ggplot(hr_attrition, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
# There are more number of employees in the company spending 1 to 15 years working for the company 

#YearsSinceLastPromotion
plot_grid(ggplot(hr_attrition, aes(YearsSinceLastPromotion))+ geom_histogram(binwidth = 1),
          ggplot(hr_attrition, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
# Employees are more without promotion when the years since last promotion is less than 3 years

#TotalWorkingYears
plot_grid(ggplot(hr_attrition, aes(TotalWorkingYears))+ geom_histogram(binwidth = 1),
          ggplot(hr_attrition, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
# Employees are more whose total working experience is less than or equal to 10 years

# Box plot for Age, Distance from home and Monthly income against attrition status
plot_grid(ggplot(hr_attrition, aes(x=Attrition,y=Age, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(hr_attrition, aes(x=Attrition,y=DistanceFromHome, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(hr_attrition, aes(x=Attrition,y=MonthlyIncome, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)

# Box plot for PercentSalaryHike, TrainingTimesLastYear and NumCompaniesWorked against attrition status
plot_grid(ggplot(hr_attrition, aes(x=Attrition,y=PercentSalaryHike, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(hr_attrition, aes(x=Attrition,y=TrainingTimesLastYear, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(hr_attrition, aes(x=Attrition,y=NumCompaniesWorked, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)	
# Variation in TrainingTimesLastYear is less

# Box plot for YearsAtCompany, YearsSinceLastPromotion and YearsWithCurrManager against attrition status
plot_grid(ggplot(hr_attrition, aes(x=Attrition,y=YearsAtCompany, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(hr_attrition, aes(x=Attrition,y=YearsSinceLastPromotion, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(hr_attrition, aes(x=Attrition,y=YearsWithCurrManager, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)	
# Variation of Years since last promotion for the person who has left the organision is less
# compared to the employees who hasn't

#### Correlation between numeric variables
library(GGally)
ggpairs(hr_attrition[, c("Age", "DistanceFromHome", "MonthlyIncome","NumCompaniesWorked",
                         "YearsWithCurrManager","PercentSalaryHike","TrainingTimesLastYear",
                         "YearsAtCompany","YearsSinceLastPromotion","TotalWorkingYears")])

#Years at company and years with current manager are highly correlated (corr 0.77) followed by
#Total working years and years at company (corr 0.63), then followed by
#Years since last promotion and Years in company (corr 0.62)

##############################
### Checking for OUTLIERS#####
##############################
# str(hr_attrition)
# Only considered numeric variables for outlier detection
View(sapply(hr_attrition[,c(2,6,7,14,15,17,19,20,22:24,30)], 
            function(x) quantile(x,seq(0,1,.01),na.rm = T)))

# Monthly Income has got outliers, there is a sudden jump after 90 to 91 percentile. However
# This can be ignored considering 10% of the data is above the sudden jump 

#TotalWorkingYears has got some outliers after 99 percentile, Lets replace with 99% value
boxplot(hr_attrition$TotalWorkingYears)
boxplot.stats(hr_attrition$TotalWorkingYears)$stats
quantile(hr_attrition$TotalWorkingYears,seq(0,1,0.01))

hr_attrition$TotalWorkingYears[which(hr_attrition$TotalWorkingYears > 35)] <- 35

#YearsAtCompany has got some outliers after 98 percentile, Lets replace with 98% value
boxplot(hr_attrition$YearsAtCompany)
boxplot.stats(hr_attrition$YearsAtCompany)$stats
quantile(hr_attrition$YearsAtCompany,seq(0,1,0.01))

hr_attrition$YearsAtCompany[which(hr_attrition$YearsAtCompany > 24)] <- 24

#YearsWithCurrManager has got some outliers after 99 percentile, Lets replace with 99% value
boxplot(hr_attrition$YearsWithCurrManager)
boxplot.stats(hr_attrition$YearsWithCurrManager)$stats
quantile(hr_attrition$YearsWithCurrManager,seq(0,1,0.01))

hr_attrition$YearsWithCurrManager[which(hr_attrition$YearsWithCurrManager > 14)] <- 14

###### END OF OUTLIERS handling #########

#############################################################################################
# Feature standardisation
#############################################################################################
# Normalising continuous features 
hr_attrition$Age<- scale(hr_attrition$Age) 
hr_attrition$DistanceFromHome<- scale(hr_attrition$DistanceFromHome)
hr_attrition$MonthlyIncome<- scale(hr_attrition$MonthlyIncome)
hr_attrition$NumCompaniesWorked<- scale(hr_attrition$NumCompaniesWorked)
hr_attrition$PercentSalaryHike<- scale(hr_attrition$PercentSalaryHike)
hr_attrition$StockOptionLevel<- scale(hr_attrition$StockOptionLevel)
hr_attrition$TotalWorkingYears<- scale(hr_attrition$TotalWorkingYears)
hr_attrition$TrainingTimesLastYear<- scale(hr_attrition$TrainingTimesLastYear)
hr_attrition$YearsAtCompany<- scale(hr_attrition$YearsAtCompany)
hr_attrition$YearsSinceLastPromotion<- scale(hr_attrition$YearsSinceLastPromotion)
hr_attrition$YearsWithCurrManager<- scale(hr_attrition$YearsWithCurrManager)
hr_attrition$leave_count<- scale(hr_attrition$leave_count)


str(hr_attrition)
#View(hr_attrition)

#Below is the list of categorical attributes, few variables are categorized from 1 to 5 and 1 to 4
#which are categorical in nature as they mean "low to high" or "below college to doctor"
#Over18             -- 1 level We can see that Over18 fileld has only one level(no variation)
#Attrition, Gender  -- 2 levels
#More than 2 levels -- EnvironmentSatisfaction, JobSatisfaction, WorkLifeBalance, JobRole, MaritalStatus,
#                      BusinessTravel, Department,Education, EducationField, JobInvolvement, JobLevel, 
#                      PerformanceRating

#Converting the "Attrition,Gender and Over18" attributes with 2 levels into numbers(0,1)
hr_attrition$Attrition <- ifelse(hr_attrition$Attrition == "Yes", 1,0)
hr_attrition$Gender    <- ifelse(hr_attrition$Gender == "Male",1,0)
hr_attrition$Over18    <- ifelse(hr_attrition$Over18 == "Y", 1,0)

# Checking Attrition rate of prospect customer
Attrition <- sum(hr_attrition$Attrition)/nrow(hr_attrition)
Attrition # 16.12% churn rate. 

# creating a dataframe of categorical features
hr_categorical <- hr_attrition[,c(25,26,27,12,13,4,5,7,8,28,11,29)]
#View(hr_categorical)

# converting categorical attributes to factor
hr_categorical_fact<- data.frame(sapply(hr_categorical, function(x) factor(x)))
str(hr_categorical_fact)
summary(hr_categorical_fact)

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(hr_categorical_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =hr_categorical_fact))[,-1]))

# Final dataset
hr_attrition_final<- cbind(hr_attrition[,-c(25,26,27,12,13,4,5,7,8,28,11,29)],dummies) 
hr_attrition_final <- hr_attrition_final[,-1] # To remove employee id as it is not required
View(hr_attrition_final)
str(hr_attrition_final) #4410 obs. of  59 variables

################## END - data preparation and EDA ###################################### 

###########################################
# splitting the data between train and test
###########################################

set.seed(100)
# randomly generate row indices for train dataset
indices = sample.split(hr_attrition_final$Attrition, SplitRatio = 0.7)
# generate the train data set
train = hr_attrition_final[indices,]
#Similarly store the rest of the observations into an object "test".
test = hr_attrition_final[!(indices),]

########################################################################
# START - Logistic Regression: 
########################################################################

#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) #AIC 2104.9....nullDev 2728.0...resDev 1994

# Stepwise selection
library("MASS")
model_2<- stepAIC(model_1, direction="both")

summary(model_2)

# Removing multicollinearity through VIF check
library(car)
sort(vif(model_2),decreasing = T)

# Removing EducationField.xLife.Sciences as VIF is 15.27 and also the p value is high 0.12
model_3 <- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
                 avg_ofc_hours + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + Education.x5 + 
                 EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobInvolvement.x2 + JobInvolvement.x3 + 
                 JobLevel.x2, family = "binomial", data = train)

summary(model_3) 

sort(vif(model_3),decreasing = T)

# YearsAtCompany has high vif 4.83 and P as 0.32, Since the p value is not <0.05 wehave checked 
# the next three variables with high VIF but the p value is very low. Hence removing Years at company
# as part of model4

# MODEL 4 - Excluding Years at company 
model_4 <- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 avg_ofc_hours + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + Education.x5 + 
                 EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobInvolvement.x2 + JobInvolvement.x3 + 
                 JobLevel.x2, family = "binomial", data = train)

summary(model_4) 

sort(vif(model_4),decreasing = T)

# MODEL 5 - Excluding MaritalStatus.xMarried as it has got high VIF 2.14 and high p value 0.12
model_5 <- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 avg_ofc_hours + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + Education.x5 + 
                 EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobInvolvement.x2 + JobInvolvement.x3 + 
                 JobLevel.x2, family = "binomial", data = train)

summary(model_5) 

sort(vif(model_5),decreasing = T)

# After executing model 5 and analysing the top 5 variables with high VIF values, Its found that
# the P value is very low. Hence lets check for the correlation between the HIGH VIF variables
cor(hr_attrition_final$BusinessTravel.xTravel_Frequently,hr_attrition_final$BusinessTravel.xTravel_Rarely)
# -0.75, there exists strong negative correlation. Hence lets remove BusinessTravel.xTravel_Frequently
# in model 7

# MODEL 7 - Excluding BusinessTravel.xTravel_Frequently as per above explanation
model_7 <- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 avg_ofc_hours + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + Department.xSales 
               + Education.x5 +EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobInvolvement.x2 + JobInvolvement.x3 + 
                 JobLevel.x2, family = "binomial", data = train)

summary(model_7) 

sort(vif(model_7),decreasing = T)

# MODEL 8 - Excluding JobInvolvement.x2, though it got VIF as 1.91 the P value is much higher i.e. 0.17
model_8 <- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 avg_ofc_hours + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + Department.xSales 
               + Education.x5 +EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobInvolvement.x3 + JobLevel.x2, family = "binomial", data = train)

summary(model_8) 

sort(vif(model_8),decreasing = T)

# Again most of the TOP VIF value variables has very less P value, Lets check the correlation
cor(hr_attrition_final$Department.xSales,hr_attrition_final$Department.xResearch...Development)
# Very high negative correlation exists between these two variables i.e. -0.91
# Hence lets remove Department.xSales in our next model

# MODEL 9 - Excluding Department.xSales, as per explanation above
model_9 <- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 avg_ofc_hours + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + Education.x5 
               + EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobInvolvement.x3 + JobLevel.x2, family = "binomial", data = train)

summary(model_9) 

sort(vif(model_9),decreasing = T)

# Again most of the variables in descending order of VIF have very low p value
# Hence lets remove EducationField.xMedical which got high p value 0.38

# MODEL 10 - Excluding EducationField.xMedical, as per explanation above
model_10 <- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  avg_ofc_hours + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + Education.x5 
                + EducationField.xMarketing + EducationField.xOther + 
                  EducationField.xTechnical.Degree + JobInvolvement.x3 + JobLevel.x2, family = "binomial", data = train)

summary(model_10) 

sort(vif(model_10),decreasing = T)

# Again all the top variables in descending order of VIF have very low p value
# Hence lets remove JobInvolvement.x3 which got high p value 0.305

# MODEL 11 - Excluding JobInvolvement.x3, as per explanation above
model_11 <- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  avg_ofc_hours + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + Education.x5 
                + EducationField.xMarketing + EducationField.xOther + 
                  EducationField.xTechnical.Degree + JobLevel.x2, family = "binomial", data = train)

summary(model_11) 

sort(vif(model_11),decreasing = T)

# Again all the top variables in descending order of VIF have very low p value
# Hence lets remove distance from home which got high p value 0.098

# MODEL 12 - Excluding DistanceFromHome, as per explanation above
model_12 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  avg_ofc_hours + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + Education.x5 
                + EducationField.xMarketing + EducationField.xOther + 
                  EducationField.xTechnical.Degree + JobLevel.x2, family = "binomial", data = train)

summary(model_12) 

sort(vif(model_12),decreasing = T)


# Again all the top variables in descending order of VIF have very low p value
# Hence lets remove BusinessTravel.xTravel_Rarely which got high p value 0.07

# MODEL 13 - Excluding BusinessTravel.xTravel_Rarely, as per explanation above
model_13 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  avg_ofc_hours + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  Department.xResearch...Development + Education.x5 + EducationField.xMarketing + EducationField.xOther + 
                  EducationField.xTechnical.Degree + JobLevel.x2, family = "binomial", data = train)

summary(model_13) 

sort(vif(model_13),decreasing = T)

# MODEL 14 - Excluding Education.x5 which got high p value 0.068
model_14 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  avg_ofc_hours + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  Department.xResearch...Development + EducationField.xMarketing + EducationField.xOther + 
                  EducationField.xTechnical.Degree + JobLevel.x2, family = "binomial", data = train)

summary(model_14) 

sort(vif(model_14),decreasing = T)

# MODEL 15 - Excluding EducationField.xTechnical.Degree which got high p value 0.05
model_15 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  avg_ofc_hours + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  Department.xResearch...Development + EducationField.xMarketing + EducationField.xOther + 
                  JobLevel.x2, family = "binomial", data = train)

summary(model_15) 

sort(vif(model_15),decreasing = T)

# MODEL 16 - Excluding EducationField.xOther which got high p value 0.06
model_16 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  avg_ofc_hours + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  Department.xResearch...Development + EducationField.xMarketing + 
                  JobLevel.x2, family = "binomial", data = train)

summary(model_16) 

sort(vif(model_16),decreasing = T)

# MODEL 17 - Excluding EducationField.xMarketing which got high p value 0.06
model_17 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  avg_ofc_hours + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  Department.xResearch...Development + JobLevel.x2, family = "binomial", data = train)

summary(model_17) 

sort(vif(model_17),decreasing = T)

# MODEL 18 - Excluding Department.xResearch...Development which got high p value 0.19
model_18 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  avg_ofc_hours + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  JobLevel.x2, family = "binomial", data = train)

summary(model_18) 

sort(vif(model_18),decreasing = T)

# MODEL 19 - Excluding JobRole.xLaboratory.Technician which got high p value 0.02 (*)
model_19 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  avg_ofc_hours + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + JobLevel.x2, family = "binomial", data = train)

summary(model_19) 

sort(vif(model_19),decreasing = T)

# MODEL 20 - Excluding JobRole.xResearch.Scientist which got high p value 0.05 (*)
model_20 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  avg_ofc_hours + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobRole.xResearch.Director + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle + JobLevel.x2, family = "binomial", data = train)

summary(model_20) 

sort(vif(model_20),decreasing = T)

# MODEL 21 - Excluding NumCompaniesWorked which got high p value 0.02 (*)
model_21 <- glm(formula = Attrition ~ Age + MonthlyIncome + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  avg_ofc_hours + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobRole.xResearch.Director + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle + JobLevel.x2, family = "binomial", data = train)

summary(model_21) 

sort(vif(model_21),decreasing = T)

# MODEL 22 - Excluding MonthlyIncome which got high p value 0.01 (*)
model_22 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  avg_ofc_hours + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobRole.xResearch.Director + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle + JobLevel.x2, family = "binomial", data = train)

summary(model_22) 

sort(vif(model_22),decreasing = T)

# MODEL 23 - Excluding JobLevel.x2 which got high p value 0.003 (**)
model_23 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  avg_ofc_hours + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobRole.xResearch.Director + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle, family = "binomial", data = train)

summary(model_23) 

sort(vif(model_23),decreasing = T)

# MODEL 24 - Excluding JobRole.xResearch.Director which got high p value 0.001 (**)
model_24 <- glm(formula = Attrition ~ Age + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + avg_ofc_hours + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + WorkLifeBalance.x2 + 
                  WorkLifeBalance.x3 + WorkLifeBalance.x4 + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle, family = "binomial", data = train)

summary(model_24) 

sort(vif(model_24),decreasing = T)

# MODEL 25 - Excluding JobRole.xSales.Executive which got high p value 0.003 (**)
model_25 <- glm(formula = Attrition ~ Age + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + avg_ofc_hours + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + WorkLifeBalance.x2 + 
                  WorkLifeBalance.x3 + WorkLifeBalance.x4 + MaritalStatus.xSingle, family = "binomial", data = train)

summary(model_25) 

sort(vif(model_25),decreasing = T)

# MODEL 26 - Excluding WorkLifeBalance.x4 which got high p value 0.002 (**)
model_26 <- glm(formula = Attrition ~ Age + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + avg_ofc_hours + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + WorkLifeBalance.x2 + 
                  WorkLifeBalance.x3 + MaritalStatus.xSingle, family = "binomial", data = train)

summary(model_26) 

sort(vif(model_26),decreasing = T)

# MODEL 27 - Excluding WorkLifeBalance.x2 which got high p value 0.008 (**)
model_27 <- glm(formula = Attrition ~ Age + TotalWorkingYears + TrainingTimesLastYear +  
                  YearsSinceLastPromotion + YearsWithCurrManager + avg_ofc_hours + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x3 + MaritalStatus.xSingle, family = "binomial", data = train)

summary(model_27) 

sort(vif(model_27),decreasing = T)

### After MODEL 27 we have 14 variables with low VIF and low p values (i.e. ***)
### Lets check whether any other variable can be removed 

# Age and Total working years can have correlation, Also Total working years and Age occupies
# the first two slots for the highest VIF i.e. 2.32 and 1.08 respectively
# Hence lets find the correlation between these two
cor(hr_attrition_final$Age,hr_attrition_final$TotalWorkingYears)
# 0.68 is the correlation. Hence lets exclude Total working years as it has highest VIF  

# MODEL 28 - Excluding Age Total working years explained above
model_28 <- glm(formula = Attrition ~ Age + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + avg_ofc_hours + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + 
                  JobSatisfaction.x3 + JobSatisfaction.x4 + WorkLifeBalance.x3 + MaritalStatus.xSingle 
                , family = "binomial", data = train)

summary(model_28) 

sort(vif(model_28),decreasing = T)

############ MODEL 28 is our FINAL MODEL ##############################

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                -1.13129    0.16331  -6.927 4.29e-12 ***
#  Age                        -0.45799    0.05934  -7.718 1.18e-14 ***
#  TrainingTimesLastYear      -0.18525    0.05568  -3.327 0.000879 ***
#  YearsSinceLastPromotion     0.37649    0.06962   5.408 6.37e-08 ***
#  YearsWithCurrManager       -0.63028    0.07616  -8.275  < 2e-16 ***
#  avg_ofc_hours               1.29066    0.11186  11.538  < 2e-16 ***
#  EnvironmentSatisfaction.x2 -0.92218    0.16538  -5.576 2.46e-08 ***
#  EnvironmentSatisfaction.x3 -0.96936    0.14768  -6.564 5.24e-11 ***
#  EnvironmentSatisfaction.x4 -1.19211    0.15315  -7.784 7.02e-15 ***
#  JobSatisfaction.x2         -0.59763    0.16798  -3.558 0.000374 ***
#  JobSatisfaction.x3         -0.50662    0.14501  -3.494 0.000477 ***
#  JobSatisfaction.x4         -1.18175    0.15873  -7.445 9.70e-14 ***
#  WorkLifeBalance.x3         -0.36410    0.10958  -3.323 0.000891 ***
#  MaritalStatus.xSingle       1.04169    0.11138   9.353  < 2e-16 ***

#VIF values
#JobSatisfaction.x3       JobSatisfaction.x4 EnvironmentSatisfaction.x4   EnvironmentSatisfaction.x3 
#1.597555                   1.556632                   1.531721                   1.530331 
#YearsSinceLastPromotion  YearsWithCurrManager   JobSatisfaction.x2       EnvironmentSatisfaction.x2 
#1.529929                   1.477179                   1.453038                   1.416789 
#Age                      avg_ofc_hours        MaritalStatus.xSingle      TrainingTimesLastYear 
#1.111878                   1.056913                   1.047938                   1.012758 
#WorkLifeBalance.x3 
#1.011311

##########################################################################
########################################################################
# With 13 significant variables in the model
final_model<- model_28

#######################################################################
### Model Evaluation 
#######################################################################

### Test Data ####
#predicted probabilities of Attrition for test data
str(test)
test_pred = predict(final_model, type = "response", 
                    newdata = test[,-2])

# Let's see the summary 
summary(test_pred)

test$prob <- test_pred
#View(test)
#range(test$prob)

#### Let's use the probability cutoff of 50% before calculation of optimal cutoff
test_pred_Attrition_50 <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_Attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))
library(e1071)

table(test_actual_Attrition,test_pred_Attrition_50)
### InformationValue package needs to be detached if error "unused argument (positive = "Yes")"  
### is triggered during confusion matrix step execution  
# detach("package:InformationValue", unload=TRUE)
test_conf_50 <- confusionMatrix(test_pred_Attrition_50, test_actual_Attrition, positive = "Yes")
# Got below with 50 percent cutoff
# Accuracy - 85%
# Sensitivity - 22%
# Specificity - 98%

#### Let's use the probability cutoff of 40%, since for the above the sensitivity is very low
test_pred_Attrition_40 <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))

### InformationValue package needs to be detached if error "unused argument (positive = "Yes")"  
### is triggered during confusion matrix step execution  
# detach("package:InformationValue", unload=TRUE)
test_conf_40 <- confusionMatrix(test_pred_Attrition_40, test_actual_Attrition, positive = "Yes")
# Got below with 40 percent cutoff
# Accuracy - 85%
# Sensitivity - 31%
# Specificity - 95%

#########################################################################################
# Let's Choose the cutoff value. 
# Let's find out the optimal probalility cutoff 
#########################################################################################

### InformationValue package needs to be detached if error "unused argument (positive = "Yes")"  
### is triggered during confusion matrix step execution  
# detach("package:InformationValue", unload=TRUE)

perform_fn <- function(cutoff) 
{
  predicted_Attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_Attrition, test_actual_Attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.002365931 0.890867781 for plotting and initiallizing a matrix of 100 X 3.
# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)

### InformationValue package needs to be detached if error "unused argument (positive = "Yes")"  
### is triggered during confusion matrix step/function executing confusion matrix  
# detach("package:InformationValue", unload=TRUE)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.02)]
cutoff <- min(cutoff)

# Let's choose the min cutoff value for final model
test_cutoff_Attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))

### InformationValue package needs to be detached if error "unused argument (positive = "Yes")"  
### is triggered during confusion matrix step execution  
# detach("package:InformationValue", unload=TRUE)
conf_final <- confusionMatrix(test_cutoff_Attrition, test_actual_Attrition, positive = "Yes")

acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]

acc
sens
spec

# Got below with optimal cutoff value (i.e. 0.1536364)
# Accuracy - 0.7399849     (74%)
# Sensitivity - 0.7558685  (75%)  
# Specificity - 0.7369369  (74%)    

View(test)

##################################################################################################
### KS -statistic - Test Data ######
##################################################################################################

test_cutoff_Attrition_KS <- ifelse(test_cutoff_Attrition=="Yes",1,0)
test_actual_Attrition_KS <- ifelse(test_actual_Attrition=="Yes",1,0)


library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_Attrition_KS, test_actual_Attrition_KS)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)
# 0.4928055

####################################################################
# Lift & Gain Chart 
# plotting the lift chart
####################################################################

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attrition_decile = lift(test_actual_Attrition_KS, test_pred, groups = 10)

######## plot the lift chart #######

plot(Attrition_decile$Cumlift, type="l", lwd=2, col="red",# lty=4,
     xlim = c(0,10),
     ylim = c(0,4),
     main = "Lift Chart",
     xlab = "Decile",
     ylab = "Lift")
abline(h=1, col="brown")
axis(1, 1:10)
abline(h=0:10, v=0:10, lty=3)

######### Plot Gain Chart #########

#install.packages("InformationValue")
library(InformationValue)
ks_plot(test_actual_Attrition_KS, test_cutoff_Attrition_KS) # Gain chart plot
# To avoid "invalid 'name' argument" error while execution of confusion matrix when this 
# complete code is rerun we are detaching the package.
detach("package:InformationValue", unload=TRUE) 
#######################################################################################
# ************************* conclusion ********************************* #
#                            COEFFICIENTS
#  Age                        -0.45799    
#  TrainingTimesLastYear      -0.18525    
#  YearsSinceLastPromotion     0.37649    
#  YearsWithCurrManager       -0.63028    
#  avg_ofc_hours               1.29066    
#  EnvironmentSatisfaction.x2 -0.92218    
#  EnvironmentSatisfaction.x3 -0.96936    
#  EnvironmentSatisfaction.x4 -1.19211    
#  JobSatisfaction.x2         -0.59763    
#  JobSatisfaction.x3         -0.50662    
#  JobSatisfaction.x4         -1.18175    
#  WorkLifeBalance.x3         -0.36410    
#  MaritalStatus.xSingle       1.04169    

#Age                        - Employees with less AGE group are prone to quit the company

#TrainingTimesLastYear      - IF employee is getting more frequent trainings, then employee is NOT
#                             quitting the organisation 

#Years Since Last Promotion - IF Employees are getting frequent promotions then less chances of quitting  
#                             the oraganisation when compared to employees who promotion gets delayed

#Years with Current manager - Employee who works with same manager for longer time then less chances 
#                             of quitting the organisation

#Environment Satisfaction, Job Satisfaction and Work life balance -
#                           The better these are for employees the less Employee will quit the organisation

#Average working hours -      The more an employee works overtime on an average the more are the
#                             chances that employee will leave the company.

#Marital status single      - Employees who is single are prone to leave the company

#########################################################################################################
##############################################END########################################################
#########################################################################################################







