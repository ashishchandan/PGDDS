#################EDA CASE STUDY#############################
############################################################

#loading libraries
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("stringr")
#install.packages("gmodels")

library(ggplot2)
library(dplyr)
library(stringr)
library(gmodels)

# import data
loan_df <- read.csv("loan.csv" , stringsAsFactors = F)
loan <- loan_df

str(loan)

# Data Cleaning

# Checking for any duplicate id and member_id
nrow(loan_df)==length(loan_df$id)
nrow(loan_df)==length(loan_df$member_id)

names(loan)


# We can eliminate any fields that would not have been known at the time of issuance, 
# as we'll be trying to make decisions on loan investments using available pre-issuance data. 
# We can also eliminate a few indicative data fields that are repetitive or too granular to be analyzed,

# Select variables to keep and subset the data
variables <- c("id", "loan_amnt", "term", "int_rate", "installment", "grade", 
               "sub_grade", "emp_length", "home_ownership", "annual_inc", 
               "verification_status", "issue_d","loan_status", "purpose", "addr_state", "dti", 
               "delinq_2yrs", "inq_last_6mths", "open_acc", 
               "pub_rec", "revol_bal", "revol_util", "total_acc")

loan <- loan[,variables]

#Convert int_rate into numeric 
loan$int_rate <- str_replace(loan$int_rate,"%","") %>% as.numeric()

#Convert revol_util into numeric 
loan$revol_util <- str_replace(loan$revol_util,"%","") %>% as.numeric()

#Checking if the conversion has happned properly
sum(is.na(loan$revol_util)) == (sum(is.na(loan_df$revol_util)) + sum(loan_df$revol_util==""))

#Convert issue_d into date format
loan$issue_d1 <- paste0("01-", loan$issue_d) %>% as.POSIXct(format = "%d-%b-%y")

#Checking if the conversion has happned properly
sum(is.na(loan$issue_d1))

#extracting year and month from issue date
loan$issue_year <- as.numeric(format(loan$issue_d1 , "%Y"))
loan$issue_mnt <- format(loan$issue_d1 , "%b")

# Convert a required variables to factors, Since during analysis and plotting the graph factor are required
loan$delinq_2yrs <- factor(loan$delinq_2yrs)
loan$inq_last_6mths <- factor(loan$inq_last_6mths)
loan$open_acc <- factor(loan$open_acc)
loan$pub_rec <- factor(loan$pub_rec)
loan$total_acc <- factor(loan$total_acc)

# removing extra space from term field
loan$term <- trimws(loan$term)

#########Check for missing values##############
anyNA(loan$int_rate) #No Missing values
anyNA(loan_df$revol_util) #No missing values
anyNA(loan$installment) #No missing values
anyNA(loan$annual_inc) #No Missing values
anyNA(loan$loan_status) #No Missing values
anyNA(loan$dti) #No missing values
anyNA(loan$delinq_2yrs) #No missing values
anyNA(loan$inq_last_6mths) #No missing values
anyNA(loan$term) #No missing values
anyNA(loan$loan_amnt) #No missing values
anyNA(loan$grade) #No missing values
anyNA(loan$sub_grade) #No missing values
anyNA(loan$emp_length) #No missing values
anyNA(loan$home_ownership) #No missing values
anyNA(loan$verification_status) #No missing values

#Final dataset for analysis
#View(loan)

##################################################################
# We have to find driving variables to determine defaulters, so just keeping "Chraged Off" and "Fully Paid" loan_status
# remove "Current" as we cannot catagories the record into defaulter/non-defaulter
##################################################################
loan <- filter(loan,loan_status!="Current")
#View(loan)

######################################Exploring Data#######################################

###### Grades and Subgrades ######

#This P2P loan company maps borrowers to a series of grades [A-F] and subgrades [A-F][1-5] based on their risk profile. 
#Loans in each subgrade are then given appropriate interest rates. The specific rates will change over time 
#according to market conditions, but generally they will fall within a tight range for each subgrade.

#Cross table Grade
CrossTable(loan$grade, loan$loan_status,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE,prop.chisq = FALSE )

#Plot for Grade
p <- ggplot(loan)
plot_grade <- p + geom_bar(aes(x=grade,fill=factor(loan_status, levels = c("Fully Paid","Charged Off"))),position = "fill") +
        labs(title = "Ratio of loan Status for each Grade", x = "GRADE",fill = "Loan Status")  
plot_grade

#Cross table subgrade
CrossTable(loan$sub_grade, loan$loan_status,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE,prop.chisq = FALSE )

# plot for sub grade
plot_Subgrade <- p + geom_bar(aes(x=factor(sub_grade, levels=c(paste0(c(rep("A",5),rep("B",5),rep("C",5),rep("D",5),rep("E",5),rep("F",5),rep("G",5)),c(1:5)))),fill=factor(loan_status, levels = c("Fully Paid","Charged Off"))),position = "fill") +
  labs(title = "Ratio of loan Status for each SubGrade", x = "SUB GRADE",fill = "Loan Status")
plot_Subgrade

# plot_grade shows that rates of default steadily increase as the loan grades worsen from A to G, as expected.
# We see a similar pattern for the subgrades, although the trend begins to weaken across the G1-G5 subgrades. 
# On further investigation, We found that there are only a fewer data points for each of these subgrades(G1-G5), 
# in contrast to data points for the A-F subgrades, and these differences are not large enough to be significant.
# Hence G1-G5 Sub grades can be ignored

#####################################################
#####Home Ownership(home_ownership) ######
######################################################

#Frequency for home ownership
table(loan$loan_status, loan$home_ownership)
# from the above frequency table, we can determine that count/frequency of NONE and OTHER (home_ownership) are comparitely low
# so will exclude to deternine defaulters

#Cross table only for mortage, own and rent
loan1 <- filter(loan,home_ownership %in% c("MORTGAGE", "OWN", "RENT"))
CrossTable(loan1$home_ownership, loan1$loan_status,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE,prop.chisq = FALSE )

#Plot for Home ownership 
plot_home_ownership <- ggplot(filter(loan,home_ownership %in% c("MORTGAGE", "OWN", "RENT"))) + geom_bar(aes(x=home_ownership, fill=factor(loan_status, levels = c("Fully Paid","Charged Off"))),position = "fill") +
  labs(title = "Ratio of loan Status for each home_ownership", x = "Home Ownership", fill = "Loan Status")
plot_home_ownership

# So from frequency table home_ownership_status and plot_home_ownership, 
# we can say that those with mortgages default (Charged off) the least,
# followed by those who own their homes  and Finally those who rent.However, the difference looks very small.


###############################################
#####Debt to Income Ratio(dti) #####
###############################################

# Debt to income ratio indicates the ratio between a borrowers monthly debt payment and monthly income.
# Bucketing it into 5% increments to better visualize the effect on status

range(loan$dti)
loan$dti_bkt <- cut(loan$dti, breaks = c(0, 5, 10, 15, 20, 25, 30))

#Cross table
CrossTable(loan$dti_bkt,loan$loan_status,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE,prop.chisq = FALSE )
# We can see from above table that count in (25,30] is very small, so just ignoring as of now

plot_dti <- ggplot(loan[!is.na(loan$dti_bkt),]) + geom_bar(aes(x=dti_bkt,fill=factor(loan_status, levels = c("Fully Paid","Charged Off"))), position = "fill") +
  labs(title = "Ratio of loan Status for each Debt to Income brackets", x = "Debt to Income", fill = "Loan Status")
plot_dti

# From prob_dti_status and plot_dti, we can say that
# there is a steady increase in the percentage of defaulting loans as DTI increases, 
# reflecting the constraints that increased debt put onto borrower ability to repay.

######################################################
##### Revolving Utilization Percent(revol_util) #####
######################################################

#Revolving utilization percent is the portion of a borrower's revolving 
# credit limit (i.e. credit card limit) that they actually are using

# Bucketing it into 20% increments to better visualize the effect on status
range(loan$revol_util,na.rm = T)
loan$revol_util_bkt <- cut(loan$revol_util, breaks = c(0, 20, 30, 40, 50, 60, 70, 80, 100))

#Cross table
CrossTable(loan$revol_util_bkt,loan$loan_status,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE,prop.chisq = FALSE )

# Plot for Revolving utilization
plot_revol_util <- ggplot(loan[!is.na(loan$revol_util_bkt),]) + geom_bar(aes(x=revol_util_bkt,fill=factor(loan_status, levels = c("Fully Paid","Charged Off"))), position = "fill") +
  labs(title = "Ratio of loan Status for Revolving Utilization(%)", x = "Revolving Utilization Percent", fill = "Loan Status")
plot_revol_util

# From Loan status and plot_revol_util cross table and plot , we can say that
# The percentage of defaulting loans steadily increases with utilization rate. 
# Borrowers with high utilization rates are more likely to have high fixed credit card payments which might 
# affect their ability to repay their loans.

####################################
##### Loan Purpose(purpose) ########
####################################

# Loan purpose refers to the borrower's stated reason for taking out the loan.

# Cross table
CrossTable(loan$purpose,loan$loan_status,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE,prop.chisq = FALSE )

#Plot for loan purpose
plot_purpose <- p + geom_bar(aes(x=purpose,fill=factor(loan_status, levels = c("Fully Paid","Charged Off"))), position = "fill") +
  labs(title = "Ratio of loan Status for each Loan Purpose", x = "Loan Purpose", fill = "Loan Status")
plot_purpose

# From cross table and Loan status Small business loans status is very poor

##########################################################
##### Inquiries in the Past 6 Months(inq_last_6mths) #####
##########################################################

# Number of inquiries refers to the number of times a borrower's credit report is accessed by financial institutions,
# which generally happens when the borrower is seeking a loan or credit line.

# Inquiries in the last 6 months (combine factor levels for any > 4)
levels(loan$inq_last_6mths) <- c("0", "1", "2", "3", rep("4+", 5))
CrossTable(loan$inq_last_6mths,loan$loan_status,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE,prop.chisq = FALSE )

plot_inq_last_6mths <- p + geom_bar(aes(x=inq_last_6mths,fill=factor(loan_status, levels = c("Fully Paid","Charged Off"))), position = "fill") +
  labs(title = "Ratio of Loan Status against  Inquiries in the Past 6 Months", x = "Inquiries in the Past 6 Months", fill = "Loan Status")
plot_inq_last_6mths

# Analysis - More inquiries leads to higher rates of defaulters

################################################
##### Number of Total Accounts(total_acc) ######
################################################

# Number of total accounts (combine factor levels into groups of 5, then 23+)
loan$total_acc_bkt <- as.numeric(loan$total_acc)
loan$total_acc_bkt <- cut(loan$total_acc_bkt,breaks = c(0,7,12,18,22,90))

# Cross table
CrossTable(loan$total_acc_bkt,loan$loan_status,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE,prop.chisq = FALSE )

# Plot for total Account
plot_total_acc <- ggplot(loan) + geom_bar(aes(x=total_acc_bkt,fill=factor(loan_status, levels = c("Fully Paid","Charged Off"))), position = "fill") +
  labs(title = "Ratio of Loan Status against Number of Total Accounts", x = "Number of Total Accounts", fill = "Loan Status")
plot_total_acc

# Analysis - We see steady increases in the rates of non-defaulting loans as the number of accounts increases

######################################
##### Annual Income(annual_inc) ######
######################################

# Annual Income (factor into quantiles of 20%)
loan$annual_inc_bkt <- cut(loan$annual_inc,quantile(loan$annual_inc, na.rm = TRUE,probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)))

# Frequency table

CrossTable(loan$annual_inc_bkt,loan$loan_status,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE,prop.chisq = FALSE,scipen=999 )

annual_inc_status <- table(loan$loan_status, loan$annual_inc_bkt)
prop_annual_inc_status <- round(prop.table(annual_inc_status, 2) * 100, 2)

plot_annual_inc <- ggplot(loan[!is.na(loan$annual_inc_bkt),]) + geom_bar(aes(x=annual_inc_bkt,fill=factor(loan_status, levels = c("Fully Paid","Charged Off"))), position = "fill") +
  labs(title = "Ratio of Loan Status against Annual Income", x = "Annual Income", fill = "Loan Status")
plot_annual_inc

# Analysis - The higher a borrower's annual income the more likely they are to be able to repay their loans.

##################################
##### Loan Amount(loan_amnt) #####
##################################

# Loan Amount (break into < 15k, 15k - 30k, 30k - 35k)
loan$loan_amnt_bkt <- cut(loan$loan_amnt,c(0, 15000, 30000, 35000))

# Cross Table
CrossTable(loan$loan_amnt_bkt,loan$loan_status,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE,prop.chisq = FALSE )

# Plot for Applicant Annual Income
plot_annual_inc <- ggplot(loan[!is.na(loan$annual_inc_bkt),]) + geom_bar(aes(x=annual_inc_bkt,fill=factor(loan_status, levels = c("Fully Paid","Charged Off"))), position = "fill") +
  labs(title = "Ratio of Loan Status against Loan Amount", x = "Loan Amount", fill = "Loan Status")
plot_annual_inc

# Analysis - As the amount borrowed increases, we see increasing rates of defaulting loans.

#########################################
##### Employment Length(emp_length) #####
#########################################

# Cross table
CrossTable(loan$emp_length,loan$loan_status,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE,prop.chisq = FALSE)

#  Plot
plot_emp_length <- ggplot(loan) + geom_bar(aes(x=emp_length,fill=factor(loan_status, levels = c("Fully Paid","Charged Off"))), position = "fill") +
  labs(title = "Ratio of Loan Status against Employment Length", x = "Employment Length", fill = "Loan Status")
plot_emp_length

# Analysis - unemployed are most likely to default

##########################################################
##### Delinquencies in the Past 2 Years(delinq_2yrs) #####
##########################################################
#The number of delinquencies in the past 2 years indicates the number of times a borrower has been behind on payments.

table(loan$loan_status, loan$delinq_2yrs)

# Cross table for Delinquencies in the past 2 Years (combine factors levels for any > 3)
loan$delinq_2yrs_bkt <- cut(as.numeric(loan$delinq_2yrs), breaks = c(0,1,2,3,11))
CrossTable(loan$delinq_2yrs_bkt,loan$loan_status,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE,prop.chisq = FALSE)

#Plot
plot_delinq_2yrs <- ggplot(loan) + geom_bar(aes(x=delinq_2yrs_bkt,fill=factor(loan_status, levels = c("Fully Paid","Charged Off"))), position = "fill") +
  labs(title = "Ratio of Loan Status against Delinquencies in the Past 2 Years", x = "Delinquencies in the Past 2 Years", fill = "Loan Status")
plot_delinq_2yrs

# Analysis - in general , with number of delinquencies , defaulting chance increases ( we can ignore 
# is quite low) 3+ delinquencies data as frequency 

##############################################
##### Number of Open Accounts (open_acc) #####
##############################################

table(loan$loan_status, loan$open_acc)

# Number of Open Accounts (combine factor levels into groups of 5)
loan$open_acc_bkt <- cut(as.numeric(loan$open_acc), breaks = c(0,5,10,15,20, 45))

# Cross table
CrossTable(loan$open_acc_bkt, loan$loan_status,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE,prop.chisq = FALSE )

# Plot
plot_open_acc <- ggplot(loan) + geom_bar(aes(x=open_acc_bkt,fill=factor(loan_status, levels = c("Fully Paid","Charged Off"))), position = "fill") +
  labs(title = "Ratio of Loan Status against Number of Open Accounts", x = "Number of Open Accounts", fill = "Loan Status")
plot_open_acc

# Analysis - there is no as such trend

##################################################
##### Verified Income Status (verification_status)
##################################################
# Lending company categorizes income verification into three statuses: not verified, source verified, and verified. 
# Verified income means that the company independently verified both the source and size of reported income, 
# source verified means that they verified only the source of the income, 
# and not verified means there was no independent verification of the reported values.

# Cross table
CrossTable(loan$verification_status,loan$loan_status,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE,prop.chisq = FALSE )

#Plot
plot_verification_status <- ggplot(loan) + geom_bar(aes(x=verification_status,fill=factor(loan_status, levels = c("Fully Paid","Charged Off"))), position = "fill") +
  labs(title = "Ratio of Loan Status against Verified Income Status", x = "Verified Income Status", fill = "Loan Status")
plot_verification_status

# Analysis - Interestingly, we see that as income verification increases, the loan performance actually worsens.

##############################################
##### Number of Public Records (pub_rec) #####
##############################################

range(as.numeric(x = loan$pub_rec))

# Number of Public Records (break factor levels into 0, 1, 2+)
loan$pub_rec_bkt <- cut(as.numeric(loan$pub_rec), breaks = c(0,1,2,5))

#Cross tables
CrossTable(loan$pub_rec_bkt,loan$loan_status,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE,prop.chisq = FALSE )

#Plot
plot_pub_rec <- ggplot(loan) + geom_bar(aes(x=pub_rec_bkt,fill=factor(loan_status, levels = c("Fully Paid","Charged Off"))), position = "fill") +
  labs(title = "Ratio of Loan Status against Number of Public Records", x = "Number of Public Records", fill = "Loan Status")
plot_pub_rec

#Public records generally refer to bankruptcies, so we would expect those with 
# more public records to show worse performance.
# However, here there is as such trend apprearing.


#######################################
##### Interest Rate (int_rate) ########
#######################################

range(loan$int_rate)

# Number of Public Records (break factor levels into 0, 1, 2+)
loan$int_rate_bkt <- cut(loan$int_rate, breaks = c(5,10,15,20,25))

#Cross table
CrossTable(loan$int_rate_bkt,loan$loan_status,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE,prop.chisq = FALSE )

# Plot
plot_int_rate <- ggplot(loan) + geom_bar(aes(x=int_rate_bkt,fill=factor(loan_status, levels = c("Fully Paid","Charged Off"))), position = "fill") +
  labs(title = "Ratio of Loan Status against Interest Rate", x = "Interest Rate", fill = "Loan Status")
plot_int_rate

# Analysis - charge (Defaulters) Increase with increase in interest rates

#############################################
##### Installement Amount (installment) #####
#############################################

range(loan$installment)

boxplot(loan$installment)

# Installement Amount 
loan$installment_bkt <- cut(loan$installment,breaks = c(0,300,600,900,1350))

# Cross table
CrossTable(loan$installment_bkt,loan$loan_status,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE,prop.chisq = FALSE )

# Plot
plot_installment <- ggplot(loan) + geom_bar(aes(x=installment_bkt,fill=factor(loan_status, levels = c("Fully Paid","Charged Off"))), position = "fill") +
  labs(title = "Ratio of Loan Status against Installement Amount", x = "Installement Amount", fill = "Loan Status")
plot_installment

# As the installment amount increased the defaulter percentage increase as well

#########################################
##### Address State (addr_state) ########
#########################################

# Cross table
CrossTable(loan$addr_state,loan$loan_status,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE,prop.chisq = FALSE )

# Plot
plot_addr_state <- ggplot(loan) + geom_bar(aes(x=addr_state,fill=factor(loan_status, levels = c("Fully Paid","Charged Off"))), position = "fill") +
  labs(title = "Ratio of Loan Status against Address State", x = "Address State", fill = "Loan Status")
plot_addr_state

# Analysis - plot shows that Nebraska (NE) has chance of defaulting however count for the state is quite low, 
# so cannot conclude anything.

#####################################
##### Loan Issue Date (issue_d) #####
#####################################

# we have already extracted issue_year from issue_d

# Cross table
CrossTable(loan$issue_year,loan$loan_status,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE,prop.chisq = FALSE )

# Plot
plot_issue_year <- ggplot(loan) + geom_bar(aes(x=issue_year,fill=factor(loan_status, levels = c("Fully Paid","Charged Off"))), position = "fill") +
  labs(title = "Ratio of Loan Status against Loan Issued Year", x = "Loan Issued Year", fill = "Loan Status")
plot_issue_year

# Analysis - No relation found between loan status and loan issue year.

####################################
##### Term To Re-pay (term) ########
####################################

# Cross table
CrossTable(loan$term,loan$loan_status,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE,prop.chisq = FALSE )

# Plot
plot_term <- ggplot(loan) + geom_bar(aes(x=term,fill=factor(loan_status, levels = c("Fully Paid","Charged Off"))), position = "fill") +               
  labs(title = "Ratio of Loan Status against Term To Re-pay", x = "Term To Re-pay", fill = "Loan Status")
plot_term

########################################################################################################################

############################# Conclusion #####################################

"
Grade and Sub-grade variables provide the most predictive power for determining expected loan performance.

A large number of the other variables also provide strong indications of expected performance, most descriptive among them are: 
 Grades/ Sub-grades /Interest Rates
 Home Ownership Status
 Loan Purpose
 Loan Amount
 Annual Income
 Verified Income Status
 Employment Length
 Revolving Utilization Percent
 Instalment Amount
 Term to Re-pay

Verified income status and show results opposite from what we would expect. This is likely due to increased standards on borrowers with poorer credit history, so all else equal we see outperformance in these loans.
"
######################################################End ############################################