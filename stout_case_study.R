library(tidyverse)
library(ggplot2)
library(Hmisc)
library(corrplot)
library(caTools)
library(randomForest)
library(randomForestExplainer)
library(car)

loans <- read.csv('C:/Users/jried/Downloads/loans_full_schema.csv')

data <- subset(loans, select = -c(emp_title))
states <- factor(data$state)

#Tranfsorm emp_length

data$emp_length <- replace_na(data$emp_length, mean(data$emp_length, na.rm = T))

#Transform home ownership

data <- data %>%
  mutate(dMortgage = ifelse(homeownership == "MORTGAGE", 1, 0),
         dRent = ifelse(homeownership == "RENT", 1, 0),
         dOwn = ifelse(homeownership == "OWN", 1, 0))
data <- subset(data, select = -c(homeownership))

#Transform verified income

data <- data %>%
  mutate(dNotVerified = ifelse(verified_income == "Not Verified", 1, 0),
         dSourceVerified = ifelse(verified_income == "Source Verified", 1, 0),
         dVerified = ifelse(verified_income == "Verified", 1, 0))
data <- subset(data, select = -c(verified_income))

#Transform income/joint income

data <- data %>%
  mutate(mIncome = ifelse(is.na(annual_income_joint), annual_income, annual_income_joint))
data <- subset(data, select = -c(annual_income_joint, annual_income))

#Transform loan purpose

data <- data %>%
  mutate(dCar = ifelse(loan_purpose == "car", 1, 0),
         dCreditCard = ifelse(loan_purpose == "credit_card", 1, 0),
         dDebtConsolidation = ifelse(loan_purpose == "debt_consolidation", 1, 0),
         dHomeImprovement = ifelse(loan_purpose == "home_improvement", 1, 0),
         dHouse = ifelse(loan_purpose == "house", 1, 0),
         dMajorPurchase = ifelse(loan_purpose == "major_purchase", 1, 0),
         dMedical = ifelse(loan_purpose == "medical", 1, 0),
         dMoving = ifelse(loan_purpose == "moving", 1, 0),
         dOther = ifelse(loan_purpose == "other", 1, 0),
         dRenewableEnergy = ifelse(loan_purpose == "renewable_energy", 1, 0),
         dSmallBusiness = ifelse(loan_purpose == "small_business", 1, 0),
         dVacation = ifelse(loan_purpose == "vacation", 1, 0))

#Transform grade

data <- data %>%
  mutate(dGrade = ifelse(grade == 'A', 6, 
                         ifelse(grade == 'B', 5, 
                                ifelse(grade == 'C', 4,
                                      ifelse(grade == 'D', 3,
                                             ifelse(grade == 'E', 2,
                                                    ifelse(grade == 'F', 1, 0)))))))

# Transform application type

data$application_type <- ifelse(data$application_type == 'joint', 1, 0)

#Transform debt to income

data$debt_to_income <- replace_na(data$debt_to_income, mean(data$debt_to_income, na.rm = T))

#Create featured set

feat_set <- subset(data, select = c(emp_length, debt_to_income, delinq_2y, open_credit_lines, num_active_debit_accounts, 
                                    total_credit_utilized, total_credit_limit, total_debit_limit, num_historical_failed_to_pay,
                                    current_accounts_delinq, total_collection_amount_ever, num_total_cc_accounts, num_open_cc_accounts,
                                    num_cc_carrying_balance, num_mort_accounts, account_never_delinq_percent, public_record_bankrupt,
                                    application_type, loan_amount, term, interest_rate, installment,
                                    dMortgage, dRent, dOwn, dNotVerified, dSourceVerified, dVerified, 
                                    mIncome, dCar, dCreditCard, dDebtConsolidation, dHomeImprovement,
                                    dHouse, dMajorPurchase, dMedical, dMoving, dOther, dRenewableEnergy,
                                    dSmallBusiness, dVacation, dGrade, balance, paid_principal, paid_interest))

# Linear regression model

lin.mod <- lm(interest_rate ~ ., data = feat_set)                   
summary(lin.mod)                   

# Random Forest Model

# Train data

sample <- sample.split(feat_set$interest_rate, SplitRatio = .1)
train <- subset(feat_set, sample == TRUE)
test <- subset(feat_set, sample == FALSE)

rf <- randomForest(interest_rate ~ ., data=train, localImp = T)
print(rf)

# Correlation Matrix

corr_matrix <- rcorr(as.matrix(feat_set))

corrplot(corr_matrix$r, type = "upper", order = "hclust", tl.col = "black", 
         tl.cex = 0.5)

# Minimal Depth

plot_min_depth_distribution(rf, mean_sample = "relevant_trees")

# Importance Measures

plot_importance_ggpairs(rf)

# AV Plots of Significant Variables

avPlots(lin.mod, terms = ~debt_to_income + num_mort_accounts + loan_amount + term + installment + 
                           dCreditCard + dHomeImprovement + dGrade + paid_interest, 
        layout = c(3,3), id = F, grid = T)

# Mean min depth interactions

plot_multi_way_importance(rf, size_measure = "no_of_nodes")



