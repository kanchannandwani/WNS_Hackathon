#.................To predict whether a potential promotee at checkpoint in the test set will be promoted or not after the evaluation process..........................#

library(MASS)
library(caret)
library(ggplot2)
library(Information)
library(caTools)
library(ROCR)
library(dummies)
library(car)

#..............Read the file..............#
train <- read.csv("~/Desktop/Hackathon/Train.csv" , na.strings = c("" , "NA" , "-" , "na") , stringsAsFactors = FALSE)
View(train)
str(train)
dim(train)
summary(train)

# Data Preparation - Checking for the missing values
sum(is.na(train))
sum(is.na(train$education))# 2409  missing values
sum(is.na(train$previous_year_rating)) # 4124 missing values
summary(train)

train$previous_year_rating[is.na(train$previous_year_rating)] <- 0

#Checking for the outliers
n<-boxplot(train$no_of_trainings)
quantile(train$no_of_trainings,c(0.01,0.02,0.03,0.1,0.2,0.3,0.4,0.50,0.6,0.7,0.8,0.9,1))
train[(which(train$no_of_trainings>4)),]$no_of_trainings <- 4

m<-boxplot(train_data$age)
quantile(train$age,c(0.01,0.02,0.03,0.1,0.2,0.3,0.4,0.50,0.6,0.7,0.8,0.9,0.91,0.92,0.93,0.94,0.95,0.97,0.98,0.99,1))
train[(which(train$age>51)),]$age <- 51

o <-boxplot(train_data$previous_year_rating)
quantile(train$previous_year_rating,c(0.01,0.02,0.03,0.1,0.2,0.3,0.4,0.50,0.6,0.7,0.8,0.9,0.91,0.92,0.93,0.94,0.95,0.97,0.98,0.99,1))

q<-boxplot(train_data$length_of_service)
quantile(train$length_of_service,c(0.01,0.02,0.03,0.1,0.2,0.3,0.4,0.50,0.6,0.7,0.8,0.9,0.91,0.92,0.93,0.94,0.95,0.97,0.98,0.99,1))
train[(which(train$length_of_service > 15)),]$length_of_service <- 15

p<-boxplot(train_data$avg_training_score)
quantile(train$avg_training_score,c(0.01,0.02,0.03,0.1,0.2,0.3,0.4,0.50,0.6,0.7,0.8,0.9,0.91,0.92,0.93,0.94,0.95,0.97,0.98,0.99,1))

# Converting the characters into factors
train$department <- as.factor(train$department)
train$education <- as.factor(train$education)
train$gender <- as.factor(train$gender)
train$recruitment_channel <- as.factor(train$recruitment_channel)
train$KPIs_met..80. <- as.factor(train$KPIs_met..80.)
train$awards_won. <- as.factor(train$awards_won.)
train$is_promoted <- as.factor(train$is_promoted)

#Creating the dummy variables
train <- dummy.data.frame(train, names=c("department", "education", "gender","recruitment_channel","KPIs_met..80.","awards_won."), sep ="-")
colnames(train)
View(train)
 


# Remove Extra column
train <- train[ , -c(15)]

################################## Model Building #######################################

##-------------------------Logistic Regression-------------------------#

#---------------------------------------------------------    






# splitting into train and test data

set.seed(10)

split_indices <- sample.split(train$is_promoted , SplitRatio = 0.70)

train1 <- train[split_indices, ]

test1 <- train[!split_indices, ]


#---------------------------------------------------------    
### Modelling: Logistic Regression

logistic_1 <- glm(is_promoted ~ ., family = "binomial", data = train1[,-1])

summary(logistic_1)

# Using stepwise algorithm for removing insignificant variables 

logistic_2 <- stepAIC(logistic_1, direction = "both",k=5)

summary(logistic_2)
# checking vif for logistic_2 

vif(logistic_2)

# Removing " region" variable.
logistic_3 <- glm(formula = is_promoted ~ department_Analytics + department_Finance + 
                    department_HR + department_Legal + department_Operations + 
                    department_Procurement + `department_R&D` + `department_Sales & Marketing`  
                  #region 
                  + `education_Bachelor's` + `education_Master's & above` + 
                    no_of_trainings + age + previous_year_rating + length_of_service + 
                    KPIs_met..80._0 + awards_won._0 + avg_training_score, family = "binomial", 
                  data = train1[, -1])
#checking for summary
summary(logistic_3)
# checking vif for logistic_3 
vif(logistic_3)


#Assign logistic_3 to the logistic_final as the final variable
logistic_final <- logistic_3


# Predicting probabilities of responding for the test data

predictions_logit <- predict(logistic_final, newdata = test1[, -c(1)], type = "response")
pred_q <- ifelse(predictions_logit > 0.5 , 1 , 0)
confusion_Matrix <- table(test1[,27] , pred_q > 0.5)

precision = 14950/(14950 + 92)
recall =  14950 / (14950 + 92)

# Evaluating F1 score

Fscore <- (2 * precision * recall)/(precision + recall)
Fscore

##################******************************************************#########################**************************************


# Outer Sample Testing - Testing on Test Data



test <- read.csv("~/Desktop/Hackathon/Test.csv" , header=T,
                 na.strings = c("" , "NA" , "-" , "na") , stringsAsFactors = FALSE )

#Checking For the Missing Values
sum(is.na(test)) # Total Missing Values : 6533
sum(is.na(test$education)) # Missing Values : 1034
sum(is.na(test$previous_year_rating)) # Missing Values : 1812




# Checking for the Outlier Treatment
quantile(test$no_of_trainings , c(0.01,0.02,0.03,0.1,0.2,0.3,0.4,0.50,0.6,0.7,0.8,0.9,0.95,0.99,1))

test[(which(test$no_of_trainings>4)), ]$no_of_trainings <- 4    

quantile(test$age , c(0.01,0.02,0.03,0.1,0.2,0.3,0.4,0.50,0.6,0.7,0.8,0.9,0.95,0.99,1))
test[(which(test$age >= 55)) , ]$age  <- 55

test$previous_year_rating[is.na(test$previous_year_rating)] <-  0
quantile(test$previous_year_rating , c(0.01,0.02,0.03,0.1,0.2,0.3,0.4,0.50,0.6,0.7,0.8,0.9,0.95,0.99,1))

quantile(test$length_of_service , c(0.01,0.02,0.03,0.1,0.2,0.3,0.4,0.50,0.6,0.7,0.8,0.9,0.95,0.99,1))
test[(which(test$length_of_service >= 15)), ]$length_of_service <- 15 
quantile(test$avg_training_score , c(0.01,0.02,0.03,0.1,0.2,0.3,0.4,0.50,0.6,0.7,0.8,0.9,0.95,0.99,1))


# converting into factor variables
table(test$department)
test$department <- as.factor(test$department)
test$gender <- as.factor(test$gender)
table(test$recruitment_channel)
test$recruitment_channel <- as.factor(test$recruitment_channel)
table(test$education)
test$education <- as.factor(test$education)

table(test$KPIs_met..80.)
test$KPIs_met..80. <- as.factor(test$KPIs_met..80.)
test$awards_won. <- as.factor(test$awards_won.)

 # Checking for the dummy variables
test <- dummy.data.frame(test, names=c("department", "gender",
                                       "education", "recruitment_channel",
                                       "awards_won.", "KPIs_met..80."), sep="_")


View(test)
colnames(test) 
dim(test)


pred <- predict(logistic_final , newdata = test , type = "response")

y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- train$is_promoted

y_pred

mean(y_pred == y_act)

test$is_promoted <- y_pred

# Write the .CSV file
getwd()
write.csv(test[c("employee_id","is_promoted")] ,"Output_WNS_Hackathon_Kanchan.csv")

