#Tutorial By Analytics Vidhya

# Load the required library
library('caret')
library('RANN')
# Set seed to Random
set.seed(1)

setwd('C:/Users/aanirudh/Downloads')
# Load the Dataset
data<-read.csv('loan.csv')

#Structure of Data
str(data)

#Does data contain missing value
sum(is.na(data))
#Imputing missing values using median
preProcValues <- preProcess(data, method = c("medianImpute","center","scale"))
data_preprocessed <- predict(preProcValues, data)
sum(is.na(data_preprocessed))

#Spliting training set into two parts based on outcome: 75% and 25%
index <- createDataPartition(data_preprocessed$Loan_Status, p =0.75, list = FALSE)
trainset <- data_preprocessed[index,]
testset <- data_preprocessed[-index,]

# Defining training controls for multiple models

fitControl <- trainControl(method = "cv",number = 5,savePredictions = 'final',classProbs = T)

#Define predictors and outcome
predictors<-c("Credit_History", "LoanAmount", "Loan_Amount_Term", "ApplicantIncome",
              "CoapplicantIncome")
outcomeName <- 'Loan_Status'
#Training the random forest model
model_rf <- train(trainset[,predictors],trainset[,outcomeName], method = 'rf' ,trControl = fitControl, tuneLength = 3)
#Predicting using random forest model
testset$pred_rf <- predict(object = model_rf , testset[,predictors])

#Checking the accuracy of the random forest model
confusionMatrix(testset$Loan_Status, testset$pred_rf)

# Let's Try with KNN
model_knn <- train(trainset[,predictors], trainset[,outcomeName], method = 'knn', trControl = fitControl, tuneLength = 3)
#Predicting using KNN
testset$pred_knn <- predict(object = model_knn, testset[,predictors])

#Check confusion matrix of the KNN Model
confusionMatrix(testset$Loan_Status, testset$pred_knn)

#Let's try Logistic regression model
model_lr <- train(trainset[,predictors], trainset[,outcomeName], method = 'glm', trControl = fitControl, tuneLength = 3)
#Predicitng using Logistic Regression
testset$pred_lr <- predict(object = model_lr, testset[,predictors])

confusionMatrix(testset$Loan_Status, testset$pred_lr)
#Averaging

testset$pred_rf_prob<-predict(object = model_rf,testset[,predictors],type='prob')
testset$pred_knn_prob <- predict(object = model_knn, testset[,predictors], type = 'prob')
testset$pred_lr_prob <- predict(object = model_lr, testset[,predictors], type = 'prob')

testset$pred_avg<-(testset$pred_rf_prob$Y+testset$pred_knn_prob$Y+testset$pred_lr_prob$Y)/3
#Splitting into binary classes at 0.5
testset$pred_avg<-as.factor(ifelse(testset$pred_avg>0.5,'Y','N'))

#CONFUSION Matrix
confusionMatrix(testset$Loan_Status, testset$pred_avg)

# Majority Voting

testset$pred_majority <- as.factor(ifelse(testset$pred_rf == 'Y' & testset$pred_knn =='Y', 'Y', 
                                          ifelse(testset$pred_rf == 'Y' & testset$pred_lr == 'Y', 'Y',
                                                 ifelse(testset$pred_knn == 'Y' & testset$pred_lr == 'Y', 'Y','N'))))
confusionMatrix(testset$Loan_Status, testset$pred_majority)

#Weighted Average

testset$pred_weighted_avg <- (testset$pred_knn_prob$Y * 0.25) + (testset$pred_rf_prob$Y * 0.25) + (testset$pred_lr_prob$Y * 0.5)
#Splitting into binary classes at 0.5
testset$pred_weighted_avg<-as.factor(ifelse(testset$pred_weighted_avg>0.5,'Y','N'))

confusionMatrix(testset$Loan_Status, testset$pred_weighted_avg)
