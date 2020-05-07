# Problem 1
###################################################################

library(readr)
library(data.table)
library(caret)


hd_URL = "http://archive.ics.uci.edu/ml/machine-learning-databases/00243/yacht_hydrodynamics.data"
hd_data = fread(hd_URL, header = FALSE)

hd_Header = c("longitudinalPos","prismaticCoef","LDR","BDR","LBR","froudeNo","Residuary")
colnames(hd_data) = hd_Header

#Direct use of attribute names in code
attach(hd_data)

#Creating 80-20 Training Testing Split, createDataPartition() returns the indices
trainIndex = createDataPartition(y = hd_data$Residuary , p = 0.8, list = FALSE)

#Training data
trainData = hd_data[trainIndex,]

#Testing data (note the minus sign)
testData = hd_data[-trainIndex,]    

#Training fit for linear model
linearModel1 = lm(hd_data$Residuary~hd_data$longitudinalPos + hd_data$prismaticCoef + hd_data$LDR + hd_data$BDR + hd_data$BDR + hd_data$LBR +hd_data$froudeNo, data = trainData)

# Function to compute MSE
MSE = function(yActual, yPred)
{
  return (mean((yActual - yPred)^2))
}
mse1 = MSE(hd_data$Residuary, linearModel1$fitted.values )

# Summarize the results
cat("Training MSE: ", mse1)
cat("Training RMSE: ", sqrt(mse1))
cat("Training R-squared: ",summary(linearModel1)$r.sq)

# Define training control
train.control =  trainControl(method = "boot", number = 1000)

# Train the model
linearModel2 = train(Residuary~., data = trainData, method = "lm", trControl = train.control)

# 5 Point Summary for resulting RMSE for each resample  
summary(linearModel2$resample$RMSE)

# 5 Point Summary for resulting R-Squared for each resample 
summary(linearModel2$resample$Rsquared)

# Histogram of RMSE values
hist(linearModel2$resample$RMSE, xlab = "RMSE Values", main = "Histogram of the RMSE values")

# Calculating the MSE from RMSE
mse2 = mean(linearModel2$resample$RMSE)^2

# Summarize the results
cat("Training Mean MSE (Bootstrap): ", mse2)
cat("Training Mean RMSE (Bootstrap): ", mean(linearModel2$resample$RMSE))
cat("Training Mean R-squared (Bootstrap): ",mean(linearModel2$resample$Rsquared))

predVals_boot = predict(linearModel2,testData)
mse3 = MSE(testData$Residuary, predVals_boot)


RSS = function (yActual, yPred)
{
  return (sum((yActual - yPred)^2))
}

TSS = function (yActual)
{
  return (sum((yActual - mean(yActual))^2))
}

rss = RSS(testData$Residuary, predVals_boot)
tss = TSS(testData$Residuary)

# Summarize the results
cat("Testing MSE (Bootstrap): ", mse3)
cat("Testing RMSE (Bootstrap): ", sqrt(mse3))
cat("Testing Mean R-squared (Bootstrap): ",1 - (rss/tss))

# Problem 2
###################################################################
library(readr)
library(data.table)
library(caret)

gcd_URL = "https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data-numeric"
gc_data = fread(gcd_URL, header = FALSE)

#As the response variable need to be numeric, converting categorical V25 to a factor
gc_data$V25 = factor(gc_data$V25)

#Creating 80-20 Training Testing Split, createDataPartition() returns the indices
trainIndex = createDataPartition(y = gc_data$V25 , p = 0.8, list = FALSE)

#Training data
trainData = gc_data[trainIndex,]

#Testing data (note the minus sign)
testData = gc_data[-trainIndex,] 

# Creating model for y = V25 using glm
logisticModel1 = glm(V25~.,family=binomial,data=trainData)

actualVals = trainData$V25

# Using a 50% cut-off factor i.e probabilities > 0.5 are 2 and rest are 1 
fittedVals = ifelse(logisticModel1$fitted.values > 0.5,2,1)
fittedVals = factor(fittedVals)

# Confusion matrix
cm = confusionMatrix(fittedVals, trainData$V25)
cm

# Summarize the results
cat("Training Precision: ", cm$byClass[5] * 100, "%")
cat("Training Recall: ", cm$byClass[6] * 100, "%")
cat("Training F1-Score: ", cm$byClass[7] * 100, "%")

# Predict [By setting the parameter type='response', R will output probabilities in the form of P(y=1|X)]
probs = predict(logisticModel1, testData, type = "response")

#Using a 50% cut-off factor i.e probabilities > 0.5 are Males and rest are Females 
fittedVals_test = ifelse(probs > 0.5,2,1)
fittedVals_test = factor(fittedVals_test)

# Confusion matrix
cm_test = confusionMatrix(fittedVals_test, testData$V25)
cm_test

# Summarize the results
cat("Testing Precision: ", cm_test$byClass[5] * 100, "%")
cat("Testing Recall: ", cm_test$byClass[6] * 100, "%")
cat("Testing F1-Score: ", cm_test$byClass[7] * 100, "%")
#--------------------Cross-Validation---------------------------
# Define training control
train.control =  trainControl(method = "cv", number = 10)

# Train the model
logisticModel2 = train(V25~., data = trainData, method = "glm", family = "binomial", trControl = train.control)

fittedVals_cv = ifelse(logisticModel2$finalModel$fitted.values > 0.5,2,1)
fittedVals_cv = factor(fittedVals_cv)

# Confusion matrix
cm_cv = confusionMatrix(fittedVals_cv, trainData$V25)
cm_cv

# Summarize the results
cat("Training Precision with 10-fold CV: ", cm_cv$byClass[5] * 100, "%")
cat("Training Recall with 10-fold CV: ", cm_cv$byClass[6] * 100, "%")
cat("Training F1-Score with 10-fold CV: ", cm_cv$byClass[7] * 100, "%")

# Predict [By setting the parameter type='response', R will output probabilities in the form of P(y=1|X)]
probs_cv = predict(logisticModel2, testData, type = "prob")

#Using a 50% cut-off factor i.e probabilities > 0.5 are Males and rest are Females 
fittedVals_cv_test = ifelse(probs > 0.5,2,1)
fittedVals_cv_test = factor(fittedVals_test)

# Confusion matrix
cm_cv_test = confusionMatrix(fittedVals_test, testData$V25)
cm_cv_test

# Summarize the results
cat("Testing Precision: ", cm_cv_test$byClass[5] * 100, "%")
cat("Testing Recall: ", cm_cv_test$byClass[6] * 100, "%")
cat("Testing F1-Score: ", cm_cv_test$byClass[7] * 100, "%")

###################################################################
# Problem 3
data("mtcars")

# Creating 80-20 Training Testing Split, createDataPartition() returns the indices
smp_size <- floor(0.8 * nrow(mtcars))

# Setting the seed to make your partition reproducible
set.seed(123)

train_ind <- sample(seq_len(nrow(mtcars)), size = smp_size)

# Training data
trainData = mtcars[train_ind, ]

# Testing data (note the minus sign)
testData = mtcars[-train_ind, ] 

# Fitting linear model
linearModel1 = lm(mpg ~ ., trainData)

# Analyze the t-stat and p-values to select relevant features
summary(linearModel1)

# coefficient values for relevant features 
linearModel1$coefficients

# Predict out-of-sample
predicted = predict(linearModel1, testData, type = "response")

# Evaluate error
actual = testData[, "mpg"]
cat("Out-of-Sample test MSE for regular linear model  = ", mean((predicted - actual)^2))
#---------------------Ridge-Regression---------------------
library(glmnet)

# Lamda vector of 101 elements Ranging from 0 - 100000
lambda_seq = 10^seq(5, -5, by = -.1)

# Extract x and y from training data
y = trainData$mpg
x = model.matrix(mpg~. ,trainData)[,-1]

# Cross-validation to perform minimum lambda
cv_fit = cv.glmnet(x, y, alpha = 0, lambda = lambda_seq)

opt_lambda = cv_fit$lambda.min
cat("Optimal Lambda = ",opt_lambda)

# Fitting Ridge Regression with optimal lambda
fit = glmnet(x, y, alpha = 0, lambda = opt_lambda)

# Plot the model
plot(cv_fit)

# Coeff. of Ridge Regression
coef(fit)

x_ridge = model.matrix(mpg~. ,testData)[,-1]

# Predicting on out-of-sample test data
predicted_rdg = predict(fit, s = opt_lambda, newx = x_ridge)


# Evaluate error
actual = testData[, "mpg"]
cat("Out-of-Sample test MSE with Ridge Regression  = ", mean((predicted_rdg - actual)^2))

###################################################################
# Problem 4
data("swiss")

# Creating 80-20 Training Testing Split, createDataPartition() returns the indices
smp_size <- floor(0.8 * nrow(swiss))

# Setting the seed to make your partition reproducible
set.seed(123)

train_ind <- sample(seq_len(nrow(swiss)), size = smp_size)

# Training data
trainData = swiss[train_ind, ]

# Testing data (note the minus sign)
testData = swiss[-train_ind, ] 

# Fitting linear model
linearModel1 = lm(Fertility ~ ., trainData)

# Analyze the t-stat and p-values to select relevant features
summary(linearModel1)

# coefficient values for relevant features 
linearModel1$coefficients

# Predict out-of-sample
predicted = predict(linearModel1, testData, type = "response")

# Evaluate error
actual = testData[, "Fertility"]
cat("Out-of-Sample test MSE for regular linear model  = ", mean((predicted - actual)^2))
#---------------------Lasso-Regression---------------------
library(glmnet)

# Lamda vector of 101 elements Ranging from 0 - 100000
lambda_seq = 10^seq(5, -5, by = -.1)

# Extract x and y from training data
y = trainData$Fertility
x = model.matrix(Fertility~. ,trainData)[,-1]

# Cross-validation to perform minimum lambda
cv_fit = cv.glmnet(x, y, alpha = 1, lambda = lambda_seq)

opt_lambda = cv_fit$lambda.min
cat("Optimal Lambda = ",opt_lambda)

# Fitting Lasso Regression with optimal lambda
fit = glmnet(x, y, alpha = 1, lambda = opt_lambda)

# Plot the model
plot(cv_fit)

# Coeff. of Lasso Regression
coef(fit)

x_lasso = model.matrix(Fertility~. ,testData)[,-1]

# Predicting on out-of-sample test data
predicted_lso = predict(fit, s = opt_lambda, newx = x_lasso)


# Evaluate error
actual = testData[, "Fertility"]
cat("Out-of-Sample test MSE with Lasso Regression  = ", mean((predicted_lso - actual)^2))
