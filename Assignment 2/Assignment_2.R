#Problem 1
#--------------------------------------------------------------------------
library(MASS)
library(ggplot2)
bostonData = data.frame(Boston)
attach(Boston)
#--------------------------------------------------------------------------
linearModel1 = lm(medv~lstat, data = bostonData)
summary(linearModel1)
coef(linearModel1)
cat("R-squared for Linear Model: ",summary(linearModel1)$r.sq)
#plot(lstat,medv)
#abline(linearModel1, col="red")
plot(linearModel1)
ggplot(bostonData, aes(lstat, medv)) +  geom_point() +  stat_smooth(method = lm, se =  TRUE) + ggtitle("Linear Fit")
confint(linearModel1)
predict(linearModel1,data.frame(lstat=(c(5,10,15))), interval="confidence")
predict(linearModel1,data.frame(lstat=(c(5,10,15))), interval="prediction")
ggplot(linearModel1, aes(x =linearModel1$fitted.values, y = linearModel1$residuals)) + geom_point() + stat_smooth(se = TRUE) + labs(x = "Fitted Values", y = "Residuals") + ggtitle("Fitted Values VS Residuals for Linear Model")
#--------------------------------------------------------------------------
cat("Modifying the model to include lstat^2")
linearModel2=lm(medv~lstat + I(lstat^2))
summary(linearModel2)
coef(linearModel2)
cat("R-squared for Non-Linear Model: ",summary(linearModel2)$r.sq)
#To compare two models
anova(linearModel1,linearModel2)
ggplot(bostonData,aes(x= lstat,y =  medv)) + geom_point() + stat_smooth(method = "lm", formula = y ~ x + I(x^2), se = TRUE) + ggtitle("Non Linear Fit")
ggplot(linearModel2, aes(x =linearModel2$fitted.values, y = linearModel2$residuals)) + geom_point() + stat_smooth(se = TRUE) + labs(x = "Fitted Values", y = "Residuals") + ggtitle("Fitted Values VS Residuals for Non-Linear Model")
#--------------------------------------------------------------------------

#Problem 2
#--------------------------------------------------------------------------

library(readr)
library(data.table)
library(corrplot)
library(caret)

abaloneURl = "https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"
abaloneData = fread(abaloneURl, header = FALSE) 
abaloneHeader = c("Sex","Length","Diameter","Height","Whole weight","Shucked weight","Viscera weight","Shell weight","Rings")
colnames(abaloneData) = abaloneHeader

#Direct use of attribute names in code
attach(abaloneData)

#Returns the indices when the following condition is true in which()
condition = which(abaloneData$Sex!="I")

#All the Rows with Sex = I are removed
abaloneData2 = abaloneData[condition]

#As the response variable need to be numeric, converting categorical Sex to a factor
abaloneData2$Sex = factor(abaloneData2$Sex)

#Creating 80-20 Training Testing Split, createDataPartition() returns the indices
trainIndex = createDataPartition(y = abaloneData2$Sex, p = 0.8, list = FALSE)

#Training data
trainData = abaloneData2[trainIndex,]

#Testing data (note the minus sign)
testData = abaloneData2[-trainIndex,]    

#Predicting Sex using glm
model <- glm(Sex~.,family=binomial,data=trainData)

#Summary of our model
summary(model)

#Coefficients of our model
coef(model)

#Confidence Interval
confint(model)

#Predict [By setting the parameter type='response', R will output probabilities in the form of P(y=1|X)]
probs = predict(model, testData, type = "response")

#Using a 50% cut-off factor i.e probabilities > 0.5 are Males and rest are Females 
resultSet = ifelse(probs > 0.5,"M","F")
resultSet2 = factor(resultSet)

#Creating a confusion matrix
confusionMatrix(resultSet2, testData$Sex)

#Plotting the ROC Curve
library(ROCR)
roc.pred = prediction(probs,testData$Sex)
roc.perf = performance(roc.pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf)
abline(0,1)
auc.perf = performance(roc.pred, measure = "auc")
cat("Area Under the Curve: ")
auc.perf@y.values

#Plotting the correlations between the predictors
cm = cor(abaloneData2[,-1])
corrplot(cm, method = "number")


#Problem 3
#--------------------------------------------------------------------------
library(data.table) #Data Import
library(e1071)      #Naive Bayes

#Setting up the URL for data import
mushroomURL = "https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data"
mushroomData = fread(mushroomURL,header=FALSE)

#Adding the headers
mushroomHeader = c("Class","cap-shape","cap-surface","cap-color","bruises","odor","gill-attachment","gill-spacing","gill-size","gill-color","stalk-shape","stalk-root","stalk-surface-above-ring","stalk-surface-below-ring","stalk-color-above-ring","stalk-color-below-ring","veil-type","veil-color","ring-number","ring-type","spore-print-color","population","habitat")
colnames(mushroomData) = mushroomHeader

#Class Distribution
table(mushroomData$Class)

#Converting Class Attribute to a factor
mushroomData$Class = factor(mushroomData$Class)

#Dimensions
dim(mushroomData)

#Structure 
str(mushroomData)

#Finding the number of missing values
cat("Number of missing values = ",sum(mushroomData=="?"))

#New dataset with removed missing values
mushroomData2 = mushroomData[mushroomData$`stalk-root`!="?"]

#Creating a split
trainSize = floor(0.80*nrow(mushroomData2))
trainIndex = sample(nrow(mushroomData2), size = trainSize)
trainData = mushroomData2[trainIndex,]
testData = mushroomData2[-trainIndex,]

#Naive Bayes 
model = naiveBayes(trainData[,-1],trainData$Class)

#Prediction on Testing Data
testPred = predict(model,testData[,-1])

#Prediction on Training Data
trainPred = predict(model,trainData[,-1])

#Accuracy of Testing Model
cat("Accuracy of Testing Model: ",mean(testPred == testData$Class)*100,"%")

#Accuracy of Training Model
cat("Accuracy of Training Model: ",mean(trainPred == trainData$Class)*100,"%")

#Confusion Matrix
table(testPred, testData$Class)
