#Problem 1
#--------------------------------------------------------------------------
library(datasets)
irisData = data.frame(iris)     #loading iris data set into a dataframe
boxplot(irisData, xlab = "Attributes", ylab = "Values", main = "Iris Data")
cat("\n-----------------------------------------------------------------")
cat("\nIQR of SepalLength: ",IQR(irisData$Sepal.Length))
cat("\nIQR of SepalWidth: ",IQR(irisData$Sepal.Width))
cat("\nIQR of PetalLength: ",IQR(irisData$Petal.Length))
cat("\nIQR of PetalWidth: ",IQR(irisData$Petal.Width))
cat("\n-----------------------------------------------------------------")
cat("\nStandard Deviation of SepalLength: ",sd(irisData$Sepal.Length))
cat("\nStandard Deviation of SepalWidth: ",sd(irisData$Sepal.Width))
cat("\nStandard Deviation of PetalLength: ",sd(irisData$Petal.Length))
cat("\nStandard Deviation of PetalWidth: ",sd(irisData$Petal.Width))
cat("\n-----------------------------------------------------------------")
library(ggplot2)
ggplot(data = irisData, aes(x = Species, y = Sepal.Length, fill = Species)) + geom_boxplot() + ggtitle("SepalLength")
ggplot(data = irisData, aes(x = Species, y = Sepal.Width, fill = Species)) + geom_boxplot() + ggtitle("SepalWidth")
ggplot(data = irisData, aes(x = Species, y = Petal.Length, fill = Species)) + geom_boxplot() + ggtitle("PetalLength")
ggplot(data = irisData, aes(x = Species, y = Petal.Width, fill = Species)) + geom_boxplot() + ggtitle("PetalWidth")

#--------------------------------------------------------------------------

#Problem 2

treeData = data.frame(trees)
summary(trees$Girth)
summary(trees$Height)
summary(trees$Volume)

par(mfrow=c(1,3))
hist(trees$Girth,xlab = "Girth", main="Histogram of Girth")
hist(trees$Height,xlab = "Height", main="Histogram of Height")
hist(trees$Volume,xlab = "Volume", main="Histogram of Volume")
library(e1071)
cat("\nSkewness of Girth:",skewness(treeData$Girth))
cat("\nSkewness of Height:",skewness(treeData$Height))
cat("\nSkewness of Volume:",skewness(treeData$Volume))

#--------------------------------------------------------------------------

#Problem 3

mpgData = read.table(url("https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data"),header = F, sep = "", stringsAsFactors = F)
mpgHeader = c("mpg","cylinders","displacement","horsepower","weight","acceleration","model year","origin","car name")
colnames(mpgData) = mpgHeader                           # setting up the headers
mpgData$horsepower = as.numeric(mpgData$horsepower)     # converting factors to numeric type
cat("\nMean before replacment:",mean(mpgData$horsepower, na.rm = T))
mpg_median = median(mpgData$horsepower, na.rm = T)

mpgData$horsepower[is.na(mpgData$horsepower)] = mpg_median
cat("\nMean after replacment:",mean(mpgData$horsepower, na.rm = T))

#--------------------------------------------------------------------------
