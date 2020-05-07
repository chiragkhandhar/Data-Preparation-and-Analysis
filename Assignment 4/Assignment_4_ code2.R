library(readr)
library(data.table)
library(corrplot)

wn_URL = "https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"
wn_data = fread(wn_URL, header = FALSE)
wn_Header = c("class","Alcohol","Malic_Acid","Ash","Alcalinity","Magnesium", "Total_Phenols","Flavanoids","Nonflavanoid" ,"Proanthocyanins","Color_Intensity", "Hue", "OD280/OD315", "Proline")
colnames(wn_data) = wn_Header

apply(wn_data[,-1], 2, mean)

wn_pca = prcomp(wn_data[,-1], scale = TRUE, center = TRUE)
summary(wn_pca)
wn_pca$center
wn_pca$scale

plot(wn_pca)
biplot(wn_pca, scale = 0)

corrplot(cor(wn_data[,-1]), method="number")
cat("Correlation between Malic_Acid and Hue: ",cor(wn_data$Malic_Acid, wn_data$Hue))

std_dev = wn_pca$sdev
pr_var = std_dev^2
prop_varex = pr_var/sum(pr_var)

plot(prop_varex, xlab = "Principal Component", ylab = "Proportion of Variance Explained",type = "b")

cat("Percentage of total variance explained by PC1 and PC2 = ", (prop_varex[1]+prop_varex[2])*100,"%")
