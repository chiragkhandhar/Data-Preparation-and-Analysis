library(readr)
library(data.table)
library(corrplot)
library(dplyr)

wq_URL = "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"
wq_raw_data = fread(wq_URL, header = TRUE)
wq_data = wq_raw_data[,-12]

head(wq_data)
summary(wq_data)

wq_data_scale = scale(wq_data)

head(wq_data_scale)
summary(wq_data_scale)

# --------------------------------------------------------------------------------------------------
hc.complete = hclust (dist(wq_data_scale), method = "complete")
plot(hc.complete, main =" Complete Linkage ", xlab = "")
abline(h =tail(hc.complete$height,1), col="red")
cat("Distance value where the two penultimate clusters merge =", tail(hc.complete$height,1),"\n")
hc.completeCut = cutree(hc.complete, 2)
table(hc.completeCut)
wq_data2 = wq_data
wq_data2$Clusters = hc.completeCut
wq_data2 = dplyr::group_by(wq_data2,Clusters)
a = dplyr::summarise_each(wq_data2,funs(mean))
print.data.frame(a)
# Feature Means Difference
abs(a[2,-1]-a[1,-1])
# --------------------------------------------------------------------------------------------------
hc.single = hclust (dist(wq_data_scale), method = "single")
plot(hc.single, main =" Single Linkage ", xlab = "")
abline(h =tail(hc.single$height,1), col="red")
cat("Distance value where the two penultimate clusters merge =", tail(hc.single$height,1),"\n")
hc.singleCut = cutree(hc.single, 2)
table(hc.singleCut)
wq_data3 = wq_data
wq_data3$Clusters = hc.singleCut
wq_data3 = dplyr::group_by(wq_data3,Clusters)
b = dplyr::summarise_each(wq_data3,funs(mean))
print.data.frame(b)
# Feature Means Difference
abs(b[2,-1]-b[1,-1])
# --------------------------------------------------------------------------------------------------
