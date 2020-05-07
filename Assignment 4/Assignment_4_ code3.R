library("factoextra")

data = data.frame(USArrests)     
states = row.names(data) 
states
names(data)

# Before Scaling
head(data)
summary(data)

# Scaling
new_data = scale(data)

# After Scaling
head(new_data)
summary(new_data)

withinss = c()
i = 2
for (k in 2:10){
  set.seed(123)
  model = kmeans(new_data, k, nstart = 25)
  withinss[i] = model$tot.withinss
  cat(" k = ",k,"Withinss = ",withinss[i],"\n")
  i = i + 1
}

plot(withinss, xlab = "Values of K", main = "Values of within-cluster sum of squares for each value of k ", type = "b", col = "dark red")

model = kmeans(new_data, 4, nstart = 25)
fviz_cluster(model, data = new_data,ggtheme = theme_minimal(),main = " Optimal Clustering Plot for K = 4")
