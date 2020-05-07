library(rpart)
library(rpart.plot)


gini = function(p) {
  gini.index = 2 * p * (1 - p)
  return (gini.index)
}

entropy = function(p) {
  entropy = (p * log(p) + (1 - p) * log(1 - p))
  return (entropy)
}

# ------------------------------------------------------------------------------
df1 = data.frame(x1 = rnorm(150,5,2),class = rep("Y",150))
df2 = data.frame(x1 = rnorm(150,-5,2),class = rep("N",150))
dataset1 = rbind(df1,df2)

fit1 = rpart(class ~ x1, method="class", data = dataset1)
printcp(fit1)  # cp = cost complexity parameter in CART

rpart.plot(fit1, main = "Classification Tree for dataset1")

p = c(.50, 0.01, 1)
gini_values = sapply(p, gini)
cat("Gini Values for dataset1: ",gini_values)
entropy_values = sapply(p, entropy)
cat("Entropy Values for dataset1: ",entropy_values)

# ------------------------------------------------------------------------------
df3 = data.frame(x1 = rnorm(250,1,2),class = rep("Y",250))
df4 = data.frame(x1 = rnorm(250,-1,2),class = rep("N",250))
dataset2 = rbind(df3,df4)

fit2 = rpart(class ~ x1, method="class", data = dataset2)
rpart.plot(fit2, main = "Classification Tree for dataset2")

p = c(.50, 0.38, 0.49,0.53,0.51,0.54,0.51,0.55,0.51,0.56,0.22,0.34,0.27,0.27,0.38,0.44,0.68,0.80,0.68,0.71,0.81)
gini_values = sapply(p, gini)
cat("Gini Values for dataset2: ",gini_values)
entropy_values = sapply(p, entropy)
cat("Entropy Values for dataset2: ",entropy_values)
# Pruning
fit2_prune = prune.rpart(fit2, cp = 0.1)
rpart.plot(fit2_prune, main = "Classification Tree for dataset2 after Pruning")

