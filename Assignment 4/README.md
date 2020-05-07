# Assignment 4


<p align="center">
  <a style="text-decoration:none" >
   <img src="https://img.shields.io/badge/Graded%20Score-100%2F100-blue" alt="version" />
 </a>
</p>

These exercises are to be found in: **Introduction to Statistical Learning,
7th Printing (Online Edition)** by *Gareth James, Daniela Witten, Trevor
Hastie, Robert Tibshirani*.

### 1. Recitation Problems

### 1.1 Chapter 8

Exercises: 1,3,4,5

### 1.2 Chapter 10

Exercises: 1,2,3,4,6

### 2. Practicum Problems

These problems will primarily reference the lecture materials and the examples
given in class using **R** and **CRAN**. It is suggested that a *RStudio* session be
used for the programmatic components.

### 2.1 Problem 1

Simulate a binary classification dataset with a single feature via a mixture of
normal distributions using **R** (**Hint:** Generate two data frames with the random
number and a class label, and combine them together). The normal distribution
parameters (using the function **rnorm**) should be (5,2) and (-5,2) for the pair of
samples - you can determine an appropriate number of samples. Induce a binary
decision tree (using **rpart**), and obtain the threshold value for the feature in
the first split. How does this value compare to the empirical distribution of
the feature? How many nodes does this tree have? What is the entropy and
Gini at each? Repeat with normal distributions of (1,2) and (-1,2). How many
nodes does this tree have? Why? Prune this tree (using **rpart.prune**) with a
complexity parameter of 0.1. Describe the resulting tree.

### 2.2 Problem 2

Load the *Wine* sample dataset from the UCI Machine Learning Repository
(wine.data) into **R** using a dataframe (**Note:** The column names will need
to be loaded separately). Use either the **prcomp** or **princomp** methods to
perform a PCA of the wine data - justify whether scaling of the inputs should
be used or not when performing the decomposition. Plot a **biplot** of the re-
sults - identify a feature which is pointed in the opposite direction of Hue in
principal component/rotated feature space. What does this imply regarding the
correlation of this feature to *Hue*? Support your result with a calcualted value.
Finally, plot a **screeplot** of your results and determine the percentage of total
variance explained by **PC1** and **PC2**.

### 2.3 Problem 3

Load the *USArrests* sample dataset from the built-in datasets (data(**USArrests**))
into **R** using a dataframe (**Note:** Row names are states, not numerical values!).
Use the **kmeans** package to perform a clustering of the data with increasing
values of k from 2 to 10 - you will need to decide whether or not to center/scale
the observations - justify your choice. Plot the *within-cluster* sum of squares for
each value of k - what is the optimal number of clusters? Use the **tidyverse** and
fviz cluster plotting method from **factoextra** to plot the optimal clustering.

### 2.4 Problem 4

Load the *Wine Quality* sample dataset from the UCI Machine Learning Reposi-
tory (**winequality-white.csv**) into **R** using a dataframe (**Note:** There is both
a red and white wine file, we will use white!). Excluding the *quality* target
variable, use hclust to perform a hierarchical clustering of the data with *sin-
gle* as well as *complete linkage*. You will need to decide on whether or not
to center/scale the observations - justify your choice. At what distance value
are the two penultimate clusters merged? Use the **cutree** method to obtain
these two clusters, and calculate their summary statistics. What feature means
have the largest differences? Which linkage method produces are more balanced
clustering?
