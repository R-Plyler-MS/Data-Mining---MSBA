#normalize data
#standardize Sepal.Width
head(iris)
iris.df.c<-iris
iris.df.c$Sepal.Width <- (iris.df.c$Sepal.Width - mean(iris.df.c$Sepal.Width)) / sd(iris.df.c$Sepal.Width)
head(iris.df.c)
iris.df.c$Petal.Width <- scale(iris.df.c$Petal.Width)
head(iris.df.c)
idc<-iris
idc$Sepal.Width<-scale(idc$Sepal.Width)
head(idc)
scale(idc[,1:4])

#define Min-Max normalization function
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

#apply Min-Max normalization to first four columns in iris dataset
iris_norm <- as.data.frame(lapply(iris[1:4], min_max_norm))

#view first six rows of normalized iris dataset
head(iris_norm)
#apply Min-Max normalization to first four columns in iris dataset
iris_norm_a <- apply(iris[1:4],2, min_max_norm)

#view first six rows of normalized iris dataset
head(iris_norm_a)

# The function to normalize data is (x - min(x))/(max(x) - min(x))
# We take only the numerical values to normalize
iris_norm <- as.data.frame(apply(iris[, 1:4], 2, function(x) (x - min(x))/(max(x)-min(x))))
iris_norm$Species <- iris$Species
str(iris_norm)
