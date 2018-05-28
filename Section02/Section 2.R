## 1.	Algorithms in machine learning
# Perceptron

x1 <- runif(30,-1,1)
x2 <- runif(30,-1,1)

x<- cbind(x1,x2)

Y <- ifelse(x2>0.5+x1,+1,-1)

#plot the actual separator
plot(x,pch=ifelse(Y>0,"+","-"),xlim=c(-1,1),
     ylim=c(-1,1),cex=2) #cex is zoom
abline(0.5,1) #correct Y

#helper function to calculate distance from hyperplane
calculate_distance = function(x,w,b) {
  sum(x*w) + b
}

#linear classifier
linear_classifier = function(x,w,b) {
  distances =apply(x, 1, calculate_distance, w, b)
  return(ifelse(distances < 0, -1, +1))
}

linear_classifier(x,c(-1,1)/sqrt(2), -sqrt(2)/4)

second_norm = function(x) {sqrt(sum(x*x))}

#perceptron training algorithm
perceptron = function(x, y, learning_rate=1) {
  
  w = vector(length = ncol(x)) # initialize w
  b = 0 # Initialize b
  k = 0 # count iterations
  
  R = max(apply(x, 1, second_norm))#constant with value greater than distance of furthest point
  
  incorrect = TRUE # flag to identify classifier
  #initialize plot
  plot(x,cex=0.2)
  
  #loop till correct classifier is not found
  while (incorrect ) {
    
    incorrect = FALSE
    #classify with current weights
    yc <- linear_classifier(x,w,b)
    #Loop over each point in the input x
    for (i in 1:nrow(x)) {
      #update weights if point not classified correctly
      if (y[i] != yc[i]) {
        w <- w + learning_rate * y[i]*x[i,]
        b <- b + learning_rate * y[i]*R^2
        k <- k+1
        
        #currect classifier's components
        if(k%%5 == 0){
          intercept <- - b / w[[2]]
          slope <- - w[[1]] / w[[2]]
          #plot the classifier hyper plane
          abline(intercept,slope,col="red")
          #wait for user input
          cat ("Iteration # ",k,"\n")
          cat ("Press [enter] to continue")
          line <- readline()
        }
        incorrect =TRUE
      }
    } }
  
  s = second_norm(w)
  #scale the classifier with unit vector
  return(list(w=w/s,b=b/s,updates=k))
}

p<- perceptron(x,Y)

intercept <- - p$b / p$w[[2]]
slope <- - p$w[[1]] /p$ w[[2]]
abline(intercept,slope,col="green")

y<- linear_classifier(x, p$w, p$b)

plot(x, cex=0.2)

#zoom into points near the separator and color code them
points(subset(x,Y==1),col="black",pch="+",cex=2)
points(subset(x,Y==-1),col="red",pch="-",cex=2)

# compute intercept on y axis of separator from w and b
intercept <- - p$b / p$w[[2]]
slope <- - p$w[[1]] /p$ w[[2]]  # compute slope of separator from w
abline(intercept,slope,col="green") # draw separating boundary


## Families of algorithms-- Supervised Learning

height <- c(69.1,56.4,65.3,62.8,63,57.3,59.8,62.5,62.5,59.0,
            51.3,64,56.4,66.5,72.2,65.0,67.0,57.6,66.6)

weight <- c(113,84,99,103,102,83,85,113,84,
            99,51,90,77,112,150,128,133,85,112)

plot(height,weight)

cor(height,weight)

model <- lm(weight ~ height)

model

attributes(model)

model$coefficients[1]

model$coefficients[2]

residuals(model)

model$coefficients[[2]]*50 + model$coefficients[[1]]

summary(model)

plot(height,weight)

abline(model)
# K-Nearest Neighbors (KNN)

# Collecting and exploring data

iris

#skip these steps if you already have iris on your system
iris <- read.csv(url("http://archive.ics.uci.edu/ml/
                     machine-learningdatabases/iris/iris.data"),
                 header = FALSE)

names(iris)<- c("Sepal.Length","Sepal.Width","Petal.Length",
                "Petal.Width","Species")

head(iris)

str(iris)

summary(iris)

install.packages("ggvis")

library(ggvis)

iris %>% ggvis(~Petal.Length,~Petal.Width, fill= ~factor(Species))%>%
  layer_points()

# Normalizing data

#normalization function
min_max_normalizer <- function(x)
{
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

normalized_iris <- as.data.frame(lapply(iris[1:4],
                                        min_max_normalizer))

normalized_iris

summary(normalized_iris)

# Creating training and test data sets

table(iris$Species)

set.seed(1234)

random_samples <- sample(2, nrow(iris), replace= TRUE,
                         prob=c(0.67,0.33))

iris.training <- iris[random_samples==1,1:4]
iris.trainLabels <- iris[random_samples==1, 5]

iris.test <- iris[random_samples==2, 1:4]
iris.testLabels <- iris[random_samples==2, 5]

# Learning from data/training the model

library(class)

iris_model <- knn(train= iris.training, test= iris.test,
                  cl=iris.trainLabels,k=3)

iris_model

# Evaluating the model

library(gmodels)

CrossTable(x=iris.testLabels, y= iris_model,prop.chisq = FALSE)


## Families of algorithms-- Unsupervised Learning
# Apriori algorithm

library(arules)
data("Adult");

summary(Adult)

inspect(Adult[0:5])

rules <- apriori(Adult, parameter=list(support=0.5, confidence=0.8,
                                       target="rules"))

summary(rules)

as(head(sort(rules,by=c("confidence","support")),n=3),"data.frame")

# K-Means

kmean_iris <-iris

kmean_iris$Species <- NULL

(clusters <- kmeans(kmean_iris,3))

table(iris$Species, clusters$cluster)

plot(kmean_iris[c("Sepal.Length", "Sepal.Width")],
     col=clusters$cluster,
     pch = c(15, 16, 17)[as.numeric(clusters$cluster)])

points(clusters$centers[,c("Sepal.Length", "Sepal.Width")],
       col=1:3, pch=8, cex=4)











