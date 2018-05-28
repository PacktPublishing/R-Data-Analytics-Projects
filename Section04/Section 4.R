## Building a recommender engine
# Matrix factorization: Implementation

raw_ratings<- read.csv("product_ratings.csv")

ratings_matrix <- data.matrix(raw_ratings)

rows<- nrow(ratings_matrix)

columns <- ncol(ratings_matrix)

K<-2

X<- matrix(runif(rows*K), nrow=rows, byrow=TRUE)

Y <- matrix(runif(columns*K), nrow=columns, byrow=TRUE)

mf_based_ucf <- function(ratings_matrix, X, Y, K, epoch=5000, alpha=0.0002, beta=0.02){
  
  #transpose Y
  Y <- t(Y)
  
  # Iterate epoch number of times
  for (step in seq(epoch)){
    for (i in seq(nrow(ratings_matrix))){
      for (j in seq(length(ratings_matrix[i, ]))){
        if (ratings_matrix[i, j] > 0){
          # error 
          eij = ratings_matrix[i, j] - as.numeric(X[i, ] %*% Y[, j])
          
          # gradient calculation 
          for (k in seq(K)){
            X[i, k] = X[i, k] + alpha * (2 * eij * Y[k, j] - beta * X[i, k])
            Y[k, j] = Y[k, j] + alpha * (2 * eij * X[i, k] - beta * Y[k, j])
          }
        }
      }
    }
    
    # Overall Squared Error Calculation
    e = 0
    
    for (i in seq(nrow(ratings_matrix))){
      for (j in seq(length(ratings_matrix[i, ]))){
        if (ratings_matrix[i, j] > 0){
          e = e + (ratings_matrix[i, j] - as.numeric(X[i, ] %*% Y[, j]))^2
          for (k in seq(K)){
            e = e + (beta/2) * (X[i, k]^2 + Y[k, j]^2)
          }
        }
      }
    }
    
    # stop if error falls below this threshold
    if (e < 0.001){
      break
    }
  }
  
  #inner product
  pR <- X %*% Y
  pR <- round(pR, 2)
  return (pR)
}

epoch <- 10000

alpha<-0.0002

beta<-0.02

pred.matrix<- mf_based_ucf(ratings_matrix, X, Y, K, epoch = epoch)

colnames(pred.matrix)<- c("iPhone.4","iPhone.5s","Nexus.5","Moto.X",
                          "Moto.G","Nexus.6","One.Plus.One")
                        
# Load recommenderlab library
library("recommenderlab")

# Read dataset from csv file
raw_data <- read.csv("product_ratings_data.csv")

# Create rating matrix from data 
ratings_matrix<- as(raw_data, "realRatingMatrix")

############### Exploring Data ##################

#view transformed data
image(ratings_matrix[1:6,1:10])

# Extract a sample from ratings matrix
sample_ratings <-sample(ratings_matrix,1000)

# Get the mean product ratings as given by first user
rowMeans(sample_ratings[1,])

# Get distribution of item ratings
hist(getRatings(sample_ratings), breaks=100,xlab = "Product Ratings",
     main = " Histogram of Product Ratings")

# Get distribution of normalized item ratings
hist(getRatings(normalize(sample_ratings)),breaks=100, xlab = "Normalized Product Ratings",
     main = " Histogram of Normalized Product Ratings")

# Number of items rated per user
hist(rowCounts(sample_ratings),breaks=50,xlab = "Number of Products",
     main = " Histogram of Rated Products Distribution")


############### Prepare Recommendation Model and Predict ##################

# Create 'User Based collaborative filtering' model 
ubcf_recommender <- Recommender(ratings_matrix[1:1000],"UBCF")

# Predict list of product which can be recommended to given users
recommendations <- predict(ubcf_recommender, ratings_matrix[1010:1011], n=5)

# show recommendation in form of the list
as(recommendations, "list")


############### Testing of the recommender algorithm ##################

# Evaluation scheme
eval_scheme <- evaluationScheme(ratings_matrix[1:500],method="split",train=0.9,given=15)

# View the evaluation scheme
eval_scheme

# Training model using UBCF
training_recommender <- Recommender(getData(eval_scheme, "train"), "UBCF")

# Preditions on the test dataset
test_rating <- predict(training_recommender, getData(eval_scheme, "known"), type="ratings")

#Error 
error <- calcPredictionAccuracy(test_rating, getData(eval_scheme, "unknown"))

error

# Training model using IBCF
training_recommender_2 <- Recommender(getData(eval_scheme, "train"), "IBCF")

# Preditions on the test dataset
test_rating_2 <- predict(training_recommender_2, getData(eval_scheme, "known"), type="ratings")

error_compare <- rbind(calcPredictionAccuracy(test_rating, getData(eval_scheme, "unknown")),
                       calcPredictionAccuracy(test_rating_2, getData(eval_scheme, "unknown")))

rownames(error_compare) <- c("User Based CF","Item Based CF")

error_compare

## evaluate topNLists instead (you need to specify given and goodRating!)
test_rating_3 <- predict(training_recommender, getData(eval_scheme, "known"), type="topNList")

# Calculate Prediction Accuracy
calcPredictionAccuracy(test_rating_3, getData(eval_scheme, "unknown"), given=15,goodRating=5) 











