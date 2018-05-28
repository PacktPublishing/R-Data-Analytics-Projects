
credit.df<-read.csv("credit_dataset_final.csv", header=TRUE, sep=",")

#Data preprocessing

## data type transformations - factoring
to.factors <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- as.factor(df[[variable]])
  }
  return(df)
}

## normalizing - scaling
scale.features <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- scale(df[[variable]], center=T, scale=T)
  }
  return(df)
}

numeric.vars<- c("credit.duration.months","age","credit.amount")

credit.df<- scale.features(credit.df,numeric.vars)

# factor variables
categorical.vars <- c('credit.rating', 'account.balance', 
                      'previous.credit.payment.status',
                      'credit.purpose', 'savings',
                      'employment.duration', 'installment.rate',
                      'marital.status', 'guarantor', 
                      'residence.duration', 'current.assets',
                      'other.credits', 'apartment.type',
                      'bank.credits', 'occupation', 
                      'dependents', 'telephone', 'foreign.worker')

credit.df<- to.factors(df=credit.df,variables = categorical.vars)

indexes<-sample(1:nrow(credit.df),size= 0.6*nrow(credit.df))

train.data<-credit.df[indexes,]

test.data<- credit.df[-indexes,]


## Feature selection

library(caret)
library(randomForest)

run.feature.selection <- function(num.iters=20, feature.vars, class.var){
  set.seed(10)
  variable.sizes <- 1:10
  control <- rfeControl(functions = rfFuncs, method = "cv",
                        verbose = FALSE, returnResamp = "all",
                        number = num.iters)
  results.rfe <- rfe(x = feature.vars, y = class.var,
                     sizes = variable.sizes,
                     rfeControl = control)
  return(results.rfe)
}

rfe.results <- run.feature.selection(feature.vars=train.data[,-1], 
                                     class.var=train.data[,1])

rfe.results

varImp(rfe.results)

## Modeling using logistic regression

library(caret)
library(ROCR)

source("performance_plot_utils.R")

test.feature.vars<-test.data[,-1]
test.class.var<- test.data[,1]

formula.init<- "credit.rating ~ ."
formula.init<- as.formula(formula.init)

lr.model<- glm(formula = formula.init,data = train.data,
               family = "binomial")

# view model details
summary(lr.model)

lr.predictions<- predict(lr.model,test.data, type="response")

lr.predictions<- round(lr.predictions)

u=union(lr.predictions,test.class.var)

confusionMatrix(table(factor(lr.predictions,u),factor(test.class.var,u)))

formula<- "credit.rating ~ ."

formula<-as.formula(formula)

control<-trainControl(method = "repeatedcv", number = 10, repeats = 2)

model<- train(formula, data=train.data, method="glm",trControl=control)

importance<- varImp(model,scale = FALSE)

plot(importance)

# build new model with selected features
formula.new <- "credit.rating ~ account.balance + 
credit.purpose + previous.credit.payment.status 
+ savings + credit.duration.months"

formula.new<- as.formula(formula.new)

lr.model.new<- glm(formula= formula.new, data=train.data,family = "binomial")

lr.predictions.new <- predict(lr.model.new, test.data, type="response") 
lr.predictions.new <- round(lr.predictions.new)
u= union(lr.predictions.new,test.class.var)
confusionMatrix(table(factor(lr.predictions.new, u),
                      factor(test.class.var, u)))

lr.model.best <- lr.model


lr.prediction.values <- predict(lr.model.best, test.feature.vars,
                                type="response")

predictions <- prediction(lr.prediction.values, test.class.var)

par(mfrow=c(1,2))

plot.roc.curve(predictions, title.text="LR ROC Curve")

plot.pr.curve(predictions, title.text="LR Precision/Recall Curve")

auc<- performance(predictions,"auc")
auc<- unlist(slot(auc,"y.values"))
auc<- round(auc,4)
auc

## Modeling using support vector machines

library(e1071) # svm model
library(caret) # model training\optimizations
library(kernlab) # svm model for hyperparameters
library(ROCR) # model evaluation
source("performance_plot_utils.R") # plot model metrics

test.feature.vars<- test.data[,-1]

test.class.var<- test.data[,1]

## build initial model with training data
formula.init <- "credit.rating ~ ."
formula.init <- as.formula(formula.init)

svm.model<- svm(formula= formula.init, data = train.data,
                kernel="radial", cost=100, gamma=1)

summary(svm.model)

svm.predictions<- predict(svm.model, test.feature.vars)

u= union(svm.predictions,test.class.var)
confusionMatrix(table(factor(svm.predictions, u),
                      factor(test.class.var, u)))
## svm specific feature selection
formula.init <- "credit.rating ~ ."
formula.init <- as.formula(formula.init)
control <- trainControl(method="repeatedcv", number=10, repeats=2)
model <- train(formula.init, data=train.data, method="svmRadial", 
               trControl=control)
importance <- varImp(model, scale=FALSE)
plot(importance, cex.lab=0.5)

## build new model with selected features
formula.new <- "credit.rating ~ account.balance +
credit.duration.months +savings + 
previous.credit.payment.status +credit.amount"
formula.new <- as.formula(formula.new)
svm.model.new <- svm(formula=formula.new, data=train.data, 
                     kernel="radial", cost=10, gamma=0.25)

## predict results with new model on test data and 
##evaluate new model performance
svm.predictions.new <- predict(svm.model.new, test.feature.vars)
u= union(svm.predictions.new,test.class.var)
confusionMatrix(table(factor(svm.predictions.new, u), 
                      factor(test.class.var, u)))

cost.weights <- c(0.1,10,100)

gamma.weights <- c(0.01,0.25,0.5,1)

tuning.results <- tune(svm, formula.new,data=train.data,
                       kernel="radial",
                       ranges = list(cost=cost.weights,gamma=gamma.weights))

print(tuning.results)

plot(tuning.results, cex.main=0.6, cex.lab=0.8, xaxs="i",yaxs="i")

svm.model.best<- tuning.results$best.model

svm.predictions.best<- predict(svm.model.best,test.feature.vars)

u= union(svm.predictions.best,test.class.var)
confusionMatrix(table(factor(svm.predictions.best, u),
                      factor(test.class.var, u)))

svm.predictions.best<-predict(svm.model.best,test.feature.vars,
                              decision.values=T)


svm.prediction.values<- attributes(svm.predictions.best)$decision.values

predictions<- prediction(svm.prediction.values,test.class.var)

par(mfrow=c(1,2))

plot.roc.curve(predictions,title.text = "SVM ROC Curve")

plot.pr.curve(predictions, title.text = "SVM Precision/Recall Curve")

auc<- performance(predictions,"auc")
auc<- unlist(slot(auc,"y.values"))
auc<- round(auc,4)
auc

# data transformation
transformed.train <- train.data
transformed.test <- test.data
for (variable in categorical.vars){
  new.train.var <- make.names(train.data[[variable]])
  transformed.train[[variable]] <- new.train.var
  new.test.var <- make.names(test.data[[variable]])
  transformed.test[[variable]] <- new.test.var
}
transformed.train <- to.factors(df=transformed.train, variables=categorical.vars)
transformed.test <- to.factors(df=transformed.test, variables=categorical.vars)
transformed.test.feature.vars <- transformed.test[,-1]
transformed.test.class.var <- transformed.test[,1]

# build optimal model based on AUC
grid <- expand.grid(C=c(1,10,100), 
                    sigma=c(0.01, 0.05, 0.1, 0.5, 1))
ctr <- trainControl(method='cv', number=10,
                    classProbs=TRUE,
                    summaryFunction=twoClassSummary)
svm.roc.model <- train(formula.init, transformed.train,
                       method='svmRadial', trControl=ctr, 
                       tuneGrid=grid, metric="ROC")

predictions <- predict(svm.roc.model, transformed.test.feature.vars)
u= union(predictions,transformed.test.class.var)
confusionMatrix(table(factor(predictions, u),
                      factor(transformed.test.class.var, u)))

svm.predictions<-predict(svm.roc.model,transformed.test.feature.vars,
                         type="prob")
svm.prediction.values <- svm.predictions[,2]
predictions <- prediction(svm.prediction.values, test.class.var)
par(mfrow=c(1,2))
plot.roc.curve(predictions, title.text="SVM ROC Curve")
plot.pr.curve(predictions, title.text="SVM Precision/Recall Curve")

auc<- performance(predictions,"auc")
auc<- unlist(slot(auc,"y.values"))
auc<- round(auc,4)
auc

##  Modeling using decision trees

library(rpart)# tree models 
library(caret) # feature selection
library(rpart.plot) # plot dtree
library(ROCR) # model evaluation
library(e1071) # tuning model
source("performance_plot_utils.R") # plotting curves

## separate feature and class variables
test.feature.vars <- test.data[,-1]
test.class.var <- test.data[,1]

formula.init <- "credit.rating ~ ."
formula.init <- as.formula(formula.init)
dt.model <- rpart(formula=formula.init, method="class",data=train.data, 
                  control = rpart.control(minsplit=20, cp=0.05))

dt.predictions <- predict(dt.model, test.feature.vars, type="class")
u= union(dt.predictions,test.class.var)
confusionMatrix(table(factor(dt.predictions, u), 
                      factor(test.class.var, u)))

## dt specific feature selection
formula.init <- "credit.rating ~ ."
formula.init <- as.formula(formula.init)
control <- trainControl(method="repeatedcv", number=10, repeats=2)
model <- train(formula.init, data=train.data, method="rpart", 
               trControl=control)
importance <- varImp(model, scale=FALSE)
plot(importance, cex.lab=0.5)

formula.new <- "credit.rating ~ account.balance + savings +
                        credit.amount + credit.duration.months + 
previous.credit.payment.status"
formula.new <- as.formula(formula.new)
dt.model.new <- rpart(formula=formula.new, method="class",
                      data=train.data, 
                      control = rpart.control(minsplit=20, cp=0.05),
                      parms = list(prior = c(0.7, 0.3)))

dt.predictions.new <- predict(dt.model.new, test.feature.vars,
                              type="class")
u= union(dt.predictions.new ,test.class.var)
confusionMatrix(table(factor(dt.predictions.new, u),
                      factor(test.class.var, u)))

dt.model.best <- dt.model.new
print(dt.model.best)

par(mfrow=c(1,1))
prp(dt.model.best, type=1, extra=3, varlen=0, faclen=0)

dt.predictions.best <- predict(dt.model.best, test.feature.vars,
                               type="prob")
dt.prediction.values <- dt.predictions.best[,2]
predictions <- prediction(dt.prediction.values, test.class.var)
par(mfrow=c(1,2))
plot.roc.curve(predictions, title.text="DT ROC Curve")
plot.pr.curve(predictions, title.text="DT Precision/Recall Curve")

auc<- performance(predictions,"auc")
auc<- unlist(slot(auc,"y.values"))
auc<- round(auc,4)
auc

## Modeling using random forests

library(randomForest) #rf model
library(caret) # feature selection
library(e1071) # model tuning
library(ROCR) # model evaluation
source("performance_plot_utils.R") # plot curves

test.feature.vars <- test.data[,-1]
test.class.var <- test.data[,1]

formula.init <- "credit.rating ~ ."
formula.init <- as.formula(formula.init)
rf.model <- randomForest(formula.init, data = train.data,
                         importance=T, proximity=T)

print(rf.model)

rf.predictions <- predict(rf.model, test.feature.vars, type="class")
u= union(rf.predictions,test.class.var)
confusionMatrix(table(factor(rf.predictions, u), 
                      factor(test.class.var, u)))

formula.new <- "credit.rating ~ account.balance + savings +
credit.amount + credit.duration.months + 
previous.credit.payment.status"
formula.new <- as.formula(formula.new)
rf.model.new <- randomForest(formula.new, data = train.data, 
                             importance=T, proximity=T)

rf.predictions.new <- predict(rf.model.new, test.feature.vars,
                              type="class")
u= union(rf.predictions.new, test.class.var)
confusionMatrix(table(factor(rf.predictions.new, u),
                      factor(test.class.var, u)))

nodesize.vals <- c(2, 3, 4, 5)
ntree.vals <- c(200, 500, 1000, 2000)
tuning.results <- tune.randomForest(formula.new, 
                                    data = train.data,
                                    mtry=3, 
                                    nodesize=nodesize.vals,
                                    ntree=ntree.vals)
print(tuning.results)

rf.model.best <- tuning.results$best.model
rf.predictions.best <- predict(rf.model.best, test.feature.vars,
                               type="class")
u= union(rf.predictions.best,test.class.var)
confusionMatrix(table(factor(rf.predictions.best, u),
                      factor(test.class.var, u)))

rf.predictions.best <- predict(rf.model.best, test.feature.vars,
                               type="prob")
rf.prediction.values <- rf.predictions.best[,2]
predictions <- prediction(rf.prediction.values, test.class.var)
par(mfrow=c(1,2))
plot.roc.curve(predictions, title.text="RF ROC Curve")
plot.pr.curve(predictions, title.text="RF Precision/Recall Curve")

auc<- performance(predictions,"auc")
auc<- unlist(slot(auc,"y.values"))
auc<- round(auc,4)
auc

## Modeling using neural networks

library(caret) # nn models
library(ROCR) # evaluate models
source("performance_plot_utils.R") # plot curves

test.feature.vars <- test.data[,-1]
test.class.var <- test.data[,1]

# data transformation
transformed.train <- train.data
transformed.test <- test.data
for (variable in categorical.vars){
  new.train.var <- make.names(train.data[[variable]])
  transformed.train[[variable]] <- new.train.var
  new.test.var <- make.names(test.data[[variable]])
  transformed.test[[variable]] <- new.test.var
}
transformed.train <- to.factors(df=transformed.train, variables=categorical.vars)
transformed.test <- to.factors(df=transformed.test, variables=categorical.vars)
transformed.test.feature.vars <- transformed.test[,-1]
transformed.test.class.var <- transformed.test[,1]

formula.init <- "credit.rating ~ ."
formula.init <- as.formula(formula.init)
nn.model <- train(formula.init, data = transformed.train,
                  method="nnet")
print(nn.model)

nn.predictions <- predict(nn.model, transformed.test.feature.vars,
                          type="raw")
u= union(nn.predictions, transformed.test.class.var)
confusionMatrix(table(factor(nn.predictions, u), 
                      factor(transformed.test.class.var, u)))

formula.init <- "credit.rating ~ ."
formula.init <- as.formula(formula.init)
control <- trainControl(method="repeatedcv", number=10, repeats=2)
model <- train(formula.init, data=transformed.train, method="nnet", 
               trControl=control)
importance <- varImp(model, scale=FALSE)
plot(importance, cex.lab=0.5)

formula.new <- "credit.rating ~ account.balance + credit.purpose + 
savings + current.assets +foreign.worker +
previous.credit.payment.status"
formula.new <- as.formula(formula.new)
nn.model.new <- train(formula.new, data=transformed.train,
                      method="nnet")

nn.predictions.new <- predict(nn.model.new, transformed.test.feature.vars,
                              type="raw")
u= union(nn.predictions.new, transformed.test.class.var)
confusionMatrix(table(factor(nn.predictions.new, u),
                      factor(transformed.test.class.var, u)))

plot(nn.model.new, cex.lab=0.5)

nn.model.best <- nn.model
nn.predictions.best <-predict(nn.model.best,transformed.test.feature.vars,
                               type="prob")
nn.prediction.values <- nn.predictions.best[,2]
predictions <- prediction(nn.prediction.values, test.class.var)
par(mfrow=c(1,2))
plot.roc.curve(predictions, title.text="NN ROC Curve")
plot.pr.curve(predictions, title.text="NN Precision/Recall Curve")

auc<- performance(predictions,"auc")
auc<- unlist(slot(auc,"y.values"))
auc<- round(auc,4)
auc




