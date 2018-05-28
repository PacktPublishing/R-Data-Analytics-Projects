## Credit Risk Detection and Prediction - Descriptive Analytics
# Getting the data

credit.df<- read.csv("german_credit_dataset.csv", header=TRUE,sep = ",")

class(credit.df)

head(credit.df)

str(credit.df)

##Data preprocessing
#Dealing with missing values

sum(is.na(credit.df))

sum(complete.cases(credit.df))

# Datatype conversions

str(credit.df)

# data transformation
to.factors <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- as.factor(df[[variable]])
  }
  return(df)
}

categorical.vars <- c('credit.rating', 'account.balance', 'previous.credit.payment.status',
                      'credit.purpose', 'savings', 'employment.duration', 'installment.rate',
                      'marital.status', 'guarantor', 'residence.duration', 'current.assets',
                      'other.credits', 'apartment.type', 'bank.credits', 'occupation',
                      'dependents', 'telephone', 'foreign.worker')

credit.df <- to.factors(df=credit.df,variables = categorical.vars)

str(credit.df)

# Data analysis and transformation
# Building analysis utilities

library(pastecs)
library(gmodels)
library(gridExtra)
library(ggplot2)

# install.packages(c("pastecs","gmodels","gridExtra","ggplot2"))

# analyze numerical variables
# summary statistics
get.numeric.variable.stats <- function(indep.var, detailed=FALSE){
  options(scipen=100)
  options(digits=2)
  if (detailed){
    var.stats <- stat.desc(indep.var)
  }else{
    var.stats <- summary(indep.var)
  }
  
  df <- data.frame(round(as.numeric(var.stats),2))
  colnames(df) <- deparse(substitute(indep.var))
  rownames(df) <- names(var.stats)
  
  if (names(dev.cur()) != "null device"){
    dev.off()
  }
  grid.table(t(df))
}

# visualizations
# histograms\density
visualize.distribution <- function(indep.var){
  pl1 <- qplot(indep.var, geom="histogram",
               fill=I('gray'), binwidth=5,
               col=I('black'))+ theme_bw()
  pl2 <- qplot(indep.var, geom="density",
               fill=I('gray'), binwidth=5,
               col=I('black'))+ theme_bw()
  
  grid.arrange(pl1,pl2, ncol=2)
}

# box plots
visualize.boxplot <- function(indep.var, dep.var){
  pl1 <- qplot(factor(0),indep.var, geom="boxplot",
               xlab = deparse(substitute(indep.var)),
               ylab="values") + theme_bw()
  pl2 <- qplot(dep.var,indep.var,geom="boxplot",
               xlab = deparse(substitute(dep.var)),
               ylab = deparse(substitute(indep.var))) + theme_bw()
  
  grid.arrange(pl1,pl2, ncol=2)
}

# analyze categorical variables
# summary statistics
get.categorical.variable.stats <- function(indep.var){
  
  feature.name = deparse(substitute(indep.var))
  df1 <- data.frame(table(indep.var))
  colnames(df1) <- c(feature.name, "Frequency")
  df2 <- data.frame(prop.table(table(indep.var)))
  colnames(df2) <- c(feature.name, "Proportion")
  
  df <- merge(
    df1, df2, by = feature.name
  )
  ndf <- df[order(-df$Frequency),]
  if (names(dev.cur()) != "null device"){
    dev.off()
  }
  grid.table(ndf)
}

# generate contingency table
get.contingency.table <- function(dep.var, indep.var,
                                  stat.tests=F){
  if(stat.tests == F){
    CrossTable(dep.var, indep.var, digits=1,
               prop.r=F, prop.t=F, prop.chisq=F)
  }else{
    CrossTable(dep.var, indep.var, digits=1,
               prop.r=F, prop.t=F, prop.chisq=F,
               chisq=T, fisher=T)
  }
}


# visualizations
# barcharts
visualize.barchart <- function(indep.var){
  qplot(indep.var, geom="bar",
        fill=I('gray'), col=I('black'),
        xlab = deparse(substitute(indep.var))) + theme_bw()
}


# mosaic plots
visualize.contingency.table <- function(dep.var, indep.var){
  if (names(dev.cur()) != "null device"){
    dev.off()
  }
  mosaicplot(dep.var ~ indep.var, color=T,
             main = "Contingency table plot")
}


## Analyzing the dataset

attach(credit.df)

get.categorical.variable.stats(credit.rating)

visualize.barchart(credit.rating)

get.categorical.variable.stats(account.balance)

visualize.barchart(account.balance)

library(car)

new.account.balance<- recode(account.balance,"1=1;2=2;3=3;4=3")

credit.df$account.balance<- new.account.balance

get.contingency.table(credit.rating,new.account.balance,
                      stat.tests = T)

visualize.contingency.table(credit.rating,new.account.balance)

get.numeric.variable.stats(credit.duration.months)

visualize.distribution(credit.duration.months)

visualize.boxplot(credit.duration.months,credit.rating)

get.categorical.variable.stats(previous.credit.payment.status)

visualize.barchart(previous.credit.payment.status)


new.previous.credit.payment.status<- recode(previous.credit.payment.status,
                                            "0=1;1=1;2=2;3=3;4=3")
credit.df$previous.credit.payment.status<-
  new.previous.credit.payment.status

get.contingency.table(credit.rating,new.previous.credit.payment.status)

get.categorical.variable.stats(credit.purpose)

visualize.barchart(credit.purpose)

new.credit.purpose <- recode(credit.purpose,
                      "0=4;1=1;2=2;3=3;4=3;5=3;6=3;7=4;8=4;9=4;10=4")

credit.df$credit.purpose<- new.credit.purpose

get.contingency.table(credit.rating,new.credit.purpose)

get.numeric.variable.stats(credit.amount)

visualize.distribution(credit.amount)

visualize.boxplot(credit.amount,credit.rating)

new.savings<- recode(savings,"1=1;2=2;3=3;4=3;5=4")

credit.df$savings<-new.savings

get.contingency.table(credit.rating,new.savings)

# feature: employment.duration - recode classes and update data frame
new.employment.duration <- recode(employment.duration,
                                  "1=1;2=1;3=2;4=3;5=4")
credit.df$employment.duration <- new.employment.duration

# contingency table
get.contingency.table(credit.rating, new.employment.duration)


get.contingency.table(credit.rating,installment.rate,stat.tests = TRUE)

# feature: marital.status - recode classes and update data frame
new.marital.status <- recode(marital.status, "1=1;2=1;3=3;4=4")
credit.df$marital.status <- new.marital.status

# contingency table
get.contingency.table(credit.rating, new.marital.status)


# feature: guarantor - recode classes and update data frame
new.guarantor <- recode(guarantor, "1=1;2=2;3=2")
credit.df$guarantor <- new.guarantor

# perform statistical tests
fisher.test(credit.rating, new.guarantor)
chisq.test(credit.rating, new.guarantor)

# perform statistical tests for residence.duration
fisher.test(credit.rating, residence.duration)
chisq.test(credit.rating, residence.duration)

# perform statistical tests for current.assets
fisher.test(credit.rating, current.assets)
chisq.test(credit.rating, current.assets)

get.numeric.variable.stats(age)

visualize.distribution(age)

visualize.boxplot(age,credit.rating)

# feature: other.credits - recode classes and update data frame
new.other.credits <- recode(other.credits, "1=1;2=1;3=2")
credit.df$other.credits <- new.other.credits

# perform statistical tests
fisher.test(credit.rating, new.other.credits)
chisq.test(credit.rating, new.other.credits)

# perform statistical tests for apartment.type
fisher.test(credit.rating, apartment.type)
chisq.test(credit.rating, apartment.type)

# feature: bank.credits - recode classes and update data frame
new.bank.credits <- recode(bank.credits, "1=1;2=2;3=2;4=2")
credit.df$bank.credits <- new.bank.credits

# perform statistical tests
fisher.test(credit.rating, new.bank.credits)
chisq.test(credit.rating, new.bank.credits)

# perform statistical tests for occupation
fisher.test(credit.rating, occupation)
chisq.test(credit.rating, occupation)

# perform statistical tests for dependents
fisher.test(credit.rating, dependents)
chisq.test(credit.rating, dependents)

# perform statistical tests for telephone
fisher.test(credit.rating, telephone)
chisq.test(credit.rating, telephone)


# perform statistical tests for foreign.worker
fisher.test(credit.rating, foreign.worker)
chisq.test(credit.rating, foreign.worker)

write.csv(file='credit_datset_final.csv',x=credit.df,row.names = F)

