## Evaluating a product contingency matrix
#Getting the data

data<- read.csv("top_supermarket_transactions.csv")
data

row.names(data) <- data[[1]]

data <- subset(data, select = c(-1))

cat("Products Transactions Contingency Matrix")

data

# Analyzing and visualizing the data

data['milk', ]

sort(data['milk', ], decreasing = TRUE)

data['bread', ]

sort(data['bread', ],decreasing = TRUE)

# Visualizing the data
mosaicplot(as.matrix(data), 
           color=TRUE, 
           title(main="Products Contingency Mosaic Plot"),
           las=2
)

# Global recommendations

cat("Recommendations based on global products contingency matrix")
 
items <- names(data)

for (item in items){
  cat(paste("Top 2 recommended items to buy with", item, "are: "))
  item.data <- subset(data[item,], select=names(data)[!names(data) %in% item])
  cat(names(item.data[order(item.data, decreasing = TRUE)][c(1,2)]))
  cat("\n")
}

# Advanced contingency matrices

library(arules)

data("Groceries")

# inspecting the first 3 transactions
inspect(Groceries[1:3])

# count based product contigency matrix 
ct<- crossTable(Groceries, measure="count", sort= TRUE)
ct[1:5, 1:5]

# support based product contigency matrix
ct<- crossTable(Groceries, measure="support", sort= TRUE)
ct[1:5,1:5]

# lift based product contigency matrix 
ct<- crossTable(Groceries, measure="lift", sort=TRUE)
ct[1:5, 1:5]


## Frequent itemset generation

library(dplyr)
library(gridExtra)

## Utility function: Appends vectors to a list
list.append <- function (mylist, ...){
  mylist <- c(mylist, list(...))
  return(mylist)
}

# Data retrieval and transformation

# Step 1: Function to read the dataset into memory from file
get_transaction_dataset <- function(filename){
    df <- read.csv(filename, header = FALSE)
  dataset <- list()
  
  for (index in seq(nrow(df))){
    transaction.set <- as.vector(unlist(df[index,]))
    transaction.set <- transaction.set[transaction.set != ""]
    dataset <- list.append(dataset, transaction.set)
  }
    return(dataset)
}  

# Step 2: Function to convert dataset into a data frame
get_item_freq_table <- function(dataset){
  item.freq.table <- unlist(dataset) %>% table %>% data.frame
  return (item.freq.table)
}

#Step 3: Function to prune items based on minimum frequency 
#as specified by the user.
prune_item_freq_table <- function(item.freq.table, item.min.freq){
  pruned.item.table <- item.freq.table[item.freq.table$Freq >= item.min.freq,]
  return (pruned.item.table)
}

# Building an itemset association matrix

## Step 4: Function to get possible itemset combinations where each itemset
##         has n number of items where n is specified by the user.
##         Here n <- num.items 
get_associated_itemset_combinations <- function(pruned.item.table, num.items){
  itemset.associations <- c()
  itemset.association.matrix <- combn(pruned.item.table$., num.items)
  for (index in seq(ncol(itemset.association.matrix))){
    itemset.associations <- c(itemset.associations,
                              paste(itemset.association.matrix[,index],
                                    collapse = ", ")
    )
  }
  itemset.associations <- unique(itemset.associations)
  return (itemset.associations)
}

## Step 5: Function to build an itemset association matrix where we 
##         see a contingency table showing itemset association occurence
##         in each transaction of the dataset
build_itemset_association_matrix <- function(dataset, itemset.association.labels, 
                                             itemset.combination.nums){
  
  itemset.transaction.labels <- sapply(dataset, paste, collapse=", ")
  itemset.associations <- lapply(itemset.association.labels, 
                                 function(itemset){
                                   unlist(strsplit(itemset, ", ", 
                                                   fixed = TRUE)
                                   )
                                 }
  )
  # building the itemset association matrix
  association.vector <- c()
  for (itemset.association in itemset.associations){
    association.vector <- c(association.vector,
                            unlist(
                              lapply(dataset, 
                                     function(dataitem, num.items=itemset.combination.nums){ 
                                       m <- match(dataitem, itemset.association)
                                       m <- length(m[!is.na(m)])
                                       if (m == num.items){
                                         1
                                       }else{
                                         NA
                                       }
                                     }
                              )
                            )
    )
  }
  
  itemset.association.matrix <- matrix(association.vector, nrow = length(dataset))
  itemset.association.labels <- sapply(itemset.association.labels, 
                                       function(item) {
                                         paste0('{', paste(item, collapse = ', '), '}')
                                       }
  )
  itemset.transaction.labels <- sapply(dataset, 
                                       function(itemset){
                                         paste0('{', paste(itemset, collapse = ', '), '}')
                                       }
  )
  colnames(itemset.association.matrix) <- itemset.association.labels
  rownames(itemset.association.matrix) <- itemset.transaction.labels
  
  return (itemset.association.matrix)
}

## Step 6: Function to generate total occurrences of each itemset in the
##         transactional dataset based on data from the association matrix
get_frequent_itemset_details <- function(itemset.association.matrix){
  frequent.itemsets.table <- apply(itemset.association.matrix, 2, sum, na.rm=TRUE)
  return (frequent.itemsets.table)
}

# Creating a frequent itemsets generation workflow

## Step 7: Function containing entire workflow to generate frequent itemsets
frequent.itemsets.generator <- function(data.file.path, itemset.combination.nums=2, 
                                        item.min.freq=2, minsup=0.2){
  # get the dataset
  dataset <- get_transaction_dataset(data.file.path)
  
  # convert data into item frequency table
  item.freq.table <- get_item_freq_table(dataset)
  pruned.item.table <- prune_item_freq_table(item.freq.table, item.min.freq)
  
  # get itemset associations
  itemset.association.labels <- get_associated_itemset_combinations(pruned.item.table, 
                                                                    itemset.combination.nums)
  itemset.association.matrix <- build_itemset_association_matrix(dataset, 
                                                                 itemset.association.labels, 
                                                                 itemset.combination.nums)
  
  # generate frequent itemsets
  frequent.itemsets.table <- get_frequent_itemset_details(itemset.association.matrix)
  frequent.itemsets.table <- sort(frequent.itemsets.table[frequent.itemsets.table > 0], 
                                  decreasing = TRUE)
  
  frequent.itemsets.names <- names(frequent.itemsets.table)
  frequent.itemsets.frequencies <- as.vector(frequent.itemsets.table)
  frequent.itemsets.support <- round((frequent.itemsets.frequencies * 100) / length(dataset), 
                                     digits=2)
  
  frequent.itemsets <- data.frame(Itemset=frequent.itemsets.names,
                                  Frequency=frequent.itemsets.frequencies,
                                  Support=frequent.itemsets.support)
  # apply minimum support cutoff to get frequent itemsets
  minsup.percentage <- minsup * 100
  frequent.itemsets <- subset(frequent.itemsets, frequent.itemsets['Support'] >= minsup.percentage)
  frequent.itemsets.support <- sapply(frequent.itemsets.support,
                                      function(value){
                                        paste0(value,'%')
                                      }
  )
  
  # printing to console
  cat("\nItem Association Matrix\n")
  print(itemset.association.matrix)
  cat("\n\n")
  cat("\nValid Frequent Itemsets with Frequency and Support\n")
  print(frequent.itemsets)
  
  # displaying frequent itemsets as a pretty table
  if (names(dev.cur()) != "null device"){
    dev.off()
  }
  grid.table(frequent.itemsets)
}

# Detecting Shopping Trends

frequent.itemsets.generator(
  data.file.path='shopping_transaction_log.csv',
  itemset.combination.nums=2, item.min.freq=3, minsup=0.2)


frequent.itemsets.generator(
  data.file.path='shopping_transaction_log.csv',
  itemset.combination.nums=3, item.min.freq=1, minsup=0.2)


## Association rule mining

library(arules)
library(arulesViz)

data("Groceries")

inspect(Groceries[1:3])

sort(itemFrequency(Groceries, type= "absolute"),
     decreasing = TRUE)[1:10]

itemFrequencyPlot(Groceries, topN=10,type="absolute")

# Detecting and predicting shopping trends

metric.params <- list(supp=0.001, conf=0.5)

rules <- apriori(Groceries, parameter = metric.params)

inspect(rules[1:5])

# pruning duplicate rules
prune.dup.rules <- function(rules){
  rule.subset.matrix <- is.subset(rules, rules)
  rule.subset.matrix[lower.tri(rule.subset.matrix, diag=T)] <- NA
  dup.rules <- colSums(rule.subset.matrix, na.rm=T) >= 1
  pruned.rules <- rules[!dup.rules]
  return(pruned.rules)
}

rules <- sort(rules, by="confidence", decreasing = TRUE)

rules <- prune.dup.rules(rules)

inspect(rules[1:5])

rules <- sort(rules, by="lift", decreasing = TRUE)

inspect(rules[1:5])

metric.params <- list(supp=0.001,conf=0.5,minlen=2)

rules<-apriori(data=Groceries, parameter=metric.params, 
               appearance = list(default="lhs",rhs="soda"),
               control = list(verbose=F))

rules<- sort(rules,decreasing = TRUE, by="confidence")

inspect(rules[1:5])

# finding items which are bought when we have an itemset on LHS
metric.params <- list(supp=0.001,conf = 0.3, minlen=2)
rules<-apriori(data=Groceries, parameter=metric.params, 
               appearance = list(default="rhs",lhs=c("yogurt", "sugar")),
               control = list(verbose=F))

rules<-sort(rules,decreasing = TRUE, by="confidence")
inspect(rules[1:5])

# Visualizing association rules

plot(rules, method="graph", engine= "interactive")














