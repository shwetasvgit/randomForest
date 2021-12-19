setwd("E:/RHUL/Data Analysis/Assignment")

#Loading the Boston dataset from MASS library
library(MASS)
# print(names(Boston))

#Splitting the columns into label and attributes
p <- ncol(Boston) - 1
attributes <- names(Boston)[1:p]


#Set seed and split data into train and test data
set.seed(1403)
train_index <- sample(1:nrow(Boston), nrow(Boston) / 2)
train_data <- Boston[train_index, ]
test_data <- Boston[-train_index, ]

#Bootstrapping
B = 100
h = 3

setClass(
  "node",
  slots = list(
    is.leaf = "logical",
    prediction = "numeric",
    attr = "character",
    split = "numeric",
    left.node = "node",
    right.node = "node"
  )
)

#Function for calculating rss using sum of average of labels
rss <- function(data)
{
  #d = last column usually the label
  d <- ncol(data)
  #stroing the mean of the labels received in the data
  average <- mean(data[, d])
  deviate <- data[, d] - average
  #return the rss = sum of square of y-yavg , where y is the label and yavg is the average
  return(sum(deviate ^ 2))
}

#Function for calculating the optimal split value
optimum_split_value <- function(boot_table, attributeSample)
{
  optRssSum <- Inf
  for (attribute in attributeSample)
  {
    for (uniqueValue in unique(boot_table[, attribute]))
    {
      Lsplit <- boot_table[boot_table[attribute] <= uniqueValue, ]
      Rsplit <- boot_table[boot_table[attribute] > uniqueValue, ]
      #internal call to rss function for calculating the rss
      rssL <- rss(Lsplit)
      rssR <- rss(Rsplit)
      total_rss <- rssL + rssR
      #updating the split value according to the smallest rss sum
      if (total_rss < optRssSum)
      {
        optRssSum <- total_rss
        optAttr <- attribute
        optimalUniqueValue <- uniqueValue
      }
    }
  }
  return (list(optimalUniqueValue, optAttr))
}


#Fucntion to create node and leaves
generate <-  function(data, indx, depth) {
    #if you do not want to limit the depth of the decision tree, you can set depth = len(indx)
    node <- new("node")
    d <- dim(data)[2]
    if (depth == 0) {
      node@is.leaf <- TRUE
      if (length(indx) == 0) {
        node@prediction = 0
      }
      else{
        node@prediction <- mean(data[indx, d])
      }
    } else{
      #extract all labels in data[indx], and check if they are all the same
      #if they are all the same, no split is needed, "node" is a leaf, and the prediction value is same as the common value of all the labels
      all_labels <- data[indx, d]
      all_labels <- unique(all_labels)
      if (length(all_labels) == 1) {
        node@is.leaf <- TRUE
        node@prediction <- data[indx[1], d]
      } else{
        #in this case, "node" is internal, a split is needed, and we need to compute the optimal split
        node@is.leaf <- FALSE
        node_sample_val <- sample(p, ceiling(p / 3), replace = FALSE)
        opt_split <-
          optimum_split_value(data, attributes[node_sample_val])
        node@attr <- opt_split[[2]]
        node@split <- opt_split[[1]]
        #after computing the optimal split, we split data into two parts, one going to the left child, and the other going to the right child
        #we compute the indices of observations that go to left or right below
        Lindx <- c()
        Rindx <- c()
        for (i in indx) {
          if (data[i, node@attr] <= node@split) {
            Lindx <- append(Lindx, i)
          }
          else{
            Rindx <- append(Rindx, i)
          }
        }
        #then we create two new nodes, which are the left child and the right child of "node"
        #after this, we recursively call the function "generate" on the two children, to create one sub-tree under each child
        Lnode <- generate(data, Lindx, depth - 1)
        Rnode <- generate(data, Rindx, depth - 1)
        node@left.node <- Lnode
        node@right.node <- Rnode
      }
    }
    return(node)
  }


gen_decision_tree <- function(data, depth)
{
  #if you do not want to limit the depth of the decision tree, you can set depth = len(indx)
  
  node <- new("node")
  d <- dim(data)[2]
  if (depth == 0) {
    node@is.leaf <- TRUE
    node@prediction <- mean(data[, d])
    ## print("leaf node reached")
  }
  else
  {
    #extract all labels in data[indx], and check if they are all the same
    #if they are all the same, no split is needed, "node" is a leaf, and the prediction value is same as the common value of all the labels
    all_labels <- data[, d]
    # # print("medv")
    print(all_labels)
    
    all_labels <- unique(all_labels)
    if (length(all_labels) == 1)
    {
      node@is.leaf <- TRUE
      node@prediction <- data[1, d]
      
    }
    else
    {
      ## print("Child node")
      #in this case, "node" is internal, a split is needed, and we need to compute the optimal split
      node@is.leaf <- FALSE
      node_sample_val <- sample(p, ceiling(p / 3), replace = FALSE)
      print(attributes[node_sample_val])
      #opt_rss <- Inf
      opt_split <-
        optimum_split_value(data, attributes[node_sample_val])
      #opt_split <- optimalSplit(data,indx)
      node@attr <- opt_split[[2]]
      node@split <- opt_split[[1]]
      #after computing the optimal split, we split data into two parts, one going to the left child, and the other going to the right child
      #we compute the indices of observations that go to left or right below
      Lindx <- c()
      Rindx <- c()
      # Lindx <- data[data[,node@attr]<=node@split,]
      # Rindx <- data[data[,node@attr]>node@split,]
      #print(Rindx)
      for (i in nrow(data)) {
        if (data[i, node@attr] <= node@split) {
          Lindx <- append(Lindx, i)
        }
        else{
          Rindx <- append(Rindx, i)
        }
      }
      #then we create two new nodes, which are the left child and the right child of "node"
      #after this, we recursively call the function "generate" on the two children, to create one sub-tree under each child
      
      Lnode <- gen_decision_tree(data[Lindx], depth - 1)
      Rnode <- gen_decision_tree(data[Rindx], depth - 1)
      node@left.node <- Lnode
      node@right.node <- Rnode
      
    }
  }
  return(node)
}

MSE_train_val <- c()
MSE_test_val <- c()

####MAIN FUNCTION######
#loop for bootstrapping data values

test_matrix <- matrix(0, nrow = B , ncol = 253)
train_matrix <- matrix(0, nrow = B , ncol = 253)

#For each BTS create a decision tree
for (bts in 1:B)
{
  #Bootstrap generation using sample
  boot_index <- sample(train_index, replace = TRUE)
  #Storing the BTS data in a variable
  data <- Boston[boot_index, ]
  root <- generate(data, 1:(dim(data)[1]), h)
  #Vector initialisation to store values of y_pred
  error_vec_train <- c()
  error_vec_test <- c()
  
  #To find the train MSE, traverse through the train data and find the prediction using the generated tree
  for (train_sample in 1:nrow(train_data))
  {
    current_node <- root
    while (current_node@is.leaf != TRUE)
    {
      if (train_data[train_sample, current_node@attr] <= current_node@split)
      {
        current_node <- current_node@left.node
      } else
      {
        current_node <- current_node@right.node
      }
    }
    pred_y_train <- current_node@prediction
    #error_sq <- (train_sample["medv"]-pred_y)^2
    #if(is.nan(pred_y_train)){break}
    error_vec_train <- append(error_vec_train, pred_y_train)
  }
  train_matrix[bts, ] <- error_vec_train
  
  #To find the test MSE, traverse through the test data and find the prediction using the generated tree
  for (test_sample in 1:nrow(test_data))
  {
    current_node <- root
    while (current_node@is.leaf != TRUE)
    {
      if (test_data[test_sample, current_node@attr] <= current_node@split)
      {
        current_node <- current_node@left.node
      } else
      {
        current_node <- current_node@right.node
      }
    }
    pred_y_test <- current_node@prediction
    #error_sq <- (train_sample["medv"]-pred_y)^2
    
    error_vec_test <- append(error_vec_test, pred_y_test)
  }
  
  test_matrix[bts, ] <- error_vec_test
  
}
#Vectors to store the difference between the y_pred and y
avg_pred_test_tree <- c()
avg_pred_train_tree <- c()
#Loop to calculate the difference for each prediction
for (iter in 1:253)
{
  avg_pred_train_tree <-
    append(avg_pred_train_tree, mean(train_matrix[, iter]))
  avg_pred_test_tree <-
    append(avg_pred_test_tree, mean(test_matrix[, iter]))
}

#MSE Caluclation

MSE_train <-
  sum((train_data[["medv"]] - avg_pred_train_tree) ** 2) / nrow(train_data)
MSE_test <-
  sum((test_data[["medv"]] - avg_pred_test_tree) ** 2) / nrow(test_data)


###Plot for B/H  vs MSE values
# MSE_train_val <- append(MSE_train_val,MSE_train)
# MSE_test_val <- append(MSE_test_val,MSE_test)
# plot(h[1:4], MSE_train_val, type="l",col="red", main="Plot with B=50", , xlab="Heights",ylab='MSE_values', lty=1, ylim=c(10,50))
# lines(h[1:4], MSE_test_val,col="green", type="l", lty=1)
# legend(5, 50, legend=c("MSE_test", "MSE_train"),
#        col=c("green", "red"), lty=1:2, cex=0.8)