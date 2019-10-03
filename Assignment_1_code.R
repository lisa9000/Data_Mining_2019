library(data.tree)

# Calculate the impurity of a node using the Gini index
#   values: A binary vector
impurity.gini <- function(values) {
  len <- length(values)
  pos <- sum(values == 1)/len
  neg <- 1-pos
  return(neg*pos)
}

# Calculate the class label
#   values: a vector of binary class values
mode <- function(values) return(sum(values)>(0.5*length(values)))

# Returns the best split for an attribute of the dataset
#   x: The rows of the dataset corresponding to x_row
#   x_row: The row numbers of the dataset in the current node that can be used for splitting
#   y: The class labels of the corresponding part of the dataset of x_row
#   minleaf: The minimum number of datapoints that needs to be in a node to not be a leafnode
best.split <- function(x, x_row, y, minleaf) {
  
  # Find the possible split points for the data
  x_unique <- unique(x)
  x_sorted <- sort(x_unique)
  x_length <- length(x_sorted)
  # If there is only one split point a split is not possible, since the length of the data is too small
  if (length(x_sorted) < 2) {
    return(NULL)
  }
  x_splitpoints <- (x_sorted[1:(x_length-1)] + x_sorted[2:x_length])/2
  
  # Calculate impurity of the parent node
  parent_i <- impurity.gini(y)
  best_splitpoint <- -999
  best_reduction <- -999
  
  # Loop over the data to determine the best split point based on the highest impurity reduction
  for (val in x_splitpoints) {
    left_split <- y[x > val]
    right_split <- y[x <= val]
    
    # Only consider splits with that meet the minleaf constraint, others are not allowed
    if (length(right_split) >= minleaf && length(left_split) >= minleaf) {
      left_imp <- impurity.gini(left_split)
      right_imp <- impurity.gini(right_split)
      reduction_imp <- parent_i - ((right_imp * (length(right_split)/length(x)))+(left_imp*(length(left_split)/length(x))))
      if (reduction_imp > best_reduction) {
        best_splitpoint <- val
        best_reduction <- reduction_imp
        best_splitrows <- list('left_split'= x_row[x > val], 'right_split'= x_row[x <= val])
      }
    }
  }
  # If there was no split found
  if (best_splitpoint == -999) {
    return(NULL)
  }
  return(append(list('best_reduction' = best_reduction, 'best_splitpoint'= best_splitpoint), best_splitrows))
}

# Determine the best split over all of the attributes at a certain node
#   x_row: a vector with the row numbers of the dataset for a given node
#   y: a binary vector with the class labels corresponding to x_row in the dataset
#   nfeat: The number of features that need to be considerd by the sampeling for predictors
#   minleaf: The minimum number of datapoints that needs to be in a node to not be a leaf node
get.split <- function(x_row, y, nfeat, minleaf) {
  
  # Extract the relevant rows of the data using x_row
  x <- train_data[x_row, ]
  b_gini <- -999
  b_split <- NULL
  predictors <- sample(ncol(x), nfeat)
  
  # Loop over the sampled predictors until the best split is found
  for (i in predictors){
    split <- best.split(x[,i], x_row, y, minleaf)
    
    # If there was no split found go to next predictor
    if (is.null(split)) next
    
    # Unlist the impurity value since we don't need it further in the program
    gini <- unlist(split$best_reduction)
   
     # The best split is the split with best reduction
    if (gini > b_gini){
      b_gini <- gini
      b_split <- append(list('attribute'= i), split[2:4])
    }
  }
  return(b_split)
  
}

# Sets a class labels for a node if it becomes a leaf node
#   node: the node that needs to have it's class label set
to.leaf <- function(node) {
  
  # Uses the classlabels that are stored in the node
  node$Set(label = mode(node$dataY))
}

# Grows a tree datastructure
#   x_row: The row numbers of the dataset
#   y: The class labels of the dataset
#   nmin: The minimm length of the data in a node to not be a leaf node. 
#   minleaf: minleaf: The minimum number of datapoints that needs to be in a node to not be a leafnode
#   nfeat: The number of features that need to be sampled from the dataset
tree.grow <- function(x_row, y, nmin, minleaf, nfeat){
  # Collect the columnames from the dataset
  attribute_names = colnames(train_data)
  
  # Set the rootnode with the full dataset row numbers and the all it's class labels
  root <- Node$new('start', dataX = x_row, dataY = y)
  
  # Convert the root into a list
  nodelist <- list(root)
  
  # While there are nodes in the nodes list expand the tree
  while(length(nodelist) > 0) {
    
    # Select the first node
    node <- nodelist[[1]]
    nodelist[[1]] <- NULL
    
    # Only consider a split if the node is not already pure of it the lenght of the dataset is shorter than nmin
    # Else the node is a leaf node
    if (impurity.gini(node$dataY) > 0 && length(node$dataX) > nmin) {
      
      # The best split point as given by best split
      split <- get.split(node$dataX, node$dataY, nfeat, minleaf)
      
      # # If there was no split found make the node a leaf node else create childeren based on the split
      if(is.null(split)) {
        isLeaf(node)
      } else {
        
        # Split the class labels into corresponding to the left and right data
        left_y <- y[split$left_split]
        right_y <- y[split$right_split]
        
        # Set the split attribute and value for the current node and create the childeren
        node$Set(splitAttribute = attribute_names[split$attribute], splitValue = split$best_splitpoint)
        leftchild <- node$AddChild('leftChild', dataX = split$left_split, dataY = left_y)
        rightchild <- node$AddChild('rightChild', dataX = split$right_split, dataY = right_y)
        
        # Add the childeren to the leaf node
        nodelist <- append(nodelist, list(leftchild, rightchild))
      }
    } else {
      to.leaf(node)
    }
  }
  return(root)
}


# Predicts the class labels for a given dataset and a tree structure
#   data: The dataset that needs to have it's class labels predicted
#   tr: The data.tree structure build by tree.grow
tree.classify <- function(data, tr) {
  y <- c(1:100)
  for (i in 1:nrow(data)) {
    node <- tr
    # While the node is not a leaf node, (isLeaf() is a function of data.tree package)
    while(!isLeaf(node)) {
      
      # If the value of the data is larger then the split value go to left child if smaller go to right child
      if (data[i, node$splitAttribute] > node$splitValue) {
        node <- node$leftChild
      } else {
        node <- node$rightChild
      }
    }
    
    # Retrieve the class label and store at the corresponding spot in the y vector
    y[i] <- node$label
  }
  return(y)
}

# Multiple times grows a tree using tree.grow and stores them in a list
#   x_row: The row numbers of the dataset
#   y: The class labels of the dataset
#   nmin: The minimm length of the data in a node to not be a leaf node. 
#   minleaf: The minimum number of datapoints that needs to be in a node to not be a leafnode
#   nfeat: The number of features that need to be sampled from the dataset
#   m: The number of trees that need to be grown
tree.grow.bag <- function(x_row, y, nmin, minleaf, nfeat, m) {
  trees <- c()
  for (i in 1:m) {
    
    # Sample  from the rows of x to create a distinct datasets
    samples <- sample(length(x_row), length(x_row), TRUE)
    
    # Give the samples to tree.grow as the row numbers of the dataset
    trees <- append(trees, tree.grow(samples, y[samples], nmin, minleaf, nfeat))
  }
  return(trees)
}

# Classifies the data using a list of trees generated by tree.grow.bag
#   data: The dataset that needs to have it's class labels predicted
#   tr: A list of containing m data.tree structures created by tree.grow.bag
tree.classify.bag <- function(data, tr) {
  
  # Create a matrix for the predictions of each tree
  labels <- matrix(nrow=nrow(data), ncol=length(tr))
  predictions <- c()
  # Classify the dataset for each tree in tr and put the predictions in the right column of the labels matrix
  for (i in 1:length(tr)) {
    labels[,i] <- tree.classify(data, tr[[i]])
  }
  # Calculate the final class label for each row based on what each tree predicted
  for(i in 1:nrow(labels)) {
    predictions[i] <- sum(labels[i,])>(0.5*length(labels[i,]))
  }
  return(predictions)
}

# Gives the precision, recall and accuracy for a two binary vectors
#   y: the golden standard labels
#   predictions: the predictions made by one of the tree.classify functions
measures <- function(y, predictions) {
  TP <- 0
  FP <- 0
  TN <- 0 
  FN <- 0
  for (i in 1:length(predictions)){
    if (predictions[i] == 1 && y[i] == 1) TP <- TP + 1
    if (predictions[i] == 1 && y[i] == 0) FP <- FP + 1
    if (predictions[i] == 0 && y[i] == 0) TN <- TN + 1
    if (predictions[i] == 0 && y[i] == 1) FN <- FN + 1
  }
  precision <- TP/(TP+FP)
  recall <- TP/(TP+FN)
  accuracy <- (TP+TN)/length(predictions)
  return(list('precision'= precision, 'recall'= recall, 'accuracy'= accuracy))
}


# Load in training and test data
train_data <- read.csv2("./eclipse-metrics-packages-2.0.csv")
test_data <- read.csv2("./eclipse-metrics-packages-3.0.csv")

# Create the class labels from the post data
train_classes <- train_data[,4] > 0
test_classes <- test_data[,4] > 0

# Take relevant columns from data
train_data <- train_data[,c(3, 5:44)]
test_data <- test_data[,c(3, 5:44)]

# Convert the columns to numeric data to prevent errors
for (col in 1:ncol(train_data)){
  train_data[,col] <- as.numeric(as.character(train_data[,col]))
  test_data[,col] <- as.numeric(as.character(test_data[,col]))
}

# Grow a single tree, since the dataset is global only row numbers ar given to tree.grow
tree_1 <- tree.grow(c(1:nrow(train_data)), train_classes, 15, 5, 41)
# Prints the tree in the correct way
print(tree_1, 'splitValue', "splitAttribute")
predictions_1 <- tree.classify(test_data, tree_1)
measures_1 <- measures(test_classes, predictions_1)
print(measures_1)
# Prints the confusion matrix
print(table(predictions_1, test_classes))

# Grow trees using random forest and classify
trees_1 <- tree.grow.bag(c(1:nrow(train_data)), train_classes, 15, 5, 41, 100)
# print(trees_1[[1]], "splitValue", 'splitAttribute')
predictions_2 <- tree.classify.bag(test_data, trees_1)
measures_2 <- measures(test_classes, predictions_2)
print(measures_2)
print(table(predictions_2, test_classes))

# Grow trees using bagging and classify
trees_2 <- tree.grow.bag(c(1:nrow(train_data)), train_classes, 15, 5, 6, 100)
# print(trees_2[[1]], "splitValue", 'splitAttribute')
predictions_3 <- tree.classify.bag(test_data, trees_2)
measures_3 <- measures(test_classes, predictions_3)
print(measures_3)
print(table(predictions_3, test_classes))
