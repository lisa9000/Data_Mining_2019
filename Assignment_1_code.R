library(data.tree)

# Calculate the impurity of a node using the Gini index
impurity.gini <- function(values) {
  pos = sum(values == 1)/length(values)
  neg = 1 - pos
  imp = pos*neg
}

mode <- function(x) {
  x_unique <- unique(x)
  return(x_unique[which.max(tabulate(match(x, x_unique)))])
}

# Returns the best split for an attribute of the dataset
best.split <- function(x, x_row, y, minleaf) {
  parent_i <- impurity.gini(y)
  x <- as.numeric(as.character(x))
  x_unique <- unique(x)
  x_sorted <- sort(x_unique)
  x_length <- length(x_sorted)
  if (length(x_sorted) < 2) {
    return(NULL)
  }
  x_splitpoints <- (x_sorted[1:(x_length-1)] + x_sorted[2:x_length])/2
  best_splitpoint <- -999
  best_reduction <- -999
  # Loop over the data to determine the best split point based on impurity reduction
  for (val in x_splitpoints) {
    left_split <- y[x > val]
    right_split <- y[x <= val]
    left_imp <- impurity.gini(left_split)
    right_imp <- impurity.gini(right_split)
    reduction_imp = parent_i -(right_imp * (length(right_split)/length(x))+left_imp*(length(left_split)/length(x)))
    if (length(right_split) >= minleaf && length(left_split) >= minleaf) {
      if (reduction_imp > best_reduction) {
        best_splitpoint <- val
        best_reduction <- reduction_imp
        best_splitrows <- list(x_row[x > val], x_row[x <= val])
      }
    }
  }
  if (best_splitpoint == -999) {
    return(NULL)
  }
  return(append(c(best_reduction, best_splitpoint),best_splitrows))
}

# Determine the best split over all of the attributes at a certain node
get.split <- function(x_row, y, nfeat, minleaf) {
  # x <- pima_data[x_row, ]
  x <- data_eclipse_2.0[x_row, ]
  b_gini <- 0
  predictors <- sample(ncol(x), nfeat)
  for (i in predictors){
    split <- best.split(x[,i], x_row, y, minleaf)
    
    if (is.null(split)) next
    gini <- unlist(split[1])
    # The best split is the split with best reduction
    if (gini > b_gini){
      b_gini <- gini
      b_split <- append(i,split[2:4])
    }
  }
  return(b_split)
}

to.leaf <- function(node) {
  node$Set(label = mode(node$dataY))
}

tree.grow <- function(x, y, nmin, minleaf, nfeat){
  attribute_names = colnames(x)
  root <- Node$new('start', dataX = c(1:length(x[,1])), dataY = y)
  nodelist <- list(root)
  
  while(length(nodelist) > 0) {
    node <- nodelist[[1]]
    nodelist[[1]] <- NULL
    
    if (impurity.gini(node$dataY) > 0 && length(node$dataY) >= nmin) {
      split <- get.split(node$dataX, node$dataY, nfeat, minleaf)
      
      split_attribute <- split[[1]]
      split_value <- split[[2]]
      
      left_data <- split[[3]]
      right_data <- split[[4]]
      
      left_y <- y[left_data]
      right_y <- y[right_data]
      node$Set(splitAttribute = attribute_names[split_attribute], splitValue = split_value)
      leftchild <- node$AddChild('leftChild', dataX = left_data, dataY = left_y)
      rightchild <- node$AddChild('rightChild', dataX = right_data, dataY = right_y)
    
      nodelist <- c(nodelist, list(leftchild, rightchild))
    } else {
      to.leaf(node)
    }
  }
  return(root)
}

tree.classify <- function(x, tr) {
  y <- NULL
  for (row in 1:nrow(x)) {
    node <- tr
    
    while(!isLeaf(node)) {
      if (as.numeric(as.character(x[row, node$splitAttribute])) > node$splitValue) {
        node <- node$leftChild
      } else {
        node <- node$rightChild
      }
    }
    y[row] <- node$label
  }
  return(y)
}

tree.grow.bag <- function(x, y, nmin, minleaf, nfeat, m) {
  trees <- c()
  for (i in 1:m) {
    samples <- sample(nrow(x), nrow(x), TRUE)
    trees <- append(trees, tree.grow(x[samples, ], y[samples], nmin, minleaf, nfeat))
  }
  return(trees)
}

tree.classify.bag <- function(x, tr) {
  predictions <- c()
  for (i in 1:nrow(x)) {
    labels <- c()
    for (tree in tr) {
      labels <- append(labels, tree.classify(x[i,], tree))
    }
    predictions[i] <- mode(labels)
  }
  return(predictions)
}

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
  print(c(TP, FP, TN, FN))
  return(c(precision, recall, accuracy))
}


# training data
eclipse_2.0 <- read.csv2('C:/Users/Lisa/Desktop/UU/Data_Mining_2019/eclipse-metrics-packages-2.0.csv')
data_eclipse_2.0 <- eclipse_2.0[,c(3, 5:44)]
eclipse_2.0_labels <- as.numeric(eclipse_2.0[,4] > 0)

# test data
eclipse_3.0 <- read.csv2('C:/Users/Lisa/Desktop/UU/Data_Mining_2019/eclipse-metrics-packages-3.0.csv')
data_eclipse_3.0 <- eclipse_3.0[,c(3, 5:44)]
eclipse_3.0_labels <- as.numeric(eclipse_3.0[,4] > 0)

tree_1 <- tree.grow(data_eclipse_2.0, eclipse_2.0_labels, 15, 5, 41)
predictions_1 <- tree.classify(data_eclipse_3.0, tree_1)
measures_1 <- measures(eclipse_3.0_labels, predictions_1)


trees_1 <- tree.grow.bag(data_eclipse_2.0, eclipse_2.0_labels, 15, 5, 41, 100)
predictions_2 <- tree.classify.bag(data_eclipse_3.0, trees_1)
measures_2 <- measures(eclipse_3.0_labels, predictions_2)
print(measures_2)

trees_2 <- tree.grow.bag(data_eclipse_2.0, eclipse_2.0_labels, 15, 5, 6, 100)
predictions_3 <- tree.classify.bag(data_eclipse_3.0, trees_2)
measures_3 <- measures(eclipse_3.0_labels, predictions_3)

print(measures(eclipse_3.0_labels, predictions_1))
head(data_eclipse_2.0)
summary(as.numeric(as.character(data_eclipse_2.0[,ncol(data_eclipse_2.0)])))

        