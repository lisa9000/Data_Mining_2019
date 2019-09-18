library(collections)
library(data.tree)

# Calculate the impurity of a node using the Gini index
impurity.gini <- function(values) {
  pos = sum(values == 1)/length(values)
  neg = 1 - pos
  imp = pos*neg
}

# Get the x-row index for a certain split points
get_x_index <- function(x, split_point){ 
  best_small_split = c()
  best_big_split = c()
  cs= 1 
  cb= 1
  for (i in 1:length(x)){
    if (x[i] < split_point) {
      best_small_split[cs] = i
      cs = cs+1
    } else {
      best_big_split[cb] = i
      cb = cb+1
    }
  }
  return(list(best_big_split, best_small_split))
}

# Returns the best split for an attribute of the dataset
best_split <- function(x, y) {
  parent_i <- impurity.gini(y)
  x.sorted <- sort(unique(x))
  x.length <- length(x.sorted)
  x.splitpoints <- x.sorted[2]
  x.splitpoints <- (x.sorted[1:x.length-1]+x.sorted[2:x.length])/2
  best_split_point <- 0
  best_reduction <- 0
  # Loop over the data to determine the best split point based on impurity reduction
  for (val in x.splitpoints) {
    big_split <- y[x > val]
    small_split <- y[x <= val]
    big_imp <- impurity.gini(big_split)
    small_imp <- impurity.gini(small_split)
    reduction_imp = parent_i -(small_imp * (length(small_split)/length(x))+big_imp*(length(big_split)/length(x)))
    if (reduction_imp > best_reduction) {
      best_split_point <- val
      best_reduction <- reduction_imp
      best_split_rows = get_x_index(x, best_split_point)
     }
  }
  return(list(best_reduction, best_split_point, unlist(best_split_rows[1]), unlist(best_split_rows[2])))
}

# Determine the best split over all of the attributes at a certain node
get_split <- function(x, y, nfeat) {
  b_gini <- 0
  for (i in 1:nfeat){
    split <- best_split(x[,i], y)
    gini <- unlist(split[1])
    # The best split is the split with best reduction
    if (gini > b_gini){
      b_gini <- gini
      b_split <- append(i,split[2:4])
    }
  }
  return(b_split)
}

toLeafNode <- function(tree, node, y) {
  data <- c(node$Get("data_l"), node$Get("data_r"))
}

split <- function(node, x, y, nmin, minleaf, nfeat, attributes) {
  l <- node$Get('data_l')
  r <- node$Get('data_r')
  
  if (length(left) < nmin){
    toLeafNode(tree, node, y)
  } else {
    split_l <- get_split(x, y, nfeat)
    print(split_l)
    node$AddChild('left', attribute_n = attributes[unlist(split_l[1])], split_point = split_l[2], data_l = split_l[3], data_r = split_l[4])
  } 
  if (length(right) < nmin) {
    toLeafNode(tree, node, y)
  } else {
    split_l <- get_split(x, y, nfeat)
    node$AddChild('left', attribute_n = attributes[unlist(split_l[1])], split_point = split_l[2], data_l = split_l[3], data_r = split_l[4])
  }
  # print(node, 'attribute_n','splitpoint', 'data_l', 'data_r')
}


tree.grow <- function(x, y, nmin, minleaf, nfeat){
  # Get the attribute names
  attributes <- colnames(x)
  root_split <- get_split(x, y, nfeat)
  root_name <- attributes[unlist(root_split[1])]
  # print(root_split[2])
  tree <- Node$new("start", attribute_n = root_name, split_point = root_split[2], data_l = root_split[3], data_r = root_split[4])
  # print(tree, 'attribute_n','splitpoint', 'data_l', 'data_r')
  split(tree, x, y, nmin, minleaf, nfeat, attributes)
}

credit <- read.csv('C:/Users/Lisa/Desktop/UU/Data_Mining_2019/credit.txt', header = TRUE)
# print(credit)
b_split <- best_split(credit[,4],credit[,6])
cx <- credit[,4]

credit.y <- credit[,6]
credit.x <- credit[1:5]
credit.nfeat <- length(credit.x[1,])
get_split(credit.x, credit.y, credit.nfeat)

tree.grow(credit.x, credit.y, 2, 1, credit.nfeat)

