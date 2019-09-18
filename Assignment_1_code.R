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
best_split <- function(x, y, minleaf) {
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
    if (length(small_split) >= minleaf && length(big_split) >= minleaf) {
      if (reduction_imp > best_reduction) {
        best_split_point <- val
        best_reduction <- reduction_imp
        best_split_rows = get_x_index(x, best_split_point)
      }
    } 
  }
  return(list(best_reduction, best_split_point, unlist(best_split_rows[1]), unlist(best_split_rows[2])))
}

# Determine the best split over all of the attributes at a certain node
get_split <- function(x, y, nfeat, minleaf) {
  b_gini <- 0
  for (i in 1:nfeat){
    split <- best_split(x[,i], y, minleaf)
    gini <- unlist(split[1])
    # The best split is the split with best reduction
    if (gini > b_gini){
      b_gini <- gini
      b_split <- append(i,split[2:4])
    }
  }
  return(b_split)
}

tree.grow <- function(x, y, nmin, minleaf, nfeat){
  root <- Node$new('start', data_x = x, data_y = y)
  nodelist <- list(root)
  while(length(nodelist) > 0) {
    node <- nodelist[[1]]
    nodelist[[1]] <- NULL
    
    if (impurity.gini(node$data_y) > 0 && length(node$data_y) >= nmin) {
      split <- get_split(node$data_x, node$data_y, minleaf)
      split_attribute <- split[1]
      split_value <- split[2]
      print(split_attribute)
      print(split_value)
    }
  }
  
}

credit <- read.csv('C:/Users/Lisa/Desktop/UU/Data_Mining_2019/credit.txt', header = TRUE)
# print(credit)
# b_split <- best_split(credit[,4],credit[,6])
cx <- credit[,4]

credit.y <- credit[,6]
credit.x <- credit[1:5]
credit.nfeat <- length(credit.x[1,])


tree.grow(credit.x, credit.y, 2, 1, credit.nfeat)

