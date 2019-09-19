library(collections)
library(data.tree)

# Calculate the impurity of a node using the Gini index
impurity.gini <- function(values) {
  pos = sum(values == 1)/length(values)
  neg = 1 - pos
  imp = pos*neg
}


# Returns the best split for an attribute of the dataset
best_split <- function(x, x_row, y, minleaf) {
  parent_i <- impurity.gini(y)
  x.sorted <- sort(unique(x))
  x.length <- length(x.sorted)
  x.splitpoints <- x.sorted[2]
  x.splitpoints <- (x.sorted[1:x.length-1]+x.sorted[2:x.length])/2
  best_split_point <- -999
  best_reduction <- -999
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
        best_split_rows <- list(x_row[x > val], x_row[x <= val])
      }
    }
  }
  if (best_split_point == -999) {
    return(NULL)
  }
  return(list(best_reduction, best_split_point, unlist(best_split_rows[1]), unlist(best_split_rows[2])))
}

# Determine the best split over all of the attributes at a certain node
get_split <- function(x_row, y, nfeat, minleaf) {
  x <- credit[x_row, ]
  b_gini <- 0
  for (i in 1:nfeat){
    split <- best_split(x[,i], x_row, y, minleaf)
    
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

toLeafNode <- function(node) {
  max_index <- which.max(node$data_y)
  node$Set(label = node$data_y[max_index])
}

tree.grow <- function(x, y, nmin, minleaf, nfeat){
  attribute_names = colnames(x)
  root <- Node$new('start', data_x = c(1:length(x[,1])), data_y = y)
  nodelist <- list(root)
  while(length(nodelist) > 0) {
    node <- nodelist[[1]]
    nodelist[[1]] <- NULL
    
    if (impurity.gini(node$data_y) > 0 && length(node$data_y) >= nmin) {
      split <- get_split(node$data_x, node$data_y, nfeat, minleaf)
      
      split_attribute <- split[[1]]
      split_value <- split[[2]]
      
      left_data <- split[[3]]
      right_data <- split[[4]]
      
      left_y <- y[left_data]
      right_y <- y[right_data]

      leftchild <- node$AddChild('Bigger', data_x = left_data, data_y = left_y, split_attribute = attribute_names[split_attribute], split_value = split_value)
      rightchild <- node$AddChild('Smaller', data_x = right_data, data_y = right_y, split_attribute = attribute_names[split_attribute], split_value = split_value)
    
      nodelist <- c(nodelist, list(leftchild, rightchild))
    } else {
      toLeafNode(node)
    }
  }
  print(root, 'split_attribute', "split_value", "data_y", "label")
  # return(root)
}

credit <- read.csv('C:/Users/Lisa/Desktop/UU/Data_Mining_2019/credit.txt', header = TRUE)
# # print(credit)
# # b_split <- best_split(credit[,4],credit[,6])
# cx <- credit[,4]

credit.y <- credit[,6]
credit.x <- credit[1:5]
credit.nfeat <- length(credit.x[1,])


tree.grow(credit.x, credit.y, 2, 1, credit.nfeat)

