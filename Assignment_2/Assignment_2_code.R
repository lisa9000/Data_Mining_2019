library(readtext)

loadData <- function(location_t, location_d, from, to){
  train_data <- matrix(nrow = 1, ncol= 2)
  for(i in from:to){
    fold <- paste("fold", i, sep = "")
    
    list_of_files_t <- list.files(path = paste(location_t,"/", fold,"/", sep=""), pattern = ".*.txt", full.names = TRUE, all.files = FALSE)
    for(fileName in list_of_files_t){
      file <- readtext(fileName)$text
      train_data <- rbind(train_data, c(file, 1))
    }
  }
  train_data <- train_data[-1,]
  for(i in from:to){
    fold <- paste("fold", i, sep = "")
    
    list_of_files_d <- list.files(path = paste(location_d,"/", fold,"/", sep=""), pattern = ".*.txt", full.names = TRUE, all.files = FALSE)
    for(fileName in list_of_files_d){
      file <- readtext(fileName)$text
      train_data <- rbind(train_data, c(file, 0))
    }
  }
  return(train_data)
}

train.data <- loadData('./op_spam_v1.4/negative_polarity/truthful_from_Web', './op_spam_v1.4/negative_polarity/deceptive_from_MTurk', 1, 4)
test.data <- loadData('./op_spam_v1.4/negative_polarity/truthful_from_Web', './op_spam_v1.4/negative_polarity/deceptive_from_MTurk', 5, 5)



