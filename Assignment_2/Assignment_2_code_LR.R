library(readtext)
library(tm)
library(textstem)
library(e1071)
library(caret)
library(textmineR)
library(RWeka)
library(entropy)
library(glmnet)

loadData <- function(location_t, location_d, from, to){
  data <- matrix(nrow = 1, ncol= 2)
  for(i in from:to){
    fold <- paste("fold", i, sep = "")
    
    list_of_files_t <- list.files(path = paste(location_t,"/", fold,"/", sep=""), pattern = ".*.txt", full.names = TRUE, all.files = FALSE)
    for(fileName in list_of_files_t){
      file <- as.character(readtext(fileName)$text)
      data <- rbind(data, c(file, 1))
    }
  }
  data <- data[-1,]
  for(i in from:to){
    fold <- paste("fold", i, sep = "")
    
    list_of_files_d <- list.files(path = paste(location_d,"/", fold,"/", sep=""), pattern = ".*.txt", full.names = TRUE, all.files = FALSE)
    for(fileName in list_of_files_d){
      file <- as.character(readtext(fileName)$text)
      data <- rbind(data, c(file, 0))
    }
  }
  colnames(data) <- c("text", "label")
  return(data)
}

BiGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=2,max=2))

convert_values <- function(x){
  x <- ifelse(x>0, "yes", "no")
  return(x)
}

# Load the train and test set
train.data <- loadData('./op_spam_v1.4/negative_polarity/truthful_from_Web', './op_spam_v1.4/negative_polarity/deceptive_from_MTurk', 1, 4)
test.data <-loadData('./op_spam_v1.4/negative_polarity/truthful_from_Web', './op_spam_v1.4/negative_polarity/deceptive_from_MTurk', 5, 5)

#Turn data into a corpus
train_corpus <- VCorpus(VectorSource(train.data[,1]))
#Turn data into a corpus
train_corpus <- VCorpus(VectorSource(train.data[,1]))
train_corpus <- train_corpus %>% tm_map(content_transformer(tolower)) %>% tm_map(removeWords, stopwords("english")) %>%
  tm_map(lemmatize_words)%>%  tm_map(removePunctuation) %>%tm_map(removeNumbers) %>% 
  tm_map(stripWhitespace)%>% tm_map(removeWords, c(stopwords("en"), "th", "us", "also")) 

# For unigram
train_dtm_uni <- DocumentTermMatrix(train_corpus)
# For bigram
train_dtm_bi <- DocumentTermMatrix(train_corpus, control = list(tokenize = BiGramTokenizer))

# # Remove sparse terms
train_dtm_uni <- removeSparseTerms(train_dtm_uni, 0.99)
train_dtm_bi <- removeSparseTerms(train_dtm_bi, 0.99)

# Feature selection with mutual information
train_mi_uni <- apply(as.matrix(train_dtm_uni),2, function(x,y){mi.plugin(table(x,y)/length(y))},train.data[,2])
train_mi_uni_order <- order(train_mi_uni, decreasing = T)
train_mi_uni[train_mi_uni_order[1:10]]

train_mi_bi <- apply(as.matrix(train_dtm_bi),2, function(x,y){mi.plugin(table(x,y)/length(y))},train.data[,2])
train_mi_bi_order <- order(train_mi_bi, decreasing = T)
train_mi_bi[train_mi_bi_order[1:10]]

# Create a test set for the unigram and bigram
test_corpus <- VCorpus(VectorSource(test.data[,1]))
test_corpus <- test_corpus %>% tm_map(content_transformer(tolower)) %>% tm_map(removeWords, stopwords("english")) %>%
  tm_map(lemmatize_words)%>%  tm_map(removePunctuation) %>%tm_map(removeNumbers) %>% 
  tm_map(stripWhitespace)%>% tm_map(removeWords, c(stopwords("en"), "th", "us", "also")) 

test_dtm_uni <- DocumentTermMatrix(test_corpus, list(dictionary=dimnames(train_dtm_uni)[[2]]))
test_dtm_bi <- DocumentTermMatrix(test_corpus,control = list(tokenize = BiGramTokenizer, dictionary=dimnames(train_dtm_bi)[[2]]))

dim(train_dtm_uni)
reviews.glmnet.uni <- cv.glmnet(as.matrix(train_dtm_uni), as.numeric(train.data[,2]), family = "binomial", type.measure = "class")
plot(reviews.glmnet.uni)
coef(reviews.glmnet.uni, s="lambda.1se")
reviews.logreg.pred <- predict(reviews.glmnet.uni, newx = as.matrix(test_dtm_uni), s="lambda.1se", type="class")
result_uni <- confusionMatrix(data = as.factor(reviews.logreg.pred), reference = as.factor(test.data[,2]))
result_uni$byClass


reviews.glmnet.bi <- cv.glmnet(as.matrix(train_dtm_bi), as.numeric(train.data[,2]), family = "binomial", type.measure = "class")
plot(reviews.glmnet.bi)
coef(reviews.glmnet.bi, s="lambda.1se")
reviews.logreg.pred <- predict(reviews.glmnet.bi, newx = as.matrix(test_dtm_bi), s="lambda.1se", type="class")
result_bi <- confusionMatrix(data = as.factor(reviews.logreg.pred), reference = as.factor(test.data[,2]))
result_bi$byClass
