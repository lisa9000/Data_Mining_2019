library(readtext)
library(tm)
library(textstem)
library(e1071)
library(caret)
library(textmineR)
library(RWeka)


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

# Preprocess the train data, standard preprocessing. 
train_corpus <- tm_map(train_corpus, removeWords, stopwords("english"))
train_corpus <- tm_map(train_corpus, tolower)
train_corpus <- tm_map(train_corpus, stripWhitespace)
train_corpus <- tm_map(train_corpus, removePunctuation)
train_corpus <- tm_map(train_corpus, removeNumbers)
# train_corpus <- tm_map(train_corpus, )

# Stem or lemmatize document
train_corpus <- tm_map(train_corpus, stemDocument)
train_corpus <- tm_map(train_corpus, lemmatize_strings)
train_corpus <- tm_map(train_corpus, BiGramTokenizer)

# Convert to plaint text document otherwise bigram document term matrix can't be made
train_corpus <- tm_map(train_corpus, PlainTextDocument)
train_corpus[[1]]$content

# For unigram
train_dtm_uni <- DocumentTermMatrix(train_corpus, control = list(tolower = TRUE, stopwords = TRUE, removePunctuation = TRUE,
                                                             removeNumbers = TRUE, stripWhitespace = TRUE, lemmatize_words = TRUE, weighting = weightTf))

# For bigram
train_dtm_bi <- DocumentTermMatrix(train_corpus, control = list(tolower = TRUE, stripWhitespace = TRUE, stopwords = TRUE, removeNumbers = TRUE, lemmatize_words = TRUE, tokenize = BiGramTokenizer))

train_dtm_uni$dimnames$Terms[1:10]
dim(train_dtm_bi)

test_corpus <- VCorpus(VectorSource(test.data$text))

# Create a test set for the unigram and bigram
test_dtm_uni <- DocumentTermMatrix(test_corpus,control = list(tolower = TRUE, stopwords = TRUE, removePunctuation = TRUE,
                                                          removeNumbers = TRUE, stripWhitespace = TRUE, lemmatize_words = TRUE))
test_dtm_bi <- DocumentTermMatrix(test_corpus,control = list(tolower = TRUE, stopwords = TRUE, removePunctuation = TRUE,
                                                          removeNumbers = TRUE, stripWhitespace = TRUE, lemmatize_words = TRUE, tokenize= BiGramTokenizer))

# Remove sparse terms
train_dtm_freq_uni <- removeSparseTerms(train_dtm_uni, 0.975)
train_dtm_freq_bi <- removeSparseTerms(train_dtm_bi, 0.99)

# Create train and test unigram
train <- apply(train_dtm_freq_uni, MARGIN = 2, convert_values)
test <- apply(test_dtm_uni, MARGIN = 2, convert_values)

# Create train and test categorical Biram
train_bi <- apply(train_dtm_freq_bi, MARGIN = 2, convert_values)
test_bi <- apply(test_dtm_bi, MARGIN = 2, convert_values)

# Naive Bayes classiffier unigram
NV_classifier <- naiveBayes(train, as.factor(train.data[,2]))
NV_pred <- predict(NV_classifier, test)

# Naive bayes classifier Bigram
NV_classifier_bi <- naiveBayes(train_bi, as.factor(train.data[,2]))
NV_pred_bi <- predict(NV_classifier_bi, test_bi)

# ConfusionMatrices
confusionMatrix(data = NV_pred, reference = as.factor(test.data[,2]), positive = "1",dnn = c("prediction", "actual"))
confusionMatrix(data = NV_pred_bi, reference = as.factor(test.data[,2]), positive = "1",dnn = c("prediction", "actual"))
