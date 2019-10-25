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


# train_dtm <- DocumentTermMatrix(train_corpus[1], control = list(tolower = TRUE, stopwords = TRUE, removePunctuation = TRUE,
                                                             # removeNumbers = TRUE, stripWhitespace = TRUE, lemmatize_words = TRUE, tokenize = function(x) NGramTokenizer(x, Weka_control(min=2,max=2))))
train_dtm <- TermDocumentMatrix(train_corpus[1], control = list(tolower = TRUE, stripWhitespace = TRUE, stopwords = TRUE,removeNumbers = TRUE, tokenize = BiGramTokenizer))
train_dtm <- DocumentTermMatrix(train_corpus)
train_dtm$dimnames$Terms[10:100]
dim(train_dtm)
test_corpus <- VCorpus(VectorSource(test.data$text))
test_dtm <- DocumentTermMatrix(test_corpus,)


dim(train_dtm)
dim(test_dtm)
train_dtm_freq <- removeSparseTerms(train_dtm, 0.95)
dim(train_dtm_freq)

convert_values <- function(x){
  x <- ifelse(x>0, "yes", "no")
  return(x)
}

train <- apply(train_dtm_freq, MARGIN = 2, convert_values)
test <- apply(test_dtm, MARGIN = 2, convert_values)

NV_classifier <- naiveBayes(train, train.data$label)
NV_pred <- predict(NV_classifier, test)


confusionMatrix(data = NV_pred, reference = test.data$label, positive = "1",dnn = c("prediction", "actual"))
