stemming <- function(text) {
  #Takes a vector of reviews from businessUserData
  #
  #Args: 
  # text: vector of reviews
  #Returns:
  # afterStemText: vector of reviews after stemming
  
  #vector of words
  List <- strsplit(text, " ")
  Words=unlist(List)
  afterStemText = c()
  #stemming on all reviews
  for(k in 1:length(text)){
    #remove special characters and split the string
    businessUserText <- gsub("\"", "", trim(text[k]))
    businessUserText <- gsub("\\(|\\)", "", businessUserText)
    businessUserVector <- strsplit(businessUserText, " ")
    businessUserWords = unlist(businessUserVector)
    #stemming
    stemDocCorpus <- stemDocument(businessUserWords)
    #stem completion
    stemCompleteDocCorpus <-  stemCompletion(stemDocCorpus, dictionary=Words)
    
    f=c()
    stemText=c()
    for(i in 1:length(stemCompleteDocCorpus)){
      f = c(f,stemCompleteDocCorpus[[i]])
      stemText[i] = paste(f, collapse=" ")
    }
    # Review stored in vector after stemming
    afterStemText[k] = stemText[length(stemText)]
  }
  return(afterStemText)
}


generateTermDocumentMatrix <- function(reviews, ng) {
  #Takes a vector of reviews from businessUserData and n-gram size
  #
  #Args: 
  # reviews: vector of reviews (after stemming)
  # ng: n-gram size
  #Returns:
  # doctm: term document matrix for n-grams
  
  #create a corpus (a collection of documents)
  docCorpus <- Corpus(VectorSource(reviews))
  #remove url
  removeURL <- function(x) gsub('http.*\\s*', '', x)
  docCorpus <- tm_map(docCorpus, content_transformer(removeURL))
  #remove punctuation
  docCorpus <- tm_map(docCorpus, removePunctuation)
  #to lower case
  docCorpus<- tm_map(docCorpus, content_transformer(tolower))
  #remove numbers
  docCorpus <- tm_map(docCorpus, removeNumbers)
  #remove space
  docCorpus <- tm_map(docCorpus, stripWhitespace)
  
  options(mc.cores=1) 
  #prepare tokens of size 'n'
  ngramTokenizer <- function(x) RWeka::NGramTokenizer(x, Weka_control(min = ng, max = ng)) 
  #term document matrix for n-grams
  doctm <- TermDocumentMatrix(docCorpus, control = list(tokenize = ngramTokenizer)) 
  #return Term Document Matrix
  return(doctm)
}


calculateTfidf=function(mat){
  tf <- mat
  id=function(col){
    sum(!col==0)
  }
  idf <- log(nrow(mat)/apply(mat, 2, id))
  tfidf <- mat
  for(word in names(idf)){
    tfidf[,word] <- as.integer(tf[,word] * idf[word])
  }
  return(tfidf)
}


# Function to trim a string
trim <- function( x ) {
  #Takes a string
  #
  #Args: 
  # x: string
  #Returns:
  # string after trim
  
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

# Function to get last word of a sentence
lastWord <- function(sentence) {
  #Takes a sentence as input
  #
  #Args: 
  # sentence: string
  #Returns:
  # last word of the sentence
  
  return(word(sentence,-1))
}

# Function to return firstwords (all except last word)
firstWords <- function(sentence) {
  #Takes a sentence as input
  #
  #Args: 
  # sentence: string
  #Returns:
  # all word except the last word of the sentence as a string
  
  return(word(sentence,start=1,end=-2))
}



KNSmoothing = function(dt, n){
  # Takes a data table and n-gram size as input
  #
  # Args: 
  #    dt: a data table with term, nextTerm and frequency
  #    n:  the n-gram size 
  # Returns:
  #    dt: data table with smoothed probabilities
  
  col <- colnames(dt)
  col <- col[1:n]
  
  
  # Y = N_c / (N_c + 2N_(c+1)), where N_c is the count of ngrams with count==c (for Kneser-Ney)
  #     Note: This is a simplified calculation of Y using only the two lowest kept freqs.
  c1 <- min(dt$Freq) # The lowest available Freq
  c2 <- min(subset(dt, Freq>c1)$Freq) # And the second lowest
  Y <- nrow(dt[Freq == c1]) / (nrow(dt[Freq == c1]) + 2 * nrow(dt[Freq == c2])) # 1:0.50 2:0.56 3+:0.62  
  
  # D = Discounting parameter different for freq==1, freq==2 and freq>=3
  #     Ref Goodman and Chen (1999)
  dt[, D := 0]
  dt[Freq == 1]$D <- 1 - 2 * Y * (nrow(dt[Freq == 2]) / nrow(dt[Freq == 1]))
  dt[Freq == 2]$D <- 2 - 3 * Y * (nrow(dt[Freq == 3]) / nrow(dt[Freq == 2]))
  dt[Freq > 2]$D  <- 3 - 4 * Y * (nrow(dt[Freq == 4]) / nrow(dt[Freq == 3]))
  
  # Nom = First nominator in P_KN formula ( max{c(w_i-1, w_i)-D, 0} )
  dt <- dt[, Nom := pmax(Freq-D, 0)]
  
  # Denom = Denominator is the count of the preceding word(s) 
  if(n==1) {
    dt <- dt[, Denom := sum(Freq)]
  } else if (n==2) {
    dt <- dt[, .(nextWord, Freq, D, Nom, Denom = sum(Freq)), by = word1]
  } else if (n==3) {
    dt <- dt[, .(nextWord, Freq, D, Nom, Denom = sum(Freq)), by = list(word2, word1)]
  } else if (n==4) {
    dt <- dt[, .(nextWord, Freq, D, Nom, Denom = sum(Freq)), by = list(word3, word2, word1)]
  }
  
  
  # NN = number of word types that follows w_i-1 in the training data
  if(n==1) {
    dt <- dt[, .(nextWord, Freq, D, Nom, Denom, NN = length(nextWord))]
  } else if (n==2) {
    dt <- dt[, .(nextWord, Freq, D, Nom, Denom, NN = length(nextWord)), by=word1]
  } else if (n==3) {
    dt <- dt[, .(nextWord, Freq, D, Nom, Denom, NN = length(nextWord)), by=list(word2, word1)]
  } else if (n==4) {
    dt <- dt[, .(nextWord, Freq, D, Nom, Denom, NN = length(nextWord)), by=list(word3, word2, word1)]
  }
  
  
  # L  = Lambda, normalizing constant, the probability mass we've discounted
  dt[, L := (D / Freq) * NN]
  
  # N = The number of different ngrams this nextword completes in training set 
  #     (c(w_(i-1)) for Kneser-Ney). Used in P_continutation (PC)
  if(n==1) {
    dt <- dt[, .(nextWord, Freq, D, Nom, Denom, NN, L, .N)]
  } else if (n==2) {
    dt <- dt[, .(word1, Freq, D, Nom, Denom, NN, L, .N), by=nextWord]
  } else if (n==3) {
    dt <- dt[, .(word2, word1, Freq, D, Nom, Denom, NN, L, .N), by=nextWord]
  } else if (n==4) {
    dt <- dt[, .(word3, word2, word1, Freq, D, Nom, Denom, NN, L, .N), by=nextWord]
  }
  
  
  # PC = P_continuation
  dt[, PC := N / nrow(dt)] # Count of this novel continuation div. by number of unique grams
  
  # Prob_KN - Estimated KN probability
  dt[, P_KN := (Nom/Denom) + ((D/Denom) * NN) * PC]
  
  all_freq <- sum(dt$Freq)
  dt[, leftOver:=(1-sum((((Nom/Denom) + ((D/Denom) * NN) * PC)*Freq)/all_freq))]
  
  return(dt)
}


getLastTerms = function(inputString,num){
  # Takes a string and number of terms to be returned
  #
  # Args:
  # inputString: string
  # num: number of last terms to be returned
  # Returns:
  # prints last 'num' terms
  
  # Preprocessing
  inputString = gsub("[[:space:]]+", " ", str_trim(tolower(inputString)))
  
  words = unlist(strsplit(inputString, " "))
  from = length(words)-num+1
  to = length(words)
  tempWords = words[from:to]
  paste(tempWords, collapse=" ")
}

predictionGiven3Words = function(inputString){
  # Takes a string as input
  #
  # Args:
  # inputString: string
  # Returns:
  # matrix with term, nextTerm, frequency, smoothedProbability, wholeTerm, ldaProbability, probability
  
  # Preprocessing
  inputString = gsub("[[:space:]]+", " ", str_trim(tolower(inputString)))
  words = unlist(strsplit(inputString, " "))
  
  # If number of words is less than 3 then calls predictionGiven2Words
  if (length(words) < 3){
    return(predictionGiven2Words(inputString))
  }
  # Call to get last 3 terms if number of words is >=3
  inputSplit.last3Words <- getLastTerms(inputString,3)
  # Matches the words with quad-grams
  quadGram.termsMatch = quad.combinedProb[term == inputSplit.last3Words]
  # If there is a match
  if (nrow(quadGram.termsMatch) > 0){
    # returns a matrix with term, nextTerm, frequency of occurrence, smoothedProbability, wholeTerm, ldaProbability and probability
    return(quadGram.termsMatch[order(quadGram.termsMatch$probability, decreasing = TRUE),])
  }
  # If there is no match
  else {
    return(predictionGiven2Words(inputString))
  }
}

predictionGiven2Words = function(inputString){
  # Takes a string as input
  #
  # Args:
  # inputString: string
  # Returns:
  # matrix with term, nextTerm, frequency, smoothedProbability, wholeTerm, ldaProbability, probability
  
  # Preprocessing
  inputString = gsub("[[:space:]]+", " ", str_trim(tolower(inputString)))
  words = unlist(strsplit(inputString, " "))
  
  # If number of words is less than 2 then calls predictionGiven1Word
  if (length(words) < 2){
    predictionGiven1Word(inputString)
    return
  }
  # Call to get last 2 terms if number of words is >=2
  inputSplit.last2Words <- getLastTerms(inputString,2)
  # Matches the words with tri-grams
  triGram.termsMatch = tri.combinedProb[term == inputSplit.last2Words]
  # If there is a match
  if (nrow(triGram.termsMatch) > 0){
    # returns a matrix with term, nextTerm, frequency of occurrence, smoothedProbability, wholeTerm, ldaProbability and probability
    return(triGram.termsMatch[order(triGram.termsMatch$probability, decreasing = TRUE),])
  }
  # If there is no match
  else {
    return(predictionGiven1Word(inputString))
  }
}


predictionGiven1Word = function(inputString){
  # Takes a string as input
  #
  # Args:
  # inputString: string
  # Returns:
  # matrix with term, nextTerm, frequency, smoothedProbability, wholeTerm, ldaProbability, probability
  
  # Preprocessing
  inputString = gsub("[[:space:]]+", " ", str_trim(tolower(inputString)))
  words = unlist(strsplit(inputString, " "))
  
  # If number of words is less than 1 then calls predictionGiven0Word
  if (length(words) < 1){
    return(predictionGiven0Word(inputString))
  }
  # Call to get last term if number of words is >=1
  inputSplit.last1Words <- getLastTerms(inputString,1)
  # Matches the words with bi-grams
  biGram.termsMatch = bi.combinedProb[term == inputSplit.last1Words]
  # If there is a match
  if (nrow(biGram.termsMatch) > 0){
    # returns a matrix with term, nextTerm, frequency of occurrence, smoothedProbability, wholeTerm, ldaProbability and probability
    return(biGram.termsMatch[order(biGram.termsMatch$probability, decreasing = TRUE),])
  }
  # If there is no match
  else {
    return(predictionGiven0Word(inputString))
  }
}

predictionGiven0Word = function(inputString){
  # Takes a string as input
  #
  # Args:
  # inputString: string
  # Returns:
  # uni-grams sorted according to probability
  
  orderedOutput <- uni.combinedProb[order(uni.combinedProb$probability, decreasing = TRUE),]
  if (nrow(orderedOutput) > 0){
    return(orderedOutput)
  } else {
    stop(sprintf("[%s] has no predictions that we could offer.", inputString))
  }
}
