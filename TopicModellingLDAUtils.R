generateWordCloud <- function(nGramReviewMatrix, freqValue) {
  #Takes a Term Document Matrix, minimum frequency value for wordcloud
  #
  #Args: 
  # nGramReviewMatrix: n-gram term document matrix
  # freqValue: minimum frequency value
  #Returns:
  # wordcloud for n-grams
  
  freq <- sort(colSums(nGramReviewMatrix), decreasing=TRUE) 
  set.seed(123)   
  # Minimum frequency is set to 2
  wordcloud(names(freq),freq,min.freq=freqValue,colors=brewer.pal(6,"Dark2"))
}


generateTopicModellingUsingLDA <- function (nGramMatrix,k) {
  #Takes a Term Document Matrix and number of topics
  #
  #Args: 
  # nGramMatrix: n-gram term document matrix
  # k: number of topics
  #Returns:
  # LDA topics and terms
  
  #Set parameters for Gibbs sampling
  burnin <- 4000
  iter <- 2000
  thin <- 500
  seed <-list(2003,5,63,100001,765)
  nstart <- 5
  best <- TRUE
  k <- k
  #Run LDA using Gibbs sampling
  ldaOut <-LDA(nGramMatrix,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
  #Topics assigned to each word
  ldaOut.topics <- as.matrix(topics(ldaOut))
  
  return(ldaOut)
}


generateTopNTermsInTopic <- function(ldaModel,n) {
  #Takes an LDA model and parameter 'n'
  #
  #Args: 
  # ldaModel: LDA output
  # n: number of terms
  #Returns:
  # matrix with top N terms in each topic
  
  as.matrix(terms(ldaModel,n))
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


# Function to calculate probability of each topic in each document
showTopicProbabilities <- function(ldaModel) { 
  #Takes an LDA model as input
  #
  #Args: 
  # ldaModel: LDA output
  #Returns:
  # probability for each topic vs document
  
  #probabilities associated with each topic assignment for a document
  topicProbabilities <- as.data.frame(ldaModel)
  names(topicProbabilities) <- c(1:10)
  head(topicProbabilities, 10)
}


# Function to calculate probability of each topic across all documents
calculateTopicProbabilityAcrossAllDocuments <- function(ldaModel) {
  #Takes an LDA model as input
  #
  #Args: 
  # ldaModel: LDA output
  #Returns:
  # probability for each topic across all documents
  
  #Computing the mean ocuurence of each topic across documents and sorting it in decreasing order
  topicProbabilities <- as.data.frame(ldaModel)
  names(topicProbabilities) <- c(1:10)
  
  probab = c()
  numb = c()
  for(i in 1:10){
    numb[i] = i
    probab[i] = mean(topicProbabilities[,i])
  }
  meanProbabDF <- data.frame(numb, probab)
  meanOrderedProbabDF <- meanProbabDF[order(meanProbabDF$probab, decreasing = TRUE),]
  names(meanOrderedProbabDF) <- c("Topic", "Probability")
  barplot(meanOrderedProbabDF$Probability, names=meanOrderedProbabDF$Topic, xlab="Topic", ylab="Probability", main="Topic vs Probability", col=c("lightblue"))
  return(meanOrderedProbabDF)
}


# Assign each term the probability of its LDA topic
assignTermLdaProbability <- function(ldaModel,meanOrderedProbabDF) {
  #Takes an LDA model and probability of topics across all documents (as data frame)
  #
  #Args: 
  # ldaModel: LDA output
  # meanOrderedProbabDF: probability of each topic across all documents
  #Returns:
  # each term with LDA probability
  
  topTermsPerTopic <- as.data.frame(terms(ldaModel,40))
  names(topTermsPerTopic) <- c(1:10)
  
  #Order the terms according to topic probability
  oldDT <- data.table()
  for(i in 1:nrow(meanOrderedProbabDF)) {
    row <- meanOrderedProbabDF[i,]
    topicTerms <- as.vector(topTermsPerTopic[[row$Topic]])
    for(term in topicTerms) {
      newDT <- data.table(terms=term,ldaProbability=(row$Probability))
      if(nrow(oldDT) == 0) {
        oldDT <- rbind.fill(oldDT,newDT)
      } else {
        duplicateTerms <- subset(oldDT,terms == term)
        if(nrow(duplicateTerms) == 0) {
          oldDT <- rbind.fill(oldDT,newDT)
        }
      }
    }
  }
  return(oldDT)
}


# Function to calculate mean probability of LDA and Back-off Model
mergeProbability <- function(ldaDT,smoothedDT) {
  #Takes LDA and KN smoothing output as inputs
  #
  #Args: 
  # ldaDT: LDA output 
  # smoothedDT: KN smoothing output
  #Returns:
  # mean of LDA and Back-off probability for each term
  
  if("nextTerm" %in% colnames(smoothedDT))
  {
    smoothedDT$wholeTerm = paste(smoothedDT$term, smoothedDT$nextTerm, sep=" ")
    
    # merge the two data frames based on terms in lda and whole terms in back-off
    setkey(as.data.table(ldaDT),terms)
    setkey(smoothedDT,wholeTerm)
    
    # perform the join, eliminating not matched rows from Right
    combinedDT <- smoothedDT[ldaDT, nomatch=0]
  }  else {
    # merge the two data frames based on terms in lda and whole terms in back-off
    setkey(as.data.table(ldaDT),terms)
    setkey(smoothedDT,term)
    
    # perform the join, eliminating not matched rows from Right
    combinedDT <- smoothedDT[ldaDT, nomatch=0]
  }
  
  combinedDT$mean=rowMeans(combinedDT[,c("probability", "ldaProbability")], na.rm=TRUE)
  return(combinedDT)
}


