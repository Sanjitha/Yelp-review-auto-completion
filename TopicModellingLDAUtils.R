print(paste("Function generateWordCloud to generate the word cloud for the ngrams produced"))
generateWordCloud <- function(nGramReviewMatrix) {
  freq <- sort(colSums(nGramReviewMatrix), decreasing=TRUE) 
  
  set.seed(123)   
  wordcloud(names(freq),freq,min.freq=2,colors=brewer.pal(6,"Dark2"))
}

generateTopicModellingUsingLDA <- function (nGramMatrix,k) {
  #Set parameters for Gibbs sampling
  burnin <- 4000
  iter <- 2000
  thin <- 500
  seed <-list(2003,5,63,100001,765)
  nstart <- 5
  best <- TRUE
  
  k <- k
  
  print(paste("Run LDA using Gibbs sampling"))
  ldaOut <-LDA(nGramMatrix,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
  
  print(paste("Topics assigned to each word"))
  ldaOut.topics <- as.matrix(topics(ldaOut))
  
  return(ldaOut)
}

generateTopNTermsInTopic <- function(ldaModel,n) {
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

trim <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

showTopicProbabilities <- function(ldaModel) {
  #probabilities associated with each topic assignment for a document
  topicProbabilities <- as.data.frame(ldaModel)
  names(topicProbabilities) <- c(1:10)
  topicProbabilities
}

calculateTopicProbabilityAcrossAllDocuments <- function(ldaModel) {
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
  return(meanOrderedProbabDF)
}


assignTermLdaProbability <- function(ldaModel,meanOrderedProbabDF) {
  
  topTermsPerTopic <- as.data.frame(terms(ldaModel,40))
  names(topTermsPerTopic) <- c(1:10)
  
  #Order the terms according to topic probability
  oldDT <- data.table()
  for(i in 1:nrow(meanOrderedProbabDF)) {
    row <- meanOrderedProbabDF[i,]
    topicTerms <- as.vector(topTermsPerTopic[[row$numb]])
    for(term in topicTerms) {
      newDT <- data.table(terms=term,ldaProbability=(row$probab))
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


mergeProbability <- function(ldaDT,smoothedDT) {
  
  if("nextTerm" %in% colnames(smoothedDT))
  {
    smoothedDT$wholeTerm = paste(smoothedDT$term, smoothedDT$nextTerm, sep=" ")
  }  
  # merge the two data frames based on terms in lda and whole terms in back-off
  setkey(as.data.table(ldaDT),terms)
  setkey(smoothedDT,wholeTerm)
  
  # perform the join, eliminating not matched rows from Right
  combinedDT <- smoothedDT[ldaDT, nomatch=0]
  
  combinedDT$mean=rowMeans(combinedDT[,c("probability", "ldaProbability")], na.rm=TRUE)
  return(combinedDT)
}








