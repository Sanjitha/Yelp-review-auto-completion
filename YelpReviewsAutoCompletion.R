setwd("C:/Users/Sanju/Google Drive/Semester3/FDS/Project/Final")

source('ExternalLibraries.R')
source('SmoothingBackOffUtils.R')
source('TopicModellingLDAUtils.R')


print(paste("Read the processed file."))
data <- read_delim("processed.csv",delim= "~", col_names=c("date", "stars", "user_id", "review_id", "business_id", "text"))

print(paste("Get the highest restuarant and user review."))
businessReviews <- data.frame(count(data,vars = c("business_id", "user_id")))

print(paste("Get the total number of reviews for the restuarant Ihop"))
businessUserData <- data[which( data$business_id=="T1jYZFB_7cqdhuvzpxfFWQ" | data$user_id=="q200TCFOheJfS_MvvwMfJw" ),]


trainRows <- ceiling(0.9*nrow(businessUserData))
trainData <- businessUserData[1:trainRows,]
testData <- businessUserData[(trainRows+1):nrow(businessUserData),]

#Topic Modelling for Restuarant
stemmedTrainData <- stemming(trainData$text)


print(paste("Uni-Gram"))
businessData.uniGramDTM = generateTermDocumentMatrix(stemmedTrainData, 1)
businessData.uniGramDTM.matrix <- as.matrix(businessData.uniGramDTM)
businessData.uniGramDTMTfIdf <- calculateTfidf(businessData.uniGramDTM.matrix)
freq1 <- sort(rowSums(businessData.uniGramDTMTfIdf), decreasing=TRUE)
wf1 <- data.table(term=names(freq1), Freq=freq1)


print(paste("Bi-Gram"))
businessData.biGramDTM = generateTermDocumentMatrix(stemmedTrainData, 2)
businessData.biGramDTM.matrix <- as.matrix(businessData.biGramDTM)
businessData.biGramDTMTfIdf <- calculateTfidf(businessData.biGramDTM.matrix)
freq2 <- sort(rowSums(businessData.biGramDTMTfIdf), decreasing=TRUE)
biGramfirstTerms <- firstWords(names(freq2))
biGramlastTerms <- lastWord(names((freq2)))
wf2 <- data.table(word1=biGramfirstTerms, nextWord=biGramlastTerms, Freq=freq2)


print(paste("Tri-Gram"))
businessData.triGramDTM= generateTermDocumentMatrix(stemmedTrainData, 3)
businessData.triGramDTM.matrix <- as.matrix(businessData.triGramDTM)
businessData.triGramDTMTfIdf <- calculateTfidf(businessData.triGramDTM.matrix)
freq3 <- sort(rowSums(businessData.triGramDTMTfIdf), decreasing=TRUE)
triGramfirstTerms <- firstWords(names(freq3))
triGramlastTerms <- lastWord(names((freq3)))
triGramTemp <- data.table(words=triGramfirstTerms, w=triGramlastTerms, Freq=freq3)
triGramTemp[, c("w1", "w2") := tstrsplit(words, " ", fixed=TRUE)]
wf3 <- data.table(word1=triGramTemp$w1, word2=triGramTemp$w2, nextWord=triGramTemp$w, Freq=triGramTemp$Freq)


print(paste("Quad-Gram"))
businessData.quadGramDTM= generateTermDocumentMatrix(stemmedTrainData, 4)
businessData.quadGramDTM.matrix <- as.matrix(businessData.quadGramDTM)
businessData.quadGramDTMTfIdf <- calculateTfidf(businessData.quadGramDTM.matrix)
freq4 <- sort(rowSums(businessData.quadGramDTMTfIdf), decreasing=TRUE)
quadGramfirstTerms <- firstWords(names(freq4))
quadGramlastTerms <- lastWord(names((freq4)))
quadGramTemp <- data.table(words=quadGramfirstTerms, w=quadGramlastTerms, Freq=freq4)
quadGramTemp[, c("w1", "w2", "w3") := tstrsplit(words, " ", fixed=TRUE)]
wf4 <- data.table(word1=quadGramTemp$w1, word2=quadGramTemp$w2, word3=quadGramTemp$w3, nextWord=quadGramTemp$w, Freq=quadGramTemp$Freq)





#Smoothing (Uni-Gram)
sumFreq <- sum(wf1$Freq)
probab = c()
for(i in 1:nrow(wf1)){
  probab[i] = wf1$Freq[i]/sumFreq
}
uniGramSmoothedData <- data.table(wf1$term, wf1$Freq, probab)
names(uniGramSmoothedData) <- c("term", "frequency", "probability")


#KN Smoothing (Bi-Gram)
biGram <- KNSmoothing(wf2, 2)
biGramSmoothedData <- data.table(biGram$word1, biGram$nextWord, biGram$Freq, biGram$P_KN)
names(biGramSmoothedData) <- c("term", "nextTerm", "frequency", "probability")


#KN Smoothing (Tri-Gram)
triGram <- KNSmoothing(wf3, 3)
triGramSmoothedData <- data.table(paste(triGram$word1, triGram$word2, sep=" "), triGram$nextWord, triGram$Freq, triGram$P_KN)
names(triGramSmoothedData) <- c("term", "nextTerm", "frequency", "probability")


#KN Smoothing (Quad-Gram)
quadGram <- KNSmoothing(wf4, 4)
quadGramSmoothedData <- data.table(paste(quadGram$word1, quadGram$word2, quadGram$word3, sep=" "), quadGram$nextWord, quadGram$Freq, quadGram$P_KN)
names(quadGramSmoothedData) <- c("term", "nextTerm", "frequency", "probability")




businessData.uniGramDTMTfIdf <- t(businessData.uniGramDTMTfIdf)
businessData.biGramDTMTfIdf <- t(businessData.biGramDTMTfIdf)
businessData.triGramDTMTfIdf <- t(businessData.triGramDTMTfIdf)
businessData.quadGramDTMTfIdf <- t(businessData.quadGramDTMTfIdf)

buisness.uniGram.lda <- generateTopicModellingUsingLDA(businessData.uniGramDTMTfIdf,10)
buisness.biGram.lda <- generateTopicModellingUsingLDA(as.matrix(businessData.biGramDTMTfIdf),10)
buisness.triGram.lda <- generateTopicModellingUsingLDA(businessData.triGramDTMTfIdf,10)
buisness.quadGram.lda <- generateTopicModellingUsingLDA(businessData.quadGramDTMTfIdf,10)

showTopicProbabilities(buisness.uniGram.lda@gamma)
showTopicProbabilities(buisness.biGram.lda@gamma)
showTopicProbabilities(buisness.triGram.lda@gamma)
showTopicProbabilities(buisness.quadGram.lda@gamma)

uni.topicMeanProbabDF <- calculateTopicProbabilityAcrossAllDocuments(buisness.uniGram.lda@gamma)
bi.topicMeanProbabDF <- calculateTopicProbabilityAcrossAllDocuments(buisness.biGram.lda@gamma)
tri.topicMeanProbabDF <- calculateTopicProbabilityAcrossAllDocuments(buisness.triGram.lda@gamma)
quad.topicMeanProbabDF <- calculateTopicProbabilityAcrossAllDocuments(buisness.quadGram.lda@gamma)

uni.termLDAProbability <- assignTermLdaProbability(buisness.uniGram.lda,uni.topicMeanProbabDF)
bi.termLDAProbability <- assignTermLdaProbability(buisness.biGram.lda,bi.topicMeanProbabDF)
tri.termLDAProbability <- assignTermLdaProbability(buisness.triGram.lda,tri.topicMeanProbabDF)
quad.termLDAProbability <- assignTermLdaProbability(buisness.quadGram.lda,quad.topicMeanProbabDF)

uni.combinedProb <- mergeProbability(uni.termLDAProbability,uniGramSmoothedData)
names(uni.combinedProb) <- c("nextTerm", "frequency", "smoothedProbability","ldaProbability","probability")

bi.combinedProb <- mergeProbability(bi.termLDAProbability,biGramSmoothedData)
names(bi.combinedProb) <- c("term","nextTerm","frequency", "smoothedProbability","wholeTerm","ldaProbability","probability")

tri.combinedProb <- mergeProbability(tri.termLDAProbability,triGramSmoothedData)
names(tri.combinedProb) <- c("term","nextTerm","frequency", "smoothedProbability","wholeTerm","ldaProbability","probability")

quad.combinedProb <- mergeProbability(quad.termLDAProbability,quadGramSmoothedData)
names(quad.combinedProb) <- c("term","nextTerm","frequency", "smoothedProbability","wholeTerm","ldaProbability","probability")



predictionGiven2Words("my wife")
predictionGiven3Words("my wife ordered")
predictionGiven0Word("my wife ordered")

#Evaluation

#Cosine 
predictedText = "My wife had chicken tenders that was extremely tasty.Tonight the restaurant was almost fully crowded."
for(i in 1 : length(testData$text)) {
  eachRow <- testData$text[i]
  tdm <- generateTermDocumentMatrix(c(predictedText,eachRow),1)
  tdm.matrix <- as.matrix(tdm)
  cosine <- cosine(tdm.matrix[,1],tdm.matrix[,2])
  cat("Cosine similarity between two documents ",cosine,"\n")
}

#Jaccard
predictedText = "My wife had chicken tenders that was extremely tasty.Tonight the restaurant was almost fully crowded."
for(i in 1 : length(testData$text)) {
  eachRow <- testData$text[i]
  tdm <- generateTermDocumentMatrix(c(predictedText,eachRow),1)
  tdm.matrix <- as.matrix(tdm)
  jaccard <- cluster_similarity(tdm.matrix[,1], tdm.matrix[,2], similarity = "jaccard",method = "independence")
  cat("Jaccard similarity between two documents ",jaccard,"\n")
}







