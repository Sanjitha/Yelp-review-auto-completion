# Set working directory
#setwd(C:/Users/SANYA/Desktop/Auto-Complete Tool for Yelp Reviews/Code")

source('ExternalLibraries.R')
source('SmoothingBackOffUtils.R')
source('TopicModellingLDAUtils.R')


print(paste("Read the processed file."))
data <- read_delim("processed.csv",delim= "~", col_names=c("date", "stars", "user_id", "review_id", "business_id", "text"))

print(paste("Get the highest restuarant and user review."))
businessReviews <- data.frame(count(data,vars = c("business_id", "user_id")))

print(paste("Get the total number of reviews for the restuarant Ihop"))
businessUserData <- data[which( data$business_id=="T1jYZFB_7cqdhuvzpxfFWQ" | data$user_id=="q200TCFOheJfS_MvvwMfJw" ),]

# Divide data into training and test sets (9:1)
trainRows <- ceiling(0.9*nrow(businessUserData))
trainData <- businessUserData[1:trainRows,]
testData <- businessUserData[(trainRows+1):nrow(businessUserData),]

#Topic Modelling for Restuarant
stemmedTrainData <- stemming(trainData$text)


### Steps to generate term document matrix, tf-idf matrix, sort data according to frequency and create a data table with terms and frequency of occurrence for each n-gram
# Uni-Gram
businessData.uniGramDTM = generateTermDocumentMatrix(stemmedTrainData, 1)
businessData.uniGramDTM.matrix <- as.matrix(businessData.uniGramDTM)
businessData.uniGramDTMTfIdf <- calculateTfidf(businessData.uniGramDTM.matrix)
freq1 <- sort(rowSums(businessData.uniGramDTMTfIdf), decreasing=TRUE)
wf1 <- data.table(term=names(freq1), Freq=freq1)
# Generate transpose of TDM as LDA accepts only DTM
businessData.uniGramDTMTfIdf <- t(businessData.uniGramDTMTfIdf)
generateWordCloud(businessData.uniGramDTMTfIdf, 45)
generateDFPlot(businessData.uniGramDTMTfIdf, "Uni-Gram", "Uni-Gram vs Frequency (Top 10)")


# Bi-Gram
businessData.biGramDTM = generateTermDocumentMatrix(stemmedTrainData, 2)
businessData.biGramDTM.matrix <- as.matrix(businessData.biGramDTM)
businessData.biGramDTMTfIdf <- calculateTfidf(businessData.biGramDTM.matrix)
freq2 <- sort(rowSums(businessData.biGramDTMTfIdf), decreasing=TRUE)
biGramfirstTerms <- firstWords(names(freq2))
biGramlastTerms <- lastWord(names((freq2)))
wf2 <- data.table(word1=biGramfirstTerms, nextWord=biGramlastTerms, Freq=freq2)
# Generate transpose of TDM as LDA accepts only DTM
businessData.biGramDTMTfIdf <- t(businessData.biGramDTMTfIdf)
generateWordCloud(businessData.biGramDTMTfIdf, 9.5)
generateDFPlot(businessData.biGramDTMTfIdf, "Bi-Gram", "Bi-Gram vs Frequency (Top 10)")


# Tri-Gram
businessData.triGramDTM= generateTermDocumentMatrix(stemmedTrainData, 3)
businessData.triGramDTM.matrix <- as.matrix(businessData.triGramDTM)
businessData.triGramDTMTfIdf <- calculateTfidf(businessData.triGramDTM.matrix)
freq3 <- sort(rowSums(businessData.triGramDTMTfIdf), decreasing=TRUE)
triGramfirstTerms <- firstWords(names(freq3))
triGramlastTerms <- lastWord(names((freq3)))
triGramTemp <- data.table(words=triGramfirstTerms, w=triGramlastTerms, Freq=freq3)
triGramTemp[, c("w1", "w2") := tstrsplit(words, " ", fixed=TRUE)]
wf3 <- data.table(word1=triGramTemp$w1, word2=triGramTemp$w2, nextWord=triGramTemp$w, Freq=triGramTemp$Freq)
# Generate transpose of TDM as LDA accepts only DTM
businessData.triGramDTMTfIdf <- t(businessData.triGramDTMTfIdf)
generateWordCloud(businessData.triGramDTMTfIdf, 9)
generateDFPlot(businessData.triGramDTMTfIdf, "Tri-Gram", "Tri-Gram vs Frequency (Top 10)")


# Quad-Gram
businessData.quadGramDTM= generateTermDocumentMatrix(stemmedTrainData, 4)
businessData.quadGramDTM.matrix <- as.matrix(businessData.quadGramDTM)
businessData.quadGramDTMTfIdf <- calculateTfidf(businessData.quadGramDTM.matrix)
freq4 <- sort(rowSums(businessData.quadGramDTMTfIdf), decreasing=TRUE)
quadGramfirstTerms <- firstWords(names(freq4))
quadGramlastTerms <- lastWord(names((freq4)))
quadGramTemp <- data.table(words=quadGramfirstTerms, w=quadGramlastTerms, Freq=freq4)
quadGramTemp[, c("w1", "w2", "w3") := tstrsplit(words, " ", fixed=TRUE)]
wf4 <- data.table(word1=quadGramTemp$w1, word2=quadGramTemp$w2, word3=quadGramTemp$w3, nextWord=quadGramTemp$w, Freq=quadGramTemp$Freq)
# Generate transpose of TDM as LDA accepts only DTM
businessData.quadGramDTMTfIdf <- t(businessData.quadGramDTMTfIdf)
generateWordCloud(businessData.quadGramDTMTfIdf, 6)
generateDFPlot(businessData.quadGramDTMTfIdf, "Quad-Gram", "Quad-Gram vs Frequency (Top 10)")


#Barplot for NGram Count
nGram.count <- c(ncol(businessData.uniGramDTMTfIdf),ncol(businessData.biGramDTMTfIdf),ncol(businessData.triGramDTMTfIdf), ncol(businessData.quadGramDTMTfIdf))
nGram.name <- c('Uni-Gram', 'Bi-Gram', 'Tri-Gram', 'Quad-Gram')
countDF <- data.frame(nGram.name, nGram.count)
plot_ly(countDF, x=~nGram.name, y=~nGram.count, type="bar", name="Number of N-Grams vs N-Gram size", marker = list(color =c("Orange", "Lightblue", "Lightgreen", 'Gray') ), color=~nGram.name)%>%
  layout(yaxis = list(title = 'Count'), xaxis=list(title='N-Grams'), showlegend=TRUE)


### Smoothing
# Uni-Gram
sumFreq <- sum(wf1$Freq)
probab = c()
for(i in 1:nrow(wf1)){
  probab[i] = wf1$Freq[i]/sumFreq
}
uniGramSmoothedData <- data.table(wf1$term, wf1$Freq, probab)
names(uniGramSmoothedData) <- c("term", "frequency", "probability")
head(uniGramSmoothedData, 10)

# Bi-Gram
biGram <- KNSmoothing(wf2, 2)
biGramSmoothedData <- data.table(biGram$word1, biGram$nextWord, biGram$Freq, biGram$P_KN)
names(biGramSmoothedData) <- c("term", "nextTerm", "frequency", "probability")
head(biGramSmoothedData, 10)

# Tri-Gram
triGram <- KNSmoothing(wf3, 3)
triGramSmoothedData <- data.table(paste(triGram$word1, triGram$word2, sep=" "), triGram$nextWord, triGram$Freq, triGram$P_KN)
names(triGramSmoothedData) <- c("term", "nextTerm", "frequency", "probability")
head(triGramSmoothedData, 10)

# Quad-Gram
quadGram <- KNSmoothing(wf4, 4)
quadGramSmoothedData <- data.table(paste(quadGram$word1, quadGram$word2, quadGram$word3, sep=" "), quadGram$nextWord, quadGram$Freq, quadGram$P_KN)
names(quadGramSmoothedData) <- c("term", "nextTerm", "frequency", "probability")
head(quadGramSmoothedData, 10)


### LDA
buisness.uniGram.lda <- generateTopicModellingUsingLDA(businessData.uniGramDTMTfIdf,10)
buisness.biGram.lda <- generateTopicModellingUsingLDA(as.matrix(businessData.biGramDTMTfIdf),10)
buisness.triGram.lda <- generateTopicModellingUsingLDA(businessData.triGramDTMTfIdf,10)
buisness.quadGram.lda <- generateTopicModellingUsingLDA(businessData.quadGramDTMTfIdf,10)

# Output from LDA (Top 10 rows)
showTopicProbabilities(buisness.uniGram.lda@gamma)
showTopicProbabilities(buisness.biGram.lda@gamma)
showTopicProbabilities(buisness.triGram.lda@gamma)
showTopicProbabilities(buisness.quadGram.lda@gamma)

# Top 10 terms under each topic
generateTopNTermsInTopic(buisness.uniGram.lda, 10)
generateTopNTermsInTopic(buisness.biGram.lda, 10)
generateTopNTermsInTopic(buisness.triGram.lda, 10)
generateTopNTermsInTopic(buisness.quadGram.lda, 10)

# Mean topic probability across all documents
uni.topicMeanProbabDF <- calculateTopicProbabilityAcrossAllDocuments(buisness.uniGram.lda@gamma)
bi.topicMeanProbabDF <- calculateTopicProbabilityAcrossAllDocuments(buisness.biGram.lda@gamma)
tri.topicMeanProbabDF <- calculateTopicProbabilityAcrossAllDocuments(buisness.triGram.lda@gamma)
quad.topicMeanProbabDF <- calculateTopicProbabilityAcrossAllDocuments(buisness.quadGram.lda@gamma)

# Assign topic probability to LDA top N terms
uni.termLDAProbability <- assignTermLdaProbability(buisness.uniGram.lda,uni.topicMeanProbabDF)
bi.termLDAProbability <- assignTermLdaProbability(buisness.biGram.lda,bi.topicMeanProbabDF)
tri.termLDAProbability <- assignTermLdaProbability(buisness.triGram.lda,tri.topicMeanProbabDF)
quad.termLDAProbability <- assignTermLdaProbability(buisness.quadGram.lda,quad.topicMeanProbabDF)


# Mean probability calculation for each term
uni.combinedProb <- mergeProbability(uni.termLDAProbability,uniGramSmoothedData)
names(uni.combinedProb) <- c("nextTerm", "frequency", "smoothedProbability","ldaProbability","probability")
bi.combinedProb <- mergeProbability(bi.termLDAProbability,biGramSmoothedData)
names(bi.combinedProb) <- c("term","nextTerm","frequency", "smoothedProbability","wholeTerm","ldaProbability","probability")
tri.combinedProb <- mergeProbability(tri.termLDAProbability,triGramSmoothedData)
names(tri.combinedProb) <- c("term","nextTerm","frequency", "smoothedProbability","wholeTerm","ldaProbability","probability")
quad.combinedProb <- mergeProbability(quad.termLDAProbability,quadGramSmoothedData)
names(quad.combinedProb) <- c("term","nextTerm","frequency", "smoothedProbability","wholeTerm","ldaProbability","probability")



### Evaluation
# Predicted text from Auto-complete tool
predictedText = "My wife had chicken tenders that was extremely tasty.Tonight the restaurant was almost fully crowded."

doc = c(1:length(testData$text))
similarity_index = c()
for(i in 1 : length(testData$text)) {
  eachRow <- testData$text[i]
  tdm <- generateTermDocumentMatrix(c(predictedText,eachRow),1)
  tdm.matrix <- as.matrix(tdm)
  jaccard <- cluster_similarity(tdm.matrix[,1], tdm.matrix[,2], similarity = "jaccard",method = "independence")
  cat("Jaccard similarity between two documents ",jaccard,"\n")
  similarity_index[i] = jaccard
}
evaluationDF <- data.frame(doc, similarity_index)
names(evaluationDF) <- c("Document", "Value")
plot_ly(evaluationDF, x=~doc, y=~similarity_index, type="bar", name="Jaccard Similarity Index", color=~Document)%>%
  layout(yaxis = list(title = 'Similarity Index'), xaxis=list(title='Document'), showlegend=TRUE)



### Sample Output during prediction
predictionGiven2Words("my wife")
predictionGiven3Words("my wife ordered")
predictionGiven0Word("my wife ordered")




