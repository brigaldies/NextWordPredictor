source('config.R')
source('utils.R')
source('load_corpus.R')
source('predictor.R')

require(dplyr)

# -----------------------------------------------------------------------------
# Load the model under test
# -----------------------------------------------------------------------------
modelUnderTest = readRDS('model.rds')
# Poke at the data
head(modelUnderTest$unigrams[order(logprob, decreasing = TRUE)])

# -----------------------------------------------------------------------------
# Load and clean the test corpus
# -----------------------------------------------------------------------------
partitionRate = 10
sampleRate = 5
partitionCorpusFile(directory = g_corpus_directory_en, file_name = paste0('en_US.blogs.txt.partition_', sprintf('%.1f', 100-partitionRate)), sample_rate = sampleRate/100)
partitionCorpusFile(directory = g_corpus_directory_en, file_name = paste0('en_US.news.txt.partition_', sprintf('%.1f', 100-partitionRate)), sample_rate = sampleRate/100)
partitionCorpusFile(directory = g_corpus_directory_en, file_name = paste0('en_US.twitter.txt.partition_', sprintf('%.1f', 100-partitionRate)), sample_rate = sampleRate/100)
# Check the file pattern
corpusTestFilesPattern = paste0('^en_US(.)*partition_', sprintf('%.1f', 100-partitionRate), '.partition_', sprintf('%.1f', sampleRate), '$')
dirSamples = DirSource(g_corpus_directory_en, pattern = corpusTestFilesPattern)
print(dirSamples$filelist)

cTest = loadCorpus(directory = g_corpus_directory_en, files_pattern = corpusTestFilesPattern)
cTest <- cleanCorpus(cTest)
cTestPathName = paste0(g_corpus_directory_en, '\\test_cleaned_corpus_partition_', sprintf('%.1f', 100-partitionRate), '_sample_', sprintf('%.1f', sampleRate), '.rds')
saveRDS(object = cTest, file = cTestPathName)
# cTest = readRDS(file = cTestPathName)

# -----------------------------------------------------------------------------
# Create the test bigrams
# ~ 21% success
# -----------------------------------------------------------------------------
tokenizer2 = function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
testBigramsDtmPathName = paste0(g_corpus_directory_en, '\\test_bigrams_dtm_partition_', sprintf('%.1f', 100-partitionRate), '_sample_', sprintf('%.1f', sampleRate), '.rds')
execTime = system.time({
    testBigramsDtm = DocumentTermMatrix(cTest, control = list(tokenize = tokenizer2))
    saveRDS(object = testBigramsDtm, file = testBigramsDtmPathName)
})
message(paste('Test bigrams DTM built in', round(execTime["elapsed"], 2), "secs"))
testBigramsCount = colSums(as.matrix(testBigramsDtm))
testBigramsCount = testBigramsCount[testBigramsCount >= 2] # Remove single occurrences
testBigramsGram = names(testBigramsCount)
testBigramsLowerGram = sapply(testBigramsGram, function(gram) { getFirstNTokensFast(gram, 1)})
testBigramsWordToPredict = sapply(testBigramsGram, function(gram) { getLastToken(gram) })
testBigramsDT = data.table(gram = testBigramsGram, 
                           lowergram = testBigramsLowerGram,
                           word_to_predict = testBigramsWordToPredict)
testBigramsDTPathName = paste0(g_corpus_directory_en, '/test_bigrams_DT_partition_', sprintf('%.1f', 100-partitionRate), '_sample_', sprintf('%.1f', sampleRate), '.rds')
saveRDS(object = testBigramsDT, file = testBigramsDTPathName)
# testBigramsDT = readRDS(file = testBigramsDTPathName)
dim(testBigramsDT)

# Quick test the model
set.seed(10)
testSample = sample_n(testBigramsDT, 1)
predictWithStupidBackoff2(model = modelUnderTest, sentence = testSample$lowergram, matches_count_max = 10)

# Test the model's performance against the test bigrams
execTime <- system.time({
    set.seed(10)
    testSize = 100000
    remove(testData)
    testData = sample_n(testBigramsDT, testSize)
    testCount = dim(testData)[1]
    testResults = mapply(function(lowergram, word_to_predict) {
        word_to_predict %in% predictWithStupidBackoff2(model = modelUnderTest,
                                                       sentence = lowergram)$`Predicted Word`
    },
    testData$lowergram,
    testData$word_to_predict)
    predictionmatchCount = sum(testResults)
    message(paste('Prediction match count   :', predictionmatchCount, 'out of', testCount, '(', round(predictionmatchCount*100/testCount, 2), '%)'))
})
execTimeSeconds = execTime["elapsed"]
execTimeMins = execTimeSeconds/60
execTimeSecsAfterMins = execTimeSeconds %% 60
message(paste('Test executed in', round(execTimeMins), 'mins, ', round(execTimeSecsAfterMins, 2), 'secs'))

# -----------------------------------------------------------------------------
# Create the test trigrams
# ~ 52% success
# -----------------------------------------------------------------------------
tokenizer3 = function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
testTrigramsDtmPathName = paste0(g_corpus_directory_en, '\\test_trigrams_dtm_partition_', sprintf('%.1f', 100-partitionRate), '_sample_', sprintf('%.1f', sampleRate), '.rds')
execTime = system.time({
    testTrigramsDtm = DocumentTermMatrix(cTest, control = list(tokenize = tokenizer3))
    saveRDS(object = testTrigramsDtm, file = testTrigramsDtmPathName)
})
message(paste('Test trigrams DTM built in', round(execTime["elapsed"], 2), "secs"))
testTrigramsCount = colSums(as.matrix(testTrigramsDtm))
testTrigramsCount = testTrigramsCount[testTrigramsCount >= 2] # Remove single occurrences
testTrigramsGram = names(testTrigramsCount)
testTrigramsLowerGram = sapply(testTrigramsGram, function(gram) { getFirstNTokensFast(gram, 2)})
testTrigramsWordToPredict = sapply(testTrigramsGram, function(gram) { getLastToken(gram) })
testTrigramsDT = data.table(gram = testTrigramsGram, 
                           lowergram = testTrigramsLowerGram,
                           word_to_predict = testTrigramsWordToPredict)
testTrigramsDTPathName = paste0(g_corpus_directory_en, '/test_trigrams_DT_partition_', sprintf('%.1f', 100-partitionRate), '_sample_', sprintf('%.1f', sampleRate), '.rds')
saveRDS(object = testTrigramsDT, file = testTrigramsDTPathName)
# testTrigramsDT = readRDS(file = testTrigramsDTPathName)
dim(testTrigramsDT)

# Quick test the model
set.seed(10)
testSample = sample_n(testTrigramsDT, 1)
predictWithStupidBackoff2(model = modelUnderTest, sentence = testSample$lowergram, matches_count_max = 10)

# Test the model's performance against the test trigrams
execTime <- system.time({
    set.seed(10)
    testSize = 100000
    remove(testData)
    testData = sample_n(testTrigramsDT, testSize)
    testCount = dim(testData)[1]
    testResults = mapply(function(lowergram, word_to_predict) {
        word_to_predict %in% predictWithStupidBackoff2(model = modelUnderTest,
                                                       sentence = lowergram)$`Predicted Word`
    },
    testData$lowergram,
    testData$word_to_predict)
    predictionmatchCount = sum(testResults)
    message(paste('Prediction match count   :', predictionmatchCount, 'out of', testCount, '(', round(predictionmatchCount*100/testCount, 2), '%)'))
})
execTimeSeconds = execTime["elapsed"]
execTimeMins = execTimeSeconds/60
execTimeSecsAfterMins = execTimeSeconds %% 60
message(paste('Test executed in', round(execTimeMins), 'mins, ', round(execTimeSecsAfterMins, 2), 'secs'))

# -----------------------------------------------------------------------------
# create the test quadgrams
# ~ 74% success
# -----------------------------------------------------------------------------
tokenizer4 = function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
testQuadgramsDtmPathName = paste0(g_corpus_directory_en, '/test_quadgrams_dtm_partition_', sprintf('%.1f', 100-partitionRate), '_sample_', sprintf('%.1f', sampleRate), '.rds')
execTime = system.time({
    testQuadgramsDtm = DocumentTermMatrix(cTest, control = list(tokenize = tokenizer4))
    saveRDS(object = testQuadgramsDtm, file = testQuadgramsDtmPathName)
})
message(paste('DTM built in', round(execTime["elapsed"], 2), "secs"))

# Read the quadgrams DTM if necessary
testQuadgramsDtm = readRDS(file = testQuadgramsDtmPathName)

# Get the list of quadgrams
testQuadgramsCount = colSums(as.matrix(testQuadgramsDtm))
testQuadgramsCount = testQuadgramsCount[testQuadgramsCount >= 2] # Remove single occurrences
testQuadgramsGram = names(testQuadgramsCount)
testQuadgramsLowerGram = sapply(testQuadgramsGram, function(gram) { getFirstNTokensFast(gram, 3)})
testQuadgramsWordToPredict = sapply(testQuadgramsGram, function(gram) { getLastToken(gram) })
testQuadgramsDT = data.table(gram = testQuadgramsGram, 
                             lowergram = testQuadgramsLowerGram, 
                             word_to_predict = testQuadgramsWordToPredict)
testQuadgramsDTPathName = paste0(g_corpus_directory_en, '/test_quadgrams_DT_partition_', sprintf('%.1f', 100-partitionRate), '_sample_', sprintf('%.1f', sampleRate), '.rds')
saveRDS(object = testQuadgramsDT, file = testQuadgramsDTPathName)
# testQuadgramsDT = readRDS(file = testQuadgramsDTPathName)
dim(testQuadgramsDT)

# Quick test the model
set.seed(10)
testSample = sample_n(testQuadgramsDT, 1)
predictWithStupidBackoff2(model = modelUnderTest, sentence = testSample$lowergram, matches_count_max = 10)

# Test the model's performance against the test quadgrams
execTime <- system.time({
    set.seed(10)
    testSize = 100000
    remove(testData)
    testData = sample_n(testQuadgramsDT, testSize)
    testCount = dim(testData)[1]
    testResults = mapply(function(lowergram, word_to_predict) {
        word_to_predict %in% predictWithStupidBackoff2(model = modelUnderTest,
                                                       sentence = lowergram)$`Predicted Word`
    },
    testData$lowergram,
    testData$word_to_predict)
    predictionmatchCount = sum(testResults)
    message(paste('Prediction match count   :', predictionmatchCount, 'out of', testCount, '(', round(predictionmatchCount*100/testCount, 2), '%)'))
})
execTimeSeconds = execTime["elapsed"]
execTimeMins = execTimeSeconds/60
execTimeSecsAfterMins = execTimeSeconds %% 60
message(paste('Test executed in', round(execTimeMins), 'mins, ', round(execTimeSecsAfterMins, 2), 'secs'))

# -----------------------------------------------------------------------------
# create the test pentagrams
# ~ 78 % success
# -----------------------------------------------------------------------------
tokenizer5 = function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5))
testPentagramsDtmPathName = paste0(g_corpus_directory_en, '/test_pentagrams_dtm_partition_', sprintf('%.1f', 100-partitionRate), '_sample_', sprintf('%.1f', sampleRate), '.rds')
execTime = system.time({
    testPentagramsDtm = DocumentTermMatrix(cTest, control = list(tokenize = tokenizer5))
    saveRDS(object = testPentagramsDtm, file = testPentagramsDtmPathName)
})
message(paste('DTM built in', round(execTime["elapsed"], 2), "secs"))
# testPentagramsDtm = readRDS(file = testPentagramsDtmPathName)
testPentagramsCount = colSums(as.matrix(testPentagramsDtm))
testPentagramsCount = testPentagramsCount[testPentagramsCount >= 2] # Remove single occurrences
testPentagramsGram = names(testPentagramsCount)
testPentagramsLowerGram = sapply(testPentagramsGram, function(gram) { getFirstNTokensFast(gram, 4)})
testPentagramsWordToPredict = sapply(testPentagramsGram, function(gram) { getLastToken(gram) })
testPentagramsDT = data.table(gram = testPentagramsGram, 
                             lowergram = testPentagramsLowerGram, 
                             word_to_predict = testPentagramsWordToPredict)
testPentagramsDTPathName = paste0(g_corpus_directory_en, '/test_pentagrams_DT_partition_', sprintf('%.1f', 100-partitionRate), '_sample_', sprintf('%.1f', sampleRate), '.rds')
saveRDS(object = testPentagramsDT, file = testPentagramsDTPathName)
# testPentagramsDT = readRDS(file = testPentagramsDTPathName)
dim(testPentagramsDT)

# Quick test the model
set.seed(10)
testSample = sample_n(testPentagramsDT, 1)
predictWithStupidBackoff2(model = modelUnderTest, sentence = testSample$lowergram, matches_count_max = 10)

# Test the model's performance against the test quadgrams
execTime <- system.time({
    set.seed(10)
    remove(testData)
    testData = testPentagramsDT
    testCount = dim(testData)[1]
    testResults = mapply(function(lowergram, word_to_predict) {
        word_to_predict %in% predictWithStupidBackoff2(model = modelUnderTest,
                                                       sentence = lowergram)$`Predicted Word`
    },
    testData$lowergram,
    testData$word_to_predict)
    predictionmatchCount = sum(testResults)
    message(paste('Prediction match count   :', predictionmatchCount, 'out of', testCount, '(', round(predictionmatchCount*100/testCount, 2), '%)'))
})
execTimeSeconds = execTime["elapsed"]
execTimeMins = execTimeSeconds/60
execTimeSecsAfterMins = execTimeSeconds %% 60
message(paste('Test executed in', round(execTimeMins), 'mins, ', round(execTimeSecsAfterMins, 2), 'secs'))


# -----------------------------------------------------------------------------
# Mixed N-Grams Test
# -----------------------------------------------------------------------------
execTime <- system.time({
    set.seed(10)
    remove(testData)
    testData = rbind(
        sample_n(testBigramsDT, 25000),
        sample_n(testTrigramsDT, 25000),
        sample_n(testQuadgramsDT, 25000),
        sample_n(testPentagramsDT, 25000)
    )
    testCount = dim(testData)[1]
    testResults = mapply(function(lowergram, word_to_predict) {
        word_to_predict %in% predictWithStupidBackoff2(model = modelUnderTest,
                                                       sentence = lowergram)$`Predicted Word`
    },
    testData$lowergram,
    testData$word_to_predict)
    predictionmatchCount = sum(testResults)
    message(paste('Prediction match count   :', predictionmatchCount, 'out of', testCount, '(', round(predictionmatchCount*100/testCount, 2), '%)'))
})
execTimeSeconds = execTime["elapsed"]
execTimeMins = execTimeSeconds/60
execTimeSecsAfterMins = execTimeSeconds %% 60
message(paste('Test executed in', round(execTimeMins), 'mins, ', round(execTimeSecsAfterMins, 2), 'secs'))


# ------------------------------------ Old experimentations -------------------
# Test!
# Training data: 10% partition
# Probability mode: MLE
# Test data: 1% of the 90% partition (The non-training data set)
# Matches with model MLE v1                                               : 28623 out of 85259 (33.57%) Date ???
# Matches with model MLE v2 (Cleaner data)                                : 32668 out of 85259 (38.32%) 9/17/2016
# Matches with model MLE v3 (Including count == 1 for bigram and trigrams): 32668 out of 85259 (61.30%) 9/17/2016
# Matches with model AKS k = 0.5                                          : 32668 out of 85259 (61.30%) 9/17/2016
# Matches with model AKS k = 0.05                                         : Same as with k = 0.5. Suspicious?
# Matches with model AKS k = 0.01                                         : Same as with k = 0.5. Suspicious?
# Matches with model RFC                                                  : Same as MLE or AKS!
#
# BIG MISTAKE WITH THE ABOVE DATA: I 1%-sampled the 10% training data to extract a test data! Duh!
#
# With a 5%-sample of the 80% partition: 25% success.

modelType = 'MLE'
execTime <- system.time({
    noPredictionCount = 0
    predictionNoMatchCount = 0
    predictionmatchCount = 0
    testCount = dim(testQuadgramsDT)[1]
    for (i in 1:testCount) {
        # message(paste('----- Test', i, '-----'))
        gramUnderTest = testQuadgramsDT[i, .(gram, lowergram)]
        # message(paste0('Gram under test: "', as.character(gramUnderTest[1, gram]), '"'))
        sentenceUnderTest = as.character(gramUnderTest[1,lowergram])
        wordToPredict = getLastNTokens(as.character(gramUnderTest[1,gram]), 1)
        # message(paste0('Predicting "', sentenceUnderTest, '"'))
        predictedWords = predictNextNBestWords(model = modelUnderTest, 
                                               type = modelType,
                                               sentence = sentenceUnderTest, 
                                               matches_count_max = 5)
        # print(predictedWords)
        if (length(predictedWords) == 0) {
            # message('No prediction!')
            noPredictionCount = noPredictionCount + 1
        } else if (wordToPredict %in% predictedWords) {
            # message(paste0('*** Prediction match!'))    
            predictionmatchCount = predictionmatchCount + 1
        } else {
            # message(paste0('No prediction matched!'))   
            predictionNoMatchCount = predictionNoMatchCount + 1
        }
    }
    message(paste('Test results with model type', modelType))
    message(paste('No prediction count      :', noPredictionCount))
    message(paste('Prediction no match count:', predictionNoMatchCount))
    message(paste('Prediction match count   :', predictionmatchCount, 'out of', testCount, '(', round(predictionmatchCount*100/testCount, 2), '%)'))
})
execTimeSeconds = execTime["elapsed"]
execTimeMins = execTimeSeconds/60
execTimeSecsAfterMins = execTimeSeconds %% 60
message(paste('Test executed in', round(execTimeMins), 'mins, ', round(execTimeSecsAfterMins, 2), 'secs'))

cluster8 = makeCluster(8, 'SOCK')
clusterExport(cluster8, list("getLastNTokens"))
clusterExport(cluster8, list("getLastToken"))
clusterExport(cluster8, list("predictNextNBestWords"))
clusterEvalQ(cluster8, library(tm))
clusterEvalQ(cluster8, library(data.table))

testData = testQuadgramsDT
testData = testQuadgramsDT[1:10000]
dim(testData)
execTime <- system.time({
    testCount = dim(testData)[1]
    testResults = mapply(function(lowergram, word_to_predict) {
        # word_to_predict %in% predictNextNBestWords(model = modelUnderTest,
        #                                            type = modelType,
        #                                            sentence = lowergram,
        #                                            clean_sentence = FALSE,
        #                                            matches_count_max = 5)
        word_to_predict %in% predictWithStupidBackoff(model = modelUnderTest,
                                                      sentence = lowergram,
                                                      matches_count_max = 5)$predicted_word
    },
    testData$lowergram,
    testData$word_to_predict)
    message(paste('Test results with model type', modelType))
    predictionmatchCount = sum(testResults)
    message(paste('Prediction match count   :', predictionmatchCount, 'out of', testCount, '(', round(predictionmatchCount*100/testCount, 2), '%)'))
})
execTimeSeconds = execTime["elapsed"]
execTimeMins = execTimeSeconds/60
execTimeSecsAfterMins = execTimeSeconds %% 60
message(paste('Test executed in', round(execTimeMins), 'mins, ', round(execTimeSecsAfterMins, 2), 'secs'))

