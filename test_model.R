# -----------------------------------------------------------------------------
# Load and clean the test corpus
# -----------------------------------------------------------------------------
partitionRate = 10
sampleRate = 1
# Check the file pattern
corpusTestFilesPattern = paste0('^en_US(.)*partition_', sprintf('%.1f', 100-partitionRate), '.partition_', sprintf('%.1f', sampleRate), '$')
dirSamples = DirSource(g_corpus_directory_en, pattern = corpusTestFilesPattern)
print(dirSamples$filelist)

cTest = loadCorpus(directory = g_corpus_directory_en, files_pattern = corpusTestFilesPattern)
cTest <- cleanCorpus(cTest)

# Create the test quadgrams
ngramName = 'quadgram'
message(paste('Get a', ngramName, 'tokenizer...'))
tokenizer4 = function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
message(paste('Compute the DTM...'))
testQuadgramsDtmPathName = paste0(g_corpus_directory_en, '/test_quadgrams_dtm_partition_', sprintf('%.1f', 100-partitionRate), '.partition_', sprintf('%.1f', sampleRate), '.rds')
execTime = system.time({
    testQuadgramsDtm = DocumentTermMatrix(cTest, control = list(tokenize = tokenizer4))
    saveRDS(object = testQuadgramsDtm, file = testQuadgramsDtmPathName)
})
message(paste(ngramName, 'DTM built in', round(execTime["elapsed"], 2), "secs"))
message(paste(ngramName, 'DTM size:', round(object.size(testQuadgramsDtm)/1024/2014, 2), 'MB'))

# Read the bigrams DTM if necessary
testQuadgramsDtm = readRDS(file = testQuadgramsDtmPathName)

# Get the list of quadgrams
testQuadgramsCount = colSums(as.matrix(testQuadgramsDtm))
# testQuadgramsCount = testQuadgramsCount[testQuadgramsCount >= 2] # Remove single occurrences
testQuadgramsGram = names(testQuadgramsCount)
testQuadgramsLowerGram = sapply(testQuadgramsGram, function(gram) { getFirstNTokensFast(gram, 3)})
testQuadgramsWordToPredict = sapply(testQuadgramsGram, function(gram) { getLastToken(gram) })
testQuadgramsDT = data.table(gram = testQuadgramsGram, 
                             lowergram = testQuadgramsLowerGram, 
                             word_to_predict = testQuadgramsWordToPredict)
testQuadgramsDTPathName = paste0(g_corpus_directory_en, '/test_quadgrams_DT_partition_', sprintf('%.1f', 100-partitionRate), '.partition_', sprintf('%.1f', sampleRate), '.rds')
saveRDS(object = testQuadgramsDT, file = testQuadgramsDTPathName)

# Read the test data table
testQuadgramsDT = readRDS(file = testQuadgramsDTPathName)

# Quick test the model
predictions = predictNextNBestWords(model = modelUnderTest, type = 'MLE', sentence = 'Hello how are you doing today', matches_count_max = 5)
predictions = predictNextNBestWords(model = modelUnderTest, type = 'MLE', sentence = 'djsh dsjhd sjhd  sdh sdj sjdh jsdh sdhjshdsjdh', matches_count_max = 5)

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

