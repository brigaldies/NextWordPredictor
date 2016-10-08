```{r}
source('config.R')
source('utils.R')
source('load_corpus.R')
source('predictor.R')

require(dplyr)

# -----------------------------------------------------------------------------
# Load the model under test
# -----------------------------------------------------------------------------
modelUnderTest = readRDS('model.rds')

# -----------------------------------------------------------------------------
# Load and clean the test corpus
# -----------------------------------------------------------------------------
partitionRate = 10
sampleRate = 5
partitionCorpusFile(directory = g_corpus_directory_en, file_name = paste0('en_US.blogs.txt.partition_', sprintf('%.1f', 100-partitionRate)), sample_rate = sampleRate/100)
partitionCorpusFile(directory = g_corpus_directory_en, file_name = paste0('en_US.news.txt.partition_', sprintf('%.1f', 100-partitionRate)), sample_rate = sampleRate/100)
partitionCorpusFile(directory = g_corpus_directory_en, file_name = paste0('en_US.twitter.txt.partition_', sprintf('%.1f', 100-partitionRate)), sample_rate = sampleRate/100)
corpusTestFilesPattern = paste0('^en_US(.)*partition_', sprintf('%.1f', 100-partitionRate), '.partition_', sprintf('%.1f', sampleRate), '$')
dirSamples = DirSource(g_corpus_directory_en, pattern = corpusTestFilesPattern)

cTest = loadCorpus(directory = g_corpus_directory_en, files_pattern = corpusTestFilesPattern)
cTest <- cleanCorpus(cTest)
cTestPathName = paste0(g_corpus_directory_en, '\\test_cleaned_corpus_partition_', sprintf('%.1f', 100-partitionRate), '_sample_', sprintf('%.1f', sampleRate), '.rds')
saveRDS(object = cTest, file = cTestPathName)

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
# ~ 55 % success
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
```
