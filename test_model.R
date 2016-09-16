# -----------------------------------------------------------------------------
# Load and clean the test corpus
# -----------------------------------------------------------------------------
partitionRate = 10
sampleRate = 1
# Check the file pattern
corpusTestFilesPattern = paste0('^en_US(.)*partition_', sprintf('%.1f', partitionRate), '.partition_', sprintf('%.1f', sampleRate), '$')
dirSamples = DirSource(g_corpus_directory_en, pattern = corpusTestFilesPattern)
print(dirSamples$filelist)

cTest = loadCorpus(directory = g_corpus_directory_en, files_pattern = corpusTestFilesPattern)
cTest <- cleanCorpus(cTest)

# Create the test quadgrams
ngramName = 'quadgram'
message(paste('Get a', ngramName, 'tokenizer...'))
tokenizer4 = function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
message(paste('Compute the DTM...'))
testQuadgramsDtmPathName = paste0(g_corpus_directory_en, '/test_quadgrams_dtm_partition_', sprintf('%.1f', partitionRate), '.partition_', sprintf('%.1f', sampleRate), '.rds')
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
testQuadgramsGram = names(testQuadgramsCount)
testQuadgramsLowerGram = sapply(testQuadgramsGram, function(x) { getFirstNTokens(x, 3)})
testQuadgramsDT = data.table(gram = testQuadgramsGram, lowergram = testQuadgramsLowerGram)
testQuadgramsDTPathName = paste0(g_corpus_directory_en, '/test_quadgrams_DT_partition_', sprintf('%.1f', partitionRate), '.partition_', sprintf('%.1f', sampleRate), '.rds')
saveRDS(object = testQuadgramsDT, file = testQuadgramsDTPathName)

# Load the model under test
modelMLE = loadModel(directory = '.', sample_rate = 0.1)

# Quick test the model
predictions = predictNextNBestWords(model = modelMLE, sentence = 'Hello how are you doing today', matches_count_max = 5)
predictions = predictNextNBestWords(model = modelMLE, sentence = 'djsh dsjhd sjhd  sdh sdj sjdh jsdh sdhjshdsjdh', matches_count_max = 5)

# Test!
# Training data: 10% partition
# Probability mode: MLE
# Test data: 1% of the 90% partition (The non-training data set)
# Result: Match(es): 28623 our of 85259 ( 33.57 %)
execTime <- system.time({
    matchCount = 0
    testCount = dim(testQuadgramsDT)[1]
    for (i in 1:testCount) {
        # message(paste('----- Test', i, '-----'))
        gramUnderTest = testQuadgramsDT[i, .(gram, lowergram)]
        # message(paste0('Gram under test: "', as.character(gramUnderTest[1, gram]), '"'))
        sentenceUnderTest = as.character(gramUnderTest[1,lowergram])
        wordToPredict = getLastNTokens(as.character(gramUnderTest[1,gram]), 1)
        # message(paste0('Predicting "', sentenceUnderTest, '"'))
        predictedWords = predictNextNBestWords(model = modelMLE, sentenceUnderTest, matches_count_max = 5)
        # print(predictedWords)
        if (length(predictedWord) == 0) {
            message('No prediction!')
        } else if (wordToPredict %in% predictedWords) {
            # message(paste0('*** Prediction match!'))    
            matchCount = matchCount + 1
        } else {
            # message(paste0('No prediction matched!'))    
        }
    }
    message(paste('Match(es):', matchCount, 'our of', testCount, '(', round(matchCount*100/testCount, 2), '%)'))
})
execTimeSeconds = execTime["elapsed"]
execTimeMins = execTimeSeconds/60
execTimeSecsAfterMins = execTimeSeconds %% 60
message(paste('Test executed in', round(execTimeMins), 'mins, ', round(execTimeSecsAfterMins, 2), 'secs'))
