library(data.table)

uni_dat = readRDS('P:\\APCC\\CISD11\\Data Science\\unigram_data.RDS')
bi_dat = readRDS('P:\\APCC\\CISD11\\Data Science\\bigram_data.RDS')
tri_dat = readRDS('P:\\APCC\\CISD11\\Data Science\\trigram_data.RDS')
quad_dat = readRDS('P:\\APCC\\CISD11\\Data Science\\quadgram_data.RDS')

source('P:\\APCC\\CISD11\\Data Science\\PredictWord.R')

# Load the test data with test_model.R

# Extract a test sample
testNGramSize = 10000
execTime <- system.time({
    set.seed(10)
    remove(testData)
    testData = rbind(
        sample_n(testBigramsDT, testNGramSize),
        sample_n(testTrigramsDT, testNGramSize),
        sample_n(testQuadgramsDT, testNGramSize),
        sample_n(testPentagramsDT, testNGramSize)
    )
    testCount = dim(testData)[1]
    testResults = mapply(function(lowergram, word_to_predict) {
        word_to_predict %in% predictWord(lowergram)
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

