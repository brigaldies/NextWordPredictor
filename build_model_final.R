source('./config.R')
source('./utils.R')
source('./load_corpus.R')
source('./build_ngram.R')
require(ggplot2)

# -----------------------------------------------------------------------------
# Load and clean the corpus
# -----------------------------------------------------------------------------
partitionRate = 10
# Check the file pattern
corpusFilesPattern = paste0('^en_US(.)*partition_', sprintf('%.1f', partitionRate), '$')
dirSamples = DirSource(g_corpus_directory_en, pattern = corpusFilesPattern)
print(dirSamples$filelist)

# Load the Corpus
c1 = loadCorpus(directory = g_corpus_directory_en, files_pattern = corpusFilesPattern) # <1 mins for 10% or 20% training partition.
c1 = cleanCorpus(c1) # ~2mins with a 10% sample.

# Write the cleaned documents to a single text file for inspection
writeCorpusToTextFile(c1, paste0(g_corpus_directory_en, '/corpus_cleaned_partition_', sprintf('%.1f', partitionRate), '.txt'))

# Save the cleaned corpus
saveRDS(object = c1, file = paste0(g_corpus_directory_en, '/corpus_cleaned_partition_', sprintf('%.1f', partitionRate), '.rds'))
c1 = readRDS(file = paste0(g_corpus_directory_en, '/corpus_cleaned_partition_', sprintf('%.1f', partitionRate), '.rds'))

# -----------------------------------------------------------------------------
# Build unigrams with Quanteda
# -----------------------------------------------------------------------------
execTime = system.time({
    unigramsDat = buildNGramQuanteda(c1, 1)
    saveRDS(object = unigramsDat, file = paste0(g_corpus_directory_en, '\\unigrams_mle_partition_', sprintf('%.1f', partitionRate), '.rds'))
})
execTimeSeconds = execTime["elapsed"]
execTimeMins = execTimeSeconds/60
execTimeSecsAfterMins = execTimeSeconds %% 60
message(paste('Unigrams model built in', round(execTimeMins), 'mins, ', round(execTimeSecsAfterMins, 2), 'secs'))
unigramsDat = readRDS(file = paste0(g_corpus_directory_en, '\\unigrams_mle_partition_', sprintf('%.1f', partitionRate), '.rds'))
message(paste(round(object.size(unigramsDat)/1024/1024, 2), 'MB'))
unigramsModel = unigramsDat
head(unigramsModel[order(logprob, decreasing = TRUE)])
topGramsPlot(unigramsModel, 'Unigram', 10, 25, 'COUNT')
topGramsPlot(unigramsModel, 'Unigram', 10, 25, 'MLE')

# -----------------------------------------------------------------------------
# Build bigrams with TM/RWeka 
#
# ~25 mins with sample rate = 10%
# -----------------------------------------------------------------------------
ngramName = 'bigram'
message(paste('Get a', ngramName, 'tokenizer...'))
tokenizer2 = function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
message(paste('Compute the DTM...'))
bigramsDtmPathName = paste0(g_corpus_directory_en, '\\bigrams_dtm_partition_', sprintf('%.1f', partitionRate), '.rds')
execTime = system.time({
    bigramsDtm = DocumentTermMatrix(c1, control = list(tokenize = tokenizer2))
    saveRDS(object = bigramsDtm, file = bigramsDtmPathName)
})
message(paste(ngramName, 'DTM built in', round(execTime["elapsed"], 2), "secs"))
message(paste(ngramName, 'DTM size:', round(object.size(bigramsDtm)/1024/2014, 2), 'MB'))
# bigramsDtm = readRDS(file = bigramsDtmPathName)
bigramsModelPathNameNoKSmoothing = paste0(g_corpus_directory_en, 
                                          '\\bigrams_mle_partition_', sprintf('%.1f', partitionRate), 
                                          '.rds')

execTime = system.time({
    bigramsDat = buildNGramWithMLE3(corpus = NULL, # Use 'c1' is bigramsDtm was not calculated ahead of time.
                                    dtm = bigramsDtm,
                                    n = 2,
                                    k = 0,
                                    lowerOrderNGram = unigramsDat,
                                    minCount = 2,
                                    parallel = FALSE)
    # Remove entries with NA
    bigramsDat2 = bigramsDat[!is.na(logprob)]
    saveRDS(object = bigramsDat2, file = bigramsModelPathNameNoKSmoothing)
})
execTimeSeconds = execTime["elapsed"]
execTimeMins = execTimeSeconds/60
execTimeSecsAfterMins = execTimeSeconds %% 60
message(paste('Bigrams model built in', round(execTimeMins), 'mins, ', round(execTimeSecsAfterMins, 2), 'secs'))
# bigramsDat2 = readRDS(file = bigramsModelPathNameNoKSmoothing)

message(paste(round(object.size(bigramsDat2)/1024/1024, 2), 'MB'))
bigramsModel = bigramsDat2[logprob != 0]
head(bigramsModel[order(logprob, decreasing = TRUE)])
topGramsPlot(bigramsModel, 'Bigram', 10, 25, 'COUNT')
topGramsPlot(bigramsModel, 'Bigram', 10, 25, 'MLE')

# -----------------------------------------------------------------------------
# Build trigrams with TM/RWeka 
# -----------------------------------------------------------------------------

ngramName = 'trigram'
message(paste('Get a', ngramName, 'tokenizer...'))
tokenizer3 = function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
message(paste('Compute the DTM...'))
trigramsDtmPathName = paste0(g_corpus_directory_en, '\\trigrams_dtm_partition_', sprintf('%.1f', partitionRate), '.rds')
execTime = system.time({
    trigramsDtm = DocumentTermMatrix(c1, control = list(tokenize = tokenizer3))
    saveRDS(object = trigramsDtm, file = trigramsDtmPathName)
})
message(paste(ngramName, 'DTM built in', round(execTime["elapsed"], 2), "secs"))
message(paste(ngramName, 'DTM size:', round(object.size(trigramsDtm)/1024/2014, 2), 'MB'))
# trigramsDtm = readRDS(file = trigramsDtmPathName)
trigramsModelPathNameNoKSmoothing = paste0(g_corpus_directory_en, 
                                           '\\trigrams_mle_partition_', sprintf('%.1f', partitionRate), 
                                           '.rds')

execTime = system.time({
    trigramsDat = buildNGramWithMLE3(corpus = NULL, # Use 'c1' is trigramsDtm was not calculated ahead of time.
                                     dtm = trigramsDtm,
                                     n = 3,
                                     k = 0,
                                     lowerOrderNGram = bigramsDat2,
                                     minCount = 2,
                                     parallel = FALSE)
    trigramsDat2 = trigramsDat[!is.na(logprob)]
    saveRDS(object = trigramsDat2, file = trigramsModelPathNameNoKSmoothing)
})
execTimeSeconds = execTime["elapsed"]
execTimeMins = execTimeSeconds/60
execTimeSecsAfterMins = execTimeSeconds %% 60
message(paste('Trigrams model built in', round(execTimeMins, 2), 'mins, ', round(execTimeSecsAfterMins, 2), 'secs'))
# trigramsDat2 = readRDS(file = trigramsModelPathNameNoKSmoothing)

message(paste(round(object.size(trigramsDat2)/1024/1024, 2), 'MB'))
trigramsModel = trigramsDat2[logprob != 0]
head(trigramsModel[order(logprob, decreasing = TRUE)])
topGramsPlot(trigramsModel, 'Trigram', 10, 25, 'MLE')
topGramsPlot(trigramsModel, 'Trigram', 10, 25, 'COUNT')

# -----------------------------------------------------------------------------
# Build quadgrams with TM/RWeka 
# -----------------------------------------------------------------------------

ngramName = paste0(4, '-gram')
tokenizer4 = function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
quadgramsDtmPathName = paste0(g_corpus_directory_en, '\\quadgrams_dtm_partition_', sprintf('%.1f', partitionRate), '.rds')
message(paste('Compute the DTM...'))
execTime = system.time({
    quadgramsDtm = DocumentTermMatrix(c1, control = list(tokenize = tokenizer4))
    saveRDS(object = quadgramsDtm, file = quadgramsDtmPathName)
})
message(paste(ngramName, 'DTM built in', round(execTime["elapsed"], 2), "secs"))
# quadgramsDtm = readRDS(file = quadgramsDtmPathName)
quadgramsModelPathNameNoKSmoothing = paste0(g_corpus_directory_en, 
                                            '\\quadgrams_mle_partition_', sprintf('%.1f', partitionRate), 
                                            '.rds')
execTime = system.time({
    quadgramsDat = buildNGramWithMLE3(corpus = NULL, # Use 'c1' is quadgramsDtm was not calculated ahead of time.
                                      dtm = quadgramsDtm,
                                      n = 4,
                                      k = 0,
                                      lowerOrderNGram = trigramsDat2,
                                      minCount = 2,
                                      parallel = FALSE)
    quadgramsDat2 = quadgramsDat[!is.na(logprob)]
    saveRDS(object = quadgramsDat2, file = quadgramsModelPathNameNoKSmoothing)
})
execTimeSeconds = execTime["elapsed"]
execTimeMins = execTimeSeconds/60
execTimeSecsAfterMins = execTimeSeconds %% 60
message(paste('Quadgrams model built in', round(execTimeMins, 2), 'mins, ', round(execTimeSecsAfterMins, 2), 'secs'))
# quadgramsDat2 = readRDS(file = quadgramsModelPathNameNoKSmoothing)

message(paste(round(object.size(quadgramsDat2)/1024/1024, 2), 'MB'))
quadgramsModel = quadgramsDat2[logprob != 0]
head(quadgramsModel[order(logprob, decreasing = TRUE)])
topGramsPlot(quadgramsModel, 'Quadgram', 10, 25, 'MLE')
topGramsPlot(quadgramsModel, 'Quadgram', 10, 25, 'COUNT')

# -----------------------------------------------------------------------------
# Build pentagrams with TM/RWeka 
# With a 10% sample:
# TDM build     : ~7 mins
# pentagramsDat2: ~ <1 min
# -----------------------------------------------------------------------------

ngramName = paste0(5, '-gram')
tokenizer5 = function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5))
pentagramsDtmPathName = paste0(g_corpus_directory_en, '\\pentagrams_dtm_partition_', sprintf('%.1f', partitionRate), '.rds')
message(paste('Compute the DTM...'))
execTime = system.time({
    pentagramsDtm = DocumentTermMatrix(c1, control = list(tokenize = tokenizer5))
    saveRDS(object = pentagramsDtm, file = pentagramsDtmPathName)
})
message(paste(ngramName, 'DTM built in', round(execTime["elapsed"], 2), "secs"))
# pentagramsDtm = readRDS(file = pentagramsDtmPathName)
pentagramsModelPathNameNoKSmoothing = paste0(g_corpus_directory_en, 
                                            '\\pentagrams_mle_partition_', sprintf('%.1f', partitionRate), 
                                            '.rds')
execTime = system.time({
    pentagramsDat = buildNGramWithMLE3(corpus = NULL, # Use 'c1' is pentagramsDtm was not calculated ahead of time.
                                      dtm = pentagramsDtm,
                                      n = 5,
                                      k = 0,
                                      lowerOrderNGram = quadgramsDat2,
                                      minCount = 2,
                                      parallel = FALSE)
    pentagramsDat2 = pentagramsDat[!is.na(logprob)]
    saveRDS(object = pentagramsDat2, file = pentagramsModelPathNameNoKSmoothing)
})
execTimeSeconds = execTime["elapsed"]
execTimeMins = execTimeSeconds/60
execTimeSecsAfterMins = execTimeSeconds %% 60
message(paste('Pentagrams model built in', round(execTimeMins, 2), 'mins, ', round(execTimeSecsAfterMins, 2), 'secs'))
# pentagramsDat2 = readRDS(file = pentagramsModelPathNameNoKSmoothing)

message(paste(round(object.size(pentagramsDat2)/1024/1024, 2), 'MB'))
pentagramsModel = pentagramsDat2[logprob != 0]
head(pentagramsModel[order(logprob, decreasing = TRUE)])
topGramsPlot(pentagramsModel, 'Quadgram', 10, 25, 'MLE')
topGramsPlot(pentagramsModel, 'Quadgram', 10, 25, 'COUNT')

# -----------------------------------------------------------------------------
# Build final model
# -----------------------------------------------------------------------------

remove(modelUnderTest)
modelUnderTest = list()

# Load the unigrams
modelUnderTest$unigrams = unigramsDat[count >= 2 & logprob != 0][, .(gram, count, logprob, logpercent)][order(logprob, decreasing = TRUE)]
setkey(modelUnderTest$unigrams, gram)

# Load the bigrams
modelUnderTest$bigrams = bigramsDat2[count >= 2 & logprob != 0][, .(gram, count, lowergram, logprob, logpercent, last_word_in_gram)][order(logprob, decreasing = TRUE)]
setkey(modelUnderTest$bigrams, lowergram)

# Load the trigrams
modelUnderTest$trigrams = trigramsDat2[count >= 2 & logprob != 0][, .(gram, count, lowergram, logprob, logpercent, last_word_in_gram)][order(logprob, decreasing = TRUE)]
setkey(modelUnderTest$trigrams, lowergram)

# Load the quadgrams
modelUnderTest$quadgrams = quadgramsDat2[count >= 2 & logprob != 0][, .(gram, count, lowergram, logprob, logpercent, last_word_in_gram)][order(logprob, decreasing = TRUE)]
setkey(modelUnderTest$quadgrams, lowergram)

# Load the pentagrams
modelUnderTest$pentagrams = pentagramsDat2[count >= 2 & logprob != 0][, .(gram, count, lowergram, logprob, logpercent, last_word_in_gram)][order(logprob, decreasing = TRUE)]
setkey(modelUnderTest$pentagrams, lowergram)

message(paste(round(object.size(modelUnderTest)/1024/1024, 2), 'MB'))

saveRDS(object = modelUnderTest, file = paste0(g_corpus_directory_en, '/modelUnderTest_partition_', sprintf('%.1f', partitionRate), '_', Sys.Date(), '.rds'))
