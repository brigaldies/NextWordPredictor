source('./config.R')
source('./utils.R')
source('./load_corpus.R')
source('./build_ngram.R')

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

# -----------------------------------------------------------------------------
# Build bigrams with TM/RWeka 
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
    bigramsDat2 = bigramsDat[!is.na(logprob)]
    saveRDS(object = bigramsDat2, file = bigramsModelPathNameNoKSmoothing)
})
execTimeSeconds = execTime["elapsed"]
execTimeMins = execTimeSeconds/60
execTimeSecsAfterMins = execTimeSeconds %% 60
message(paste('Bigrams model built in', round(execTimeMins), 'mins, ', round(execTimeSecsAfterMins, 2), 'secs'))

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

# -----------------------------------------------------------------------------
# Build final model
# -----------------------------------------------------------------------------

remove(modelUnderTest)
modelUnderTest = list()

# Load the unigrams
modelUnderTest$unigrams = unigramsDat[count >= 2][, .(gram, logprob)][order(logprob, decreasing = TRUE)]

# Load the bigrams
modelUnderTest$bigrams = bigramsDat2[count >= 2][, .(gram, lowergram, logprob, last_word_in_gram)][order(logprob, decreasing = TRUE)]
setkey(modelUnderTest$bigrams, lowergram)

# Load the trigrams
modelUnderTest$trigrams = trigramsDat2[count >= 2][, .(gram, lowergram, logprob, last_word_in_gram)][order(logprob, decreasing = TRUE)]
setkey(modelUnderTest$trigrams, lowergram)

# Load the quadgrams
modelUnderTest$quadgrams = quadgramsDat2[count >= 2][, .(gram, lowergram, logprob, last_word_in_gram)][order(logprob, decreasing = TRUE)]
setkey(modelUnderTest$quadgrams, lowergram)

message(paste(round(object.size(modelUnderTest)/1024/1024, 2), 'MB'))

saveRDS(object = modelUnderTest, file = paste0(g_corpus_directory_en, '/modelUnderTest_partition_', sprintf('%.1f', partitionRate), '_', Sys.Date(), '.rds'))
