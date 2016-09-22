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

# Re-load the cleaned corpus from disk
c1 = readRDS(file = paste0(g_corpus_directory_en, '/corpus_cleaned_partition_', sprintf('%.1f', partitionRate), '.rds'))

# -----------------------------------------------------------------------------
# Build unigrams with Quanteda
# (Issue with RWeka unigram tokenizer: It removes stop words!)
# ~ 2 mins with 10% partition
# ~ 5 mins with 20% partition
# -----------------------------------------------------------------------------
execTime = system.time({
    unigramsDat = buildNGramQuanteda(c1, 1)
    saveRDS(object = unigramsDat, file = paste0(g_corpus_directory_en, '\\unigrams_mle_partition_', sprintf('%.1f', partitionRate), '.rds'))
})
execTimeSeconds = execTime["elapsed"]
execTimeMins = execTimeSeconds/60
execTimeSecsAfterMins = execTimeSeconds %% 60
message(paste('Unigrams model built in', round(execTimeMins), 'mins, ', round(execTimeSecsAfterMins, 2), 'secs'))
# Re-load the unigrams
unigramsDat1 = readRDS(file = paste0('.', '/unigrams.rds')) # version 1
unigramsDat = readRDS(file = paste0(g_corpus_directory_en, '\\unigrams_mle_partition_', sprintf('%.1f', partitionRate), '.rds')) # version 2

# -----------------------------------------------------------------------------
# Build bigrams with TM/RWeka 
# (Issue with Quanteda: The dfm construction is much slower than that of the TM DTM)
# With a 10% partition:
# DTM built in ~ 2 mins
# bigrams built in ~ 2 mins with the use of "merge".
#
# With a 20% partition:
# DTM built in ~ 4 mins
# bigrams built in ?
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

# Read the bigrams DTM if necessary
bigramsDtm = readRDS(file = bigramsDtmPathName)

kSmoothingFraction = 0.05
bigramsModelPathNameRfc = paste0(g_corpus_directory_en, 
                                          '\\bigrams_rfc_partition_', sprintf('%.1f', partitionRate), 
                                          '.rds')
bigramsModelPathNameNoKSmoothing = paste0(g_corpus_directory_en, 
                              '\\bigrams_mle_partition_', sprintf('%.1f', partitionRate), 
                              '.rds')
bigramsModelPathName = paste0(g_corpus_directory_en, 
                              '\\bigrams_mle_k', kSmoothingFraction,
                              '_partition_', sprintf('%.1f', partitionRate), 
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
    
    # Simple Frequency Count probabilities
    # bigramsDat2 = buildNGramTMWithRFC(corpus = NULL, 
    #                                   dtm = bigramsDtm,
    #                                   n = 2, 
    #                                   minCount = 2,
    #                                   parallel = FALSE)
    # saveRDS(object = bigramsDat2, file = bigramsModelPathNameRfc)
})
execTimeSeconds = execTime["elapsed"]
execTimeMins = execTimeSeconds/60
execTimeSecsAfterMins = execTimeSeconds %% 60
message(paste('Bigrams model built in', round(execTimeMins), 'mins, ', round(execTimeSecsAfterMins, 2), 'secs'))
bigramsDat2 = readRDS(file = bigramsModelPathNameRfc)
bigramsDat2 = readRDS(file = bigramsModelPathNameNoKSmoothing)
bigramsDat2 = readRDS(file = bigramsModelPathName)

# Build the additional columns from the non-k-add model
bigramsDat2[, c("lowergramcount") := {count/exp(logprob)}]
bigramsUniqueCount = dim(bigramsDat2)[1]
bigramsDat2[, c("logprobk") := {log((count + kSmoothingFraction)/(lowergramcount + kSmoothingFraction*bigramsUniqueCount))}]
bigramsTotalCount = sum(bigramsDat2$count)
bigramsDat2[, c("logfreq")  := {log(count/bigramsTotalCount)}]
saveRDS(object = bigramsDat2, file = bigramsModelPathName)

# -----------------------------------------------------------------------------
# Build trigrams with TM/RWeka 
# (Issue with Quanteda: The dfm construction is much slower than that of the TM DTM)
# With a 10% partition:
# DTM built in ~ 5 mins
# trigrams built in ~ 5.3 hours
# 
# With a 20% partition:
# DTM built in ~ 10 mins
# trigrams built in ~ 5.3 hours
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

# Read the trigrams DTM if necessary
trigramsDtm = readRDS(file = trigramsDtmPathName)

trigramsModelPathNameRfc = paste0(g_corpus_directory_en, 
                                 '\\trigrams_rfc_partition_', sprintf('%.1f', partitionRate), 
                                 '.rds')

trigramsModelPathNameNoKSmoothing = paste0(g_corpus_directory_en, 
                                          '\\trigrams_mle_partition_', sprintf('%.1f', partitionRate), 
                                          '.rds')
trigramsModelPathName = paste0(g_corpus_directory_en, 
                              '\\trigrams_mle_k', kSmoothingFraction,
                              '_partition_', sprintf('%.1f', partitionRate), 
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
    
    # Simple Frequency Count probabilities
    # trigramsDat2 = buildNGramTMWithRFC(corpus = NULL, 
    #                                    dtm = trigramsDtm,
    #                                    n = 3, 
    #                                    minCount = 2,
    #                                    parallel = FALSE)
    # saveRDS(object = trigramsDat2, file = trigramsModelPathNameRfc)
    
})
execTimeSeconds = execTime["elapsed"]
execTimeMins = execTimeSeconds/60
execTimeSecsAfterMins = execTimeSeconds %% 60
message(paste('Trigrams model built in', round(execTimeMins, 2), 'mins, ', round(execTimeSecsAfterMins, 2), 'secs'))
trigramsDat2 = readRDS(file = trigramsModelPathNameRfc)
trigramsDat2 = readRDS(file = trigramsModelPathNameNoKSmoothing)
trigramsDat2 = readRDS(file = trigramsModelPathName)
trigramsDat3 = trigramsDat2
setkey(trigramsDat3, 'lowergram')
key(trigramsDat3)

# Build the additional columns from the non-k-add model
trigramsDat2[, c("lowergramcount") := {count/exp(logprob)}]
trigramsUniqueCount = dim(trigramsDat2)[1]
trigramsDat2[, c("logprobk") := {log((count + kSmoothingFraction)/(lowergramcount + kSmoothingFraction*trigramsUniqueCount))}]
trigramsTotalCount = sum(trigramsDat2$count)
trigramsDat2[, c("logfreq")  := {log(count/trigramsTotalCount)}]
saveRDS(object = trigramsDat2, file = trigramsModelPathName)

# -----------------------------------------------------------------------------
# Build quadgrams with TM/RWeka
# (Issue with Quanteda: The dfm construction is much slower than that of the TM DTM)
# With a 10% partition:
# DTM built in ~ 6 mins
# trigrams built in ~ 
#
# With a 20% partition:
# DTM built in ~ 13 mins
# trigrams built in ~ 
# -----------------------------------------------------------------------------

# Build the TDM
ngramName = paste0(4, '-gram')
tokenizer4 = function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
quadgramsDtmPathName = paste0(g_corpus_directory_en, '\\quadgrams_dtm_partition_', sprintf('%.1f', partitionRate), '.rds')
message(paste('Compute the DTM...'))
execTime = system.time({
    quadgramsDtm = DocumentTermMatrix(c1, control = list(tokenize = tokenizer4))
    saveRDS(object = quadgramsDtm, file = quadgramsDtmPathName)
})
message(paste(ngramName, 'DTM built in', round(execTime["elapsed"], 2), "secs"))

# Read the quadgrams DTM if necessary
quadgramsDtm = readRDS(file = quadgramsDtmPathName)

# Build the quadgrams
quadgramsModelPathNameRfc = paste0(g_corpus_directory_en, 
                                  '\\quadgrams_rfc_partition_', sprintf('%.1f', partitionRate), 
                                  '.rds')
quadgramsModelPathNameNoKSmoothing = paste0(g_corpus_directory_en, 
                                           '\\quadgrams_mle_partition_', sprintf('%.1f', partitionRate), 
                                           '.rds')
quadgramsModelPathName = paste0(g_corpus_directory_en, 
                               '\\quadgrams_mle_k', kSmoothingFraction,
                               '_partition_', sprintf('%.1f', partitionRate), 
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
    
    # Simple Frequency Count probabilities
    # quadgramsDat2 = buildNGramTMWithRFC(corpus = NULL, 
    #                                     dtm = quadgramsDtm,
    #                                     n = 4, 
    #                                     minCount = 2,
    #                                     parallel = FALSE)
    # saveRDS(object = quadgramsDat2, file = quadgramsModelPathNameRfc)
    
})
execTimeSeconds = execTime["elapsed"]
execTimeMins = execTimeSeconds/60
execTimeSecsAfterMins = execTimeSeconds %% 60
message(paste('Quadgrams model built in', round(execTimeMins, 2), 'mins, ', round(execTimeSecsAfterMins, 2), 'secs'))
quadgramsDat2 = readRDS(file = quadgramsModelPathNameRfc)
quadgramsDat2 = readRDS(file = quadgramsModelPathNameNoKSmoothing)
quadgramsDat2 = readRDS(file = quadgramsModelPathName)

# Build the additional columns from the non-k-add model
quadgramsUniqueCount = dim(quadgramsDat2)[1]
quadgramsDat2[, c("logprobk") := {log((count + kSmoothingFraction)/(lowercount + kSmoothingFraction*quadgramsUniqueCount))}]
quadgramsTotalCount = sum(quadgramsDat2$count)
quadgramsDat2[, c("logfreq")  := {log(count/quadgramsTotalCount)}]
saveRDS(object = quadgramsDat2, file = quadgramsModelPathName)

# With parallel processing
require(snow)
# Build the lower order grams
cluster8 = makeCluster(8, 'SOCK')
clusterExport(cluster8, list("getFirstNTokens")) # Make getFirstNTokens available to the worker threads.
clusterEvalQ(cluster8, library(stringi)) # Make the package stringi available to the worker threads.
execTime = system.time({
    message(paste0('Construct the lower order ', 3, '-grams...'))
    quadgramsLowerOrderString = unlist(clusterApply(cluster8, quadgramsString, function(gram) { getFirstNTokens(gram, 3) }))
})
execTimeSeconds = execTime["elapsed"]
execTimeMins = execTimeSeconds/60
execTimeSecsAfterMins = execTimeSeconds %% 60
message(paste('Done in', round(execTimeMins, 2), 'mins, ', round(execTimeSecsAfterMins, 2), 'secs'))
stopCluster(cluster8)
remove(cluster8)

# Retrieve lower order grams counts
cluster4 = makeCluster(4, 'SOCK')
clusterEvalQ(cluster4, library(data.table)) # Make the package stringi available to the worker threads.
clusterExport(cluster4, list("trigramsDat2")) # Make the trigrams available to the worker threads (ATTENTION: The data table is copied in each worker thread's process! Watch the RAM usage)
execTime = system.time({
    message(paste0('Retrieve the lower order ', 3, '-grams frequency counts...'))
    quadgramsLowerOrderCount = unlist(clusterApply(cluster4, quadgramsLowerOrderString, function(lowerOrderGram) { as.numeric(trigramsDat2[lowerOrderGram][1,count]) }))
})
execTimeSeconds = execTime["elapsed"]
execTimeMins = execTimeSeconds/60
execTimeSecsAfterMins = execTimeSeconds %% 60
message(paste('Done in', round(execTimeMins, 2), 'mins, ', round(execTimeSecsAfterMins, 2), 'secs'))
stopCluster(cluster4)
remove(cluster4)

# Calculate the quadgrams MLE
message(paste0('Compute the quadgrams MLE...'))
execTime = system.time({
    quadgramsLogProb = log(quadgramsCount/quadgramsLowerOrderCount)
})
execTimeSeconds = execTime["elapsed"]
execTimeMins = execTimeSeconds/60
execTimeSecsAfterMins = execTimeSeconds %% 60
message(paste('Done in', round(execTimeMins, 2), 'mins, ', round(execTimeSecsAfterMins, 2), 'secs'))

# Build the quadgrams data table
quadgramsDat2 = data.table(gram = quadgramsString, 
                            count = quadgramsCount,
                            lowergram = quadgramsLowerOrderString, 
                            lowercount =  quadgramsLowerOrderCount,
                            logprob = quadgramsLogProb,
                            key = "gram")    
# Save the quadgrams
saveRDS(object = quadgramsDat2, file = paste0(g_corpus_directory_en, '\\quadgrams_mle_partition_', sprintf('%.1f', partitionRate), '.rds'))

# Build final model
remove(modelUnderTest)
modelUnderTest = list()

# Load the unigrams
modelUnderTest$unigrams = unigramsDat[count >= 2][, .(gram, logprob)][order(logprob, decreasing = TRUE)]
setkey(modelUnderTest$bigrams, lowergram)

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

modelUnderTest = readRDS(paste0(g_corpus_directory_en, '/modelUnderTest_partition10.0.rds'))
