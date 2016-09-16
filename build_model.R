# -----------------------------------------------------------------------------
# Load and clean the corpus
# -----------------------------------------------------------------------------
sampleRate = 10
# Check the file pattern
corpusFilesPattern = paste0('^en_US(.)*partition_', sprintf('%.1f', sampleRate), '$')
dirSamples = DirSource(g_corpus_directory_en, pattern = files_pattern)
print(dirSamples$filelist)

# Load the Corpus
c1 = loadCorpus(directory = g_corpus_directory_en, files_pattern = corpusFilesPattern) # ~2 mins with a 10% sample.
c1 = cleanCorpus(c1) # ~2mins with a 10% sample.

# Write the cleaned documents to a single text file for inspection
writeCorpusToTextFile(c1, paste0(g_corpus_directory_en, '/corpus_cleaned_partition_', sprintf('%.1f', sampleRate), '.txt'))

# Save the cleaned corpus
saveRDS(object = c1, file = paste0(g_corpus_directory_en, '/corpus_cleaned_partition_', sprintf('%.1f', sampleRate), '.rds'))

# Re-load the cleaned corpus from disk
c1 = readRDS(file = paste0(g_corpus_directory_en, '/corpus_cleaned_partition_', sprintf('%.1f', sampleRate), '.rds'))

# -----------------------------------------------------------------------------
# Build unigrams with Quanteda
# (Issue with RWeka unigram tokenizer: It removes stop words!)
# -----------------------------------------------------------------------------
execTime = system.time({
    unigramsDat = buildNGramQuanteda(c1, 1)
    saveRDS(object = unigramsDat, file = paste0(g_corpus_directory_en, '\\unigrams_partition_', sprintf('%.1f', sampleRate), '.rds'))
})
execTimeSeconds = execTime["elapsed"]
execTimeMins = execTimeSeconds/60
execTimeSecsAfterMins = execTimeSeconds %% 60
message(paste('Unigrams model built in', round(execTimeMins), 'mins, ', round(execTimeSecsAfterMins, 2), 'secs'))
# Re-load the unigrams
unigramsDat1 = readRDS(file = paste0(g_corpus_directory_en, '\\unigrams_sample_', sprintf('%.1f', sampleRate), '.rds')) # version 1
unigramsDat = readRDS(file = paste0(g_corpus_directory_en, '\\unigrams_partition_', sprintf('%.1f', sampleRate), '.rds')) # version 2

# -----------------------------------------------------------------------------
# Build bigrams with TM/RWeka 
# (Issue with Quanteda: The dfm construction is much slower than that of the TM DTM)
# With a 10% partition:
# DTM built in ~ 2 mins
# bigrams built in ~ 2 hours
# -----------------------------------------------------------------------------
ngramName = 'bigram'
message(paste('Get a', ngramName, 'tokenizer...'))
tokenizer2 = function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
message(paste('Compute the DTM...'))
bigramsDtmPathName = paste0(g_corpus_directory_en, '\\bigrams_dtm_partition_', sprintf('%.1f', sampleRate), '.rds')
execTime = system.time({
    bigramsDtm = DocumentTermMatrix(c1, control = list(tokenize = tokenizer2))
    saveRDS(object = bigramsDtm, file = bigramsDtmPathName)
})
message(paste(ngramName, 'DTM built in', round(execTime["elapsed"], 2), "secs"))
message(paste(ngramName, 'DTM size:', round(object.size(bigramsDtm)/1024/2014, 2), 'MB'))

# Read the bigrams DTM if necessary
bigramsDtm = readRDS(file = bigramsDtmPathName)

execTime = system.time({
    bigramsDat = buildNGramWithMLE(corpus = c1, 
                                   dtm = bigramsDtm,
                                   n = 2, 
                                   lowerOrderNGram = unigramsDat,
                                   max_loop = NULL)
    bigramsDat2 = bigramsDat[!is.na(logprob)]
    saveRDS(object = bigramsDat2, file = paste0(g_corpus_directory_en, '\\bigrams_partition_', sprintf('%.1f', sampleRate), '.rds'))
})
execTimeSeconds = execTime["elapsed"]
execTimeMins = execTimeSeconds/60
execTimeSecsAfterMins = execTimeSeconds %% 60
message(paste('Bigrams model built in', round(execTimeMins), 'mins, ', round(execTimeSecsAfterMins, 2), 'secs'))
bigramsDat2 = readRDS(file = paste0(g_corpus_directory_en, '\\bigrams_partition_', sprintf('%.1f', sampleRate), '.rds'))

# -----------------------------------------------------------------------------
# Build trigrams with TM/RWeka 
# (Issue with Quanteda: The dfm construction is much slower than that of the TM DTM)
# With a 10% partition:
# DTM built in ~ 5 mins
# bigrams built in ~ 5 1/2 hours
# -----------------------------------------------------------------------------
ngramName = 'trigram'
message(paste('Get a', ngramName, 'tokenizer...'))
tokenizer3 = function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
message(paste('Compute the DTM...'))
trigramsDtmPathName = paste0(g_corpus_directory_en, '\\trigrams_dtm_partition_', sprintf('%.1f', sampleRate), '.rds')
execTime = system.time({
    trigramsDtm = DocumentTermMatrix(c1, control = list(tokenize = tokenizer3))
    saveRDS(object = trigramsDtm, file = trigramsDtmPathName)
})
message(paste(ngramName, 'DTM built in', round(execTime["elapsed"], 2), "secs"))
message(paste(ngramName, 'DTM size:', round(object.size(trigramsDtm)/1024/2014, 2), 'MB'))

# Read the bigrams DTM if necessary
trigramsDtm = readRDS(file = trigramsDtmPathName)

execTime = system.time({
    trigramsDat = buildNGramWithMLE(corpus = c1, 
                                    dtm = trigramsDtm,
                                    n = 3,
                                    lowerOrderNGram = bigramsDat2,
                                    max_loop = NULL)
    trigramsDat2 = trigramsDat[!is.na(logprob)]
    saveRDS(object = trigramsDat2, file = paste0(g_corpus_directory_en, '\\trigrams_partition_', sprintf('%.1f', sampleRate), '.rds'))
})
execTimeSeconds = execTime["elapsed"]
execTimeMins = execTimeSeconds/60
execTimeSecsAfterMins = execTimeSeconds %% 60
message(paste('Trigrams model built in', round(execTimeMins, 2), 'mins, ', round(execTimeSecsAfterMins, 2), 'secs'))
trigramsDat2 = readRDS(file = paste0(g_corpus_directory_en, '\\trigrams_partition_', sprintf('%.1f', sampleRate), '.rds'))
trigramsDat3 = trigramsDat2
setkey(trigramsDat3, 'lowergram')
key(trigramsDat3)

# -----------------------------------------------------------------------------
# Build quadgrams with TM/RWeka
# (Issue with Quanteda: The dfm construction is much slower than that of the TM DTM)
# -----------------------------------------------------------------------------

# Build the TDM
ngramName = paste0(4, '-gram')
tokenizer4 = function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
message(paste('Compute the DTM...'))
execTime = system.time({
    quadgramsDtm = DocumentTermMatrix(c1, control = list(tokenize = tokenizer4))
    saveRDS(object = quadgramsDtm, file = paste0(g_corpus_directory_en, '\\quadgrams_dtm_partition_', sprintf('%.1f', sampleRate), '.rds'))
})
message(paste(ngramName, 'DTM built in', round(execTime["elapsed"], 2), "secs"))

# Build the quadgrams
execTime = system.time({
    quadgramsDat = buildNGramWithMLE(corpus = c1,
                                     dtm = quadgramsDtm, 
                                     n = 4,
                                     lowerOrderNGram = trigramsDat2,
                                     max_loop = NULL)
    quadgramsDat2 = quadgramsDat[!is.na(logprob)]
    saveRDS(object = quadgramsDat2, file = paste0(g_corpus_directory_en, '\\quadgrams_partition_', sprintf('%.1f', sampleRate), '.rds'))
})
execTimeSeconds = execTime["elapsed"]
execTimeMins = execTimeSeconds/60
execTimeSecsAfterMins = execTimeSeconds %% 60
message(paste('Quadgrams model built in', round(execTimeMins, 2), 'mins, ', round(execTimeSecsAfterMins, 2), 'secs'))
quadgramsDat2 = readRDS(file = paste0(g_corpus_directory_en, '\\quadgrams_partition_', sprintf('%.1f', sampleRate), '.rds'))


# model = loadModel(directory = paste0(g_corpus_directory_en, '\\models\\10_percent_sample_mle'), sample_rate = 10)