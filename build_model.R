# -----------------------------------------------------------------------------
# Load and clean the corpus
# -----------------------------------------------------------------------------
sampleRate = 10
corpusFilesPattern = paste0('^en_US(.)*sample_', sprintf('%.1f', sampleRate), '$')
c1 = loadCorpus(directory = g_corpus_directory_en, files_pattern = corpusFilesPattern)
c1 = cleanCorpus(c1)

# Save the cleaned corpus
saveRDS(object = c1, file = paste0(g_corpus_directory_en, '\\corpus_cleaned_sample_', sprintf('%.1f', sampleRate), '.rds'))

# Re-load the cleaned corpus from disk
c1 = readRDS(file = paste0(g_corpus_directory_en, '\\corpus_cleaned_sample_', sprintf('%.1f', sampleRate), '.rds'))

# -----------------------------------------------------------------------------
# Build unigrams with Quanteda
# (Issue with RWeka unigram tokenizer: It removes stop words!)
# -----------------------------------------------------------------------------
execTime = system.time({
    unigramsDat = buildNGramQuanteda(c1, 1)
    saveRDS(object = unigramsDat, file = paste0(g_corpus_directory_en, '\\unigrams_sample_', sprintf('%.1f', sampleRate), '.rds'))
})
execTimeSeconds = execTime["elapsed"]
execTimeMins = execTimeSeconds/60
execTimeSecsAfterMins = execTimeSeconds %% 60
message(paste('Unigrams model built in', round(execTimeMins), 'mins, ', round(execTimeSecsAfterMins, 2), 'secs'))
# Re-load the unigrams
unigramsDat = readRDS(file = paste0(g_corpus_directory_en, '\\unigrams_sample_', sprintf('%.1f', sampleRate), '.rds'))

# -----------------------------------------------------------------------------
# Build bigrams with TM/RWeka 
# (Issue with Quanteda: The dfm construction is much slower than that of the TM DTM)
# -----------------------------------------------------------------------------
execTime = system.time({
    bigramsDat = buildNGramTM2(corpus = c1, 
                               dtm = NULL,
                               n = 2, 
                               lowerOrderNGram = unigramsDat,
                               max_loop = NULL)
    bigramsDat2 = bigramsDat[!is.na(logprob)]
    saveRDS(object = bigramsDat2, file = paste0(g_corpus_directory_en, '\\bigrams_sample_', sprintf('%.1f', sampleRate), '.rds'))
})
execTimeSeconds = execTime["elapsed"]
execTimeMins = execTimeSeconds/60
execTimeSecsAfterMins = execTimeSeconds %% 60
message(paste('Bigrams model built in', round(execTimeMins), 'mins, ', round(execTimeSecsAfterMins, 2), 'secs'))
bigramsDat2 = readRDS(file = paste0(g_corpus_directory_en, '\\bigrams_sample_', sprintf('%.1f', sampleRate), '.rds'))

# -----------------------------------------------------------------------------
# Build trigrams with TM/RWeka 
# (Issue with Quanteda: The dfm construction is much slower than that of the TM DTM)
# -----------------------------------------------------------------------------
execTime = system.time({
    trigramsDat = buildNGramTM2(corpus = c1, 
                                dtm = NULL,
                                n = 3,
                                lowerOrderNGram = bigramsDat2,
                                max_loop = NULL)
    trigramsDat2 = trigramsDat[!is.na(logprob)]
    saveRDS(object = trigramsDat2, file = paste0(g_corpus_directory_en, '\\trigrams_sample_', sprintf('%.1f', sampleRate), '.rds'))
})
execTimeSeconds = execTime["elapsed"]
execTimeMins = execTimeSeconds/60
execTimeSecsAfterMins = execTimeSeconds %% 60
message(paste('Trigrams model built in', round(execTimeMins, 2), 'mins, ', round(execTimeSecsAfterMins, 2), 'secs'))
trigramsDat2 = readRDS(file = paste0(g_corpus_directory_en, '\\trigrams_sample_', sprintf('%.1f', sampleRate), '.rds'))
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
    saveRDS(object = quadgramsDtm, file = paste0(g_corpus_directory_en, '\\quadgrams_dtm_sample_', sprintf('%.1f', sampleRate), '.rds'))
})
message(paste(ngramName, 'DTM built in', round(execTime["elapsed"], 2), "secs"))

# Build the quadgrams
execTime = system.time({
    quadgramsDat = buildNGramTM2(corpus = c1,
                                 dtm = quadgramsDtm, 
                                 n = 4,
                                 lowerOrderNGram = trigramsDat2,
                                 max_loop = NULL)
    quadgramsDat2 = quadgramsDat[!is.na(logprob)]
    saveRDS(object = quadgramsDat2, file = paste0(g_corpus_directory_en, '\\quadgrams_sample_', sprintf('%.1f', sampleRate), '.rds'))
})
execTimeSeconds = execTime["elapsed"]
execTimeMins = execTimeSeconds/60
execTimeSecsAfterMins = execTimeSeconds %% 60
message(paste('Quadgrams model built in', round(execTimeMins, 2), 'mins, ', round(execTimeSecsAfterMins, 2), 'secs'))
quadgramsDat2 = readRDS(file = paste0(g_corpus_directory_en, '\\quadgrams_sample_', sprintf('%.1f', sampleRate), '.rds'))


# model = loadModel(directory = paste0(g_corpus_directory_en, '\\models\\10_percent_sample_mle'), sample_rate = 10)