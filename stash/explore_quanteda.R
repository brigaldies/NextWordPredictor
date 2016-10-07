require(quanteda)
t1 = "A lion and an_antelop. This is 2016; Hello there! I'm testing-Quanteda, to see -- how fast it is and how easy it might be to use. Let's get going..."
cq = corpus(t1)
summary(cq)
dfm = dfm(cq)

# Load from a TM Vcorpus
cq = quanteda::corpus(c1)
summary(cq)

# Load from files
cq = corpus(textfile(file = paste0(g_corpus_directory_en, '\\', 'en_US.news.txt.sample_1.0.txt')))
summary(cq)

require(data.table)
dat = data.table(ngram = features(dfm), count = colSums(dfm), key = "ngram")
dat <- dat[order(count, decreasing = TRUE)]
dat

# Problem: The tokens concatenator is the '_' character.

buildNGramQ = function(cq, tokenizingWhat, n) {
    execTime <- system.time({
        ngramName = paste0(n, '-gram')
        dfmMatrix = parallelizeTask(
            dfm,
            cq, 
            what = tokenizingWhat,
            # concatenator = ' ',
            removeNumbers = TRUE,
            removePunct = TRUE, 
            removeSeparators = TRUE,
            removeTwitter = FALSE, 
            removeHyphens = FALSE,
            ngrams = n,
            simplify = TRUE,
            verbose = TRUE
        )
    })
    message(paste('Quanteda DFM for', ngramName, 'built in', round(execTime["elapsed"], 2), "secs"))
    dfmMatrix    
}

# Unigrams
dfm1 = buildNGramQ(cq, "fastestword", 1)
dat1 = data.table(ngram = features(dfm1), count = colSums(dfm1), key = "ngram")
#dat1 = dat1[order(count, decreasing = TRUE)]
head(dat1, n = 10)

# Bigrams
dfm2 = buildNGramQ(cq, "fastestword", 2)
dat2 = data.table(ngram = features(dfm2), count = colSums(dfm2), key = "ngram")
dat2

# Alternative ngram construction
ng1 = ngrams(tokenize(t1, removePunct = TRUE, removeNumbers = TRUE), n = 1, concatenator = ' ')
dfm1 = dfm(ng1)
dat1 = data.table(ngram = features(dfm1), count = colSums(dfm1), key = "ngram")
dat1[order(count, decreasing = TRUE)]

ng2 = ngrams(tokenize(cq, removePunct = TRUE, removeNumbers = TRUE), n = 2, concatenator = ' ')
dat2 = data.table(ngram = features(dfm2), count = colSums(dfm2), key = "ngram")
dat2[order(count, decreasing = TRUE)]
