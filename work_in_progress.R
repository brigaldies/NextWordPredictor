# c1 = parallelizeTask(cleanCorpus, c1) # No gain!

# Write out the corpus back out to a file after cleanup to inspect
outPath = paste0(g_corpus_directory_en, '\\', 'cleaned_corpus_sample_', sprintf('%.1f', sampleRate), '.txt')
outFile = file(outPath, open = "wt", encoding = "UTF-8")
docsCount = 3
for (i in 1:docsCount) {
    doc = c1[[i]]$content
    writeLines(paste('*** Document', i, '***'), con = outFile)
    linesCount = length(doc)
    for (j in 1:linesCount) {
        writeLines(doc[j], con = outFile)
    }
}
close(outFile)
# Re-load from the cleanup corpus
c1 = loadCorpus(directory = g_corpus_directory_en, files_pattern = 'cleaned_corpus_sample_10.0.txt')

tokenizer1 = function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
#tokenizer1 = function(x) WordTokenizer(x, Weka_control(min = 1, max = 1))
#tokenizer1 = function(x) AlphabeticalTokenizer(x)
#c1 = cTest
execTime <- system.time({
    unigramsDtm = DocumentTermMatrix(c1, control = list(tokenize = tokenizer1))
    unigramsCount = colSums(as.matrix(unigramsDtm))
    unigramsString = names(unigramsCount)
    #unigramsDat = data.frame(gram = unigramsString, count = unigramsCount)
    unigramsDat = data.table(gram = unigramsString, count = unigramsCount, key = 'gram')
    #unigramsDat = unigramsDat[order(count, decreasing = TRUE)]
    head(unigramsDat)
})
message(paste('Unigrams model built in', round(execTime["elapsed"], 2), "secs"))

# 
tokenizer2 = function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
# c1 = cTest
bigramsDtm = DocumentTermMatrix(c1, control = list(tokenize = tokenizer2))
bigramsCount = colSums(as.matrix(bigramsDtm))
bigramsString = names(bigramsCount)
bigramsDat = data.table(gram = bigramsString, count = bigramsCount, key = "gram")
head(bigramsDat)

execTime <- system.time({
    gramsDtm = DocumentTermMatrix(c1, control = list(tokenize = tokenizer2))
    gramsCount = colSums(as.matrix(gramsDtm))
    gramsString = names(gramsCount)
    #unigramsDat = data.frame(gram = unigramsString, count = unigramsCount)
    gramsDat = data.table(gram = gramsString, count = gramsCount, key = 'gram')
    #unigramsDat = unigramsDat[order(count, decreasing = TRUE)]
    head(gramsDat)
})
message(paste('1+2-grams model built in', round(execTime["elapsed"], 2), "secs"))

# ----------------------- WORK IN PROGRESS (BEGIN) ----------------------------


# ----------------------- WORK IN PROGRESS (END) ----------------------------