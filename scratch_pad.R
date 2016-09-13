# Set 'directory' to your own directory
directory = 'C:\\training\\coursera\\datascience\\Capstone\\dataset\\final\\en_US'
fileName = 'en_US.news.txt.sample_1.0'
filePath = paste0(directory, '\\', fileName)
cnx = file(filePath, open = "r")
#cnx = file(filePath, open = "r", encoding = "UTF-8")
#cnx = file(filePath, open = "r", encoding = "UTF-16")
# cnx = file(filePath, open = "r", encoding = "UCS-2LE")
message(paste('File', filePath, 'opened for reading'))
linesChunkSize = 100000
totalLinesCount = 0
while (TRUE) {
    lines = readLines(cnx, n = linesChunkSize, encoding = 'UTF-8')  
    linesCount = length(lines)    
    totalLinesCount = totalLinesCount + linesCount
    if (linesCount == 0 ) {
        message("Reached EOF")
        break
    }    
    message(paste(linesCount, 'lines read. Total thus far:', totalLinesCount))
}
close(cnx)

# Extract the sentences with quanteda
directory = 'C:\\training\\coursera\\datascience\\Capstone\\dataset\\final\\en_US'
fileName = 'sample1.txt'
filePath = paste0(directory, '\\', fileName)
# qcorpus <- corpus(textfile(filePath, encoding = "UTF-8"))
qcorpus <- corpus(textfile(filePath))
summary(qcorpus)
qsentences <- tokenize(qcorpus, what = "sentence", removeNumbers = TRUE,
                   removePunct = TRUE, removeSeparators = TRUE,
                   removeTwitter = TRUE, removeHyphens = TRUE)
qbigrams <- tokenize(qcorpus, what = "word", removeNumbers = TRUE,
         removePunct = TRUE, removeSeparators = TRUE,
         removeTwitter = FALSE, removeHyphens = TRUE,
         ngrams = 2, simplify = TRUE)

# Extract the sentences
sentences = c()
for (i in 1:length(lines)) {
    message(paste0('Line ', i, ' [', lines[i], ']'))
    sentences = c(sentences, extractSentences(lines[i]))    
}

# Using tm
# Load Corpus
# sentences = c(s1, s2)
c1 = Corpus(VectorSource(sentences))

# Load the Corpus from files
execTime <- system.time({
    sampleRate = 10
    corpusFilesPattern = paste0('^en_US(.)*sample_', sprintf('%.1f', sampleRate), '$')
    c10 = loadCorpus(directory = g_corpus_directory_en, files_pattern = corpusFilesPattern)
    c10 = cleanCorpus(c10)
    unigrams = buildNGram(c10, 1)
    dim(unigrams)
    head(unigrams)

    bigrams = buildNGram(c10, 2)
    dim(bigrams)
    head(bigrams)

    trigrams = buildNGram(c10, 3)
    dim(trigrams)
    head(trigrams)

    quadgrams = buildNGram(c10, 4)
    dim(quadgrams)
    head(quadgrams)
})
execTimeSeconds = execTime["elapsed"]
execTimeMins = execTimeSeconds/60
message(paste('1/2/3/4-grams model built in', round(execTimeMins, 2), 'mins,', round(execTimeSeconds %% 60, 2), 'secs'))
modelSize = object.size(unigrams) + object.size(bigrams) + object.size(trigrams) + object.size(quadgrams)
message(paste('Model size: ', round(modelSize/1024/1024, 2), 'MB'))

# Build unigrams
unigramCount = 1
unigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = unigramCount, max = unigramCount))
execTime <- system.time({
    unigramsDtm <- DocumentTermMatrix(c1, control = list(tokenize = unigramTokenizer))
})
message(paste('Unigrams DTM built in', round(execTime["elapsed"], 2), "secs")) # ~ 22 seconds with 10% sampling.
message(paste('Unigrams DTM size:', round(object.size(unigramsDtm)/1024/2014, 2), 'MB'))
message(paste0('Compute the ', unigramCount, '-grams probabilities...'))
unigramsFreq <- colSums(as.matrix(unigramsDtm))
unigramsTotalCount = sum(unigramsFreq)
unigramsPercent <- (unigramsFreq/unigramsTotalCount)*100
unigramsPercentSortDesc <- order(unigramsPercent, decreasing=TRUE)
unigramsPercentSortedDat <- as.data.frame(unigramsPercent[unigramsPercentSortDesc])
unigramsPercentSortedDat <- add_rownames(unigramsPercentSortedDat)
names(unigramsPercentSortedDat) <- c('gram', 'prob')
message(paste('Unigrams data frame size:', round(object.size(unigramsPercentSortedDat)/1024/2014, 2), 'MB'))

# Build bigrams
bigramCount = 2
bigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = bigramCount, max = bigramCount))
execTime <- system.time({
    message(paste0('Compute the Document-Term Matrix (TDM) for ', bigramCount, '-grams...'))
    bigramsDtm <- DocumentTermMatrix(c1, control = list(tokenize = bigramTokenizer))
})
message(paste('Bigrams DTM built in', round(execTime["elapsed"], 2), "secs")) # ~2 mins with 10% sampling.
message(paste('Bigrams DTM size:', round(object.size(bigramsDtm)/1024/2014, 2), 'MB'))
message(paste0('Compute the ', bigramCount, '-grams probabilities...'))
bigramsFreq <- colSums(as.matrix(bigramsDtm))
bigramsTotalCount = sum(bigramsFreq)
bigramsPercent <- (bigramsFreq/bigramsTotalCount)*100
bigramsPercentSortDesc <- order(bigramsPercent, decreasing=TRUE)
bigramsPercentSortedDat <- as.data.frame(bigramsPercent[bigramsPercentSortDesc])
bigramsPercentSortedDat <- add_rownames(bigramsPercentSortedDat)
names(bigramsPercentSortedDat) <- c('gram', 'prob')
message(paste('Bigrams data frame size:', round(object.size(bigramsPercentSortedDat)/1024/2014, 2), 'MB'))

# Build trigrams
trigramCount = 3
trigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = trigramCount, max = trigramCount))
execTime <- system.time({
    message(paste0('Compute the Document-Term Matrix (DTM) for ', trigramCount, '-grams...'))
    trigramsDtm <- DocumentTermMatrix(c1, control = list(tokenize = trigramTokenizer))
})
message(paste('Trigrams DTM built in', round(execTime["elapsed"], 2), "secs")) # ~5 mins secs with 10% sampling.
message(paste('Trigrams DTM size:', round(object.size(trigramsDtm)/1024/2014, 2), 'MB'))
message(paste0('Compute the ', trigramCount, '-grams probabilities...'))
trigramsFreq <- colSums(as.matrix(trigramsDtm))
trigramsTotalCount = colSums(as.data.frame(trigramsFreq))
trigramsPercent <- (trigramsFreq/trigramsTotalCount)*100
trigramsPercentSortDesc <- order(trigramsPercent, decreasing=TRUE)
trigramsPercentSortedDat <- as.data.frame(trigramsPercent[trigramsPercentSortDesc])
trigramsPercentSortedDat <- add_rownames(trigramsPercentSortedDat)
names(trigramsPercentSortedDat) <- c('gram', 'prob')
message(paste('Trigrams data frame size:', round(object.size(trigramsPercentSortedDat)/1024/2014, 2), 'MB'))

# Build quadgrams
fourgramCount = 4
fourgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = fourgramCount, max = fourgramCount))
execTime <- system.time({
    message(paste0('Compute the Document-Term Matrix (DTM) for ', fourgramCount, '-grams...'))
    fourgramsDtm <- DocumentTermMatrix(c1, control = list(tokenize = fourgramTokenizer))
}) 
message(paste('Fourgrams DTM built in', round(execTime["elapsed"], 2), "secs")) # ~6 mins with 10% sampling
fourgramsDtmSizeMB = round(object.size(fourgramsDtm)/1024/2014, 2)
message(paste('Fourgrams DTM size:', fourgramsDtmSizeMB, 'MB'))
message(paste0('Compute the ', fourgramCount, '-grams probabilities...'))
fourgramsFreq <- colSums(as.matrix(fourgramsDtm))
fourgramsTotalCount = colSums(as.data.frame(fourgramsFreq))
fourgramsPercent <- log(fourgramsFreq/fourgramsTotalCount)
fourgramsPercentSortDesc <- order(fourgramsPercent, decreasing=TRUE)
fourgramsPercentSortedDat <- as.data.frame(fourgramsPercent[fourgramsPercentSortDesc])
fourgramsPercentSortedDat <- add_rownames(fourgramsPercentSortedDat)
names(fourgramsPercentSortedDat) <- c('gram', 'prob')
message(paste('Fourgrams data frame size:', round(object.size(fourgramsPercentSortedDat)/1024/2014, 2), 'MB'))

# Quiz 2, NLP 1, with fourgrams
filter(filter(fourgramsPercentSortedDat, grepl('^a case of(\\s+)', gram)), grepl('pretzels|cheese|beer|soad', gram)) # Matches beer

filter(filter(fourgramsPercentSortedDat, grepl('^would mean the(\\s+)', gram)), grepl('most|universe|best|world', gram)) # Matches world

filter(filter(fourgramsPercentSortedDat, grepl('^make me the(\\s+)', gram)), grepl('happiest|bluest|saddest|smelliest', gram)) # Matches happiest

filter(filter(fourgramsPercentSortedDat, grepl('^struggling but the(\\s+)', gram)), grepl('referees|crowd|defense|players', gram)) # No match
filter(filter(trigramsPercentSortedDat, grepl('^but the(\\s+)', gram)), grepl('referees|crowd|defense|players', gram)) # Backoff: Matched crowd

filter(filter(fourgramsPercentSortedDat, grepl('^date at the(\\s+)', gram)), grepl('movies|mall|beach|grocery', gram)) # Matches grocery

filter(filter(fourgramsPercentSortedDat, grepl('^be on my(\\s+)', gram)), grepl('way|phone|motorcyle|horse', gram)) # Matches way

filter(filter(fourgramsPercentSortedDat, grepl('^in quite some(\\s+)', gram)), grepl('time|years|thing|weeks', gram)) # Matches time

filter(filter(fourgramsPercentSortedDat, grepl('^with his little(\\s+)', gram)), grepl('eyes|fingers|toes|ears', gram)) # No match
filter(filter(trigramsPercentSortedDat, grepl('^his little(\\s+)', gram)), grepl('eyes|fingers|toes|ears', gram)) # Backoff: No match
filter(filter(bigramsPercentSortedDat, grepl('^little(\\s+)', gram)), grepl('eyes|fingers|toes|ears', gram)) # Backoff: Matches ears, eyes, and fingers

filter(filter(fourgramsPercentSortedDat, grepl('^faith during the(\\s+)', gram)), grepl('(\\s)hard|(\\s)bad|(\\s)sad|(\\s)worse', gram)) # No match
filter(filter(trigramsPercentSortedDat, grepl('^during the(\\s+)', gram)), grepl('(\\s)hard|(\\s)bad|(\\s)sad|(\\s)worse', gram)) # Backoff: Matches hardest (Stemming would have worked)

filter(filter(fourgramsPercentSortedDat, grepl('^you must be(\\s+)', gram)), grepl('(\\s)asleep|(\\s)insane|(\\s)insensitive|(\\s)callous', gram)) # No match
filter(filter(trigramsPercentSortedDat, grepl('^must be(\\s+)', gram)), grepl('(\\s)asleep|(\\s)insane|(\\s)insensitive|(\\s)callous', gram)) # Backoff: No match
filter(filter(bigramsPercentSortedDat, grepl('^be(\\s+)', gram)), grepl('(\\s)asleep$|(\\s)insane$|(\\s)insensitive$|(\\s)callous$', gram)) # Backoff: Matches asleep

# Cleanup
c1 <- tm_map(c1, stripWhitespace)
c1 <- tm_map(c1, content_transformer(tolower))
#c1 <- tm_map(c1, removeNumbers)

# My punctuation remover. Keep the single quote "'" to keep "it's" as is instead of "its", which has a very different meaning.
replaceWithNothing <- content_transformer(function(x, pattern) {return (gsub(pattern, '', x))})
c1 <- tm_map(c1, replaceWithNothing, '[\".,;]')

# Write out the cleaned corpus to a file for inspection
outCnx = file(paste0(directory, '\\', 'sentences.txt'), open = "wt")
for (i in 1:length(c1)) {
    writeLines(c1[[i]]$content, outCnx)
}
close(outCnx)

# Build n-grams
# Unigrams
unigrams = c()
for (i in 1:length(c1)) {
    sentenceUnigrams = vapply(NLP::ngrams(unlist(strsplit(paste0('<s>', c1[[i]]$content, '</s>'), " ")), n = 1), paste, "", collapse = " ")
    unigrams = c(unigrams, sentenceUnigrams)
}
unigrams.freq = arrange(as.data.frame(xtabs(~unigrams)), desc(Freq))

# Bigrams
bigrams = c()
for (i in 1:length(c1)) {
    sentenceBigrams = vapply(NLP::ngrams(unlist(strsplit(paste0('<s>', c1[[i]]$content, '</s>'), " ")), n = 2), paste, "", collapse = " ")
    bigrams = c(bigrams, sentenceBigrams)
}

#bigrams1 = vapply(NLP::ngrams(unlist(strsplit(paste0('<s>', c1[[1]]$content, '</s>'), " ")), n = 2), paste, "", collapse = " ")
#bigrams = c(bigrams1, bigrams2)
bigrams.freq = arrange(as.data.frame(xtabs(~bigrams)), desc(Freq))

# Tri-grams

# Using quanteda
c2 <- corpus(c(line1, line2))
