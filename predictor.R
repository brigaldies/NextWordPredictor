predictNextBestWord <- function(model, sentence) {
    bestNextWord = NULL
    sortedPredictions = predictNextNBestWords(model, sentence, matches_count_max = 1)
    if (length(sortedPredictions) > 0) {
        bestNextWord = getLastNTokens(sortedPredictions[1], 1)
    }
    bestNextWord
}

predictNextNBestWords <- function(model, sentence, matches_count_max, trace = FALSE) {
    if (trace) message(paste0('Predicting the next word for "', sentence, '"'))
    
    answer = c()
    
    # Cleanup the input sentence via a TM corpus cleanup
    sentenceCorpus = Corpus(VectorSource(sentence))
    sentenceCorpus <- tm_map(sentenceCorpus, removePunctuation)    
    sentenceCorpus <- tm_map(sentenceCorpus, removeNumbers)
    sentenceCorpus <- tm_map(sentenceCorpus, stripWhitespace)
    sentenceCorpus <- tm_map(sentenceCorpus, content_transformer(tolower))
    
    sentenceTest = sentenceCorpus[[1]]$content
    
    if (trace) message(paste0('Cleaned sentence: "', sentenceTest, '"'))
    
    # Look up 4-grams
    gramTypeMatch = NULL
    gramLookup = getLastNTokens(sentenceTest, 3)
    if (trace) message(paste0('Looking up "', gramLookup, '"', ' in quagrams...'))
    quadgramsMatches = model$quadgrams[gramLookup][order(logprob, decreasing = TRUE)]
    if (dim(quadgramsMatches)[1] > 0 & !is.na(quadgramsMatches[1, gram])) {
        gramTypeMatch = 4
        answer = as.vector(head(quadgramsMatches, n = matches_count_max)[, gram])
    } else {
        # Back off to 3-grams
        gramLookup = getLastNTokens(sentenceTest, 2)
        if (trace) message(paste0('Looking up "', gramLookup, '"', ' in trigrams...'))
        trigramsMatches = model$trigrams[gramLookup][order(logprob, decreasing = TRUE)]
        if (dim(trigramsMatches)[1] > 0 & !is.na(trigramsMatches[1, gram])) {
            gramTypeMatch = 3
            answer = as.vector(head(trigramsMatches, n = matches_count_max)[, gram])
        } else {
            # back off to 2-grams
            gramLookup = getLastNTokens(sentenceTest, 1)
            if (trace) message(paste0('Looking up "', gramLookup, '"', ' in bigrams...'))
            bigramsMatches = model$bigrams[gramLookup][order(logprob, decreasing = TRUE)]
            if (dim(bigramsMatches)[1] > 0 & !is.na(bigramsMatches[1, gram])) {
                gramTypeMatch = 2
                answer = as.vector(head(bigramsMatches, n = matches_count_max)[, gram])
            } else {
                if (trace) message('No match')
            }
        }   
    }
    
    if (length(answer) > 0) {
        answer = sapply(answer, function(x) { getLastNTokens(x, 1)})
    }
    answer
}