```{r}
# -----------------------------------------------------------------------------
# Function: buildNGramWithMLE
#
# Description: Build an N-gram using MLE probabilities.
#
# Arguments:
# corpus         : The corpus to build the N-gram from.
# dtm            : The Document Term Matrix to build the N-gram from (As an alternative to the corpus).
# n              : The 'N' in the N-gram, or the N-gram order.
# k              : Smoothing fractional count (0 by default)
# lowerOrderNGram: The 'N-1'-gram, which is used in the MLE calculation.
# minCount       : Minimum gram frequency count. Any gram with a lower frequency is throw away.
#
# Returns: The N-gram with MLE probabilities.
# -----------------------------------------------------------------------------
buildNGramWithMLE3 <- function(corpus, dtm, n, k = 0, lowerOrderNGram, minCount = 1) {
    ngramName = paste0(n, '-gram')
    ngramLowerOrderName = paste0(n-1, '-gram')
    message(paste('Building a', ngramName, 'with TM/RWeka, starting on', Sys.time()))
    
    # Arguments checks
    message(paste('Checking args...')) 
    if (n <= 0) {
        stop(paste('Invalid gram count', n))
    } else if (n >= 2 & is.null(lowerOrderNGram)) {
        stop(paste('Missing lower-order ngram argument with n =', n))
    }
    
    # Get the tokenizer
    ngramsDtm = dtm
    if (is.null(dtm)) {
        message(paste('Get a', ngramName, 'tokenizer...'))
        tokenizer = function(x) NGramTokenizer(x, Weka_control(min = n, max = n))
        
        message(paste('Compute the DTM...'))
        execTime = system.time({
            ngramsDtm = DocumentTermMatrix(corpus, control = list(tokenize = tokenizer))
        })
        message(paste(ngramName, 'DTM built in', round(execTime["elapsed"], 2), "secs"))
        message(paste(ngramName, 'DTM size:', round(object.size(ngramsDtm)/1024/2014, 2), 'MB'))
    } else {
        message(paste('Using the provided DTM.'))
    }
    ngramsDtmAsMatrix = as.matrix(ngramsDtm)
    ngramsCount = colSums(ngramsDtmAsMatrix) # ngram frequency count vector.
    message(paste(ngramName, 'count:', length(ngramsCount)))
    ngramsCount = ngramsCount[ngramsCount >= minCount] # Remove grams with frequency count < minCount
    message(paste(ngramName, 'count:', length(ngramsCount), 'after remove frequency counts <', minCount))
    ngramsUniqueCount = length(ngramsCount)
    message(paste('Compute the', ngramName, 'probabilities for', ngramsUniqueCount, 'grams'))    
    ngramsString = names(ngramsCount)
    ngramsTotalCount = sum(ngramsCount) # Total number of ngrams in the Corpus.
    
    ngramLogProb = NULL
    if (n == 1) {
        # unigrams        
        ngramsLogProb = log(ngramsCount/ngramsTotalCount)
        
    } else {
        # 2+ grams        
        # Use the lower order NGram to compute the MLE probabilities
        
        # Construct the N-grams' (N-1)-grams
        message(paste0('Construct the ', n-1, '-grams...'))
        ngramsLowerOrderString = NA
        execTime = system.time({
            ngramsLowerOrderString = sapply(ngramsString, function(gram) { getFirstNTokens(gram, n-1)})    
        })
        execTimeSeconds = execTime["elapsed"]
        execTimeMins = execTimeSeconds/60
        execTimeSecsAfterMins = execTimeSeconds %% 60
        message(paste('Done in', round(execTimeMins, 2), 'mins, ', round(execTimeSecsAfterMins, 2), 'secs'))
        
        # Construct the N-grams' last words
        message(paste0('Construct the last words...'))
        ngramsLastWord = NA
        execTime = system.time({
            # Without parallelization
            ngramsLastWord = sapply(ngramsString, function(gram) { getLastToken(gram)})    
        })
        execTimeSeconds = execTime["elapsed"]
        execTimeMins = execTimeSeconds/60
        execTimeSecsAfterMins = execTimeSeconds %% 60
        message(paste('Done in', round(execTimeMins, 2), 'mins, ', round(execTimeSecsAfterMins, 2), 'secs'))
        
        # Retrieve the N-grams' (N-1)-gram counts (Could be NA)
        message(paste0('Retrieve the ', n-1, '-grams counts...'))
        ngramsLowerOrderCount = NA
        execTime = system.time({
            ngramsLowerOrderCount = base::merge(x = data.frame(gram = ngramsLowerOrderString),
                                                y = data.frame(gram = lowerOrderNGram$gram, count = lowerOrderNGram$count),
                                                all.x = TRUE)$count
        })
        execTimeSeconds = execTime["elapsed"]
        execTimeMins = execTimeSeconds/60
        execTimeSecsAfterMins = execTimeSeconds %% 60
        message(paste('Done in', round(execTimeMins, 2), 'mins, ', round(execTimeSecsAfterMins, 2), 'secs'))

        # Compute the N-grams' MLE. vectorized calculation is very fast without parallelization.
        message(paste0('Compute the ', n, '-grams MLE...'))
        execTime = system.time({
            # MLE
            ngramsLogProb = log((ngramsCount + k)/(ngramsLowerOrderCount + k*ngramsTotalCount))
            # Simple percent use
            ngramsLogPercent = log(ngramsCount/ngramsTotalCount)
        })
        execTimeSeconds = execTime["elapsed"]
        execTimeMins = execTimeSeconds/60
        execTimeSecsAfterMins = execTimeSeconds %% 60
        message(paste('Done in', round(execTimeMins, 2), 'mins, ', round(execTimeSecsAfterMins, 2), 'secs'))
    }
    
    # Produce a data table
    message(paste('Build the data table...'))
    ngramsDat = data.table(
        gram = ngramsString, 
        count = ngramsCount,
        lowergram = ngramsLowerOrderString, 
        lowercount =  ngramsLowerOrderCount,
        last_word_in_gram = ngramsLastWord,
        logprob = ngramsLogProb,
        logpercent = ngramsLogPercent,
        key = "gram")    

    message(paste(ngramName, 'unique grams count:', dim(ngramsDat)[1]))
    message(paste(ngramName, 'data table size   :', round(object.size(ngramsDat)/1024/2014, 2), 'MB'))
    
    # Return the ngram data table
    ngramsDat
}

buildNGramQuanteda <- function(corpus, n) {
    require(quanteda)
    message(paste0('Building a', n, '-gram with Quanteda...'))   
    if (n >= 2) {
        stop('buildNGramQuanteda: Not implemented for n = 2 and higher!')
    }
    cq = corpus
    if (class(corpus)[1] == 'VCorpus') {
        cq = quanteda::corpus(corpus)
    }
    
    dfmMatrix = dfm(
        cq, 
        what = 'word',
        ngrams = 1,
        simplify = TRUE
    )
    
    gramsString = features(dfmMatrix)
    gramsCount = colSums(dfmMatrix)
    gramsTotalCount = sum(gramsCount)
    gramsLogProb = log(gramsCount/gramsTotalCount)
    
    data.table(
        gram = gramsString, 
        count = gramsCount, 
        logprob = gramsLogProb,
        logpercent = gramsLogProb, # Same as logprob for unigrams
        key = "gram")
}
```
