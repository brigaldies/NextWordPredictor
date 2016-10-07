buildNGram <- function(corpus, n) {
    ngram = NULL
    packageLowerCase = tolower(g_ngram_package)
    if (packageLowerCase == 'tm') {
        ngram = buildNGramTM(corpus, n)
    } else if (packageLowerCase == 'quanteda') {
        ngram = buildNGramQuanteda(corpus, n)
    } else {
        stop(paste('buildNGram: Unrecognized package name', package))
    }    
    ngram
}

# -----------------------------------------------------------------------------
# Function: buildNGramTMWithRFC 
# 
# Description: Builds an N-gram with Relative Frequency Count (RFC)-based probabilities
#
# Arguments:
# corpus  : The corpus to build the N-gram from.
# dtm     : The Document Term Matrix to build the N-gram from (As an alternative to the corpus).
# n       : The 'N' in the N-gram, or the N-gram order.
# minCount: Minimum gram frequency count. Any gram with a lower frequency is throw away.
#
# Returns: The built N-gram.
# -----------------------------------------------------------------------------
buildNGramTMWithRFC <- function(corpus, dtm, n, minCount = 1, parallel = TRUE) {
    ngramName = paste0(n, '-gram')
    message(paste('Building an Relative Frequency Count (RFC)', ngramName, 'with TM/RWeka...'))    
    
    # Arguments checks
    message(paste('Checking args...')) 
    if (n <= 0) {
        stop(paste('Invalid gram count', n))
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
    }
    
    ngramsDtmAsMatrix = as.matrix(ngramsDtm)
    ngramsCount = colSums(ngramsDtmAsMatrix) # ngram frequency count vector.
    message(paste(ngramName, 'count:', length(ngramsCount)))
    ngramsCount = ngramsCount[ngramsCount >= minCount] # Remove grams with frequency count < minCount
    message(paste(ngramName, 'count:', length(ngramsCount), 'after remove frequency counts <', minCount))
    ngramsUniqueCount = length(ngramsCount)
    message(paste('Compute the', ngramName, 'RFCs for', ngramsUniqueCount, 'grams'))    
    ngramsString = names(ngramsCount)
    ngramsTotalCount = sum(ngramsCount) # Total number of ngrams in the Corpus.
    ngramsLogProb = log(ngramsCount/ngramsTotalCount)
    
    ngramsLowerOrderString = c()
    if (!parallel) {
        # Without parallelization
        message(paste('Extracting the lowergrams without parallelization...'))    
        ngramsLowerOrderString = sapply(ngramsString, function(gram) { getFirstNTokensFast(gram, n-1)})    
    } else {
        message(paste('Extracting the lowergrams with parallelization...'))    
        # With parallelization with the SNOW package
        message(paste('Make an 8-wide cluster...'))
        cluster8 = makeCluster(8, 'SOCK')
        clusterExport(cluster8, list("getFirstNTokens")) # Make getFirstNTokens available to the worker threads.
        clusterEvalQ(cluster8, library(stringi)) # Make the package stringi available to the worker threads.
        message(paste('Make an 8-wide cluster done.'))
        message(paste('clusterApply getFirstNTokens...'))
        ngramsLowerOrderString = unlist(clusterApply(cluster8, ngramsString, function(gram) { getFirstNTokens(gram, n-1) }))
        message(paste('Stop the cluster...'))
        stopCluster(cluster8)
        remove(cluster8)
        message(paste('Stop the cluster done.'))
    }
    
    # Produce a data table
    ngramsDat = data.table(
        gram = ngramsString, 
        count = ngramsCount, 
        lowergram = ngramsLowerOrderString,
        logfreq = ngramsLogProb,
        key = "gram")    
    
    message(paste(ngramName, 'unique grams count:', dim(ngramsDat)[1]))
    message(paste(ngramName, 'data table size   :', round(object.size(ngramsDat)/1024/2014, 2), 'MB'))
    
    # Return the ngram data table
    ngramsDat
}

# -----------------------------------------------------------------------------
# Function: buildNGramWithMLE
#
# Description: Build an N-gram using MLE probabilities.
#
# Arguments:
# corpus         : The corpus to build the N-gram from.
# dtm            : The Document Term Matrix to build the N-gram from (As an alternative to the corpus).
# n              : The 'N' in the N-gram, or the N-gram order.
# lowerOrderNGram: The 'N-1'-gram, which is used in the MLE calculation.
# max_loop       : Max loop count (Used in testing).
#
# Returns: The N-gram with MLE probabilities.
# -----------------------------------------------------------------------------
buildNGramWithMLE <- function(corpus, dtm, n, lowerOrderNGram, max_loop) {
    ngramName = paste0(n, '-gram')
    ngramLowerOrderName = paste0(n-1, '-gram')
    message(paste('Building a', ngramName, 'with TM/RWeka...'))    
    
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
    }
    ngramsDtmAsMatrix = as.matrix(ngramsDtm)
    ngramsUniqueCount = dim(ngramsDtmAsMatrix)[2]
    message(paste('Compute the', ngramName, 'probabilities for', ngramsUniqueCount, 'grams'))    
    ngramsCount = colSums(ngramsDtmAsMatrix) # ngram occurrences count.
    ngramsString = names(ngramsCount)
    ngramsTotalCount = sum(ngramsCount) # Total number of ngrams in the Corpus.
    
    ngramLogProb = NULL
    if (n == 1) {
        # unigrams        
        ngramsLogProb = log(ngramsCount/ngramsTotalCount)
        
    } else {
        # 2+ grams        
        # Use the lower order NGram to compute the MLE probabilities
        
        key(lowerOrderNGram)
        
        ngramsListCount = length(ngramsCount)
        ngramsLowerString = rep(NA, ngramsListCount) # Pre-allocate the vector's size for performance.
        ngramsLogProb = rep(NA, ngramsListCount) # Pre-allocate the vector's size for performance.
        matchedLowerOrderGramCount = 0
        missingLowerOrderGramCount = 0
        missingLowerOrderGrams = c()
        loopCount = ngramsListCount
        if (!is.null(max_loop)) {
            loopCount = max_loop
        }
        message(paste('Loop count:', loopCount, ', starting on', Sys.time()))
        startSysTime = Sys.time() # Rough execution timer
        #loopCount = 100 # For Testing
        for (i in 1:loopCount) {
            #for (i in 1:100000) {
            if (i %% 100000 == 0) {
                endSysTime = Sys.time()
                message(paste('Processed', i, 'grams,', '(', round(i*100/ngramsListCount), '%) in', format(endSysTime - startSysTime),
                              'matchedLowerOrderGramCount =', matchedLowerOrderGramCount, 
                              '(', round(matchedLowerOrderGramCount*100/ngramsListCount), '%),',
                              'missingLowerOrderGramCount =', missingLowerOrderGramCount))
            }
            # Extract the lower-order (n-1)-gram in the n-gram:
            ngramString = as.character(ngramsString[i])
            ngramCount = as.integer(ngramsCount[i])                
            lowerOrderGramString = getFirstNTokens(ngramString, n-1)
            lowerOrderGramCount = 0
            
            # if (i == 1) {
            #     message(paste0("ngramString: '", ngramString, "', ngramCount: ",ngramCount))
            #     message(paste0("lowerOrderGramString: '", lowerOrderGramString, "'"))
            # }
            
            # Look up the lower-order (n-1)-gram in the lower order ngram data table:
            #lookupResult = lowerOrderNGram[gram == lowerOrderGramString]
            lookupResult = lowerOrderNGram[lowerOrderGramString] # data.table subsetting notation for a single-column keyed table
            if (dim(lookupResult)[1] == 0 | is.na(lookupResult[1, count])) {
                # No match found. It should be in the training set!?
                # message(paste('No match found for lower-order gram', lowerOrderGramString))
                missingLowerOrderGramCount = missingLowerOrderGramCount + 1
                missingLowerOrderGrams = c(missingLowerOrderGrams, lowerOrderGramString)
                # if (missingLowerOrderGramCount > 1000) {
                #     stop(paste('Reached', missingLowerOrderGramCount, 'missing lower-order ngram count.'))
                # }
            } else {
                # message(paste('Found lower-order gram', lowerOrderGramString, ', count', lookupResult[1, count]))
                matchedLowerOrderGramCount = matchedLowerOrderGramCount + 1
                lowerOrderGramCount = lookupResult[1, count]
            }
            
            if (lowerOrderGramCount > 0) {
                # ngramsLowerString = c(ngramsLowerString, lowerOrderGramString)
                ngramsLowerString[i] = lowerOrderGramString
                # ngramsLogProb = c(ngramsLogProb, log(ngramCount/lowerOrderGramCount))
                ngramsLogProb[i] = log(ngramCount/lowerOrderGramCount)
            } else {
                # ngramsLowerString = c(ngramsLowerString, NA)
                ngramsLowerString[i] = NA
                # ngramsLogProb = c(ngramsLogProb, NA)
                ngramsLogProb[i] = NA
            }
        }
        
        message(paste('Found', matchedLowerOrderGramCount, 'lower-order ngrams'))
        message(paste('Could not find', missingLowerOrderGramCount, 'lower-order ngrams'))
        if (missingLowerOrderGramCount > 0) {
            filePath = paste0(g_corpus_directory_en, '\\', 'missing-', ngramLowerOrderName, '.txt')
            outFile = file(filePath, open = "wt", encoding = "UTF-8")
            writeLines(text = sort(unique(tolower(missingLowerOrderGrams))), con = outFile)
            close(outFile)
        }
    }
    
    # Produce a data table
    startSysTime = Sys.time()
    ngramsDat = data.table(
        gram = ngramsString, 
        lowergram = ngramsLowerString, 
        count = ngramsCount, 
        logprob = ngramsLogProb,
        key = "gram")    
    endSysTime = Sys.time()
    message(paste('Data table created in', format(endSysTime - startSysTime)))
    
    message(paste(ngramName, 'unique grams count:', dim(ngramsDat)[1]))
    message(paste(ngramName, 'data frame size   :', round(object.size(ngramsDat)/1024/2014, 2), 'MB'))
    
    # Return the ngram data table
    ngramsDat
}

# -----------------------------------------------------------------------------
# Function: buildNGramWithMLE2
# (Improved version of buildNGramWithMLE with the addition of the 'minCount' argument)
#
# Description: Build an N-gram using MLE probabilities.
#
# Arguments:
# corpus         : The corpus to build the N-gram from.
# dtm            : The Document Term Matrix to build the N-gram from (As an alternative to the corpus).
# n              : The 'N' in the N-gram, or the N-gram order.
# lowerOrderNGram: The 'N-1'-gram, which is used in the MLE calculation.
# minCount       : Minimum gram frequency count. Any gram with a lower frequency is throw away.
# max_loop       : Max loop count (Used in testing).
#
# Returns: The N-gram with MLE probabilities.
# -----------------------------------------------------------------------------
buildNGramWithMLE2 <- function(corpus, dtm, n, lowerOrderNGram, minCount, max_loop) {
    ngramName = paste0(n, '-gram')
    ngramLowerOrderName = paste0(n-1, '-gram')
    message(paste('Building a', ngramName, 'with TM/RWeka...'))    
    
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
        
        key(lowerOrderNGram)
        
        ngramsListCount = length(ngramsCount)
        ngramsLowerString = rep(NA, ngramsListCount) # Pre-allocate the vector's size for performance.
        ngramsLogProb = rep(NA, ngramsListCount) # Pre-allocate the vector's size for performance.
        matchedLowerOrderGramCount = 0
        missingLowerOrderGramCount = 0
        missingLowerOrderGrams = c()
        loopCount = ngramsListCount
        if (!is.null(max_loop)) {
            loopCount = max_loop
        }
        message(paste('Loop count:', loopCount, ', starting on', Sys.time()))
        startSysTime = Sys.time() # Rough execution timer
        #loopCount = 100 # For Testing
        for (i in 1:loopCount) {
            #for (i in 1:100000) {
            if (i %% 100000 == 0) {
                endSysTime = Sys.time()
                message(paste('Processed', i, 'grams,', '(', round(i*100/ngramsListCount), '%) in', format(endSysTime - startSysTime),
                              'matchedLowerOrderGramCount =', matchedLowerOrderGramCount, 
                              '(', round(matchedLowerOrderGramCount*100/ngramsListCount), '%),',
                              'missingLowerOrderGramCount =', missingLowerOrderGramCount))
            }
            # Extract the lower-order (n-1)-gram in the n-gram:
            ngramString = as.character(ngramsString[i])
            ngramCount = as.integer(ngramsCount[i])                
            lowerOrderGramString = getFirstNTokens(ngramString, n-1)
            lowerOrderGramCount = 0
            
            # if (i == 1) {
            #     message(paste0("ngramString: '", ngramString, "', ngramCount: ",ngramCount))
            #     message(paste0("lowerOrderGramString: '", lowerOrderGramString, "'"))
            # }
            
            # Look up the lower-order (n-1)-gram in the lower order ngram data table:
            #lookupResult = lowerOrderNGram[gram == lowerOrderGramString]
            lookupResult = lowerOrderNGram[lowerOrderGramString] # data.table subsetting notation for a single-column keyed table
            if (dim(lookupResult)[1] == 0 | is.na(lookupResult[1, count])) {
                # No match found. It should be in the training set!?
                # message(paste('No match found for lower-order gram', lowerOrderGramString))
                missingLowerOrderGramCount = missingLowerOrderGramCount + 1
                missingLowerOrderGrams = c(missingLowerOrderGrams, lowerOrderGramString)
                # if (missingLowerOrderGramCount > 1000) {
                #     stop(paste('Reached', missingLowerOrderGramCount, 'missing lower-order ngram count.'))
                # }
            } else {
                # message(paste('Found lower-order gram', lowerOrderGramString, ', count', lookupResult[1, count]))
                matchedLowerOrderGramCount = matchedLowerOrderGramCount + 1
                lowerOrderGramCount = lookupResult[1, count]
            }
            
            if (lowerOrderGramCount > 0) {
                # ngramsLowerString = c(ngramsLowerString, lowerOrderGramString)
                ngramsLowerString[i] = lowerOrderGramString
                # ngramsLogProb = c(ngramsLogProb, log(ngramCount/lowerOrderGramCount))
                ngramsLogProb[i] = log(ngramCount/lowerOrderGramCount)
            } else {
                # ngramsLowerString = c(ngramsLowerString, NA)
                ngramsLowerString[i] = NA
                # ngramsLogProb = c(ngramsLogProb, NA)
                ngramsLogProb[i] = NA
            }
        }
        
        message(paste('Found', matchedLowerOrderGramCount, 'lower-order ngrams'))
        message(paste('Could not find', missingLowerOrderGramCount, 'lower-order ngrams'))
        if (missingLowerOrderGramCount > 0) {
            filePath = paste0(g_corpus_directory_en, '\\', 'missing-', ngramLowerOrderName, '.txt')
            outFile = file(filePath, open = "wt", encoding = "UTF-8")
            writeLines(text = sort(unique(tolower(missingLowerOrderGrams))), con = outFile)
            close(outFile)
        }
    }
    
    # Produce a data table
    startSysTime = Sys.time()
    ngramsDat = data.table(
        gram = ngramsString, 
        lowergram = ngramsLowerString, 
        count = ngramsCount, 
        logprob = ngramsLogProb,
        key = "gram")    
    endSysTime = Sys.time()
    message(paste('Data table created in', format(endSysTime - startSysTime)))
    
    message(paste(ngramName, 'unique grams count:', dim(ngramsDat)[1]))
    message(paste(ngramName, 'data table size   :', round(object.size(ngramsDat)/1024/2014, 2), 'MB'))
    
    # Return the ngram data table
    ngramsDat
}