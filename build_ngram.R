require(tm)

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

buildNGramTM <- function(corpus, n) {
    require(RWeka)
    ngramName = paste0(n, '-gram')
    message(paste('Building a', ngramName, 'with TM/RWeka...'))    
    
    # Get the tokenizer
    tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = n, max = n))
    
    execTime <- system.time({
        ngramDtm <- DocumentTermMatrix(corpus, control = list(tokenize = tokenizer))
    })
    message(paste(ngramName, 'DTM built in', round(execTime["elapsed"], 2), "secs"))
    message(paste(ngramName, 'DTM size:', round(object.size(ngramDtm)/1024/2014, 2), 'MB'))
    message(paste('Compute the', ngramName, 'probabilities...'))
    ngramFreq <- colSums(as.matrix(ngramDtm))
    ngramTotalCount = sum(ngramFreq)
    ngramLogPercent <- log(ngramFreq/ngramTotalCount)
    ngramLogPercentSortDesc <- order(ngramLogPercent, decreasing=TRUE)
    ngramLogPercentSortedDat <- as.data.frame(ngramLogPercent[ngramLogPercentSortDesc])
    ngramLogPercentSortedDat <- add_rownames(ngramLogPercentSortedDat)
    names(ngramLogPercentSortedDat) <- c('gram', 'prob')
    
    remove(ngramDtm)
    remove(ngramFreq)
    remove(ngramLogPercent)
    remove(ngramLogPercentSortDesc)
        
    message(paste(ngramName, 'data frame size:', round(object.size(ngramLogPercentSortedDat)/1024/2014, 2), 'MB'))
    ngramLogPercentSortedDat
}

buildNGramTM2 <- function(corpus, dtm, n, lowerOrderNGram, max_loop) {
    require(RWeka)
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
    if (is.na(dtm)) {
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

g_spacesRegex = '\\s+'
getFirstNTokens <- function(ngram_string, tokens_count_to_retrieve) {
    if (tokens_count_to_retrieve <= 0) {
        stop(paste('Invalid tokens count to retrieve', tokens_count_to_retrieve))
    }
    tokens = unlist(strsplit(ngram_string, g_spacesRegex))
    tokensCount = length(tokens)
    if (tokens_count_to_retrieve > tokensCount) {
        stop(paste('Invalid tokens count', tokens_count_to_retrieve))
    }    
    ntokens = tokens[1:tokens_count_to_retrieve]
    stri_paste(ntokens, collapse = ' ')
}

getLastNTokens <- function(ngram, tokens_count_to_retrieve) {
    if (tokens_count_to_retrieve <= 0) {
        stop(paste('Invalid tokens count to retrieve', tokens_count_to_retrieve))
    }
    tokens = unlist(strsplit(ngram, g_spacesRegex))
    tokensCount = length(tokens)
    if (tokens_count_to_retrieve > tokensCount) {
        stop(paste('Invalid tokens count', tokens_count_to_retrieve))
    }    
    ntokens = tokens[max(tokensCount -(tokens_count_to_retrieve-1),1):tokensCount]
    stri_paste(ntokens, collapse = ' ')
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
        key = "gram")
}

objectSizeMB <- function(obj) {
    object.size(obj)/1024/1024
}