predictNextBestWord <- function(model, sentence) {
    bestNextWord = NULL
    sortedPredictions = predictNextNBestWords(model, sentence, matches_count_max = 1)
    if (length(sortedPredictions) > 0) {
        bestNextWord = getLastNTokens(sortedPredictions[1], 1)
    }
    bestNextWord
}

predictNextNBestWords <- function(model, 
                                  type = 'MLE', 
                                  sentence, 
                                  clean_sentence = TRUE, 
                                  clean_type = 'TM',
                                  matches_count_max, 
                                  trace = FALSE) {
    if (trace) message(paste0('Predicting the next word for "', sentence, '"'))
    
    if (!(type %in% c('RFC', 'MLE', 'AKS'))) {
        stop(paste('Unknown model type', type))
    }
    
    answer = c()
    
    sentenceTest = sentence
    
    # Cleanup the input sentence via a TM corpus cleanup
    if (clean_sentence) {
        
        if (clean_type == 'TM') {
            sentenceCorpus = Corpus(VectorSource(sentence))
            sentenceCorpus <- tm_map(sentenceCorpus, removePunctuation)    
            sentenceCorpus <- tm_map(sentenceCorpus, removeNumbers)
            sentenceCorpus <- tm_map(sentenceCorpus, stripWhitespace)
            sentenceCorpus <- tm_map(sentenceCorpus, content_transformer(tolower))
            sentenceTest = sentenceCorpus[[1]]$content
        } else if (clean_type == 'GSUB') {
            # TODO: Have a fast cleanup mode with gsub operations only
            stop(paste('Clean type', clean_type, 'not implemented!'))
        } else {
            stop(paste('Unknown clean type', clean_type))
        }
    }
    
    if (trace) message(paste0('Cleaned sentence: "', sentenceTest, '"'))
    
    # Look up 4-grams
    gramTypeMatch = NULL
    gramLookup = getLastNTokens(sentenceTest, 3)
    if (trace) message(paste0('Looking up "', gramLookup, '"', ' in quagrams...'))
    quadgramsMatches = model$quadgrams[gramLookup]
    if (dim(quadgramsMatches)[1] > 0 & !is.na(quadgramsMatches[1, gram])) {
        gramTypeMatch = 4
        if (type == 'RFC')
            quadgramsMatches = quadgramsMatches[order(logfreq, decreasing = TRUE)]
        else if (type == 'MLE')
            quadgramsMatches = quadgramsMatches[order(logprob, decreasing = TRUE)]
        else if (type == 'AKS')
            quadgramsMatches = quadgramsMatches[order(logprobk, decreasing = TRUE)]
        answer = as.vector(head(quadgramsMatches, n = matches_count_max)[, gram])
    } else {
        # Back off to 3-grams
        gramLookup = getLastNTokens(sentenceTest, 2)
        if (trace) message(paste0('Looking up "', gramLookup, '"', ' in trigrams...'))
        trigramsMatches = model$trigrams[gramLookup]
        if (dim(trigramsMatches)[1] > 0 & !is.na(trigramsMatches[1, gram])) {
            gramTypeMatch = 3
            if (type == 'RFC')
                trigramsMatches = trigramsMatches[order(logfreq, decreasing = TRUE)]
            else if (type == 'MLE')
                trigramsMatches = trigramsMatches[order(logprob, decreasing = TRUE)]
            else if (type == 'AKS')
                trigramsMatches = trigramsMatches[order(logprobk, decreasing = TRUE)]
            answer = as.vector(head(trigramsMatches, n = matches_count_max)[, gram])
        } else {
            # back off to 2-grams
            gramLookup = getLastNTokens(sentenceTest, 1)
            if (trace) message(paste0('Looking up "', gramLookup, '"', ' in bigrams...'))
            bigramsMatches = model$bigrams[gramLookup]
            if (dim(bigramsMatches)[1] > 0 & !is.na(bigramsMatches[1, gram])) {
                gramTypeMatch = 2
                if (type == 'RFC')
                    bigramsMatches = bigramsMatches[order(logfreq, decreasing = TRUE)]
                else if (type == 'MLE')
                    bigramsMatches = bigramsMatches[order(logprob, decreasing = TRUE)]
                else if (type == 'AKS')
                    bigramsMatches = bigramsMatches[order(logprobk, decreasing = TRUE)]
                answer = as.vector(head(bigramsMatches, n = matches_count_max)[, gram])
            } else {
                if (trace) message('No match')
            }
        }   
    }
    
    if (length(answer) > 0) {
        answer = sapply(answer, function(x) { getLastToken(x)})
    }
    answer
}

# -----------------------------------------------------------------------------
# Function: predictWithStupidBackoff
#
# Description: Predict the next X "best" words given a sentence according to the
# "Stupid Backoff" algorithm described in [Insert URL]
#
# Arguments:
# model            : The N-gram model
# sentence         : The previous words in a sentence
# matches_count_max: Maximum number of candidate words to return
# lambda           : The "weight" in the stupid backoff algorithm, with a default value of 0.4 per the algorithm's author.
# trace            : TRUE or FALSE (default) to display messages during execution
#
# The matches_count_max or less best predictions.
# -----------------------------------------------------------------------------
predictWithStupidBackoff <- function(model, sentence, matches_count_max = 10, lambda = 0.4, trace = FALSE) {
    
    if (trace) message(paste('Input sentence:', sentence))
    
    # Clean the input sentence
    sentenceCleaned = gsub('\'', '', sentence) # Remove quotes
    sentenceCleaned = stringr::str_replace_all(sentence, '[^[:alpha:]]', ' ') # Keep alpha characters only
    sentenceCleaned = tolower(sentenceCleaned) # Convert to lower case
    
    if (trace) message(paste('Cleaned sentence:', sentenceCleaned))
    
    # Tokenize the sentence
    tokens = unlist(strsplit(sentenceCleaned, '\\s+'))
    tokensCount = length(tokens)
    
    # TODO: Handle cases when there are less than 3 words in the sentence
    
    lastThreeWords = paste(tokens[(tokensCount-2):tokensCount], collapse = ' ')
    lastTwoWords = paste(tokens[(tokensCount-1):tokensCount], collapse = ' ')
    lastWord = tokens[tokensCount]
    
    if (trace) {
        message(paste('lastThreeWords:', lastThreeWords))
        message(paste('lastTwoWords  :', lastTwoWords))
        message(paste('lastWord      :', lastWord))
    }
    
    matches = NULL
    matchesCount = 0
    
    # Look up the quadgrams for matches
    if (trace) message(paste('Look up the quadgrams for lower gram="', lastThreeWords, '"'))
    quadgramMatches = model$quadgrams[lastThreeWords][!is.na(gram)][,.(gram, logprob, last_word_in_gram)]
    quadgramMatchesCount = dim(quadgramMatches)[1]
    if (quadgramMatchesCount > 0) {
        quadgramMatches[, c('sprob') := { exp(logprob) }] # Calculate the weighted probability
        #print(quadgramMatches)
        matches = quadgramMatches
        matchesCount = quadgramMatchesCount
    }
    
    # Look up the trigams if the total number of matches has not been reached
    if (matchesCount < matches_count_max) {
        if (trace) message(paste('Look up the trigrams for lower gram="', lastTwoWords, '"'))
        trigramMatches = model$trigrams[lastTwoWords][!(last_word_in_gram %in% matches$last_word_in_gram)][!is.na(gram)][,.(gram, logprob, last_word_in_gram)]
        trigramMatchesCount = dim(trigramMatches)[1]
        if (trigramMatchesCount > 0) {
            trigramMatches[, c('sprob') := { lambda * exp(logprob) }] # Calculate the weighted probability
            #print(trigramMatches)
            additionalMatchesCount = min(matches_count_max - matchesCount, trigramMatchesCount)
            if (is.null(matches)) {
                matches = trigramMatches[1:additionalMatchesCount]
            } else {
                matches = rbind(matches, trigramMatches[1:additionalMatchesCount])
            }
            
            matchesCount = matchesCount + additionalMatchesCount
        }
        
        # Look up the bigrams if the total number of matches has not been reached
        if (matchesCount < matches_count_max) {
            if (trace) message(paste('Look up the bigrams for lower gram="', lastWord, '"'))
            bigramMatches = model$bigrams[lastWord][!(last_word_in_gram %in% matches$last_word_in_gram)][!is.na(gram)][,.(gram, logprob, last_word_in_gram)]
            bigramMatchesCount = dim(bigramMatches)[1]
            if (bigramMatchesCount > 0) {
                bigramMatches[, c('sprob') := { lambda * lambda * exp(logprob) }] # Calculate the weighted probability
                #print(bigramMatches)
                additionalMatchesCount = min(matches_count_max - matchesCount, bigramMatchesCount)
                if (is.null(matches)) {
                    matches = bigramMatches[1:additionalMatchesCount]
                } else {
                    matches = rbind(matches, bigramMatches[1:additionalMatchesCount])
                }
                matchesCount = matchesCount + additionalMatchesCount
            }
            
            if (matchesCount < matches_count_max) {
                if (trace) message(paste('Look up the top unigrams...'))
                unigramMatches = model$unigrams[!(gram %in% matches$last_word_in_gram)][1:matches_count_max - matchesCount][!is.na(gram)]
                unigramMatchesCount = dim(unigramMatches)[1]
                if (unigramMatchesCount > 0) {
                    unigramMatches[, c('sprob') := { lambda * lambda * lambda * exp(logprob) }] # Calculate the weighted probability
                    unigramMatches[, c('last_word_in_gram') := { gram }]
                    #print(unigramMatches)
                    additionalMatchesCount = unigramMatchesCount
                    if (is.null(matches)) {
                        matches = unigramMatches
                    } else {
                        matches = rbind(matches, bigramMatches)
                    }
                    matchesCount = matchesCount + additionalMatchesCount
                }
            }
        }
    }
    
    if (trace) message(paste('matchesCount=', matchesCount))
    if (!is.null(matches)) {
        matches = matches[1:min(matchesCount, matches_count_max)]
    }
    matches
}

# -----------------------------------------------------------------------------
# Function: predictWithStupidBackoff2
#
# Description: Predict the next X "best" words given a sentence according to the
# "Stupid Backoff" algorithm described in [Insert URL]. Improved version of
# predictWithStupidBackoff, using a recursive search that can start with any 
# number of words in the sentence.
#
# Arguments:
# model            : The N-gram model
# n                : The 'N' in the N-gram model, to start the algorithm at.
#                    Default value is 5, which is the highest gram order in the
#                    model. n must be 2 or higher.
# sentence         : The previous words in a sentence
# matches_count_max: Maximum number of candidate words to return
# lambda           : The "weight" in the stupid backoff algorithm, with a 
#                    default value of 0.4 per the algorithm's author.
# trace            : TRUE or FALSE (default) to display messages during 
#                    execution
#
# The matches_count_max best predictions.
# -----------------------------------------------------------------------------
predictWithStupidBackoff2 <- function(model, n = 5, sentence, matches_count_max = 10, lambda = 0.4, trace = FALSE) {
    
    if (trace) {
        message(paste('n:', n))
        message(paste('Input sentence:', sentence))
    }
    
    # Clean the input sentence
    sentenceCleaned = gsub('\'', '', sentence) # Remove quotes
    sentenceCleaned = stringr::str_replace_all(sentence, '[^[:alpha:]]', ' ') # Keep alpha characters only
    sentenceCleaned = tolower(sentenceCleaned) # Convert to lower case
    
    if (trace) message(paste0('Cleaned sentence: "', sentenceCleaned, '"'))
    
    # Tokenize the sentence
    tokens = unlist(strsplit(sentenceCleaned, '\\s+'))
    tokensCount = length(tokens)
    
    lookupWordsSequence = tokens[max(1, tokensCount - (n-2)):tokensCount] 
    if (trace) message(paste0('Lookup words sequence: "', paste(lookupWordsSequence, collapse = ' '), '"'))
    
    # Identify the N-gram model to start the prediction with, and start the recursive search.
    matches = NULL
    predictions = gramLookup(model, lookupWordsSequence, matches, matches_count_max, lambda, trace)
    predictions = predictions[order(sprob, decreasing = TRUE)][,.(last_word_in_gram, gram, sprob, logprob)]
    names(predictions) = c('Predicted Word', 'From Gram', 'Weighted Probability', 'MLE (Log)')
    predictions
}

gramLookup <- function(model, lookupWordsSequence, matchesThusFar, matches_count_max = 10, lambda = 0.4, trace = FALSE) {
    wordsCount = length(lookupWordsSequence)
    matchesThusFarCount = ifelse(is.null(matchesThusFar), 0, dim(matchesThusFar)[1])
    if (trace) {
        message(paste('matchesThusFarCount:', matchesThusFarCount))
        message(paste('wordsCount:', wordsCount))
        print(lookupWordsSequence)
    }
    if (wordsCount > 0) {
        # Bigrams and higher
        matches = model[[(wordsCount+1)]][paste(lookupWordsSequence, collapse = ' ')][!(last_word_in_gram %in% matchesThusFar$last_word_in_gram)][!is.na(gram)][,.(gram, logprob, last_word_in_gram)][order(logprob, decreasing = TRUE)]
    } else {
        # Unigrams
        matches = model$unigrams[!(gram %in% matchesThusFar$last_word_in_gram)][order(logprob, decreasing = TRUE)][1:(matches_count_max - matchesThusFarCount)][!is.na(gram)][,.(gram, logprob)]
    }
    if (trace) {
        message(paste('Matches:'))
        print(matches)
    }
    matchesCount = dim(matches)[1]
    if (matchesCount > 0) {
        matches[, c('sprob') := { (lambda ^ (4 - wordsCount)) * exp(logprob) }] # Calculate the weighted probability
        if (wordsCount == 0) {
            matches[, c('last_word_in_gram') := { gram }]
        }
        additionalMatchesCount = min(matches_count_max - matchesThusFarCount, matchesCount)
        if (is.null(matchesThusFar)) {
            matches = matches[1:additionalMatchesCount]
        } else {
            matches = rbind(matchesThusFar, matches[1:additionalMatchesCount])
        }
        matchesThusFarCount = matchesThusFarCount + additionalMatchesCount
    } else {
        matches = matchesThusFar
    }
    
    if (matchesThusFarCount < matches_count_max) {
        nextWordsSequence = c()
        if (wordsCount > 1) {
            nextWordsSequence = lookupWordsSequence[2:wordsCount]
        }
        matches = gramLookup(model, nextWordsSequence, matches, matches_count_max, lambda, trace)
    }
    
    matches
}