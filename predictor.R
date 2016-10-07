# -----------------------------------------------------------------------------
# Function: predictWithStupidBackoff
#
# Description: Predict the next X "best" words given a sentence according to the
# "Stupid Backoff" algorithm. This is a facade to gramLookup, which does all the
# work.
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
predictWithStupidBackoff <- function(model, n = 5, sentence, matches_count_max = 10, lambda = 0.4, trace = FALSE) {
    
    if (trace) {
        message(paste('n:', n))
        message(paste('Input sentence:', sentence))
    }
    
    # Clean the input sentence
    sentenceCleaned = gsub('\'', '', sentence) # Remove quotes
    sentenceCleaned = stringr::str_replace_all(sentenceCleaned, '[^[:alpha:]]', ' ') # Keep alpha characters only
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

# -----------------------------------------------------------------------------
# Function: gramLookup
#
# Description: Recursive implementation of the Stupid Backoff algorithm.
#
# Arguments: See predictWithStupidBackoff.
#
# Side effects: None.
#
# Returns: The top matches_count_max grams, ranked by descending MLE.
# -----------------------------------------------------------------------------
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