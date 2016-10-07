# -----------------------------------------------------------------------------
# Function: getFirstNTokens
#
# Description: Retrieve the first N words, or tokens, in a string.
#
# Arguments:
# ngram_string            : String of words.
# tokens_count_to_retrieve: Number of words to retrieve.
#
# Side effects: None.
#
# Returns:  The N words are returned "pasted", with a space as separator between 
# words.
# -----------------------------------------------------------------------------
getFirstNTokens <- function(ngram_string, tokens_count_to_retrieve) {
    if (tokens_count_to_retrieve <= 0) {
        stop(paste('Invalid tokens count to retrieve', tokens_count_to_retrieve))
    }
    tokens = unlist(strsplit(ngram_string, '\\s+'))
    tokensCount = length(tokens)
    if (tokens_count_to_retrieve > tokensCount) {
        stop(paste('Invalid tokens count', tokens_count_to_retrieve))
    }    
    ntokens = tokens[1:tokens_count_to_retrieve]
    stri_paste(ntokens, collapse = ' ')
}

# -----------------------------------------------------------------------------
# Function: getFirstNTokensFast
# 
# Description: Fast getFirstNTokens without defense coding, and with the use of
# the base paste function.
#
# Arguments: See getFirstNTokens.
#
# Side effects: None.
#
# Returns: See getFirstNTokens.
# -----------------------------------------------------------------------------
getFirstNTokensFast <- function(ngram_string, tokens_count_to_retrieve) {
    paste(unlist(strsplit(ngram_string, '\\s+'))[1:tokens_count_to_retrieve], collapse = ' ')
}

getLastNTokens <- function(ngram, tokens_count_to_retrieve) {
    if (tokens_count_to_retrieve <= 0) {
        stop(paste('Invalid tokens count to retrieve', tokens_count_to_retrieve))
    }
    tokens = unlist(strsplit(ngram, '\\s+'))
    tokensCount = length(tokens)
    if (tokens_count_to_retrieve > tokensCount) {
        stop(paste('Invalid tokens count', tokens_count_to_retrieve))
    }    
    ntokens = tokens[max(tokensCount -(tokens_count_to_retrieve-1),1):tokensCount]
    stri_paste(ntokens, collapse = ' ')
}


# -----------------------------------------------------------------------------
# Function: getLastToken
#
# Description: Retrieve the last word (token) in a string.
#
# Arguments:
# ngram: String of words.
#
# Side effects: None.
#
# Returns: The first word in the input string.
# -----------------------------------------------------------------------------
getLastToken <- function(ngram) {
    tokens = unlist(strsplit(ngram, '\\s+'))
    tokensCount = length(tokens)
    tokens[tokensCount]
}