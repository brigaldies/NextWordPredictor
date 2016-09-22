# require(doParallel)
# 
# # Generic function for parallelizing any task (when possible)
# parallelizeTask <- function(task, ...) {
#     # Calculate the number of cores
#     ncores <- detectCores() - 1
#     # Initiate cluster
#     cl <- makeCluster(ncores)
#     registerDoParallel(cl)
#     #print("Starting task")
#     r <- task(...)
#     #print("Task done")
#     stopCluster(cl)
#     r
# }

# g_spacesRegex = '\\s+'
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
# Fast getFirstNTokens without defense coding, and with the use of the base 
# paste function.
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

getLastToken <- function(ngram) {
    tokens = unlist(strsplit(ngram, '\\s+'))
    tokensCount = length(tokens)
    tokens[tokensCount]
}

# -----------------------------------------------------------------------------
# Microbenchmarking shows that it is not faster than getFirstNTokens()
# -----------------------------------------------------------------------------
removeLastWord <- function(gram) {
    lastWord = NA
    len = nchar(gram)
    lastSpace = -1
    i = len
    while (i >= 1) {
        if (substr(gram, i, i) == ' ') {
            lastSpace = i
            # message(paste('Found space in position', i))
            break
        }
        i = i - 1
    }
    if (lastSpace == -1) {
        lastWord = gram
    } else {
        lastWord = substr(s1, 1, lastSpace - 1)
    }
    lastWord
}