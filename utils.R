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
