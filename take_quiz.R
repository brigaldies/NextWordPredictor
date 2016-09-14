answerQuiz <- function(sentence, choices) {
    answer = NULL
    choicesRegex = stri_paste(paste0(paste0('(\\s)', choices), '$'), collapse = '|')
    
    message(paste0('Predicting the next word for "', sentence, '"'))
    
    # Cleanup the input sentence via a TM corpus cleanup
    sentenceCorpus = Corpus(VectorSource(sentence))
    sentenceCorpus = cleanCorpus(sentenceCorpus)
    sentenceTest = sentenceCorpus[[1]]$content
    
    message(paste0('Cleaned sentence: "', sentenceTest, '"'))
    
    # Look up 4-grams
    gramTypeMatch = NULL
    gramLookup = getLastNTokens(sentenceTest, 3)
    message(paste0('Looking up "', gramLookup, '"'))
    quadgramsMatches = quadgramsDat3[gramLookup][grepl(choicesRegex, gram)][order(logprob, decreasing = TRUE)]
    if (dim(quadgramsMatches)[1] > 0 & !is.na(quadgramsMatches[1, gram])) {
        gramTypeMatch = 4
        answer = quadgramsMatches[1, gram]
    } else {
        # Back off to 3-grams
        gramLookup = getLastNTokens(sentenceTest, 2)
        message(paste0('Looking up "', gramLookup, '"'))
        trigramsMatches = trigramsDat3[gramLookup][grepl(choicesRegex, gram)][order(logprob, decreasing = TRUE)]
        if (dim(trigramsMatches)[1] > 0 & !is.na(trigramsMatches[1, gram])) {
            gramTypeMatch = 3
            answer = trigramsMatches[1, gram]
        } else {
            # back off to 2-grams
            gramLookup = getLastNTokens(sentenceTest, 1)
            message(paste0('Looking up "', gramLookup, '"'))
            bigramsMatches = bigramsDat3[gramLookup][grepl(choicesRegex, gram)][order(logprob, decreasing = TRUE)]
            if (dim(bigramsMatches)[1] > 0 & !is.na(bigramsMatches[1, gram])) {
                gramTypeMatch = 2
                answer = bigramsMatches[1, gram]
            }
        }   
    }
    
    if (!is.null(answer)) {
        message(paste0('Matched ', gramTypeMatch, '-gram "', answer, '"'))
        getLastNTokens(answer, 1)
    } else {
        NULL
    }
}
