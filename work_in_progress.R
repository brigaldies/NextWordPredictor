sampleRate = 10
corpusFilesPattern = paste0('^en_US(.)*sample_', sprintf('%.1f', sampleRate), '$')
c1 = loadCorpus(directory = g_corpus_directory_en, files_pattern = corpusFilesPattern)
c1 = cleanCorpus(c1)
saveRDS(object = c1, file = paste0(g_corpus_directory_en, '\\corpus_cleaned_sample_', sprintf('%.1f', sampleRate), '.rds'))
c1 = readRDS(file = paste0(g_corpus_directory_en, '\\corpus_cleaned_sample_', sprintf('%.1f', sampleRate), '.rds'))

# c1 = parallelizeTask(cleanCorpus, c1) # No gain!

# Write out the corpus back out to a file after cleanup to inspect
outPath = paste0(g_corpus_directory_en, '\\', 'cleaned_corpus_sample_', sprintf('%.1f', sampleRate), '.txt')
outFile = file(outPath, open = "wt", encoding = "UTF-8")
docsCount = 3
for (i in 1:docsCount) {
    doc = c1[[i]]$content
    writeLines(paste('*** Document', i, '***'), con = outFile)
    linesCount = length(doc)
    for (j in 1:linesCount) {
        writeLines(doc[j], con = outFile)
    }
}
close(outFile)
# Re-load from the cleanup corpus
c1 = loadCorpus(directory = g_corpus_directory_en, files_pattern = 'cleaned_corpus_sample_10.0.txt')

tokenizer1 = function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
#tokenizer1 = function(x) WordTokenizer(x, Weka_control(min = 1, max = 1))
#tokenizer1 = function(x) AlphabeticalTokenizer(x)
#c1 = cTest
execTime <- system.time({
    unigramsDtm = DocumentTermMatrix(c1, control = list(tokenize = tokenizer1))
    unigramsCount = colSums(as.matrix(unigramsDtm))
    unigramsString = names(unigramsCount)
    #unigramsDat = data.frame(gram = unigramsString, count = unigramsCount)
    unigramsDat = data.table(gram = unigramsString, count = unigramsCount, key = 'gram')
    #unigramsDat = unigramsDat[order(count, decreasing = TRUE)]
    head(unigramsDat)
})
message(paste('Unigrams model built in', round(execTime["elapsed"], 2), "secs"))

# 
tokenizer2 = function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
# c1 = cTest
bigramsDtm = DocumentTermMatrix(c1, control = list(tokenize = tokenizer2))
bigramsCount = colSums(as.matrix(bigramsDtm))
bigramsString = names(bigramsCount)
bigramsDat = data.table(gram = bigramsString, count = bigramsCount, key = "gram")
head(bigramsDat)

execTime <- system.time({
    gramsDtm = DocumentTermMatrix(c1, control = list(tokenize = tokenizer2))
    gramsCount = colSums(as.matrix(gramsDtm))
    gramsString = names(gramsCount)
    #unigramsDat = data.frame(gram = unigramsString, count = unigramsCount)
    gramsDat = data.table(gram = gramsString, count = gramsCount, key = 'gram')
    #unigramsDat = unigramsDat[order(count, decreasing = TRUE)]
    head(gramsDat)
})
message(paste('1+2-grams model built in', round(execTime["elapsed"], 2), "secs"))

# ----------------------- WORK IN PROGRESS (BEGIN) ----------------------------
# Build unigrams with Quanteda (Issue with RWeka unigram tokenizer: It removes stop words!)
execTime = system.time({
    unigramsDat = buildNGramQuanteda(c1, 1)
    saveRDS(object = unigramsDat, file = paste0(g_corpus_directory_en, '\\unigrams_sample_', sprintf('%.1f', sampleRate), '.rds'))
})
execTimeSeconds = execTime["elapsed"]
execTimeMins = execTimeSeconds/60
execTimeSecsAfterMins = execTimeSeconds %% 60
message(paste('Unigrams model built in', round(execTimeMins), 'mins, ', round(execTimeSecsAfterMins, 2), 'secs'))
# Re-load the unigrams
unigramsDat = readRDS(file = paste0(g_corpus_directory_en, '\\unigrams_sample_', sprintf('%.1f', sampleRate), '.rds'))

# Build bigrams with TM/RWeka (Issue with Quanteda: The 2+ grams don't complete!?)
execTime = system.time({
    bigramsDat = buildNGramTM2(c1, 2, unigramsDat, max_loop = NULL)
    bigramsDat2 = bigramsDat[!is.na(logprob)]
    saveRDS(object = bigramsDat2, file = paste0(g_corpus_directory_en, '\\bigrams_sample_', sprintf('%.1f', sampleRate), '.rds'))
})
execTimeSeconds = execTime["elapsed"]
execTimeMins = execTimeSeconds/60
execTimeSecsAfterMins = execTimeSeconds %% 60
message(paste('Bigrams model built in', round(execTimeMins), 'mins, ', round(execTimeSecsAfterMins, 2), 'secs'))
bigramsDat2 = readRDS(file = paste0(g_corpus_directory_en, '\\bigrams_sample_', sprintf('%.1f', sampleRate), '.rds'))

# Build trigrams with TM/RWeka (Issue with Quanteda: The 2+ grams don't complete!?)
execTime = system.time({
    trigramsDat = buildNGramTM2(c1, 3, bigramsDat2, max_loop = NULL)
    trigramsDat2 = trigramsDat[!is.na(logprob)]
    saveRDS(object = trigramsDat2, file = paste0(g_corpus_directory_en, '\\trigrams_sample_', sprintf('%.1f', sampleRate), '.rds'))
})
execTimeSeconds = execTime["elapsed"]
execTimeMins = execTimeSeconds/60
execTimeSecsAfterMins = execTimeSeconds %% 60
message(paste('Trigrams model built in', round(execTimeMins, 2), 'mins, ', round(execTimeSecsAfterMins, 2), 'secs'))
trigramsDat2 = readRDS(file = paste0(g_corpus_directory_en, '\\trigrams_sample_', sprintf('%.1f', sampleRate), '.rds'))
trigramsDat3 = trigramsDat2
setkey(trigramsDat3, 'lowergram')
key(trigramsDat3)

# Quiz #2 with the 3-gram: 5/10
trigramsDat2[grepl('^case of(\\s+)', gram)][grepl('(\\s)pretzels$|(\\s)cheese$|(\\s)beer$|(\\s)soda$', gram)][order(logprob, decreasing = TRUE)] # No match
trigramsDat2[grepl('^mean the(\\s+)', gram)][grepl('(\\s)most$|(\\s)universe$|(\\s)best$|(\\s)world$', gram)][order(logprob, decreasing = TRUE)] # Match
trigramsDat2[grepl('^me the(\\s+)', gram)][grepl('(\\s)happiest$|(\\s)bluest$|(\\s)saddest$|(\\s)smelliest$', gram)][order(logprob, decreasing = TRUE)] # Match
trigramsDat2[grepl('^but the(\\s+)', gram)][grepl('(\\s)referees$|(\\s)crowd$|(\\s)defense$|(\\s)players$', gram)][order(logprob, decreasing = TRUE)] # No match
trigramsDat2[grepl('^at the(\\s+)', gram)][grepl('(\\s)movies$|(\\s)mall$|(\\s)beach$|(\\s)grocery$', gram)][order(logprob, decreasing = TRUE)] # Match
trigramsDat2[grepl('^on my(\\s+)', gram)][grepl('(\\s)way$|(\\s)phone$|(\\s)motorcyle$|(\\s)horse$', gram)][order(logprob, decreasing = TRUE)] # Match
trigramsDat2[grepl('^quite some(\\s+)', gram)][grepl('(\\s)time$|(\\s)years$|(\\s)thing$|(\\s)weeks$', gram)][order(logprob, decreasing = TRUE)] # Match
trigramsDat2[grepl('^his little(\\s+)', gram)][grepl('eyes|fingers|toes|ears', gram)][order(logprob, decreasing = TRUE)] # No match
trigramsDat2[grepl('^during the(\\s+)', gram)][grepl('(\\s)hard|(\\s)bad|(\\s)sad|(\\s)worse', gram)][order(logprob, decreasing = TRUE)] # No match
trigramsDat2[grepl('^must be(\\s+)', gram)][grepl('(\\s)asleep$|(\\s)insane$|(\\s)insensitive$|(\\s)callous$', gram)][order(logprob, decreasing = TRUE)] # No match

# Build quadgrams with TM/RWeka (Issue with Quanteda: The 2+ grams don't complete!?)
# Build the TDM
ngramName = paste0(4, '-gram')
tokenizer4 = function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
message(paste('Compute the DTM...'))
execTime = system.time({
    quadgramsDtm = DocumentTermMatrix(c1, control = list(tokenize = tokenizer4))
    saveRDS(object = quadgramsDtm, file = paste0(g_corpus_directory_en, '\\quadgrams_dtm_sample_', sprintf('%.1f', sampleRate), '.rds'))
})
message(paste(ngramName, 'DTM built in', round(execTime["elapsed"], 2), "secs"))

# Build the quadgrams
execTime = system.time({
    quadgramsDat = buildNGramTM2(corpus = c1,
                                 dtm = quadgramsDtm, 
                                 n = 4,
                                 lowerOrderNGram = trigramsDat2,
                                 max_loop = NULL)
    quadgramsDat2 = quadgramsDat[!is.na(logprob)]
    saveRDS(object = quadgramsDat2, file = paste0(g_corpus_directory_en, '\\quadgrams_sample_', sprintf('%.1f', sampleRate), '.rds'))
})
execTimeSeconds = execTime["elapsed"]
execTimeMins = execTimeSeconds/60
execTimeSecsAfterMins = execTimeSeconds %% 60
message(paste('Quadgrams model built in', round(execTimeMins, 2), 'mins, ', round(execTimeSecsAfterMins, 2), 'secs'))
quadgramsDat2 = readRDS(file = paste0(g_corpus_directory_en, '\\quadgrams_sample_', sprintf('%.1f', sampleRate), '.rds'))

# Quiz #2 with the 4-gram: 6/10
quadgramsDat3 = quadgramsDat2
setkey(quadgramsDat3, 'lowergram')
key(quadgramsDat3)
quadgramsDat3['a case of'][grepl('(\\s)pretzels$|(\\s)cheese$|(\\s)beer$|(\\s)soda$', gram)][order(logprob, decreasing = TRUE)] # Match

quadgramsDat3['would mean the'][grepl('(\\s)most$|(\\s)universe$|(\\s)best$|(\\s)world$', gram)][order(logprob, decreasing = TRUE)] # Match

quadgramsDat3['make me the'][grepl('(\\s)happiest$|(\\s)bluest$|(\\s)saddest$|(\\s)smelliest$', gram)][order(logprob, decreasing = TRUE)] # Match

quadgramsDat3['struggling but the'][grepl('(\\s)referees$|(\\s)crowd$|(\\s)defense$|(\\s)players$', gram)][order(logprob, decreasing = TRUE)] # No match
trigramsDat3['but the'][grepl('(\\s)referees$|(\\s)crowd$|(\\s)defense$|(\\s)players$', gram)][order(logprob, decreasing = TRUE)] # Matched crowd

quadgramsDat3['date at the'][grepl('(\\s)movies$|(\\s)mall$|(\\s)beach$|(\\s)grocery$', gram)][order(logprob, decreasing = TRUE)] # Matched grocery
trigramsDat3['at the'][grepl('(\\s)movies$|(\\s)mall$|(\\s)beach$|(\\s)grocery$', gram)][order(logprob, decreasing = TRUE)] # Match beach

quadgramsDat3['be on my'][grepl('(\\s)way$|(\\s)phone$|(\\s)motorcyle$|(\\s)horse$', gram)][order(logprob, decreasing = TRUE)] # Match

quadgramsDat3['in quite some'][grepl('(\\s)time$|(\\s)years$|(\\s)thing$|(\\s)weeks$', gram)][order(logprob, decreasing = TRUE)] # Match

quadgramsDat3['with his little'][grepl('(\\s)eyes$|(\\s)fingers$|(\\s)toes$|(\\s)ears$', gram)][order(logprob, decreasing = TRUE)] # No match
trigramsDat3['his little'][grepl('(\\s)eyes$|(\\s)fingers$|(\\s)toes$|(\\s)ears$', gram)][order(logprob, decreasing = TRUE)] # No match
bigramsDat2[grepl('^little(\\s+)', gram)][grepl('(\\s)eyes$|(\\s)fingers$|(\\s)toes$|(\\s)ears$', gram)][order(logprob, decreasing = TRUE)] # Match with 'ears'

quadgramsDat3['faith during the'][grepl('(\\s)hard$|(\\s)bad$|(\\s)sad$|(\\s)worse$', gram)][order(logprob, decreasing = TRUE)] # No match
trigramsDat3['during the'][grepl('(\\s)hard$|(\\s)bad$|(\\s)sad$|(\\s)worse$', gram)][order(logprob, decreasing = TRUE)] # No match
bigramsDat2[grepl('^the(\\s+)', gram)][grepl('(\\s)hard$|(\\s)bad$|(\\s)sad$|(\\s)worse$', gram)][order(logprob, decreasing = TRUE)] # Match

quadgramsDat3['you must be'][grepl('(\\s)asleep$|(\\s)insane$|(\\s)insensitive$|(\\s)callous$', gram)][order(logprob, decreasing = TRUE)] # No match
trigramsDat3['must be'][grepl('(\\s)asleep$|(\\s)insane$|(\\s)insensitive$|(\\s)callous$', gram)][order(logprob, decreasing = TRUE)] # No match
bigramsDat2[grepl('^be(\\s+)', gram)][grepl('(\\s)asleep$|(\\s)insane$|(\\s)insensitive$|(\\s)callous$', gram)][order(logprob, decreasing = TRUE)] # Match 'asleep' as first choice.

# QUiz #3: 4/10!
sentenceTest = "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd"
choices = c('give', 'eat', 'die', 'sleep')
answerQuiz(sentenceTest, choices) # give WRONG

sentenceTest = "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his"
choices = c('financial', 'marital', 'horticultural', 'spiritual')
answerQuiz(sentenceTest, choices) # Financial WRONG

sentenceTest = "I'd give anything to see arctic monkeys this"
choices = c('month', 'morning', 'weekend', 'decade')
answerQuiz(sentenceTest, choices) # morning WRONG

sentenceTest = "Talking to your mom has the same effect as a hug and helps reduce your"
choices = c('sleepiness', 'happiness', 'stress', 'hunger')
answerQuiz(sentenceTest, choices) # stress RIGHT

sentenceTest = "When you were in Holland you were like 1 inch away from me but you hadn't time to take a"
choices = c('picture', 'minute', 'walk', 'look')
answerQuiz(sentenceTest, choices) # look WRONG

sentenceTest = "I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the"
choices = c('case', 'mater', 'account', 'incident')
answerQuiz(sentenceTest, choices) # case WRONG

sentenceTest = "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each"
choices = c('finger', 'arm', 'hand', 'toe')
answerQuiz(sentenceTest, choices) # hand RIGHT

sentenceTest = "Every inch of you is perfect from the bottom to the"
choices = c('top', 'side', 'center', 'middle')
answerQuiz(sentenceTest, choices) # top RIGHT

sentenceTest = "I’m thankful my childhood was filled with imagination and bruises from playing"
choices = c('inside', 'outside', 'weekly', 'daily')
answerQuiz(sentenceTest, choices) # outside RIGHT

sentenceTest = "I like how the same people are in almost all of Adam Sandler's"
choices = c('stories', 'novels', 'movies', 'pictures')
answerQuiz(sentenceTest, choices) # NULL
unigramsDat[grepl('^stories$|^novels$|^movies$|^pictures$', gram)][order(logprob, decreasing = TRUE)] # Stories is most probable



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
            bigramsMatches = bigramsDat2[grepl(paste0('^', gramLookup, '(\\s+)'), gram)][grepl(choicesRegex, gram)][order(logprob, decreasing = TRUE)]
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


# ----------------------- WORK IN PROGRESS (END) ----------------------------