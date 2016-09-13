sampleRate = 10
corpusFilesPattern = paste0('^en_US(.)*sample_', sprintf('%.1f', sampleRate), '$')
c1 = loadCorpus(directory = g_corpus_directory_en, files_pattern = corpusFilesPattern)
c1 = cleanCorpus(c1)
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
    saveRDS(object = unigramsDat, file = paste0(g_corpus_directory_en, '\\unigrams.rds'))
})
execTimeSeconds = execTime["elapsed"]
execTimeMins = execTimeSeconds/60
execTimeSecsAfterMins = execTimeSeconds %% 60
message(paste('Unigrams model built in', round(execTimeMins), 'mins, ', round(execTimeSecsAfterMins, 2), 'secs'))
# Re-load the unigrams
unigramsDat = readRDS(file = paste0(g_corpus_directory_en, '\\unigrams.rds'))

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
execTime = system.time({
    quadgramsDat = buildNGramTM2(c1, 4, trigramsDat2, max_loop = NULL)
    quadgramsDat2 = quadgramsDat[!is.na(logprob)]
    saveRDS(object = quadgramsDat2, file = paste0(g_corpus_directory_en, '\\quadgrams_sample_', sprintf('%.1f', sampleRate), '.rds'))
})
execTimeSeconds = execTime["elapsed"]
execTimeMins = execTimeSeconds/60
execTimeSecsAfterMins = execTimeSeconds %% 60
message(paste('Quadgrams model built in', round(execTimeMins, 2), 'mins, ', round(execTimeSecsAfterMins, 2), 'secs'))

# Quiz #2 with the 4-gram: 
quadgramsDat2[grepl('^a case of(\\s+)', gram)][grepl('(\\s)pretzels$|(\\s)cheese$|(\\s)beer$|(\\s)soda$', gram)][order(logprob, decreasing = TRUE)] # No match
trigramsDat2[grepl('^case of(\\s+)', gram)][grepl('(\\s)pretzels$|(\\s)cheese$|(\\s)beer$|(\\s)soda$', gram)][order(logprob, decreasing = TRUE)] # No match
bigramsDat2[grepl('^of(\\s+)', gram)][grepl('(\\s)pretzels$|(\\s)cheese$|(\\s)beer$|(\\s)soda$', gram)][order(logprob, decreasing = TRUE)] # Match

quadgramsDat2[grepl('^would mean the(\\s+)', gram)][grepl('(\\s)most$|(\\s)universe$|(\\s)best$|(\\s)world$', gram)][order(logprob, decreasing = TRUE)] # Match

quadgramsDat2[grepl('^make me the(\\s+)', gram)][grepl('(\\s)happiest$|(\\s)bluest$|(\\s)saddest$|(\\s)smelliest$', gram)][order(logprob, decreasing = TRUE)] # Match

quadgramsDat2[grepl('^struggling but the(\\s+)', gram)][grepl('(\\s)referees$|(\\s)crowd$|(\\s)defense$|(\\s)players$', gram)][order(logprob, decreasing = TRUE)] # No match
trigramsDat2[grepl('^but the(\\s+)', gram)][grepl('(\\s)referees$|(\\s)crowd$|(\\s)defense$|(\\s)players$', gram)][order(logprob, decreasing = TRUE)] # No match
bigramsDat2[grepl('^the(\\s+)', gram)][grepl('(\\s)referees$|(\\s)crowd$|(\\s)defense$|(\\s)players$', gram)][order(logprob, decreasing = TRUE)] # Matching crowd as first choice.

quadgramsDat2[grepl('^date at the(\\s+)', gram)][grepl('(\\s)movies$|(\\s)mall$|(\\s)beach$|(\\s)grocery$', gram)][order(logprob, decreasing = TRUE)] # No Match
trigramsDat2[grepl('^at the(\\s+)', gram)][grepl('(\\s)movies$|(\\s)mall$|(\\s)beach$|(\\s)grocery$', gram)][order(logprob, decreasing = TRUE)] # Match

quadgramsDat2[grepl('^be on my(\\s+)', gram)][grepl('(\\s)way$|(\\s)phone$|(\\s)motorcyle$|(\\s)horse$', gram)][order(logprob, decreasing = TRUE)] # No Match
trigramsDat2[grepl('^on my(\\s+)', gram)][grepl('(\\s)way$|(\\s)phone$|(\\s)motorcyle$|(\\s)horse$', gram)][order(logprob, decreasing = TRUE)] # Match

quadgramsDat2[grepl('^in quite some(\\s+)', gram)][grepl('(\\s)time$|(\\s)years$|(\\s)thing$|(\\s)weeks$', gram)][order(logprob, decreasing = TRUE)] # No Match
trigramsDat2[grepl('^quite some(\\s+)', gram)][grepl('(\\s)time$|(\\s)years$|(\\s)thing$|(\\s)weeks$', gram)][order(logprob, decreasing = TRUE)] # Match

quadgramsDat2[grepl('^with his little(\\s+)', gram)][grepl('(\\s)eyes$|(\\s)fingers$|(\\s)toes$|(\\s)ears$', gram)][order(logprob, decreasing = TRUE)] # No match
trigramsDat2[grepl('^his little(\\s+)', gram)][grepl('(\\s)eyes$|(\\s)fingers$|(\\s)toes$|(\\s)ears$', gram)][order(logprob, decreasing = TRUE)] # No match
bigramsDat2[grepl('^little(\\s+)', gram)][grepl('(\\s)eyes$|(\\s)fingers$|(\\s)toes$|(\\s)ears$', gram)][order(logprob, decreasing = TRUE)] # Match with 'eyes'

quadgramsDat2[grepl('^faith during the(\\s+)', gram)][grepl('(\\s)hard$|(\\s)bad$|(\\s)sad$|(\\s)worse$', gram)][order(logprob, decreasing = TRUE)] # No match
trigramsDat2[grepl('^during the(\\s+)', gram)][grepl('(\\s)hard$|(\\s)bad$|(\\s)sad$|(\\s)worse$', gram)][order(logprob, decreasing = TRUE)] # No match
bigramsDat2[grepl('^the(\\s+)', gram)][grepl('(\\s)hard$|(\\s)bad$|(\\s)sad$|(\\s)worse$', gram)][order(logprob, decreasing = TRUE)] # Match

quadgramsDat2[grepl('^you must be(\\s+)', gram)][grepl('(\\s)asleep$|(\\s)insane$|(\\s)insensitive$|(\\s)callous$', gram)][order(logprob, decreasing = TRUE)] # No match
trigramsDat2[grepl('^must be(\\s+)', gram)][grepl('(\\s)asleep$|(\\s)insane$|(\\s)insensitive$|(\\s)callous$', gram)][order(logprob, decreasing = TRUE)] # No match
bigramsDat2[grepl('^be(\\s+)', gram)][grepl('(\\s)asleep$|(\\s)insane$|(\\s)insensitive$|(\\s)callous$', gram)][order(logprob, decreasing = TRUE)] # Match 'asleep' as first choice.


# ----------------------- WORK IN PROGRESS (END) ----------------------------