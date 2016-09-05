# ngram analysis with the ngram package

sampleRate = 1
corpusFilesPattern = paste0('^en_US(.)*sample_', sampleRate, '$')
dirSamples = DirSource(directory = corpus_directory_en, pattern = corpusFilesPattern)

# Load the text,