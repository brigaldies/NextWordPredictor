g_corpus_directory_en = 'C:\\training\\coursera\\datascience\\Capstone\\dataset\\final\\en_US'
filePath = paste0(g_corpus_directory_en, '\\', 'out.txt')
outFile = file(filePath, open = "wt", encoding = "UTF-8")
words = c('C', 'e', 'a', 'A', 'b')
writeLines(text = sort(unique(tolower(words))), con = outFile)
close(outFile)

require(tm)
require(RWeka)
sampleRate = 1
corpusFilesPattern = paste0('^en_US(.)*sample_', sprintf('%.1f', sampleRate), '$')
c1 = buildCorpusTM(directory = g_corpus_directory_en, files_pattern = corpusFilesPattern)
c1 = cleanCorpusTM(c1)

filePath = paste0(g_corpus_directory_en, '\\', 'cleaned-corpus.txt')
outFile = file(filePath, open = "wt", encoding = "UTF-8")
for (i in 1:3) {
    doc = c1[[i]]$content
    writeLines(paste('*** Document', i, '***'), con = outFile)
    linesCount = length(doc)
    for (j in 1:linesCount) {
        writeLines(doc[j], con = outFile)
    }
}
close(outFile)

buildCorpusTM <- function(directory, files_pattern) {        
    message(paste0("Loading the Corpus files with pattern '", files_pattern, "'..."))
    dirSamples = DirSource(directory, pattern = files_pattern)
    print(dirSamples$filelist)    
    c = Corpus(dirSamples, readerControl=list(reader=readPlain)) # Language is assumed to be english        
}

cleanCorpusTM <- function(c) {
    
    message('Corpus cleansing ...')
    
    message('Remove punctuation ...')
    c <- tm_map(c, removePunctuation)    
    replaceWithSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, ' ', x))})
    c <- tm_map(c, replaceWithSpace, "`")
    c <- tm_map(c, replaceWithSpace, "’")
    c <- tm_map(c, replaceWithSpace, "‘")
    c <- tm_map(c, replaceWithSpace, "”")   
    c <- tm_map(c, replaceWithSpace, "“")     
    c <- tm_map(c, replaceWithSpace, "–")
    c <- tm_map(c, replaceWithSpace, "—")    
    c <- tm_map(c, replaceWithSpace, ":")
    c <- tm_map(c, replaceWithSpace, "…")  
    c <- tm_map(c, replaceWithSpace, "€")
    
    message('Remove numbers ...')
    c <- tm_map(c, removeNumbers)
    
    message('Remove profanity ...')
    enProfanityUrl = "https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
    enProfanityFile = paste0(g_corpus_directory_en, '\\', 'en_profanity.txt')
    if (!file.exists(enProfanityFile)) {
        download.file(enProfanityUrl, destfile=enProfanityFile)
    }
    enProfanityWords <- read.table(enProfanityFile, header=FALSE, sep="\n", strip.white = TRUE)
    c <- tm_map(c, removeWords, enProfanityWords[,1])
    
    message('Strip white spaces ...')
    c <- tm_map(c, stripWhitespace)
    
    message('Convert to lower case ...')
    c <- tm_map(c, content_transformer(tolower))
    
    # Return the cleaned Corpus
    c
}
