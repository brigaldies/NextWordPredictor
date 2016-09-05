# Required packages
require(tm) # Text mining package
require(SnowballC)

# Directory where the corpora reside
corpus_directory_en = 'C:\\training\\coursera\\datascience\\Capstone\\dataset\\final\\en_US'

# List of english bad words
enProfanityUrl = "https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
enProfanityFile = paste0(corpus_directory_en, '\\', 'en_profanity.txt')
if (!file.exists(enProfanityFile)) {
    download.file(enProfanityUrl, destfile=enProfanityFile)
}
enProfanityWords <- read.table(enProfanityFile, header=FALSE, sep="\n", strip.white = TRUE)
   
# Count the number of lines in the corpora
# for (fileName in c('en_US.blogs.txt', 'en_US.news.txt', 'en_US.twitter.txt')) {
fileLinesCount <- function(filepath) {    
    cnx = file(filepath, open = "r")
    linesCount = 0
    maxLineLength = 0
    while (TRUE) {
        line = readLines(cnx, n = 1)    
        if (length(line)  == 0 ) {
            # print("Reached EOF")
            break
        }
        linesCount = linesCount + 1
        lineLength = nchar(line)
        if (lineLength > maxLineLength) {
            maxLineLength = lineLength
        }
        
    }
    #print(paste(fileName, "- lines count    :", linesCount))
    #print(paste(fileName, "- max line length:", maxLineLength))
    close(cnx)
    c(linesCount, maxLineLength)
}

# Read N lines
count = 10
for (i in 1:count) {
    line = readLines(en_twitter_cnx, n = 1)
    if ( length(line) == 0 ) {
        print("Reached EOF")
        break
    }
    print(line)
}

# Tokenize a file.
fileName = 'en_US.blogs.txt.sample_10'
filePath = paste0(corpus_directory_en, '\\', fileName)
cnx = file(filePath, open = "r")
lines = readLines(cnx, n = 10)
tokens = c()
linesChunkSize = 1000
while (TRUE) {
    lines = readLines(cnx, n = linesChunkSize)  
    linesCount = length(lines)
    if (linesCount == 0 ) {
        print("Reached EOF")
        break
    }    
    print(paste(linesCount, 'lines read'))
    # Tokenize using the following delimiters:
    # Any form of space
    # Punctuation: 
    spacesRegex = '\\s+'
    punctuationRegex = '[.,;:]'
    tokens <- c(tokens, strsplit("a    b\tc\nd\re,f;g", split = '(\\s+)|[.,;]')[[1]])  
}
close(cnx)

# cumulative usage analysis
unigramDat$rownumber <- as.integer(rownames(unigramDat))
unigramCount = dim(unigramDat)[[1]]
coverageDat <- data.frame(percent_grams = (unigramDat$rownum/unigramCount)*100, corpus_coverage = cumsum(unigramDat$usage))
