require(ggplot2)
require(tm)
require(RWeka)
require(dplyr)

# -----------------------------------------------------------------------------
# Function: ngramAnalysis
#
# Description: Performs an n-gram analysis.
#
# Arguments:
# directory: Location of the corpus files.
# pattern  : Corpus files pattern.
# -----------------------------------------------------------------------------
ngramAnalysis <- function(directory, pattern, ngram_count) {
    
    message('Loading the Corpus files:')
    tmDirSource = DirSource(directory = directory, pattern = pattern)
    sapply(tmDirSource$filelist, message)
    message('Loading the Corpus...')
    execTime <- system.time({
        c1 = Corpus(tmDirSource, readerControl=list(reader=readPlain)) # Language is assumed to be english
    })
    message(paste("Corpus built in ", round(execTime["elapsed"], 2), "secs"))
    summary(c1)
        
    message('Loading the profanity words to exclude ...')
    execTime <- system.time({
        enProfanityUrl = "https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
        enProfanityFile = paste0(corpus_directory_en, '\\', 'en_profanity.txt')
        if (!file.exists(enProfanityFile)) {
            download.file(enProfanityUrl, destfile=enProfanityFile)
        }
        enProfanityWords <- read.table(enProfanityFile, header=FALSE, sep="\n", strip.white = TRUE)    
    })
    message(paste("Profanity words loaded in ", round(execTime["elapsed"], 2), "secs"))
        
    message('Cleanup/transformations ...')
    execTime <- system.time({
        message('Strip white spaces ...')
        c1 <- tm_map(c1, stripWhitespace)
        message('To lower case ...')
        c1 <- tm_map(c1, content_transformer(tolower))
#        if (ngram_count == 1) {
#            message('Strip english stop words ...')
#            c1 <- tm_map(c1, removeWords, stopwords("english"))
#        }
        message('Remove profanity ...')
        c1 <- tm_map(c1, removeWords, enProfanityWords[,1])
        message('Remove punctuation ...')
        c1 <- tm_map(c1, removePunctuation)
        message('Remove numbers ...')
        c1 <- tm_map(c1, removeNumbers)
        # c1 <- tm_map(c1, content_transformer(gsub), pattern = "it's", replacement = "it is")
        # c1 <- tm_map(c1, content_transformer(gsub), pattern = "I'm", replacement = "I am")
        #c1 <- tm_map(c1, stemDocument, language = "english") # Erase words suffixes to retrieve their radicals
    })
    message(paste("Cleanup/transformations performed in ", round(execTime["elapsed"], 2), "secs"))
    
    message('Compute the Document-Term matrix ...')    
    ngramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = ngram_count, max = ngram_count))
    execTime <- system.time({
        dtm <- DocumentTermMatrix(c1, control = list(tokenize = ngramTokenizer))
    })
    message(paste("Document-Term matrix for ", ngram_count, "-grams built in ", round(execTime["elapsed"], 2), "secs"))
    
    message('Compute some ngrams stats...')
    topRange = 40
    freq <- colSums(as.matrix(dtm))
    freqSortDesc <- order(freq, decreasing=TRUE)
    freq[head(freqSortDesc, n=topRange)]
    
    # Total number of grams:
    ngramsTotalCount = colSums(as.data.frame(freq))
    message(ngramsTotalCount, ' ', ngram_count, '-grams total.')
    coverage <- (freq/ngramsTotalCount)*100
    coverageSortDesc <- order(coverage, decreasing=TRUE)
    coverage[head(coverageSortDesc, n=topRange)]
    
    message('Plot top coverage ngrams...')
    # dat <- as.data.frame(freq[head(freqSortDesc, n=40)])
    dat <- as.data.frame(coverage[head(coverageSortDesc, n=40)])
    dat <- add_rownames(dat)
    names(dat) <- c('ngram', 'freq')
    ggplot(dat, aes(x = factor(ngram, levels = dat$ngram), y = freq)) + 
        geom_bar(stat = "identity") + 
        coord_flip()
}