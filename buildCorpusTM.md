```{r}
# -----------------------------------------------------------------------------
# Function: loadCorpus
#
# Description: Load a Corpus from files in a given directory and according to a
# given files regex pattern.
#
# Implementation with the package TM.
#
# Returns: The loaded Corpus
# -----------------------------------------------------------------------------
buildCorpusTM <- function(directory, files_pattern) {        
    message(paste0("Loading the Corpus files with pattern '", files_pattern, "'..."))
    dirSamples = DirSource(directory, pattern = files_pattern)
    print(dirSamples$filelist)    
    c = Corpus(dirSamples, readerControl=list(reader=readPlain)) # Language is assumed to be english        
}

# -----------------------------------------------------------------------------
# Function: cleanCorpus
#
# Description: Clean/Pre-process a given Corpus with the TM package.
# 
# Cleaning steps:
# - Strip white spaces
# - Convert all characters to lower case;
# - Remove profanity words (See URL to references list in the implementation)
# - Remove punctuation;
# - Remove numbers.
#
# Returns: The loaded Corpus
# -----------------------------------------------------------------------------
cleanCorpusTM <- function(c) {
    
    message('Corpus cleansing ...')

    message('Convert to lower case ...')
    c <- tm_map(c, content_transformer(tolower))
    
    message('Remove punctuation ...')
    c <- tm_map(c, removePunctuation)    
    
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
    
    # Return the cleaned Corpus
    c
}
```
