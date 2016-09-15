# Configuration dependency
if (is.na(g_ngram_package)) {
    stop(paste('Global parameter g_ngram_package is undefined!'))
}

# -----------------------------------------------------------------------------
# Function: loadCorpus
#
# Description: Load a Corpus from files in a given directory and according to a
# given files regex pattern.
#
# This function acts as a dispatcher to a specific Corpus loading implementation
# as specified by the global variable g_ngram_package.
#
# Returns: The loaded Corpus
# -----------------------------------------------------------------------------
loadCorpus <- function(directory, files_pattern) {
    c = NULL
    execTime <- system.time({
        packageLowerCase = tolower(g_ngram_package)
        message(paste('Using package', g_ngram_package))
        if (packageLowerCase == 'tm') {
            c = buildCorpusTM(directory, files_pattern)
        } else if (packageLowerCase == 'quanteda') {
            c = buildCorpusQuanteda(directory, files_pattern)
        } else {
            stop(paste('loadCorpus: Unrecognized package name', package))
        }    
    })
    message(paste('Corpus loaded in', round(execTime["elapsed"], 2), "secs"))
    c
}

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
# Function: loadCorpus *** NOT IMPLEMENTED YET ***
#
# Description: Load a Corpus from files in a given directory and according to a
# given files regex pattern.
#
# Implementation with the package Quanteda.
#
# Returns: The loaded Corpus
# -----------------------------------------------------------------------------
buildCorpusQuanteda <- function(directory, files_pattern) {        
    stop('loadCorpusQuanteda: Not implemented!')
}

# -----------------------------------------------------------------------------
# Function: cleanCorpus
#
# Description: Clean/Pre-process a given Corpus.
# 
# This function acts as a dispatcher to a specific Corpus cleaning implementation
# as specified by the global variable g_ngram_package.
#
# Returns: The loaded Corpus
# -----------------------------------------------------------------------------
cleanCorpus <- function(c) {    
    execTime <- system.time({
        packageLowerCase = tolower(g_ngram_package)
        message(paste('Using package', g_ngram_package))
        if (packageLowerCase == 'tm') {
            c = cleanCorpusTM(c)
        } else if (packageLowerCase == 'quanteda') {
            c = cleanCorpusQuanteda(c)
        } else {
            stop(paste('cleanCorpus: Unrecognized package name', package))
        }    
    })
    message(paste('Corpus cleaned in', round(execTime["elapsed"], 2), "secs"))
    c
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
    
    message('Remove punctuation ...')
    replaceWith <- content_transformer(function(x, pattern, replacement) {return (gsub(pattern, replacement, x))})
    replaceWithSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, ' ', x))})
    c <- tm_map(c, replaceWith, "`", "'")
    c <- tm_map(c, replaceWith, "’", "'")
    c <- tm_map(c, replaceWith, "‘", "'")    
    c <- tm_map(c, replaceWith, "“", '"')
    c <- tm_map(c, replaceWith, "”", '"')   
    c <- tm_map(c, replaceWithSpace, "—")     
    c <- tm_map(c, replaceWithSpace, "–")
    c <- tm_map(c, replaceWithSpace, "…")  
    c <- tm_map(c, replaceWithSpace, "™")
    c <- tm_map(c, replaceWith, "€", "Euros")

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
    
    message('Convert to lower case ...')
    c <- tm_map(c, content_transformer(tolower))
    
    # Return the cleaned Corpus
    c
}

# -----------------------------------------------------------------------------
# Function: cleanCorpus *** NOT IMPLEMENTED YET ***
#
# Description: Clean/Pre-process a given Corpus with the Quanteda package.
# 
# Returns: The loaded Corpus
# -----------------------------------------------------------------------------
cleanCorpusQuanteda <- function(c) {        
    stop('cleanCorpusQuanteda: Not implemented!')
}
