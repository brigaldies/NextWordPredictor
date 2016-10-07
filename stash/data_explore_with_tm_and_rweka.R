require(dplyr)
require(ggplot2)
require(tm)
require(RWeka)

# Directory where the corpora reside
corpus_directory_en = 'C:\\training\\coursera\\datascience\\Capstone\\dataset\\final\\en_US'

# Tokenize
tokens = MC_tokenize(c1[[1]])
tokens[unlist(lapply(tokens,function(token) { nchar(token) >0} ))]

# Compute the Document-Term matrix
execTime <- system.time({
    dtm <- DocumentTermMatrix(c1)
})
message(paste("Document-Term matrix for 1-grams (words) built in ", round(execTime["elapsed"], 2), "secs"))
dtm
inspect(dtm[1-3,1:10])

# Grams stats
freq <- colSums(as.matrix(dtm))
freqSortDesc <- order(freq,decreasing=TRUE)
freq[head(freqSortDesc, n=20)]
# Total number of grams:
ngramsTotalCount = colSums(as.data.frame(freq))
coverage <- (freq/ngramsTotalCount)*100
coverageSortDesc <- order(coverage, decreasing=TRUE)
coverage[head(coverageSortDesc, n=20)]


# N-grams:

# cleanup/transformations
c1 <- tm_map(c1, stripWhitespace)
c1 <- tm_map(c1, content_transformer(tolower))
# c1 <- tm_map(c1, removeWords, stopwords("english"))
c1 <- tm_map(c1, removeWords, enProfanityWords[,1])
c1 <- tm_map(c1, removePunctuation)
c1 <- tm_map(c1, removeNumbers)
#c1 <- tm_map(c1, stemDocument, language = "english") # Erase words suffixes to retrieve their radicals

# Tokenize the n-grams:
ngramCount = 3
bigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = ngramCount, max = ngramCount))
execTime <- system.time({
    dtm <- DocumentTermMatrix(c1, control = list(tokenize = bigramTokenizer))
})
message(paste("Document-Term matrix for ", ngramCount, "-grams built in ", round(execTime["elapsed"], 2), "secs"))
inspect(dtm[1:3,100:110])

execTime <- system.time({
    tdm <- TermDocumentMatrix(c1, control = list(tokenize = bigramTokenizer))
})
message(paste("Term-Document matrix for ", ngramCount, "-grams built in ", round(execTime["elapsed"], 2), "secs"))
inspect(tdm[100:110,1:3])

# Some plotting
# Make a data frame
dat <- as.data.frame(freq[head(freqSortDesc, n=40)])
dat <- as.data.frame(coverage[head(coverageSortDesc, n=40)])
dat <- add_rownames(dat)
names(dat) <- c('ngram', 'freq')
ggplot(dat, aes(x = factor(ngram, levels = dat$ngram), y = freq)) + 
    geom_bar(stat = "identity") + 
    coord_flip()
