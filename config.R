# Project configuration

# Required libraries
#require(dplyr)
#require(ggplot2)
#require(grid)
#require(gridExtra)
#require(xtable)
#require(htmlTable)
require(tm) # Corpus and n-gram models.
require(RWeka) # For tokenizer.
#require(NLP)
require(quanteda)
require(stringi)
require(stringr)
require(data.table)
#require(ngram) # For 'concat' function.
options(table_counter = TRUE)

R.Version()
sessionInfo()

# Directory where the corpora files and samples reside
g_corpus_directory_en = 'C:\\training\\coursera\\datascience\\Capstone\\dataset\\final\\en_US'
# Text processing and ngram building package (TM is used in conjunction with RWeka)
g_ngram_package = 'TM'
