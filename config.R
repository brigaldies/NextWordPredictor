# Project configuration

# Required libraries
require(tm) # Corpus and n-gram models.
options(java.parameters = "-Xmx2g")
require(RWeka) # For tokenizer.
require(quanteda)
require(stringi)
require(data.table)
options(table_counter = TRUE)
require(snow)
require(microbenchmark)

R.Version()
sessionInfo()

# Directory where the corpora files and samples reside
g_corpus_directory_en = 'C:\\training\\coursera\\datascience\\Capstone\\dataset\\final\\en_US'
# Text processing and ngram building package (TM is used in conjunction with RWeka)
g_ngram_package = 'TM'