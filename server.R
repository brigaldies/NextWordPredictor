# Required libraries
require(tm) # Corpus and n-gram models.
require(RWeka) # For 2+ gram tokenizer.
require(quanteda) # For unigram tokenizer (The RWeka tokenizer removes the stop words)
require(stringi)
require(stringr)
require(data.table)

# -----------------------------------------------------------------------------
# Startup processing (One-time)
# -----------------------------------------------------------------------------

# Global variables
g_corpus_directory_en = '.'
g_ngram_package = 'TM'
g_spacesRegex = '\\s+'

source('utils.R', local = TRUE)
source('load_model.R', local = TRUE)
source('predictor.R', local = TRUE)

# Load the model
model = loadModel(directory = paste0('.'), sample_rate = 10)

# -----------------------------------------------------------------------------
# Shiny server
# -----------------------------------------------------------------------------
shinyServer(function(input, output) {
    
    output$prediction <- renderText({ 
        sentence = input$sentence
        message(paste('Input sentence: "', sentence, '"'))
        words = unlist(strsplit(sentence, g_spacesRegex))
        predictionFirstWord = ''
        if (length(words) >= 3) {
            predictions = predictNextWord(model = model,
                                          sentence = sentence,
                                          matches_count_max = 5)
            predictionFirstWord = predictions[1]
        }
        
        predictionFirstWord
    })
})