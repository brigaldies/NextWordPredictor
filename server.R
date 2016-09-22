# Required libraries
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
    
    dataset <- reactive({            
        predictions = NULL
        sentence = input$sentence
        message(paste('Input sentence: "', sentence, '"'))
        words = unlist(strsplit(sentence, g_spacesRegex))
        if (length(words) >= 3) {
            predictions = predictWithStupidBackoff(model, sentence) 
        }
        predictions            
    })
    
    output$predictionsDT <- renderTable({
        dataset()
    })
})