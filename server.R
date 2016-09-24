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
shinyServer(function(input, output, session) {
    
    dataset <- reactive({            
        predictions = NULL
        sentence = input$sentence
        message(paste('Input sentence: "', sentence, '"'))
        # words = unlist(strsplit(sentence, g_spacesRegex))
        # if (length(words) >= 3) {
        #     predictions = predictWithStupidBackoff2(model, sentence) 
        # }
        # predictions            
        predictWithStupidBackoff2(model, sentence) 
    })
    
    top_prediction <- reactive({
        prediction = NULL
        if (!is.null(dataset())) {
            prediction = as.character(as.data.table(dataset())[1, .(last_word_in_gram)])
        }
        message(paste('Top prediction:', prediction))
        prediction
    })

    # This creates an infinite dependency loop!    
    observeEvent(input$word_1, {
        # Run whenever 'word_1' button is pressed
        message(paste("Action button 'word_1' clicked!"))

        # This will change the value of input$sentence
        if (!is.null(top_prediction())) {
            updateTextInput(session, "sentence", value = paste(input$sentence, top_prediction()))
        }
    })
    
    output$predictionsDT <- renderTable({
        dataset()
    })
    
    observe(
        if (!is.null(top_prediction())) {
            updateActionButton(session, "word_1", label = top_prediction())
        }
    )
})