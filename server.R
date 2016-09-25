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
model = NULL
modelLoaded = FALSE
modelLoadExecTime = NULL

# Some session variables
predictionsCount = 0

# -----------------------------------------------------------------------------
# Shiny server
# -----------------------------------------------------------------------------
shinyServer(function(input, output, session) {
    
    model <- reactive({
        # Load model if not already loaded
        if (!modelLoaded) {
            modelLoadExecTime = system.time({
                message('Loading model...')
                withProgress(message = 'Loading the model...', value = 0, {
                    incProgress(1/2, detail = '')
                    model = loadModel(directory = paste0('.'), sample_rate = 10)
                    incProgress(1, detail = 'Done.')
                })
                message('Model loaded.')
            })
            modelLoaded = TRUE
            output$notificationMenu <- renderMenu({
                dropdownMenu(type = "notifications",
                             notificationItem(
                                 text = paste('Model loaded in', round(modelLoadExecTime['elapsed'], 2), 'secs'),
                                 icon("thumbs-up"),
                                 status = "success"),
                             notificationItem(
                                 text = paste('Model size: ', round(object.size(model)/1024/1024, 2), 'MB'),
                                 icon("thumbs-up"),
                                 status = "success")
                )
            })
        }
        model
    })
    
    # TODO: Does not update the notificatio menu successfully yet!
    predictionsCounter <- reactive({
        sentence = input$sentence
        predictionsCount = predictionsCount + 1
        message(paste('Predictions count:', predictionsCount))
        predictionsCount
    })
    
    dataset <- reactive({            
        predictions = NULL
        sentence = input$sentence
        message(paste('Input sentence: "', sentence, '"'))
        lambda = isolate(input$lambda) # Do NOT trigger another prediction after changing lambda. The next prediction will pick up the new lambda value.
        message(paste('Lambda:', lambda))
        n = isolate(as.numeric(input$ngram))
        message(paste('N-gram:', n))
        traceFlag = FALSE # TODO: Add a parameter to set tracing
        predictions = predictWithStupidBackoff2(model = model(), n = n, sentence = sentence, lambda = lambda, trace = traceFlag) 
        predictions
    })
    
    top_prediction <- reactive({
        prediction = NULL
        if (!is.null(dataset())) {
            prediction = as.character(as.data.table(dataset())[1, .(`Predicted Word`)])
        }
        message(paste('Top prediction:', prediction))
        prediction
    })

    # Observe the button clicks on the "predicted words" buttons below the input text box.
    observeEvent(input$word_1, {
        processWordButtonClick(session, input$sentence, dataset(), 1)
    })
    observeEvent(input$word_2, {
        processWordButtonClick(session, input$sentence, dataset(), 2)
    })
    observeEvent(input$word_3, {
        processWordButtonClick(session, input$sentence, dataset(), 3)
    })
    observeEvent(input$word_4, {
        processWordButtonClick(session, input$sentence, dataset(), 4)
    })
    observeEvent(input$word_5, {
        processWordButtonClick(session, input$sentence, dataset(), 5)
    })
    observeEvent(input$word_6, {
        processWordButtonClick(session, input$sentence, dataset(), 6)
    })
    observeEvent(input$word_7, {
        processWordButtonClick(session, input$sentence, dataset(), 7)
    })
    observeEvent(input$word_8, {
        processWordButtonClick(session, input$sentence, dataset(), 8)
    })
    observeEvent(input$word_9, {
        processWordButtonClick(session, input$sentence, dataset(), 9)
    })
    observeEvent(input$word_10, {
        processWordButtonClick(session, input$sentence, dataset(), 10)
    })
    
    
    
    output$predictionsDT <- renderTable(
        dataset(),
        digits = 4
    )
    
    observe(
        if (!is.null(dataset())) {
            for (i in 1:10) {
                updateActionButton(session, paste0("word_", i), label = as.character(as.data.table(dataset())[i, .(`Predicted Word`)]))
            }
        }
    )
    
    observeEvent(input$reset_parameters, {
        message(paste('Parameters reset.'))
        updateSliderInput(session, inputId = "lambda", value = 0.4)
        updateRadioButtons(session, inputId = "ngram", selected = 5)
        
    })
})

# Helper functions
message(paste("Action button 'word_2' clicked!"))

# Append the selected word to the end of input$sentence
processWordButtonClick <- function(session, sentence, predictions, buttonNumber) {
    message(paste0("Action button 'word_", buttonNumber, "' clicked!"))
    if (!is.null(predictions)) {
        updateTextInput(session, "sentence", value = paste(sentence, as.character(as.data.table(predictions)[buttonNumber, .(`Predicted Word`)])))
    }
}