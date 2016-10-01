# Required libraries
require(stringi)
require(stringr)
require(data.table)
require(ggplot2)
require(gridExtra)

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

# Training data sample rate
sampleRate = 10
execTime = system.time({
    message(paste('Loading the English corpus statistics...'))
    fileStats = readRDS('./en_US.filestats.rds')
    fileStatsTraining = readRDS(paste0('./en_US.filestats_partition_', sprintf('%.1f', sampleRate), '.rds'))
})
execTimeSeconds = execTime["elapsed"]
execTimeMins = execTimeSeconds/60
execTimeSecsAfterMins = execTimeSeconds %% 60
message(paste('English corpus statistics loaded in', round(execTimeMins, 2), 'mins, ', round(execTimeSecsAfterMins, 2), 'secs'))

# -----------------------------------------------------------------------------
# Shiny server
# -----------------------------------------------------------------------------
shinyServer(function(input, output, session) {
    
    # -------------------------------------------------------------------------
    # Application's internal links
    # -------------------------------------------------------------------------
    observeEvent(input$link_to_parameters, {
        updateTabItems(session, inputId = "menu", selected = 'parameters')
    })
    observeEvent(input$link_to_algorithm, {
        updateTabItems(session, inputId = "menu", selected = 'algorithm')
    })
    observeEvent(input$link_to_application_code, {
        updateTabItems(session, inputId = "menu", selected = 'code')
    })
    
    # -------------------------------------------------------------------------
    # The model
    # -------------------------------------------------------------------------
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
    
    # TODO: Does not update the notification menu successfully yet!
    predictionsCounter <- reactive({
        sentence = input$sentence
        predictionsCount = predictionsCount + 1
        message(paste('Predictions count:', predictionsCount))
        predictionsCount
    })
    
    # -------------------------------------------------------------------------
    # The predicted words
    # -------------------------------------------------------------------------
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
    
    # -------------------------------------------------------------------------
    # The top predicted word
    # -------------------------------------------------------------------------
    top_prediction <- reactive({
        prediction = NULL
        if (!is.null(dataset())) {
            prediction = as.character(as.data.table(dataset())[1, .(`Predicted Word`)])
        }
        message(paste('Top prediction:', prediction))
        prediction
    })

    # -------------------------------------------------------------------------
    # Observe the button clicks on the "predicted words" buttons below the 
    # input text box.
    # -------------------------------------------------------------------------
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
    observeEvent(input$cleanInputText, {
        updateTextInput(session, "sentence", value = '')
    })
    
    # -------------------------------------------------------------------------
    # The table of predicted words
    # -------------------------------------------------------------------------
    output$predictionsDT <- renderTable(
        dataset(),
        digits = 4
    )
    
    # -------------------------------------------------------------------------
    # Update the 10 buttons below the input text box to show the top 10 predicted
    # words
    # -------------------------------------------------------------------------
    observe(
        if (!is.null(dataset())) {
            for (i in 1:10) {
                updateActionButton(session, paste0("word_", i), label = as.character(as.data.table(dataset())[i, .(`Predicted Word`)]))
            }
        }
    )
    
    # -------------------------------------------------------------------------
    # Parameters & settings
    # -------------------------------------------------------------------------
    observeEvent(input$reset_parameters, {
        message(paste('Parameters reset.'))
        updateSliderInput(session, inputId = "lambda", value = 0.4)
        updateRadioButtons(session, inputId = "ngram", selected = 5)
        
    })
    
    # -------------------------------------------------------------------------
    # Model documentation
    # -------------------------------------------------------------------------
    output$enUSFilesStatsDF <- renderTable(
        fileStats,
        caption = 'Table 1: English Corpus Documents from HC Corpora',
        align = "lrrrrr"
    )
    output$enUSFilesStatsTrainingDataDF <- renderTable(
        fileStatsTraining,
        caption = paste0('Table 2: ', sampleRate, '%-sampled English Corpus Documents from HC Corpora'),
        align = "lrrrrr"
    )
    output$modelTopUnigramsDT <- renderTable(
        head(model()$unigrams[order(logprob, decreasing = TRUE)], n = 5),
        display = c('s','s','d','f','f'),
        digits = 4
    )
    output$modelTopBigramsDT <- renderTable(
        head(model()$bigrams[order(logprob, decreasing = TRUE)], n = 5),
        display = c('s','s','d','s','f','f','s'),
        digits = 4
    )
    output$modelTopTrigramsDT <- renderTable(
        head(model()$trigrams[order(logprob, decreasing = TRUE)], n = 5),
        display = c('s','s','d','s','f','f','s'),
        digits = 4
    )
    output$modelTopQuadgramsDT <- renderTable(
        head(model()$quadgrams[order(logprob, decreasing = TRUE)], n = 5),
        display = c('s','s','d','s','f','f','s'),
        digits = 4
    )
    output$modelTopPentagramsDT <- renderTable(
        head(model()$pentagrams[order(logprob, decreasing = TRUE)], n = 5),
        display = c('s','s','d','s','f','f','s'),
        digits = 4
    )
    
    output$unigramsPlot <- renderPlot({
        plotMLE = topGramsPlot(1, model()$unigrams, 'Unigram', sampleRate, prob = 'MLE')    
        plotPercent = topGramsPlot(2, model()$unigrams, 'Unigram', sampleRate, prob = 'COUNT')  
        grid.arrange(plotMLE, plotPercent, ncol=2)
    })
    output$bigramsPlot <- renderPlot({
        plotMLE = topGramsPlot(3, model()$bigrams, 'Bigram', sampleRate, prob = 'MLE')
        plotPercent = topGramsPlot(4, model()$bigrams, 'Bigram', sampleRate, prob = 'COUNT')
        grid.arrange(plotMLE, plotPercent, ncol=2)
    })
    output$trigramsPlot <- renderPlot({
        plotMLE = topGramsPlot(5, model()$trigrams, 'Trigram', sampleRate, prob = 'MLE')
        plotPercent = topGramsPlot(6, model()$trigrams, 'Trigram', sampleRate, prob = 'COUNT')
        grid.arrange(plotMLE, plotPercent, ncol=2)
    })
    output$quadgramsPlot <- renderPlot({
        plotMLE = topGramsPlot(7, model()$quadgrams, 'quadgram', sampleRate, prob = 'MLE')
        plotPercent = topGramsPlot(8, model()$quadgrams, 'quadgram', sampleRate, prob = 'COUNT')
        grid.arrange(plotMLE, plotPercent, ncol=2)
    })
    output$pentagramsPlot <- renderPlot({
        plotMLE = topGramsPlot(9, model()$pentagrams, 'Pentagram', sampleRate, prob = 'MLE')
        plotPercent = topGramsPlot(10, model()$pentagrams, 'Pentagram', sampleRate, prob = 'COUNT')
        grid.arrange(plotMLE, plotPercent, ncol=2)
    })
})

# Helper functions

# -----------------------------------------------------------------------------
# Function: processWordButtonClick
# 
# Description: Append the selected word to the end of typed sentence in the
# input text box.
#
# Arguments:
# session     : End-user's connection session.
# sentence    : The sentence typed thus far.
# predictions : The predictions.
# buttonNumber: The button's number (1 through 10).
#
# Side effects: Updated text in the input text box.
#
# Returns: None.
# -----------------------------------------------------------------------------
processWordButtonClick <- function(session, sentence, predictions, buttonNumber) {
    message(paste0("Action button 'word_", buttonNumber, "' clicked!"))
    if (!is.null(predictions)) {
        updateTextInput(session, "sentence", value = paste(sentence, as.character(as.data.table(predictions)[buttonNumber, .(`Predicted Word`)])))
    }
}

# -----------------------------------------------------------------------------
# Function: topGramsPlot
#
# Description: Plots the top MLE grams.
#
# gramsModel    : The n-grams model.
# gramsModelName: The n-grams model name (e.g., 'unigram', bigram')
# sampleRate    : The sample (partition) rate to create the training data.
# topCount      : The number of grams to display in the plot.
#
# Side effects: None.
#
# Returns: The plot.
# -----------------------------------------------------------------------------
topGramsPlot <- function(fig_num, gramsModel, gramsModelName, sampleRate, topCount = 25, prob = 'MLE') {
    if (prob == 'MLE') {
        xlabel = 'Maximum Likelihood Estimation'
        gramsTopDat = head(gramsModel[order(logprob, decreasing = TRUE)], n = topCount)
        gramsTopDat$prob = exp(gramsTopDat$logprob)
    } else if (prob == 'COUNT') {
        xlabel = 'Percent Used'
        gramsTopDat = head(gramsModel[order(logpercent, decreasing = TRUE)], n = topCount)
        gramsTopDat$prob = exp(gramsTopDat$logpercent)
    } else {
        stop(paste0('Unknown probability mode argument "', prob, '"'))
    }
    plot = ggplot(gramsTopDat, aes(x = factor(gram, levels = gramsTopDat$gram), y = prob)) + 
        geom_bar(stat = "identity") + 
        coord_flip() +
        labs(title=paste0('Figure ', fig_num, ': Top ', topCount, ' Probable ', gramsModelName, 's'), 
             x=gramsModelName, 
             y=paste0(xlabel, ' (', sampleRate, '% Sampled English Corpus)')) +
        theme(plot.title = element_text(size=12, face="bold", margin = margin(10, 0, 10, 0)))
    plot
}