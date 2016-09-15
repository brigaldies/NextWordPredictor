library(shiny)

shinyUI(fluidPage(
    verticalLayout(
        titlePanel("Sentence Next Word Predictor"),
        textInput('sentence', 'Enter a sentence', value = "", width = NULL, placeholder = 'Enter a sentence'),
        submitButton(text = "Predict!", icon = NULL, width = NULL),
        verbatimTextOutput('prediction')
    )
))