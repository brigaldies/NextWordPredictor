library(shiny)
library(DT)

shinyUI(navbarPage(
    title = 'Word Predictor',
    tabPanel(title = 'Predictor',
             fluidRow(wellPanel(
                 textInput(
                     'sentence',
                     'Enter a sentence:',
                     value = "",
                     width = NULL,
                     placeholder = 'Enter a sentence'
                 ),
                 actionButton("word_1", label = "")
             )),
             fluidRow(wellPanel(
                 tableOutput('predictionsDT')
             ))),
    tabPanel(title = 'Model', tags$p(
        "Documentation on the model's construction."
    ))
))