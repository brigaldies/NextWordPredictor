library(shiny)
library(DT)

shinyUI(fluidPage(fluidRow(
    column(2, tags$img(
        height = 100,
        width = 100,
        src = 'bigorb.png'
    )),
    column(10, titlePanel("Sentence Next Word Predictor"))
),
tabsetPanel(
    tabPanel('Predictor',
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
    tabPanel('Model', tags$p("Documentation on the model's construction."))
)))