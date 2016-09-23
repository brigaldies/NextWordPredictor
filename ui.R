library(shiny)
library(shinydashboard)
library(DT)

shinyUI(dashboardPage(
    dashboardHeader(title = "Word Predictor"),
    dashboardSidebar(sidebarMenu(
        menuItem(
            "Predictor",
            tabName = "predictor",
            icon = icon("dashboard")
        ),
        menuItem("Model", tabName = "model", icon = icon("th"))
    )),
    dashboardBody(tabItems(
        # First tab: Predictor
        tabItem(tabName = 'predictor',
                fluidRow(column(12, box(title = "Sentence Input", status = "primary", solidHeader = TRUE,
                    textInput(
                        'sentence',
                        'Enter a sentence:',
                        value = "",
                        width = NULL,
                        placeholder = 'Enter a sentence'
                    ),
                    actionButton("word_1", label = "")
                ))),
                fluidRow(column(12, box(title = "Prediction Results", status = "primary", solidHeader = TRUE,
                    tableOutput('predictionsDT')
                )))),
        
        tabItem(tabName = 'model',
                tags$p(
                    "Documentation on the model's construction."
                ))
    ))
))