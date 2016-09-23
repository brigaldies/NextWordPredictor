library(shiny)
library(shinydashboard)
library(DT)

shinyUI(dashboardPage(
    dashboardHeader(title = "Word Predictor"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Predictor", tabName = "predictor", icon = icon("calculator")),
            menuItem("Parameters", tabName = "parameters", icon = icon("dashboard")),
            menuItem("Model", tabName = "model", icon = icon("cube")),
            menuItem("Algorithm", tabName = "algorithm", icon = icon("cogs")),            
            menuItem("Application Code", tabName = "code", icon = icon("code"))
            # Add:
            # Acknowledgments
            # COntact information
        )
    ),
    dashboardBody(
        tabItems(
            # First tab: Predictor
            tabItem(tabName = 'predictor',
                    fluidRow(
                        box(title = "Sentence Input", width = 12, status = "primary", solidHeader = TRUE,
                            tags$p('*** Write instructions here ***'),
                            textInput('sentence',
                                      'Enter a sentence:',
                                      value = "",
                                    width = NULL,
                                    placeholder = 'Enter a sentence'),
                            actionButton("word_1", label = "-"),
                            actionButton("word_2", label = "-"),
                            actionButton("word_3", label = "-"),
                            actionButton("word_4", label = "-"),
                            actionButton("word_5", label = "-"),
                            actionButton("word_6", label = "-"),
                            actionButton("word_7", label = "-"),
                            actionButton("word_8", label = "-"),
                            actionButton("word_9", label = "-"),
                            actionButton("word_10", label = "-")
                        )
                    ),
                    fluidRow(
                        box(title = "Proposed Next Words", width = 12, status = "primary", solidHeader = TRUE,
                            tableOutput('predictionsDT')
                        )
                    )
            ),
            tabItem(tabName = 'parameters',
                    tags$p("Predictor's parameters.")
            ),
            tabItem(tabName = 'model',
                    tags$p("Documentation on the model's construction.")
            ),
            tabItem(tabName = 'algorithm',
                    tags$p("Explain the prediction algorithm here.")
            ),
            tabItem(tabName = 'code',
                    tags$p("provide some code snippets.")
            )
        )
    )
))