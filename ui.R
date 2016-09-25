library(shiny)
library(shinydashboard)
library(DT)

shinyUI(dashboardPage(
    dashboardHeader(title = "Word Predictor 1.0", dropdownMenuOutput("notificationMenu")),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Predictor", tabName = "predictor", icon = icon("calculator")),
            menuItem("Parameters", tabName = "parameters", icon = icon("dashboard")),
            menuItem("Model", tabName = "model", icon = icon("cube")),
            menuItem("Prediction Algorithm", tabName = "algorithm", icon = icon("cogs")),            
            menuItem("Application Code", tabName = "code", icon = icon("code")),
            menuItem("Acknowledgements", tabName = "ack", icon = icon("smile-o")),
            menuItem("Contact Information", tabName = "contact", icon = icon("linkedin"))
        )
    ),
    dashboardBody(
        tabItems(
            # Tab: Predictor
            tabItem(tabName = 'predictor',
                    fluidRow(
                        box(title = "Sentence Input", width = 12, status = "primary", solidHeader = TRUE,
                            tags$p('Go ahead and type, in', tags$strong('English'), ', in the input text box below. Each key stroke is communicated to the server, 
                                   hence there is no Submit button to hit to see the predictions of the next word.'),
                            tags$p('The ten buttons below the input text show the weighted probability-ranked predicted words. Click on any button 
                                   if you see the next word you need. The selected word will be appended to your text.'),
                            tags$p('Go to the Parameters page to choose different algorithm settings.'),
                            tags$p(tags$strong('Note'), ': Prior to the very first prediction, the model will be (lazy) loaded, which takes a few seconds.'),
                            tags$p('Have fun!'),
                            hr(),
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
            # Tab: parameters
            tabItem(tabName = 'parameters',
                    fluidRow(
                        box(title = "Stupid Backoff Algorithm Weight (Lambda)", width = 12, status = "primary", solidHeader = TRUE,
                            "In the \"Stupid Backoff\" prediction algorithm, if a higher-order N-gram does not provide any prediction, 
                            the algorithm \"backs off\" to the next lower order (N-1)-gram, weighted by a fixed weight, called \"lambda\". 
                            Use the slider below to set the value of lambda. The default value is 0.4, per the algortihm's authors' recommendation (Brants et al., 2007).", 
                            br(),
                            sliderInput(inputId = "lambda", 
                                        label = "Lamdba:", 
                                        min = 0, 
                                        max = 1, 
                                        value = 0.4,
                                        step = 0.1)
                        )
                    ),
                    fluidRow(
                        box(title = "The 'N' in the N-gram Model", width = 12, status = "primary", solidHeader = TRUE,
                            "Our prediction model is composed of 5 separate N-grams, with N = 1 (unigrams) to 5 (pentagrams). 
                            The prediction algorithm can be configured to start at any one of the N-grams (With N >= 2 as it does not make sense to predict with unigrams only), 
                            and backoff from there. 
                            Use the radio buttons below to choose N. The default value is 5.", 
                            br(),
                            radioButtons("ngram", "N-gram to Start the Prediction Algorithm From:",
                                         c("Bigrams (N = 2)" = 2,
                                           "Trigrams (N = 3)" = 3,
                                           "Quadgrams (N = 4)" = 4,
                                           "Pentagrams (N = 5)" = 5
                                           ),
                                         selected = 5)
                        )
                    ),
                    fluidRow(
                        box(width = 12, status = "info", actionButton("reset_parameters", label = "Reset parameters to their default values"))
                    )
            ),
            # Tab: model
            tabItem(tabName = 'model',
                    tags$p("Documentation on the model's construction.")
            ),
            # Tab: algorithm
            tabItem(tabName = 'algorithm',
                    tags$p("Explain the prediction algorithm here.")
            ),
            # Tab: code snippets
            tabItem(tabName = 'code',
                    tags$p("provide some code snippets.")
            ),
            # Tab: acknowledgement
            tabItem(tabName = 'ack',
                    tags$p("The following information sources were instrumental in conducting the research for, and producing, this predictive application:"),
                    tags$ol(
                        tags$li('First, and foremost, my fellow class mates on the Coursera Captone class forum!'),
                        tags$li('A number of articles suggested by the Coursera Captone class forum, most notably:'),
                        tags$ul(
                            tags$li(tags$a(href = 'https://eight2late.wordpress.com/2015/05/27/a-gentle-introduction-to-text-mining-using-r/', 'Gentle Introduction to Text Mining Using R')),
                            tags$li(tags$a(href = 'https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html#plot-word-frequencies', 'Basic Text Mining in R')),
                            tags$li(tags$a(href = 'https://english.boisestate.edu/johnfry/files/2013/04/bigram-2x2.pdf', 'Bigrams and Trigrams'))
                        ),
                        tags$li(tags$a(href = 'https://en.wikipedia.org/wiki/Katz%27s_back-off_model', 'The Wikipedia page on the Katz\'s Back-Off Model')),
                        tags$li('And, last, but not least, stackoverflow.com! What would we do without it?')
                    )
            ),
            # Tab: contact information
            tabItem(tabName = 'contact',
                    tags$p("Find me on LinkedIn", tags$a(href="https://www.linkedin.com/in/bertrandrigaldies", "here."))
            )
        )
    )
))