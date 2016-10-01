library(shiny)
library(shinydashboard)
library(DT)
library(markdown)

sampleRate = 10

shinyUI(dashboardPage(
    dashboardHeader(title = "Word Predictor 1.0", dropdownMenuOutput("notificationMenu")),
    dashboardSidebar(
        sidebarMenu(id = 'menu',
            menuItem("Predictor", tabName = "predictor", icon = icon("calculator")),
            menuItem("Parameters", tabName = "parameters", icon = icon("wrench")),
            menuItem("Model", tabName = "model", icon = icon("cube")),
            menuItem("Prediction Algorithm", tabName = "algorithm", icon = icon("cogs")),            
            menuItem("Application Code", tabName = "code", icon = icon("code")),
            menuItem("References", tabName = "refs", icon = icon("university")),
            menuItem("Acknowledgements", tabName = "ack", icon = icon("smile-o")),
            menuItem("Contact Information", tabName = "contact", icon = icon("linkedin"))
        )
    ),
    dashboardBody(
        tabItems(
            # -----------------------------------------------------------------
            # Tab: Predictor
            # -----------------------------------------------------------------
            tabItem(tabName = 'predictor',
                    fluidRow(
                        box(title = "Next Word Predictor", width = 12, status = "primary", solidHeader = TRUE,
                            tags$p('Go ahead and type in', tags$strong('English'), 'in the input text box below. Each key stroke is communicated to the server, 
                                   hence there is no Submit button to hit to see the predictions of the next word in your sentence.'),
                            tags$p('The ten buttons below the input text show the weighted probability-ranked predicted words. Click on any button 
                                   if you see the next word you need. The selected word will be appended to your sentence.'),
                            tags$p('Go to the', actionLink("link_to_parameters", "Parameters"), 'page to choose different algorithm settings.'),
                            tags$p(tags$strong('Note'), ': Prior to the very first prediction, the model will be (lazy) loaded, which takes a few seconds.'),
                            textInput('sentence',
                                      'Type your sentence in the input text box below:',
                                      value = "",
                                    width = NULL,
                                    placeholder = 'Type here...'),
                            actionButton("word_1", label = "-"),
                            actionButton("word_2", label = "-"),
                            actionButton("word_3", label = "-"),
                            actionButton("word_4", label = "-"),
                            actionButton("word_5", label = "-"),
                            actionButton("word_6", label = "-"),
                            actionButton("word_7", label = "-"),
                            actionButton("word_8", label = "-"),
                            actionButton("word_9", label = "-"),
                            actionButton("word_10", label = "-"),
                            br(),
                            actionButton("cleanInputText", label = "Clear Input Box", icon("close"), 
                                         style="background-color: #a9a9a9;")
                        )
                    ),
                    fluidRow(
                        box(title = "Proposed Next Words, Ranked by Decreasing Lambda-Weighted Probability:", width = 12, status = "primary", solidHeader = TRUE,
                            tableOutput('predictionsDT')
                        )
                    )
            ),
            # -----------------------------------------------------------------
            # Tab: parameters
            # -----------------------------------------------------------------
            tabItem(tabName = 'parameters',
                    fluidRow(
                        box(title = "Stupid Backoff Algorithm Weight (Lambda)", width = 12, status = "primary", solidHeader = TRUE,
                            "In our implementation of the \"Stupid Backoff\" prediction", actionLink("link_to_algorithm", "algorithm"), "if a higher-order N-gram does not provide any prediction, 
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
            # -----------------------------------------------------------------
            # Tab: model
            # -----------------------------------------------------------------
            tabItem(tabName = 'model',
                    withMathJax(),
                    fluidRow(
                        box(title = "The Raw English Files", width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                            tags$p('Our English next-word-in-a-sentence predictor was trained, tested, and validated on
                            the Coursera-provided english version of the Corpus from', tags$a(href = 'www.corpora.heliohost.org', 'HC Corpora')),
                            tags$p('The Corpus contains the three files, or documents, as listed in Table 1 below:'),
                            tableOutput('enUSFilesStatsDF'),
                            tags$p('The English Corpus was partitioned as follows:'),
                            tags$ol(
                                tags$li(paste(sprintf('%.1f', sampleRate), '% for training.')),
                                tags$li(paste('Another', sprintf('%.1f', sampleRate), 'for testing.')),
                                tags$li(paste('The remaining', sprintf('%.1f', 100-2*sampleRate), 'for validation.'))
                            ),
                            tags$p('The sampled files that are used to produce the training dataset are further characterized in table 2 below:'),
                            tableOutput('enUSFilesStatsTrainingDataDF')
                            
                        )
                    ),
                    fluidRow(
                        box(title = "English Corpus for Training", width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                            includeMarkdown("training.md")
                        )
                    ),
                    fluidRow(
                        box(title = "The N-Grams Model", width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                            includeMarkdown("model.md"),
                            tags$h2("Peeking at the Model Data"),
                            tags$p("For illustration, the top-5 N-grams, ranked by descending MLE, are shown below (See also the plotting of the top-25 grams ranked by MLE and PU, further down the page.)"),
                            tags$code("head(model$unigrams[order(logprob, decreasing = TRUE)], n = 5)"),
                            tableOutput("modelTopUnigramsDT"),
                            tags$code("head(model$bigrams[order(logprob, decreasing = TRUE)], n = 5)"),
                            tableOutput("modelTopBigramsDT"),
                            tags$code("head(model$trigrams[order(logprob, decreasing = TRUE)], n = 5)"),
                            tableOutput("modelTopTrigramsDT"),
                            tags$code("head(model$quadgrams[order(logprob, decreasing = TRUE)], n = 5)"),
                            tableOutput("modelTopQuadgramsDT"),
                            tags$code("head(model$pentagrams[order(logprob, decreasing = TRUE)], n = 5)"),
                            tableOutput("modelTopPentagramsDT")
                        )
                    ),
                    fluidRow(
                        box(title = "Model's Top 25 Unigrams", width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                            plotOutput(outputId = 'unigramsPlot')
                        )
                    ),
                    fluidRow(
                        box(title = "Model's Top 25 Bigrams", width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                            plotOutput(outputId = 'bigramsPlot')
                        )
                    ),
                    fluidRow(
                        box(title = "Model's Top 25 trigrams", width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                            plotOutput(outputId = 'trigramsPlot')
                        )
                    ),
                    fluidRow(
                        box(title = "Model's Top 25 Quadgrams", width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                            plotOutput(outputId = 'quadgramsPlot')
                        )
                    ),
                    fluidRow(
                        box(title = "Model's Top 25 Pentagrams", width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                            plotOutput(outputId = 'pentagramsPlot')
                        )
                    )
            ),
            # -----------------------------------------------------------------
            # Tab: algorithm
            # -----------------------------------------------------------------
            tabItem(tabName = 'algorithm',
                    tags$p("Explain the prediction algorithm here.")
            ),
            # -----------------------------------------------------------------
            # Tab: code snippets
            # -----------------------------------------------------------------
            tabItem(tabName = 'code',
                    fluidRow(
                        box(title = "Source Control", width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                            tags$p("The entire application's code is available in Github", tags$a(href = 'https://github.com/brigaldies/NextWordPredictor', 'here'))
                        )
                    ),
                    fluidRow(
                        box(title = "Files Partitioning", width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                            includeMarkdown("partitionCorpusFile.md")
                        )
                    ),
                    fluidRow(
                        box(title = "Corpus Building and Cleaning", width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                            includeMarkdown("buildCorpusTM.md")
                        )
                    )
            ),
            # -----------------------------------------------------------------
            # Tab: References
            # -----------------------------------------------------------------
            tabItem(tabName = 'refs',
                    includeMarkdown('references.md')
            ),
            # -----------------------------------------------------------------
            # Tab: acknowledgement
            # -----------------------------------------------------------------
            tabItem(tabName = 'ack',
                    tags$p("The following information sources were instrumental in conducting the research for, and producing, this predictive application:"),
                    tags$ol(
                        tags$li('First, and foremost, my fellow class mates on the Coursera Captone class forum!'),
                        tags$li('A number of articles suggested by the Coursera Captone class forum, most notably:'),
                        tags$ul(
                            tags$li(tags$a(href = 'https://eight2late.wordpress.com/2015/05/27/a-gentle-introduction-to-text-mining-using-r/', 'Gentle Introduction to Text Mining Using R')),
                            tags$li(tags$a(href = 'https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html#plot-word-frequencies', 'Basic Text Mining in R')),
                            tags$li(tags$a(href = 'https://english.boisestate.edu/johnfry/files/2013/04/bigram-2x2.pdf', 'Bigrams and Trigrams')),
                            tags$li(tags$a(href = 'https://web.stanford.edu/~jurafsky/slp3/4.pdf', 'Speech and Language Processing, Daniel Jurafsky & James H. martin, January 9, 2015'))
                        ),
                        tags$li(tags$a(href = 'https://en.wikipedia.org/wiki/Katz%27s_back-off_model', 'The Wikipedia page on the Katz\'s Back-Off Model')),
                        tags$li('And, last, but not least, stackoverflow.com! What would we do without it?')
                    )
            ),
            # -----------------------------------------------------------------
            # Tab: contact information
            # -----------------------------------------------------------------
            tabItem(tabName = 'contact',
                    tags$p("Find me on LinkedIn", tags$a(href="https://www.linkedin.com/in/bertrandrigaldies", "here."))
            )
        )
    )
))