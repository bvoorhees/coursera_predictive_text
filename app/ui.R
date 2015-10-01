
library(shiny)

# Define UI for dataset viewer application
shinyUI(
          pageWithSidebar(
                    # Application title
                    headerPanel("PredIt! - A word prediction app.",),
                   

                    sidebarPanel(
                              textInput(inputId = 'text',label = "Input text here:", value = 'predict the')
                    ),
                    mainPanel(
                              h2('About and how to use this application:'),
                              p("What is the next word in the sentance? Simply enter in the text on the left to find out!"),
                              p("A Naive Bayes classifier will predict the next word given the features in the text you have entered!"),
                              h2('Results:'),
                              h3("You've entered"),
                              verbatimTextOutput("inputValue"),
                              h3('Which resulted in a single word prediction of '),
                              verbatimTextOutput("prediction"),
                              h3('...and some additional suggestions'),
                              verbatimTextOutput("other_suggestions")
                    )
          )
)

