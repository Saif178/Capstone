library(shiny)


shinyUI(fluidPage(
    titlePanel("Predicting the Next Word"),
    fluidRow(
        column(12,
               br(),
               h4('This application tries to predict the next word in a phrase
                  as you type it'),
               br(),
               h4('To run the application, type a phrase in the box below'),
               br(),
               h4('Beneath the input box, you will see the text you have typed
                  along with a suggested'),
               h4('completion of the word you are currently typing'),
               br(),
               h4('Below that, you will see the predicted next word of your phrase'),
               br(),
               br()
               )
        ),
    
    fluidRow(
        column(6,
               textInput('input_str',
                         label='Enter some text without punctuation:',
                         value=' ')
               )
        ),
    
    fluidRow(
        column(12,
               br(),
               br(),
               br(),
               br(),
               h4('You entered (with suggested completion of your current
                  word):',style='color:red'),
               verbatimTextOutput('text1')
               )
        ),
    
    fluidRow(
        column(12,
               br(),
               br(),
               h4('Predicted next word:',style='color:blue'),
               verbatimTextOutput('text2')
               )
        )
))
