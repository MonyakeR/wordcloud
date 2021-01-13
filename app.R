library(tidyverse)
library(shiny)


ui <- fluidPage(
  titlePanel("Free WordCloud Generator"),
  
  sidebarLayout(
    
    sidebarPanel(
      helpText("Enter text to generate wordcloud")
    ),
    mainPanel(
      
      tabsetPanel(
        type = "tabs",
        tabPanel("Text Input",
                 textAreaInput("UserText", "Enter your text:",
                               "Type or paste your text here or upload a file to generate a word cloud", 
                                height = '400px') %>% 
                   shiny::tagAppendAttributes(style = 'width:100%;')),
        tabPanel("Word Cloud", plotOutput("wordcloud"))
      )
      
    )
  )
)

server = function(input, output) {
  
}

shinyApp(ui, server)