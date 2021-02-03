library(tidyverse)
library(wordcloud2)
library(quanteda)
library(shiny)

ui <- fluidPage(
  titlePanel("Free Word Cloud Generator"),
  
  tabsetPanel(
    
    tabPanel("Text Input",
             textAreaInput("usertext", "",
        
                           height = '400px') %>% 
               shiny::tagAppendAttributes(style = 'width:100%;'),
             ),
    
    tabPanel("Word Cloud",
             sidebarLayout(
               
               sidebarPanel(
                 helpText("Enter text to generate wordcloud")
               ),
               mainPanel(
                 wordcloud2Output("mywordcloud"))
               )
      )
  )
)

server = function(input, output, session) {
  
  data_source <- reactive({
    
    data <- req(input$usertext)
    
    # create a quanteda corpus using the input text
    my_corpus <- corpus(data)
    
    # construct the document feature matrix
    
    dfmat <- my_corpus %>% 
      tokens(remove_punct = TRUE,
             remove_symbols = TRUE,
             remove_numbers = TRUE,
             remove_url = TRUE) %>% 
      tokens_tolower(keep_acronyms =TRUE) %>% 
      tokens_remove(pattern = stopwords("en")) %>% 
      dfm()
    
    # get frequencies
    term_frequency <- textstat_frequency(dfmat)
    
    # create data frame for word cloud
    word_df <- as_tibble(term_frequency) %>% 
      rename(word = feature,freq = frequency) %>% 
      select(word, freq)
    return(word_df)
  })
  
  output$mywordcloud <- renderWordcloud2({
    wordcloud2(data_source())
  })
}

shinyApp(ui, server)