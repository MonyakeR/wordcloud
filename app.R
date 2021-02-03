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

server = function(input, output) {
  
  data_source <- reactive({
    data <- req(input$usertext)
    my_corpus <- corpus(data)
    my_tokens <- tokens(my_corpus,
                        remove_punct = TRUE,
                        remove_symbols = TRUE,
                        remove_numbers = TRUE,
                        remove_url = TRUE)
    # make lowercase
    my_tokens <-  tokens_tolower(my_tokens, keep_acronyms =TRUE)
    # remove stop words
    my_tokens_no_stop <- tokens_remove(my_tokens,
                                       pattern = stopwords("en"))
    
    # construct the document feature matrix
    dfmat <- dfm(my_tokens_no_stop)
    
    # get frequencies
    term_frequency <- textstat_frequency(dfmat)
    
    # create df for wordcloud
    word_df <- as_tibble(term_frequency) %>% 
      rename(word = feature,
             freq = frequency) %>% 
      select(word, freq)
    return(word_df)
  })
  
  output$mywordcloud <- renderWordcloud2({
    wordcloud2(data_source())
  })
}

shinyApp(ui, server)