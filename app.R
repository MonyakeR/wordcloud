library(tidyverse)
library(wordcloud2)
library(quanteda)
library(shiny)
library(colourpicker)
library(htmlwidgets)
library(webshot)
#webshot::install_phantomjs()

ui <- fluidPage(
  titlePanel("Free Word Cloud Generator"),
  
  tabsetPanel(
    
    tabPanel("Text Input",
             textAreaInput("usertext", "",
        
                           height = '400px') %>% 
               shiny::tagAppendAttributes(style = 'width:100%;'),
             ),
    
    tabPanel("Word Cloud",
             
             tags$div( style = "padding:10px",
               
  
               sidebarLayout(
                 
                 sidebarPanel(
                   
                   helpText(h3("Use the options below to customise the word cloud")),
                   
                   selectInput(
                     inputId = "font",
                     label = "Font family",
                     choices = list(
                       "sans-serif",
                       "monospace",
                       "cursive",
                       "Playfair display",
                       "Open Sans",
                       "Poppins",
                       "Rubik",
                       "Montserrat",
                       "Oswald",
                       "Quicksand",
                       "fantasy",
                       "system-ui",
                       "ui-serif",
                       "ui-sans-serif",
                       "ui-monospace",
                       "ui-rounded",
                       "emoji",
                       "math",
                       "fangsong"
                     ),
                     selected = "sans-serif"
                   ),
                   
                   selectInput(
                     inputId = "shape",
                     label = "Shape of the cloud to draw",
                     choices = list(
                       "circle",
                       "cardioid",
                       "diamond",
                       "triangle-forward",
                       "triangle",
                       "pentagon",
                       "star"
                     ),
                     selected = "circle"
                   ),
                   
                   colourInput(
                     inputId = "background",
                     label = "Colour of the background",
                     returnName = TRUE,
                     palette = "square",
                     closeOnClick = TRUE
                   ),
                   
                   selectInput(
                     inputId = "colour",
                     label = "Colour of text",
                     choices = list(
                       "random-dark",
                       "random-light"
                     ),
                     selected = "random-dark"
                   ),
                   
                   downloadButton("wordcloud_download",
                                  label = "Download Word Cloud",
                                  class = "btn-block")
                   
                 ),
                 mainPanel(
                   wordcloud2Output("mywordcloud", width = "100%",
                                    height = "460px")
                   )
               )
               
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
  
  word_cloud <- reactive({
    wordcloud2(
      data_source(),
      fontFamily = input$font,
      backgroundColor = input$background,
      color = input$colour,
      shape = input$shape,
      rotateRatio = 0
    )
  })
  
  output$mywordcloud <- renderWordcloud2({
    word_cloud()
  })
  
  output$wordcloud_download <- downloadHandler(
    
    filename = function() paste("wordcloud", ".png", sep=""), 
    
    content = function(file) {
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      saveWidget(word_cloud(), "temp.html", selfcontained = F)
      webshot("temp.html", file = file, delay = 5,
              cliprect = "viewport")

    }
  )
}

shinyApp(ui, server)