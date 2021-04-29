library(tidyverse)
library(wordcloud2)
library(quanteda)
library(readtext)
library(stringr)
library(shiny)
library(colourpicker)
library(htmlwidgets)
library(webshot)
library(shinythemes)
webshot::install_phantomjs()

ui <- fluidPage(

  theme = shinytheme("flatly"),
  titlePanel("Word Cloud Generator"),
  
  tabsetPanel(
    
    tabPanel("Text Input",
             textAreaInput("usertext", label = NULL,
                           placeholder = "Type or paste your text here or upload a file to generate a word cloud",
                           height = '400px') %>% 
               shiny::tagAppendAttributes(style = 'width:100%;'),
             
             fileInput("upload", "Upload a text file", accept = c(".csv", ".txt"))
             ),
    
    tabPanel("Word Cloud",
             
             tags$div( style = "padding:10px",
                       
              fluidRow(
                column(
                  2,
                  selectInput(
                    inputId = "font",
                    label = "Font family",
                    choices = list(
                      "Sans serif" = "sans-serif",
                      "Monospace" = "monospace",
                      "Cursive" = "cursive",
                      "Playfair display" = "Playfair display",
                      "Open Sans" = "Open Sans",
                      "Poppins" = "Poppins",
                      "Rubik" = "Rubik",
                      "Montserrat" = "Montserrat",
                      "Oswald" = "Oswald",
                      "Quicksand" = "Quicksand",
                      "Fantasy" = "fantasy",
                      "System-ui" = "system-ui",
                      "Ui-serif" = "ui-serif",
                      "Ui-sans-serif" = "ui-sans-serif",
                      "Ui-monospace" = "ui-monospace",
                      "Ui-rounded" = "ui-rounded",
                      "Emoji" = "emoji",
                      "Math" = "math",
                      "Fangsong" = "fangsong"
                    ),
                    selected = "sans-serif"
                  )
                ),
                column(2,
                       selectInput(
                         inputId = "shape",
                         label = "Shape of the cloud to draw",
                         choices = list(
                           "Circle" = "circle",
                           "Cardioid" = "cardioid",
                           "Diamond" = "diamond",
                           "Triangle-forward" = "triangle-forward",
                           "Triangle" = "triangle",
                           "Pentagon" = "pentagon",
                           "Star" = "star"
                         ),
                         selected = "circle"
                       )
                  ),
                  column(2,
                         tags$style(
                           ".form-control.shiny-colour-input.colourpicker-input {
                               padding: 6px 12px;
                               height: 34px;
                               border: 1px solid #dce4ec;
                             }
                           .selectize-input {
                              border: 1px solid #dce4ec;
                           }
                           "
                         ),
                         colourInput(
                           inputId = "background",
                           label = "Colour of the background",
                           returnName = FALSE,
                           palette = "square",
                           closeOnClick = TRUE,
                         )
                       ),
                column(2,
                       selectInput(
                         inputId = "colour",
                         label = "Colour of text",
                         choices = list(
                           "Random-dark" = "random-dark",
                           "Random-light" = "random-light"
                         ),
                         selected = "random-dark"
                       )
                       
                  ),
                column(4,
                       downloadButton("wordcloud_download",
                                      label = "Download Word Cloud",
                                      class = "btn-block"),
                       tags$style(
                         ".shiny-download-link {
                            padding: 4px 2px 4px 2px;
                            margin-top: 26px; 
                            background-color: #154c79;
                            border-color: #154c79;
                         }
                         
                         .shiny-download-link:hover {
                            background-color: #4682B4;
                            border-color: #4682B4;
                         }"
                       )
                       
                    )
              ),
              
              fluidRow(
                column(12,
                       wordcloud2Output("mywordcloud", width = "100%",
                                        height = "768px")
                )
              )
          )
      )
  )
)

server = function(input, output, session) {
  
  data_source <- reactive({
    
    pasted_data <- input$usertext
    
    if(str_length(pasted_data) > 1) {
      data <- pasted_data
    } else {
      req(input$upload)
      # read in the text file content
      uploaded_data <- readtext(input$upload$datapath)
      data <- uploaded_data$text
    }
    
    # create a quanteda corpus using the input text
    my_corpus <- corpus(data)
    
    # create tokens
    toks <- tokens(
      my_corpus,
      remove_punct = TRUE,
      remove_symbols = TRUE,
      padding = TRUE
    ) %>% 
      tokens_remove(stopwords("en"), padding = TRUE)
    
    # Get compound words
    
    colls <- textstat_collocations(toks, size = c(2,3),  min_count = 2, tolower = TRUE)
    
    toks_comp <- tokens_compound(
      toks,
      pattern = colls,
      case_insensitive = TRUE,
    )
    
    # construct the document feature matrix
    
    dfmat <- dfm(
      toks_comp
    )
    
    
    # get frequencies
    
    term_frequency <- textstat_frequency(dfmat)
    
    # Split strings
    
    term_frequency <- term_frequency %>% 
      filter(feature != "") %>% 
      mutate(
        feature = str_replace_all(feature, "_"," "),
      )
    
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
      rotateRatio = 0,
      size = 0.9
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
      webshot("temp.html", file = file, delay = 10,
              cliprect = "viewport")

    }
  )
  
  # plot frequency
  output$frequency <- renderPlot({
    data_source() %>% 
      filter(freq > 1) %>% 
      ggplot(aes(word, freq)) + 
      geom_point() + 
      coord_flip()
  })
}

shinyApp(ui, server)