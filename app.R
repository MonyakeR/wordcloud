library(dplyr)
library(wordcloud2)
library(quanteda)
library(quanteda.textstats)
library(readtext)
library(stringr)
library(shiny)
library(colourpicker)
library(htmlwidgets)
library(shinyWidgets)
library(webshot)
library(shinythemes)
library(reactable)
library(htmltools)
library(shinydashboard)
webshot::install_phantomjs()


# Render a bar chart with a label on the left
bar_chart <- function(label, width = "100%", height = "14px", fill = "#00bfc4", background = NULL) {
  bar <- div(style = list(background = fill, width = width, height = height))
  chart <- div(style = list(flexGrow = 1, marginLeft = "6px", background = background), bar)
  div(style = list(display = "flex", alignItems = "center"), label, chart)
}

ui <- navbarPage(
  theme = shinytheme("flatly"),
  "Word Cloud Generator",
  id = "tabs",
  
  tabPanel("Text Input",
           textAreaInput("usertext", label = NULL,
                         placeholder = "Type or paste your text here or upload a file to generate a word cloud",
                         height = '400px') %>% 
             shiny::tagAppendAttributes(style = 'width:100%;'),
           
           fileInput("upload", "Upload a text file", accept = c(".csv", ".txt")),
           
           
           tags$style(
             
             ".progress-bar {
                    background-color: #12c462;
                    font-size: 12px;
                    line-height: 12px;
                }"
           )
           
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
                                label = "Shape",
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
                                label = "Background Colour",
                                returnName = FALSE,
                                palette = "square",
                                closeOnClick = TRUE,
                              )
                       ),
                       column(2,
                              selectInput(
                                inputId = "colour",
                                label = "Text Colour",
                                choices = list(
                                  "Random-dark" = "random-dark",
                                  "Random-light" = "random-light"
                                ),
                                selected = "random-dark"
                              )
                              
                       ),
                       column(2,
                         selectInput(
                           "min_freq",
                           "Min Frequency",
                           selected = 1,
                           choices = c(1, 2, 3, 4, 5)
                         )
                       ),
                       column(2,
                              downloadButton("wordcloud_download",
                                             label = "Download",
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
                              wordcloud2Output("mywordcloud", width = "auto",
                              height = "600px")
                       )
                     )
           )
  ),
  
  tabPanel("Keyword-In-Context",
           fluidRow(
             column(
               4,
               reactableOutput("table")
             ),
             column(
               8,
               style = "
                      box-shadow: 0 2px 4px 0;
                      border-radius: 5px;
                      padding: 5px;
                       ",
               
               fluidRow(
                 column(
                   6,
                   tags$style("
                              .row {
                                margin-right: 0;
                                margin-left: 0;
                              }
                              "),
                   tags$style(
                     ".form-control.shiny-bound-input {
                        height: 34px;
                     }
                     "
                   ),
                   textInput(
                     "keyword",
                     label = "Enter keyword to get context",
                     width = "100%"
                   )
                 ),
                 column(
                   6, 
                   selectInput("window",
                                "Change window size",
                                selected = 5,
                               choices = c(3, 4, 5, 6, 7, 8, 9, 10))
                 ),
                 fluidRow(
                   column(
                     12,
                     reactableOutput("keyword_context"),
                     tags$style(
                       ".keyword {
                          display: inline-block;
                            padding: 2px 12px;
                            border-radius: 15px;
                            font-weight: 600;
                            font-size: 12px;
                            background: hsl(116, 60%, 90%);
                            color: hsl(116, 30%, 25%);
                          }
                        "
                     )
                   )
                 )
               )
             )
           )
  )
  
  
)

server = function(input, output, session) {
  
  to_listen <- reactive({
    list(input$usertext, input$upload)
  })
  
  observeEvent(to_listen(), {
    # check if data has been pasted or uploaded
    if (str_length(input$usertext) < 1 & is.null(input$upload)) {
      hideTab(inputId = "tabs", target = "Word Cloud")
      hideTab(inputId = "tabs", target = "Keyword-In-Context")
    } else {
      showTab(inputId = "tabs", target = "Word Cloud")
      showTab(inputId = "tabs", target = "Keyword-In-Context")
      updateNavbarPage(session, "tabs", selected = "Word Cloud")
    }
  })
  
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
  })
  
  word_cloud_df <- reactive({
    
    # create a quanteda corpus using the input text
    my_corpus <- corpus(data_source())
    
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
      select(word, freq) %>% 
      filter(str_length(word) > 2 & freq > input$min_freq)
    
    return(word_df)
    
  })
  
  word_cloud <- reactive({
    wordcloud2(
      word_cloud_df(),
      fontFamily = input$font,
      backgroundColor = input$background,
      color = input$colour,
      shape = input$shape,
      rotateRatio = 0,
      size = 0.55
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
  
  # plot frequency table
  output$table <- renderReactable({
    word_cloud_df() %>% 
      filter(freq > 1 & str_length(word) > 2) %>% 
      reactable(
        defaultPageSize = 15,
        defaultSorted = "freq",
        columns = list(
          word = colDef(
            name = "Word"
          ),
          freq = colDef(
            name = "Frequency",
            defaultSortOrder = "desc",
            # Render the bar charts using custom cell render function
            cell = function(value) {
              width <- paste0(value * 100 / max(word_cloud_df()$freq), "%")
              # Fix each label using the width of the widest number
              value <- format(value, width = 3, justify = "right")
              bar_chart(value, width = width, fill  = "#12c462")
            },
            # And left-align the columns
            align = "left",
            # Use the operating system's default monospace font, and
            # preserve white space to prevent it from being collapsed by default
            style = list(fontFamily = "monospace", whiteSpace = "pre")
          )
        )
      )
  })
  
  # get keyword in context
  output$keyword_context <- renderReactable({
    
    my_corpus <- corpus(data_source())
    # create tokens
    toks <- tokens(
      my_corpus,
      remove_symbols = TRUE,
      padding = TRUE
    )
    req(input$keyword)
    kw <- kwic(toks, pattern = phrase(input$keyword),
               window = input$window)
    data.frame(kw) %>%
      select(pre, keyword, post)%>%
      reactable(
        defaultPageSize = 13,
        columns = list(
          keyword = colDef(cell = function(value) {
            class <- "keyword"
            div(class = class, value)
          })
        )
      )
  })
}

shinyApp(ui, server)