options(shiny.maxRequestSize = 9*1024^2)

if (!require("pacman")) install.packages("pacman")

pacman::p_load(shiny,
               shinydashboard)

server <- function(input, output, session){
  
  reactive_content <- reactive({
    
    #Content is the original long text input on which the summary will be created
    
    filePath <- input$long_content$datapath
    
    if (is.null(filePath)){
      return(NULL)}
    
    else{
      fileText <- paste(readLines(filePath,
                                  warn = F), collapse = "\n")
      fileText <- as.character(fileText)
    }
  })
  
  #Viewing the (original) content
  output$o_content <- renderText({
    
    #Removing punctuations
    reactive_content() <- gsub('[[:punct:] ]+',
                               ' ',
                               reactive_content())
    
    reactive_content()
  })
  
  #Number of words in the original content
  
  output$o_number_words <- renderInfoBox({
    infoBox(title = "Number of words in the original content",
            value = sapply(strsplit(as.character(reactive_content()),
                                    " "), 
                           length),
            color = "red",
            icon = icon("info"),
            width = 2
    )
  })
  

  
  #Number of sentences in the original content
  
  output$o_number_sentences <- renderInfoBox({
    
    infoBox(title = "Number of sentences in the original content",
            value = length(sent_detect(reactive_content())),
            color = "blue",
            icon = icon("info"),
            width = 2)
    
  })
  
  #Summarization based on lexical ranking
  
  output$o_summary <- renderPrint({
    
    if (is.null(reactive_content())){
      return("NO DATA IS AVAILABLE TO DO SUMMARIZATION")}
    
    else{
    agg_summary <- lexRank(text = reactive_content(),
                           n = input$no._summarization_sentences,
                           usePageRank = T)
    
    agg_summary$sentence
    }
  })
  
  
  #Plutchik sentiment
  
  output$sentiment_plutchik <- renderDataTable({

    stopwords_regex = paste(stopwords('en'),
                            collapse = '\\b|\\b')

    stopwords_regex = paste0('\\b',
                             stopwords_regex,
                             '\\b')

    documents = stringr::str_replace_all(reactive_content(),
                                         stopwords_regex,
                                         '')

    bb <- classify_emotion(documents)

    # bb <- as.data.frame(bb)
    # 
    # bb <- bb[1, 1:5]

  })
  
  
  
  
  
  
  
  
  
  
  
  
  
}