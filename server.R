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
          fileText <- paste(readLines(filePath), collapse = "\n")
          fileText <- as.character(fileText)
          }
     })
     
     #Viewing the (original) content
     output$o_content <- renderText({
          
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
                  value = nsentence(as.character(reactive_content())),
                  color = "blue",
                  icon = icon("info"),
                  width = 2
          )
     })
     
     #Summarization based on lexical ranking
     
     output$o_summary <- renderPrint({
          
          agg_summary <- lexRank(text = reactive_content(),
                                 n = input$no._summarization_sentences,
                                 usePageRank = T)
          
          agg_summary$sentence
     })
     

     
     
     

     
}