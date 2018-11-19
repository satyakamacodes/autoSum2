options(shiny.maxRequestSize = 9*1024^2)

if (!require("pacman")) install.packages("pacman")

pacman::p_load(shiny,
               shinydashboard,
               micropan,
               quanteda,
               lexRankr,
               udpipe)

ui <- dashboardPage(
     
     dashboardHeader(
          title = "AutoSumm",
          titleWidth = 150),
     
     dashboardSidebar(
          width = 150
     ),
     
     dashboardBody(
          
          fileInput(inputId = "long_content",
                    label = "Upload your content",
                    multiple = FALSE,
                    accept = c(".txt"),
                    placeholder = "Upload a file with .txt"),
          
          fluidRow(
               column(width = 12,
                      infoBoxOutput("o_number_words",
                                    width = 6),
                      infoBoxOutput("o_number_sentences",
                                    width = 6))
          ),
          

          
          sliderInput(inputId = "no._summarization_sentences",
                      label = "Number of sentences required in summary",
                      min = 4,
                      max = 10,
                      value = 6,
                      step = 1,
                      ticks = F),
          
          fluidRow(
               column(width = 8,
                      verbatimTextOutput("o_summary"))
          )

     )
)