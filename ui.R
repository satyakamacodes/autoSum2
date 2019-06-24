options(shiny.maxRequestSize = 9*1024^2)

if (!require("pacman")) install.packages("pacman")

pacman::p_load(shiny,
               shinydashboard)


ui <- dashboardPage(
  
  skin = "yellow",
  
  dashboardHeader(
    title = "An intelligent application for Autism detection",
    titleWidth = 600
  ),
  
  dashboardSidebar(),
  
  dashboardBody(
    
    fluidRow(
      
      tabBox(
        width = 12,
        
        #title = "First tabBox",
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset1", 
        height = "1000px",
        tabPanel(h4("Predictions"), 
                 "First tab content"),
        tabPanel(h4("Explainable AI"), 
                 "Second tab content")
      )
      
    )
    

  )
)