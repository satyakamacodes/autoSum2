options(shiny.maxRequestSize = 9*1024^2)

if (!require("pacman")) install.packages("pacman")

pacman::p_load(shiny,
               shinydashboard)

shinyServer(function(input, output)
  {
  
  
  
})