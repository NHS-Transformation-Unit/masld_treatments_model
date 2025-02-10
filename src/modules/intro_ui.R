library(shiny)

intro_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(title = "Introduction",
           fluidPage()
  )
}