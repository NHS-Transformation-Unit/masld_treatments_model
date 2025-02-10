library(shiny)
library(here)


# Source the assumptions UI module
source(paste0(here(), "/src/modules/intro_ui.R"))
source(paste0(here(), "/src/modules/assumptions_ui.R"))
source(paste0(here(), "/src/modules/pathway_activities_ui.R"))
source(paste0(here(), "/src/modules/assumptions_server.R"))

# Create a temporary app to display just the UI
shinyApp(
  ui = navbarPage(
    title = "MASLD Treatment Activity and Costs Modelling",
    id = "navbar",
    intro_ui("intro"),
    assumptions_ui("assumptions"),
    pathway_activities_ui("pathway_activities")),
  server = function(input, output, session) {
    assumptions_server("assumptions")
  }
)
