library(shiny)
library(here)


# Source the assumptions UI module
source(paste0(here(), "/src/requirements/packages.R"))
source(paste0(here(), "/src/modules/intro_ui.R"))
source(paste0(here(), "/src/modules/assumptions_ui.R"))
source(paste0(here(), "/src/modules/treatments_ui.R"))
source(paste0(here(), "/src/modules/semaglutide_pathway_activities_ui.R"))
source(paste0(here(), "/src/modules/activities_costings_ui.R"))
source(paste0(here(), "/src/modules/assumptions_server.R"))
source(paste0(here(), "/src/modules/activities_costings_server.R"))

# Create a temporary app to display just the UI
shinyApp(
  ui = navbarPage(
    title = "MASLD Treatment Activity and Costs Modelling",
    id = "navbar",
    intro_ui("intro"),
    assumptions_ui("assumptions"),
    treatments_ui("treatments"),
    semaglutide_pathway_activities_ui("semaglutide_pathway_activities"),
    activities_costing_ui("activities_costing")),
  server = function(input, output, session) {
    assumptions_list <- assumptions_server("assumptions")
  }
)
