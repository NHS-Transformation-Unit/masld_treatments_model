library(here)
library(shiny)

# Source the assumptions UI module
source(paste0(here(), "/src/modules/ui.R"))
source(paste0(here(), "/src/modules/server.R"))

shinyApp(ui, server)