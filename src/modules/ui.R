library(shiny)
library(scales)
library(bslib)
library(DT)
library(shinyMatrix)

ui <- fluidPage( 
  
  tags$link(rel = "stylesheet", type = "text/css", href = "config/app_theme.css"),
  
  tags$head(tags$style(HTML("
  #mm_assess_setting_sem table {
    table-layout: fixed;
    width: 100%;
    font-size: 12px; /* Adjust font size for entire table */
  }

  #mm_assess_setting_sem th {
    white-space: normal !important; 
    word-wrap: break-word !important;
    text-align: center;
    font-size: 12px; /* Make column headers smaller */
  }

  /* Apply wrapping only to the first column (row labels) */
  #mm_assess_setting_sem td:first-child {
    max-width: 180px; /* Adjust width as needed */
    white-space: normal !important;
    word-wrap: break-word !important;
    word-break: break-word !important;
    font-size: 12px; /* Make first column text smaller */
  }

  /* Adjust font size for all table cells */
  #mm_assess_setting_sem td {
    font-size: 12px; /* Adjust font size */
  }
"))),
  
  tags$head(tags$style(HTML("
  #treatment_setting_0_16_matrix_sem table {
    table-layout: fixed;
    width: 100%;
    font-size: 12px; /* Adjust font size for entire table */
  }

  #treatment_setting_0_16_matrix_sem th {
    white-space: normal !important; 
    word-wrap: break-word !important;
    text-align: center;
    font-size: 12px; /* Make column headers smaller */
  }

  /* Apply wrapping only to the first column (row labels) */
  #treatment_setting_0_16_matrix_sem td:first-child {
    max-width: 180px; /* Adjust width as needed */
    white-space: normal !important;
    word-wrap: break-word !important;
    word-break: break-word !important;
    font-size: 12px; /* Make first column text smaller */
  }

  /* Adjust font size for all table cells */
  #treatment_setting_0_16_matrix_sem td {
    font-size: 12px; /* Adjust font size */
  }
"))),
  
  tags$head(tags$style(HTML("
  #semaglutide_20_71_delivery_setting table {
    table-layout: fixed;
    width: 100%;
    font-size: 12px; /* Adjust font size for entire table */
  }

  #semaglutide_20_71_delivery_setting th {
    white-space: normal !important; 
    word-wrap: break-word !important;
    text-align: center;
    font-size: 12px; /* Make column headers smaller */
  }

  /* Apply wrapping only to the first column (row labels) */
  #semaglutide_20_71_delivery_setting td:first-child {
    max-width: 180px; /* Adjust width as needed */
    white-space: normal !important;
    word-wrap: break-word !important;
    word-break: break-word !important;
    font-size: 12px; /* Make first column text smaller */
  }

  /* Adjust font size for all table cells */
  #semaglutide_20_71_delivery_setting td {
    font-size: 12px; /* Adjust font size */
  }
"))),
  
  
  tags$head(tags$style(HTML("
  #continuation_delivery_setting_sem table {
    table-layout: fixed;
    width: 100%;
    font-size: 12px; /* Adjust font size for entire table */
  }

  #continuation_delivery_setting_sem th {
    white-space: normal !important; 
    word-wrap: break-word !important;
    text-align: center;
    font-size: 12px; /* Make column headers smaller */
  }

  /* Apply wrapping only to the first column (row labels) */
  #continuation_delivery_setting_sem td:first-child {
    max-width: 180px; /* Adjust width as needed */
    white-space: normal !important;
    word-wrap: break-word !important;
    word-break: break-word !important;
    font-size: 12px; /* Make first column text smaller */
  }

  /* Adjust font size for all table cells */
  #continuation_delivery_setting_sem td {
    font-size: 12px; /* Adjust font size */
  }
"))),
  
  tags$head(tags$style(HTML("
  #semaglutide_73_103_delivery_setting table {
    table-layout: fixed;
    width: 100%;
    font-size: 12px; /* Adjust font size for entire table */
  }

  #semaglutide_73_103_delivery_setting th {
    white-space: normal !important; 
    word-wrap: break-word !important;
    text-align: center;
    font-size: 12px; /* Make column headers smaller */
  }

  /* Apply wrapping only to the first column (row labels) */
  #semaglutide_73_103_delivery_setting td:first-child {
    max-width: 180px; /* Adjust width as needed */
    white-space: normal !important;
    word-wrap: break-word !important;
    word-break: break-word !important;
    font-size: 12px; /* Make first column text smaller */
  }

  /* Adjust font size for all table cells */
  #semaglutide_73_103_delivery_setting td {
    font-size: 12px; /* Adjust font size */
  }
"))),
  
  tags$head(tags$style(HTML("
  #semaglutide_ongoing_delivery_setting table {
    table-layout: fixed;
    width: 100%;
    font-size: 12px; /* Adjust font size for entire table */
  }

  #semaglutide_ongoing_delivery_setting th {
    white-space: normal !important; 
    word-wrap: break-word !important;
    text-align: center;
    font-size: 12px; /* Make column headers smaller */
  }

  /* Apply wrapping only to the first column (row labels) */
  #semaglutide_ongoing_delivery_setting td:first-child {
    max-width: 180px; /* Adjust width as needed */
    white-space: normal !important;
    word-wrap: break-word !important;
    word-break: break-word !important;
    font-size: 12px; /* Make first column text smaller */
  }

  /* Adjust font size for all table cells */
  #semaglutide_ongoing_delivery_setting td {
    font-size: 12px; /* Adjust font size */
  }
"))),
  
  tags$head(tags$style(HTML("
  #mm_assess_setting_surv table {
    table-layout: fixed;
    width: 100%;
    font-size: 12px; /* Adjust font size for entire table */
  }

  #mm_assess_setting_surv th {
    white-space: normal !important; 
    word-wrap: break-word !important;
    text-align: center;
    font-size: 12px; /* Make column headers smaller */
  }

  /* Apply wrapping only to the first column (row labels) */
  #mm_assess_setting_surv td:first-child {
    max-width: 180px; /* Adjust width as needed */
    white-space: normal !important;
    word-wrap: break-word !important;
    word-break: break-word !important;
    font-size: 12px; /* Make first column text smaller */
  }

  /* Adjust font size for all table cells */
  #mm_assess_setting_surv td {
    font-size: 12px; /* Adjust font size */
  }
"))),
  
  tags$head(tags$style(HTML("
  #treatment_setting_0_24_matrix_surv table {
    table-layout: fixed;
    width: 100%;
    font-size: 12px; /* Adjust font size for entire table */
  }

  #treatment_setting_0_24_matrix_surv th {
    white-space: normal !important; 
    word-wrap: break-word !important;
    text-align: center;
    font-size: 12px; /* Make column headers smaller */
  }

  /* Apply wrapping only to the first column (row labels) */
  #treatment_setting_0_24_matrix_surv td:first-child {
    max-width: 180px; /* Adjust width as needed */
    white-space: normal !important;
    word-wrap: break-word !important;
    word-break: break-word !important;
    font-size: 12px; /* Make first column text smaller */
  }

  /* Adjust font size for all table cells */
  #treatment_setting_0_24_matrix_surv td {
    font-size: 12px; /* Adjust font size */
  }
"))),
  
  tags$head(tags$style(HTML("
  #treatment_setting_25_71_matrix_surv table {
    table-layout: fixed;
    width: 100%;
    font-size: 12px; /* Adjust font size for entire table */
  }

  #treatment_setting_25_71_matrix_surv th {
    white-space: normal !important; 
    word-wrap: break-word !important;
    text-align: center;
    font-size: 12px; /* Make column headers smaller */
  }

  /* Apply wrapping only to the first column (row labels) */
  #treatment_setting_25_71_matrix_surv td:first-child {
    max-width: 180px; /* Adjust width as needed */
    white-space: normal !important;
    word-wrap: break-word !important;
    word-break: break-word !important;
    font-size: 12px; /* Make first column text smaller */
  }

  /* Adjust font size for all table cells */
  #treatment_setting_25_71_matrix_surv td {
    font-size: 12px; /* Adjust font size */
  }
"))),
  
  
  tags$head(tags$style(HTML("
  #continuation_delivery_setting_surv table {
    table-layout: fixed;
    width: 100%;
    font-size: 12px; /* Adjust font size for entire table */
  }

  #continuation_delivery_setting_surv th {
    white-space: normal !important; 
    word-wrap: break-word !important;
    text-align: center;
    font-size: 12px; /* Make column headers smaller */
  }

  /* Apply wrapping only to the first column (row labels) */
  #continuation_delivery_setting_surv td:first-child {
    max-width: 180px; /* Adjust width as needed */
    white-space: normal !important;
    word-wrap: break-word !important;
    word-break: break-word !important;
    font-size: 12px; /* Make first column text smaller */
  }

  /* Adjust font size for all table cells */
  #continuation_delivery_setting_surv td {
    font-size: 12px; /* Adjust font size */
  }
"))),
  
  tags$head(tags$style(HTML("
  #treatment_setting_73_103_matrix_surv table {
    table-layout: fixed;
    width: 100%;
    font-size: 12px; /* Adjust font size for entire table */
  }

  #treatment_setting_73_103_matrix_surv th {
    white-space: normal !important; 
    word-wrap: break-word !important;
    text-align: center;
    font-size: 12px; /* Make column headers smaller */
  }

  /* Apply wrapping only to the first column (row labels) */
  #treatment_setting_73_103_matrix_surv td:first-child {
    max-width: 180px; /* Adjust width as needed */
    white-space: normal !important;
    word-wrap: break-word !important;
    word-break: break-word !important;
    font-size: 12px; /* Make first column text smaller */
  }

  /* Adjust font size for all table cells */
  #treatment_setting_73_103_matrix_surv td {
    font-size: 12px; /* Adjust font size */
  }
"))),
  
  tags$head(tags$style(HTML("
  #survodutide_ongoing_delivery_setting table {
    table-layout: fixed;
    width: 100%;
    font-size: 12px; /* Adjust font size for entire table */
  }

  #survodutide_ongoing_delivery_setting th {
    white-space: normal !important; 
    word-wrap: break-word !important;
    text-align: center;
    font-size: 12px; /* Make column headers smaller */
  }

  /* Apply wrapping only to the first column (row labels) */
  #survodutide_ongoing_delivery_setting td:first-child {
    max-width: 180px; /* Adjust width as needed */
    white-space: normal !important;
    word-wrap: break-word !important;
    word-break: break-word !important;
    font-size: 12px; /* Make first column text smaller */
  }

  /* Adjust font size for all table cells */
  #survodutide_ongoing_delivery_setting td {
    font-size: 12px; /* Adjust font size */
  }
"))),
  
  tags$head(tags$style(HTML("
  #mm_assess_setting_res table {
    table-layout: fixed;
    width: 100%;
    font-size: 12px; /* Adjust font size for entire table */
  }

  #mm_assess_setting_res th {
    white-space: normal !important; 
    word-wrap: break-word !important;
    text-align: center;
    font-size: 12px; /* Make column headers smaller */
  }

  /* Apply wrapping only to the first column (row labels) */
  #mm_assess_setting_res td:first-child {
    max-width: 180px; /* Adjust width as needed */
    white-space: normal !important;
    word-wrap: break-word !important;
    word-break: break-word !important;
    font-size: 12px; /* Make first column text smaller */
  }

  /* Adjust font size for all table cells */
  #mm_assess_setting_res td {
    font-size: 12px; /* Adjust font size */
  }
"))),
  
  tags$head(tags$style(HTML("
  #treatment_setting_0_52_matrix_res table {
    table-layout: fixed;
    width: 100%;
    font-size: 12px; /* Adjust font size for entire table */
  }

  #treatment_setting_0_52_matrix_res th {
    white-space: normal !important; 
    word-wrap: break-word !important;
    text-align: center;
    font-size: 12px; /* Make column headers smaller */
  }

  /* Apply wrapping only to the first column (row labels) */
  #treatment_setting_0_52_matrix_res td:first-child {
    max-width: 180px; /* Adjust width as needed */
    white-space: normal !important;
    word-wrap: break-word !important;
    word-break: break-word !important;
    font-size: 12px; /* Make first column text smaller */
  }

  /* Adjust font size for all table cells */
  #treatment_setting_0_52_matrix_res td {
    font-size: 12px; /* Adjust font size */
  }
"))),
  
  tags$head(tags$style(HTML("
  #continuation_52_delivery_setting_res table {
    table-layout: fixed;
    width: 100%;
    font-size: 12px; /* Adjust font size for entire table */
  }

  #continuation_52_delivery_setting_res th {
    white-space: normal !important; 
    word-wrap: break-word !important;
    text-align: center;
    font-size: 12px; /* Make column headers smaller */
  }

  /* Apply wrapping only to the first column (row labels) */
  #continuation_52_delivery_setting_res td:first-child {
    max-width: 180px; /* Adjust width as needed */
    white-space: normal !important;
    word-wrap: break-word !important;
    word-break: break-word !important;
    font-size: 12px; /* Make first column text smaller */
  }

  /* Adjust font size for all table cells */
  #continuation_52_delivery_setting_res td {
    font-size: 12px; /* Adjust font size */
  }
"))),
  
  tags$head(tags$style(HTML("
  #treatment_setting_52_71_matrix_res table {
    table-layout: fixed;
    width: 100%;
    font-size: 12px; /* Adjust font size for entire table */
  }

  #treatment_setting_52_71_matrix_res th {
    white-space: normal !important; 
    word-wrap: break-word !important;
    text-align: center;
    font-size: 12px; /* Make column headers smaller */
  }

  /* Apply wrapping only to the first column (row labels) */
  #treatment_setting_52_71_matrix_res td:first-child {
    max-width: 180px; /* Adjust width as needed */
    white-space: normal !important;
    word-wrap: break-word !important;
    word-break: break-word !important;
    font-size: 12px; /* Make first column text smaller */
  }

  /* Adjust font size for all table cells */
  #treatment_setting_52_71_matrix_res td {
    font-size: 12px; /* Adjust font size */
  }
"))),

  
  
  tags$head(tags$style(HTML("
  #continuation_delivery_setting_res table {
    table-layout: fixed;
    width: 100%;
    font-size: 12px; /* Adjust font size for entire table */
  }

  #continuation_delivery_setting_res th {
    white-space: normal !important; 
    word-wrap: break-word !important;
    text-align: center;
    font-size: 12px; /* Make column headers smaller */
  }

  /* Apply wrapping only to the first column (row labels) */
  #continuation_delivery_setting_res td:first-child {
    max-width: 180px; /* Adjust width as needed */
    white-space: normal !important;
    word-wrap: break-word !important;
    word-break: break-word !important;
    font-size: 12px; /* Make first column text smaller */
  }

  /* Adjust font size for all table cells */
  #continuation_delivery_setting_res td {
    font-size: 12px; /* Adjust font size */
  }
"))),
  
  tags$head(tags$style(HTML("
  #treatment_setting_73_103_matrix_res table {
    table-layout: fixed;
    width: 100%;
    font-size: 12px; /* Adjust font size for entire table */
  }

  #treatment_setting_73_103_matrix_res th {
    white-space: normal !important; 
    word-wrap: break-word !important;
    text-align: center;
    font-size: 12px; /* Make column headers smaller */
  }

  /* Apply wrapping only to the first column (row labels) */
  #treatment_setting_73_103_matrix_res td:first-child {
    max-width: 180px; /* Adjust width as needed */
    white-space: normal !important;
    word-wrap: break-word !important;
    word-break: break-word !important;
    font-size: 12px; /* Make first column text smaller */
  }

  /* Adjust font size for all table cells */
  #treatment_setting_73_103_matrix_res td {
    font-size: 12px; /* Adjust font size */
  }
"))),
  
  tags$head(tags$style(HTML("
  #resmetirom_ongoing_delivery_setting table {
    table-layout: fixed;
    width: 100%;
    font-size: 12px; /* Adjust font size for entire table */
  }

  #resmetirom_ongoing_delivery_setting th {
    white-space: normal !important; 
    word-wrap: break-word !important;
    text-align: center;
    font-size: 12px; /* Make column headers smaller */
  }

  /* Apply wrapping only to the first column (row labels) */
  #resmetirom_ongoing_delivery_setting td:first-child {
    max-width: 180px; /* Adjust width as needed */
    white-space: normal !important;
    word-wrap: break-word !important;
    word-break: break-word !important;
    font-size: 12px; /* Make first column text smaller */
  }

  /* Adjust font size for all table cells */
  #resmetirom_ongoing_delivery_setting td {
    font-size: 12px; /* Adjust font size */
  }
"))),
  
  tags$head(tags$style(HTML("
  #mm_assess_setting_lan table {
    table-layout: fixed;
    width: 100%;
    font-size: 12px; /* Adjust font size for entire table */
  }

  #mm_assess_setting_lan th {
    white-space: normal !important; 
    word-wrap: break-word !important;
    text-align: center;
    font-size: 12px; /* Make column headers smaller */
  }

  /* Apply wrapping only to the first column (row labels) */
  #mm_assess_setting_lan td:first-child {
    max-width: 180px; /* Adjust width as needed */
    white-space: normal !important;
    word-wrap: break-word !important;
    word-break: break-word !important;
    font-size: 12px; /* Make first column text smaller */
  }

  /* Adjust font size for all table cells */
  #mm_assess_setting_lan td {
    font-size: 12px; /* Adjust font size */
  }
"))),
  
  tags$head(tags$style(HTML("
  #treatment_setting_0_52_matrix_lan table {
    table-layout: fixed;
    width: 100%;
    font-size: 12px; /* Adjust font size for entire table */
  }

  #treatment_setting_0_52_matrix_lan th {
    white-space: normal !important; 
    word-wrap: break-word !important;
    text-align: center;
    font-size: 12px; /* Make column headers smaller */
  }

  /* Apply wrapping only to the first column (row labels) */
  #treatment_setting_0_52_matrix_lan td:first-child {
    max-width: 180px; /* Adjust width as needed */
    white-space: normal !important;
    word-wrap: break-word !important;
    word-break: break-word !important;
    font-size: 12px; /* Make first column text smaller */
  }

  /* Adjust font size for all table cells */
  #treatment_setting_0_52_matrix_lan td {
    font-size: 12px; /* Adjust font size */
  }
"))),
  
  tags$head(tags$style(HTML("
  #continuation_52_delivery_setting_lan table {
    table-layout: fixed;
    width: 100%;
    font-size: 12px; /* Adjust font size for entire table */
  }

  #continuation_52_delivery_setting_lan th {
    white-space: normal !important; 
    word-wrap: break-word !important;
    text-align: center;
    font-size: 12px; /* Make column headers smaller */
  }

  /* Apply wrapping only to the first column (row labels) */
  #continuation_52_delivery_setting_lan td:first-child {
    max-width: 180px; /* Adjust width as needed */
    white-space: normal !important;
    word-wrap: break-word !important;
    word-break: break-word !important;
    font-size: 12px; /* Make first column text smaller */
  }

  /* Adjust font size for all table cells */
  #continuation_52_delivery_setting_lan td {
    font-size: 12px; /* Adjust font size */
  }
"))),
  
  tags$head(tags$style(HTML("
  #treatment_setting_52_71_matrix_lan table {
    table-layout: fixed;
    width: 100%;
    font-size: 12px; /* Adjust font size for entire table */
  }

  #treatment_setting_52_71_matrix_lan th {
    white-space: normal !important; 
    word-wrap: break-word !important;
    text-align: center;
    font-size: 12px; /* Make column headers smaller */
  }

  /* Apply wrapping only to the first column (row labels) */
  #treatment_setting_52_71_matrix_lan td:first-child {
    max-width: 180px; /* Adjust width as needed */
    white-space: normal !important;
    word-wrap: break-word !important;
    word-break: break-word !important;
    font-size: 12px; /* Make first column text smaller */
  }

  /* Adjust font size for all table cells */
  #treatment_setting_52_71_matrix_lan td {
    font-size: 12px; /* Adjust font size */
  }
"))),

  
  
  tags$head(tags$style(HTML("
  #continuation_delivery_setting_lan table {
    table-layout: fixed;
    width: 100%;
    font-size: 12px; /* Adjust font size for entire table */
  }

  #continuation_delivery_setting_lan th {
    white-space: normal !important; 
    word-wrap: break-word !important;
    text-align: center;
    font-size: 12px; /* Make column headers smaller */
  }

  /* Apply wrapping only to the first column (row labels) */
  #continuation_delivery_setting_lan td:first-child {
    max-width: 180px; /* Adjust width as needed */
    white-space: normal !important;
    word-wrap: break-word !important;
    word-break: break-word !important;
    font-size: 12px; /* Make first column text smaller */
  }

  /* Adjust font size for all table cells */
  #continuation_delivery_setting_lan td {
    font-size: 12px; /* Adjust font size */
  }
"))),
  
  tags$head(tags$style(HTML("
  #treatment_setting_73_103_matrix_lan table {
    table-layout: fixed;
    width: 100%;
    font-size: 12px; /* Adjust font size for entire table */
  }

  #treatment_setting_73_103_matrix_lan th {
    white-space: normal !important; 
    word-wrap: break-word !important;
    text-align: center;
    font-size: 12px; /* Make column headers smaller */
  }

  /* Apply wrapping only to the first column (row labels) */
  #treatment_setting_73_103_matrix_lan td:first-child {
    max-width: 180px; /* Adjust width as needed */
    white-space: normal !important;
    word-wrap: break-word !important;
    word-break: break-word !important;
    font-size: 12px; /* Make first column text smaller */
  }

  /* Adjust font size for all table cells */
  #treatment_setting_73_103_matrix_lan td {
    font-size: 12px; /* Adjust font size */
  }
"))),
  
  tags$head(tags$style(HTML("
  #lanifibranor_ongoing_delivery_setting table {
    table-layout: fixed;
    width: 100%;
    font-size: 12px; /* Adjust font size for entire table */
  }

  #lanifibranor_ongoing_delivery_setting th {
    white-space: normal !important; 
    word-wrap: break-word !important;
    text-align: center;
    font-size: 12px; /* Make column headers smaller */
  }

  /* Apply wrapping only to the first column (row labels) */
  #lanifibranor_ongoing_delivery_setting td:first-child {
    max-width: 180px; /* Adjust width as needed */
    white-space: normal !important;
    word-wrap: break-word !important;
    word-break: break-word !important;
    font-size: 12px; /* Make first column text smaller */
  }

  /* Adjust font size for all table cells */
  #lanifibranor_ongoing_delivery_setting td {
    font-size: 12px; /* Adjust font size */
  }
"))),
  
  
  
  navbarPage(
  
  title = "MASLD Treatment Activity and Costs Modelling",
  id = "navbar",
  
  
  


  tabPanel("Introduction",
           fluidPage(
             h1("Introduction to the MASLD Treatment Activity and Costs Model"),
             p("This model has been developed to simulate the populations, clinical activities and associated costs 
               of implementing four new treatment pathways for ", strong("Metabolic Dysfunction-Associated Steatotic Liver Disease (MASLD).")),
             p("These four new treatment pathways include the use of the following treatments:",
               tags$ul(
                 tags$li("Semaglutide"),
                 tags$li("Survodutide"),
                 tags$li("Resmetirom"),
                 tags$li("Lanifibranor")
               )),
             p("The three tabs below provide guidance and information on:",
               tags$ul(
                 tags$li("Using the model"),
                 tags$li("Understanding the assumptions in the model and how these are applied"),
                 tags$li("Links to more detailed documentation regarding the development of the model")
               )),
             navset_tab(
               nav_panel("How to use the model",
                         h2("How to use the model"),
                         p("This section provides an overview of how to use the model and covers:",
                           tags$ul(
                             tags$li("The structure of the model"),
                             tags$li("Navigating between sections"),
                             tags$li("Changing different input assumptions"),
                             tags$li("Reviewing the model outputs"),
                             tags$li("Extracting model outputs and assumptions")
                           )),
                         br(),
                         h3("Model Structure"),
                         p("This model has been created using the open source programming language ",
                           tags$a("R", href = "https://www.r-project.org/", target = "_blank"), "to create a shinyApp. 
                           The model contains a series of input tabs relating to different components of the model. These 
                           different input components include:",
                           tags$ul(
                             tags$li(strong("Population Assumptions:"), "These assumptions are used to determine the size of 
                                     population of patients that are potentially eligible for treatment. Therefore, this includes 
                                     assumptions such as the prevalence of MASLD, the breakdown of fibrosis stage (in accordance with NASH CRN system) and rates of 
                                     diagnosis for each Fibrosis Stage. The assumptions applied in this section will output an 
                                     estimate of the diagnosed population of patients at each Fibrosis Stage. These estimates are 
                                     then taken forward into the next section."),
                             tags$li(strong("Treatment Implementation:"), "These assumptions take the estimates of each diagnosed 
                                     Fibrosis Stage to model the percentage of patients being treated. There are separate assumptions 
                                     for each of the four treatments. The output of this section will be the number of patients 
                                     starting each treatment."),
                             tags$li(strong("Semaglutide Pathway Activity Assumptions:"), "The assumptions within this section relate 
                                     to how the new treatment pathway for Semaglutide is expected to be delivered. These assumptions 
                                     cover areas such as retention rates, the number of monitoring tests and the proportion of these 
                                     tests including specific diagnostics such as FibroScan or liver biopsies. These assumptions 
                                     are applied to the estimated number of patients starting the Semaglutide treatment pathway to 
                                     determine the number of each clinical activity along this pathway."),
                             tags$li(strong("Survodutide Pathway Activity Assumptions:"), "Similarly to the Semaglutide pathway above, 
                                     this section contains the assumptions for the Survodutide pathway."),
                             tags$li(strong("Resmetirom Pathway Activity Assumptions:"), "Similarly to the Semaglutide pathway above, 
                                     this section contains the assumptions for the Resmetirom pathway."),
                             tags$li(strong("Lanifibranor Pathway Activity Assumptions:"), "Similarly to the Semaglutide pathway above, 
                                     this section contains the assumptions for the Lanifibranor pathway."),
                             tags$li(strong("Financial Assumptions:"), "The assumptions within this section cover the costs associated 
                                     with the delivery of each treatment. These include the costs of the treatments and associated 
                                     activities such as the cost of an appointment with a Heptalogist in secondary care or the cost of 
                                     a liver biopsy. These assumptions are applied to the pathway activities determined in the previous 
                                     sections to determine the cost implications of each treatment pathway."),
                             tags$li(strong("Model Outputs - Activity and Costs:"), "This section contains the outputs of the model. 
                                     These outputs include the volume of patients treated, the clinical activities undertaken for these 
                                     treatment cohorts and the costs associated with these. The outputs are produced at a summary level 
                                     and also include a breakdown of the activities and costs at each stage of the pathways.")
                           )
                         ),
                        br(),
                        h3("Navigating the model"),
                        p("You can navigate through the different stages of the model using the navigation bar at the top of this page. 
                          Clicking on any of the tabs will take you to the relevant screen for inputting assumptions or viewing the 
                          outputs of the model."),
                        br(),
                        h3("Changing model assumptions"),
                        p("This model has been created to enable the ability to create bespoke scenarios for delivering these new treatments. 
                          Consequently, each of the model assumptions can be amended to simulate activities and costs under those conditions. 
                          The default assumptions will be based on available literature, input from clinical experts and feedback from the pathway mapping 
                          workstream. To amend any of these assumptions simply:",
                          tags$ul(
                            tags$li("Change the value of the slider input to the desired value"),
                            tags$li("Input a specific number into an input box"),
                            tags$li("Amend the table of percentages to model the setting for specific activities")
                          )),
                        p("Some of the assumptions will have data validation or limits applied to stop impossible assumptions being applied. 
                          For example, applying negative percentages.")
                         ),
               nav_panel("Explanation of Assumptions"),
               nav_panel("Modelling Documentation")
             )
           )
  ),
  
  tabPanel(title = "Population Assumptions",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 h4("Set Adult Population:"),
                 numericInput("adult_pop",
                              "Set Adult Population:",
                              value = 45691677,
                              min = 0,
                              step = 1),
                 h4("Set the MASLD and MASH Prevalence:"),
                 sliderInput("masld_prev",
                             "Set MASLD Prevalence:",
                             value = 24,
                             min = 0,
                             max = 100,
                             step = 1,
                             post = "%"),
                 sliderInput("masld_prev_tol",
                             "Set MASLD Prevalence tolerance:",
                             value = 5,
                             min = 0,
                             max = 25,
                             step = 0.5,
                             post = "%"),
                 sliderInput("mash_prev",
                             "Set MASH Prevalence:",
                             value = 4.5,
                             min = 0,
                             max = 100,
                             step = 1,
                             post = "%"),
                 sliderInput("mash_prev_tol",
                             "Set MASH Prevalence tolerance:",
                             value = 1,
                             min = 0,
                             max = 10,
                             step = 0.25,
                             post = "%"),
                 br(),
                 h4("Set the Fibrosis Stage Assumptions"),
                 sliderInput("F0_prop",
                             "Set proportion at stage F0:",
                             value = 23,
                             min = 0,
                             max = 100,
                             step = 1,
                             post = "%"),
                 sliderInput("F1_prop",
                             "Set the proportion at stage F1:",
                             value = 36,
                             min = 0,
                             max = 100,
                             step = 1,
                             post = "%"),
                 sliderInput("F2_prop",
                             "Set the proportion at stage F2:",
                             value = 20,
                             min = 0,
                             max = 100,
                             step = 1,
                             post = "%"),
                 sliderInput("F3_prop",
                             "Set the proportion at stage F3:",
                             value = 13,
                             min = 0,
                             max = 100,
                             step = 1,
                             post = "%"),
                 sliderInput("F4_prop",
                             "Set the proportion at stage F4:",
                             value = 8,
                             min = 0,
                             max = 100,
                             step = 1,
                             post = "%"),
                 br(),
                 h4("Set the Prevalence rates of Obesity and Diabetes"),
                 sliderInput("ob_prev",
                             "Prevalence of obesity within MASLD population:",
                             value = 55,
                             min = 0,
                             max = 100,
                             step = 1,
                             post = "%"),
                 sliderInput("ob_prev_tol",
                             "Set the obesity prevalence tolerance:",
                             value = 10,
                             min = 0,
                             max = 20,
                             step = 1,
                             post = "%"),
                 sliderInput("t2d_prev",
                             "Prevalence of Type 2 Diabetes within MASLD population",
                             value = 30,
                             min = 0,
                             max = 100,
                             step = 1,
                             post = "%"),
                 sliderInput("t2d_prev_tol",
                             "Set the Type 2 Diabetes prevalence tolerance:",
                             value = 12,
                             min = 0,
                             max = 25,
                             step = 1,
                             post = "%"),
                 br(),
                 h4("Set the diagnosis rates for each Fibrosis Stage:"),
                 sliderInput("diag_F0",
                             "Set the diagnosis rate for F0:",
                             value = 2,
                             min = 0,
                             max = 100,
                             step = 1,
                             post = "%"),
                 sliderInput("diag_F1",
                             "Set the diagnosis rate for F1:",
                             value = 2,
                             min = 0,
                             max = 100,
                             step = 1,
                             post = "%"),
                 sliderInput("diag_F2",
                             "Set the diagnosis rate for F2:",
                             value = 16.5,
                             min = 0,
                             max = 100,
                             step = 1,
                             post = "%"),
                 sliderInput("diag_F3",
                             "Set the diagnosis rate for F3:",
                             value = 58.3,
                             min = 0,
                             max = 100,
                             step = 1,
                             post = "%"),
                 sliderInput("diag_F4",
                             "Set the diagnosis rate for F4:",
                             value = 100,
                             min = 0,
                             max = 100,
                             step = 1,
                             post = "%"),
                 br(),
                 h4("Download Assumptions"),
                 downloadButton("download_population_assumptions", "Download Population Assumptions")
               ),
               mainPanel(
                 h1("Population Assumptions"),
                 p("The section below shows the impact of each of the assumptions to the left on determining the population 
                   of patients that would be eligible for the new treatment pathways. Each section below will contain:",
                   tags$ul(
                     tags$li("A histogram showing the populations simulated from the assumptions"),
                     tags$li("A data table showing the populations for each simulation. Within each table:",
                             tags$ul(
                               tags$li("Simulation 1: The central estimate based on the assumptions"),
                               tags$li("Simulation 2: The lower bound estimate based on the lower tolerance for each assumption"),
                               tags$li("Simulation 3: The upper bound estimate based on the higher tolerance for each assumption"),
                               tags$li("Simulations 4 - 100: Simulated data based on the above lower, central and upper parameters")
                             ))
                   )),
                 h3("MASLD Prevalence"),
                 p("The tabs below show the simulated MASLD populations to be used within the model as either a histogram or table:"),
                 textOutput("masld_prev"),
                 navset_tab(
                   nav_panel("Histogram", plotOutput("masld_pop_histogram")),
                   nav_panel("Table", DTOutput("masld_pop_DT"))
                 ),
                 h3("MASH Prevalence"),
                 p("The tabs below show the simulated MASH populations to be used within the model as either a histogram or table:"),
                 navset_tab(
                   nav_panel("Histogram", plotOutput("mash_pop_histogram")),
                   nav_panel("Table", DTOutput("mash_pop_DT"))
                 ),
                 h3("Fibrosis Stage Populations"),
                 p("The tabs below show the simulated MASH populations by Fibrosis stage that will be used within the model as either a histogram or table:"),
                 navset_tab(
                   nav_panel("Histogram", plotOutput("f_stage_pop_histogram")),
                   nav_panel("Table", DTOutput("f_stage_pop_DT"))
                 ),
                 h3("Co-morbidities"),
                 p("TODO"),
                 h3("Diagnosed Population Estimates"),
                 p("The tabs below show the estimated population at each Fibrosis stage that is diagnosed. This is shown as either a histogram or a table:"),
                 navset_tab(
                   nav_panel("Histogram", plotOutput("f_stage_diag_histogram")),
                   nav_panel("Table", DTOutput("f_stage_diag_DT"))
                 ),
                 
               )
             )
           )
  ),
  
  tabPanel(title = "Treatment Implementation",
           fluidPage(
             h1("Populations receiving treatments"),
             p("In this section please specify the proportion of each group who will receive each treatment type."),
             navset_tab(
               nav_panel("Semaglutide",
                         sidebarLayout(
                           sidebarPanel(
                                h3("Semaglutide"),
                                hr(),
                                sliderInput("treat_pop_F0_sem",
                                            "Select the proportion of F0 patients receiving treatment:",
                                            value = 0,
                                            min = 0,
                                            max = 100,
                                            post = "%"),
                                sliderInput("treat_pop_F1_sem",
                                            "Select the proportion of F1 patients receiving treatment:",
                                            value = 0,
                                            min = 0,
                                            max = 100,
                                            post = "%"),
                                sliderInput("treat_pop_F2_sem",
                                            "Select the proportion of F2 patients receiving treatment:",
                                            value = 20,
                                            min = 0,
                                            max = 100,
                                            post = "%"),
                                sliderInput("treat_pop_F3_sem",
                                            "Select the proportion of F3 patients receiving treatment:",
                                            value = 35,
                                            min = 0,
                                            max = 100,
                                            post = "%"),
                                sliderInput("treat_pop_F4_sem",
                                            "Select the proportion of F4 patients receiving treatment:",
                                            value = 0,
                                            min = 0,
                                            max = 100,
                                            post = "%"),
                                ),
                           mainPanel(
                                h4("Table of treatment population"),
                                DTOutput("treat_pop_sem_DT")
                           )
                         )
               ),
               nav_panel("Survodutide",
                         sidebarLayout(
                           sidebarPanel(
                             h3("Survodutide"),
                             hr(),
                             sliderInput("treat_pop_F0_surv",
                                         "Select the proportion of F0 patients receiving treatment:",
                                         value = 0,
                                         min = 0,
                                         max = 100,
                                         post = "%"),
                             sliderInput("treat_pop_F1_surv",
                                         "Select the proportion of F1 patients receiving treatment:",
                                         value = 0,
                                         min = 0,
                                         max = 100,
                                         post = "%"),
                             sliderInput("treat_pop_F2_surv",
                                         "Select the proportion of F2 patients receiving treatment:",
                                         value = 5,
                                         min = 0,
                                         max = 100,
                                         post = "%"),
                             sliderInput("treat_pop_F3_surv",
                                         "Select the proportion of F3 patients receiving treatment:",
                                         value = 5,
                                         min = 0,
                                         max = 100,
                                         post = "%"),
                             sliderInput("treat_pop_F4_surv",
                                         "Select the proportion of F4 patients receiving treatment:",
                                         value = 0,
                                         min = 0,
                                         max = 100,
                                         post = "%"),
                           ),
                           mainPanel(
                             h4("Table of treatment population"),
                             DTOutput("treat_pop_surv_DT")
                           )
                         )
               ),
               nav_panel("Resmetirom",
                         sidebarLayout(
                           sidebarPanel(
                             h3("Resmetirom"),
                             hr(),
                             sliderInput("treat_pop_F0_res",
                                         "Select the proportion of F0 patients receiving treatment:",
                                         value = 0,
                                         min = 0,
                                         max = 100,
                                         post = "%"),
                             sliderInput("treat_pop_F1_res",
                                         "Select the proportion of F1 patients receiving treatment:",
                                         value = 0,
                                         min = 0,
                                         max = 100,
                                         post = "%"),
                             sliderInput("treat_pop_F2_res",
                                         "Select the proportion of F2 patients receiving treatment:",
                                         value = 10,
                                         min = 0,
                                         max = 100,
                                         post = "%"),
                             sliderInput("treat_pop_F3_res",
                                         "Select the proportion of F3 patients receiving treatment:",
                                         value = 10,
                                         min = 0,
                                         max = 100,
                                         post = "%"),
                             sliderInput("treat_pop_F4_res",
                                         "Select the proportion of F4 patients receiving treatment:",
                                         value = 0,
                                         min = 0,
                                         max = 100,
                                         post = "%")
                           ),
                           mainPanel(
                             h4("Table of treatment population"),
                             DTOutput("treat_pop_res_DT")
                           )
                         )
               ),
               nav_panel("Lanifibranor",
                         sidebarLayout(
                           sidebarPanel(
                             h3("Lanifibranor"),
                             hr(),
                             sliderInput("treat_pop_F0_lan",
                                         "Select the proportion of F0 patients receiving treatment:",
                                         value = 0,
                                         min = 0,
                                         max = 100,
                                         post = "%"),
                             sliderInput("treat_pop_F1_lan",
                                         "Select the proportion of F1 patients receiving treatment:",
                                         value = 0,
                                         min = 0,
                                         max = 100,
                                         post = "%"),
                             sliderInput("treat_pop_F2_lan",
                                         "Select the proportion of F2 patients receiving treatment:",
                                         value = 5,
                                         min = 0,
                                         max = 100,
                                         post = "%"),
                             sliderInput("treat_pop_F3_lan",
                                         "Select the proportion of F3 patients receiving treatment:",
                                         value = 5,
                                         min = 0,
                                         max = 100,
                                         post = "%"),
                             sliderInput("treat_pop_F4_lan",
                                         "Select the proportion of F4 patients receiving treatment:",
                                         value = 0,
                                         min = 0,
                                         max = 100,
                                         post = "%")
                           ),
                           mainPanel(
                             h4("Table of treatment population"),
                             DTOutput("treat_pop_lan_DT")
                           )
                         )
               ),
               nav_panel("All Treatments",
                         h4("Table of Total Population Receiving Treatments"),
                         DTOutput("treat_pop_all_DT"),
                         br(),
                         downloadButton("download_treat_imp_assumptions", "Download Treatment Implementation Assumptions")
                         )
                         
             )
           )
  ),
  tabPanel(title = "Semaglutide Pathway Activity Assumptions",
           fluidPage(
             h1("Semaglutide Pathway Activity Assumptions"),
             p("In this section you can set the assumptions about the clinical activities that occur along the treatment pathway.
               These assumptions will then be applied in conjunction with the number starting this treatment and financial assumptions 
               to generate the associated activities and costs. Please refer to the ", strong("Pathway Map"), " tab to review the 
               pathway."),
             downloadButton("download_sem_pathway_assumptions", "Download Semaglutide Pathway Assumptions"),
             navset_tab(
               nav_panel("Pathway Assumptions",
                 fluidRow(
                   column(2,
                          h3("Pre-Treatment: Diagnostics"),
                          hr(),
                          h4("Initial Eligibility Assessment"),
                          sliderInput("mm_assess_prop_sem",
                                      "Set percentage undergoing initial eligibility assessment:",
                                      value = 100,
                                      min = 0,
                                      max = 100,
                                      post = "%"),
                          matrixInput("mm_assess_setting_sem",
                                      "Input setting and minutes for initial eligibility assessment:",
                                      value = matrix(c(35, 20,
                                                       0, 20,
                                                       45, 20,
                                                       15, 20,
                                                       0, 30,
                                                       5, 30),
                                                     ncol = 2,
                                                     byrow = TRUE,
                                                     dimnames = list(c("Primary Care - GP",
                                                                       "Primary Care - Nurse Led",
                                                                       "Hepatology / Gastro Consultant",
                                                                       "Hepatology / Gastro CNS",
                                                                       "Community - Diagnostician",
                                                                       "Community - Pharmacist"),
                                                                     c("Percentage (%)", "Mins")
                                                     )),
                                      class = "numeric",
                          ),
                          hr(),
                          h4("Diagnostics"),
                          sliderInput("pre_liver_biopsy_prop_sem",
                                      "Set percentage undergoing biopsy:",
                                      value = 5,
                                      min = 0,
                                      max = 100,
                                      post= "%"),
                          sliderInput("pre_elf_prop_sem",
                                      "Set percentage undergoing ELF testing:",
                                      value = 35,
                                      min = 0,
                                      max = 100,
                                      post= "%"),
                          sliderInput("pre_biomarkers_prop_sem",
                                      "Set percentage undergoing other biomarker testing (inc. LFTs and FIB-4):",
                                      value = 100,
                                      min = 0,
                                      max = 100,
                                      post= "%"),
                          sliderInput("pre_fibro_prop_sem",
                                      "Set percentage undergoing FibroScan:",
                                      value = 70,
                                      min = 0,
                                      max = 100,
                                      post= "%")
                          
                   ),
                   column(2,
                          h3("Week 0: Treatment Delivery"),
                          hr(),
                          h4("Treatment Retention"),
                          sliderInput("retention_sem_0_16",
                                      "Input retention on Semaglutide for 16 weeks:",
                                      value = 93,
                                      min = 0,
                                      max = 100,
                                      post = "%"),
                          hr(),
                          h4("Treatment Setting"),
                          matrixInput("treatment_setting_0_16_matrix_sem",
                                      "Input setting and minutes for delivery of initial treatment:",
                                      value = matrix(c(5, 20,
                                                       5, 20,
                                                       20, 20,
                                                       70, 20,
                                                       0, 30,
                                                       0, 30),
                                                     ncol = 2,
                                                     byrow = TRUE,
                                                     dimnames = list(c("Primary Care - GP",
                                                                       "Primary Care - Nurse Led",
                                                                       "Hepatology / Gastro Consultant",
                                                                       "Hepatology / Gastro CNS",
                                                                       "Community - Diagnostician",
                                                                       "Community - Pharmacist"),
                                                                     c("Percentage (%)", "Mins")
                                                     )),
                                      class = "numeric",
                          ),
                          numericInput("appts_0_16_sem",
                                       "Select the number of appointments over weeks 0-16:",
                                       value = 4,
                                       min = 1,
                                       max = 16,
                                       step = 1)
                          
                   ),
                   column(2,
                          h3("Weeks 4-71: Monitoring and Dosage Maintenance"),
                          hr(),
                          h4("Monitoring Tests"),
                          numericInput("monitoring_tests_number_sem",
                                       "Select number of monitoring tests:",
                                       value = 5,
                                       min = 0,
                                       max = NA,
                                       step = 1),
                          sliderInput("monitoring_elf_prop_sem",
                                      "Select percentage of monitoring tests that are ELF:",
                                      value = 0,
                                      min = 0,
                                      max = 100,
                                      post = "%"),
                          sliderInput("monitoring_bio_prop_sem",
                                      "Select percentage of monitoring tests that are biomarkers:",
                                      value = 100,
                                      min = 0,
                                      max = 100,
                                      post = "%"),
                          hr(),
                          h4("Dosage Maintenence"),
                          sliderInput("dosage_retention_20_71_sem",
                                      "Select the retention rate of those at Dosage Maintenance:",
                                      value = 93,
                                      min = 0,
                                      max = 100,
                                      post = "%"),
                          matrixInput("semaglutide_20_71_delivery_setting",
                                      "Input setting and minutes for delivery of treatment for weeks 20-71:",
                                      value = matrix(c(10, 20,
                                                       50, 20,
                                                       0, 20,
                                                       40, 20,
                                                       0, 30,
                                                       0, 30),
                                                     ncol = 2,
                                                     byrow = TRUE,
                                                     dimnames = list(c("Primary Care - GP",
                                                                       "Primary Care - Nurse Led",
                                                                       "Hepatology / Gastro Consultant",
                                                                       "Hepatology / Gastro CNS",
                                                                       "Community - Diagnostician",
                                                                       "Community - Pharmacist"),
                                                                     c("Percentage (%)", "Mins")
                                                     )),
                                      class = "numeric",
                          ),
                          numericInput("appts_20_71_sem",
                                       "Select the number of appointments over weeks 20-71:",
                                       value = 3,
                                       min = 1,
                                       max = 55,
                                       step = 1)
                   ),
                   column(2,
                          h3("Week 72: Continuation Decision"),
                          hr(),
                          h4("Efficacy Assessment"),
                          sliderInput("efficacy_liver_biopsy_prop_sem",
                                      "Select the percentage of patients undergoing a liver biopsy:",
                                      value = 0,
                                      min = 0,
                                      max = 100,
                                      post = "%"),
                          sliderInput("efficacy_elf_prop_sem",
                                      "Select the percentage of patients undergoing ELF testing:",
                                      value = 35,
                                      min = 0,
                                      max = 100,
                                      post = "%"),
                          sliderInput("efficacy_fibro_prop_sem",
                                      "Set the percentage of patients undergoing a Fibroscan:",
                                      value = 70,
                                      min = 0,
                                      max = 100,
                                      post = "%"),
                          sliderInput("efficacy_biomarkers_prop_sem",
                                      "Select the percentage of patients undergoing biomarker testing:",
                                      value = 100,
                                      min = 0,
                                      max = 100,
                                      post = "%"),
                          hr(),
                          h4("Continuation Decision"),
                          sliderInput("continuation_prop_sem",
                                      "Select the percentage of patients continuing with treatment:",
                                      value = 67,
                                      min = 0,
                                      max = 100,
                                      post = "%"),
                          matrixInput("continuation_delivery_setting_sem",
                                      "Input the setting and minutes for continuation decision appointment:",
                                      value = matrix(c(5, 20,
                                                       0, 20,
                                                       95, 20,
                                                       0, 20,
                                                       0, 30,
                                                       0, 30),
                                                     ncol = 2,
                                                     byrow = TRUE,
                                                     dimnames = list(c("Primary Care - GP",
                                                                       "Primary Care - Nurse Led",
                                                                       "Hepatology / Gastro Consultant",
                                                                       "Hepatology / Gastro CNS",
                                                                       "Community - Diagnostician",
                                                                       "Community - Pharmacist"),
                                                                     c("Percentage (%)", "Mins")
                                                     )),
                                      class = "numeric",
                          )
                          
                   ),
                   column(2,
                          h3("Week 73-103: Dosage Maintenance"),
                          hr(),
                          h4("Retention Rate"),
                          sliderInput("retention_73_103_sem",
                                      "Select the retention rate for patients receiving dosage maintenance:",
                                      value = 93,
                                      min = 0,
                                      max = 100,
                                      post = "%"),
                          matrixInput("semaglutide_73_103_delivery_setting",
                                      "Input the setting and minutes for delivery of Semaglutide for weeks 73-103:",
                                      value = matrix(c(15, 20,
                                                       60, 20,
                                                       0, 20,
                                                       20, 20,
                                                       0, 30,
                                                       5, 30),
                                                     ncol = 2,
                                                     byrow = TRUE,
                                                     dimnames = list(c("Primary Care - GP",
                                                                       "Primary Care - Nurse Led",
                                                                       "Hepatology / Gastro Consultant",
                                                                       "Hepatology / Gastro CNS",
                                                                       "Community - Diagnostician",
                                                                       "Community - Pharmacist"),
                                                                     c("Percentage (%)", "Mins")
                                                     )),
                                      class = "numeric",
                          ),
                          numericInput("appts_73_103_sem",
                                       "Select the number of appointments over weeks 73-103:",
                                       value = 2,
                                       min = 1,
                                       max = 30,
                                       step = 1),
                          hr(),
                          h4("Monitoring Testing"),
                          numericInput("monitoring_tests_number_73_103_sem",
                                       "Select number of monitoring tests:",
                                       value = 2,
                                       min = 0,
                                       max = NA,
                                       step = 1),
                          sliderInput("monitoring_tests_73_103_elf_sem",
                                      "Select the percentage of patients undergoing ELF testing:",
                                      value = 0,
                                      min = 0,
                                      max = 100,
                                      post = "%"),
                          sliderInput("monitoring_tests_73_103_biomarkers_sem",
                                      "Select the percentage of patients undergoing Biomarker testing:",
                                      value = 100,
                                      min = 0,
                                      max = 100,
                                      post = "%")
                   ),
                   column(2,
                          h3("Week 104+: Ongoing Treatment"),
                          hr(),
                          h4("Treatment Weeks to Model"),
                          numericInput("ongoing_period_sem",
                                       "Select the number of weeks to continue modelling treatment",
                                       value = 104,
                                       min = 52,
                                       max = 208,
                                       step = 1),
                          hr(),
                          h4("Retention by End-point"),
                          sliderInput("retention_end_sem",
                                      "Select the retention rate by the end-point of the model:",
                                      value = 50,
                                      min = 0,
                                      max = 100,
                                      post = "%"),
                          hr(),
                          h4("Ongoing Treatment Setting"),
                          matrixInput("semaglutide_ongoing_delivery_setting",
                                      "Input the setting and minutes for delivery of Semaglutide until end-point:",
                                      value = matrix(c(5, 20,
                                                       90, 20,
                                                       0, 20,
                                                       0, 20,
                                                       0, 30,
                                                       5, 30),
                                                     ncol = 2,
                                                     byrow = TRUE,
                                                     dimnames = list(c("Primary Care - GP",
                                                                       "Primary Care - Nurse Led",
                                                                       "Hepatology / Gastro Consultant",
                                                                       "Hepatology / Gastro CNS",
                                                                       "Community - Diagnostician",
                                                                       "Community - Pharmacist"),
                                                                     c("Percentage (%)", "Mins")
                                                     )),
                                      class = "numeric",
                          ),
                          numericInput("appts_ongoing_sem",
                                       "Select the number of appointments post 104 weeks:",
                                       value = 2,
                                       min = 1,
                                       max = NA,
                                       step = 1),
                          hr(),
                          h4("Annual Assessment"),
                          sliderInput("ongoing_annual_prop_elf_sem",
                                      "Set the proportion of annual assessments undergoing ELF test:",
                                      value = 35,
                                      min = 0,
                                      max = 100,
                                      post = "%"),
                          sliderInput("ongoing_annual_prop_biomarkers_sem",
                                      "Set the proportion of annual assessments undergoing other biomarkers:",
                                      value = 100,
                                      min = 0,
                                      max = 100,
                                      post = "%"),
                          sliderInput("ongoing_annual_prop_fibro_sem",
                                      "Set the proportion of annual assessments undergoing FibroScan:",
                                      value = 70,
                                      min = 0,
                                      max = 100,
                                      post = "%")
                   )
                 )
               ),
               nav_panel("Pathway Map",
                         includeHTML("www/images/Semaglutide Real World.drawio.html")
             )
            )
           )
  ),
  tabPanel(title = "Survodutide Pathway Activity Assumptions",
           fluidPage(
             h1("Survodutide Pathway Activity Assumptions"),
             p("In this section you can set the assumptions about the clinical activities that occur along the treatment pathway.
               These assumptions will then be applied in conjunction with the number starting this treatment and financial assumptions 
               to generate the associated activities and costs. Please refer to the ", strong("Pathway Map"), " tab to review the 
               pathway."),
             downloadButton("download_surv_pathway_assumptions", "Download Survodutide Pathway Assumptions"),
             navset_tab(
               nav_panel("Pathway Assumptions",
                         fluidRow(
                           column(2,
                                  h3("Pre-Treatment: Diagnostics"),
                                  hr(),
                                  h4("Initial Eligibility Assessment"),
                                  sliderInput("mm_assess_prop_surv",
                                              "Set percentage undergoing initial eligibility assessment:",
                                              value = 100,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  matrixInput("mm_assess_setting_surv",
                                              "Input setting and minutes for initial eligibility assessment:",
                                              value = matrix(c(35, 20,
                                                               0, 20,
                                                               45, 20,
                                                               15, 20,
                                                               0, 30,
                                                               5, 30),
                                                             ncol = 2,
                                                             byrow = TRUE,
                                                             dimnames = list(c("Primary Care - GP",
                                                                               "Primary Care - Nurse Led",
                                                                               "Hepatology / Gastro Consultant",
                                                                               "Hepatology / Gastro CNS",
                                                                               "Community - Diagnostician",
                                                                               "Community - Pharmacist"),
                                                                             c("Percentage (%)", "Mins")
                                                             )),
                                              class = "numeric",
                                  ),
                                  hr(),
                                  h4("Diagnostics"),
                                  sliderInput("pre_liver_biopsy_prop_surv",
                                              "Set percentage undergoing biopsy:",
                                              value = 5,
                                              min = 0,
                                              max = 100,
                                              post= "%"),
                                  sliderInput("pre_elf_prop_surv",
                                              "Set percentage undergoing ELF testing:",
                                              value = 35,
                                              min = 0,
                                              max = 100,
                                              post= "%"),
                                  sliderInput("pre_biomarkers_prop_surv",
                                              "Set percentage undergoing other biomarker testing (inc. LFTs and FIB-4):",
                                              value = 100,
                                              min = 0,
                                              max = 100,
                                              post= "%"),
                                  sliderInput("pre_fibro_prop_surv",
                                              "Set percentage undergoing FibroScan:",
                                              value = 70,
                                              min = 0,
                                              max = 100,
                                              post= "%")
                                  
                           ),
                           column(2,
                                  h3("Week 0: Treatment Delivery"),
                                  hr(),
                                  h4("Treatment Retention"),
                                  sliderInput("retention_surv_0_24",
                                              "Set the retention rate for weeks 0 - 24:",
                                              value = 95,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  hr(),
                                  h4("Treatment Setting"),
                                  matrixInput("treatment_setting_0_24_matrix_surv",
                                              "Input setting and minutes for delivery of initial treatment:",
                                              value = matrix(c(0, 20,
                                                               0, 20,
                                                               25, 20,
                                                               75, 20,
                                                               0, 30,
                                                               0, 30),
                                                             ncol = 2,
                                                             byrow = TRUE,
                                                             dimnames = list(c("Primary Care - GP",
                                                                               "Primary Care - Nurse Led",
                                                                               "Hepatology / Gastro Consultant",
                                                                               "Hepatology / Gastro CNS",
                                                                               "Community - Diagnostician",
                                                                               "Community - Pharmacist"),
                                                                             c("Percentage (%)", "Mins")
                                                             )),
                                              class = "numeric",
                                  ),
                                  numericInput("appts_0_24_surv",
                                               "Select the number of appointments over weeks 0-24:",
                                               value = 4,
                                               min = 1,
                                               max = 16,
                                               step = 1)
                                  
                           ),
                           column(2,
                                  h3("Weeks 0-71: Monitoring and Dosage Maintenance"),
                                  hr(),
                                  h4("Monitoring Tests"),
                                  numericInput("monitor_tests_0_71_num_surv",
                                               "Set the number of monitoring tests:",
                                               value = 4,
                                               min = 0,
                                               max = NA,
                                               step = 1),
                                  sliderInput("monitor_tests_0_71_elf_prop_surv",
                                              "Set the percentage receiving an ELF test:",
                                              value = 0,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  sliderInput("monitor_tests_0_71_bio_prop_surv",
                                              "Set the percentage receiving other biomarker tests:",
                                              value = 100,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  hr(),
                                  h4("Treatment Weeks 25 - 71"),
                                  sliderInput("retention_25_71_surv",
                                              "Set the retention rate for weeks 25 - 71:",
                                              value = 95,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  matrixInput("treatment_setting_25_71_matrix_surv",
                                              "Input setting and minutes for delivery of treatment for weeks 25 - 71:",
                                              value = matrix(c(5, 20,
                                                               0, 20,
                                                               15, 20,
                                                               80, 20,
                                                               0, 30,
                                                               0, 30),
                                                             ncol = 2,
                                                             byrow = TRUE,
                                                             dimnames = list(c("Primary Care - GP",
                                                                               "Primary Care - Nurse Led",
                                                                               "Hepatology / Gastro Consultant",
                                                                               "Hepatology / Gastro CNS",
                                                                               "Community - Diagnostician",
                                                                               "Community - Pharmacist"),
                                                                             c("Percentage (%)", "Mins")
                                                             )),
                                              class = "numeric",
                                  )
                                  ),
                           column(2,
                                  h3("Week 72: Continuation Decision"),
                                  hr(),
                                  h4("Efficacy Assessment"),
                                  sliderInput("efficacy_liver_biopsy_surv",
                                              "Set the percentage of paitents undergoing a liver biopsy:",
                                              value = 0,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  sliderInput("efficacy_elf_prop_surv",
                                              "Set the percentage of patients undergoing ELF testing:",
                                              value = 35,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  sliderInput("efficacy_fibro_prop_surv",
                                              "Set the percentage of patients undergoing a Fibroscan:",
                                              value = 70,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  sliderInput("efficacy_biomarkers_prop_surv",
                                              "Set the percentage of patients undergoing other biomarkers:",
                                              value = 100,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  hr(),
                                  h4("Continuation Decision"),
                                  sliderInput("continuation_prop_surv",
                                              "Select the percentage of patients continuing with treatment:",
                                              value = 49,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  matrixInput("continuation_delivery_setting_surv",
                                              "Input setting and minutes for appointment of conitnuation decision:",
                                              value = matrix(c(0, 20,
                                                               0, 20,
                                                               100, 20,
                                                               0, 20,
                                                               0, 30,
                                                               0, 30),
                                                             ncol = 2,
                                                             byrow = TRUE,
                                                             dimnames = list(c("Primary Care - GP",
                                                                               "Primary Care - Nurse Led",
                                                                               "Hepatology / Gastro Consultant",
                                                                               "Hepatology / Gastro CNS",
                                                                               "Community - Diagnostician",
                                                                               "Community - Pharmacist"),
                                                                             c("Percentage (%)", "Mins")
                                                             )),
                                              class = "numeric",
                                  )
                                  ),
                           column(2,
                                  h3("Week 73-103: Dosage Maintenance"),
                                  hr(),
                                  h4("Retention Rate"),
                                  sliderInput("retention_73_103_surv",
                                              "Select the retention rate for patients receiving dosage maintenance:",
                                              value = 99,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  hr(),
                                  h4("Treatment Setting"),
                                  matrixInput("treatment_setting_73_103_matrix_surv",
                                              "Input setting and minutes for delivery of treatment:",
                                              value = matrix(c(10, 20,
                                                               0, 20,
                                                               10, 20,
                                                               75, 20,
                                                               0, 30,
                                                               5, 30),
                                                             ncol = 2,
                                                             byrow = TRUE,
                                                             dimnames = list(c("Primary Care - GP",
                                                                               "Primary Care - Nurse Led",
                                                                               "Hepatology / Gastro Consultant",
                                                                               "Hepatology / Gastro CNS",
                                                                               "Community - Diagnostician",
                                                                               "Community - Pharmacist"),
                                                                             c("Percentage (%)", "Mins")
                                                             )),
                                              class = "numeric",
                                  ),
                                  numericInput("appts_73_103_surv",
                                               "Select the number of appointments over weeks 73-103:",
                                               value = 2,
                                               min = 1,
                                               max = 30,
                                               step = 1),
                                  hr(),
                                  h4("Monitoring Testing"),
                                  numericInput("monitoring_tests_number_73_103_surv",
                                               "Select number of monitoring tests:",
                                               value = 2,
                                               min = 0,
                                               max = NA,
                                               step = 1),
                                  sliderInput("monitoring_tests_73_103_elf_surv",
                                              "Select the percentage of patients undergoing ELF testing:",
                                              value = 0,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  sliderInput("monitoring_tests_73_103_biomarkers_surv",
                                              "Select the percentage of patients undergoing Biomarker testing:",
                                              value = 100,
                                              min = 0,
                                              max = 100,
                                              post = "%")
                           ),
                           column(2,
                                  h3("Week 104+: Ongoing Treatment"),
                                  hr(),
                                  h4("Treatment Weeks to Model"),
                                  numericInput("ongoing_period_surv",
                                               "Select the number of weeks to continue modelling treatment",
                                               value = 104,
                                               min = 52,
                                               max = 208,
                                               step = 1),
                                  hr(),
                                  h4("Retention by End-point"),
                                  sliderInput("retention_end_surv",
                                              "Select the retention rate by the end-point of the model:",
                                              value = 99,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  hr(),
                                  h4("Ongoing Treatment Setting"),
                                  matrixInput("survodutide_ongoing_delivery_setting",
                                              "Input setting and minutes for delivery of treatment:",
                                              value = matrix(c(5, 20,
                                                               70, 20,
                                                               0, 20,
                                                               20, 20,
                                                               0, 30,
                                                               5, 30),
                                                             ncol = 2,
                                                             byrow = TRUE,
                                                             dimnames = list(c("Primary Care - GP",
                                                                               "Primary Care - Nurse Led",
                                                                               "Hepatology / Gastro Consultant",
                                                                               "Hepatology / Gastro CNS",
                                                                               "Community - Diagnostician",
                                                                               "Community - Pharmacist"),
                                                                             c("Percentage (%)", "Mins")
                                                             )),
                                              class = "numeric",
                                  ),
                                  numericInput("appts_ongoing_surv",
                                               "Select the number of appointments post 104 weeks:",
                                               value = 2,
                                               min = 1,
                                               max = NA,
                                               step = 1),
                                  hr(),
                                  h4("Annual Assessment"),
                                  sliderInput("ongoing_annual_prop_elf_surv",
                                              "Set the proportion of annual assessments undergoing ELF test:",
                                              value = 35,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  sliderInput("ongoing_annual_prop_biomarkers_surv",
                                              "Set the proportion of annual assessments undergoing other biomarkers:",
                                              value = 100,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  sliderInput("ongoing_annual_prop_fibro_surv",
                                              "Set the proportion of annual assessments undergoing FibroScan:",
                                              value = 70,
                                              min = 0,
                                              max = 100,
                                              post = "%")
                           )
                         )
                         ),
               nav_panel("Pathway Map",
                         includeHTML("www/images/Survodutide Real World.drawio.html")
               )
             )
           )
  ),
  tabPanel(title = "Resmetirom Pathway Activity Assumptions",
           fluidPage(
             h1("Resmetirom Pathway Activity Assumptions"),
             p("In this section you can set the assumptions about the clinical activities that occur along the treatment pathway.
               These assumptions will then be applied in conjunction with the number starting this treatment and financial assumptions 
               to generate the associated activities and costs. Please refer to the ", strong("Pathway Map"), " tab to review the 
               pathway."),
             downloadButton("download_res_pathway_assumptions", "Download Resmetirom Pathway Assumptions"),
             navset_tab(
               nav_panel("Pathway Assumptions",
                         fluidRow(
                           column(2,
                                  h3("Pre-Treatment: Diagnostics"),
                                  hr(),
                                  h4("Initial Eligibility Assessment"),
                                  sliderInput("mm_assess_prop_res",
                                              "Set percentage undergoing initial eligibility assessment:",
                                              value = 100,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  matrixInput("mm_assess_setting_res",
                                              "Input setting and minutes for initial eligibility assessment:",
                                              value = matrix(c(35, 20,
                                                               0, 20,
                                                               45, 20,
                                                               15, 20,
                                                               0, 30,
                                                               5, 30),
                                                             ncol = 2,
                                                             byrow = TRUE,
                                                             dimnames = list(c("Primary Care - GP",
                                                                               "Primary Care - Nurse Led",
                                                                               "Hepatology / Gastro Consultant",
                                                                               "Hepatology / Gastro CNS",
                                                                               "Community - Diagnostician",
                                                                               "Community - Pharmacist"),
                                                                             c("Percentage (%)", "Mins")
                                                             )),
                                              class = "numeric",
                                  ),
                                  hr(),
                                  h4("Diagnostics"),
                                  sliderInput("pre_liver_biopsy_prop_res",
                                              "Set percentage undergoing biopsy:",
                                              value = 5,
                                              min = 0,
                                              max = 100,
                                              post= "%"),
                                  sliderInput("pre_elf_prop_res",
                                              "Set percentage undergoing ELF testing:",
                                              value = 35,
                                              min = 0,
                                              max = 100,
                                              post= "%"),
                                  sliderInput("pre_biomarkers_prop_res",
                                              "Set percentage undergoing other biomarker testing (inc. LFTs and FIB-4):",
                                              value = 100,
                                              min = 0,
                                              max = 100,
                                              post= "%"),
                                  sliderInput("pre_fibro_prop_res",
                                              "Set percentage undergoing FibroScan:",
                                              value = 70,
                                              min = 0,
                                              max = 100,
                                              post= "%")
                                  
                           ),
                           column(2,
                                  h3("Weeks 0 - 52: Initial Treatment"),
                                  hr(),
                                  h4("Dosage"),
                                  sliderInput("dosage_0_52_res",
                                              "Set proportion of those starting treatment under 100kg:",
                                              value = 80,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  hr(),
                                  h4("Treatment Retention"),
                                  sliderInput("retention_res_0_52",
                                              "Set the retention rate for weeks 0 - 52:",
                                              value = 97,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  hr(),
                                  h4("Treatment Setting"),
                                  matrixInput("treatment_setting_0_52_matrix_res",
                                              "Input setting and minutes for delivery of initial treatment:",
                                              value = matrix(c(0, 20,
                                                               0, 20,
                                                               25, 20,
                                                               75, 20,
                                                               0, 30,
                                                               0, 30),
                                                             ncol = 2,
                                                             byrow = TRUE,
                                                             dimnames = list(c("Primary Care - GP",
                                                                               "Primary Care - Nurse Led",
                                                                               "Hepatology / Gastro Consultant",
                                                                               "Hepatology / Gastro CNS",
                                                                               "Community - Diagnostician",
                                                                               "Community - Pharmacist"),
                                                                             c("Percentage (%)", "Mins")
                                                             )),
                                              class = "numeric",
                                  ),
                                  numericInput("appts_0_52_res",
                                               "Select the number of appointments between week 0 and 52:",
                                               value = 2,
                                               min = 1,
                                               max = NA,
                                               step = 1),
                                  hr(),
                                  h4("Monitoring"),
                                  numericInput("monitor_tests_0_52_res",
                                               "Set the number of monitoring tests:",
                                               value = 2,
                                               min = 0,
                                               max = NA,
                                               step = 1),
                                  sliderInput("monitor_tests_0_52_elf_res",
                                              "Set the proportion of monitoring tests including ELF testing:",
                                              value = 0,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  sliderInput("monitor_tests_0_52_biomarkers_res",
                                              "Set the proportion of monitoring tests including other biomarkers:",
                                              value = 100,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  sliderInput("monitor_tests_0_52_fibro_res",
                                              "Se the proportion of monitoring tests including a Fibroscan:",
                                              value = 0,
                                              min = 0,
                                              max = 100,
                                              post = "%")
                           ),
                           column(2,
                                  h3("Weeks 52-71: Efficacy and Continuation"),
                                  hr(),
                                  h4("Efficacy Assessment Diagnostics"),
                                  sliderInput("efficacy_52_liver_biopsy_res",
                                              "Set the percentage of paitents undergoing a liver biopsy:",
                                              value = 0,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  sliderInput("efficacy_52_elf_prop_res",
                                              "Set the percentage of patients undergoing ELF testing:",
                                              value = 35,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  sliderInput("efficacy_52_fibro_prop_res",
                                              "Set the percentage of patients undergoing a Fibroscan:",
                                              value = 70,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  sliderInput("efficacy_52_biomarkers_prop_res",
                                              "Set the percentage of patients undergoing other biomarkers:",
                                              value = 100,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  hr(),
                                  h4("Continuation Decision"),
                                  sliderInput("continuation_52_prop_res",
                                              "Select the percentage of patients continuing with treatment:",
                                              value = 49,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  matrixInput("continuation_52_delivery_setting_res",
                                              "Input setting and minutes for appointment of conitnuation decision:",
                                              value = matrix(c(0, 20,
                                                               0, 20,
                                                               100, 20,
                                                               0, 20,
                                                               0, 30,
                                                               0, 30),
                                                             ncol = 2,
                                                             byrow = TRUE,
                                                             dimnames = list(c("Primary Care - GP",
                                                                               "Primary Care - Nurse Led",
                                                                               "Hepatology / Gastro Consultant",
                                                                               "Hepatology / Gastro CNS",
                                                                               "Community - Diagnostician",
                                                                               "Community - Pharmacist"),
                                                                             c("Percentage (%)", "Mins")
                                                             )),
                                              class = "numeric",
                                  ),
                                  hr(),
                                  h4("Week 52 - 71 Treatment Delivery"),
                                  sliderInput("retention_res_52_71",
                                              "Set the retention rate for weeks 52 - 71:",
                                              value = 99,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  matrixInput("treatment_setting_52_71_matrix_res",
                                              "Input setting and minutes for delivery of continued treatment:",
                                              value = matrix(c(10, 20,
                                                               0, 20,
                                                               10, 20,
                                                               75, 20,
                                                               0, 30,
                                                               5, 30),
                                                             ncol = 2,
                                                             byrow = TRUE,
                                                             dimnames = list(c("Primary Care - GP",
                                                                               "Primary Care - Nurse Led",
                                                                               "Hepatology / Gastro Consultant",
                                                                               "Hepatology / Gastro CNS",
                                                                               "Community - Diagnostician",
                                                                               "Community - Pharmacist"),
                                                                             c("Percentage (%)", "Mins")
                                                             )),
                                              class = "numeric",
                                  ),
                                  numericInput("appts_52_71_res",
                                               "Select the number of appointments between week 52 and 71:",
                                               value = 2,
                                               min = 1,
                                               max = NA,
                                               step = 1),
                                  
                           ),
                           column(2,
                                  h3("Week 72: Continuation Decision"),
                                  hr(),
                                  h4("Efficacy Assessment"),
                                  sliderInput("efficacy_liver_biopsy_res",
                                              "Set the percentage of paitents undergoing a liver biopsy:",
                                              value = 5,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  sliderInput("efficacy_elf_prop_res",
                                              "Set the percentage of patients undergoing ELF testing:",
                                              value = 35,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  sliderInput("efficacy_fibro_prop_res",
                                              "Set the percentage of patients undergoing a Fibroscan:",
                                              value = 70,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  sliderInput("efficacy_biomarkers_prop_res",
                                              "Set the percentage of patients undergoing other biomarkers:",
                                              value = 100,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  hr(),
                                  h4("Continuation Decision"),
                                  sliderInput("continuation_prop_res",
                                              "Select the percentage of patients continuing with treatment:",
                                              value = 80,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  matrixInput("continuation_delivery_setting_res",
                                              "Input setting and minutes for appointment of conitnuation decision:",
                                              value = matrix(c(0, 20,
                                                               0, 20,
                                                               100, 20,
                                                               0, 20,
                                                               0, 30,
                                                               0, 30),
                                                             ncol = 2,
                                                             byrow = TRUE,
                                                             dimnames = list(c("Primary Care - GP",
                                                                               "Primary Care - Nurse Led",
                                                                               "Hepatology / Gastro Consultant",
                                                                               "Hepatology / Gastro CNS",
                                                                               "Community - Diagnostician",
                                                                               "Community - Pharmacist"),
                                                                             c("Percentage (%)", "Mins")
                                                             )),
                                              class = "numeric",
                                  )
                                  ),
                           column(2,
                                  h3("Week 73-103: Dosage Maintenance"),
                                  hr(),
                                  h4("Retention Rate"),
                                  sliderInput("retention_73_103_res",
                                              "Select the retention rate for patients receiving dosage maintenance:",
                                              value = 99,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  hr(),
                                  h4("Treatment Setting"),
                                  matrixInput("treatment_setting_73_103_matrix_res",
                                              "Input setting and minutes for delivery of treatment:",
                                              value = matrix(c(10, 20,
                                                               0, 20,
                                                               10, 20,
                                                               75, 20,
                                                               0, 30,
                                                               5, 30),
                                                             ncol = 2,
                                                             byrow = TRUE,
                                                             dimnames = list(c("Primary Care - GP",
                                                                               "Primary Care - Nurse Led",
                                                                               "Hepatology / Gastro Consultant",
                                                                               "Hepatology / Gastro CNS",
                                                                               "Community - Diagnostician",
                                                                               "Community - Pharmacist"),
                                                                             c("Percentage (%)", "Mins")
                                                             )),
                                              class = "numeric",
                                  ),
                                  numericInput("appts_73_103_res",
                                               "Select the number of appointments over weeks 73-103:",
                                               value = 2,
                                               min = 1,
                                               max = 30,
                                               step = 1),
                                  hr(),
                                  h4("Monitoring Testing"),
                                  numericInput("monitoring_tests_number_73_103_res",
                                               "Select number of monitoring tests:",
                                               value = 2,
                                               min = 0,
                                               max = NA,
                                               step = 1),
                                  sliderInput("monitoring_tests_73_103_elf_res",
                                              "Select the percentage of patients undergoing ELF testing:",
                                              value = 0,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  sliderInput("monitoring_tests_73_103_biomarkers_res",
                                              "Select the percentage of patients undergoing Biomarker testing:",
                                              value = 100,
                                              min = 0,
                                              max = 100,
                                              post = "%")
                           ),
                           column(2,
                                  h3("Week 104+: Ongoing Treatment"),
                                  hr(),
                                  h4("Treatment Weeks to Model"),
                                  numericInput("ongoing_period_res",
                                               "Select the number of weeks to continue modelling treatment",
                                               value = 104,
                                               min = 52,
                                               max = 208,
                                               step = 1),
                                  hr(),
                                  h4("Retention by End-point"),
                                  sliderInput("retention_end_res",
                                              "Select the retention rate by the end-point of the model:",
                                              value = 99,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  hr(),
                                  h4("Ongoing Treatment Setting"),
                                  matrixInput("resmetirom_ongoing_delivery_setting",
                                              "Input setting and minutes for delivery of treatment:",
                                              value = matrix(c(5, 20,
                                                               70, 20,
                                                               0, 20,
                                                               20, 20,
                                                               0, 30,
                                                               5, 30),
                                                             ncol = 2,
                                                             byrow = TRUE,
                                                             dimnames = list(c("Primary Care - GP",
                                                                               "Primary Care - Nurse Led",
                                                                               "Hepatology / Gastro Consultant",
                                                                               "Hepatology / Gastro CNS",
                                                                               "Community - Diagnostician",
                                                                               "Community - Pharmacist"),
                                                                             c("Percentage (%)", "Mins")
                                                             )),
                                              class = "numeric",
                                  ),
                                  numericInput("appts_ongoing_res",
                                               "Select the number of appointments post 104 weeks:",
                                               value = 2,
                                               min = 1,
                                               max = NA,
                                               step = 1),
                                  hr(),
                                  h4("Annual Assessment"),
                                  sliderInput("ongoing_annual_prop_elf_res",
                                              "Set the proportion of annual assessments undergoing ELF test:",
                                              value = 35,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  sliderInput("ongoing_annual_prop_biomarkers_res",
                                              "Set the proportion of annual assessments undergoing other biomarkers:",
                                              value = 100,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  sliderInput("ongoing_annual_prop_fibro_res",
                                              "Set the proportion of annual assessments undergoing FibroScan:",
                                              value = 70,
                                              min = 0,
                                              max = 100,
                                              post = "%")
                           )
                         )
               ),
               nav_panel("Pathway Map",
                         includeHTML("www/images/Resmetirom Real World.drawio.html")
               )
             )
           )
  ),
  tabPanel(title = "Lanifibranor Pathway Activity Assumptions",
           fluidPage(
             h1("Lanifibranor Pathway Activity Assumptions"),
             p("In this section you can set the assumptions about the clinical activities that occur along the treatment pathway.
               These assumptions will then be applied in conjunction with the number starting this treatment and financial assumptions 
               to generate the associated activities and costs. Please refer to the ", strong("Pathway Map"), " tab to review the 
               pathway."),
             downloadButton("download_lan_pathway_assumptions", "Download Lanifibranor Pathway Assumptions"),
             navset_tab(
               nav_panel("Pathway Assumptions",
                         fluidRow(
                           column(2,
                                  h3("Pre-Treatment: Diagnostics"),
                                  hr(),
                                  h4("Initial Eligibility Assessment"),
                                  sliderInput("mm_assess_prop_lan",
                                              "Set percentage undergoing initial eligibility assessment:",
                                              value = 100,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  matrixInput("mm_assess_setting_lan",
                                              "Input setting and minutes for initial eligibility assessment:",
                                              value = matrix(c(35, 20,
                                                               0, 20,
                                                               45, 20,
                                                               15, 20,
                                                               0, 30,
                                                               5, 30),
                                                             ncol = 2,
                                                             byrow = TRUE,
                                                             dimnames = list(c("Primary Care - GP",
                                                                               "Primary Care - Nurse Led",
                                                                               "Hepatology / Gastro Consultant",
                                                                               "Hepatology / Gastro CNS",
                                                                               "Community - Diagnostician",
                                                                               "Community - Pharmacist"),
                                                                             c("Percentage (%)", "Mins")
                                                             )),
                                              class = "numeric",
                                  ),
                                  hr(),
                                  h4("Diagnostics"),
                                  sliderInput("pre_liver_biopsy_prop_lan",
                                              "Set percentage undergoing biopsy:",
                                              value = 5,
                                              min = 0,
                                              max = 100,
                                              post= "%"),
                                  sliderInput("pre_elf_prop_lan",
                                              "Set percentage undergoing ELF testing:",
                                              value = 35,
                                              min = 0,
                                              max = 100,
                                              post= "%"),
                                  sliderInput("pre_biomarkers_prop_lan",
                                              "Set percentage undergoing other biomarker testing (inc. LFTs and FIB-4):",
                                              value = 100,
                                              min = 0,
                                              max = 100,
                                              post= "%"),
                                  sliderInput("pre_fibro_prop_lan",
                                              "Set percentage undergoing FibroScan:",
                                              value = 70,
                                              min = 0,
                                              max = 100,
                                              post= "%")
                                  
                           ),
                           column(2,
                                  h3("Weeks 0 - 52: Initial Treatment"),
                                  hr(),
                                  h4("Dosage"),
                                  sliderInput("dosage_0_52_lan",
                                              "Set proportion of those starting treatment on 1200mg:",
                                              value = 50,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  hr(),
                                  h4("Treatment Retention"),
                                  sliderInput("retention_lan_0_52",
                                              "Set the retention rate for weeks 0 - 52:",
                                              value = 95,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  hr(),
                                  h4("Treatment Setting"),
                                  matrixInput("treatment_setting_0_52_matrix_lan",
                                              "Input setting and minutes for delivery of initial treatment:",
                                              value = matrix(c(0, 20,
                                                               0, 20,
                                                               25, 20,
                                                               75, 20,
                                                               0, 30,
                                                               0, 30),
                                                             ncol = 2,
                                                             byrow = TRUE,
                                                             dimnames = list(c("Primary Care - GP",
                                                                               "Primary Care - Nurse Led",
                                                                               "Hepatology / Gastro Consultant",
                                                                               "Hepatology / Gastro CNS",
                                                                               "Community - Diagnostician",
                                                                               "Community - Pharmacist"),
                                                                             c("Percentage (%)", "Mins")
                                                             )),
                                              class = "numeric",
                                  ),
                                  numericInput("appts_0_52_lan",
                                               "Select the number of appointments between week 0 and 52:",
                                               value = 2,
                                               min = 1,
                                               max = NA,
                                               step = 1),
                                  hr(),
                                  h4("Monitoring"),
                                  numericInput("monitor_tests_0_52_lan",
                                               "Set the number of monitoring tests:",
                                               value = 2,
                                               min = 0,
                                               max = NA,
                                               step = 1),
                                  sliderInput("monitor_tests_0_52_elf_lan",
                                              "Set the proportion of monitoring tests including ELF testing:",
                                              value = 0,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  sliderInput("monitor_tests_0_52_biomarkers_lan",
                                              "Set the proportion of monitoring tests including other biomarkers:",
                                              value = 100,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  sliderInput("monitor_tests_0_52_fibro_lan",
                                              "Se the proportion of monitoring tests including a Fibroscan:",
                                              value = 0,
                                              min = 0,
                                              max = 100,
                                              post = "%")
                           ),
                           column(2,
                                  h3("Weeks 52-71: Efficacy and Continuation"),
                                  hr(),
                                  h4("Efficacy Assessment Diagnostics"),
                                  sliderInput("efficacy_52_liver_biopsy_lan",
                                              "Set the percentage of paitents undergoing a liver biopsy:",
                                              value = 0,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  sliderInput("efficacy_52_elf_prop_lan",
                                              "Set the percentage of patients undergoing ELF testing:",
                                              value = 35,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  sliderInput("efficacy_52_fibro_prop_lan",
                                              "Set the percentage of patients undergoing a Fibroscan:",
                                              value = 70,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  sliderInput("efficacy_52_biomarkers_prop_lan",
                                              "Set the percentage of patients undergoing other biomarkers:",
                                              value = 100,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  hr(),
                                  h4("Continuation Decision"),
                                  sliderInput("continuation_52_prop_lan",
                                              "Select the percentage of patients continuing with treatment:",
                                              value = 49,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  matrixInput("continuation_52_delivery_setting_lan",
                                              "Input setting and minutes for appointment of conitnuation decision:",
                                              value = matrix(c(0, 20,
                                                               0, 20,
                                                               100, 20,
                                                               0, 20,
                                                               0, 30,
                                                               0, 30),
                                                             ncol = 2,
                                                             byrow = TRUE,
                                                             dimnames = list(c("Primary Care - GP",
                                                                               "Primary Care - Nurse Led",
                                                                               "Hepatology / Gastro Consultant",
                                                                               "Hepatology / Gastro CNS",
                                                                               "Community - Diagnostician",
                                                                               "Community - Pharmacist"),
                                                                             c("Percentage (%)", "Mins")
                                                             )),
                                              class = "numeric",
                                  ),
                                  hr(),
                                  h4("Week 52 - 71 Treatment Delivery"),
                                  sliderInput("retention_lan_52_71",
                                              "Set the retention rate for weeks 52 - 71:",
                                              value = 99,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  matrixInput("treatment_setting_52_71_matrix_lan",
                                              "Input setting and minutes for delivery of continued treatment:",
                                              value = matrix(c(10, 20,
                                                               0, 20,
                                                               10, 20,
                                                               75, 20,
                                                               0, 30,
                                                               5, 30),
                                                             ncol = 2,
                                                             byrow = TRUE,
                                                             dimnames = list(c("Primary Care - GP",
                                                                               "Primary Care - Nurse Led",
                                                                               "Hepatology / Gastro Consultant",
                                                                               "Hepatology / Gastro CNS",
                                                                               "Community - Diagnostician",
                                                                               "Community - Pharmacist"),
                                                                             c("Percentage (%)", "Mins")
                                                             )),
                                              class = "numeric",
                                  ),
                                  numericInput("appts_52_71_lan",
                                               "Select the number of appointments between week 52 and 71:",
                                               value = 2,
                                               min = 1,
                                               max = NA,
                                               step = 1),
                                  
                           ),
                           column(2,
                                  h3("Week 72: Continuation Decision"),
                                  hr(),
                                  h4("Efficacy Assessment"),
                                  sliderInput("efficacy_liver_biopsy_lan",
                                              "Set the percentage of paitents undergoing a liver biopsy:",
                                              value = 5,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  sliderInput("efficacy_elf_prop_lan",
                                              "Set the percentage of patients undergoing ELF testing:",
                                              value = 35,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  sliderInput("efficacy_fibro_prop_lan",
                                              "Set the percentage of patients undergoing a Fibroscan:",
                                              value = 70,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  sliderInput("efficacy_biomarkers_prop_lan",
                                              "Set the percentage of patients undergoing other biomarkers:",
                                              value = 100,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  hr(),
                                  h4("Continuation Decision"),
                                  sliderInput("continuation_prop_lan",
                                              "Select the percentage of patients continuing with treatment:",
                                              value = 80,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  matrixInput("continuation_delivery_setting_lan",
                                              "Input setting and minutes for appointment of conitnuation decision:",
                                              value = matrix(c(0, 20,
                                                               0, 20,
                                                               100, 20,
                                                               0, 20,
                                                               0, 30,
                                                               0, 30),
                                                             ncol = 2,
                                                             byrow = TRUE,
                                                             dimnames = list(c("Primary Care - GP",
                                                                               "Primary Care - Nurse Led",
                                                                               "Hepatology / Gastro Consultant",
                                                                               "Hepatology / Gastro CNS",
                                                                               "Community - Diagnostician",
                                                                               "Community - Pharmacist"),
                                                                             c("Percentage (%)", "Mins")
                                                             )),
                                              class = "numeric",
                                  )
                           ),
                           column(2,
                                  h3("Week 73-103: Dosage Maintenance"),
                                  hr(),
                                  h4("Retention Rate"),
                                  sliderInput("retention_73_103_lan",
                                              "Select the retention rate for patients receiving dosage maintenance:",
                                              value = 99,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  hr(),
                                  h4("Treatment Setting"),
                                  matrixInput("treatment_setting_73_103_matrix_lan",
                                              "Input setting and minutes for delivery of treatment:",
                                              value = matrix(c(10, 20,
                                                               0, 20,
                                                               10, 20,
                                                               75, 20,
                                                               0, 30,
                                                               5, 30),
                                                             ncol = 2,
                                                             byrow = TRUE,
                                                             dimnames = list(c("Primary Care - GP",
                                                                               "Primary Care - Nurse Led",
                                                                               "Hepatology / Gastro Consultant",
                                                                               "Hepatology / Gastro CNS",
                                                                               "Community - Diagnostician",
                                                                               "Community - Pharmacist"),
                                                                             c("Percentage (%)", "Mins")
                                                             )),
                                              class = "numeric",
                                  ),
                                  numericInput("appts_73_103_lan",
                                               "Select the number of appointments over weeks 73-103:",
                                               value = 2,
                                               min = 1,
                                               max = 30,
                                               step = 1),
                                  hr(),
                                  h4("Monitoring Testing"),
                                  numericInput("monitoring_tests_number_73_103_lan",
                                               "Select number of monitoring tests:",
                                               value = 2,
                                               min = 0,
                                               max = NA,
                                               step = 1),
                                  sliderInput("monitoring_tests_73_103_elf_lan",
                                              "Select the percentage of patients undergoing ELF testing:",
                                              value = 0,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  sliderInput("monitoring_tests_73_103_biomarkers_lan",
                                              "Select the percentage of patients undergoing Biomarker testing:",
                                              value = 100,
                                              min = 0,
                                              max = 100,
                                              post = "%")
                           ),
                           column(2,
                                  h3("Week 104+: Ongoing Treatment"),
                                  hr(),
                                  h4("Treatment Weeks to Model"),
                                  numericInput("ongoing_period_lan",
                                               "Select the number of weeks to continue modelling treatment",
                                               value = 104,
                                               min = 52,
                                               max = 208,
                                               step = 1),
                                  hr(),
                                  h4("Retention by End-point"),
                                  sliderInput("retention_end_lan",
                                              "Select the retention rate by the end-point of the model:",
                                              value = 99,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  hr(),
                                  h4("Ongoing Treatment Setting"),
                                  matrixInput("lanifibranor_ongoing_delivery_setting",
                                              "Input setting and minutes for delivery of treatment:",
                                              value = matrix(c(5, 20,
                                                               70, 20,
                                                               0, 20,
                                                               20, 20,
                                                               0, 30,
                                                               5, 30),
                                                             ncol = 2,
                                                             byrow = TRUE,
                                                             dimnames = list(c("Primary Care - GP",
                                                                               "Primary Care - Nurse Led",
                                                                               "Hepatology / Gastro Consultant",
                                                                               "Hepatology / Gastro CNS",
                                                                               "Community - Diagnostician",
                                                                               "Community - Pharmacist"),
                                                                             c("Percentage (%)", "Mins")
                                                             )),
                                              class = "numeric",
                                  ),
                                  numericInput("appts_ongoing_lan",
                                               "Select the number of appointments post 104 weeks:",
                                               value = 2,
                                               min = 1,
                                               max = NA,
                                               step = 1),
                                  hr(),
                                  h4("Annual Assessment"),
                                  sliderInput("ongoing_annual_prop_elf_lan",
                                              "Set the proportion of annual assessments undergoing ELF test:",
                                              value = 35,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  sliderInput("ongoing_annual_prop_biomarkers_lan",
                                              "Set the proportion of annual assessments undergoing other biomarkers:",
                                              value = 100,
                                              min = 0,
                                              max = 100,
                                              post = "%"),
                                  sliderInput("ongoing_annual_prop_fibro_lan",
                                              "Set the proportion of annual assessments undergoing FibroScan:",
                                              value = 70,
                                              min = 0,
                                              max = 100,
                                              post = "%")
                           )
                         )
               ),
               nav_panel("Pathway Map",
                         includeHTML("www/images/Lanifibranor Real World.drawio.html")
               )
             )
           )
  ),
  tabPanel(title = "Financial Assumptions",
           fluidPage(
             h1("Financial Assumptions"),
             p("This section contains the assumptions covering the costs of treatments and associated 
               clinical activities. These can be amended in each of the sections below:"),
             downloadButton("download_fin_assumptions", "Download Finance Assumptions"),
             br(),
             column(3,
                    h3("Diagnostic Investigations"),
                    hr(),
                    h4("Liver Biopsy"),
                    numericInput("fin_liv_bio",
                                 "Specify the cost of a liver biopsy:",
                                 value = 962,
                                 min = 0,
                                 max = NA,
                                 step = 1),
                    h4("ELF Test"),
                    numericInput("fin_elf",
                                 "Specify the cost of an ELF test:",
                                 value = 136,
                                 min = 0,
                                 max = NA,
                                 step = 1),
                    h4("Biomarker Tests"),
                    numericInput("fin_biomarkers",
                                 "Specify the cost of other Biomarker tests:",
                                 value = 38,
                                 min = 0,
                                 max = NA,
                                 step = 1),
                    h4("Fibroscanning"),
                    numericInput("fin_fibro",
                                 "Specify the cost of Fibroscanning:",
                                 value = 70,
                                 min = 0,
                                 max = NA,
                                 step = 1)
             ),
             column(3,
                    h3("Appointment Costs per Patient Hour"),
                    hr(),
                    h4("GP Appointment"),
                    numericInput("fin_appt_pc_gp_pph",
                                 "Select the cost per patient hour of a GP appointment:",
                                 value = 296,
                                 min = 0,
                                 max = NA,
                                 step = 1),
                    h4("Practice Nurse Appointment"),
                    numericInput("fin_appt_pc_nur_pph",
                                 "Select the cost per patient hour of a Practice Nurse appointment:",
                                 value = 74,
                                 min = 0,
                                 max = NA,
                                 step = 1),
                    h4("Secondary Care Hepatology/Gastroenterology Consultant"),
                    numericInput("fin_appt_sc_hgc_pph",
                                 "Select the cost per patient hour of a Consultant:",
                                 value = 141,
                                 min = 0,
                                 max = NA,
                                 step = 1),
                    h4("Secondary Care Hepatology/Gastroenterology Nurse"),
                    numericInput("fin_appt_sc_hgn_pph",
                                 "Select the cost per patient hour of a secondary care nurse:",
                                 value = 61,
                                 min = 0,
                                 max = NA,
                                 step = 1),
                    h4("Community Diagnostician"),
                    numericInput("fin_appt_com_dia_pph",
                                 "Select the cost per patient hour of a community diagnostician:",
                                 value = 72,
                                 min = 0,
                                 max = NA,
                                 step = 1),
                    h4("Community Pharmacist"),
                    numericInput("fin_appt_com_pha_pph",
                                 "Select the cost per patient hour of a community pharmacist:",
                                 value = 63,
                                 min = 0,
                                 max = NA,
                                 step = 1)
                    ),
             column(3,
                    h3("Activity Costs"),
                    hr(),
                    h4("Face to Face Appointments - Secondary Care"),
                    numericInput("fin_appt_sc_f2f_first_cons",
                                 "Select the cost of a consultant led first appointment - face to face:",
                                 value = 279,
                                 min = 0,
                                 max = NA,
                                 step = 1),
                    numericInput("fin_appt_sc_f2f_fu_cons",
                                 "Select the cost of a consultant lef follow-up appointment - face to face:",
                                 value = 316,
                                 min = 0,
                                 max = NA,
                                 step = 1),
                    numericInput("fin_appt_sc_f2f_first_noncons",
                                 "Select the cost of a non-consultant led first appointment - face to face:",
                                 value = 166,
                                 min = 0,
                                 max = NA,
                                 step = 1),
                    numericInput("fin_appt_sc_f2f_fu_noncons",
                                 "Select the cost of a non-consultant led follow-up appointment - face to face:",
                                 value = 225,
                                 min = 0,
                                 max = NA,
                                 step = 1)
                    )
           )
  ),
  tabPanel(title = "Model Outputs - Activity and Costs",
           fluidPage(
           h1("Model Outputs"),
           hr(),
           p("This section contains the outputs of the model. The first section has an overall summary showing 
             key outputs including:",
             tags$ul(
               tags$li("Total cost for each treatment pathway and average cost per patient starting treatment"),
               tags$li("Total costs for each stage of the pathway"),
               tags$li("A split between the costs of all treatments modelled together and those of individual treatments")
             )),
           p("There are additional sections that include more detail for each stage of the pathway. Again, this provides 
             a view of activity and costs overall and for each treatment individually."),
           h3("Overall Pathway Costs"),
           navset_tab(
             nav_panel("All Treatments",
                       p("The table below provides a summary of:",
                         tags$ul(
                           tags$li("The number of patients starting treatment and reaching the modelling end-point"),
                           tags$li("The total cost to treat these patients and an average cost per pathway"),
                           tags$li("Costs for the main stages of the pathway")
                         )),
                       DTOutput("combined_all_DT")
                       ),
             nav_panel("Semaglutide",
                       p("The table below provides a summary of:",
                         tags$ul(
                           tags$li("The number of patients starting treatment and reaching the modelling end-point"),
                           tags$li("The total cost to treat these patients and an average cost per pathway"),
                           tags$li("Costs for the main stages of the pathway")
                         )),
                       DTOutput("combined_sem_DT")
                       
             ),
             nav_panel("Survodutide",
                       p("The table below provides a summary of:",
                         tags$ul(
                           tags$li("The number of patients starting treatment and reaching the modelling end-point"),
                           tags$li("The total cost to treat these patients and an average cost per pathway"),
                           tags$li("Costs for the main stages of the pathway")
                         )),
                       DTOutput("combined_surv_DT")
             ),
             nav_panel("Resmetirom",
                       p("The table below provides a summary of:",
                         tags$ul(
                           tags$li("The number of patients starting treatment and reaching the modelling end-point"),
                           tags$li("The total cost to treat these patients and an average cost per pathway"),
                           tags$li("Costs for the main stages of the pathway")
                         )),
                       DTOutput("combined_res_DT")
             ),
             nav_panel("Lanifibranor",
                       p("The table below provides a summary of:",
                         tags$ul(
                           tags$li("The number of patients starting treatment and reaching the modelling end-point"),
                           tags$li("The total cost to treat these patients and an average cost per pathway"),
                           tags$li("Costs for the main stages of the pathway")
                         )),
                       DTOutput("combined_lan_DT")
             )
           ),
           br(),
           h3("Pre-Treatment"),
           hr(),
           h4("Initial Eligibility Assessment"),
           p("Some IE Assessment text"),
           navset_tab(
             nav_panel("All Treatments",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the applied assumptions the modelled activity and costs for pre-treatment 
                                     initial eligibility assessments are:",
                                     tags$ul(
                                       tags$li("A central estimate of ",
                                               textOutput("pre_treat_mm_assess_all_sum_1_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_mm_assess_all_sum_1_cost", inline = TRUE)
                                       ),
                                       tags$li("A lower estimate of ",
                                               textOutput("pre_treat_mm_assess_all_sum_2_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_mm_assess_all_sum_2_cost", inline = TRUE)
                                       ),
                                       tags$li("An upper estimate of ",
                                               textOutput("pre_treat_mm_assess_all_sum_3_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_mm_assess_all_sum_3_cost", inline = TRUE)
                                       )
                                     ))),
                         nav_panel("Data Table", DTOutput("pre_treat_mm_assess_all_DT"))
                       )
             ),
             nav_panel("Semaglutide",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the applied assumptions the modelled activity and costs for pre-treatment 
                                     initial eligibility assessments are:",
                                     tags$ul(
                                       tags$li("A central estimate of ",
                                               textOutput("pre_treat_mm_assess_sem_sum_1_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_mm_assess_sem_sum_1_cost", inline = TRUE)
                                       ),
                                       tags$li("A lower estimate of ",
                                               textOutput("pre_treat_mm_assess_sem_sum_2_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_mm_assess_sem_sum_2_cost", inline = TRUE)
                                       ),
                                       tags$li("An upper estimate of ",
                                               textOutput("pre_treat_mm_assess_sem_sum_3_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_mm_assess_sem_sum_3_cost", inline = TRUE)
                                       )
                                     ))),
                         nav_panel("Data Table", DTOutput("pre_treat_mm_assess_sem_DT"))
                       )
             ),
             nav_panel("Survodutide",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the applied assumptions the modelled activity and costs for pre-treatment 
                                     initial eligibility assessments are:",
                                     tags$ul(
                                       tags$li("A central estimate of ",
                                               textOutput("pre_treat_mm_assess_surv_sum_1_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_mm_assess_surv_sum_1_cost", inline = TRUE)
                                       ),
                                       tags$li("A lower estimate of ",
                                               textOutput("pre_treat_mm_assess_surv_sum_2_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_mm_assess_surv_sum_2_cost", inline = TRUE)
                                       ),
                                       tags$li("An upper estimate of ",
                                               textOutput("pre_treat_mm_assess_surv_sum_3_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_mm_assess_surv_sum_3_cost", inline = TRUE)
                                       )
                                     ))),
                         nav_panel("Data Table", DTOutput("pre_treat_mm_assess_surv_DT"))
                       )
             ),
             nav_panel("Resmetirom",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the applied assumptions the modelled activity and costs for pre-treatment 
                                     initial eligibility assessments are:",
                                     tags$ul(
                                       tags$li("A central estimate of ",
                                               textOutput("pre_treat_mm_assess_res_sum_1_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_mm_assess_res_sum_1_cost", inline = TRUE)
                                       ),
                                       tags$li("A lower estimate of ",
                                               textOutput("pre_treat_mm_assess_res_sum_2_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_mm_assess_res_sum_2_cost", inline = TRUE)
                                       ),
                                       tags$li("An upper estimate of ",
                                               textOutput("pre_treat_mm_assess_res_sum_3_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_mm_assess_res_sum_3_cost", inline = TRUE)
                                       )
                                     ))),
                         nav_panel("Data Table", DTOutput("pre_treat_mm_assess_res_DT"))
                       )
             ),
             nav_panel("Lanifibranor",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the applied assumptions the modelled activity and costs for pre-treatment 
                                     initial eligibility assessments are:",
                                     tags$ul(
                                       tags$li("A central estimate of ",
                                               textOutput("pre_treat_mm_assess_lan_sum_1_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_mm_assess_lan_sum_1_cost", inline = TRUE)
                                       ),
                                       tags$li("A lower estimate of ",
                                               textOutput("pre_treat_mm_assess_lan_sum_2_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_mm_assess_lan_sum_2_cost", inline = TRUE)
                                       ),
                                       tags$li("An upper estimate of ",
                                               textOutput("pre_treat_mm_assess_lan_sum_3_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_mm_assess_lan_sum_3_cost", inline = TRUE)
                                       )
                                     ))),
                         nav_panel("Data Table", DTOutput("pre_treat_mm_assess_lan_DT"))
                       )
             )
           ),
           br(),
           h4("Liver Biopsies"),
           p("Some text on Liver Biopsies"),
           navset_tab(
             nav_panel("All Treatments",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the applied assumptions the modelled activity and costs for 
                                     pre-treatment liver biopsies are:",
                                     tags$ul(
                                       tags$li("A central estimate of ",
                                               textOutput("pre_treat_biopsy_all_sum_1_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_biopsy_all_sum_1_cost", inline = TRUE)
                                       ),
                                       tags$li("A lower estimate of ",
                                               textOutput("pre_treat_biopsy_all_sum_2_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_biopsy_all_sum_2_cost", inline = TRUE)
                                       ),
                                       tags$li("An upper estimate of ",
                                               textOutput("pre_treat_biopsy_all_sum_3_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_biopsy_all_sum_3_cost", inline = TRUE)
                                       )
                                     ))
                         ),
                         nav_panel("Data Table", DTOutput("pre_treat_biopsy_all_DT"))
                       )
             ),
             nav_panel("Semaglutide",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the applied assumptions the modelled activity and costs for 
                                     pre-treatment liver biopsies are:",
                                     tags$ul(
                                       tags$li("A central estimate of ",
                                               textOutput("pre_treat_biopsy_sem_sum_1_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_biopsy_sem_sum_1_cost", inline = TRUE)
                                       ),
                                       tags$li("A lower estimate of ",
                                               textOutput("pre_treat_biopsy_sem_sum_2_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_biopsy_sem_sum_2_cost", inline = TRUE)
                                       ),
                                       tags$li("An upper estimate of ",
                                               textOutput("pre_treat_biopsy_sem_sum_3_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_biopsy_sem_sum_3_cost", inline = TRUE)
                                       )
                                     ))
                                   ),
                         nav_panel("Data Table", DTOutput("pre_treat_biopsy_sem_DT"))
                       )
             ),
             nav_panel("Survodutide",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the applied assumptions the modelled activity and costs for 
                                     pre-treatment liver biopsies are:",
                                     tags$ul(
                                       tags$li("A central estimate of ",
                                               textOutput("pre_treat_biopsy_surv_sum_1_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_biopsy_surv_sum_1_cost", inline = TRUE)
                                       ),
                                       tags$li("A lower estimate of ",
                                               textOutput("pre_treat_biopsy_surv_sum_2_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_biopsy_surv_sum_2_cost", inline = TRUE)
                                       ),
                                       tags$li("An upper estimate of ",
                                               textOutput("pre_treat_biopsy_surv_sum_3_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_biopsy_surv_sum_3_cost", inline = TRUE)
                                       )
                                     ))
                                   ),
                         nav_panel("Data Table", DTOutput("pre_treat_biopsy_surv_DT"))
                       )
             ),
             nav_panel("Resmetirom",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the applied assumptions the modelled activity and costs for 
                                     pre-treatment liver biopsies are:",
                                     tags$ul(
                                       tags$li("A central estimate of ",
                                               textOutput("pre_treat_biopsy_res_sum_1_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_biopsy_res_sum_1_cost", inline = TRUE)
                                       ),
                                       tags$li("A lower estimate of ",
                                               textOutput("pre_treat_biopsy_res_sum_2_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_biopsy_res_sum_2_cost", inline = TRUE)
                                       ),
                                       tags$li("An upper estimate of ",
                                               textOutput("pre_treat_biopsy_res_sum_3_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_biopsy_res_sum_3_cost", inline = TRUE)
                                       )
                                     ))
                         ),
                         nav_panel("Data Table", DTOutput("pre_treat_biopsy_res_DT"))
                       )
             ),
             nav_panel("Lanifibranor",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the applied assumptions the modelled activity and costs for 
                                     pre-treatment liver biopsies are:",
                                     tags$ul(
                                       tags$li("A central estimate of ",
                                               textOutput("pre_treat_biopsy_lan_sum_1_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_biopsy_lan_sum_1_cost", inline = TRUE)
                                       ),
                                       tags$li("A lower estimate of ",
                                               textOutput("pre_treat_biopsy_lan_sum_2_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_biopsy_lan_sum_2_cost", inline = TRUE)
                                       ),
                                       tags$li("An upper estimate of ",
                                               textOutput("pre_treat_biopsy_lan_sum_3_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_biopsy_lan_sum_3_cost", inline = TRUE)
                                       )
                                     ))
                         ),
                         nav_panel("Data Table", DTOutput("pre_treat_biopsy_lan_DT"))
                       )
             )
             
           ),
           br(),
           h4("ELF Testing"),
           p("Some text on ELF Testing"),
           navset_tab(
             nav_panel("All Treatments",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the applied assumptions the modelled activity and costs for 
                                     pre-treatment ELF testing are:",
                                     tags$ul(
                                       tags$li("A central estimate of ",
                                               textOutput("pre_treat_elf_all_sum_1_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_elf_all_sum_1_cost", inline = TRUE)
                                       ),
                                       tags$li("A lower estimate of ",
                                               textOutput("pre_treat_elf_all_sum_2_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_elf_all_sum_2_cost", inline = TRUE)
                                       ),
                                       tags$li("An upper estimate of ",
                                               textOutput("pre_treat_elf_all_sum_3_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_elf_all_sum_3_cost", inline = TRUE)
                                       )
                                     ))
                         ),
                         nav_panel("Data Table", DTOutput("pre_treat_elf_all_DT"))
                       )
             ),
             nav_panel("Semaglutide",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the applied assumptions the modelled activity and costs for 
                                     pre-treatment ELF testing are:",
                                     tags$ul(
                                       tags$li("A central estimate of ",
                                               textOutput("pre_treat_elf_sem_sum_1_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_elf_sem_sum_1_cost", inline = TRUE)
                                       ),
                                       tags$li("A lower estimate of ",
                                               textOutput("pre_treat_elf_sem_sum_2_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_elf_sem_sum_2_cost", inline = TRUE)
                                       ),
                                       tags$li("An upper estimate of ",
                                               textOutput("pre_treat_elf_sem_sum_3_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_elf_sem_sum_3_cost", inline = TRUE)
                                       )
                                     ))
                         ),
                         nav_panel("Data Table", DTOutput("pre_treat_elf_sem_DT"))
                       )
             ),
             nav_panel("Survodutide",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the applied assumptions the modelled activity and costs for 
                                     pre-treatment ELF testing are:",
                                     tags$ul(
                                       tags$li("A central estimate of ",
                                               textOutput("pre_treat_elf_surv_sum_1_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_elf_surv_sum_1_cost", inline = TRUE)
                                       ),
                                       tags$li("A lower estimate of ",
                                               textOutput("pre_treat_elf_surv_sum_2_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_elf_surv_sum_2_cost", inline = TRUE)
                                       ),
                                       tags$li("An upper estimate of ",
                                               textOutput("pre_treat_elf_surv_sum_3_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_elf_surv_sum_3_cost", inline = TRUE)
                                       )
                                     ))
                         ),
                         nav_panel("Data Table", DTOutput("pre_treat_elf_surv_DT"))
                       )
             ),
             nav_panel("Resmetirom",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the applied assumptions the modelled activity and costs for 
                                     pre-treatment ELF testing are:",
                                     tags$ul(
                                       tags$li("A central estimate of ",
                                               textOutput("pre_treat_elf_res_sum_1_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_elf_res_sum_1_cost", inline = TRUE)
                                       ),
                                       tags$li("A lower estimate of ",
                                               textOutput("pre_treat_elf_res_sum_2_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_elf_res_sum_2_cost", inline = TRUE)
                                       ),
                                       tags$li("An upper estimate of ",
                                               textOutput("pre_treat_elf_res_sum_3_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_elf_res_sum_3_cost", inline = TRUE)
                                       )
                                     ))
                         ),
                         nav_panel("Data Table", DTOutput("pre_treat_elf_res_DT"))
                       )
             ),
             nav_panel("Lanifibranor",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the applied assumptions the modelled activity and costs for 
                                     pre-treatment ELF testing are:",
                                     tags$ul(
                                       tags$li("A central estimate of ",
                                               textOutput("pre_treat_elf_lan_sum_1_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_elf_lan_sum_1_cost", inline = TRUE)
                                       ),
                                       tags$li("A lower estimate of ",
                                               textOutput("pre_treat_elf_lan_sum_2_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_elf_lan_sum_2_cost", inline = TRUE)
                                       ),
                                       tags$li("An upper estimate of ",
                                               textOutput("pre_treat_elf_lan_sum_3_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_elf_lan_sum_3_cost", inline = TRUE)
                                       )
                                     ))
                         ),
                         nav_panel("Data Table", DTOutput("pre_treat_elf_lan_DT"))
                       )
             )
             
           ),
           br(),
           h4("Biomarkers"),
           p("Some biomarker text"),
           navset_tab(
             nav_panel("All Treatments",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the applied assumptions the modelled activity and costs for 
                                     pre-treatment biomarkers are:",
                                     tags$ul(
                                       tags$li("A central estimate of ",
                                               textOutput("pre_treat_biomarkers_all_sum_1_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_biomarkers_all_sum_1_cost", inline = TRUE)
                                       ),
                                       tags$li("A lower estimate of ",
                                               textOutput("pre_treat_biomarkers_all_sum_2_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_biomarkers_all_sum_2_cost", inline = TRUE)
                                       ),
                                       tags$li("An upper estimate of ",
                                               textOutput("pre_treat_biomarkers_all_sum_3_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_biomarkers_all_sum_3_cost", inline = TRUE)
                                       )
                                     ))
                         ),
                         nav_panel("Data Table", DTOutput("pre_treat_biomarkers_all_DT"))
                       )
             ),
             nav_panel("Semaglutide",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the applied assumptions the modelled activity and costs for 
                                     pre-treatment biomarkers are:",
                                     tags$ul(
                                       tags$li("A central estimate of ",
                                               textOutput("pre_treat_biomarkers_sem_sum_1_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_biomarkers_sem_sum_1_cost", inline = TRUE)
                                       ),
                                       tags$li("A lower estimate of ",
                                               textOutput("pre_treat_biomarkers_sem_sum_2_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_biomarkers_sem_sum_2_cost", inline = TRUE)
                                       ),
                                       tags$li("An upper estimate of ",
                                               textOutput("pre_treat_biomarkers_sem_sum_3_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_biomarkers_sem_sum_3_cost", inline = TRUE)
                                       )
                                     ))
                         ),
                         nav_panel("Data Table", DTOutput("pre_treat_biomarkers_sem_DT"))
                       )
             ),
             nav_panel("Survodutide",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the applied assumptions the modelled activity and costs for 
                                     pre-treatment biomarkers are:",
                                     tags$ul(
                                       tags$li("A central estimate of ",
                                               textOutput("pre_treat_biomarkers_surv_sum_1_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_biomarkers_surv_sum_1_cost", inline = TRUE)
                                       ),
                                       tags$li("A lower estimate of ",
                                               textOutput("pre_treat_biomarkers_surv_sum_2_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_biomarkers_surv_sum_2_cost", inline = TRUE)
                                       ),
                                       tags$li("An upper estimate of ",
                                               textOutput("pre_treat_biomarkers_surv_sum_3_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_biomarkers_surv_sum_3_cost", inline = TRUE)
                                       )
                                     ))
                         ),
                         nav_panel("Data Table", DTOutput("pre_treat_biomarkers_surv_DT"))
                       )
             ),
             nav_panel("Resmetirom",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the applied assumptions the modelled activity and costs for 
                                     pre-treatment biomarkers are:",
                                     tags$ul(
                                       tags$li("A central estimate of ",
                                               textOutput("pre_treat_biomarkers_res_sum_1_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_biomarkers_res_sum_1_cost", inline = TRUE)
                                       ),
                                       tags$li("A lower estimate of ",
                                               textOutput("pre_treat_biomarkers_res_sum_2_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_biomarkers_res_sum_2_cost", inline = TRUE)
                                       ),
                                       tags$li("An upper estimate of ",
                                               textOutput("pre_treat_biomarkers_res_sum_3_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_biomarkers_res_sum_3_cost", inline = TRUE)
                                       )
                                     ))
                         ),
                         nav_panel("Data Table", DTOutput("pre_treat_biomarkers_res_DT"))
                       )
             ),
             nav_panel("Lanifibranor",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the applied assumptions the modelled activity and costs for 
                                     pre-treatment biomarkers are:",
                                     tags$ul(
                                       tags$li("A central estimate of ",
                                               textOutput("pre_treat_biomarkers_lan_sum_1_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_biomarkers_lan_sum_1_cost", inline = TRUE)
                                       ),
                                       tags$li("A lower estimate of ",
                                               textOutput("pre_treat_biomarkers_lan_sum_2_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_biomarkers_lan_sum_2_cost", inline = TRUE)
                                       ),
                                       tags$li("An upper estimate of ",
                                               textOutput("pre_treat_biomarkers_lan_sum_3_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_biomarkers_lan_sum_3_cost", inline = TRUE)
                                       )
                                     ))
                         ),
                         nav_panel("Data Table", DTOutput("pre_treat_biomarkers_lan_DT"))
                       )
             )
           ),
           br(),
           h4("Fibroscans"),
           p("Some Fibroscan text"),
           navset_tab(
             nav_panel("All Treatments",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the applied assumptions the modelled activity and costs for 
                                     pre-treatment fibroscans are:",
                                     tags$ul(
                                       tags$li("A central estimate of ",
                                               textOutput("pre_treat_fibro_all_sum_1_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_fibro_all_sum_1_cost", inline = TRUE)
                                       ),
                                       tags$li("A lower estimate of ",
                                               textOutput("pre_treat_fibro_all_sum_2_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_fibro_all_sum_2_cost", inline = TRUE)
                                       ),
                                       tags$li("An upper estimate of ",
                                               textOutput("pre_treat_fibro_all_sum_3_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_fibro_all_sum_3_cost", inline = TRUE)
                                       )
                                     ))
                         ),
                         nav_panel("Data Table", DTOutput("pre_treat_fibro_all_DT"))
                       )
             ),
             nav_panel("Semaglutide",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the applied assumptions the modelled activity and costs for 
                                     pre-treatment fibroscans are:",
                                     tags$ul(
                                       tags$li("A central estimate of ",
                                               textOutput("pre_treat_fibro_sem_sum_1_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_fibro_sem_sum_1_cost", inline = TRUE)
                                       ),
                                       tags$li("A lower estimate of ",
                                               textOutput("pre_treat_fibro_sem_sum_2_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_fibro_sem_sum_2_cost", inline = TRUE)
                                       ),
                                       tags$li("An upper estimate of ",
                                               textOutput("pre_treat_fibro_sem_sum_3_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_fibro_sem_sum_3_cost", inline = TRUE)
                                       )
                                     ))
                         ),
                         nav_panel("Data Table", DTOutput("pre_treat_fibro_sem_DT"))
                       )
             ),
             nav_panel("Survodutide",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the applied assumptions the modelled activity and costs for 
                                     pre-treatment fibroscans are:",
                                     tags$ul(
                                       tags$li("A central estimate of ",
                                               textOutput("pre_treat_fibro_surv_sum_1_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_fibro_surv_sum_1_cost", inline = TRUE)
                                       ),
                                       tags$li("A lower estimate of ",
                                               textOutput("pre_treat_fibro_surv_sum_2_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_fibro_surv_sum_2_cost", inline = TRUE)
                                       ),
                                       tags$li("An upper estimate of ",
                                               textOutput("pre_treat_fibro_surv_sum_3_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_fibro_surv_sum_3_cost", inline = TRUE)
                                       )
                                     ))
                         ),
                         nav_panel("Data Table", DTOutput("pre_treat_fibro_surv_DT"))
                       )
             ),
             nav_panel("Resmetirom",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the applied assumptions the modelled activity and costs for 
                                     pre-treatment fibroscans are:",
                                     tags$ul(
                                       tags$li("A central estimate of ",
                                               textOutput("pre_treat_fibro_res_sum_1_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_fibro_res_sum_1_cost", inline = TRUE)
                                       ),
                                       tags$li("A lower estimate of ",
                                               textOutput("pre_treat_fibro_res_sum_2_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_fibro_res_sum_2_cost", inline = TRUE)
                                       ),
                                       tags$li("An upper estimate of ",
                                               textOutput("pre_treat_fibro_res_sum_3_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_fibro_res_sum_3_cost", inline = TRUE)
                                       )
                                     ))
                         ),
                         nav_panel("Data Table", DTOutput("pre_treat_fibro_res_DT"))
                       )
             ),
             nav_panel("Lanifibranor",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the applied assumptions the modelled activity and costs for 
                                     pre-treatment fibroscans are:",
                                     tags$ul(
                                       tags$li("A central estimate of ",
                                               textOutput("pre_treat_fibro_lan_sum_1_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_fibro_lan_sum_1_cost", inline = TRUE)
                                       ),
                                       tags$li("A lower estimate of ",
                                               textOutput("pre_treat_fibro_lan_sum_2_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_fibro_lan_sum_2_cost", inline = TRUE)
                                       ),
                                       tags$li("An upper estimate of ",
                                               textOutput("pre_treat_fibro_lan_sum_3_act", inline = TRUE),
                                               " at a cost of ",
                                               textOutput("pre_treat_fibro_lan_sum_3_cost", inline = TRUE)
                                       )
                                     ))
                         ),
                         nav_panel("Data Table", DTOutput("pre_treat_fibro_lan_DT"))
                       )
             )
           ),
           br(),
           h3("Treatment Delivery"),
           hr(),
           p("As each of the treatment pathways are slightly different this is split across different sections to 
             demonstrate the activities and costs associated with each stage of treatment. There is an ", 
             strong("all treatments section"), " that captures all treatment delivery costs between weeks 0 and 72."),
           navset_tab(
             nav_panel("All Treatment",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the assumptions the modelled activity and costs for all treatment 
                                                 delivery between weeks 0 to 71 are:",
                                     tags$ul(
                                       tags$li("A central esimtate of ",
                                               textOutput("total_all_treat_72_sum_1_act", inline = TRUE),
                                               " activities at a cost of ",
                                               textOutput("total_all_treat_72_sum_1_cost", inline = TRUE)
                                       ),
                                       tags$li("A lower esimtate of ",
                                               textOutput("total_all_treat_72_sum_2_act", inline = TRUE),
                                               " activities at a cost of ",
                                               textOutput("total_all_treat_72_sum_2_cost", inline = TRUE)
                                       ),
                                       tags$li("An upper esimtate of ",
                                               textOutput("total_all_treat_72_sum_3_act", inline = TRUE),
                                               " activities at a cost of ",
                                               textOutput("total_all_treat_72_sum_3_cost", inline = TRUE)
                                       )
                                     ))
                                   
                         ),
                         nav_panel("Data Table",
                                   DTOutput("total_all_treat_72_DT")
                         )
                       )
                       
             ),
             nav_panel("Semaglutide",
                       p("Semaglutide treatment is modelled in two phases between weeks 0-16 and then dosage maintenance 
                         until week 71 prior to a continuation decision."),
                       navset_tab(
                         nav_panel("Weeks 0-16",
                                   navset_tab(
                                     nav_panel("Summary",
                                               p("Based on the assumptions the modelled activity and costs for Semaglutide treatment 
                                                 delivery between weeks 0 to 16 are:",
                                                 tags$ul(
                                                   tags$li("A central esimtate of ",
                                                           textOutput("init_treat_sem_016_sum_1_act", inline = TRUE),
                                                           " activities at a cost of ",
                                                           textOutput("init_treat_sem_016_sum_1_cost", inline = TRUE)
                                                           ),
                                                   tags$li("A lower esimtate of ",
                                                           textOutput("init_treat_sem_016_sum_2_act", inline = TRUE),
                                                           " activities at a cost of ",
                                                           textOutput("init_treat_sem_016_sum_2_cost", inline = TRUE)
                                                   ),
                                                   tags$li("An upper esimtate of ",
                                                           textOutput("init_treat_sem_016_sum_3_act", inline = TRUE),
                                                           " activities at a cost of ",
                                                           textOutput("init_treat_sem_016_sum_3_cost", inline = TRUE)
                                                   )
                                                 ))
                                               ),
                                     nav_panel("Data Table",
                                               p("The table below contains the activity and costs to deliver the treatment between weeks 0 and 16:"),
                                               DTOutput("init_treat_sem_DT"))
                                   )

                         ),
                         nav_panel("Weeks 16-71",
                                   navset_tab(
                                     nav_panel("Summary",
                                               p("Based on the assumptions the modelled activity and costs for Semaglutide treatment 
                                                 delivery between weeks 16 to 71 are:",
                                                 tags$ul(
                                                   tags$li("A central esimtate of ",
                                                           textOutput("dm1_treat_sem_sum_1_act", inline = TRUE),
                                                           " activities at a cost of ",
                                                           textOutput("dm1_treat_sem_sum_1_cost", inline = TRUE)
                                                   ),
                                                   tags$li("A lower esimtate of ",
                                                           textOutput("dm1_treat_sem_sum_2_act", inline = TRUE),
                                                           " activities at a cost of ",
                                                           textOutput("dm1_treat_sem_sum_2_cost", inline = TRUE)
                                                   ),
                                                   tags$li("An upper esimtate of ",
                                                           textOutput("dm1_treat_sem_sum_3_act", inline = TRUE),
                                                           " activities at a cost of ",
                                                           textOutput("dm1_treat_sem_sum_3_cost", inline = TRUE)
                                                   )
                                                 ))
                                               ),
                                     nav_panel("Data Table",
                                               p("The table below contains the activity and costs to deliver the treatment between weeks 16 and 71:"),
                                               DTOutput("dm1_treat_sem_DT"))
                                   )
                         ),
                         nav_panel("Combined Treatment to Week 72",
                                   navset_tab(
                                     nav_panel("Summary",
                                               p("Based on the assumptions the modelled activity and costs for Semaglutide treatment 
                                                 delivery between weeks 0 to 71 are:",
                                                 tags$ul(
                                                   tags$li("A central esimtate of ",
                                                           textOutput("total_sem_treat_72_sum_1_act", inline = TRUE),
                                                           " activities at a cost of ",
                                                           textOutput("total_sem_treat_72_sum_1_cost", inline = TRUE)
                                                   ),
                                                   tags$li("A lower esimtate of ",
                                                           textOutput("total_sem_treat_72_sum_2_act", inline = TRUE),
                                                           " activities at a cost of ",
                                                           textOutput("total_sem_treat_72_sum_2_cost", inline = TRUE)
                                                   ),
                                                   tags$li("An upper esimtate of ",
                                                           textOutput("total_sem_treat_72_sum_3_act", inline = TRUE),
                                                           " activities at a cost of ",
                                                           textOutput("total_sem_treat_72_sum_3_cost", inline = TRUE)
                                                   )
                                                 ))
                                               ),
                                     nav_panel("Data Table",
                                               DTOutput("total_sem_treat_72_DT"))
                                   ))
                                  
                       )
             ),
             nav_panel("Survodutide",
                       p("Survodutide treatment is modelled in two phases between weeks 0-24 and then dosage maintenance 
                         until week 71 prior to a continuation decision."),
                       navset_tab(
                         nav_panel("Weeks 0-24",
                                   navset_tab(
                                     nav_panel("Summary",
                                               p("Based on the assumptions the modelled activity and costs for Survodutide treatment 
                                                 delivery between weeks 0 to 24 are:",
                                                 tags$ul(
                                                   tags$li("A central esimtate of ",
                                                           textOutput("init_treat_surv_024_sum_1_act", inline = TRUE),
                                                           " activities at a cost of ",
                                                           textOutput("init_treat_surv_024_sum_1_cost", inline = TRUE)
                                                   ),
                                                   tags$li("A lower esimtate of ",
                                                           textOutput("init_treat_surv_024_sum_2_act", inline = TRUE),
                                                           " activities at a cost of ",
                                                           textOutput("init_treat_surv_024_sum_2_cost", inline = TRUE)
                                                   ),
                                                   tags$li("An upper esimtate of ",
                                                           textOutput("init_treat_surv_024_sum_3_act", inline = TRUE),
                                                           " activities at a cost of ",
                                                           textOutput("init_treat_surv_024_sum_3_cost", inline = TRUE)
                                                   )
                                                 ))
                                               ),
                                     nav_panel("Data Table",
                                               p("The table below contains the activity and costs to deliver the treatment between weeks 0 and 24:"),
                                               DTOutput("init_treat_surv_DT")
                                               )
                                   )
                                   
                       ),
                       nav_panel("Weeks 24-71",
                                 navset_tab(
                                   nav_panel("Summary",
                                             p("Based on the assumptions the modelled activity and costs for Survodutide treatment 
                                                 delivery between weeks 24 to 71 are:",
                                               tags$ul(
                                                 tags$li("A central esimtate of ",
                                                         textOutput("dm1_treat_surv_sum_1_act", inline = TRUE),
                                                         " activities at a cost of ",
                                                         textOutput("dm1_treat_surv_sum_1_cost", inline = TRUE)
                                                 ),
                                                 tags$li("A lower esimtate of ",
                                                         textOutput("dm1_treat_surv_sum_2_act", inline = TRUE),
                                                         " activities at a cost of ",
                                                         textOutput("dm1_treat_surv_sum_2_cost", inline = TRUE)
                                                 ),
                                                 tags$li("An upper esimtate of ",
                                                         textOutput("dm1_treat_surv_sum_3_act", inline = TRUE),
                                                         " activities at a cost of ",
                                                         textOutput("dm1_treat_surv_sum_3_cost", inline = TRUE)
                                                 )
                                               ))
                                   ),
                                   nav_panel("Data Table",
                                             p("The table below contains the activity and costs to deliver the treatment between weeks 24 and 71:"),
                                             DTOutput("dm1_treat_surv_DT")
                                   )
                                 )
                                 
                       ),
                       nav_panel("Combined Treatment to Week 72",
                                 navset_tab(
                                   nav_panel("Summary",
                                             p("Based on the assumptions the modelled activity and costs for Survodutide treatment 
                                                 delivery between weeks 0 to 72 are:",
                                               tags$ul(
                                                 tags$li("A central esimtate of ",
                                                         textOutput("total_surv_treat_72_sum_1_act", inline = TRUE),
                                                         " activities at a cost of ",
                                                         textOutput("total_surv_treat_72_sum_1_cost", inline = TRUE)
                                                 ),
                                                 tags$li("A lower esimtate of ",
                                                         textOutput("total_surv_treat_72_sum_2_act", inline = TRUE),
                                                         " activities at a cost of ",
                                                         textOutput("total_surv_treat_72_sum_2_cost", inline = TRUE)
                                                 ),
                                                 tags$li("An upper esimtate of ",
                                                         textOutput("total_surv_treat_72_sum_3_act", inline = TRUE),
                                                         " activities at a cost of ",
                                                         textOutput("total_surv_treat_72_sum_3_cost", inline = TRUE)
                                                 )
                                               ))
                                   ),
                                   nav_panel("Data Table",
                                             p("The table below contains the activity and costs to deliver the treatment between weeks 0 and 72:"),
                                             DTOutput("total_surv_treat_72_DT")
                                   )
                                 )
                                 
                       )
                       )
               
             ),
             nav_panel("Resmetirom",
                       p("Resmetirom treatment is modelled in two phases between weeks 0-52 prior to an efficacy decision and then 
                       weeks 52-71 before a second continuation decision."),
                       navset_tab(
                         nav_panel("Weeks 0-52",
                                   navset_tab(
                                     nav_panel("Summary",
                                               p("Based on the assumptions the modelled activity and costs for Resmetirom treatment 
                                                 delivery between weeks 0 to 52 are:",
                                                 tags$ul(
                                                   tags$li("A central esimtate of ",
                                                           textOutput("init_treat_res_052_sum_1_act", inline = TRUE),
                                                           " activities at a cost of ",
                                                           textOutput("init_treat_res_052_sum_1_cost", inline = TRUE)
                                                   ),
                                                   tags$li("A lower esimtate of ",
                                                           textOutput("init_treat_res_052_sum_2_act", inline = TRUE),
                                                           " activities at a cost of ",
                                                           textOutput("init_treat_res_052_sum_2_cost", inline = TRUE)
                                                   ),
                                                   tags$li("An upper esimtate of ",
                                                           textOutput("init_treat_res_052_sum_3_act", inline = TRUE),
                                                           " activities at a cost of ",
                                                           textOutput("init_treat_res_052_sum_3_cost", inline = TRUE)
                                                   )
                                                 ))
                                     ),
                                     nav_panel("Data Table",
                                               p("The table below contains the activity and costs to deliver the treatment between weeks 0 and 52:"),
                                               DTOutput("init_treat_res_DT"))
                                   )
                                   
                         ),
                         nav_panel("Weeks 52-71",
                                   navset_tab(
                                     nav_panel("Summary",
                                               p("Based on the assumptions the modelled activity and costs for Resmetirom treatment 
                                                 delivery between weeks 52 to 71 are:",
                                                 tags$ul(
                                                   tags$li("A central esimtate of ",
                                                           textOutput("dm1_treat_res_sum_1_act", inline = TRUE),
                                                           " activities at a cost of ",
                                                           textOutput("dm1_treat_res_sum_1_cost", inline = TRUE)
                                                   ),
                                                   tags$li("A lower esimtate of ",
                                                           textOutput("dm1_treat_res_sum_2_act", inline = TRUE),
                                                           " activities at a cost of ",
                                                           textOutput("dm1_treat_res_sum_2_cost", inline = TRUE)
                                                   ),
                                                   tags$li("An upper esimtate of ",
                                                           textOutput("dm1_treat_res_sum_3_act", inline = TRUE),
                                                           " activities at a cost of ",
                                                           textOutput("dm1_treat_res_sum_3_cost", inline = TRUE)
                                                   )
                                                 ))
                                     ),
                                     nav_panel("Data Table",
                                               p("The table below contains the activity and costs to deliver the treatment between weeks 52 and 71:"),
                                               DTOutput("dm1_treat_res_DT"))
                                   )
                         ),
                         nav_panel("Combined Treatment to Week 72",
                                   navset_tab(
                                     nav_panel("Summary",
                                               p("Based on the assumptions the modelled activity and costs for Resmetirom treatment 
                                                 delivery between weeks 0 to 71 are:",
                                                 tags$ul(
                                                   tags$li("A central esimtate of ",
                                                           textOutput("total_res_treat_72_sum_1_act", inline = TRUE),
                                                           " activities at a cost of ",
                                                           textOutput("total_res_treat_72_sum_1_cost", inline = TRUE)
                                                   ),
                                                   tags$li("A lower esimtate of ",
                                                           textOutput("total_res_treat_72_sum_2_act", inline = TRUE),
                                                           " activities at a cost of ",
                                                           textOutput("total_res_treat_72_sum_2_cost", inline = TRUE)
                                                   ),
                                                   tags$li("An upper esimtate of ",
                                                           textOutput("total_res_treat_72_sum_3_act", inline = TRUE),
                                                           " activities at a cost of ",
                                                           textOutput("total_res_treat_72_sum_3_cost", inline = TRUE)
                                                   )
                                                 ))
                                     ),
                                     nav_panel("Data Table",
                                               DTOutput("total_res_treat_72_DT"))
                                   ))
                         
                       )
             ),
             nav_panel("Lanifibranor",
                       p("Lanifibranor treatment is modelled in two phases between weeks 0-52 prior to an efficacy decision and then 
                       weeks 52-71 before a second continuation decision."),
                       navset_tab(
                         nav_panel("Weeks 0-52",
                                   navset_tab(
                                     nav_panel("Summary",
                                               p("Based on the assumptions the modelled activity and costs for Lanifibranor treatment 
                                                 delivery between weeks 0 to 52 are:",
                                                 tags$ul(
                                                   tags$li("A central esimtate of ",
                                                           textOutput("init_treat_lan_052_sum_1_act", inline = TRUE),
                                                           " activities at a cost of ",
                                                           textOutput("init_treat_lan_052_sum_1_cost", inline = TRUE)
                                                   ),
                                                   tags$li("A lower esimtate of ",
                                                           textOutput("init_treat_lan_052_sum_2_act", inline = TRUE),
                                                           " activities at a cost of ",
                                                           textOutput("init_treat_lan_052_sum_2_cost", inline = TRUE)
                                                   ),
                                                   tags$li("An upper esimtate of ",
                                                           textOutput("init_treat_lan_052_sum_3_act", inline = TRUE),
                                                           " activities at a cost of ",
                                                           textOutput("init_treat_lan_052_sum_3_cost", inline = TRUE)
                                                   )
                                                 ))
                                     ),
                                     nav_panel("Data Table",
                                               p("The table below contains the activity and costs to deliver the treatment between weeks 0 and 52:"),
                                               DTOutput("init_treat_lan_DT"))
                                   )
                                   
                         ),
                         nav_panel("Weeks 52-71",
                                   navset_tab(
                                     nav_panel("Summary",
                                               p("Based on the assumptions the modelled activity and costs for Lanifibranor treatment 
                                                 delivery between weeks 52 to 71 are:",
                                                 tags$ul(
                                                   tags$li("A central esimtate of ",
                                                           textOutput("dm1_treat_lan_sum_1_act", inline = TRUE),
                                                           " activities at a cost of ",
                                                           textOutput("dm1_treat_lan_sum_1_cost", inline = TRUE)
                                                   ),
                                                   tags$li("A lower esimtate of ",
                                                           textOutput("dm1_treat_lan_sum_2_act", inline = TRUE),
                                                           " activities at a cost of ",
                                                           textOutput("dm1_treat_lan_sum_2_cost", inline = TRUE)
                                                   ),
                                                   tags$li("An upper esimtate of ",
                                                           textOutput("dm1_treat_lan_sum_3_act", inline = TRUE),
                                                           " activities at a cost of ",
                                                           textOutput("dm1_treat_lan_sum_3_cost", inline = TRUE)
                                                   )
                                                 ))
                                     ),
                                     nav_panel("Data Table",
                                               p("The table below contains the activity and costs to deliver the treatment between weeks 52 and 71:"),
                                               DTOutput("dm1_treat_lan_DT"))
                                   )
                         ),
                         nav_panel("Combined Treatment to Week 72",
                                   navset_tab(
                                     nav_panel("Summary",
                                               p("Based on the assumptions the modelled activity and costs for Lanifibranor treatment 
                                                 delivery between weeks 0 to 71 are:",
                                                 tags$ul(
                                                   tags$li("A central esimtate of ",
                                                           textOutput("total_lan_treat_72_sum_1_act", inline = TRUE),
                                                           " activities at a cost of ",
                                                           textOutput("total_lan_treat_72_sum_1_cost", inline = TRUE)
                                                   ),
                                                   tags$li("A lower esimtate of ",
                                                           textOutput("total_lan_treat_72_sum_2_act", inline = TRUE),
                                                           " activities at a cost of ",
                                                           textOutput("total_lan_treat_72_sum_2_cost", inline = TRUE)
                                                   ),
                                                   tags$li("An upper esimtate of ",
                                                           textOutput("total_lan_treat_72_sum_3_act", inline = TRUE),
                                                           " activities at a cost of ",
                                                           textOutput("total_lan_treat_72_sum_3_cost", inline = TRUE)
                                                   )
                                                 ))
                                     ),
                                     nav_panel("Data Table",
                                               DTOutput("total_lan_treat_72_DT"))
                                   ))
                         
                       )
             )
             
           ),
           br(),
           h3("Diagnostic Monitoring During Treatment"),
           hr(),
           navset_tab(
             nav_panel("All Treatments",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the applied assumptions the modelled activity and costs for the monitoring diagnostics 
                                     are:",
                                     tags$ul(
                                       tags$li(
                                         "A central estimate of ",
                                         textOutput("diag_mon_all_sum_1_act", inline = TRUE),
                                         " monitoring tests at a cost of ",
                                         textOutput("diag_mon_all_sum_1_cost", inline = TRUE)
                                       ),
                                       tags$li(
                                         "An upper estimate of ",
                                         textOutput("diag_mon_all_sum_2_act", inline = TRUE),
                                         " monitoring tests at a cost of ",
                                         textOutput("diag_mon_all_sum_2_cost", inline = TRUE)
                                       ),
                                       tags$li("A lower estimate of ",
                                               textOutput("diag_mon_all_sum_3_act", inline = TRUE),
                                               " monitoring tests at a cost of ",
                                               textOutput("diag_mon_all_sum_3_cost", inline = TRUE)
                                       )
                                     )
                                   )
                                   ),
                         nav_panel("Data Table",
                                   DTOutput("diag_mon_all_DT")
                                   )
                       )
                       ),
             nav_panel("Semaglutide",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the applied assumptions the modelled activity and costs for the monitoring diagnostics 
                                     are:",
                                     tags$ul(
                                       tags$li(
                                         "A central estimate of ",
                                         textOutput("diag_mon_sem_sum_1_act", inline = TRUE),
                                         " monitoring tests at a cost of ",
                                         textOutput("diag_mon_sem_sum_1_cost", inline = TRUE)
                                       ),
                                       tags$li(
                                         "An upper estimate of ",
                                         textOutput("diag_mon_sem_sum_2_act", inline = TRUE),
                                         " monitoring tests at a cost of ",
                                         textOutput("diag_mon_sem_sum_2_cost", inline = TRUE)
                                       ),
                                       tags$li("A lower estimate of ",
                                         textOutput("diag_mon_sem_sum_3_act", inline = TRUE),
                                         " monitoring tests at a cost of ",
                                         textOutput("diag_mon_sem_sum_3_cost", inline = TRUE)
                                       )
                                       )
                                     )
                           
                         ),
                         nav_panel("Data Table",
                                   DTOutput("diag_mon_sem_DT")
                           
                         )
                       )
             ),
             nav_panel("Survodutide",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the applied assumptions the modelled activity and costs for the monitoring diagnostics 
                                     are:",
                                     tags$ul(
                                       tags$li(
                                         "A central estimate of ",
                                         textOutput("diag_mon_surv_sum_1_act", inline = TRUE),
                                         " monitoring tests at a cost of ",
                                         textOutput("diag_mon_surv_sum_1_cost", inline = TRUE)
                                       ),
                                       tags$li(
                                         "An upper estimate of ",
                                         textOutput("diag_mon_surv_sum_2_act", inline = TRUE),
                                         " monitoring tests at a cost of ",
                                         textOutput("diag_mon_surv_sum_2_cost", inline = TRUE)
                                       ),
                                       tags$li("A lower estimate of ",
                                               textOutput("diag_mon_surv_sum_3_act", inline = TRUE),
                                               " monitoring tests at a cost of ",
                                               textOutput("diag_mon_surv_sum_3_cost", inline = TRUE)
                                       )
                                     )
                                   )
                                   
                         ),
                         nav_panel("Data Table",
                                   DTOutput("diag_mon_surv_DT")
                                   
                         )
                       )
             ),
             nav_panel("Resmetirom",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the applied assumptions the modelled activity and costs for the monitoring diagnostics 
                                     are:",
                                     tags$ul(
                                       tags$li(
                                         "A central estimate of ",
                                         textOutput("diag_mon_res_sum_1_act", inline = TRUE),
                                         " monitoring tests at a cost of ",
                                         textOutput("diag_mon_res_sum_1_cost", inline = TRUE)
                                       ),
                                       tags$li(
                                         "An upper estimate of ",
                                         textOutput("diag_mon_res_sum_2_act", inline = TRUE),
                                         " monitoring tests at a cost of ",
                                         textOutput("diag_mon_res_sum_2_cost", inline = TRUE)
                                       ),
                                       tags$li("A lower estimate of ",
                                               textOutput("diag_mon_res_sum_3_act", inline = TRUE),
                                               " monitoring tests at a cost of ",
                                               textOutput("diag_mon_res_sum_3_cost", inline = TRUE)
                                       )
                                     )
                                   )
                                   
                         ),
                         nav_panel("Data Table",
                                   DTOutput("diag_mon_res_DT")
                                   
                         )
                       )
             ),
             nav_panel("Lanifibranor",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the applied assumptions the modelled activity and costs for the monitoring diagnostics 
                                     are:",
                                     tags$ul(
                                       tags$li(
                                         "A central estimate of ",
                                         textOutput("diag_mon_lan_sum_1_act", inline = TRUE),
                                         " monitoring tests at a cost of ",
                                         textOutput("diag_mon_lan_sum_1_cost", inline = TRUE)
                                       ),
                                       tags$li(
                                         "An upper estimate of ",
                                         textOutput("diag_mon_lan_sum_2_act", inline = TRUE),
                                         " monitoring tests at a cost of ",
                                         textOutput("diag_mon_lan_sum_2_cost", inline = TRUE)
                                       ),
                                       tags$li("A lower estimate of ",
                                               textOutput("diag_mon_lan_sum_3_act", inline = TRUE),
                                               " monitoring tests at a cost of ",
                                               textOutput("diag_mon_lan_sum_3_cost", inline = TRUE)
                                       )
                                     )
                                   )
                                   
                         ),
                         nav_panel("Data Table",
                                   DTOutput("diag_mon_lan_DT")
                                   
                         )
                       )
             )
           ),
           br(),
           h3("Continuation Decision"),
           hr(),
           h4("Continuation Diagnostics"),
           navset_tab(
             nav_panel("All Treatments",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the assumptions in the modelled activity and costs for the diagnostics 
                                     required at the week 52 and 72 continuation decisions are:",
                                     tags$ul(
                                       tags$li("A central estimate will result in ",
                                               textOutput("cont_dec_diag_all_sum_1_act", inline = TRUE),
                                               "diagnostic activities at a cost of ",
                                               textOutput("cont_dec_diag_all_sum_1_cost", inline = TRUE)
                                       ),
                                       tags$li("A lower estimate will result in ",
                                               textOutput("cont_dec_diag_all_sum_2_act", inline = TRUE),
                                               "diagnostic activities at a cost of ",
                                               textOutput("cont_dec_diag_all_sum_2_cost", inline = TRUE)
                                       ),
                                       tags$li("An upper estimate will result in ",
                                               textOutput("cont_dec_diag_all_sum_3_act", inline = TRUE),
                                               "diagnostic activities at a cost of ",
                                               textOutput("cont_dec_diag_all_sum_3_cost", inline = TRUE)
                                       )
                                     ))
                                   ),
                         nav_panel("Data Table",
                                   DTOutput("cont_dec_diag_all_DT"))
                       )
                       ),
             nav_panel("Semaglutide",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the assumptions in the modelled activity and costs for the diagnostics 
                                     required at the week 72 continuation decision are:",
                                     tags$ul(
                                       tags$li("A central estimate will result in ",
                                               textOutput("cont_dec_diag_sem_sum_1_pop", inline = TRUE), "patients reach undergoing diagnostics. 
                                               This equates to ",
                                               textOutput("cont_dec_diag_sem_sum_1_act", inline = TRUE),
                                               "diagnostic activities at a cost of ",
                                               textOutput("cont_dec_diag_sem_sum_1_cost", inline = TRUE)
                                               ),
                                       tags$li("A lower estimate will result in ",
                                               textOutput("cont_dec_diag_sem_sum_2_pop", inline = TRUE), "patients reach undergoing diagnostics. 
                                               This equates to ",
                                               textOutput("cont_dec_diag_sem_sum_2_act", inline = TRUE),
                                               "diagnostic activities at a cost of ",
                                               textOutput("cont_dec_diag_sem_sum_2_cost", inline = TRUE)
                                               ),
                                       tags$li("An upper estimate will result in ",
                                               textOutput("cont_dec_diag_sem_sum_3_pop", inline = TRUE), "patients reach undergoing diagnostics. 
                                               This equates to ",
                                               textOutput("cont_dec_diag_sem_sum_3_act", inline = TRUE),
                                               "diagnostic activities at a cost of ",
                                               textOutput("cont_dec_diag_sem_sum_3_cost", inline = TRUE)
                                               )
                                     ))
                                   ),
                         nav_panel("Data Table",
                                   DTOutput("cont_dec_diag_sem_DT"))
                       )
             ),
             nav_panel("Survodutide",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the assumptions in the modelled activity and costs for the diagnostics 
                                     required at the week 72 continuation decision are:",
                                     tags$ul(
                                       tags$li("A central estimate will result in ",
                                               textOutput("cont_dec_diag_surv_sum_1_pop", inline = TRUE), "patients reach undergoing diagnostics. 
                                               This equates to ",
                                               textOutput("cont_dec_diag_surv_sum_1_act", inline = TRUE),
                                               "diagnostic activities at a cost of ",
                                               textOutput("cont_dec_diag_surv_sum_1_cost", inline = TRUE)
                                       ),
                                       tags$li("A lower estimate will result in ",
                                               textOutput("cont_dec_diag_surv_sum_2_pop", inline = TRUE), "patients reach undergoing diagnostics. 
                                               This equates to ",
                                               textOutput("cont_dec_diag_surv_sum_2_act", inline = TRUE),
                                               "diagnostic activities at a cost of ",
                                               textOutput("cont_dec_diag_surv_sum_2_cost", inline = TRUE)
                                       ),
                                       tags$li("An upper estimate will result in ",
                                               textOutput("cont_dec_diag_surv_sum_3_pop", inline = TRUE), "patients reach undergoing diagnostics. 
                                               This equates to ",
                                               textOutput("cont_dec_diag_surv_sum_3_act", inline = TRUE),
                                               "diagnostic activities at a cost of ",
                                               textOutput("cont_dec_diag_surv_sum_3_cost", inline = TRUE)
                                       )
                                     ))
                                   ),
                         nav_panel("Data Table",
                                   DTOutput("cont_dec_diag_surv_DT"))
                       )
             ),
             nav_panel("Resmetirom",
                       navset_tab(
                         nav_panel("Week 52 Efficacy Assessment",
                                   navset_tab(
                                     nav_panel("Summary",
                                               p("Based on the assumptions in the modelled activity and costs for the diagnostics 
                                         required at the week 52 continuation decision are:",
                                                 tags$ul(
                                                   tags$li("A central estimate will result in ",
                                                           textOutput("cont_dec_diag_res_52_sum_1_pop", inline = TRUE), "patients reach undergoing diagnostics. 
                                                   This equates to ",
                                                           textOutput("cont_dec_diag_res_52_sum_1_act", inline = TRUE),
                                                           "diagnostic activities at a cost of ",
                                                           textOutput("cont_dec_diag_res_52_sum_1_cost", inline = TRUE)
                                                   ),
                                                   tags$li("A lower estimate will result in ",
                                                           textOutput("cont_dec_diag_res_52_sum_2_pop", inline = TRUE), "patients reach undergoing diagnostics. 
                                                   This equates to ",
                                                           textOutput("cont_dec_diag_res_52_sum_2_act", inline = TRUE),
                                                           "diagnostic activities at a cost of ",
                                                           textOutput("cont_dec_diag_res_52_sum_2_cost", inline = TRUE)
                                                   ),
                                                   tags$li("An upper estimate will result in ",
                                                           textOutput("cont_dec_diag_res_52_sum_3_pop", inline = TRUE), "patients reach undergoing diagnostics. 
                                                   This equates to ",
                                                           textOutput("cont_dec_diag_res_52_sum_3_act", inline = TRUE),
                                                           "diagnostic activities at a cost of ",
                                                           textOutput("cont_dec_diag_res_52_sum_3_cost", inline = TRUE)
                                                   )
                                                 ))
                                     ),
                                     nav_panel("Data Table",
                                               DTOutput("cont_dec_diag_res_52_DT")
                                     )
                                   )
                         ),
                         nav_panel("Week 72 Efficacy Assessment",
                                   navset_tab(
                                     nav_panel("Summary",
                                               p("Based on the assumptions in the modelled activity and costs for the diagnostics 
                                         required at the week 72 continuation decision are:",
                                                 tags$ul(
                                                   tags$li("A central estimate will result in ",
                                                           textOutput("cont_dec_diag_res_sum_1_pop", inline = TRUE), "patients reach undergoing diagnostics. 
                                                   This equates to ",
                                                           textOutput("cont_dec_diag_res_sum_1_act", inline = TRUE),
                                                           "diagnostic activities at a cost of ",
                                                           textOutput("cont_dec_diag_res_sum_1_cost", inline = TRUE)
                                                   ),
                                                   tags$li("A lower estimate will result in ",
                                                           textOutput("cont_dec_diag_res_sum_2_pop", inline = TRUE), "patients reach undergoing diagnostics. 
                                                   This equates to ",
                                                           textOutput("cont_dec_diag_res_sum_2_act", inline = TRUE),
                                                           "diagnostic activities at a cost of ",
                                                           textOutput("cont_dec_diag_res_sum_2_cost", inline = TRUE)
                                                   ),
                                                   tags$li("An upper estimate will result in ",
                                                           textOutput("cont_dec_diag_res_sum_3_pop", inline = TRUE), "patients reach undergoing diagnostics. 
                                                   This equates to ",
                                                           textOutput("cont_dec_diag_res_sum_3_act", inline = TRUE),
                                                           "diagnostic activities at a cost of ",
                                                           textOutput("cont_dec_diag_res_sum_3_cost", inline = TRUE)
                                                   )
                                                 ))
                                     ),
                                     nav_panel("Data Table",
                                               DTOutput("cont_dec_diag_res_DT")
                                     )
                                   )
                         )
                       )
             ),
             nav_panel("Lanifibranor",
                       navset_tab(
                         nav_panel("Week 52 Efficacy Assessment",
                                   navset_tab(
                                     nav_panel("Summary",
                                               p("Based on the assumptions in the modelled activity and costs for the diagnostics 
                                         required at the week 52 continuation decision are:",
                                                 tags$ul(
                                                   tags$li("A central estimate will result in ",
                                                           textOutput("cont_dec_diag_lan_52_sum_1_pop", inline = TRUE), "patients reach undergoing diagnostics. 
                                                   This equates to ",
                                                           textOutput("cont_dec_diag_lan_52_sum_1_act", inline = TRUE),
                                                           "diagnostic activities at a cost of ",
                                                           textOutput("cont_dec_diag_lan_52_sum_1_cost", inline = TRUE)
                                                   ),
                                                   tags$li("A lower estimate will result in ",
                                                           textOutput("cont_dec_diag_lan_52_sum_2_pop", inline = TRUE), "patients reach undergoing diagnostics. 
                                                   This equates to ",
                                                           textOutput("cont_dec_diag_lan_52_sum_2_act", inline = TRUE),
                                                           "diagnostic activities at a cost of ",
                                                           textOutput("cont_dec_diag_lan_52_sum_2_cost", inline = TRUE)
                                                   ),
                                                   tags$li("An upper estimate will result in ",
                                                           textOutput("cont_dec_diag_lan_52_sum_3_pop", inline = TRUE), "patients reach undergoing diagnostics. 
                                                   This equates to ",
                                                           textOutput("cont_dec_diag_lan_52_sum_3_act", inline = TRUE),
                                                           "diagnostic activities at a cost of ",
                                                           textOutput("cont_dec_diag_lan_52_sum_3_cost", inline = TRUE)
                                                   )
                                                 ))
                                     ),
                                     nav_panel("Data Table",
                                               DTOutput("cont_dec_diag_lan_52_DT")
                                     )
                                   )
                         ),
                         nav_panel("Week 72 Efficacy Assessment",
                                   navset_tab(
                                     nav_panel("Summary",
                                               p("Based on the assumptions in the modelled activity and costs for the diagnostics 
                                         required at the week 72 continuation decision are:",
                                                 tags$ul(
                                                   tags$li("A central estimate will result in ",
                                                           textOutput("cont_dec_diag_lan_sum_1_pop", inline = TRUE), "patients reach undergoing diagnostics. 
                                                   This equates to ",
                                                           textOutput("cont_dec_diag_lan_sum_1_act", inline = TRUE),
                                                           "diagnostic activities at a cost of ",
                                                           textOutput("cont_dec_diag_lan_sum_1_cost", inline = TRUE)
                                                   ),
                                                   tags$li("A lower estimate will result in ",
                                                           textOutput("cont_dec_diag_lan_sum_2_pop", inline = TRUE), "patients reach undergoing diagnostics. 
                                                   This equates to ",
                                                           textOutput("cont_dec_diag_lan_sum_2_act", inline = TRUE),
                                                           "diagnostic activities at a cost of ",
                                                           textOutput("cont_dec_diag_lan_sum_2_cost", inline = TRUE)
                                                   ),
                                                   tags$li("An upper estimate will result in ",
                                                           textOutput("cont_dec_diag_lan_sum_3_pop", inline = TRUE), "patients reach undergoing diagnostics. 
                                                   This equates to ",
                                                           textOutput("cont_dec_diag_lan_sum_3_act", inline = TRUE),
                                                           "diagnostic activities at a cost of ",
                                                           textOutput("cont_dec_diag_lan_sum_3_cost", inline = TRUE)
                                                   )
                                                 ))
                                     ),
                                     nav_panel("Data Table",
                                               DTOutput("cont_dec_diag_lan_DT")
                                     )
                                   )
                         )
                       )
             )
           ),

        
           br(),
           h4("Continuation Appointment"),
           navset_tab(
             nav_panel("All Treatments",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the assumptions in the model the required acitvity and costs assocaited with 
                                     the continuation decisions are:",
                                     tags$ul(
                                       tags$li("A central estimate will result in ",
                                               textOutput("cont_dec_appt_all_sum_1_act", inline = TRUE),
                                               " continuation appointments. The associated costs of these appointments will be ",
                                               textOutput("cont_dec_appt_all_sum_1_cost", inline = TRUE)
                                               
                                       ),
                                       tags$li("A lower estimate will result in ",
                                               textOutput("cont_dec_appt_all_sum_2_act", inline = TRUE),
                                               " continuation appointments. The associated costs of these appointments will be ",
                                               textOutput("cont_dec_appt_all_sum_2_cost", inline = TRUE)
                                       ),
                                       tags$li("An upper estimate will result in ",
                                               textOutput("cont_dec_appt_all_sum_3_act", inline = TRUE),
                                               " continuation appointments. The associated costs of these appointments will be ",
                                               textOutput("cont_dec_appt_all_sum_3_cost", inline = TRUE)
                                       )
                                   
                                   ))
                         ),
                         nav_panel("Data Table",
                                   DTOutput("cont_dec_appt_all_DT"))
                       )
             ),
             nav_panel("Semaglutide",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the assumptions in the model the required acitvity and costs assocaited with 
                                     the continuation decision at 72 weeks are:",
                                     tags$ul(
                                       tags$li("A central estimate will result in ",
                                               textOutput("cont_dec_appt_sem_sum_1_pop", inline = TRUE),
                                               " patients attending for a continuation appointment. Of these ",
                                               textOutput("cont_dec_appt_sem_sum_1_pop_end", inline = TRUE),
                                               " will continue with treatment. The associated costs of these appointments 
                                               will be ",
                                               textOutput("cont_dec_appt_sem_sum_1_cost", inline = TRUE)
                                         
                                       ),
                                       tags$li("An upper estimate will result in ",
                                               textOutput("cont_dec_appt_sem_sum_2_pop", inline = TRUE),
                                               " patients attending for a continuation appointment. Of these ",
                                               textOutput("cont_dec_appt_sem_sum_2_pop_end", inline = TRUE),
                                               " will continue with treatment. The associated costs of these appointments 
                                               will be ",
                                               textOutput("cont_dec_appt_sem_sum_2_cost", inline = TRUE)
                                               
                                       ),
                                       tags$li("A lower estimate will result in ",
                                               textOutput("cont_dec_appt_sem_sum_3_pop", inline = TRUE),
                                               " patients attending for a continuation appointment. Of these ",
                                               textOutput("cont_dec_appt_sem_sum_3_pop_end", inline = TRUE),
                                               " will continue with treatment. The associated costs of these appointments 
                                               will be ",
                                               textOutput("cont_dec_appt_sem_sum_3_cost", inline = TRUE)
                                               
                                       )
                                     )
                                   ) 
                        ),
                         nav_panel("Data Table",
                                   DTOutput("cont_dec_appt_sem_DT")
                         )
                       )
             ),
             nav_panel("Survodutide",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the assumptions in the model the required acitvity and costs assocaited with 
                                     the continuation decision at 72 weeks are:",
                                     tags$ul(
                                       tags$li("A central estimate will result in ",
                                               textOutput("cont_dec_appt_surv_sum_1_pop", inline = TRUE),
                                               " patients attending for a continuation appointment. Of these ",
                                               textOutput("cont_dec_appt_surv_sum_1_pop_end", inline = TRUE),
                                               " will continue with treatment. The associated costs of these appointments 
                                               will be ",
                                               textOutput("cont_dec_appt_surv_sum_1_cost", inline = TRUE)
                                               
                                       ),
                                       tags$li("An upper estimate will result in ",
                                               textOutput("cont_dec_appt_surv_sum_2_pop", inline = TRUE),
                                               " patients attending for a continuation appointment. Of these ",
                                               textOutput("cont_dec_appt_surv_sum_2_pop_end", inline = TRUE),
                                               " will continue with treatment. The associated costs of these appointments 
                                               will be ",
                                               textOutput("cont_dec_appt_surv_sum_2_cost", inline = TRUE)
                                               
                                       ),
                                       tags$li("A lower estimate will result in ",
                                               textOutput("cont_dec_appt_surv_sum_3_pop", inline = TRUE),
                                               " patients attending for a continuation appointment. Of these ",
                                               textOutput("cont_dec_appt_surv_sum_3_pop_end", inline = TRUE),
                                               " will continue with treatment. The associated costs of these appointments 
                                               will be ",
                                               textOutput("cont_dec_appt_surv_sum_3_cost", inline = TRUE)
                                               
                                       )
                                     )
                                   ) 
                         ),
                         nav_panel("Data Table",
                                   DTOutput("cont_dec_appt_surv_DT")
                         )
                       )
             ),
             nav_panel("Resmetirom",
                       navset_tab(
                         nav_panel("Week 52",
                                  navset_tab(
                                    nav_panel("Summary",
                                              p("Based on the assumptions in the model the required acitvity and costs assocaited with 
                                                the continuation decision at 52 weeks are:",
                                                tags$ul(
                                                  tags$li("A central estimate will result in ",
                                                          textOutput("cont_dec_appt_res_52_sum_1_pop", inline = TRUE),
                                                          " patients attending for a continuation appointment. Of these ",
                                                          textOutput("cont_dec_appt_res_52_sum_1_pop_end", inline = TRUE),
                                                          " will continue with treatment. The associated costs of these appointments 
                                                           will be ",
                                                          textOutput("cont_dec_appt_res_52_sum_1_cost", inline = TRUE)
                                                          
                                                  ),
                                                  tags$li("An upper estimate will result in ",
                                                          textOutput("cont_dec_appt_res_52_sum_2_pop", inline = TRUE),
                                                          " patients attending for a continuation appointment. Of these ",
                                                          textOutput("cont_dec_appt_res_52_sum_2_pop_end", inline = TRUE),
                                                          " will continue with treatment. The associated costs of these appointments 
                                                          will be ",
                                                          textOutput("cont_dec_appt_res_52_sum_2_cost", inline = TRUE)
                                                          
                                                  ),
                                                  tags$li("A lower estimate will result in ",
                                                          textOutput("cont_dec_appt_res_52_sum_3_pop", inline = TRUE),
                                                          " patients attending for a continuation appointment. Of these ",
                                                          textOutput("cont_dec_appt_res_52_sum_3_pop_end", inline = TRUE),
                                                          " will continue with treatment. The associated costs of these appointments 
                                                           will be ",
                                                          textOutput("cont_dec_appt_res_52_sum_3_cost", inline = TRUE)
                                                          
                                                  )
                                                )
                                              )
                                              ),
                                    nav_panel("Data Table",
                                              DTOutput("cont_dec_appt_res_52_DT")
                                              )
                                  )
                       ),
                       nav_panel("Week 72",
                                 navset_tab(
                                   nav_panel("Summary",
                                             p("Based on the assumptions in the model the required acitvity and costs assocaited with 
                                                the continuation decision at 72 weeks are:",
                                               tags$ul(
                                                 tags$li("A central estimate will result in ",
                                                         textOutput("cont_dec_appt_res_sum_1_pop", inline = TRUE),
                                                         " patients attending for a continuation appointment. Of these ",
                                                         textOutput("cont_dec_appt_res_sum_1_pop_end", inline = TRUE),
                                                         " will continue with treatment. The associated costs of these appointments 
                                                           will be ",
                                                         textOutput("cont_dec_appt_res_sum_1_cost", inline = TRUE)
                                                         
                                                 ),
                                                 tags$li("An upper estimate will result in ",
                                                         textOutput("cont_dec_appt_res_sum_2_pop", inline = TRUE),
                                                         " patients attending for a continuation appointment. Of these ",
                                                         textOutput("cont_dec_appt_res_sum_2_pop_end", inline = TRUE),
                                                         " will continue with treatment. The associated costs of these appointments 
                                                          will be ",
                                                         textOutput("cont_dec_appt_res_sum_2_cost", inline = TRUE)
                                                         
                                                 ),
                                                 tags$li("A lower estimate will result in ",
                                                         textOutput("cont_dec_appt_res_sum_3_pop", inline = TRUE),
                                                         " patients attending for a continuation appointment. Of these ",
                                                         textOutput("cont_dec_appt_res_sum_3_pop_end", inline = TRUE),
                                                         " will continue with treatment. The associated costs of these appointments 
                                                           will be ",
                                                         textOutput("cont_dec_appt_res_sum_3_cost", inline = TRUE)
                                                         
                                                 )
                                               )
                                             )
                                   ),
                                   nav_panel("Data Table",
                                             DTOutput("cont_dec_appt_res_DT")
                                   )
                                 )
                       )
                       )
             ),
             nav_panel("Lanifibranor",
                       navset_tab(
                         nav_panel("Week 52",
                                   navset_tab(
                                     nav_panel("Summary",
                                               p("Based on the assumptions in the model the required acitvity and costs assocaited with 
                                                the continuation decision at 52 weeks are:",
                                                 tags$ul(
                                                   tags$li("A central estimate will result in ",
                                                           textOutput("cont_dec_appt_lan_52_sum_1_pop", inline = TRUE),
                                                           " patients attending for a continuation appointment. Of these ",
                                                           textOutput("cont_dec_appt_lan_52_sum_1_pop_end", inline = TRUE),
                                                           " will continue with treatment. The associated costs of these appointments 
                                                           will be ",
                                                           textOutput("cont_dec_appt_lan_52_sum_1_cost", inline = TRUE)
                                                           
                                                   ),
                                                   tags$li("An upper estimate will result in ",
                                                           textOutput("cont_dec_appt_lan_52_sum_2_pop", inline = TRUE),
                                                           " patients attending for a continuation appointment. Of these ",
                                                           textOutput("cont_dec_appt_lan_52_sum_2_pop_end", inline = TRUE),
                                                           " will continue with treatment. The associated costs of these appointments 
                                                          will be ",
                                                           textOutput("cont_dec_appt_lan_52_sum_2_cost", inline = TRUE)
                                                           
                                                   ),
                                                   tags$li("A lower estimate will result in ",
                                                           textOutput("cont_dec_appt_lan_52_sum_3_pop", inline = TRUE),
                                                           " patients attending for a continuation appointment. Of these ",
                                                           textOutput("cont_dec_appt_lan_52_sum_3_pop_end", inline = TRUE),
                                                           " will continue with treatment. The associated costs of these appointments 
                                                           will be ",
                                                           textOutput("cont_dec_appt_lan_52_sum_3_cost", inline = TRUE)
                                                           
                                                   )
                                                 )
                                               )
                                     ),
                                     nav_panel("Data Table",
                                               DTOutput("cont_dec_appt_lan_52_DT")
                                     )
                                   )
                         ),
                         nav_panel("Week 72",
                                   navset_tab(
                                     nav_panel("Summary",
                                               p("Based on the assumptions in the model the required acitvity and costs assocaited with 
                                                the continuation decision at 72 weeks are:",
                                                 tags$ul(
                                                   tags$li("A central estimate will result in ",
                                                           textOutput("cont_dec_appt_lan_sum_1_pop", inline = TRUE),
                                                           " patients attending for a continuation appointment. Of these ",
                                                           textOutput("cont_dec_appt_lan_sum_1_pop_end", inline = TRUE),
                                                           " will continue with treatment. The associated costs of these appointments 
                                                           will be ",
                                                           textOutput("cont_dec_appt_lan_sum_1_cost", inline = TRUE)
                                                           
                                                   ),
                                                   tags$li("An upper estimate will result in ",
                                                           textOutput("cont_dec_appt_lan_sum_2_pop", inline = TRUE),
                                                           " patients attending for a continuation appointment. Of these ",
                                                           textOutput("cont_dec_appt_lan_sum_2_pop_end", inline = TRUE),
                                                           " will continue with treatment. The associated costs of these appointments 
                                                          will be ",
                                                           textOutput("cont_dec_appt_lan_sum_2_cost", inline = TRUE)
                                                           
                                                   ),
                                                   tags$li("A lower estimate will result in ",
                                                           textOutput("cont_dec_appt_lan_sum_3_pop", inline = TRUE),
                                                           " patients attending for a continuation appointment. Of these ",
                                                           textOutput("cont_dec_appt_lan_sum_3_pop_end", inline = TRUE),
                                                           " will continue with treatment. The associated costs of these appointments 
                                                           will be ",
                                                           textOutput("cont_dec_appt_lan_sum_3_cost", inline = TRUE)
                                                           
                                                   )
                                                 )
                                               )
                                     ),
                                     nav_panel("Data Table",
                                               DTOutput("cont_dec_appt_lan_DT")
                                     )
                                   )
                         )
                       )
             )
 
                       
           ),
           br(),
           h3("Dosage Maintenance"),
           hr(),
           h4("Dosage Maintenance Delivery"),
           navset_tab(
             nav_panel("All Treatments",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the applied assumptions the modelled activities and costs associated with 
                                     delivery the dosage maintenance treatments between weeks 73 and 103 are:",
                                     tags$ul(
                                       tags$li("A central estimate will result in ",
                                               textOutput("dos_main_all_sum_1_act", inline = TRUE),
                                               " appointment activities at a cost of ",
                                               textOutput("dos_main_all_sum_1_cost", inline = TRUE)
                                       ),
                                       tags$li("An upper estimate will result in ",
                                               textOutput("dos_main_all_sum_2_act", inline = TRUE),
                                               " appointment activities at a cost of ",
                                               textOutput("dos_main_all_sum_2_cost", inline = TRUE)
                                       ),
                                       tags$li("A lower estimate will result in ",
                                               textOutput("dos_main_all_sum_3_act", inline = TRUE),
                                               " appointment activities at a cost of ",
                                               textOutput("dos_main_all_sum_3_cost", inline = TRUE)
                                       ),
                                     ))
                                   ),
                         nav_panel("Data Table",
                                   DTOutput("dos_main_all_DT")
                                   )
                       )
                       ),
             nav_panel("Semaglutide",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the applied assumptions the modelled activities and costs associated with 
                                     delivery the dosage maintenance treatments between weeks 73 and 103 are:",
                                     tags$ul(
                                       tags$li("A central estimate will result in ",
                                               textOutput("dos_main_sem_sum_1_act", inline = TRUE),
                                               " appointment activities at a cost of ",
                                               textOutput("dos_main_sem_sum_1_cost", inline = TRUE)
                                               ),
                                       tags$li("An upper estimate will result in ",
                                               textOutput("dos_main_sem_sum_2_act", inline = TRUE),
                                               " appointment activities at a cost of ",
                                               textOutput("dos_main_sem_sum_2_cost", inline = TRUE)
                                       ),
                                       tags$li("A lower estimate will result in ",
                                               textOutput("dos_main_sem_sum_3_act", inline = TRUE),
                                               " appointment activities at a cost of ",
                                               textOutput("dos_main_sem_sum_3_cost", inline = TRUE)
                                       ),
                                     ))
                                   ),
                         nav_panel("Data Table",
                                   DTOutput("dos_main_sem_DT")
                         )
                       )
             ),
             nav_panel("Survodutide",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the applied assumptions the modelled activities and costs associated with 
                                     delivery the dosage maintenance treatments between weeks 73 and 103 are:",
                                     tags$ul(
                                       tags$li("A central estimate will result in ",
                                               textOutput("dos_main_surv_sum_1_act", inline = TRUE),
                                               " appointment activities at a cost of ",
                                               textOutput("dos_main_surv_sum_1_cost", inline = TRUE)
                                       ),
                                       tags$li("An upper estimate will result in ",
                                               textOutput("dos_main_surv_sum_2_act", inline = TRUE),
                                               " appointment activities at a cost of ",
                                               textOutput("dos_main_surv_sum_2_cost", inline = TRUE)
                                       ),
                                       tags$li("A lower estimate will result in ",
                                               textOutput("dos_main_surv_sum_3_act", inline = TRUE),
                                               " appointment activities at a cost of ",
                                               textOutput("dos_main_surv_sum_3_cost", inline = TRUE)
                                       ),
                                     ))
                         ),
                         nav_panel("Data Table",
                                   DTOutput("dos_main_surv_DT")
                         )
                       )
             ),
             nav_panel("Resmetirom",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the applied assumptions the modelled activities and costs associated with 
                                     delivery the dosage maintenance treatments between weeks 73 and 103 are:",
                                     tags$ul(
                                       tags$li("A central estimate will result in ",
                                               textOutput("dos_main_res_sum_1_act", inline = TRUE),
                                               " appointment activities at a cost of ",
                                               textOutput("dos_main_res_sum_1_cost", inline = TRUE)
                                       ),
                                       tags$li("An upper estimate will result in ",
                                               textOutput("dos_main_res_sum_2_act", inline = TRUE),
                                               " appointment activities at a cost of ",
                                               textOutput("dos_main_res_sum_2_cost", inline = TRUE)
                                       ),
                                       tags$li("A lower estimate will result in ",
                                               textOutput("dos_main_res_sum_3_act", inline = TRUE),
                                               " appointment activities at a cost of ",
                                               textOutput("dos_main_res_sum_3_cost", inline = TRUE)
                                       ),
                                     ))
                         ),
                         nav_panel("Data Table",
                                   DTOutput("dos_main_res_DT")
                         )
                       )
             ),
             nav_panel("Lanifibranor",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the applied assumptions the modelled activities and costs associated with 
                                     delivery the dosage maintenance treatments between weeks 73 and 103 are:",
                                     tags$ul(
                                       tags$li("A central estimate will result in ",
                                               textOutput("dos_main_lan_sum_1_act", inline = TRUE),
                                               " appointment activities at a cost of ",
                                               textOutput("dos_main_lan_sum_1_cost", inline = TRUE)
                                       ),
                                       tags$li("An upper estimate will result in ",
                                               textOutput("dos_main_lan_sum_2_act", inline = TRUE),
                                               " appointment activities at a cost of ",
                                               textOutput("dos_main_lan_sum_2_cost", inline = TRUE)
                                       ),
                                       tags$li("A lower estimate will result in ",
                                               textOutput("dos_main_lan_sum_3_act", inline = TRUE),
                                               " appointment activities at a cost of ",
                                               textOutput("dos_main_lan_sum_3_cost", inline = TRUE)
                                       ),
                                     ))
                         ),
                         nav_panel("Data Table",
                                   DTOutput("dos_main_lan_DT")
                         )
                       )
             )
           ),
           br(),
           h4("Dosage Maintenance Diagnostic Monitoring"),
           navset_tab(
             nav_panel("All Treatments",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the applied modelling assumptions the modelled activities and costs 
                                     associated with the diagnostic monitoring between weeks 73 and 103 are:",
                                     tags$ul(
                                       tags$li("A central estimate will result in ",
                                               textOutput("dos_main_diag_mon_all_sum_1_act", inline = TRUE),
                                               " diagnostic testing points with an associated cost of ",
                                               textOutput("dos_main_diag_mon_all_sum_1_cost", inline = TRUE)
                                       ),
                                       tags$li("An upper estimate will result in ",
                                               textOutput("dos_main_diag_mon_all_sum_2_act", inline = TRUE),
                                               " diagnostic testing points with an associated cost of ",
                                               textOutput("dos_main_diag_mon_all_sum_2_cost", inline = TRUE)
                                       ),
                                       tags$li("A lower estimate will result in ",
                                               textOutput("dos_main_diag_mon_all_sum_3_act", inline = TRUE),
                                               " diagnostic testing points with an associated cost of ",
                                               textOutput("dos_main_diag_mon_all_sum_3_cost", inline = TRUE)
                                       )
                                     ))      
                           
                         ),
                         nav_panel("Data Table",
                                   DTOutput("dos_main_diag_mon_all_DT")
                                   )
                       )
                       ),
             nav_panel("Semaglutide",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the applied modelling assumptions the modelled activities and costs 
                                     associated with the diagnostic monitoring between weeks 73 and 103 are:",
                                     tags$ul(
                                       tags$li("A central estimate will result in ",
                                               textOutput("dos_main_diag_mon_sem_sum_1_act", inline = TRUE),
                                               " diagnostic testing points with an associated cost of ",
                                               textOutput("dos_main_diag_mon_sem_sum_1_cost", inline = TRUE)
                                               ),
                                       tags$li("An upper estimate will result in ",
                                               textOutput("dos_main_diag_mon_sem_sum_2_act", inline = TRUE),
                                               " diagnostic testing points with an associated cost of ",
                                               textOutput("dos_main_diag_mon_sem_sum_2_cost", inline = TRUE)
                                       ),
                                       tags$li("A lower estimate will result in ",
                                               textOutput("dos_main_diag_mon_sem_sum_3_act", inline = TRUE),
                                               " diagnostic testing points with an associated cost of ",
                                               textOutput("dos_main_diag_mon_sem_sum_3_cost", inline = TRUE)
                                       )
                                     ))
                                   ),
                         nav_panel("Data Table",
                                   DTOutput("dos_main_diag_mon_sem_DT")
                         )
                       )
             ),
             nav_panel("Survodutide",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the applied modelling assumptions the modelled activities and costs 
                                     associated with the diagnostic monitoring between weeks 73 and 103 are:",
                                     tags$ul(
                                       tags$li("A central estimate will result in ",
                                               textOutput("dos_main_diag_mon_surv_sum_1_act", inline = TRUE),
                                               " diagnostic testing points with an associated cost of ",
                                               textOutput("dos_main_diag_mon_surv_sum_1_cost", inline = TRUE)
                                       ),
                                       tags$li("An upper estimate will result in ",
                                               textOutput("dos_main_diag_mon_surv_sum_2_act", inline = TRUE),
                                               " diagnostic testing points with an associated cost of ",
                                               textOutput("dos_main_diag_mon_surv_sum_2_cost", inline = TRUE)
                                       ),
                                       tags$li("A lower estimate will result in ",
                                               textOutput("dos_main_diag_mon_surv_sum_3_act", inline = TRUE),
                                               " diagnostic testing points with an associated cost of ",
                                               textOutput("dos_main_diag_mon_surv_sum_3_cost", inline = TRUE)
                                       )
                                     ))
                         ),
                         nav_panel("Data Table",
                                   DTOutput("dos_main_diag_mon_surv_DT")
                         )
                       )
             ),
             nav_panel("Resmetirom",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the applied modelling assumptions the modelled activities and costs 
                                     associated with the diagnostic monitoring between weeks 73 and 103 are:",
                                     tags$ul(
                                       tags$li("A central estimate will result in ",
                                               textOutput("dos_main_diag_mon_res_sum_1_act", inline = TRUE),
                                               " diagnostic testing points with an associated cost of ",
                                               textOutput("dos_main_diag_mon_res_sum_1_cost", inline = TRUE)
                                       ),
                                       tags$li("An upper estimate will result in ",
                                               textOutput("dos_main_diag_mon_res_sum_2_act", inline = TRUE),
                                               " diagnostic testing points with an associated cost of ",
                                               textOutput("dos_main_diag_mon_res_sum_2_cost", inline = TRUE)
                                       ),
                                       tags$li("A lower estimate will result in ",
                                               textOutput("dos_main_diag_mon_res_sum_3_act", inline = TRUE),
                                               " diagnostic testing points with an associated cost of ",
                                               textOutput("dos_main_diag_mon_res_sum_3_cost", inline = TRUE)
                                       )
                                     ))
                         ),
                         nav_panel("Data Table",
                                   DTOutput("dos_main_diag_mon_res_DT")
                         )
                       )
             ),
             nav_panel("Lanifibranor",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the applied modelling assumptions the modelled activities and costs 
                                     associated with the diagnostic monitoring between weeks 73 and 103 are:",
                                     tags$ul(
                                       tags$li("A central estimate will result in ",
                                               textOutput("dos_main_diag_mon_lan_sum_1_act", inline = TRUE),
                                               " diagnostic testing points with an associated cost of ",
                                               textOutput("dos_main_diag_mon_lan_sum_1_cost", inline = TRUE)
                                       ),
                                       tags$li("An upper estimate will result in ",
                                               textOutput("dos_main_diag_mon_lan_sum_2_act", inline = TRUE),
                                               " diagnostic testing points with an associated cost of ",
                                               textOutput("dos_main_diag_mon_lan_sum_2_cost", inline = TRUE)
                                       ),
                                       tags$li("A lower estimate will result in ",
                                               textOutput("dos_main_diag_mon_lan_sum_3_act", inline = TRUE),
                                               " diagnostic testing points with an associated cost of ",
                                               textOutput("dos_main_diag_mon_lan_sum_3_cost", inline = TRUE)
                                       )
                                     ))
                         ),
                         nav_panel("Data Table",
                                   DTOutput("dos_main_diag_mon_lan_DT")
                         )
                       )
             )
           ),
           br(),
           h3("On-going Treatment"),
           hr(),
           h4("On-going Treatment Delivery"),
           navset_tab(
             nav_panel("All Treatments",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the applied assumptions the modelled activities and costs associated with 
                                     delivery of treatment after 104+ weeks are:",
                                     tags$ul(
                                       tags$li("A central estimate will result in ",
                                               textOutput("ongoing_all_sum_1_act", inline = TRUE),
                                               "appointments with an associated cost of ",
                                               textOutput("ongoing_all_sum_1_cost", inline = TRUE)
                                               
                                       ),
                                       tags$li("An upper estimate will result in ",
                                               textOutput("ongoing_all_sum_2_act", inline = TRUE),
                                               "appointments with an associated cost of ",
                                               textOutput("ongoing_all_sum_2_cost", inline = TRUE)
                                               
                                       ),
                                       tags$li("A lower estimate will result in ",
                                               textOutput("ongoing_all_sum_3_act", inline = TRUE),
                                               "appointments with an associated cost of ",
                                               textOutput("ongoing_all_sum_3_cost", inline = TRUE)
                                               
                                       )
                                     ))
                                   ),
                         nav_panel("Data Table",
                                   DTOutput("ongoing_all_DT")
                                   )
                       )
                       ),
             nav_panel("Semaglutide",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the applied assumptions the modelled activities and costs associated with 
                                     delivery of treatment after 104+ weeks are:",
                                     tags$ul(
                                       tags$li("A central estimate will result in ",
                                               textOutput("ongoing_sem_sum_1_act", inline = TRUE),
                                               "appointments with an associated cost of ",
                                               textOutput("ongoing_sem_sum_1_cost", inline = TRUE)
                                         
                                       ),
                                       tags$li("An upper estimate will result in ",
                                               textOutput("ongoing_sem_sum_2_act", inline = TRUE),
                                               "appointments with an associated cost of ",
                                               textOutput("ongoing_sem_sum_2_cost", inline = TRUE)
                                               
                                       ),
                                       tags$li("A lower estimate will result in ",
                                               textOutput("ongoing_sem_sum_3_act", inline = TRUE),
                                               "appointments with an associated cost of ",
                                               textOutput("ongoing_sem_sum_3_cost", inline = TRUE)
                                               
                                       )
                                     ))
                         ),
                         nav_panel("Data Table",
                                   DTOutput("ongoing_sem_DT")),
                         
                       )
             ),
             nav_panel("Survodutide",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the applied assumptions the modelled activities and costs associated with 
                                     delivery of treatment after 104+ weeks are:",
                                     tags$ul(
                                       tags$li("A central estimate will result in ",
                                               textOutput("ongoing_surv_sum_1_act", inline = TRUE),
                                               "appointments with an associated cost of ",
                                               textOutput("ongoing_surv_sum_1_cost", inline = TRUE)
                                               
                                       ),
                                       tags$li("An upper estimate will result in ",
                                               textOutput("ongoing_surv_sum_2_act", inline = TRUE),
                                               "appointments with an associated cost of ",
                                               textOutput("ongoing_surv_sum_2_cost", inline = TRUE)
                                               
                                       ),
                                       tags$li("A lower estimate will result in ",
                                               textOutput("ongoing_surv_sum_3_act", inline = TRUE),
                                               "appointments with an associated cost of ",
                                               textOutput("ongoing_surv_sum_3_cost", inline = TRUE)
                                               
                                       )
                                     ))
                         ),
                         nav_panel("Data Table",
                                   DTOutput("ongoing_surv_DT")),
                         
                       )
             ),
             nav_panel("Resmetirom",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the applied assumptions the modelled activities and costs associated with 
                                     delivery of treatment after 104+ weeks are:",
                                     tags$ul(
                                       tags$li("A central estimate will result in ",
                                               textOutput("ongoing_res_sum_1_act", inline = TRUE),
                                               "appointments with an associated cost of ",
                                               textOutput("ongoing_res_sum_1_cost", inline = TRUE)
                                               
                                       ),
                                       tags$li("An upper estimate will result in ",
                                               textOutput("ongoing_res_sum_2_act", inline = TRUE),
                                               "appointments with an associated cost of ",
                                               textOutput("ongoing_res_sum_2_cost", inline = TRUE)
                                               
                                       ),
                                       tags$li("A lower estimate will result in ",
                                               textOutput("ongoing_res_sum_3_act", inline = TRUE),
                                               "appointments with an associated cost of ",
                                               textOutput("ongoing_res_sum_3_cost", inline = TRUE)
                                               
                                       )
                                     ))
                         ),
                         nav_panel("Data Table",
                                   DTOutput("ongoing_res_DT")),
                         
                       )
             ),
             nav_panel("Lanifibranor",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the applied assumptions the modelled activities and costs associated with 
                                     delivery of treatment after 104+ weeks are:",
                                     tags$ul(
                                       tags$li("A central estimate will result in ",
                                               textOutput("ongoing_lan_sum_1_act", inline = TRUE),
                                               "appointments with an associated cost of ",
                                               textOutput("ongoing_lan_sum_1_cost", inline = TRUE)
                                               
                                       ),
                                       tags$li("An upper estimate will result in ",
                                               textOutput("ongoing_lan_sum_2_act", inline = TRUE),
                                               "appointments with an associated cost of ",
                                               textOutput("ongoing_lan_sum_2_cost", inline = TRUE)
                                               
                                       ),
                                       tags$li("A lower estimate will result in ",
                                               textOutput("ongoing_lan_sum_3_act", inline = TRUE),
                                               "appointments with an associated cost of ",
                                               textOutput("ongoing_lan_sum_3_cost", inline = TRUE)
                                               
                                       )
                                     ))
                         ),
                         nav_panel("Data Table",
                                   DTOutput("ongoing_lan_DT")),
                         
                       )
             )
           ),
           br(),
           h4("On-going Treatment Monitoring Diagnostics"),
           navset_tab(
             nav_panel("All Treatments",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the applied assumptions the modelled activities and costs associated with 
                                     monitoring diagnostics after 104+ weeks are:",
                                     tags$ul(
                                       tags$li("A central estimate will result in ",
                                               textOutput("ongoing_diag_mon_all_sum_1_act", inline = TRUE),
                                               "appointments with an associated cost of ",
                                               textOutput("ongoing_diag_mon_all_sum_1_cost", inline = TRUE)
                                               
                                       ),
                                       tags$li("An upper estimate will result in ",
                                               textOutput("ongoing_diag_mon_all_sum_2_act", inline = TRUE),
                                               "appointments with an associated cost of ",
                                               textOutput("ongoing_diag_mon_all_sum_2_cost", inline = TRUE)
                                               
                                       ),
                                       tags$li("A lower estimate will result in ",
                                               textOutput("ongoing_diag_mon_all_sum_3_act", inline = TRUE),
                                               "appointments with an associated cost of ",
                                               textOutput("ongoing_diag_mon_all_sum_3_cost", inline = TRUE)
                                               
                                       )
                                     ))
                                   ),
                         nav_panel("Data Table",
                                   DTOutput("ongoing_diag_mon_all_DT")
                                   )
                       )
                       ),
             nav_panel("Semaglutide",
                       navset_tab(
                         nav_panel("Summary",
                                   p("Based on the applied assumptions the modelled activities and costs associated with 
                                     monitoring diagnostics after 104+ weeks are:",
                                     tags$ul(
                                       tags$li("A central estimate will result in ",
                                               textOutput("ongoing_diag_mon_sem_sum_1_act", inline = TRUE),
                                               "appointments with an associated cost of ",
                                               textOutput("ongoing_diag_mon_sem_sum_1_cost", inline = TRUE)
                                               
                                       ),
                                       tags$li("An upper estimate will result in ",
                                               textOutput("ongoing_diag_mon_sem_sum_2_act", inline = TRUE),
                                               "appointments with an associated cost of ",
                                               textOutput("ongoing_diag_mon_sem_sum_2_cost", inline = TRUE)
                                               
                                       ),
                                       tags$li("A lower estimate will result in ",
                                               textOutput("ongoing_diag_mon_sem_sum_3_act", inline = TRUE),
                                               "appointments with an associated cost of ",
                                               textOutput("ongoing_diag_mon_sem_sum_3_cost", inline = TRUE)
                                               
                                       )
                                     ))
                                   ),
                         nav_panel("Data Table",
                                   DTOutput("ongoing_diag_mon_sem_DT")
                         )
                       )
                       
                       
           ),
           nav_panel("Survodutide",
                     navset_tab(
                       nav_panel("Summary",
                                 p("Based on the applied assumptions the modelled activities and costs associated with 
                                     monitoring diagnostics after 104+ weeks are:",
                                   tags$ul(
                                     tags$li("A central estimate will result in ",
                                             textOutput("ongoing_diag_mon_surv_sum_1_act", inline = TRUE),
                                             "appointments with an associated cost of ",
                                             textOutput("ongoing_diag_mon_surv_sum_1_cost", inline = TRUE)
                                             
                                     ),
                                     tags$li("An upper estimate will result in ",
                                             textOutput("ongoing_diag_mon_surv_sum_2_act", inline = TRUE),
                                             "appointments with an associated cost of ",
                                             textOutput("ongoing_diag_mon_surv_sum_2_cost", inline = TRUE)
                                             
                                     ),
                                     tags$li("A lower estimate will result in ",
                                             textOutput("ongoing_diag_mon_surv_sum_3_act", inline = TRUE),
                                             "appointments with an associated cost of ",
                                             textOutput("ongoing_diag_mon_surv_sum_3_cost", inline = TRUE)
                                             
                                     )
                                   ))
                       ),
                       nav_panel("Data Table",
                                 DTOutput("ongoing_diag_mon_surv_DT")
                       )
                     )
                     
                     
           ),
           nav_panel("Resmetirom",
                     navset_tab(
                       nav_panel("Summary",
                                 p("Based on the applied assumptions the modelled activities and costs associated with 
                                     monitoring diagnostics after 104+ weeks are:",
                                   tags$ul(
                                     tags$li("A central estimate will result in ",
                                             textOutput("ongoing_diag_mon_res_sum_1_act", inline = TRUE),
                                             "appointments with an associated cost of ",
                                             textOutput("ongoing_diag_mon_res_sum_1_cost", inline = TRUE)
                                             
                                     ),
                                     tags$li("An upper estimate will result in ",
                                             textOutput("ongoing_diag_mon_res_sum_2_act", inline = TRUE),
                                             "appointments with an associated cost of ",
                                             textOutput("ongoing_diag_mon_res_sum_2_cost", inline = TRUE)
                                             
                                     ),
                                     tags$li("A lower estimate will result in ",
                                             textOutput("ongoing_diag_mon_res_sum_3_act", inline = TRUE),
                                             "appointments with an associated cost of ",
                                             textOutput("ongoing_diag_mon_res_sum_3_cost", inline = TRUE)
                                             
                                     )
                                   ))
                       ),
                       nav_panel("Data Table",
                                 DTOutput("ongoing_diag_mon_res_DT")
                       )
                     )
                     
                     
           ),
           nav_panel("Lanifibranor",
                     navset_tab(
                       nav_panel("Summary",
                                 p("Based on the applied assumptions the modelled activities and costs associated with 
                                     monitoring diagnostics after 104+ weeks are:",
                                   tags$ul(
                                     tags$li("A central estimate will result in ",
                                             textOutput("ongoing_diag_mon_lan_sum_1_act", inline = TRUE),
                                             "appointments with an associated cost of ",
                                             textOutput("ongoing_diag_mon_lan_sum_1_cost", inline = TRUE)
                                             
                                     ),
                                     tags$li("An upper estimate will result in ",
                                             textOutput("ongoing_diag_mon_lan_sum_2_act", inline = TRUE),
                                             "appointments with an associated cost of ",
                                             textOutput("ongoing_diag_mon_lan_sum_2_cost", inline = TRUE)
                                             
                                     ),
                                     tags$li("A lower estimate will result in ",
                                             textOutput("ongoing_diag_mon_lan_sum_3_act", inline = TRUE),
                                             "appointments with an associated cost of ",
                                             textOutput("ongoing_diag_mon_lan_sum_3_cost", inline = TRUE)
                                             
                                     )
                                   ))
                       ),
                       nav_panel("Data Table",
                                 DTOutput("ongoing_diag_mon_lan_DT")
                       )
                     )
                     
                     
           )
           )

           

           
           
           

           )
  )
  )
)