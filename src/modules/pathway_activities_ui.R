library(shiny)

pathway_activities_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(title = "Pathway Activity Assumptions",
           fluidPage(
             h1("Pathway Activity Assumptions"),
             p("In this section you can set the assumptions about the clinical activities that occur along the treatment pathway"),
             fluidRow(
               column(2,
                      h3("Pre-Treatment"),
                      h4("Liver Biopsy"),
                      sliderInput(ns("liver_biopsy_prop"),
                                  "Set Percentage undergoing biopsy:",
                                  value = 50,
                                  min = 0,
                                  max = 100,
                                  post = "%")
               ),
               column(2,
                      h3("Week 0"),
                      h4("Baseline Testing"),
                      sliderInput(ns("baseline_testing_elf"),
                                  "Set Percentage undergoing Baseline ELF:",
                                  value = 80,
                                  min = 0,
                                  max = 100,
                                  post = "%"),
                      sliderInput(ns("baseline_testing_biomarkers"),
                                  "Set Percentage undergoing Baseline biomarkers:",
                                  value = 20,
                                  min = 0,
                                  max = 100,
                                  post = "%"),
                      selectInput(ns("baseline_test_setting"),
                                  "Input setting for Baseline testing:",
                                  choice = c("Primary Care - GP",
                                             "Primary Care - Nurse led",
                                             "Secondary Care - Hepatologist"),
                                  selected = "Secondary Care - Hepatologist"),
                      h4("Treatment Delivery"),
                      sliderInput(ns("semaglutide_retention"),
                                  "Input retention on Semaglutide for 16 weeks:",
                                  value = 85,
                                  min = 0,
                                  max = 100,
                                  post = "%"),
                      selectInput(ns("semaglutide_0-16_delivery_setting"),
                                  "Input the setting for delivery of Semaglutide for weeks 0-16:",
                                  choice = c("Primary Care - GP",
                                             "Primary Care - Nurse led",
                                             "Secondary Care - Hepatologist"),
                                  selected = "Secondary Care - Hepatologist")
                      
               )
             )
           )
  )
}