library(DT)
library(dplyr)
library(tidyr)
library(ggplot2)
library(EnvStats)

server <- function(input, output, session) {
  
  assumptions <- reactive({
    
    list(
      adult_pop = input$adult_pop,
      masld_prev = input$masld_prev / 100,
      masld_prev_tol = input$masld_prev_tol / 100,
      mash_prev = input$mash_prev / 100,
      mash_prev_tol = input$mash_prev_tol / 100,
      F0_prop = input$F0_prop / 100,
      F1_prop = input$F1_prop / 100,
      F2_prop = input$F2_prop / 100,
      F3_prop = input$F3_prop / 100,
      F4_prop = input$F4_prop / 100,
      F0_diag_prop = input$diag_F0 / 100,
      F1_diag_prop = input$diag_F1 / 100,
      F2_diag_prop = input$diag_F2 / 100,
      F3_diag_prop = input$diag_F3 / 100,
      F4_diag_prop = input$diag_F4 / 100
      )
    
  })
  
  treat_imp_assumptions <- reactive({
    list(
      treat_prop_F0_sem = input$treat_pop_F0_sem / 100,
      treat_prop_F1_sem = input$treat_pop_F1_sem / 100,
      treat_prop_F2_sem = input$treat_pop_F2_sem / 100,
      treat_prop_F3_sem = input$treat_pop_F3_sem / 100,
      treat_prop_F4_sem = input$treat_pop_F4_sem / 100
    )
  })
  
  sem_pathway_assumptions <- reactive({
    
    list(
      pre_treat_biopsy_sem = input$liver_biopsy_prop_sem / 100
    )
    
  })
  
  masld_estimates <- reactive({
    params <- assumptions()
    
    masld_pop_central <- params$adult_pop * params$masld_prev
    masld_pop_lower <- params$adult_pop * max(params$masld_prev - params$masld_prev_tol, 0)
    masld_pop_upper <- params$adult_pop * min(params$masld_prev + params$masld_prev_tol, 1)
    
    list(central = masld_pop_central, lower = masld_pop_lower, upper = masld_pop_upper)
  })
  
  masld_pop_sim <- reactive({
    estimates <- masld_estimates()
    masld_pop_sim_values <- round(rtri(n = 97,
                                       min = estimates$lower,
                                       mode = estimates$central,
                                       max = estimates$upper),
                                  0)
    data.frame(
      simulation = 1:100,
      masld_population = c(estimates$central,
                           estimates$lower,
                           estimates$upper,
                           masld_pop_sim_values)
    )
    
  })
  
  mash_estimates <- reactive({
    params <- assumptions()
    
    mash_pop_central <- params$adult_pop * params$mash_prev
    mash_pop_lower <- params$adult_pop * max(params$mash_prev - params$mash_prev_tol, 0)
    mash_pop_upper <- params$adult_pop * min(params$mash_prev + params$mash_prev_tol, 1)
    
    list(central = mash_pop_central, lower = mash_pop_lower, upper = mash_pop_upper)
  })
  
  mash_pop_sim <- reactive({
    estimates <- mash_estimates()
    mash_pop_sim_values <- round(rtri(97,
                                      min = estimates$lower,
                                      mode = estimates$central,
                                      max = estimates$upper),
                                 0)
    data.frame(
      simulation = 1:100,
      mash_population = c(estimates$central,
                          estimates$lower,
                          estimates$upper,
                          mash_pop_sim_values)
    )
    
  })
  
  f_stage_estimates <- reactive({
    params <- assumptions()
    mash_pop_sim() |>
      mutate("F0" = round(mash_population * params$F0_prop, 0),
             "F1" = round(mash_population * params$F1_prop, 0),
             "F2" = round(mash_population * params$F2_prop, 0),
             "F3" = round(mash_population * params$F3_prop, 0),
             "F4" = round(mash_population * params$F4_prop, 0)) |>
      select(-c(mash_population))
    
  })
  
  f_stage_estimates_long <- reactive({
    f_stage_estimates() |>
      gather("Fibrosis_Stage", "Estimate", -c("simulation"))
  })
  
  f_stage_diagnosed <- reactive({
    params <- assumptions()
    f_stage_estimates() |>
      mutate("F0_diag" = round(F0 * params$F0_diag_prop, 0),
             "F1_diag" = round(F1 * params$F1_diag_prop, 0),
             "F2_diag" = round(F2 * params$F2_diag_prop, 0),
             "F3_diag" = round(F3 * params$F3_diag_prop, 0),
             "F4_diag" = round(F4 * params$F4_diag_prop, 0)
      ) |>
      select(-c(F0, F1, F2, F3, F4))
  })
  
  f_stage_diagnosed_long <- reactive({
    f_stage_diagnosed() |>
      gather("Fibrosis_Stage", "Estimate", -c("simulation"))
  })
  
  treat_pop_sem <- reactive({
    params <- treat_imp_assumptions()
    f_stage_diagnosed() |>
      mutate("F0_treat" = round(F0_diag * params$treat_prop_F0_sem, 0),
             "F1_treat" = round(F1_diag * params$treat_prop_F1_sem, 0),
             "F2_treat" = round(F2_diag * params$treat_prop_F2_sem, 0),
             "F3_treat" = round(F3_diag * params$treat_prop_F3_sem, 0),
             "F4_treat" = round(F4_diag * params$treat_prop_F4_sem, 0)) |>
      select(-c(F0_diag, F1_diag, F2_diag, F3_diag, F4_diag)) |>
      rowwise() |>
      mutate("treated_total" = sum(c_across(F0_treat:F4_treat)))
    
  })
  
  pre_treat_biopsy_sem <- reactive({
    
    params <- sem_pathway_assumptions()
    treat_pop_sem() |>
      select(c(simulation, treated_total)) |>
      mutate("Liver Biopsy Activities" = round(treated_total * params$pre_treat_biopsy_sem, 0))
    
  })
  
  
  output$masld_pop_histogram <- renderPlot({
    masld_pop_hist_df <- masld_pop_sim()
    ggplot(masld_pop_hist_df, aes(x = masld_population))+
      geom_histogram(binwidth = diff(range(masld_pop_hist_df$masld_population)) / 20,
                     fill = "#407EC9",
                     color = "#000000",
                     alpha = 0.5) +
      scale_y_continuous(labels = comma_format()) +
      scale_x_continuous(labels = comma_format()) +
      labs(title = "Simulated MASLD Population",
           x = "MASLD Population",
           y = "Count")
  })
  
  output$masld_prev_summary <- renderPrint({
    masld_pop <- masld_estimates() 
    
    paste0(
      "MASLD Population Estimate:\n",
      "Central: ", round(masld_pop$central, 0), "\n",
      "Lower Bound: ", round(masld_pop$lower, 0), "\n",
      "Upper Bound: ", round(masld_pop$upper, 0)
    )
  })
  
  output$masld_pop_DT <- renderDT({
    masld_pop_DT <- masld_pop_sim() |>
      rename("Simulation" = 1,
             "MASLD Population" = 2)
    datatable(masld_pop_DT,
              rownames = FALSE,
              options = list(pageLength = 10,
                                           autoWidth = TRUE
                                           ))
  })
  
  output$mash_prev_summary <- renderPrint({
    mash_pop <- mash_estimates() 
    
    paste0(
      "MASH Population Estimate:\n",
      "Central: ", round(mash_pop$central, 0), "\n",
      "Lower Bound: ", round(mash_pop$lower, 0), "\n",
      "Upper Bound: ", round(mash_pop$upper, 0)
    )
  })
  
  output$mash_pop_histogram <- renderPlot({
    mash_pop_hist_df <- mash_pop_sim()
    ggplot(mash_pop_hist_df, aes(x = mash_population))+
      geom_histogram(binwidth = diff(range(mash_pop_hist_df$mash_population)) / 20,
                     fill = "#407EC9",
                     color = "#000000",
                     alpha = 0.5) +
      scale_y_continuous(labels = comma_format()) +
      scale_x_continuous(labels = comma_format()) +
      labs(title = "Simulated MASH Population",
           x = "MASH Population",
           y = "Count")
  })
  
  output$mash_pop_DT <- renderDT({
    mash_pop_DT <- mash_pop_sim() |>
      rename("Simulation" = 1,
             "MASH Population" = 2)
    datatable(mash_pop_DT,
              rownames = FALSE,
              options = list(pageLength = 10,
                                          autoWidth = TRUE))
  })
  
  output$f_stage_pop_histogram <- renderPlot({
    f_stage_pop_hist_df <- f_stage_estimates_long()
    ggplot(f_stage_pop_hist_df, aes(x = Estimate))+
      geom_histogram(bins = 20,
                     fill = "#407EC9",
                     color = "#000000",
                     alpha = 0.5) +
      scale_y_continuous(labels = comma_format()) +
      scale_x_continuous(labels = comma_format()) +
      facet_wrap(~Fibrosis_Stage, scales = "free") +
      labs(title = "Simulated Fibrosis Stage Populations",
           x = "Population",
           y = "Count")
  })
  
  output$f_stage_pop_DT <- renderDT({
    f_stage_pop_DT <- f_stage_estimates() |>
      rename("Simulation" = 1)
    datatable(f_stage_pop_DT,
              rownames = FALSE,
              options = list(pageLength = 10,
                                             autoWidth = TRUE))
  })
  
  output$f_stage_pop_DT_long <- renderDT({
    f_stage_pop_DT_long <- f_stage_estimates_long()
    datatable(f_stage_pop_DT_long, options = list(pageLength = 10,
                                                  autoWidth = TRUE))
  })
  
  
  output$f_stage_diag_histogram <- renderPlot({
    f_stage_diag_hist_df <- f_stage_diagnosed_long()
    ggplot(f_stage_diag_hist_df, aes(x = Estimate))+
      geom_histogram(bins = 20,
                     fill = "#407EC9",
                     color = "#000000",
                     alpha = 0.5) +
      scale_y_continuous(labels = comma_format()) +
      scale_x_continuous(labels = comma_format()) +
      facet_wrap(~Fibrosis_Stage, scales = "free") +
      labs(title = "Simulated Fibrosis Stage Populations Diagnosed",
           x = "Diagnosed Population",
           y = "Count")
  })
  
  output$f_stage_diag_DT <- renderDT({
    f_stage_diag_DT <- f_stage_diagnosed() |>
      rename("Simulation" = 1,
             "F0" = 2,
             "F1" = 3,
             "F2" = 4,
             "F3" = 5,
             "F4" = 6)
    datatable(f_stage_diag_DT,
              rownames = FALSE,
              options = list(pageLength = 10,
                             autoWidth = TRUE))
  })
  
  output$treat_pop_sem_DT <- renderDT({
    treat_pop_sem <- treat_pop_sem() |>
      rename("Simulation" = 1,
             "F0" = 2,
             "F1" = 3,
             "F2" = 4,
             "F3" = 5,
             "F4" = 6,
             "Total Treated" = 7)
    datatable(treat_pop_sem,
              rownames = FALSE,
              options = list(pageLength = 10,
                             autoWidth = TRUE))
  })
  
  
  output$pre_treat_biopsy_sem_DT <- renderDT({
    pre_treat_biopsy_sem_DT <- pre_treat_biopsy_sem()
    datatable(pre_treat_biopsy_sem_DT,
              rownames = FALSE,
              options = list(pageLength = 10,
                             autoWifth = TRUE))
  })
  
  output$download_assumptions <- downloadHandler(
    filename = function() { "population_assumptions_pop.csv" },
    content = function(file) {
      
      df <- as.data.frame(t(unlist(assumptions())))
      write.csv(df, file, row.names = FALSE)
    }
  )
  
}