library(DT)

assumptions_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Ensure inputs are available before calculation
    assumptions <- reactive({
      req(input$adult_pop, input$masld_prev, input$masld_prev_tol)
      
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
        F4_prop = input$F4_prop / 100
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
      masld_pop_sim_values <- round(rtri(97,
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

    output$masld_prev_summary <- renderPrint({
      masld_pop <- masld_estimates() 
      
      paste0(
        "MASLD Population Estimate:\n",
        "Central: ", round(masld_pop$central, 0), "\n",
        "Lower Bound: ", round(masld_pop$lower, 0), "\n",
        "Upper Bound: ", round(masld_pop$upper, 0)
      )
    })
    
    output$masld_pop_histogram <- renderPlot({
      masld_pop_hist_df <- masld_pop_sim()
      ggplot(masld_pop_hist_df, aes(x = masld_population))+
        geom_histogram(binwidth = diff(range(masld_pop_hist_df$masld_population)) / 20,
                       fill = "#407EC9",
                       color = "#000000",
                       alpha = 0.5) +
        labs(title = "Simulated MASLD Population",
             x = "MASLD Population",
             y = "Count")
    })
    
    output$masld_pop_DT <- renderDT({
      masld_pop_DT <- masld_pop_sim()
      datatable(masld_pop_DT, options = list(pageLength = 10,
                                             autoWidth = TRUE))
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
        labs(title = "Simulated MASH Population",
             x = "MASH Population",
             y = "Count")
    })
    
    output$mash_pop_DT <- renderDT({
      mash_pop_DT <- mash_pop_sim()
      datatable(mash_pop_DT, options = list(pageLength = 10,
                                            autoWidth = TRUE))
    })
    
    
    
    output$download_assumptions <- downloadHandler(
      filename = function() { "assumptions_pop.csv" },
      content = function(file) {
        
        df <- as.data.frame(t(unlist(assumptions())))
        write.csv(df, file, row.names = FALSE)
      }
    )
    
  })
}
