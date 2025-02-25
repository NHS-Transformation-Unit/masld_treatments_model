library(DT)
library(dplyr)
library(tidyr)
library(ggplot2)
library(EnvStats)





server <- function(input, output, session) {
  
  addResourcePath(prefix = "config", directoryPath = "www/config")
  
# Load Inputs -------------------------------------------------------------
  
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
      treat_prop_F4_sem = input$treat_pop_F4_sem / 100,
      treat_prop_F0_surv = input$treat_pop_F0_surv / 100,
      treat_prop_F1_surv = input$treat_pop_F1_surv / 100,
      treat_prop_F2_surv = input$treat_pop_F2_surv / 100,
      treat_prop_F3_surv = input$treat_pop_F3_surv / 100,
      treat_prop_F4_surv = input$treat_pop_F4_surv / 100,
      treat_prop_F0_res = input$treat_pop_F0_res / 100,
      treat_prop_F1_res = input$treat_pop_F1_res / 100,
      treat_prop_F2_res = input$treat_pop_F2_res / 100,
      treat_prop_F3_res = input$treat_pop_F3_res / 100,
      treat_prop_F4_res = input$treat_pop_F4_res / 100,
      treat_prop_F0_lan = input$treat_pop_F0_lan / 100,
      treat_prop_F1_lan = input$treat_pop_F1_lan / 100,
      treat_prop_F2_lan = input$treat_pop_F2_lan / 100,
      treat_prop_F3_lan = input$treat_pop_F3_lan / 100,
      treat_prop_F4_lan = input$treat_pop_F4_lan / 100
    )
  })
  
  sem_pathway_assumptions <- reactive({
    
    list(
      pre_liver_biopsy_prop_sem = input$pre_liver_biopsy_prop_sem / 100,
      pre_elf_prop_sem = input$pre_elf_prop_sem / 100,
      pre_biomarkers_prop_sem = input$pre_biomarkers_prop_sem / 100,
      pre_fibro_prop_sem = input$pre_fibro_prop_sem / 100,
      mm_assess_prop_sem = input$mm_assess_prop_sem / 100,
      mm_assess_setting_sem_pc_gp = (input$mm_assess_setting_sem[1, 1]) / 100,
      mm_assess_setting_sem_pc_nur = (input$mm_assess_setting_sem[2, 1]) / 100,
      mm_assess_setting_sem_sc_hgc = (input$mm_assess_setting_sem[3, 1]) / 100,
      mm_assess_setting_sem_sc_hgn = (input$mm_assess_setting_sem[4, 1]) / 100,
      mm_assess_setting_sem_com_dia = (input$mm_assess_setting_sem[5, 1]) / 100,
      mm_assess_setting_sem_com_pha = (input$mm_assess_setting_sem[6, 1]) / 100,
      mm_assess_setting_sem_pc_gp_mins = input$mm_assess_setting_sem[1, 2],
      mm_assess_setting_sem_pc_nur_mins = input$mm_assess_setting_sem[2, 2],
      mm_assess_setting_sem_sc_hgc_mins = input$mm_assess_setting_sem[3, 2],
      mm_assess_setting_sem_sc_hgn_mins = input$mm_assess_setting_sem[4, 2],
      mm_assess_setting_sem_com_dia_mins = input$mm_assess_setting_sem[5, 2],
      mm_assess_setting_sem_com_pha_mins = input$mm_assess_setting_sem[6, 2],
      retention_sem_0_16 = input$retention_sem_0_16 / 100,
      treatment_setting_0_16_matrix_sem_pc_gp = (input$treatment_setting_0_16_matrix_sem[1, 1]) / 100,
      treatment_setting_0_16_matrix_sem_pc_nur = (input$treatment_setting_0_16_matrix_sem[2, 1]) / 100,
      treatment_setting_0_16_matrix_sem_sc_hgc = (input$treatment_setting_0_16_matrix_sem[3, 1]) / 100,
      treatment_setting_0_16_matrix_sem_sc_hgn = (input$treatment_setting_0_16_matrix_sem[4, 1]) / 100,
      treatment_setting_0_16_matrix_sem_com_dia = (input$treatment_setting_0_16_matrix_sem[5, 1]) / 100,
      treatment_setting_0_16_matrix_sem_com_pha = (input$treatment_setting_0_16_matrix_sem[6, 1]) / 100,
      treatment_setting_0_16_matrix_sem_pc_gp_mins = input$treatment_setting_0_16_matrix_sem[1, 2],
      treatment_setting_0_16_matrix_sem_pc_nur_mins = input$treatment_setting_0_16_matrix_sem[2, 2],
      treatment_setting_0_16_matrix_sem_sc_hgc_mins = input$treatment_setting_0_16_matrix_sem[3, 2],
      treatment_setting_0_16_matrix_sem_sc_hgn_mins = input$treatment_setting_0_16_matrix_sem[4, 2],
      treatment_setting_0_16_matrix_sem_com_dia_mins = input$treatment_setting_0_16_matrix_sem[5, 2],
      treatment_setting_0_16_matrix_sem_com_pha_mins = input$treatment_setting_0_16_matrix_sem[6, 2],
      appts_0_16_sem = input$appts_0_16_sem,
      monitoring_tests_number_sem = input$monitoring_tests_number_sem,
      monitoring_elf_prop_sem = input$monitoring_elf_prop_sem / 100,
      monitoring_bio_prop_sem = input$monitoring_bio_prop_sem / 100,
      dosage_retention_20_71_sem = input$dosage_retention_20_71_sem / 100,
      semaglutide_20_71_delivery_setting_pc_gp = (input$semaglutide_20_71_delivery_setting[1, 1] / 100),
      semaglutide_20_71_delivery_setting_pc_nur = (input$semaglutide_20_71_delivery_setting[2, 1] / 100),
      semaglutide_20_71_delivery_setting_sc_hgc = (input$semaglutide_20_71_delivery_setting[3, 1] / 100),
      semaglutide_20_71_delivery_setting_sc_hgn = (input$semaglutide_20_71_delivery_setting[4, 1] / 100),
      semaglutide_20_71_delivery_setting_com_dia = (input$semaglutide_20_71_delivery_setting[5, 1] / 100),
      semaglutide_20_71_delivery_setting_com_pha = (input$semaglutide_20_71_delivery_setting[6, 1] / 100),
      semaglutide_20_71_delivery_setting_pc_gp_mins = input$semaglutide_20_71_delivery_setting[1, 2],
      semaglutide_20_71_delivery_setting_pc_nur_mins = input$semaglutide_20_71_delivery_setting[2, 2],
      semaglutide_20_71_delivery_setting_sc_hgc_mins = input$semaglutide_20_71_delivery_setting[3, 2],
      semaglutide_20_71_delivery_setting_sc_hgn_mins = input$semaglutide_20_71_delivery_setting[4, 2],
      semaglutide_20_71_delivery_setting_com_dia_mins = input$semaglutide_20_71_delivery_setting[5, 2],
      semaglutide_20_71_delivery_setting_com_pha_mins = input$semaglutide_20_71_delivery_setting[6, 2],
      appts_20_71_sem = input$appts_20_71_sem,
      efficacy_liver_biopsy_prop_sem = input$efficacy_liver_biopsy_prop_sem / 100,
      efficacy_elf_prop_sem = input$efficacy_elf_prop_sem / 100,
      efficacy_fibro_prop_sem = input$efficacy_fibro_prop_surv / 100,
      efficacy_biomarkers_prop_sem = input$efficacy_biomarkers_prop_sem / 100,
      continuation_prop_sem = input$continuation_prop_sem / 100,
      continuation_delivery_setting_sem_pc_gp = (input$continuation_delivery_setting_sem[1, 1] / 100),
      continuation_delivery_setting_sem_pc_nur = (input$continuation_delivery_setting_sem[2, 1] / 100),
      continuation_delivery_setting_sem_sc_hgc = (input$continuation_delivery_setting_sem[3, 1] / 100),
      continuation_delivery_setting_sem_sc_hgn = (input$continuation_delivery_setting_sem[4, 1] / 100),
      continuation_delivery_setting_sem_com_dia = (input$continuation_delivery_setting_sem[5, 1] / 100),
      continuation_delivery_setting_sem_com_pha = (input$continuation_delivery_setting_sem[6, 1] / 100),
      retention_73_103_sem = input$retention_73_103_sem / 100,
      semaglutide_73_103_delivery_setting_pc_gp = (input$semaglutide_73_103_delivery_setting[1, 1]) / 100,
      semaglutide_73_103_delivery_setting_pc_nur = (input$semaglutide_73_103_delivery_setting[2, 1]) / 100,
      semaglutide_73_103_delivery_setting_sc_hgc = (input$semaglutide_73_103_delivery_setting[3, 1]) / 100,
      semaglutide_73_103_delivery_setting_sc_hgn = (input$semaglutide_73_103_delivery_setting[4, 1]) / 100,
      semaglutide_73_103_delivery_setting_com_dia = (input$semaglutide_73_103_delivery_setting[5, 1]) / 100,
      semaglutide_73_103_delivery_setting_com_pha = (input$semaglutide_73_103_delivery_setting[6, 1]) / 100,
      monitoring_tests_number_73_103_sem = input$monitoring_tests_number_73_103_sem,
      monitoring_tests_73_103_elf_sem = input$monitoring_tests_73_103_elf_sem / 100,
      ongoing_period_sem = input$ongoing_period_sem,
      retention_end_sem = input$retention_end_sem / 100,
      semaglutide_ongoing_delivery_setting_pc_gp = (input$semaglutide_ongoing_delivery_setting[1, 1]) / 100,
      semaglutide_ongoing_delivery_setting_pc_nur = (input$semaglutide_ongoing_delivery_setting[2, 1]) / 100,
      semaglutide_ongoing_delivery_setting_sc_hgc = (input$semaglutide_ongoing_delivery_setting[3, 1]) / 100,
      semaglutide_ongoing_delivery_setting_sc_hgn = (input$semaglutide_ongoing_delivery_setting[4, 1]) / 100,
      semaglutide_ongoing_delivery_setting_com_dia = (input$semaglutide_ongoing_delivery_setting[5, 1]) / 100,
      semaglutide_ongoing_delivery_setting_com_pha = (input$semaglutide_ongoing_delivery_setting[6, 1]) / 100
    )
    
  })
  
  surv_pathway_assumptions <- reactive({
    
    list(
      pre_liver_biopsy_prop_surv = input$pre_liver_biopsy_prop_surv / 100,
      pre_elf_prop_surv = input$pre_elf_prop_surv / 100,
      pre_biomarkers_prop_surv = input$pre_biomarkers_prop_surv / 100,
      pre_fibro_prop_surv = input$pre_fibro_prop_surv / 100,
      mm_assess_prop_surv = input$mm_assess_prop_surv / 100,
      mm_assess_setting_surv_pc_gp = (input$mm_assess_setting_surv[1, 1] / 100),
      mm_assess_setting_surv_pc_nur = (input$mm_assess_setting_surv[2, 1] / 100),
      mm_assess_setting_surv_sc_hgc = (input$mm_assess_setting_surv[3, 1] / 100),
      mm_assess_setting_surv_sc_hgn = (input$mm_assess_setting_surv[4, 1] / 100),
      mm_assess_setting_surv_com_dia = (input$mm_assess_setting_surv[5, 1] / 100),
      mm_assess_setting_surv_com_pha = (input$mm_assess_setting_surv[6, 1] / 100),
      mm_assess_setting_surv_pc_gp_mins = input$mm_assess_setting_surv[1, 2],
      mm_assess_setting_surv_pc_nur_mins = input$mm_assess_setting_surv[2, 2],
      mm_assess_setting_surv_sc_hgc_mins = input$mm_assess_setting_surv[3, 2],
      mm_assess_setting_surv_sc_hgn_mins = input$mm_assess_setting_surv[4, 2],
      mm_assess_setting_surv_com_dia_mins = input$mm_assess_setting_surv[5, 2],
      mm_assess_setting_surv_com_pha_mins = input$mm_assess_setting_surv[6, 2],
      retention_surv_0_24 = input$retention_surv_0_24 / 100,
      treatment_setting_0_24_matrix_surv_pc_gp = (input$treatment_setting_0_24_matrix_surv[1, 1]) / 100,
      treatment_setting_0_24_matrix_surv_pc_nur = (input$treatment_setting_0_24_matrix_surv[2, 1]) / 100,
      treatment_setting_0_24_matrix_surv_sc_hgc = (input$treatment_setting_0_24_matrix_surv[3, 1]) / 100,
      treatment_setting_0_24_matrix_surv_sc_hgn = (input$treatment_setting_0_24_matrix_surv[4, 1]) / 100,
      treatment_setting_0_24_matrix_surv_com_dia = (input$treatment_setting_0_24_matrix_surv[5, 1]) / 100,
      treatment_setting_0_24_matrix_surv_com_pha = (input$treatment_setting_0_24_matrix_surv[6, 1]) / 100,
      treatment_setting_0_24_matrix_surv_pc_gp_mins = input$treatment_setting_0_24_matrix_surv[1, 1],
      treatment_setting_0_24_matrix_surv_pc_nur_mins = input$treatment_setting_0_24_matrix_surv[2, 2],
      treatment_setting_0_24_matrix_surv_sc_hgc_mins = input$treatment_setting_0_24_matrix_surv[3, 2],
      treatment_setting_0_24_matrix_surv_sc_hgn_mins = input$treatment_setting_0_24_matrix_surv[4, 2],
      treatment_setting_0_24_matrix_surv_com_dia_mins = input$treatment_setting_0_24_matrix_surv[5, 2],
      treatment_setting_0_24_matrix_surv_com_pha_mins = input$treatment_setting_0_24_matrix_surv[6, 2],
      appts_0_24_surv = input$appts_0_24_surv,
      monitor_tests_0_71_num_surv = input$monitor_tests_0_71_num_surv,
      monitor_tests_0_71_elf_prop_surv = input$monitor_tests_0_71_elf_prop_surv / 100,
      monitor_tests_0_71_bio_prop_surv = input$monitor_tests_0_71_bio_prop_surv / 100,
      retention_25_71_surv = input$retention_25_71_surv / 100,
      treatment_setting_25_71_matrix_surv_pc_gp = (input$treatment_setting_25_71_matrix_surv[1, 1]) / 100,
      treatment_setting_25_71_matrix_surv_pc_nur = (input$treatment_setting_25_71_matrix_surv[2, 1]) / 100,
      treatment_setting_25_71_matrix_surv_sc_hgc = (input$treatment_setting_25_71_matrix_surv[3, 1]) / 100,
      treatment_setting_25_71_matrix_surv_sc_hgn = (input$treatment_setting_25_71_matrix_surv[4, 1]) / 100,
      treatment_setting_25_71_matrix_surv_com_dia = (input$treatment_setting_25_71_matrix_surv[5, 1]) / 100,
      treatment_setting_25_71_matrix_surv_com_pha = (input$treatment_setting_25_71_matrix_surv[6, 1]) / 100,
      treatment_setting_25_71_matrix_surv_pc_gp_mins = input$treatment_setting_25_71_matrix_surv[1, 2],
      treatment_setting_25_71_matrix_surv_pc_nur_mins = input$treatment_setting_25_71_matrix_surv[2, 2],
      treatment_setting_25_71_matrix_surv_sc_hgc_mins = input$treatment_setting_25_71_matrix_surv[3, 2],
      treatment_setting_25_71_matrix_surv_sc_hgn_mins = input$treatment_setting_25_71_matrix_surv[4, 2],
      treatment_setting_25_71_matrix_surv_com_dia_mins = input$treatment_setting_25_71_matrix_surv[5, 2],
      treatment_setting_25_71_matrix_surv_com_pha_mins = input$treatment_setting_25_71_matrix_surv[6, 2],
      efficacy_liver_biopsy_surv = input$efficacy_liver_biopsy_surv / 100,
      efficacy_elf_prop_surv = input$efficacy_elf_prop_surv / 100,
      efficacy_fibro_prop_surv = input$efficacy_fibro_prop_surv / 100,
      efficacy_biomarkers_prop_surv = input$efficacy_biomarkers_prop_surv / 100,
      continuation_prop_surv = input$continuation_prop_surv / 100,
      continuation_delivery_setting_surv_pc_gp = (input$continuation_delivery_setting_surv[1, 1]) / 100,
      continuation_delivery_setting_surv_pc_nur = (input$continuation_delivery_setting_surv[2, 1]) / 100,
      continuation_delivery_setting_surv_sc_hgc = (input$continuation_delivery_setting_surv[3, 1]) / 100,
      continuation_delivery_setting_surv_sc_hgn = (input$continuation_delivery_setting_surv[4, 1]) / 100,
      continuation_delivery_setting_surv_com_dia = (input$continuation_delivery_setting_surv[5, 1]) / 100,
      continuation_delivery_setting_surv_com_pha = (input$continuation_delivery_setting_surv[6, 1]) / 100,
      retention_73_103_surv = input$retention_73_103_surv / 100,
      treatment_setting_73_103_matrix_surv_pc_gp = (input$treatment_setting_73_103_matrix_surv[1, 1]) / 100,
      treatment_setting_73_103_matrix_surv_pc_nur = (input$treatment_setting_73_103_matrix_surv[2, 1]) / 100,
      treatment_setting_73_103_matrix_surv_sc_hgc = (input$treatment_setting_73_103_matrix_surv[3, 1]) / 100,
      treatment_setting_73_103_matrix_surv_sc_hgn = (input$treatment_setting_73_103_matrix_surv[4, 1]) / 100,
      treatment_setting_73_103_matrix_surv_com_dia = (input$treatment_setting_73_103_matrix_surv[5, 1]) / 100,
      treatment_setting_73_103_matrix_surv_com_pha = (input$treatment_setting_73_103_matrix_surv[6, 1]) / 100,
      monitoring_tests_number_73_103_surv = input$monitoring_tests_number_73_103_surv,
      monitoring_tests_73_103_elf_surv = input$monitoring_tests_73_103_elf_surv / 100,
      monitoring_tests_73_103_biomarkers_surv = input$monitoring_tests_73_103_biomarkers_surv / 100,
      ongoing_period_surv = input$ongoing_period_surv,
      retention_end_surv = input$retention_end_surv / 100,
      survodutide_ongoing_delivery_setting_pc_gp = (input$survodutide_ongoing_delivery_setting[1, 1]) / 100,
      survodutide_ongoing_delivery_setting_pc_nur = (input$survodutide_ongoing_delivery_setting[2, 1]) / 100,
      survodutide_ongoing_delivery_setting_sc_hgc = (input$survodutide_ongoing_delivery_setting[3, 1]) / 100,
      survodutide_ongoing_delivery_setting_sc_hgn = (input$survodutide_ongoing_delivery_setting[4, 1]) / 100,
      survodutide_ongoing_delivery_setting_com_dia = (input$survodutide_ongoing_delivery_setting[5, 1]) / 100,
      survodutide_ongoing_delivery_setting_com_pha = (input$survodutide_ongoing_delivery_setting[6, 1]) / 100,
      ongoing_annual_prop_elf_surv = input$ongoing_annual_prop_elf_surv / 100,
      ongoing_annual_prop_biomarkers_surv = input$ongoing_annual_prop_biomarkers_surv / 100,
      ongoing_annual_prop_fibro_surv = input$ongoing_annual_prop_fibro_surv
    )
    
  })
  
  res_pathway_assumptions <- reactive({
    
    list(
      pre_liver_biopsy_prop_res = input$pre_liver_biopsy_prop_res / 100,
      pre_elf_prop_res = input$pre_elf_prop_res / 100,
      pre_biomarkers_prop_res = input$pre_biomarkers_prop_res / 100,
      pre_fibro_prop_res = input$pre_fibro_prop_res / 100,
      mm_assess_prop_res = input$mm_assess_prop_res / 100,
      mm_assess_setting_res_pc_gp = (input$mm_assess_setting_res[1, 1]) / 100,
      mm_assess_setting_res_pc_nur = (input$mm_assess_setting_res[2, 1]) / 100,
      mm_assess_setting_res_sc_hgc = (input$mm_assess_setting_res[3, 1]) / 100,
      mm_assess_setting_res_sc_hgn = (input$mm_assess_setting_res[4, 1]) / 100,
      mm_assess_setting_res_com_dia = (input$mm_assess_setting_res[5, 1]) / 100,
      mm_assess_setting_res_com_pha = (input$mm_assess_setting_res[6, 1]) / 100,
      mm_assess_setting_res_pc_gp_mins = input$mm_assess_setting_res[1, 2],
      mm_assess_setting_res_pc_nur_mins = input$mm_assess_setting_res[2, 2],
      mm_assess_setting_res_sc_hgc_mins = input$mm_assess_setting_res[3, 2],
      mm_assess_setting_res_sc_hgn_mins = input$mm_assess_setting_res[4, 2],
      mm_assess_setting_res_com_dia_mins = input$mm_assess_setting_res[5, 2],
      mm_assess_setting_res_com_pha_mins = input$mm_assess_setting_res[6, 2],
      dosage_0_71_res = input$dosage_0_71_res / 100,
      retention_res_0_71 = input$retention_res_0_71 / 100,
      treatment_setting_0_71_matrix_res_pc_gp = (input$treatment_setting_0_71_matrix_res[1, 1]) / 100,
      treatment_setting_0_71_matrix_res_pc_nur = (input$treatment_setting_0_71_matrix_res[2, 1]) / 100,
      treatment_setting_0_71_matrix_res_sc_hgc = (input$treatment_setting_0_71_matrix_res[3, 1]) / 100,
      treatment_setting_0_71_matrix_res_sc_hgn = (input$treatment_setting_0_71_matrix_res[4, 1]) / 100,
      treatment_setting_0_71_matrix_res_com_dia = (input$treatment_setting_0_71_matrix_res[5, 1]) / 100,
      treatment_setting_0_71_matrix_res_com_pha = (input$treatment_setting_0_71_matrix_res[6, 1]) / 100,
      monitor_tests_0_71_res = input$monitor_tests_0_71_res,
      monitor_tests_0_71_elf_res = input$monitor_tests_0_71_elf_res / 100,
      monitor_tests_0_71_biomarkers_res = input$monitor_tests_0_71_biomarkers_res / 100,
      monitor_tests_0_71_fibro_res = input$monitor_tests_0_71_fibro_res / 100,
      efficacy_liver_biopsy_res = input$efficacy_liver_biopsy_res / 100,
      efficacy_elf_prop_res = input$efficacy_elf_prop_res / 100,
      efficacy_fibro_prop_res = input$efficacy_fibro_prop_res / 100,
      efficacy_biomarkers_prop_res = input$efficacy_biomarkers_prop_res / 100,
      continuation_prop_res = input$continuation_prop_res / 100,
      continuation_delivery_setting_res_pc_gp = (input$continuation_delivery_setting_res[1, 1]) / 100,
      continuation_delivery_setting_res_pc_nur = (input$continuation_delivery_setting_res[2, 1]) / 100,
      continuation_delivery_setting_res_sc_hgc = (input$continuation_delivery_setting_res[3, 1]) / 100,
      continuation_delivery_setting_res_sc_hgn = (input$continuation_delivery_setting_res[4, 1]) / 100,
      continuation_delivery_setting_res_com_dia = (input$continuation_delivery_setting_res[5, 1]) / 100,
      continuation_delivery_setting_res_com_pha = (input$continuation_delivery_setting_res[6, 1]) / 100,
      retention_73_103_res = input$retention_73_103_res / 100,
      treatment_setting_73_103_matrix_res_pc_gp = (input$treatment_setting_73_103_matrix_res[1, 1]) / 100,
      treatment_setting_73_103_matrix_res_pc_nur = (input$treatment_setting_73_103_matrix_res[2, 1]) / 100,
      treatment_setting_73_103_matrix_res_sc_hgc = (input$treatment_setting_73_103_matrix_res[3, 1]) / 100,
      treatment_setting_73_103_matrix_res_sc_hgn = (input$treatment_setting_73_103_matrix_res[4, 1]) / 100,
      treatment_setting_73_103_matrix_res_com_dia = (input$treatment_setting_73_103_matrix_res[5, 1]) / 100,
      treatment_setting_73_103_matrix_res_com_pha = (input$treatment_setting_73_103_matrix_res[6, 1]) / 100,
      monitoring_tests_number_73_103_res = input$monitoring_tests_number_73_103_res,
      monitoring_tests_73_103_elf_res = input$monitoring_tests_73_103_elf_res / 100,
      monitoring_tests_73_103_biomarkers_res = input$monitoring_tests_73_103_biomarkers_res / 100,
      ongoing_period_res = input$ongoing_period_res,
      retention_end_res = input$retention_end_res / 100,
      resmetirom_ongoing_delivery_setting_pc_gp = (input$resmetirom_ongoing_delivery_setting[1, 1]) / 100,
      resmetirom_ongoing_delivery_setting_pc_nur = (input$resmetirom_ongoing_delivery_setting[2, 1]) / 100,
      resmetirom_ongoing_delivery_setting_sc_hgc = (input$resmetirom_ongoing_delivery_setting[3, 1]) / 100,
      resmetirom_ongoing_delivery_setting_sc_hgn = (input$resmetirom_ongoing_delivery_setting[4, 1]) / 100,
      resmetirom_ongoing_delivery_setting_com_dia = (input$resmetirom_ongoing_delivery_setting[5, 1]) / 100,
      resmetirom_ongoing_delivery_setting_com_pha = (input$resmetirom_ongoing_delivery_setting[6, 1]) / 100,
      ongoing_annual_prop_elf_res = input$ongoing_annual_prop_elf_res / 100,
      ongoing_annual_prop_biomarkers_res = input$ongoing_annual_prop_biomarkers_res / 100,
      ongoing_annual_prop_fibro_res = input$ongoing_annual_prop_fibro_res / 100
    )
    
  })
  
  lan_pathway_assumptions <- reactive({
    
    list(
      pre_liver_biopsy_prop_lan = input$pre_liver_biopsy_prop_lan / 100,
      pre_elf_prop_lan = input$pre_elf_prop_lan / 100,
      pre_biomarkers_prop_lan = input$pre_biomarkers_prop_lan / 100,
      pre_fibro_prop_lan = input$pre_fibro_prop_lan / 100,
      mm_assess_prop_lan = input$mm_assess_prop_lan / 100,
      mm_assess_setting_lan_pc_gp = (input$mm_assess_setting_lan[1, 1]) / 100,
      mm_assess_setting_lan_pc_nur = (input$mm_assess_setting_lan[2, 1]) / 100,
      mm_assess_setting_lan_sc_hgc = (input$mm_assess_setting_lan[3, 1]) / 100,
      mm_assess_setting_lan_sc_hgn = (input$mm_assess_setting_lan[4, 1]) / 100,
      mm_assess_setting_lan_com_dia = (input$mm_assess_setting_lan[5, 1]) / 100,
      mm_assess_setting_lan_com_pha = (input$mm_assess_setting_lan[6, 1]) / 100,
      mm_assess_setting_lan_pc_gp_mins = input$mm_assess_setting_lan[1, 2],
      mm_assess_setting_lan_pc_nur_mins = input$mm_assess_setting_lan[2, 2],
      mm_assess_setting_lan_sc_hgc_mins = input$mm_assess_setting_lan[3, 2],
      mm_assess_setting_lan_sc_hgn_mins = input$mm_assess_setting_lan[4, 2],
      mm_assess_setting_lan_com_dia_mins = input$mm_assess_setting_lan[5, 2],
      mm_assess_setting_lan_com_pha_mins = input$mm_assess_setting_lan[6, 2],
      dosage_0_71_lan = input$dosage_0_71_lan / 100,
      retention_lan_0_71 = input$retention_lan_0_71 / 100,
      treatment_setting_0_71_matrix_lan_pc_gp = (input$treatment_setting_0_71_matrix_lan[1, 1]) / 100,
      treatment_setting_0_71_matrix_lan_pc_nur = (input$treatment_setting_0_71_matrix_lan[2, 1]) / 100,
      treatment_setting_0_71_matrix_lan_sc_hgc = (input$treatment_setting_0_71_matrix_lan[3, 1]) / 100,
      treatment_setting_0_71_matrix_lan_sc_hgn = (input$treatment_setting_0_71_matrix_lan[4, 1]) / 100,
      treatment_setting_0_71_matrix_lan_com_dia = (input$treatment_setting_0_71_matrix_lan[5, 1]) / 100,
      treatment_setting_0_71_matrix_lan_com_pha = (input$treatment_setting_0_71_matrix_lan[6, 1]) / 100,
      monitor_tests_0_71_lan = input$monitor_tests_0_71_lan,
      monitor_tests_0_71_elf_lan = input$monitor_tests_0_71_elf_lan / 100,
      monitor_tests_0_71_biomarkers_lan = input$monitor_tests_0_71_biomarkers_lan / 100,
      monitor_tests_0_71_fibro_lan = input$monitor_tests_0_71_fibro_lan / 100,
      efficacy_liver_biopsy_lan = input$efficacy_liver_biopsy_lan / 100,
      efficacy_elf_prop_lan = input$efficacy_elf_prop_lan / 100,
      efficacy_fibro_prop_lan = input$efficacy_fibro_prop_lan / 100,
      efficacy_biomarkers_prop_lan = input$efficacy_biomarkers_prop_lan / 100,
      continuation_prop_lan = input$continuation_prop_lan / 100,
      continuation_delivery_setting_lan_pc_gp = (input$continuation_delivery_setting_lan[1, 1]) / 100,
      continuation_delivery_setting_lan_pc_nur = (input$continuation_delivery_setting_lan[2, 1]) / 100,
      continuation_delivery_setting_lan_sc_hgc = (input$continuation_delivery_setting_lan[3, 1]) / 100,
      continuation_delivery_setting_lan_sc_hgn = (input$continuation_delivery_setting_lan[4, 1]) / 100,
      continuation_delivery_setting_lan_com_dia = (input$continuation_delivery_setting_lan[5, 1]) / 100,
      continuation_delivery_setting_lan_com_pha = (input$continuation_delivery_setting_lan[6, 1]) / 100,
      retention_73_103_lan = input$retention_73_103_lan / 100,
      treatment_setting_73_103_matrix_lan_pc_gp = (input$treatment_setting_73_103_matrix_lan[1, 1]) / 100,
      treatment_setting_73_103_matrix_lan_pc_nur = (input$treatment_setting_73_103_matrix_lan[2, 1]) / 100,
      treatment_setting_73_103_matrix_lan_sc_hgc = (input$treatment_setting_73_103_matrix_lan[3, 1]) / 100,
      treatment_setting_73_103_matrix_lan_sc_hgn = (input$treatment_setting_73_103_matrix_lan[4, 1]) / 100,
      treatment_setting_73_103_matrix_lan_com_dia = (input$treatment_setting_73_103_matrix_lan[5, 1]) / 100,
      treatment_setting_73_103_matrix_lan_com_pha = (input$treatment_setting_73_103_matrix_lan[6, 1]) / 100,
      monitoring_tests_number_73_103_lan = input$monitoring_tests_number_73_103_lan,
      monitoring_tests_73_103_elf_lan = input$monitoring_tests_73_103_elf_lan / 100,
      monitoring_tests_73_103_biomarkers_lan = input$monitoring_tests_73_103_biomarkers_lan / 100,
      ongoing_period_lan = input$ongoing_period_lan,
      retention_end_lan = input$retention_end_lan / 100,
      lanifibranor_ongoing_delivery_setting_pc_gp = (input$lan_ongoing_delivery_setting[1, 1]) / 100,
      lanifibranor_ongoing_delivery_setting_pc_nur = (input$lan_ongoing_delivery_setting[2, 1]) / 100,
      lanifibranor_ongoing_delivery_setting_sc_hgc = (input$lan_ongoing_delivery_setting[3, 1]) / 100,
      lanifibranor_ongoing_delivery_setting_sc_hgn = (input$lan_ongoing_delivery_setting[4, 1]) / 100,
      lanifibranor_ongoing_delivery_setting_com_dia = (input$lan_ongoing_delivery_setting[5, 1]) / 100,
      lanifibranor_ongoing_delivery_setting_com_pha = (input$lan_ongoing_delivery_setting[6, 1]) / 100,
      ongoing_annual_prop_elf_lan = input$ongoing_annual_prop_elf_lan / 100,
      ongoing_annual_prop_biomarkers_lan = input$ongoing_annual_prop_biomarkers_lan / 100,
      ongoing_annual_prop_fibro_lan = input$ongoing_annual_prop_fibro_lan / 100
    )
    
  })
  
  fin_assumptions <- reactive({
    
    list(
      fin_liv_bio = input$fin_liv_bio,
      fin_elf = input$fin_elf,
      fin_biomarkers = input$fin_biomarkers,
      fin_fibro = input$fin_fibro,
      fin_appt_pc_gp_pph = input$fin_appt_pc_gp_pph,
      fin_appt_pc_nur_pph = input$fin_appt_pc_nur_pph,
      fin_appt_sc_hgc_pph = input$fin_appt_sc_hgc_pph,
      fin_appt_sc_hgn_pph = input$fin_appt_sc_hgn_pph,
      fin_appt_com_dia_pph = input$fin_appt_com_dia_pph,
      fin_appt_com_pha_pph = input$fin_appt_com_pha_pph
      
    )
  })
  

# Population Logic --------------------------------------------------------


  
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
  
  treat_pop_surv <- reactive({
    params <- treat_imp_assumptions()
    f_stage_diagnosed() |>
      mutate("F0_treat" = round(F0_diag * params$treat_prop_F0_surv, 0),
             "F1_treat" = round(F1_diag * params$treat_prop_F1_surv, 0),
             "F2_treat" = round(F2_diag * params$treat_prop_F2_surv, 0),
             "F3_treat" = round(F3_diag * params$treat_prop_F3_surv, 0),
             "F4_treat" = round(F4_diag * params$treat_prop_F4_surv, 0)) |>
      select(-c(F0_diag, F1_diag, F2_diag, F3_diag, F4_diag)) |>
      rowwise() |>
      mutate("treated_total" = sum(c_across(F0_treat:F4_treat)))
    
  })
  
  treat_pop_res <- reactive({
    params <- treat_imp_assumptions()
    f_stage_diagnosed() |>
      mutate("F0_treat" = round(F0_diag * params$treat_prop_F0_res, 0),
             "F1_treat" = round(F1_diag * params$treat_prop_F1_res, 0),
             "F2_treat" = round(F2_diag * params$treat_prop_F2_res, 0),
             "F3_treat" = round(F3_diag * params$treat_prop_F3_res, 0),
             "F4_treat" = round(F4_diag * params$treat_prop_F4_res, 0)) |>
      select(-c(F0_diag, F1_diag, F2_diag, F3_diag, F4_diag)) |>
      rowwise() |>
      mutate("treated_total" = sum(c_across(F0_treat:F4_treat)))
    
  })
  
  treat_pop_lan <- reactive({
    params <- treat_imp_assumptions()
    f_stage_diagnosed() |>
      mutate("F0_treat" = round(F0_diag * params$treat_prop_F0_lan, 0),
             "F1_treat" = round(F1_diag * params$treat_prop_F1_lan, 0),
             "F2_treat" = round(F2_diag * params$treat_prop_F2_lan, 0),
             "F3_treat" = round(F3_diag * params$treat_prop_F3_lan, 0),
             "F4_treat" = round(F4_diag * params$treat_prop_F4_lan, 0)) |>
      select(-c(F0_diag, F1_diag, F2_diag, F3_diag, F4_diag)) |>
      rowwise() |>
      mutate("treated_total" = sum(c_across(F0_treat:F4_treat)))
    
  })
  

# Pre-Treatment: Biopsies -------------------------------------------------


  pre_treat_biopsy_sem <- reactive({
    
    params_sem <- sem_pathway_assumptions()
    params_fin <- fin_assumptions()
    treat_pop_sem() |>
      select(c(simulation, treated_total)) |>
      mutate(liv_bio_act = round(treated_total * params_sem$pre_liver_biopsy_prop_sem, 0),
             liv_bio_act_cost = round(liv_bio_act * params_fin$fin_liv_bio, 2))
    
  })
  
  pre_treat_biopsy_surv <- reactive({
    
    params_surv <- surv_pathway_assumptions()
    params_fin <- fin_assumptions()
    treat_pop_surv() |>
      select(c(simulation, treated_total)) |>
      mutate(liv_bio_act = round(treated_total * params_surv$pre_liver_biopsy_prop_surv, 0),
             liv_bio_act_cost = round(liv_bio_act * params_fin$fin_liv_bio, 2))
    
  })
  
  pre_treat_biopsy_res <- reactive({
    
    params_res <- res_pathway_assumptions()
    params_fin <- fin_assumptions()
    treat_pop_res() |>
      select(c(simulation, treated_total)) |>
      mutate(liv_bio_act = round(treated_total * params_res$pre_liver_biopsy_prop_res, 0),
             liv_bio_act_cost = round(liv_bio_act * params_fin$fin_liv_bio, 2))
    
  })
  
  pre_treat_biopsy_lan <- reactive({
    
    params_lan <- lan_pathway_assumptions()
    params_fin <- fin_assumptions()
    treat_pop_lan() |>
      select(c(simulation, treated_total)) |>
      mutate(liv_bio_act = round(treated_total * params_lan$pre_liver_biopsy_prop_lan, 0),
             liv_bio_act_cost = round(liv_bio_act * params_fin$fin_liv_bio, 2))
    
  })
  
  pre_treat_biopsy_all <- reactive({
    
    pre_treat_biopsy_sem_all <- pre_treat_biopsy_sem() |>
      select(c(simulation, liv_bio_act, liv_bio_act_cost)) |>
      rename("liv_bio_act_sem" = 2,
             "liv_bio_act_cost_sem" = 3)
    
    pre_treat_biopsy_surv_all <- pre_treat_biopsy_surv() |>
      select(c(simulation, liv_bio_act, liv_bio_act_cost)) |>
      rename("liv_bio_act_surv" = 2,
             "liv_bio_act_cost_surv" = 3)
    
    pre_treat_biopsy_res_all <- pre_treat_biopsy_res() |>
      select(c(simulation, liv_bio_act, liv_bio_act_cost)) |>
      rename("liv_bio_act_res" = 2,
             "liv_bio_act_cost_res" = 3)
    
    pre_treat_biopsy_lan_all <- pre_treat_biopsy_lan() |>
      select(c(simulation, liv_bio_act, liv_bio_act_cost)) |>
      rename("liv_bio_act_lan" = 2,
             "liv_bio_act_cost_lan" = 3)
    
    pre_treat_biopsy_sem_all|>
      left_join(pre_treat_biopsy_surv_all, by = c("simulation")) |>
      left_join(pre_treat_biopsy_res_all, by = c("simulation")) |>
      left_join(pre_treat_biopsy_lan_all, by = c("simulation")) |>
      rowwise() |>
      mutate("liv_bio_act_all" = sum(c_across(c(liv_bio_act_sem,
                                                liv_bio_act_surv,
                                                liv_bio_act_res,
                                                liv_bio_act_lan))),
             "liv_bio_act_cost_all" = sum(c_across(c(liv_bio_act_cost_sem,
                                                     liv_bio_act_cost_surv,
                                                     liv_bio_act_cost_res,
                                                     liv_bio_act_cost_lan)))
      )
    
  })
  

# Pre-Treatment: ELF ------------------------------------------------------

  
  pre_treat_elf_sem <- reactive({
    
    params_sem <- sem_pathway_assumptions()
    params_fin <- fin_assumptions()
    treat_pop_sem() |>
      select(c(simulation, treated_total)) |>
      mutate(elf_act = round(treated_total * params_sem$pre_elf_prop_sem, 0),
             elf_cost = round(elf_act * params_fin$fin_elf, 2))
    
  })
  
  pre_treat_elf_surv <- reactive({
    
    params_surv <- surv_pathway_assumptions()
    params_fin <- fin_assumptions()
    treat_pop_surv() |>
      select(c(simulation, treated_total)) |>
      mutate(elf_act = round(treated_total * params_surv$pre_elf_prop_surv, 0),
             elf_cost = round(elf_act * params_fin$fin_elf, 2))
    
  })
  
  pre_treat_elf_res <- reactive({
    
    params_res <- res_pathway_assumptions()
    params_fin <- fin_assumptions()
    treat_pop_res() |>
      select(c(simulation, treated_total)) |>
      mutate(elf_act = round(treated_total * params_res$pre_elf_prop_res, 0),
             elf_cost = round(elf_act * params_fin$fin_elf, 2))
    
  })
  
  pre_treat_elf_lan <- reactive({
    
    params_lan <- lan_pathway_assumptions()
    params_fin <- fin_assumptions()
    treat_pop_lan() |>
      select(c(simulation, treated_total)) |>
      mutate(elf_act = round(treated_total * params_lan$pre_elf_prop_lan, 0),
             elf_cost = round(elf_act * params_fin$fin_elf, 2))
    
  })
  
  pre_treat_elf_all <- reactive({
    
    pre_treat_elf_sem_all <- pre_treat_elf_sem() |>
      select(c(simulation, elf_act, elf_cost)) |>
      rename("elf_act_sem" = 2,
             "elf_act_cost_sem" = 3)
    
    pre_treat_elf_surv_all <- pre_treat_elf_surv() |>
      select(c(simulation, elf_act, elf_cost)) |>
      rename("elf_act_surv" = 2,
             "elf_act_cost_surv" = 3)
    
    pre_treat_elf_res_all <- pre_treat_elf_res() |>
      select(c(simulation, elf_act, elf_cost)) |>
      rename("elf_act_res" = 2,
             "elf_act_cost_res" = 3)
    
    pre_treat_elf_lan_all <- pre_treat_elf_lan() |>
      select(c(simulation, elf_act, elf_cost)) |>
      rename("elf_act_lan" = 2,
             "elf_act_cost_lan" = 3)
    
    pre_treat_elf_sem_all|>
      left_join(pre_treat_elf_surv_all, by = c("simulation")) |>
      left_join(pre_treat_elf_res_all, by = c("simulation")) |>
      left_join(pre_treat_elf_lan_all, by = c("simulation")) |>
      rowwise() |>
      mutate("elf_act_all" = sum(c_across(c(elf_act_sem,
                                            elf_act_surv,
                                            elf_act_res,
                                            elf_act_lan))),
             "elf_act_cost_all" = sum(c_across(c(elf_act_cost_sem,
                                                 elf_act_cost_surv,
                                                 elf_act_cost_res,
                                                 elf_act_cost_lan)))
      )
    
  })


# Pre-Treatment: Biomarkers -----------------------------------------------

  pre_treat_biomarkers_sem <- reactive({
    
    params_sem <- sem_pathway_assumptions()
    params_fin <- fin_assumptions()
    treat_pop_sem() |>
      select(c(simulation, treated_total)) |>
      mutate(biomarkers_act = round(treated_total * params_sem$pre_biomarkers_prop_sem, 0),
             biomarkers_cost = round(biomarkers_act * params_fin$fin_biomarkers, 2))
    
  })
  
  pre_treat_biomarkers_surv <- reactive({
    
    params_surv <- surv_pathway_assumptions()
    params_fin <- fin_assumptions()
    treat_pop_surv() |>
      select(c(simulation, treated_total)) |>
      mutate(biomarkers_act = round(treated_total * params_surv$pre_biomarkers_prop_surv, 0),
             biomarkers_cost = round(biomarkers_act * params_fin$fin_biomarkers, 2))
    
  })
  
  pre_treat_biomarkers_res <- reactive({
    
    params_res <- res_pathway_assumptions()
    params_fin <- fin_assumptions()
    treat_pop_res() |>
      select(c(simulation, treated_total)) |>
      mutate(biomarkers_act = round(treated_total * params_res$pre_biomarkers_prop_res, 0),
             biomarkers_cost = round(biomarkers_act * params_fin$fin_biomarkers, 2))
    
  })
  
  pre_treat_biomarkers_lan <- reactive({
    
    params_lan <- lan_pathway_assumptions()
    params_fin <- fin_assumptions()
    treat_pop_lan() |>
      select(c(simulation, treated_total)) |>
      mutate(biomarkers_act = round(treated_total * params_lan$pre_biomarkers_prop_lan, 0),
             biomarkers_cost = round(biomarkers_act * params_fin$fin_biomarkers, 2))
    
  })
  
  pre_treat_biomarkers_all <- reactive({
    
    pre_treat_biomarkers_sem_all <- pre_treat_biomarkers_sem() |>
      select(c(simulation, biomarkers_act, biomarkers_cost)) |>
      rename("biomarkers_act_sem" = 2,
             "biomarkers_act_cost_sem" = 3)
    
    pre_treat_biomarkers_surv_all <- pre_treat_biomarkers_surv() |>
      select(c(simulation, biomarkers_act, biomarkers_cost)) |>
      rename("biomarkers_act_surv" = 2,
             "biomarkers_act_cost_surv" = 3)
    
    pre_treat_biomarkers_res_all <- pre_treat_biomarkers_res() |>
      select(c(simulation, biomarkers_act, biomarkers_cost)) |>
      rename("biomarkers_act_res" = 2,
             "biomarkers_act_cost_res" = 3)
    
    pre_treat_biomarkers_lan_all <- pre_treat_biomarkers_lan() |>
      select(c(simulation, biomarkers_act, biomarkers_cost)) |>
      rename("biomarkers_act_lan" = 2,
             "biomarkers_act_cost_lan" = 3)
    
    pre_treat_biomarkers_sem_all|>
      left_join(pre_treat_biomarkers_surv_all, by = c("simulation")) |>
      left_join(pre_treat_biomarkers_res_all, by = c("simulation")) |>
      left_join(pre_treat_biomarkers_lan_all, by = c("simulation")) |>
      rowwise() |>
      mutate("biomarkers_act_all" = sum(c_across(c(biomarkers_act_sem,
                                                   biomarkers_act_surv,
                                                   biomarkers_act_res,
                                                   biomarkers_act_lan))),
             "biomarkers_act_cost_all" = sum(c_across(c(biomarkers_act_cost_sem,
                                                        biomarkers_act_cost_surv,
                                                        biomarkers_act_cost_res,
                                                        biomarkers_act_cost_lan)))
      )
    
  })
  

# Pre-Treatment: Fibroscans -----------------------------------------------

  pre_treat_fibro_sem <- reactive({
    
    params_sem <- sem_pathway_assumptions()
    params_fin <- fin_assumptions()
    treat_pop_sem() |>
      select(c(simulation, treated_total)) |>
      mutate(fibro_act = round(treated_total * params_sem$pre_fibro_prop_sem, 0),
             fibro_cost = round(fibro_act * params_fin$fin_fibro, 2))
    
  })
  
  pre_treat_fibro_surv <- reactive({
    
    params_surv <- surv_pathway_assumptions()
    params_fin <- fin_assumptions()
    treat_pop_surv() |>
      select(c(simulation, treated_total)) |>
      mutate(fibro_act = round(treated_total * params_surv$pre_fibro_prop_surv, 0),
             fibro_cost = round(fibro_act * params_fin$fin_fibro, 2))
    
  })
  
  pre_treat_fibro_res <- reactive({
    
    params_res <- res_pathway_assumptions()
    params_fin <- fin_assumptions()
    treat_pop_res() |>
      select(c(simulation, treated_total)) |>
      mutate(fibro_act = round(treated_total * params_res$pre_fibro_prop_res, 0),
             fibro_cost = round(fibro_act * params_fin$fin_fibro, 2))
    
  })
  
  pre_treat_fibro_lan <- reactive({
    
    params_lan <- lan_pathway_assumptions()
    params_fin <- fin_assumptions()
    treat_pop_lan() |>
      select(c(simulation, treated_total)) |>
      mutate(fibro_act = round(treated_total * params_lan$pre_fibro_prop_lan, 0),
             fibro_cost = round(fibro_act * params_fin$fin_fibro, 2))
    
  })
  
  pre_treat_fibro_all <- reactive({
    
    pre_treat_fibro_sem_all <- pre_treat_fibro_sem() |>
      select(c(simulation, fibro_act, fibro_cost)) |>
      rename("fibro_act_sem" = 2,
             "fibro_act_cost_sem" = 3)
    
    pre_treat_fibro_surv_all <- pre_treat_fibro_surv() |>
      select(c(simulation, fibro_act, fibro_cost)) |>
      rename("fibro_act_surv" = 2,
             "fibro_act_cost_surv" = 3)
    
    pre_treat_fibro_res_all <- pre_treat_fibro_res() |>
      select(c(simulation, fibro_act, fibro_cost)) |>
      rename("fibro_act_res" = 2,
             "fibro_act_cost_res" = 3)
    
    pre_treat_fibro_lan_all <- pre_treat_fibro_lan() |>
      select(c(simulation, fibro_act, fibro_cost)) |>
      rename("fibro_act_lan" = 2,
             "fibro_act_cost_lan" = 3)
    
    pre_treat_fibro_sem_all|>
      left_join(pre_treat_fibro_surv_all, by = c("simulation")) |>
      left_join(pre_treat_fibro_res_all, by = c("simulation")) |>
      left_join(pre_treat_fibro_lan_all, by = c("simulation")) |>
      rowwise() |>
      mutate("fibro_act_all" = sum(c_across(c(fibro_act_sem,
                                              fibro_act_surv,
                                              fibro_act_res,
                                              fibro_act_lan))),
             "fibro_act_cost_all" = sum(c_across(c(fibro_act_cost_sem,
                                                   fibro_act_cost_surv,
                                                   fibro_act_cost_res,
                                                   fibro_act_cost_lan)))
      )
    
  }) 
  

# Pre-Treatment: MM Assessment --------------------------------------------

  pre_treat_mm_assess_sem <- reactive({
    
    params_sem <- sem_pathway_assumptions()
    params_fin <- fin_assumptions()
    treat_pop_sem() |>
      select(c(simulation, treated_total)) |>
      mutate(mm_act = round(treated_total * params_sem$mm_assess_prop_sem, 0),
             mm_act_pc_gp = round(mm_act * params_sem$mm_assess_setting_sem_pc_gp, 0),
             mm_act_pc_nur = round(mm_act * params_sem$mm_assess_setting_sem_pc_nur, 0),
             mm_act_sc_hgc = round(mm_act * params_sem$mm_assess_setting_sem_sc_hgc, 0),
             mm_act_sc_hgn = round(mm_act * params_sem$mm_assess_setting_sem_sc_hgn, 0),
             mm_act_com_dia = round(mm_act * params_sem$mm_assess_setting_sem_com_dia, 0),
             mm_act_com_pha = round(mm_act * params_sem$mm_assess_setting_sem_com_pha, 0),
             mm_act_pc_gp_cost = round(mm_act_pc_gp * (params_sem$mm_assess_setting_sem_pc_gp_mins / 60) * params_fin$fin_appt_pc_gp_pph, 2),
             mm_act_pc_nur_cost = round(mm_act_pc_nur * (params_sem$mm_assess_setting_sem_pc_nur_mins / 60) * params_fin$fin_appt_pc_nur_pph, 2),
             mm_act_sc_hgc_cost = round(mm_act_sc_hgc * (params_sem$mm_assess_setting_sem_sc_hgc_mins / 60) * params_fin$fin_appt_sc_hgc_pph, 2),
             mm_act_sc_hgn_cost = round(mm_act_sc_hgn * (params_sem$mm_assess_setting_sem_sc_hgn_mins / 60) * params_fin$fin_appt_sc_hgn_pph, 2),
             mm_act_com_dia_cost = round(mm_act_com_dia * (params_sem$mm_assess_setting_sem_com_dia_mins / 60) * params_fin$fin_appt_com_dia_pph, 2),
             mm_act_com_pha_cost = round(mm_act_com_pha * (params_sem$mm_assess_setting_sem_com_pha_mins / 60) * params_fin$fin_appt_com_pha_pph, 2)
      ) |>
      rowwise() |>
      mutate(mm_act_cost_total = sum(c_across(c(mm_act_pc_gp_cost,
                                                mm_act_pc_nur_cost,
                                                mm_act_sc_hgc_cost,
                                                mm_act_sc_hgn_cost,
                                                mm_act_com_dia_cost,
                                                mm_act_com_pha_cost)))
      )
    
  })
  
  pre_treat_mm_assess_surv <- reactive({
    
    params_surv <- surv_pathway_assumptions()
    params_fin <- fin_assumptions()
    treat_pop_surv() |>
      select(c(simulation, treated_total)) |>
      mutate(mm_act = round(treated_total * params_surv$mm_assess_prop_surv, 0),
             mm_act_pc_gp = round(mm_act * params_surv$mm_assess_setting_surv_pc_gp, 0),
             mm_act_pc_nur = round(mm_act * params_surv$mm_assess_setting_surv_pc_nur, 0),
             mm_act_sc_hgc = round(mm_act * params_surv$mm_assess_setting_surv_sc_hgc, 0),
             mm_act_sc_hgn = round(mm_act * params_surv$mm_assess_setting_surv_sc_hgn, 0),
             mm_act_com_dia = round(mm_act * params_surv$mm_assess_setting_surv_com_dia, 0),
             mm_act_com_pha = round(mm_act * params_surv$mm_assess_setting_surv_com_pha, 0),
             mm_act_pc_gp_cost = round(mm_act_pc_gp * (params_surv$mm_assess_setting_surv_pc_gp_mins / 60) * params_fin$fin_appt_pc_gp_pph, 2),
             mm_act_pc_nur_cost = round(mm_act_pc_nur * (params_surv$mm_assess_setting_surv_pc_nur_mins / 60) * params_fin$fin_appt_pc_nur_pph, 2),
             mm_act_sc_hgc_cost = round(mm_act_sc_hgc * (params_surv$mm_assess_setting_surv_sc_hgc_mins / 60) * params_fin$fin_appt_sc_hgc_pph, 2),
             mm_act_sc_hgn_cost = round(mm_act_sc_hgn * (params_surv$mm_assess_setting_surv_sc_hgn_mins / 60) * params_fin$fin_appt_sc_hgn_pph, 2),
             mm_act_com_dia_cost = round(mm_act_com_dia * (params_surv$mm_assess_setting_surv_com_dia_mins / 60) * params_fin$fin_appt_com_dia_pph, 2),
             mm_act_com_pha_cost = round(mm_act_com_pha * (params_surv$mm_assess_setting_surv_com_pha_mins / 60) * params_fin$fin_appt_com_pha_pph, 2)
      ) |>
      rowwise() |>
      mutate(mm_act_cost_total = sum(c_across(c(mm_act_pc_gp_cost,
                                                mm_act_pc_nur_cost,
                                                mm_act_sc_hgc_cost,
                                                mm_act_sc_hgn_cost,
                                                mm_act_com_dia_cost,
                                                mm_act_com_pha_cost)))
      )
    
  })
  
  pre_treat_mm_assess_res <- reactive({
    
    params_res <- res_pathway_assumptions()
    params_fin <- fin_assumptions()
    treat_pop_res() |>
      select(c(simulation, treated_total)) |>
      mutate(mm_act = round(treated_total * params_res$mm_assess_prop_res, 0),
             mm_act_pc_gp = round(mm_act * params_res$mm_assess_setting_res_pc_gp, 0),
             mm_act_pc_nur = round(mm_act * params_res$mm_assess_setting_res_pc_nur, 0),
             mm_act_sc_hgc = round(mm_act * params_res$mm_assess_setting_res_sc_hgc, 0),
             mm_act_sc_hgn = round(mm_act * params_res$mm_assess_setting_res_sc_hgn, 0),
             mm_act_com_dia = round(mm_act * params_res$mm_assess_setting_res_com_dia, 0),
             mm_act_com_pha = round(mm_act * params_res$mm_assess_setting_res_com_pha, 0),
             mm_act_pc_gp_cost = round(mm_act_pc_gp * (params_res$mm_assess_setting_res_pc_gp_mins / 60) * params_fin$fin_appt_pc_gp_pph, 2),
             mm_act_pc_nur_cost = round(mm_act_pc_nur * (params_res$mm_assess_setting_res_pc_nur_mins / 60) * params_fin$fin_appt_pc_nur_pph, 2),
             mm_act_sc_hgc_cost = round(mm_act_sc_hgc * (params_res$mm_assess_setting_res_sc_hgc_mins / 60) * params_fin$fin_appt_sc_hgc_pph, 2),
             mm_act_sc_hgn_cost = round(mm_act_sc_hgn * (params_res$mm_assess_setting_res_sc_hgn_mins / 60) * params_fin$fin_appt_sc_hgn_pph, 2),
             mm_act_com_dia_cost = round(mm_act_com_dia * (params_res$mm_assess_setting_res_com_dia_mins / 60) * params_fin$fin_appt_com_dia_pph, 2),
             mm_act_com_pha_cost = round(mm_act_com_pha * (params_res$mm_assess_setting_res_com_pha_mins / 60) * params_fin$fin_appt_com_pha_pph, 2)
      ) |>
      rowwise() |>
      mutate(mm_act_cost_total = sum(c_across(c(mm_act_pc_gp_cost,
                                                mm_act_pc_nur_cost,
                                                mm_act_sc_hgc_cost,
                                                mm_act_sc_hgn_cost,
                                                mm_act_com_dia_cost,
                                                mm_act_com_pha_cost)))
      )
    
  })
  
  pre_treat_mm_assess_lan <- reactive({
    
    params_lan <- lan_pathway_assumptions()
    params_fin <- fin_assumptions()
    treat_pop_lan() |>
      select(c(simulation, treated_total)) |>
      mutate(mm_act = round(treated_total * params_lan$mm_assess_prop_lan, 0),
             mm_act_pc_gp = round(mm_act * params_lan$mm_assess_setting_lan_pc_gp, 0),
             mm_act_pc_nur = round(mm_act * params_lan$mm_assess_setting_lan_pc_nur, 0),
             mm_act_sc_hgc = round(mm_act * params_lan$mm_assess_setting_lan_sc_hgc, 0),
             mm_act_sc_hgn = round(mm_act * params_lan$mm_assess_setting_lan_sc_hgn, 0),
             mm_act_com_dia = round(mm_act * params_lan$mm_assess_setting_lan_com_dia, 0),
             mm_act_com_pha = round(mm_act * params_lan$mm_assess_setting_lan_com_pha, 0),
             mm_act_pc_gp_cost = round(mm_act_pc_gp * (params_lan$mm_assess_setting_lan_pc_gp_mins / 60) * params_fin$fin_appt_pc_gp_pph, 2),
             mm_act_pc_nur_cost = round(mm_act_pc_nur * (params_lan$mm_assess_setting_lan_pc_nur_mins / 60) * params_fin$fin_appt_pc_nur_pph, 2),
             mm_act_sc_hgc_cost = round(mm_act_sc_hgc * (params_lan$mm_assess_setting_lan_sc_hgc_mins / 60) * params_fin$fin_appt_sc_hgc_pph, 2),
             mm_act_sc_hgn_cost = round(mm_act_sc_hgn * (params_lan$mm_assess_setting_lan_sc_hgn_mins / 60) * params_fin$fin_appt_sc_hgn_pph, 2),
             mm_act_com_dia_cost = round(mm_act_com_dia * (params_lan$mm_assess_setting_lan_com_dia_mins / 60) * params_fin$fin_appt_com_dia_pph, 2),
             mm_act_com_pha_cost = round(mm_act_com_pha * (params_lan$mm_assess_setting_lan_com_pha_mins / 60) * params_fin$fin_appt_com_pha_pph, 2)
      ) |>
      rowwise() |>
      mutate(mm_act_cost_total = sum(c_across(c(mm_act_pc_gp_cost,
                                                mm_act_pc_nur_cost,
                                                mm_act_sc_hgc_cost,
                                                mm_act_sc_hgn_cost,
                                                mm_act_com_dia_cost,
                                                mm_act_com_pha_cost)))
      )
    
  })
  
  pre_treat_mm_assess_all <- reactive({
    
    pre_treat_mm_assess_sem_all <- pre_treat_mm_assess_sem() |>
      select(c(simulation,
               mm_act,
               mm_act_cost_total,
               mm_act_pc_gp,
               mm_act_pc_gp_cost,
               mm_act_pc_nur,
               mm_act_pc_nur_cost,
               mm_act_sc_hgc,
               mm_act_sc_hgc_cost,
               mm_act_sc_hgn,
               mm_act_sc_hgn_cost,
               mm_act_com_dia,
               mm_act_com_dia_cost,
               mm_act_com_pha,
               mm_act_com_pha_cost)) |>
      rename("mm_act_sem" = 2,
             "mm_act_cost_total_sem" = 3,
             "mm_act_pc_gp_sem" = 4,
             "mm_act_pc_gp_cost_sem" = 5,
             "mm_act_pc_nur_sem" = 6,
             "mm_act_pc_nur_cost_sem" = 7,
             "mm_act_sc_hgc_sem" = 8,
             "mm_act_sc_hgc_cost_sem" = 9,
             "mm_act_sc_hgn_sem" = 10,
             "mm_act_sc_hgn_cost_sem" = 11,
             "mm_act_com_dia_sem" = 12,
             "mm_act_com_dia_cost_sem" = 13,
             "mm_act_com_pha_sem" = 14,
             "mm_act_com_pha_cost_sem" = 15
      )
    
    pre_treat_mm_assess_surv_all <- pre_treat_mm_assess_surv() |>
      select(c(simulation,
               mm_act,
               mm_act_cost_total,
               mm_act_pc_gp,
               mm_act_pc_gp_cost,
               mm_act_pc_nur,
               mm_act_pc_nur_cost,
               mm_act_sc_hgc,
               mm_act_sc_hgc_cost,
               mm_act_sc_hgn,
               mm_act_sc_hgn_cost,
               mm_act_com_dia,
               mm_act_com_dia_cost,
               mm_act_com_pha,
               mm_act_com_pha_cost)) |>
      rename("mm_act_surv" = 2,
             "mm_act_cost_total_surv" = 3,
             "mm_act_pc_gp_surv" = 4,
             "mm_act_pc_gp_cost_surv" = 5,
             "mm_act_pc_nur_surv" = 6,
             "mm_act_pc_nur_cost_surv" = 7,
             "mm_act_sc_hgc_surv" = 8,
             "mm_act_sc_hgc_cost_surv" = 9,
             "mm_act_sc_hgn_surv" = 10,
             "mm_act_sc_hgn_cost_surv" = 11,
             "mm_act_com_dia_surv" = 12,
             "mm_act_com_dia_cost_surv" = 13,
             "mm_act_com_pha_surv" = 14,
             "mm_act_com_pha_cost_surv" = 15
      )
    
    pre_treat_mm_assess_res_all <- pre_treat_mm_assess_res() |>
      select(c(simulation,
               mm_act,
               mm_act_cost_total,
               mm_act_pc_gp,
               mm_act_pc_gp_cost,
               mm_act_pc_nur,
               mm_act_pc_nur_cost,
               mm_act_sc_hgc,
               mm_act_sc_hgc_cost,
               mm_act_sc_hgn,
               mm_act_sc_hgn_cost,
               mm_act_com_dia,
               mm_act_com_dia_cost,
               mm_act_com_pha,
               mm_act_com_pha_cost)) |>
      rename("mm_act_res" = 2,
             "mm_act_cost_total_res" = 3,
             "mm_act_pc_gp_res" = 4,
             "mm_act_pc_gp_cost_res" = 5,
             "mm_act_pc_nur_res" = 6,
             "mm_act_pc_nur_cost_res" = 7,
             "mm_act_sc_hgc_res" = 8,
             "mm_act_sc_hgc_cost_res" = 9,
             "mm_act_sc_hgn_res" = 10,
             "mm_act_sc_hgn_cost_res" = 11,
             "mm_act_com_dia_res" = 12,
             "mm_act_com_dia_cost_res" = 13,
             "mm_act_com_pha_res" = 14,
             "mm_act_com_pha_cost_res" = 15
      )
    
    pre_treat_mm_assess_lan_all <- pre_treat_mm_assess_lan() |>
      select(c(simulation,
               mm_act,
               mm_act_cost_total,
               mm_act_pc_gp,
               mm_act_pc_gp_cost,
               mm_act_pc_nur,
               mm_act_pc_nur_cost,
               mm_act_sc_hgc,
               mm_act_sc_hgc_cost,
               mm_act_sc_hgn,
               mm_act_sc_hgn_cost,
               mm_act_com_dia,
               mm_act_com_dia_cost,
               mm_act_com_pha,
               mm_act_com_pha_cost)) |>
      rename("mm_act_lan" = 2,
             "mm_act_cost_total_lan" = 3,
             "mm_act_pc_gp_lan" = 4,
             "mm_act_pc_gp_cost_lan" = 5,
             "mm_act_pc_nur_lan" = 6,
             "mm_act_pc_nur_cost_lan" = 7,
             "mm_act_sc_hgc_lan" = 8,
             "mm_act_sc_hgc_cost_lan" = 9,
             "mm_act_sc_hgn_lan" = 10,
             "mm_act_sc_hgn_cost_lan" = 11,
             "mm_act_com_dia_lan" = 12,
             "mm_act_com_dia_cost_lan" = 13,
             "mm_act_com_pha_lan" = 14,
             "mm_act_com_pha_cost_lan" = 15
      )
    
    pre_treat_mm_assess_sem_all|>
      left_join(pre_treat_mm_assess_surv_all, by = c("simulation")) |>
      left_join(pre_treat_mm_assess_res_all, by = c("simulation")) |>
      left_join(pre_treat_mm_assess_lan_all, by = c("simulation")) |>
      rowwise() |>
      mutate("mm_act_all" = sum(c_across(c(mm_act_sem,
                                           mm_act_surv,
                                           mm_act_res,
                                           mm_act_lan))),
             "mm_act_cost_all" = sum(c_across(c(mm_act_cost_total_sem,
                                                mm_act_cost_total_surv,
                                                mm_act_cost_total_res,
                                                mm_act_cost_total_lan))),
             "mm_act_pc_gp_all" = sum(c_across(c(mm_act_pc_gp_sem,
                                                 mm_act_pc_gp_surv,
                                                 mm_act_pc_gp_res,
                                                 mm_act_pc_gp_lan))),
             "mm_act_pc_gp_cost_all" = sum(c_across(c(mm_act_pc_gp_cost_sem,
                                                      mm_act_pc_gp_cost_surv,
                                                      mm_act_pc_gp_cost_res,
                                                      mm_act_pc_gp_cost_lan))),
             "mm_act_pc_nur_all" = sum(c_across(c(mm_act_pc_nur_sem,
                                                  mm_act_pc_nur_surv,
                                                  mm_act_pc_nur_res,
                                                  mm_act_pc_nur_lan))),
             "mm_act_pc_nur_cost_all" = sum(c_across(c(mm_act_pc_nur_cost_sem,
                                                       mm_act_pc_nur_cost_surv,
                                                       mm_act_pc_nur_cost_res,
                                                       mm_act_pc_nur_cost_lan))),
             "mm_act_sc_hgc_all" = sum(c_across(c(mm_act_sc_hgc_sem,
                                                  mm_act_sc_hgc_surv,
                                                  mm_act_sc_hgc_res,
                                                  mm_act_sc_hgc_lan))),
             "mm_act_sc_hgc_cost_all" = sum(c_across(c(mm_act_sc_hgc_cost_sem,
                                                       mm_act_sc_hgc_cost_surv,
                                                       mm_act_sc_hgc_cost_res,
                                                       mm_act_sc_hgc_cost_lan))),
             "mm_act_sc_hgn_all" = sum(c_across(c(mm_act_sc_hgn_sem,
                                                  mm_act_sc_hgn_surv,
                                                  mm_act_sc_hgn_res,
                                                  mm_act_sc_hgn_lan))),
             "mm_act_sc_hgn_cost_all" = sum(c_across(c(mm_act_sc_hgn_cost_sem,
                                                       mm_act_sc_hgn_cost_surv,
                                                       mm_act_sc_hgn_cost_res,
                                                       mm_act_sc_hgn_cost_lan))),
             "mm_act_com_dia_all" = sum(c_across(c(mm_act_com_dia_sem,
                                                   mm_act_com_dia_surv,
                                                   mm_act_com_dia_res,
                                                   mm_act_com_dia_lan))),
             "mm_act_com_dia_cost_all" = sum(c_across(c(mm_act_com_dia_cost_sem,
                                                        mm_act_com_dia_cost_surv,
                                                        mm_act_com_dia_cost_res,
                                                        mm_act_com_dia_cost_lan))),
             "mm_act_com_pha_all" = sum(c_across(c(mm_act_com_dia_sem,
                                                   mm_act_com_dia_surv,
                                                   mm_act_com_dia_res,
                                                   mm_act_com_dia_lan))),
             "mm_act_com_pha_cost_all" = sum(c_across(c(mm_act_com_dia_cost_sem,
                                                        mm_act_com_dia_cost_surv,
                                                        mm_act_com_dia_cost_res,
                                                        mm_act_com_dia_cost_lan)))
      )
    
  })


# Treatment Delivery ------------------------------------------------------

init_treat_sem <- reactive({
  
  params_sem <- sem_pathway_assumptions()
  params_fin <- fin_assumptions()
  treat_pop_sem() |>
    select(c(simulation, treated_total)) |>
    mutate(treat_init_retained = round(treated_total * params_sem$retention_sem_0_16, 0)) |>
    mutate(week = list(0:16)) |>
    unnest(week) |>
    mutate(retention_factor = 1 - (week * (1 - params_sem$retention_sem_0_16) / 16),
           appts_week = round(treated_total * retention_factor, 0)) |>
    group_by(simulation) |>
    summarise(start_treat = mean(treated_total),
              end_treat = mean(treat_init_retained),
              treat_act = sum(appts_week) * (params_sem$appts_0_16_sem / 16), .groups = "drop") |>
    mutate(treat_act_pc_gp = round(treat_act * params_sem$treatment_setting_0_16_matrix_sem_pc_gp, 0),
           treat_act_pc_nur = round(treat_act * params_sem$treatment_setting_0_16_matrix_sem_pc_nur, 0),
           treat_act_sc_hgc = round(treat_act * params_sem$treatment_setting_0_16_matrix_sem_sc_hgc, 0),
           treat_act_sc_hgn = round(treat_act * params_sem$treatment_setting_0_16_matrix_sem_sc_hgn, 0),
           treat_act_com_dia = round(treat_act * params_sem$treatment_setting_0_16_matrix_sem_com_dia, 0),
           treat_act_com_pha = round(treat_act * params_sem$treatment_setting_0_16_matrix_sem_com_pha, 0),
           treat_act_pc_gp_cost = round(treat_act_pc_gp * (params_sem$treatment_setting_0_16_matrix_sem_pc_gp_mins / 60) * params_fin$fin_appt_pc_gp_pph, 2),
           treat_act_pc_nur_cost = round(treat_act_pc_nur * (params_sem$treatment_setting_0_16_matrix_sem_pc_nur_mins / 60) * params_fin$fin_appt_pc_nur_pph, 2),
           treat_act_sc_hgc_cost = round(treat_act_sc_hgc * (params_sem$treatment_setting_0_16_matrix_sem_sc_hgc_mins / 60) * params_fin$fin_appt_sc_hgc_pph, 2),
           treat_act_sc_hgn_cost = round(treat_act_sc_hgn * (params_sem$treatment_setting_0_16_matrix_sem_sc_hgn_mins / 60) * params_fin$fin_appt_sc_hgn_pph, 2),
           treat_act_com_dia_cost = round(treat_act_com_dia * (params_sem$treatment_setting_0_16_matrix_sem_com_dia_mins / 60) * params_fin$fin_appt_com_dia_pph, 2),
           treat_act_com_pha_cost = round(treat_act_com_pha * (params_sem$treatment_setting_0_16_matrix_sem_com_pha_mins / 60) * params_fin$fin_appt_com_pha_pph, 2)
    ) |>
    rowwise() |>
    mutate(treat_act_cost_total = sum(c_across(c(treat_act_pc_gp_cost,
                                               treat_act_pc_nur_cost,
                                               treat_act_sc_hgc_cost,
                                               treat_act_sc_hgn_cost,
                                               treat_act_com_dia_cost,
                                               treat_act_com_pha_cost)))
    )
  
})
  
dm1_treat_sem <- reactive({
  
  params_sem <- sem_pathway_assumptions()
  params_fin <- fin_assumptions()
  init_treat_sem() |>
    select(c(simulation, end_treat)) |>
    rename("start_treat" = 2) |>
    mutate(treat_dm1_retained = round(start_treat * params_sem$dosage_retention_20_71_sem, 0)) |>
    mutate(week = list(16:71)) |>
    unnest(week) |>
    mutate(retention_factor = 1 - (week * (1 - params_sem$dosage_retention_20_71_sem) / 55),
           appts_week = round(start_treat * retention_factor, 0)) |>
    group_by(simulation) |>
    summarise(start_treat = mean(start_treat),
              end_treat = mean(treat_dm1_retained),
              treat_act = sum(appts_week) * (params_sem$appts_20_71_sem / 55), .groups = "drop") |>
    mutate(treat_act_pc_gp = round(treat_act * params_sem$semaglutide_20_71_delivery_setting_pc_gp, 0),
           treat_act_pc_nur = round(treat_act * params_sem$semaglutide_20_71_delivery_setting_pc_nur, 0),
           treat_act_sc_hgc = round(treat_act * params_sem$semaglutide_20_71_delivery_setting_sc_hgc, 0),
           treat_act_sc_hgn = round(treat_act * params_sem$semaglutide_20_71_delivery_setting_sc_hgn, 0),
           treat_act_com_dia = round(treat_act * params_sem$semaglutide_20_71_delivery_setting_com_dia, 0),
           treat_act_com_pha = round(treat_act * params_sem$semaglutide_20_71_delivery_setting_com_pha, 0),
           treat_act_pc_gp_cost = round(treat_act_pc_gp * (params_sem$semaglutide_20_71_delivery_setting_pc_gp_mins / 60) * params_fin$fin_appt_pc_gp_pph, 2),
           treat_act_pc_nur_cost = round(treat_act_pc_nur * (params_sem$semaglutide_20_71_delivery_setting_pc_nur_mins / 60) * params_fin$fin_appt_pc_nur_pph, 2),
           treat_act_sc_hgc_cost = round(treat_act_sc_hgc * (params_sem$semaglutide_20_71_delivery_setting_sc_hgc_mins / 60) * params_fin$fin_appt_sc_hgc_pph, 2),
           treat_act_sc_hgn_cost = round(treat_act_sc_hgn * (params_sem$semaglutide_20_71_delivery_setting_sc_hgn_mins / 60) * params_fin$fin_appt_sc_hgn_pph, 2),
           treat_act_com_dia_cost = round(treat_act_com_dia * (params_sem$semaglutide_20_71_delivery_setting_com_dia_mins / 60) * params_fin$fin_appt_com_dia_pph, 2),
           treat_act_com_pha_cost = round(treat_act_com_pha * (params_sem$semaglutide_20_71_delivery_setting_com_pha_mins / 60) * params_fin$fin_appt_com_pha_pph, 2)
    )|>
    rowwise() |>
    mutate(treat_act_cost_total = sum(c_across(c(treat_act_pc_gp_cost,
                                                 treat_act_pc_nur_cost,
                                                 treat_act_sc_hgc_cost,
                                                 treat_act_sc_hgn_cost,
                                                 treat_act_com_dia_cost,
                                                 treat_act_com_pha_cost)))
    )
    
})

total_sem_treat_72 <- reactive({
  
  init_treat_sem() |>
  rename("start_treat" = 2) |>
  rbind(dm1_treat_sem()) |>
  group_by(simulation) |>
  summarise(treat_act = sum(treat_act),
            treat_act_cost_total = sum(treat_act_cost_total),
            treat_act_pc_gp = sum(treat_act_pc_gp),
            treat_act_pc_gp_cost = sum(treat_act_pc_gp_cost),
            treat_act_pc_nur = sum(treat_act_pc_nur),
            treat_act_pc_nur_cost = sum(treat_act_pc_nur_cost),
            treat_act_sc_hgc = sum(treat_act_sc_hgc),
            treat_act_sc_hgc_cost = sum(treat_act_sc_hgc_cost),
            treat_act_sc_hgn = sum(treat_act_sc_hgn),
            treat_act_sc_hgn_cost = sum(treat_act_sc_hgn_cost),
            treat_act_com_dia = sum(treat_act_com_dia),
            treat_act_com_dia_cost = sum(treat_act_com_dia_cost),
            treat_act_com_pha = sum(treat_act_com_pha),
            treat_act_com_pha_cost = sum(treat_act_com_pha_cost),
  )
  
})

init_treat_surv <- reactive({
  
  params_surv <- surv_pathway_assumptions()
  params_fin <- fin_assumptions()
  treat_pop_surv() |>
    select(c(simulation, treated_total)) |>
    mutate(treat_init_retained = round(treated_total * params_surv$retention_surv_0_24, 0)) |>
    mutate(week = list(0:24)) |>
    unnest(week) |>
    mutate(retention_factor = 1 - (week * (1 - params_surv$retention_surv_0_24) / 24),
           appts_week = round(treated_total * retention_factor, 0)) |>
    group_by(simulation) |>
    summarise(start_treat = mean(treated_total),
              end_treat = mean(treat_init_retained),
              treat_act = sum(appts_week) * (params_surv$appts_0_24_surv / 24), .groups = "drop") |>
    mutate(treat_act_pc_gp = round(treat_act * params_surv$treatment_setting_0_24_matrix_surv_pc_gp, 0),
           treat_act_pc_nur = round(treat_act * params_surv$treatment_setting_0_24_matrix_surv_pc_nur, 0),
           treat_act_sc_hgc = round(treat_act * params_surv$treatment_setting_0_24_matrix_surv_sc_hgc, 0),
           treat_act_sc_hgn = round(treat_act * params_surv$treatment_setting_0_24_matrix_surv_sc_hgn, 0),
           treat_act_com_dia = round(treat_act * params_surv$treatment_setting_0_24_matrix_surv_com_dia, 0),
           treat_act_com_pha = round(treat_act * params_surv$treatment_setting_0_24_matrix_surv_com_pha, 0),
           treat_act_pc_gp_cost = round(treat_act_pc_gp * (params_surv$treatment_setting_0_24_matrix_surv_pc_gp_mins / 60) * params_fin$fin_appt_pc_gp_pph, 2),
           treat_act_pc_nur_cost = round(treat_act_pc_nur * (params_surv$treatment_setting_0_24_matrix_surv_pc_nur_mins / 60) * params_fin$fin_appt_pc_nur_pph, 2),
           treat_act_sc_hgc_cost = round(treat_act_sc_hgc * (params_surv$treatment_setting_0_24_matrix_surv_sc_hgc_mins / 60) * params_fin$fin_appt_sc_hgc_pph, 2),
           treat_act_sc_hgn_cost = round(treat_act_sc_hgn * (params_surv$treatment_setting_0_24_matrix_surv_sc_hgn_mins / 60) * params_fin$fin_appt_sc_hgn_pph, 2),
           treat_act_com_dia_cost = round(treat_act_com_dia * (params_surv$treatment_setting_0_24_matrix_surv_com_dia_mins / 60) * params_fin$fin_appt_com_dia_pph, 2),
           treat_act_com_pha_cost = round(treat_act_com_pha * (params_surv$treatment_setting_0_24_matrix_surv_com_pha_mins / 60) * params_fin$fin_appt_com_pha_pph, 2)
    ) |>
    rowwise() |>
    mutate(treat_act_cost_total = sum(c_across(c(treat_act_pc_gp_cost,
                                                 treat_act_pc_nur_cost,
                                                 treat_act_sc_hgc_cost,
                                                 treat_act_sc_hgn_cost,
                                                 treat_act_com_dia_cost,
                                                 treat_act_com_pha_cost)))
    )
  
})

dm1_treat_surv <- reactive({
  
  params_surv <- surv_pathway_assumptions()
  params_fin <- fin_assumptions()
  init_treat_surv() |>
    select(c(simulation, end_treat)) |>
    rename("start_treat" = 2) |>
    mutate(treat_dm1_retained = round(start_treat * params_surv$retention_25_71_surv, 0)) |>
    mutate(week = list(24:71)) |>
    unnest(week) |>
    mutate(retention_factor = 1 - (week * (1 - params_surv$retention_25_71_surv) / 47),
           appts_week = round(start_treat * retention_factor, 0)) |>
    group_by(simulation) |>
    summarise(start_treat = mean(start_treat),
              end_treat = mean(treat_dm1_retained),
              treat_act = sum(appts_week), .groups = "drop") |>
    mutate(treat_act_pc_gp = round(treat_act * params_surv$treatment_setting_25_71_matrix_surv_pc_gp, 0),
           treat_act_pc_nur = round(treat_act * params_surv$treatment_setting_25_71_matrix_surv_pc_nur, 0),
           treat_act_sc_hgc = round(treat_act * params_surv$treatment_setting_25_71_matrix_surv_sc_hgc, 0),
           treat_act_sc_hgn = round(treat_act * params_surv$treatment_setting_25_71_matrix_surv_sc_hgn, 0),
           treat_act_com_dia = round(treat_act * params_surv$treatment_setting_25_71_matrix_surv_com_dia, 0),
           treat_act_com_pha = round(treat_act * params_surv$treatment_setting_25_71_matrix_surv_com_pha, 0),
           treat_act_pc_gp_cost = round(treat_act_pc_gp * (params_surv$treatment_setting_25_71_matrix_surv_pc_gp_mins / 60) * params_fin$fin_appt_pc_gp_pph, 2),
           treat_act_pc_nur_cost = round(treat_act_pc_nur * (params_surv$treatment_setting_25_71_matrix_surv_pc_nur_mins / 60) * params_fin$fin_appt_pc_nur_pph, 2),
           treat_act_sc_hgc_cost = round(treat_act_sc_hgc * (params_surv$treatment_setting_25_71_matrix_surv_sc_hgc_mins / 60) * params_fin$fin_appt_sc_hgc_pph, 2),
           treat_act_sc_hgn_cost = round(treat_act_sc_hgn * (params_surv$treatment_setting_25_71_matrix_surv_sc_hgn / 60) * params_fin$fin_appt_sc_hgn_pph, 2),
           treat_act_com_dia_cost = round(treat_act_com_dia * (params_surv$treatment_setting_25_71_matrix_surv_com_dia_mins / 60) * params_fin$fin_appt_com_dia_pph, 2),
           treat_act_com_pha_cost = round(treat_act_com_pha * (params_surv$treatment_setting_25_71_matrix_surv_com_pha_mins / 60) * params_fin$fin_appt_com_pha_pph, 2)
    )|>
    rowwise() |>
    mutate(treat_act_cost_total = sum(c_across(c(treat_act_pc_gp_cost,
                                                 treat_act_pc_nur_cost,
                                                 treat_act_sc_hgc_cost,
                                                 treat_act_sc_hgn_cost,
                                                 treat_act_com_dia_cost,
                                                 treat_act_com_pha_cost)))
    )
  
})

total_surv_treat_72 <- reactive({
  
  init_treat_surv() |>
    rename("start_treat" = 2) |>
    rbind(dm1_treat_surv()) |>
    group_by(simulation) |>
    summarise(treat_act = sum(treat_act),
              treat_act_cost_total = sum(treat_act_cost_total),
              treat_act_pc_gp = sum(treat_act_pc_gp),
              treat_act_pc_gp_cost = sum(treat_act_pc_gp_cost),
              treat_act_pc_nur = sum(treat_act_pc_nur),
              treat_act_pc_nur_cost = sum(treat_act_pc_nur_cost),
              treat_act_sc_hgc = sum(treat_act_sc_hgc),
              treat_act_sc_hgc_cost = sum(treat_act_sc_hgc_cost),
              treat_act_sc_hgn = sum(treat_act_sc_hgn),
              treat_act_sc_hgn_cost = sum(treat_act_sc_hgn_cost),
              treat_act_com_dia = sum(treat_act_com_dia),
              treat_act_com_dia_cost = sum(treat_act_com_dia_cost),
              treat_act_com_pha = sum(treat_act_com_pha),
              treat_act_com_pha_cost = sum(treat_act_com_pha_cost),
    )
  
})


# Continuation Decision ---------------------------------------------------

cont_dec_diag_sem <- reactive({
  
  params_sem <- sem_pathway_assumptions()
  params_fin <- fin_assumptions()
  
  dm1_treat_sem() |>
    select(c(simulation, end_treat)) |>
    rename(treat_dm1_retained = 2) |>
    mutate(liv_bio_act = round(treat_dm1_retained * params_sem$efficacy_liver_biopsy_prop_sem, 0),
           liv_bio_cost = round(liv_bio_act * params_fin$fin_liv_bio, 2),
           elf_act = round(treat_dm1_retained * params_sem$efficacy_elf_prop_sem, 0),
           elf_cost = round(elf_act * params_fin$fin_elf, 2),
           fibro_act = round(treat_dm1_retained * params_sem$efficacy_fibro_prop_sem, 0),
           fibro_cost = round(fibro_act * params_fin$fin_fibro, 2),
           biomarkers_act = round(treat_dm1_retained * params_sem$efficacy_biomarkers_prop_sem, 0),
           biomarkers_cost = round(elf_act * params_fin$fin_biomarkers, 2)
    ) |>
    rowwise() |>
    mutate(cont_diag_act = sum(c_across(c(liv_bio_act,
                                          elf_act,
                                          fibro_act,
                                          biomarkers_act))),
           cont_diag_cost = sum(c_across(c(liv_bio_cost,
                                           elf_cost,
                                           fibro_cost,
                                           biomarkers_cost)))
           )
  
})

cont_dec_diag_surv <- reactive({
  
  params_surv <- surv_pathway_assumptions()
  params_fin <- fin_assumptions()
  
  dm1_treat_surv() |>
    select(c(simulation, end_treat)) |>
    rename(treat_dm1_retained = 2) |>
    mutate(liv_bio_act = round(treat_dm1_retained * params_surv$efficacy_liver_biopsy_surv, 0),
           liv_bio_cost = round(liv_bio_act * params_fin$fin_liv_bio, 2),
           elf_act = round(treat_dm1_retained * params_surv$efficacy_elf_prop_surv, 0),
           elf_cost = round(elf_act * params_fin$fin_elf, 2),
           fibro_act = round(treat_dm1_retained * params_surv$efficacy_fibro_prop_surv, 0),
           fibro_cost = round(fibro_act * params_fin$fin_fibro, 2),
           biomarkers_act = round(treat_dm1_retained * params_surv$efficacy_biomarkers_prop_surv, 0),
           biomarkers_cost = round(elf_act * params_fin$fin_biomarkers, 2)
    ) |>
    rowwise() |>
    mutate(cont_diag_act = sum(c_across(c(liv_bio_act,
                                          elf_act,
                                          fibro_act,
                                          biomarkers_act))),
           cont_diag_cost = sum(c_across(c(liv_bio_cost,
                                           elf_cost,
                                           fibro_cost,
                                           biomarkers_cost)))
    )
  
})

cont_dec_diag_res <- reactive({
  
  params_res <- res_pathway_assumptions()
  params_fin <- fin_assumptions()
  
  dm1_treat_res() |>
    select(c(simulation, end_treat)) |>
    rename(treat_dm1_retained = 2) |>
    mutate(liv_bio_act = round(treat_dm1_retained * params_res$efficacy_liver_biopsy_prop_res, 0),
           liv_bio_cost = round(liv_bio_act * params_fin$fin_liv_bio, 2),
           elf_act = round(treat_dm1_retained * params_res$efficacy_elf_prop_res, 0),
           elf_cost = round(elf_act * params_fin$fin_elf, 2),
           fibro_act = round(treat_dm1_retained * params_res$efficacy_fibro_prop_res, 0),
           fibro_cost = round(fibro_act * params_fin$fin_fibro, 2),
           biomarkers_act = round(treat_dm1_retained * params_res$efficacy_biomarkers_prop_res, 0),
           biomarkers_cost = round(elf_act * params_fin$fin_biomarkers, 2)
    ) |>
    rowwise() |>
    mutate(cont_diag_act = sum(c_across(c(liv_bio_act,
                                          elf_act,
                                          fibro_act,
                                          biomarkers_act))),
           cont_diag_cost = sum(c_across(c(liv_bio_cost,
                                           elf_cost,
                                           fibro_cost,
                                           biomarkers_cost)))
    )
  
})

cont_dec_diag_lan <- reactive({
  
  params_lan <- lan_pathway_assumptions()
  params_fin <- fin_assumptions()
  
  dm1_treat_lan() |>
    select(c(simulation, end_treat)) |>
    rename(treat_dm1_retained = 2) |>
    mutate(liv_bio_act = round(treat_dm1_retained * params_lan$efficacy_liver_biopsy_prop_lan, 0),
           liv_bio_cost = round(liv_bio_act * params_fin$fin_liv_bio, 2),
           elf_act = round(treat_dm1_retained * params_lan$efficacy_elf_prop_lan, 0),
           elf_cost = round(elf_act * params_fin$fin_elf, 2),
           fibro_act = round(treat_dm1_retained * params_lan$efficacy_fibro_prop_lan, 0),
           fibro_cost = round(fibro_act * params_fin$fin_fibro, 2),
           biomarkers_act = round(treat_dm1_retained * params_lan$efficacy_biomarkers_prop_lan, 0),
           biomarkers_cost = round(elf_act * params_fin$fin_biomarkers, 2)
    ) |>
    rowwise() |>
    mutate(cont_diag_act = sum(c_across(c(liv_bio_act,
                                          elf_act,
                                          fibro_act,
                                          biomarkers_act))),
           cont_diag_cost = sum(c_across(c(liv_bio_cost,
                                           elf_cost,
                                           fibro_cost,
                                           biomarkers_cost)))
    )
  
})

cont_dec_diag_all <- reactive({
  
  cont_dec_diag_sem() |>
  rbind(cont_dec_diag_surv, cont_dec_diag_res, cont_dec_diag_lan) |>
  group_by(simulation) |>
  summarise(treat_dm1_retained = sum(treat_dm1_retained),
            cont_diag_act = sum(cont_diag_act),
            cont_diag_cost = sum(cont_diag_cost),
            liv_bio_act = sum(liv_bio_act),
            liv_bio_cost = sum(liv_bio_cost),
            elf_act = sum(elf_act),
            elf_cost = sum(elf_cost),
            fibro_act = sum(fibro_act),
            fibro_cost = sum(fibro_cost),
            biomarkers_act = sum(biomarkers_act),
            biomarkers_cost = sum(biomarkers_cost)
            )

})
  
# Outputs: Population -----------------------------------------------------

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
  

# Outputs: Treatment Implementation ---------------------------------------
  
  output$treat_pop_sem_DT <- renderDT({
    treat_pop_sem <- treat_pop_sem() |>
      rename("Simulation" = 1,
             "F0" = 2,
             "F1" = 3,
             "F2" = 4,
             "F3" = 5,
             "F4" = 6,
             "Total Starting Treatment" = 7)
    datatable(treat_pop_sem,
              rownames = FALSE,
              options = list(pageLength = 10,
                             autoWidth = TRUE))
  })
  
  output$treat_pop_surv_DT <- renderDT({
    treat_pop_surv <- treat_pop_surv() |>
      rename("Simulation" = 1,
             "F0" = 2,
             "F1" = 3,
             "F2" = 4,
             "F3" = 5,
             "F4" = 6,
             "Total Starting Treatment" = 7)
    datatable(treat_pop_surv,
              rownames = FALSE,
              options = list(pageLength = 10,
                             autoWidth = TRUE))
  })
  
  output$treat_pop_res_DT <- renderDT({
    treat_pop_res <- treat_pop_res() |>
      rename("Simulation" = 1,
             "F0" = 2,
             "F1" = 3,
             "F2" = 4,
             "F3" = 5,
             "F4" = 6,
             "Total Starting Treatment" = 7)
    datatable(treat_pop_res,
              rownames = FALSE,
              options = list(pageLength = 10,
                             autoWidth = TRUE))
  })
  
  output$treat_pop_lan_DT <- renderDT({
    treat_pop_lan <- treat_pop_lan() |>
      rename("Simulation" = 1,
             "F0" = 2,
             "F1" = 3,
             "F2" = 4,
             "F3" = 5,
             "F4" = 6,
             "Total Starting Treatment" = 7)
    datatable(treat_pop_lan,
              rownames = FALSE,
              options = list(pageLength = 10,
                             autoWidth = TRUE))
  })
  

# Outputs: Pre-Treatment --------------------------------------------------

  output$pre_treat_biopsy_sem_sum_1_act <- renderText({
    scales::comma(pre_treat_biopsy_sem()[[1, 3]], big.mark = ",")
  })
  
  output$pre_treat_biopsy_sem_sum_2_act <- renderText({
    scales::comma(pre_treat_biopsy_sem()[[2, 3]], big.mark = ",")
  })
  
  output$pre_treat_biopsy_sem_sum_3_act <- renderText({
    scales::comma(pre_treat_biopsy_sem()[[3, 3]], big.mark = ",")
  })
  
  output$pre_treat_biopsy_sem_sum_1_cost <- renderText({
    scales::dollar(pre_treat_biopsy_sem()[[1, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_biopsy_sem_sum_2_cost <- renderText({
    scales::dollar(pre_treat_biopsy_sem()[[2, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_biopsy_sem_sum_3_cost <- renderText({
    scales::dollar(pre_treat_biopsy_sem()[[3, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_biopsy_surv_sum_1_act <- renderText({
    scales::comma(pre_treat_biopsy_surv()[[1, 3]], big.mark = ",")
  })
  
  output$pre_treat_biopsy_surv_sum_2_act <- renderText({
    scales::comma(pre_treat_biopsy_surv()[[2, 3]], big.mark = ",")
  })
  
  output$pre_treat_biopsy_surv_sum_3_act <- renderText({
    scales::comma(pre_treat_biopsy_surv()[[3, 3]], big.mark = ",")
  })
  
  output$pre_treat_biopsy_surv_sum_1_cost <- renderText({
    scales::dollar(pre_treat_biopsy_surv()[[1, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_biopsy_surv_sum_2_cost <- renderText({
    scales::dollar(pre_treat_biopsy_surv()[[2, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_biopsy_surv_sum_3_cost <- renderText({
    scales::dollar(pre_treat_biopsy_surv()[[3, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
 
  output$pre_treat_biopsy_res_sum_1_act <- renderText({
    scales::comma(pre_treat_biopsy_res()[[1, 3]], big.mark = ",")
  })
  
  output$pre_treat_biopsy_res_sum_2_act <- renderText({
    scales::comma(pre_treat_biopsy_res()[[2, 3]], big.mark = ",")
  })
  
  output$pre_treat_biopsy_res_sum_3_act <- renderText({
    scales::comma(pre_treat_biopsy_res()[[3, 3]], big.mark = ",")
  })
  
  output$pre_treat_biopsy_res_sum_1_cost <- renderText({
    scales::dollar(pre_treat_biopsy_res()[[1, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_biopsy_res_sum_2_cost <- renderText({
    scales::dollar(pre_treat_biopsy_res()[[2, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_biopsy_res_sum_3_cost <- renderText({
    scales::dollar(pre_treat_biopsy_res()[[3, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_biopsy_lan_sum_1_act <- renderText({
    scales::comma(pre_treat_biopsy_lan()[[1, 3]], big.mark = ",")
  })
  
  output$pre_treat_biopsy_lan_sum_2_act <- renderText({
    scales::comma(pre_treat_biopsy_lan()[[2, 3]], big.mark = ",")
  })
  
  output$pre_treat_biopsy_lan_sum_3_act <- renderText({
    scales::comma(pre_treat_biopsy_lan()[[3, 3]], big.mark = ",")
  })
  
  output$pre_treat_biopsy_lan_sum_1_cost <- renderText({
    scales::dollar(pre_treat_biopsy_lan()[[1, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_biopsy_lan_sum_2_cost <- renderText({
    scales::dollar(pre_treat_biopsy_lan()[[2, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_biopsy_lan_sum_3_cost <- renderText({
    scales::dollar(pre_treat_biopsy_lan()[[3, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_biopsy_all_sum_1_act <- renderText({
    scales::comma(pre_treat_biopsy_all()[[1, 10]], big.mark = ",")
  })
  
  output$pre_treat_biopsy_all_sum_2_act <- renderText({
    scales::comma(pre_treat_biopsy_all()[[2, 10]], big.mark = ",")
  })
  
  output$pre_treat_biopsy_all_sum_3_act <- renderText({
    scales::comma(pre_treat_biopsy_all()[[3, 10]], big.mark = ",")
  })
  
  output$pre_treat_biopsy_all_sum_1_cost <- renderText({
    scales::dollar(pre_treat_biopsy_all()[[1, 11]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_biopsy_all_sum_2_cost <- renderText({
    scales::dollar(pre_treat_biopsy_all()[[2, 11]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_biopsy_all_sum_3_cost <- renderText({
    scales::dollar(pre_treat_biopsy_all()[[3, 11]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_biopsy_sem_DT <- renderDT({
    pre_treat_biopsy_sem_DT <- pre_treat_biopsy_sem() |>
      select(c(simulation, liv_bio_act, liv_bio_act_cost)) |>
      rename("Simulation" = 1,
             "Liver Biopsy Activity" = 2,
             "Liver Biopsy Costs" = 3)
    datatable(pre_treat_biopsy_sem_DT,
              rownames = FALSE,
              options = list(pageLength = 10,
                             autoWidth = TRUE)) |>
      formatCurrency(columns = "Liver Biopsy Costs", currency = "£", digits = 2)
  })
  
  output$pre_treat_biopsy_surv_DT <- renderDT({
    pre_treat_biopsy_surv_DT <- pre_treat_biopsy_surv() |>
      select(c(simulation, liv_bio_act, liv_bio_act_cost)) |>
      rename("Simulation" = 1,
             "Liver Biopsy Activity" = 2,
             "Liver Biopsy Costs" = 3)
    datatable(pre_treat_biopsy_surv_DT,
              rownames = FALSE,
              options = list(pageLength = 10,
                             autoWidth = TRUE)) |>
      formatCurrency(columns = "Liver Biopsy Costs", currency = "£", digits = 2)
  })
  
  output$pre_treat_biopsy_res_DT <- renderDT({
    pre_treat_biopsy_res_DT <- pre_treat_biopsy_res() |>
      select(c(simulation, liv_bio_act, liv_bio_act_cost)) |>
      rename("Simulation" = 1,
             "Liver Biopsy Activity" = 2,
             "Liver Biopsy Costs" = 3)
    datatable(pre_treat_biopsy_res_DT,
              rownames = FALSE,
              options = list(pageLength = 10,
                             autoWidth = TRUE)) |>
      formatCurrency(columns = "Liver Biopsy Costs", currency = "£", digits = 2)
  })
  
  output$pre_treat_biopsy_lan_DT <- renderDT({
    pre_treat_biopsy_lan_DT <- pre_treat_biopsy_lan() |>
      select(c(simulation, liv_bio_act, liv_bio_act_cost)) |>
      rename("Simulation" = 1,
             "Liver Biopsy Activity" = 2,
             "Liver Biopsy Costs" = 3)
    datatable(pre_treat_biopsy_lan_DT,
              rownames = FALSE,
              options = list(pageLength = 10,
                             autoWidth = TRUE)) |>
      formatCurrency(columns = "Liver Biopsy Costs", currency = "£", digits = 2)
  })
  
  output$pre_treat_biopsy_all_DT <- renderDT({
    pre_treat_biopsy_all_DT <- pre_treat_biopsy_all() |>
      select(c(simulation, liv_bio_act_all, liv_bio_act_cost_all)) |>
      rename("Simulation" = 1,
             "Liver Biopsy Activity" = 2,
             "Liver Biopsy Costs" = 3)
    datatable(pre_treat_biopsy_all_DT,
              rownames = FALSE,
              options = list(pageLength = 10,
                             autoWidth = TRUE)) |>
      formatCurrency(columns = "Liver Biopsy Costs", currency = "£", digits = 2)
    
  })
  
  output$pre_treat_elf_sem_sum_1_act <- renderText({
    scales::comma(pre_treat_elf_sem()[[1, 3]], big.mark = ",")
  })
  
  output$pre_treat_elf_sem_sum_2_act <- renderText({
    scales::comma(pre_treat_elf_sem()[[2, 3]], big.mark = ",")
  })
  
  output$pre_treat_elf_sem_sum_3_act <- renderText({
    scales::comma(pre_treat_elf_sem()[[3, 3]], big.mark = ",")
  })
  
  output$pre_treat_elf_sem_sum_1_cost <- renderText({
    scales::dollar(pre_treat_elf_sem()[[1, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_elf_sem_sum_2_cost <- renderText({
    scales::dollar(pre_treat_elf_sem()[[2, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_elf_sem_sum_3_cost <- renderText({
    scales::dollar(pre_treat_elf_sem()[[3, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_elf_surv_sum_1_act <- renderText({
    scales::comma(pre_treat_elf_surv()[[1, 3]], big.mark = ",")
  })
  
  output$pre_treat_elf_surv_sum_2_act <- renderText({
    scales::comma(pre_treat_elf_surv()[[2, 3]], big.mark = ",")
  })
  
  output$pre_treat_elf_surv_sum_3_act <- renderText({
    scales::comma(pre_treat_elf_surv()[[3, 3]], big.mark = ",")
  })
  
  output$pre_treat_elf_surv_sum_1_cost <- renderText({
    scales::dollar(pre_treat_elf_surv()[[1, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_elf_surv_sum_2_cost <- renderText({
    scales::dollar(pre_treat_elf_surv()[[2, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_elf_surv_sum_3_cost <- renderText({
    scales::dollar(pre_treat_elf_surv()[[3, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_elf_res_sum_1_act <- renderText({
    scales::comma(pre_treat_elf_res()[[1, 3]], big.mark = ",")
  })
  
  output$pre_treat_elf_res_sum_2_act <- renderText({
    scales::comma(pre_treat_elf_res()[[2, 3]], big.mark = ",")
  })
  
  output$pre_treat_elf_res_sum_3_act <- renderText({
    scales::comma(pre_treat_elf_res()[[3, 3]], big.mark = ",")
  })
  
  output$pre_treat_elf_res_sum_1_cost <- renderText({
    scales::dollar(pre_treat_elf_res()[[1, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_elf_res_sum_2_cost <- renderText({
    scales::dollar(pre_treat_elf_res()[[2, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_elf_res_sum_3_cost <- renderText({
    scales::dollar(pre_treat_elf_res()[[3, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_elf_lan_sum_1_act <- renderText({
    scales::comma(pre_treat_elf_lan()[[1, 3]], big.mark = ",")
  })
  
  output$pre_treat_elf_lan_sum_2_act <- renderText({
    scales::comma(pre_treat_elf_lan()[[2, 3]], big.mark = ",")
  })
  
  output$pre_treat_elf_lan_sum_3_act <- renderText({
    scales::comma(pre_treat_elf_lan()[[3, 3]], big.mark = ",")
  })
  
  output$pre_treat_elf_lan_sum_1_cost <- renderText({
    scales::dollar(pre_treat_elf_lan()[[1, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_elf_lan_sum_2_cost <- renderText({
    scales::dollar(pre_treat_elf_lan()[[2, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_elf_lan_sum_3_cost <- renderText({
    scales::dollar(pre_treat_elf_lan()[[3, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_elf_all_sum_1_act <- renderText({
    scales::comma(pre_treat_elf_all()[[1, 10]], big.mark = ",")
  })
  
  output$pre_treat_elf_all_sum_2_act <- renderText({
    scales::comma(pre_treat_elf_all()[[2, 10]], big.mark = ",")
  })
  
  output$pre_treat_elf_all_sum_3_act <- renderText({
    scales::comma(pre_treat_elf_all()[[3, 10]], big.mark = ",")
  })
  
  output$pre_treat_elf_all_sum_1_cost <- renderText({
    scales::dollar(pre_treat_elf_all()[[1, 11]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_elf_all_sum_2_cost <- renderText({
    scales::dollar(pre_treat_elf_all()[[2, 11]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_elf_all_sum_3_cost <- renderText({
    scales::dollar(pre_treat_elf_all()[[3, 11]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_elf_sem_DT <- renderDT({
    pre_treat_elf_sem_DT <- pre_treat_elf_sem() |>
      select(c(simulation, elf_act, elf_cost)) |>
      rename("Simulation" = 1,
             "ELF Activity" = 2,
             "ELF Costs" = 3)
    datatable(pre_treat_elf_sem_DT,
              rownames = FALSE,
              options = list(pageLength = 10,
                             autoWidth = TRUE)) |>
      formatCurrency(columns = "ELF Costs", currency = "£", digits = 2)
  })
  
  output$pre_treat_elf_surv_DT <- renderDT({
    pre_treat_elf_surv_DT <- pre_treat_elf_surv() |>
      select(c(simulation, elf_act, elf_cost)) |>
      rename("Simulation" = 1,
             "ELF Activity" = 2,
             "ELF Costs" = 3)
    datatable(pre_treat_elf_surv_DT,
              rownames = FALSE,
              options = list(pageLength = 10,
                             autoWidth = TRUE)) |>
      formatCurrency(columns = "ELF Costs", currency = "£", digits = 2)
  })
  
  output$pre_treat_elf_res_DT <- renderDT({
    pre_treat_elf_res_DT <- pre_treat_elf_res() |>
      select(c(simulation, elf_act, elf_cost)) |>
      rename("Simulation" = 1,
             "ELF Activity" = 2,
             "ELF Costs" = 3)
    datatable(pre_treat_elf_res_DT,
              rownames = FALSE,
              options = list(pageLength = 10,
                             autoWidth = TRUE)) |>
      formatCurrency(columns = "ELF Costs", currency = "£", digits = 2)
  })
  
  output$pre_treat_elf_lan_DT <- renderDT({
    pre_treat_elf_lan_DT <- pre_treat_elf_lan() |>
      select(c(simulation, elf_act, elf_cost)) |>
      rename("Simulation" = 1,
             "ELF Activity" = 2,
             "ELF Costs" = 3)
    datatable(pre_treat_elf_lan_DT,
              rownames = FALSE,
              options = list(pageLength = 10,
                             autoWidth = TRUE)) |>
      formatCurrency(columns = "ELF Costs", currency = "£", digits = 2)
  })
  
  output$pre_treat_elf_all_DT <- renderDT({
    pre_treat_elf_all_DT <- pre_treat_elf_all() |>
      select(c(simulation, elf_act_all, elf_act_cost_all)) |>
      rename("Simulation" = 1,
             "ELF Activity" = 2,
             "ELF Costs" = 3)
    datatable(pre_treat_elf_all_DT,
              rownames = FALSE,
              options = list(pageLength = 10,
                             autoWidth = TRUE)) |>
      formatCurrency(columns = "ELF Costs", currency = "£", digits = 2)
    
  })
  
  output$pre_treat_biomarkers_sem_sum_1_act <- renderText({
    scales::comma(pre_treat_biomarkers_sem()[[1, 3]], big.mark = ",")
  })
  
  output$pre_treat_biomarkers_sem_sum_2_act <- renderText({
    scales::comma(pre_treat_biomarkers_sem()[[2, 3]], big.mark = ",")
  })
  
  output$pre_treat_biomarkers_sem_sum_3_act <- renderText({
    scales::comma(pre_treat_biomarkers_sem()[[3, 3]], big.mark = ",")
  })
  
  output$pre_treat_biomarkers_sem_sum_1_cost <- renderText({
    scales::dollar(pre_treat_biomarkers_sem()[[1, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_biomarkers_sem_sum_2_cost <- renderText({
    scales::dollar(pre_treat_biomarkers_sem()[[2, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_biomarkers_sem_sum_3_cost <- renderText({
    scales::dollar(pre_treat_biomarkers_sem()[[3, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_biomarkers_surv_sum_1_act <- renderText({
    scales::comma(pre_treat_biomarkers_surv()[[1, 3]], big.mark = ",")
  })
  
  output$pre_treat_biomarkers_surv_sum_2_act <- renderText({
    scales::comma(pre_treat_biomarkers_surv()[[2, 3]], big.mark = ",")
  })
  
  output$pre_treat_biomarkers_surv_sum_3_act <- renderText({
    scales::comma(pre_treat_biomarkers_surv()[[3, 3]], big.mark = ",")
  })
  
  output$pre_treat_biomarkers_surv_sum_1_cost <- renderText({
    scales::dollar(pre_treat_biomarkers_surv()[[1, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_biomarkers_surv_sum_2_cost <- renderText({
    scales::dollar(pre_treat_biomarkers_surv()[[2, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_biomarkers_surv_sum_3_cost <- renderText({
    scales::dollar(pre_treat_biomarkers_surv()[[3, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_biomarkers_res_sum_1_act <- renderText({
    scales::comma(pre_treat_biomarkers_res()[[1, 3]], big.mark = ",")
  })
  
  output$pre_treat_biomarkers_res_sum_2_act <- renderText({
    scales::comma(pre_treat_biomarkers_res()[[2, 3]], big.mark = ",")
  })
  
  output$pre_treat_biomarkers_res_sum_3_act <- renderText({
    scales::comma(pre_treat_biomarkers_res()[[3, 3]], big.mark = ",")
  })
  
  output$pre_treat_biomarkers_res_sum_1_cost <- renderText({
    scales::dollar(pre_treat_biomarkers_res()[[1, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_biomarkers_res_sum_2_cost <- renderText({
    scales::dollar(pre_treat_biomarkers_res()[[2, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_biomarkers_res_sum_3_cost <- renderText({
    scales::dollar(pre_treat_biomarkers_res()[[3, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_biomarkers_lan_sum_1_act <- renderText({
    scales::comma(pre_treat_biomarkers_lan()[[1, 3]], big.mark = ",")
  })
  
  output$pre_treat_biomarkers_lan_sum_2_act <- renderText({
    scales::comma(pre_treat_biomarkers_lan()[[2, 3]], big.mark = ",")
  })
  
  output$pre_treat_biomarkers_lan_sum_3_act <- renderText({
    scales::comma(pre_treat_biomarkers_lan()[[3, 3]], big.mark = ",")
  })
  
  output$pre_treat_biomarkers_lan_sum_1_cost <- renderText({
    scales::dollar(pre_treat_biomarkers_lan()[[1, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_biomarkers_lan_sum_2_cost <- renderText({
    scales::dollar(pre_treat_biomarkers_lan()[[2, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_biomarkers_lan_sum_3_cost <- renderText({
    scales::dollar(pre_treat_biomarkers_lan()[[3, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_biomarkers_all_sum_1_act <- renderText({
    scales::comma(pre_treat_biomarkers_all()[[1, 10]], big.mark = ",")
  })
  
  output$pre_treat_biomarkers_all_sum_2_act <- renderText({
    scales::comma(pre_treat_biomarkers_all()[[2, 10]], big.mark = ",")
  })
  
  output$pre_treat_biomarkers_all_sum_3_act <- renderText({
    scales::comma(pre_treat_biomarkers_all()[[3, 10]], big.mark = ",")
  })
  
  output$pre_treat_biomarkers_all_sum_1_cost <- renderText({
    scales::dollar(pre_treat_biomarkers_all()[[1, 11]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_biomarkers_all_sum_2_cost <- renderText({
    scales::dollar(pre_treat_biomarkers_all()[[2, 11]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_biomarkers_all_sum_3_cost <- renderText({
    scales::dollar(pre_treat_biomarkers_all()[[3, 11]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_biomarkers_sem_DT <- renderDT({
    pre_treat_biomarkers_sem_DT <- pre_treat_biomarkers_sem() |>
      select(c(simulation, biomarkers_act, biomarkers_cost)) |>
      rename("Simulation" = 1,
             "Biomarkers Activity" = 2,
             "Biomarkers Costs" = 3)
    datatable(pre_treat_biomarkers_sem_DT,
              rownames = FALSE,
              options = list(pageLength = 10,
                             autoWidth = TRUE)) |>
      formatCurrency(columns = "Biomarkers Costs", currency = "£", digits = 2)
  })
  
  output$pre_treat_biomarkers_surv_DT <- renderDT({
    pre_treat_biomarkers_surv_DT <- pre_treat_biomarkers_surv() |>
      select(c(simulation, biomarkers_act, biomarkers_cost)) |>
      rename("Simulation" = 1,
             "Biomarkers Activity" = 2,
             "Biomarkers Costs" = 3)
    datatable(pre_treat_biomarkers_surv_DT,
              rownames = FALSE,
              options = list(pageLength = 10,
                             autoWidth = TRUE)) |>
      formatCurrency(columns = "Biomarkers Costs", currency = "£", digits = 2)
  })
  
  output$pre_treat_biomarkers_res_DT <- renderDT({
    pre_treat_biomarkers_res_DT <- pre_treat_biomarkers_res() |>
      select(c(simulation, biomarkers_act, biomarkers_cost)) |>
      rename("Simulation" = 1,
             "Biomarkers Activity" = 2,
             "Biomarkers Costs" = 3)
    datatable(pre_treat_biomarkers_res_DT,
              rownames = FALSE,
              options = list(pageLength = 10,
                             autoWidth = TRUE)) |>
      formatCurrency(columns = "Biomarkers Costs", currency = "£", digits = 2)
  })
  
  output$pre_treat_biomarkers_lan_DT <- renderDT({
    pre_treat_biomarkers_lan_DT <- pre_treat_biomarkers_lan() |>
      select(c(simulation, biomarkers_act, biomarkers_cost)) |>
      rename("Simulation" = 1,
             "Biomarkers Activity" = 2,
             "Biomarkers Costs" = 3)
    datatable(pre_treat_biomarkers_lan_DT,
              rownames = FALSE,
              options = list(pageLength = 10,
                             autoWidth = TRUE)) |>
      formatCurrency(columns = "Biomarkers Costs", currency = "£", digits = 2)
  })
  
  output$pre_treat_biomarkers_all_DT <- renderDT({
    pre_treat_biomarkers_all_DT <- pre_treat_biomarkers_all() |>
      select(c(simulation, biomarkers_act_all, biomarkers_act_cost_all)) |>
      rename("Simulation" = 1,
             "Biomarkers Activity" = 2,
             "Biomarkers Costs" = 3)
    datatable(pre_treat_biomarkers_all_DT,
              rownames = FALSE,
              options = list(pageLength = 10,
                             autoWidth = TRUE)) |>
      formatCurrency(columns = "Biomarkers Costs", currency = "£", digits = 2)
    
  })
  
  output$pre_treat_fibro_sem_sum_1_act <- renderText({
    scales::comma(pre_treat_fibro_sem()[[1, 3]], big.mark = ",")
  })
  
  output$pre_treat_fibro_sem_sum_2_act <- renderText({
    scales::comma(pre_treat_fibro_sem()[[2, 3]], big.mark = ",")
  })
  
  output$pre_treat_fibro_sem_sum_3_act <- renderText({
    scales::comma(pre_treat_fibro_sem()[[3, 3]], big.mark = ",")
  })
  
  output$pre_treat_fibro_sem_sum_1_cost <- renderText({
    scales::dollar(pre_treat_fibro_sem()[[1, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_fibro_sem_sum_2_cost <- renderText({
    scales::dollar(pre_treat_fibro_sem()[[2, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_fibro_sem_sum_3_cost <- renderText({
    scales::dollar(pre_treat_fibro_sem()[[3, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_fibro_surv_sum_1_act <- renderText({
    scales::comma(pre_treat_fibro_surv()[[1, 3]], big.mark = ",")
  })
  
  output$pre_treat_fibro_surv_sum_2_act <- renderText({
    scales::comma(pre_treat_fibro_surv()[[2, 3]], big.mark = ",")
  })
  
  output$pre_treat_fibro_surv_sum_3_act <- renderText({
    scales::comma(pre_treat_fibro_surv()[[3, 3]], big.mark = ",")
  })
  
  output$pre_treat_fibro_surv_sum_1_cost <- renderText({
    scales::dollar(pre_treat_fibro_surv()[[1, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_fibro_surv_sum_2_cost <- renderText({
    scales::dollar(pre_treat_fibro_surv()[[2, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_fibro_surv_sum_3_cost <- renderText({
    scales::dollar(pre_treat_fibro_surv()[[3, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_fibro_res_sum_1_act <- renderText({
    scales::comma(pre_treat_fibro_res()[[1, 3]], big.mark = ",")
  })
  
  output$pre_treat_fibro_res_sum_2_act <- renderText({
    scales::comma(pre_treat_fibro_res()[[2, 3]], big.mark = ",")
  })
  
  output$pre_treat_fibro_res_sum_3_act <- renderText({
    scales::comma(pre_treat_fibro_res()[[3, 3]], big.mark = ",")
  })
  
  output$pre_treat_fibro_res_sum_1_cost <- renderText({
    scales::dollar(pre_treat_fibro_res()[[1, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_fibro_res_sum_2_cost <- renderText({
    scales::dollar(pre_treat_fibro_res()[[2, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_fibro_res_sum_3_cost <- renderText({
    scales::dollar(pre_treat_fibro_res()[[3, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_fibro_lan_sum_1_act <- renderText({
    scales::comma(pre_treat_fibro_lan()[[1, 3]], big.mark = ",")
  })
  
  output$pre_treat_fibro_lan_sum_2_act <- renderText({
    scales::comma(pre_treat_fibro_lan()[[2, 3]], big.mark = ",")
  })
  
  output$pre_treat_fibro_lan_sum_3_act <- renderText({
    scales::comma(pre_treat_fibro_lan()[[3, 3]], big.mark = ",")
  })
  
  output$pre_treat_fibro_lan_sum_1_cost <- renderText({
    scales::dollar(pre_treat_fibro_lan()[[1, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_fibro_lan_sum_2_cost <- renderText({
    scales::dollar(pre_treat_fibro_lan()[[2, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_fibro_lan_sum_3_cost <- renderText({
    scales::dollar(pre_treat_fibro_lan()[[3, 4]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_fibro_all_sum_1_act <- renderText({
    scales::comma(pre_treat_fibro_all()[[1, 10]], big.mark = ",")
  })
  
  output$pre_treat_fibro_all_sum_2_act <- renderText({
    scales::comma(pre_treat_fibro_all()[[2, 10]], big.mark = ",")
  })
  
  output$pre_treat_fibro_all_sum_3_act <- renderText({
    scales::comma(pre_treat_fibro_all()[[3, 10]], big.mark = ",")
  })
  
  output$pre_treat_fibro_all_sum_1_cost <- renderText({
    scales::dollar(pre_treat_fibro_all()[[1, 11]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_fibro_all_sum_2_cost <- renderText({
    scales::dollar(pre_treat_fibro_all()[[2, 11]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_fibro_all_sum_3_cost <- renderText({
    scales::dollar(pre_treat_fibro_all()[[3, 11]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_fibro_sem_DT <- renderDT({
    pre_treat_fibro_sem_DT <- pre_treat_fibro_sem() |>
      select(c(simulation, fibro_act, fibro_cost)) |>
      rename("Simulation" = 1,
             "Fibroscan Activity" = 2,
             "Fibroscan Costs" = 3)
    datatable(pre_treat_fibro_sem_DT,
              rownames = FALSE,
              options = list(pageLength = 10,
                             autoWidth = TRUE)) |>
      formatCurrency(columns = "Fibroscan Costs", currency = "£", digits = 2)
  })
  
  output$pre_treat_fibro_surv_DT <- renderDT({
    pre_treat_fibro_surv_DT <- pre_treat_fibro_surv() |>
      select(c(simulation, fibro_act, fibro_cost)) |>
      rename("Simulation" = 1,
             "Fibroscan Activity" = 2,
             "Fibroscan Costs" = 3)
    datatable(pre_treat_fibro_surv_DT,
              rownames = FALSE,
              options = list(pageLength = 10,
                             autoWidth = TRUE)) |>
      formatCurrency(columns = "Fibroscan Costs", currency = "£", digits = 2)
  })
  
  output$pre_treat_fibro_res_DT <- renderDT({
    pre_treat_fibro_res_DT <- pre_treat_fibro_res() |>
      select(c(simulation, fibro_act, fibro_cost)) |>
      rename("Simulation" = 1,
             "Fibroscan Activity" = 2,
             "Fibroscan Costs" = 3)
    datatable(pre_treat_fibro_res_DT,
              rownames = FALSE,
              options = list(pageLength = 10,
                             autoWidth = TRUE)) |>
      formatCurrency(columns = "Fibroscan Costs", currency = "£", digits = 2)
  })
  
  output$pre_treat_fibro_lan_DT <- renderDT({
    pre_treat_fibro_lan_DT <- pre_treat_fibro_lan() |>
      select(c(simulation, fibro_act, fibro_cost)) |>
      rename("Simulation" = 1,
             "Fibroscan Activity" = 2,
             "Fibroscan Costs" = 3)
    datatable(pre_treat_fibro_lan_DT,
              rownames = FALSE,
              options = list(pageLength = 10,
                             autoWidth = TRUE)) |>
      formatCurrency(columns = "Fibroscan Costs", currency = "£", digits = 2)
  })
  
  output$pre_treat_fibro_all_DT <- renderDT({
    pre_treat_fibro_all_DT <- pre_treat_fibro_all() |>
      select(c(simulation, fibro_act_all, fibro_act_cost_all)) |>
      rename("Simulation" = 1,
             "Fibroscan Activity" = 2,
             "Fibroscan Costs" = 3)
    datatable(pre_treat_fibro_all_DT,
              rownames = FALSE,
              options = list(pageLength = 10,
                             autoWidth = TRUE)) |>
      formatCurrency(columns = "Fibroscan Costs", currency = "£", digits = 2)
    
  })
  
  output$pre_treat_mm_assess_sem_sum_1_act <- renderText({
    scales::comma(pre_treat_mm_assess_sem()[[1, 3]], big.mark = ",")
  })
  
  output$pre_treat_mm_assess_sem_sum_2_act <- renderText({
    scales::comma(pre_treat_mm_assess_sem()[[2, 3]], big.mark = ",")
  })
  
  output$pre_treat_mm_assess_sem_sum_3_act <- renderText({
    scales::comma(pre_treat_mm_assess_sem()[[3, 3]], big.mark = ",")
  })
  
  output$pre_treat_mm_assess_sem_sum_1_cost <- renderText({
    scales::dollar(pre_treat_mm_assess_sem()[[1, 16]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_mm_assess_sem_sum_2_cost <- renderText({
    scales::dollar(pre_treat_mm_assess_sem()[[2, 16]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_mm_assess_sem_sum_3_cost <- renderText({
    scales::dollar(pre_treat_mm_assess_sem()[[3, 16]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_mm_assess_surv_sum_1_act <- renderText({
    scales::comma(pre_treat_mm_assess_surv()[[1, 3]], big.mark = ",")
  })
  
  output$pre_treat_mm_assess_surv_sum_2_act <- renderText({
    scales::comma(pre_treat_mm_assess_surv()[[2, 3]], big.mark = ",")
  })
  
  output$pre_treat_mm_assess_surv_sum_3_act <- renderText({
    scales::comma(pre_treat_mm_assess_surv()[[3, 3]], big.mark = ",")
  })
  
  output$pre_treat_mm_assess_surv_sum_1_cost <- renderText({
    scales::dollar(pre_treat_mm_assess_surv()[[1, 16]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_mm_assess_surv_sum_2_cost <- renderText({
    scales::dollar(pre_treat_mm_assess_surv()[[2, 16]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_mm_assess_surv_sum_3_cost <- renderText({
    scales::dollar(pre_treat_mm_assess_surv()[[3, 16]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_mm_assess_res_sum_1_act <- renderText({
    scales::comma(pre_treat_mm_assess_res()[[1, 3]], big.mark = ",")
  })
  
  output$pre_treat_mm_assess_res_sum_2_act <- renderText({
    scales::comma(pre_treat_mm_assess_res()[[2, 3]], big.mark = ",")
  })
  
  output$pre_treat_mm_assess_res_sum_3_act <- renderText({
    scales::comma(pre_treat_mm_assess_res()[[3, 3]], big.mark = ",")
  })
  
  output$pre_treat_mm_assess_res_sum_1_cost <- renderText({
    scales::dollar(pre_treat_mm_assess_res()[[1, 16]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_mm_assess_res_sum_2_cost <- renderText({
    scales::dollar(pre_treat_mm_assess_res()[[2, 16]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_mm_assess_res_sum_3_cost <- renderText({
    scales::dollar(pre_treat_mm_assess_res()[[3, 16]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_mm_assess_lan_sum_1_act <- renderText({
    scales::comma(pre_treat_mm_assess_lan()[[1, 3]], big.mark = ",")
  })
  
  output$pre_treat_mm_assess_lan_sum_2_act <- renderText({
    scales::comma(pre_treat_mm_assess_lan()[[2, 3]], big.mark = ",")
  })
  
  output$pre_treat_mm_assess_lan_sum_3_act <- renderText({
    scales::comma(pre_treat_mm_assess_lan()[[3, 3]], big.mark = ",")
  })
  
  output$pre_treat_mm_assess_lan_sum_1_cost <- renderText({
    scales::dollar(pre_treat_mm_assess_lan()[[1, 16]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_mm_assess_lan_sum_2_cost <- renderText({
    scales::dollar(pre_treat_mm_assess_lan()[[2, 16]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_mm_assess_lan_sum_3_cost <- renderText({
    scales::dollar(pre_treat_mm_assess_lan()[[3, 16]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_mm_assess_all_sum_1_act <- renderText({
    scales::comma(pre_treat_mm_assess_all()[[1, 58]], big.mark = ",")
  })
  
  output$pre_treat_mm_assess_all_sum_2_act <- renderText({
    scales::comma(pre_treat_mm_assess_all()[[2, 58]], big.mark = ",")
  })
  
  output$pre_treat_mm_assess_all_sum_3_act <- renderText({
    scales::comma(pre_treat_mm_assess_all()[[3, 58]], big.mark = ",")
  })
  
  output$pre_treat_mm_assess_all_sum_1_cost <- renderText({
    scales::dollar(pre_treat_mm_assess_all()[[1, 59]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_mm_assess_all_sum_2_cost <- renderText({
    scales::dollar(pre_treat_mm_assess_all()[[2, 59]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_mm_assess_all_sum_3_cost <- renderText({
    scales::dollar(pre_treat_mm_assess_all()[[3, 59]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$pre_treat_mm_assess_sem_DT <- renderDT({
    pre_treat_mm_assess_sem_DT <- pre_treat_mm_assess_sem() |>
      select(c(simulation,
               mm_act,
               mm_act_cost_total,
               mm_act_pc_gp,
               mm_act_pc_gp_cost,
               mm_act_pc_nur,
               mm_act_pc_nur_cost,
               mm_act_sc_hgc,
               mm_act_sc_hgc_cost,
               mm_act_sc_hgn,
               mm_act_sc_hgn_cost,
               mm_act_com_dia,
               mm_act_com_dia_cost,
               mm_act_com_pha,
               mm_act_com_pha_cost)) |>
      rename("Simulation" = 1,
             "Total Assessments" = 2,
             "Total Assessments Costs" = 3,
             "GP Assessments" = 4,
             "GP Assessments Costs" = 5,
             "Primary Care Nurse Assessments" = 6,
             "Primary Care Nurse Costs" = 7,
             "Hepatology / Gastro Consultant Assessments" = 8,
             "Hepatology / Gastro Consutlant Costs" = 9,
             "Hepatology / Gastro Nurse Led Assessments" = 10,
             "Hepatology / Gastro Nurse Led Costs" = 11,
             "Community Diagnostician Assessments" = 12,
             "Community Diagnostician Costs" = 13,
             "Community Pharmacist Assessments" = 14,
             "Community Pharmacist Costs" = 15)
    datatable(pre_treat_mm_assess_sem_DT,
              rownames = FALSE,
              options = list(pageLength = 10,
                             autoWidth = TRUE,
                             scrollX = TRUE)) |>
      formatCurrency(columns = c("Total Assessments Costs",
                                 "GP Assessments Costs",
                                 "Primary Care Nurse Costs",
                                 "Hepatology / Gastro Consutlant Costs",
                                 "Hepatology / Gastro Nurse Led Costs",
                                 "Community Diagnostician Costs",
                                 "Community Pharmacist Costs")
                     ,currency = "£"
                     ,digits = 2)
  })
  
  output$pre_treat_mm_assess_surv_DT <- renderDT({
    pre_treat_mm_assess_surv_DT <- pre_treat_mm_assess_surv() |>
      select(c(simulation,
               mm_act,
               mm_act_cost_total,
               mm_act_pc_gp,
               mm_act_pc_gp_cost,
               mm_act_pc_nur,
               mm_act_pc_nur_cost,
               mm_act_sc_hgc,
               mm_act_sc_hgc_cost,
               mm_act_sc_hgn,
               mm_act_sc_hgn_cost,
               mm_act_com_dia,
               mm_act_com_dia_cost,
               mm_act_com_pha,
               mm_act_com_pha_cost)) |>
      rename("Simulation" = 1,
             "Total Assessments" = 2,
             "Total Assessments Costs" = 3,
             "GP Assessments" = 4,
             "GP Assessments Costs" = 5,
             "Primary Care Nurse Assessments" = 6,
             "Primary Care Nurse Costs" = 7,
             "Hepatology / Gastro Consultant Assessments" = 8,
             "Hepatology / Gastro Consutlant Costs" = 9,
             "Hepatology / Gastro Nurse Led Assessments" = 10,
             "Hepatology / Gastro Nurse Led Costs" = 11,
             "Community Diagnostician Assessments" = 12,
             "Community Diagnostician Costs" = 13,
             "Community Pharmacist Assessments" = 14,
             "Community Pharmacist Costs" = 15)
    datatable(pre_treat_mm_assess_surv_DT,
              rownames = FALSE,
              options = list(pageLength = 10,
                             autoWidth = TRUE,
                             scrollX = TRUE)) |>
      formatCurrency(columns = c("Total Assessments Costs",
                                 "GP Assessments Costs",
                                 "Primary Care Nurse Costs",
                                 "Hepatology / Gastro Consutlant Costs",
                                 "Hepatology / Gastro Nurse Led Costs",
                                 "Community Diagnostician Costs",
                                 "Community Pharmacist Costs")
                     ,currency = "£"
                     ,digits = 2)
  })
  
  output$pre_treat_mm_assess_res_DT <- renderDT({
    pre_treat_mm_assess_res_DT <- pre_treat_mm_assess_res() |>
      select(c(simulation,
               mm_act,
               mm_act_cost_total,
               mm_act_pc_gp,
               mm_act_pc_gp_cost,
               mm_act_pc_nur,
               mm_act_pc_nur_cost,
               mm_act_sc_hgc,
               mm_act_sc_hgc_cost,
               mm_act_sc_hgn,
               mm_act_sc_hgn_cost,
               mm_act_com_dia,
               mm_act_com_dia_cost,
               mm_act_com_pha,
               mm_act_com_pha_cost)) |>
      rename("Simulation" = 1,
             "Total Assessments" = 2,
             "Total Assessments Costs" = 3,
             "GP Assessments" = 4,
             "GP Assessments Costs" = 5,
             "Primary Care Nurse Assessments" = 6,
             "Primary Care Nurse Costs" = 7,
             "Hepatology / Gastro Consultant Assessments" = 8,
             "Hepatology / Gastro Consutlant Costs" = 9,
             "Hepatology / Gastro Nurse Led Assessments" = 10,
             "Hepatology / Gastro Nurse Led Costs" = 11,
             "Community Diagnostician Assessments" = 12,
             "Community Diagnostician Costs" = 13,
             "Community Pharmacist Assessments" = 14,
             "Community Pharmacist Costs" = 15)
    datatable(pre_treat_mm_assess_res_DT,
              rownames = FALSE,
              options = list(pageLength = 10,
                             autoWidth = TRUE,
                             scrollX = TRUE)) |>
      formatCurrency(columns = c("Total Assessments Costs",
                                 "GP Assessments Costs",
                                 "Primary Care Nurse Costs",
                                 "Hepatology / Gastro Consutlant Costs",
                                 "Hepatology / Gastro Nurse Led Costs",
                                 "Community Diagnostician Costs",
                                 "Community Pharmacist Costs")
                     ,currency = "£"
                     ,digits = 2)
  })
  
  output$pre_treat_mm_assess_lan_DT <- renderDT({
    pre_treat_mm_assess_lan_DT <- pre_treat_mm_assess_lan() |>
      select(c(simulation,
               mm_act,
               mm_act_cost_total,
               mm_act_pc_gp,
               mm_act_pc_gp_cost,
               mm_act_pc_nur,
               mm_act_pc_nur_cost,
               mm_act_sc_hgc,
               mm_act_sc_hgc_cost,
               mm_act_sc_hgn,
               mm_act_sc_hgn_cost,
               mm_act_com_dia,
               mm_act_com_dia_cost,
               mm_act_com_pha,
               mm_act_com_pha_cost)) |>
      rename("Simulation" = 1,
             "Total Assessments" = 2,
             "Total Assessments Costs" = 3,
             "GP Assessments" = 4,
             "GP Assessments Costs" = 5,
             "Primary Care Nurse Assessments" = 6,
             "Primary Care Nurse Costs" = 7,
             "Hepatology / Gastro Consultant Assessments" = 8,
             "Hepatology / Gastro Consutlant Costs" = 9,
             "Hepatology / Gastro Nurse Led Assessments" = 10,
             "Hepatology / Gastro Nurse Led Costs" = 11,
             "Community Diagnostician Assessments" = 12,
             "Community Diagnostician Costs" = 13,
             "Community Pharmacist Assessments" = 14,
             "Community Pharmacist Costs" = 15)
    datatable(pre_treat_mm_assess_lan_DT,
              rownames = FALSE,
              options = list(pageLength = 10,
                             autoWidth = TRUE,
                             scrollX = TRUE)) |>
      formatCurrency(columns = c("Total Assessments Costs",
                                 "GP Assessments Costs",
                                 "Primary Care Nurse Costs",
                                 "Hepatology / Gastro Consutlant Costs",
                                 "Hepatology / Gastro Nurse Led Costs",
                                 "Community Diagnostician Costs",
                                 "Community Pharmacist Costs")
                     ,currency = "£"
                     ,digits = 2)
  })
  
  output$pre_treat_mm_assess_all_DT <- renderDT({
    pre_treat_mm_assess_all_DT <- pre_treat_mm_assess_all() |>
      select(c(simulation,
               mm_act_all,
               mm_act_cost_all,
               mm_act_pc_gp_all,
               mm_act_pc_gp_cost_all,
               mm_act_pc_nur_all,
               mm_act_pc_nur_cost_all,
               mm_act_sc_hgc_all,
               mm_act_sc_hgc_cost_all,
               mm_act_sc_hgn_all,
               mm_act_sc_hgn_cost_all,
               mm_act_com_dia_all,
               mm_act_com_dia_cost_all,
               mm_act_com_pha_all,
               mm_act_com_pha_cost_all)) |>
      rename("Simulation" = 1,
             "Total Assessments" = 2,
             "Total Assessments Costs" = 3,
             "GP Assessments" = 4,
             "GP Assessments Costs" = 5,
             "Primary Care Nurse Assessments" = 6,
             "Primary Care Nurse Costs" = 7,
             "Hepatology / Gastro Consultant Assessments" = 8,
             "Hepatology / Gastro Consutlant Costs" = 9,
             "Hepatology / Gastro Nurse Led Assessments" = 10,
             "Hepatology / Gastro Nurse Led Costs" = 11,
             "Community Diagnostician Assessments" = 12,
             "Community Diagnostician Costs" = 13,
             "Community Pharmacist Assessments" = 14,
             "Community Pharmacist Costs" = 15)
    datatable(pre_treat_mm_assess_all_DT,
              rownames = FALSE,
              options = list(pageLength = 10,
                             autoWidth = TRUE,
                             scrollX = TRUE)) |>
      formatCurrency(columns = c("Total Assessments Costs",
                                 "GP Assessments Costs",
                                 "Primary Care Nurse Costs",
                                 "Hepatology / Gastro Consutlant Costs",
                                 "Hepatology / Gastro Nurse Led Costs",
                                 "Community Diagnostician Costs",
                                 "Community Pharmacist Costs")
                     ,currency = "£"
                     ,digits = 2)
  })
  

# Outputs: Treatment Delivery ---------------------------------------------
  
  output$init_treat_sem_016_sum_1_act <- renderText({
    scales::comma(init_treat_sem()[[1, 4]], big.mark = ",")
  })
  
  output$init_treat_sem_016_sum_2_act <- renderText({
    scales::comma(init_treat_sem()[[2, 4]], big.mark = ",")
  })
  
  output$init_treat_sem_016_sum_3_act <- renderText({
    scales::comma(init_treat_sem()[[3, 4]], big.mark = ",")
  })
  
  output$init_treat_sem_016_sum_1_cost <- renderText({
    scales::dollar(init_treat_sem()[[1, 17]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$init_treat_sem_016_sum_2_cost <- renderText({
    scales::dollar(init_treat_sem()[[2, 17]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$init_treat_sem_016_sum_3_cost <- renderText({
    scales::dollar(init_treat_sem()[[3, 17]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$init_treat_sem_DT <- renderDT({
    init_treat_sem_DT <- init_treat_sem() |>
      select(c(simulation,
               start_treat,
               end_treat,
               treat_act,
               treat_act_cost_total,
               treat_act_pc_gp,
               treat_act_pc_gp_cost,
               treat_act_pc_nur,
               treat_act_pc_nur_cost,
               treat_act_sc_hgc,
               treat_act_sc_hgc_cost,
               treat_act_sc_hgn,
               treat_act_sc_hgn_cost,
               treat_act_com_dia,
               treat_act_com_dia_cost,
               treat_act_com_pha,
               treat_act_com_pha_cost)) |>
      rename("Simulation" = 1,
             "Population Starting Treatment" = 2,
             "Population Retained at Week 16" = 3,
             "Total Treatment Activities" = 4,
             "Total Treatment Activities Costs" = 5,
             "GP Activities" = 6,
             "GP Costs" = 7,
             "Primary Care Nurse Activities" = 8,
             "Primary Care Nurse Costs" = 9,
             "Hepatology / Gastro Consultant Activities" = 10,
             "Hepatology / Gastro Consultant Costs" = 11,
             "Hepatology / Gastro Nurs Led Activities" = 12,
             "Hepatology / Gastro Nurse Led Costs" = 13,
             "Community Diagnostician Activities" = 14,
             "Community Diagnostician Costs" = 15,
             "Community Pharmacist Activities" = 16,
             "Community Pharmacist Costs" = 17)
    datatable(init_treat_sem_DT,
              rownames = FALSE,
              options = list(pageLength = 10,
                             autoWidth = TRUE,
                             scrollX = TRUE)) |>
      formatCurrency(columns = c("Total Treatment Activities Costs",
                                 "GP Costs",
                                 "Primary Care Nurse Costs",
                                 "Hepatology / Gastro Consultant Costs",
                                 "Hepatology / Gastro Nurse Led Costs",
                                 "Community Diagnostician Costs",
                                 "Community Pharmacist Costs")
                     ,currency = "£"
                     ,digits = 2) |>
      formatRound(columns = c("Population Starting Treatment",
                              "Population Retained at Week 16",
                              "Total Treatment Activities",
                              "GP Activities",
                              "Primary Care Nurse Activities",
                              "Hepatology / Gastro Consultant Activities",
                              "Hepatology / Gastro Nurs Led Activities",
                              "Community Diagnostician Activities",
                              "Community Pharmacist Activities"),
                  digits = 0)
  })
  
  output$dm1_treat_sem_sum_1_act <- renderText({
    scales::comma(dm1_treat_sem()[[1, 4]], big.mark = ",")
  })
  
  output$dm1_treat_sem_sum_2_act <- renderText({
    scales::comma(dm1_treat_sem()[[2, 4]], big.mark = ",")
  })
  
  output$dm1_treat_sem_sum_3_act <- renderText({
    scales::comma(dm1_treat_sem()[[3, 4]], big.mark = ",")
  })
  
  output$dm1_treat_sem_sum_1_cost <- renderText({
    scales::dollar(dm1_treat_sem()[[1, 17]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$dm1_treat_sem_sum_2_cost <- renderText({
    scales::dollar(dm1_treat_sem()[[2, 17]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$dm1_treat_sem_sum_3_cost <- renderText({
    scales::dollar(dm1_treat_sem()[[3, 17]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$dm1_treat_sem_DT <- renderDT({
    dm1_treat_sem_DT <- dm1_treat_sem() |>
      select(c(simulation,
               start_treat,
               end_treat,
               treat_act,
               treat_act_cost_total,
               treat_act_pc_gp,
               treat_act_pc_gp_cost,
               treat_act_pc_nur,
               treat_act_pc_nur_cost,
               treat_act_sc_hgc,
               treat_act_sc_hgc_cost,
               treat_act_sc_hgn,
               treat_act_sc_hgn_cost,
               treat_act_com_dia,
               treat_act_com_dia_cost,
               treat_act_com_pha,
               treat_act_com_pha_cost)) |>
      rename("Simulation" = 1,
             "Population Continuing Treatment at Week 16" = 2,
             "Population Retained at Week 71" = 3,
             "Total Treatment Activities" = 4,
             "Total Treatment Activities Costs" = 5,
             "GP Activities" = 6,
             "GP Costs" = 7,
             "Primary Care Nurse Activities" = 8,
             "Primary Care Nurse Costs" = 9,
             "Hepatology / Gastro Consultant Activities" = 10,
             "Hepatology / Gastro Consultant Costs" = 11,
             "Hepatology / Gastro Nurse Led Activities" = 12,
             "Hepatology / Gastro Nurse Led Costs" = 13,
             "Community Diagnostician Activities" = 14,
             "Community Diagnostician Costs" = 15,
             "Community Pharmacist Activities" = 16,
             "Community Pharmacist Costs" = 17)
    datatable(dm1_treat_sem_DT,
              rownames = FALSE,
              options = list(pageLength = 10,
                             autoWidth = TRUE,
                             scrollX = TRUE)) |>
      formatCurrency(columns = c("Total Treatment Activities Costs",
                                 "GP Costs",
                                 "Primary Care Nurse Costs",
                                 "Hepatology / Gastro Consultant Costs",
                                 "Hepatology / Gastro Nurse Led Costs",
                                 "Community Diagnostician Costs",
                                 "Community Pharmacist Costs")
                     ,currency = "£"
                     ,digits = 2) |>
      formatRound(columns = c("Population Continuing Treatment at Week 16",
                              "Population Retained at Week 71",
                              "Total Treatment Activities",
                              "GP Activities",
                              "Primary Care Nurse Activities",
                              "Hepatology / Gastro Consultant Activities",
                              "Hepatology / Gastro Nurse Led Activities",
                              "Community Diagnostician Activities",
                              "Community Pharmacist Activities"),
                  digits = 0)

  })
  
  output$total_sem_treat_72_sum_1_act <- renderText({
    scales::comma(total_sem_treat_72()[[1, 2]], big.mark = ",")
  })
  
  output$total_sem_treat_72_sum_2_act <- renderText({
    scales::comma(total_sem_treat_72()[[2, 2]], big.mark = ",")
  })
  
  output$total_sem_treat_72_sum_3_act <- renderText({
    scales::comma(total_sem_treat_72()[[3, 2]], big.mark = ",")
  })
  
  output$total_sem_treat_72_sum_1_cost <- renderText({
    scales::dollar(total_sem_treat_72()[[1, 3]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$total_sem_treat_72_sum_2_cost <- renderText({
    scales::dollar(total_sem_treat_72()[[2, 3]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$total_sem_treat_72_sum_3_cost<- renderText({
    scales::dollar(total_sem_treat_72()[[3, 3]], big.mark = ",", prefix = "£", suffix = ".")
  })

  output$total_sem_treat_72_DT <- renderDT({
    total_sem_treat_72_DT <- total_sem_treat_72() |>
      rename("Simulation" = 1,
             "Total Treatment Activities" = 2,
             "Total Treatment Activities Costs" = 3,
             "GP Activities" = 4,
             "GP Costs" = 5,
             "Primary Care Nurse Activities" = 6,
             "Primary Care Nurse Costs" = 7,
             "Hepatology / Gastro Consultant Activities" = 8,
             "Hepatology / Gastro Consultant Costs" = 9,
             "Hepatology / Gastro Nurse Led Activities" = 10,
             "Hepatology / Gastro Nurse Led Costs" = 11,
             "Community Diagnostician Activities" = 12,
             "Community Diagnostician Costs" = 13,
             "Community Pharmacist Activities" = 14,
             "Community Pharmacist Costs" = 15)
    datatable(total_sem_treat_72_DT,
              rownames = FALSE,
              options = list(pageLength = 10,
                             autoWidth = TRUE,
                             scrollX = TRUE)) |>
      formatCurrency(columns = c("Total Treatment Activities Costs",
                                 "GP Costs",
                                 "Primary Care Nurse Costs",
                                 "Hepatology / Gastro Consultant Costs",
                                 "Hepatology / Gastro Nurse Led Costs",
                                 "Community Diagnostician Costs",
                                 "Community Pharmacist Costs")
                     ,currency = "£"
                     ,digits = 2) |>
      formatRound(columns = c("Total Treatment Activities",
                              "GP Activities",
                              "Primary Care Nurse Activities",
                              "Hepatology / Gastro Consultant Activities",
                              "Hepatology / Gastro Nurse Led Activities",
                              "Community Diagnostician Activities",
                              "Community Pharmacist Activities"),
                  digits = 0)
  })
  
  output$init_treat_surv_024_sum_1_act <- renderText({
    scales::comma(init_treat_surv()[[1, 4]], big.mark = ",")
  })
  
  output$init_treat_surv_024_sum_2_act <- renderText({
    scales::comma(init_treat_surv()[[2, 4]], big.mark = ",")
  })
  
  output$init_treat_surv_024_sum_3_act <- renderText({
    scales::comma(init_treat_surv()[[3, 4]], big.mark = ",")
  })
  
  output$init_treat_surv_024_sum_1_cost <- renderText({
    scales::dollar(init_treat_surv()[[1, 17]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$init_treat_surv_024_sum_2_cost <- renderText({
    scales::dollar(init_treat_surv()[[2, 17]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$init_treat_surv_024_sum_3_cost <- renderText({
    scales::dollar(init_treat_surv()[[3, 17]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$init_treat_surv_DT <- renderDT({
    init_treat_surv_DT <- init_treat_surv() |>
      select(c(simulation,
               start_treat,
               end_treat,
               treat_act,
               treat_act_cost_total,
               treat_act_pc_gp,
               treat_act_pc_gp_cost,
               treat_act_pc_nur,
               treat_act_pc_nur_cost,
               treat_act_sc_hgc,
               treat_act_sc_hgc_cost,
               treat_act_sc_hgn,
               treat_act_sc_hgn_cost,
               treat_act_com_dia,
               treat_act_com_dia_cost,
               treat_act_com_pha,
               treat_act_com_pha_cost)) |>
      rename("Simulation" = 1,
             "Population Starting Treatment" = 2,
             "Population Retained at Week 24" = 3,
             "Total Treatment Activities" = 4,
             "Total Treatment Activities Costs" = 5,
             "GP Activities" = 6,
             "GP Costs" = 7,
             "Primary Care Nurse Activities" = 8,
             "Primary Care Nurse Costs" = 9,
             "Hepatology / Gastro Consultant Activities" = 10,
             "Hepatology / Gastro Consultant Costs" = 11,
             "Hepatology / Gastro Nurs Led Activities" = 12,
             "Hepatology / Gastro Nurse Led Costs" = 13,
             "Community Diagnostician Activities" = 14,
             "Community Diagnostician Costs" = 15,
             "Community Pharmacist Activities" = 16,
             "Community Pharmacist Costs" = 17)
    datatable(init_treat_surv_DT,
              rownames = FALSE,
              options = list(pageLength = 10,
                             autoWidth = TRUE,
                             scrollX = TRUE)) |>
      formatCurrency(columns = c("Total Treatment Activities Costs",
                                 "GP Costs",
                                 "Primary Care Nurse Costs",
                                 "Hepatology / Gastro Consultant Costs",
                                 "Hepatology / Gastro Nurse Led Costs",
                                 "Community Diagnostician Costs",
                                 "Community Pharmacist Costs")
                     ,currency = "£"
                     ,digits = 2) |>
      formatRound(columns = c("Population Starting Treatment",
                              "Population Retained at Week 24",
                              "Total Treatment Activities",
                              "GP Activities",
                              "Primary Care Nurse Activities",
                              "Hepatology / Gastro Consultant Activities",
                              "Hepatology / Gastro Nurs Led Activities",
                              "Community Diagnostician Activities",
                              "Community Pharmacist Activities"),
                  digits = 0)
  })
  
  output$dm1_treat_surv_sum_1_act <- renderText({
    scales::comma(dm1_treat_surv()[[1, 4]], big.mark = ",")
  })
  
  output$dm1_treat_surv_sum_2_act <- renderText({
    scales::comma(dm1_treat_surv()[[2, 4]], big.mark = ",")
  })
  
  output$dm1_treat_surv_sum_3_act <- renderText({
    scales::comma(dm1_treat_surv()[[3, 4]], big.mark = ",")
  })
  
  output$dm1_treat_surv_sum_1_cost <- renderText({
    scales::dollar(dm1_treat_surv()[[1, 17]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$dm1_treat_surv_sum_2_cost <- renderText({
    scales::dollar(dm1_treat_surv()[[2, 17]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$dm1_treat_surv_sum_3_cost <- renderText({
    scales::dollar(dm1_treat_surv()[[3, 17]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$dm1_treat_surv_DT <- renderDT({
    dm1_treat_surv_DT <- dm1_treat_surv() |>
      select(c(simulation,
               start_treat,
               end_treat,
               treat_act,
               treat_act_cost_total,
               treat_act_pc_gp,
               treat_act_pc_gp_cost,
               treat_act_pc_nur,
               treat_act_pc_nur_cost,
               treat_act_sc_hgc,
               treat_act_sc_hgc_cost,
               treat_act_sc_hgn,
               treat_act_sc_hgn_cost,
               treat_act_com_dia,
               treat_act_com_dia_cost,
               treat_act_com_pha,
               treat_act_com_pha_cost)) |>
      rename("Simulation" = 1,
             "Population Continuing Treatment at Week 24" = 2,
             "Population Retained at Week 71" = 3,
             "Total Treatment Activities" = 4,
             "Total Treatment Activities Costs" = 5,
             "GP Activities" = 6,
             "GP Costs" = 7,
             "Primary Care Nurse Activities" = 8,
             "Primary Care Nurse Costs" = 9,
             "Hepatology / Gastro Consultant Activities" = 10,
             "Hepatology / Gastro Consultant Costs" = 11,
             "Hepatology / Gastro Nurse Led Activities" = 12,
             "Hepatology / Gastro Nurse Led Costs" = 13,
             "Community Diagnostician Activities" = 14,
             "Community Diagnostician Costs" = 15,
             "Community Pharmacist Activities" = 16,
             "Community Pharmacist Costs" = 17)
    datatable(dm1_treat_surv_DT,
              rownames = FALSE,
              options = list(pageLength = 10,
                             autoWidth = TRUE,
                             scrollX = TRUE)) |>
      formatCurrency(columns = c("Total Treatment Activities Costs",
                                 "GP Costs",
                                 "Primary Care Nurse Costs",
                                 "Hepatology / Gastro Consultant Costs",
                                 "Hepatology / Gastro Nurse Led Costs",
                                 "Community Diagnostician Costs",
                                 "Community Pharmacist Costs")
                     ,currency = "£"
                     ,digits = 2) |>
      formatRound(columns = c("Population Continuing Treatment at Week 24",
                              "Population Retained at Week 71",
                              "Total Treatment Activities",
                              "GP Activities",
                              "Primary Care Nurse Activities",
                              "Hepatology / Gastro Consultant Activities",
                              "Hepatology / Gastro Nurse Led Activities",
                              "Community Diagnostician Activities",
                              "Community Pharmacist Activities"),
                  digits = 0)
    
  })
  
  output$total_surv_treat_72_sum_1_act <- renderText({
    scales::comma(total_surv_treat_72()[[1, 2]], big.mark = ",")
  })
  
  output$total_surv_treat_72_sum_2_act <- renderText({
    scales::comma(total_surv_treat_72()[[2, 2]], big.mark = ",")
  })
  
  output$total_surv_treat_72_sum_3_act <- renderText({
    scales::comma(total_surv_treat_72()[[3, 2]], big.mark = ",")
  })
  
  output$total_surv_treat_72_sum_1_cost <- renderText({
    scales::dollar(total_surv_treat_72()[[1, 3]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$total_surv_treat_72_sum_2_cost <- renderText({
    scales::dollar(total_surv_treat_72()[[2, 3]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$total_surv_treat_72_sum_3_cost <- renderText({
    scales::dollar(total_surv_treat_72()[[3, 3]], big.mark = ",", prefix = "£", suffix = ".")
  })
  
  output$total_surv_treat_72_DT <- renderDT({
    total_surv_treat_72_DT <- total_surv_treat_72() |>
      rename("Simulation" = 1,
             "Total Treatment Activities" = 2,
             "Total Treatment Activities Costs" = 3,
             "GP Activities" = 4,
             "GP Costs" = 5,
             "Primary Care Nurse Activities" = 6,
             "Primary Care Nurse Costs" = 7,
             "Hepatology / Gastro Consultant Activities" = 8,
             "Hepatology / Gastro Consultant Costs" = 9,
             "Hepatology / Gastro Nurse Led Activities" = 10,
             "Hepatology / Gastro Nurse Led Costs" = 11,
             "Community Diagnostician Activities" = 12,
             "Community Diagnostician Costs" = 13,
             "Community Pharmacist Activities" = 14,
             "Community Pharmacist Costs" = 15)
    datatable(total_surv_treat_72_DT,
              rownames = FALSE,
              options = list(pageLength = 10,
                             autoWidth = TRUE,
                             scrollX = TRUE)) |>
      formatCurrency(columns = c("Total Treatment Activities Costs",
                                 "GP Costs",
                                 "Primary Care Nurse Costs",
                                 "Hepatology / Gastro Consultant Costs",
                                 "Hepatology / Gastro Nurse Led Costs",
                                 "Community Diagnostician Costs",
                                 "Community Pharmacist Costs")
                     ,currency = "£"
                     ,digits = 2) |>
      formatRound(columns = c("Total Treatment Activities",
                              "GP Activities",
                              "Primary Care Nurse Activities",
                              "Hepatology / Gastro Consultant Activities",
                              "Hepatology / Gastro Nurse Led Activities",
                              "Community Diagnostician Activities",
                              "Community Pharmacist Activities"),
                  digits = 0)
  })
  
  

# Outputs: Continuation Decision ------------------------------------------

output$cont_dec_diag_sem_DT <- renderDT({
  
  cont_dec_diag_sem_DT <- cont_dec_diag_sem() |>
    rename("Simulation" = 1,
           "Patients at Continuation" = 2,
           "Liver Biopsy Activity" = 3,
           "Liver Biopsy Costs" = 4,
           "ELF Activity" = 5,
           "ELF Costs" = 6,
           "Fibroscan Activity" = 7,
           "Fibroscan Costs" = 8,
           "Biomarkers Activity" = 9,
           "Biomarkers Costs" = 10,
           "Total Diagnostic Activity" = 11,
           "Total Diagnostic Costs" = 12) |>
    select(c(1, 11:12, 2:10))
  datatable(cont_dec_diag_sem_DT,
            rownames = FALSE,
            options = list(pageLength = 10,
                           autoWidth = TRUE,
                           scrollX = TRUE)) |>
    formatCurrency(columns = c("Total Diagnostic Costs",
                               "Liver Biopsy Costs",
                               "ELF Costs",
                               "Fibroscan Costs",
                               "Biomarkers Costs"),
                   currency = "£",
                   digits = 2) |>
    formatRound(columns = c("Total Diagnostic Activity",
                            "Patients at Continuation",
                            "Liver Biopsy Activity",
                            "ELF Activity",
                            "Fibroscan Activity",
                            "Biomarkers Activity"),
                digits = 0)
  
})  

  output$cont_dec_diag_surv_DT <- renderDT({
    
    cont_dec_diag_surv_DT <- cont_dec_diag_surv() |>
      rename("Simulation" = 1,
             "Patients at Continuation" = 2,
             "Liver Biopsy Activity" = 3,
             "Liver Biopsy Costs" = 4,
             "ELF Activity" = 5,
             "ELF Costs" = 6,
             "Fibroscan Activity" = 7,
             "Fibroscan Costs" = 8,
             "Biomarkers Activity" = 9,
             "Biomarkers Costs" = 10,
             "Total Diagnostic Activity" = 11,
             "Total Diagnostic Costs" = 12) |>
      select(c(1, 11:12, 2:10))
    datatable(cont_dec_diag_surv_DT,
              rownames = FALSE,
              options = list(pageLength = 10,
                             autoWidth = TRUE,
                             scrollX = TRUE)) |>
      formatCurrency(columns = c("Total Diagnostic Costs",
                                 "Liver Biopsy Costs",
                                 "ELF Costs",
                                 "Fibroscan Costs",
                                 "Biomarkers Costs"),
                     currency = "£",
                     digits = 2) |>
      formatRound(columns = c("Total Diagnostic Activity",
                              "Patients at Continuation",
                              "Liver Biopsy Activity",
                              "ELF Activity",
                              "Fibroscan Activity",
                              "Biomarkers Activity"),
                  digits = 0)
    
  })
  
  output$cont_dec_diag_res_DT <- renderDT({
    
    cont_dec_diag_res_DT <- cont_dec_diag_res() |>
      rename("Simulation" = 1,
             "Patients at Continuation" = 2,
             "Liver Biopsy Activity" = 3,
             "Liver Biopsy Costs" = 4,
             "ELF Activity" = 5,
             "ELF Costs" = 6,
             "Fibroscan Activity" = 7,
             "Fibroscan Costs" = 8,
             "Biomarkers Activity" = 9,
             "Biomarkers Costs" = 10,
             "Total Diagnostic Activity" = 11,
             "Total Diagnostic Costs" = 12) |>
      select(c(1, 11:12, 2:10))
    datatable(cont_dec_diag_res_DT,
              rownames = FALSE,
              options = list(pageLength = 10,
                             autoWidth = TRUE,
                             scrollX = TRUE)) |>
      formatCurrency(columns = c("Total Diagnostic Costs",
                                 "Liver Biopsy Costs",
                                 "ELF Costs",
                                 "Fibroscan Costs",
                                 "Biomarkers Costs"),
                     currency = "£",
                     digits = 2) |>
      formatRound(columns = c("Total Diagnostic Activity",
                              "Patients at Continuation",
                              "Liver Biopsy Activity",
                              "ELF Activity",
                              "Fibroscan Activity",
                              "Biomarkers Activity"),
                  digits = 0)
    
  }) 

# Downloads: Inputs -------------------------------------------------------

  
  output$download_assumptions <- downloadHandler(
    filename = function() { "population_assumptions_pop.csv" },
    content = function(file) {
      
      df <- as.data.frame(t(unlist(assumptions())))
      write.csv(df, file, row.names = FALSE)
    }
  )
  
}