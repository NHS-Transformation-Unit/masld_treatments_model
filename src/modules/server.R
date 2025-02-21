library(DT)
library(dplyr)
library(tidyr)
library(ggplot2)
library(EnvStats)

server <- function(input, output, session) {
  
  addResourcePath(prefix = "config", directoryPath = "www/config")
  
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
      pre_treat_biopsy_sem = input$pre_liver_biopsy_prop_sem / 100,
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
      retention_sem_0_16 = input$retention_sem_0_16 / 100,
      treatment_setting_0_16_matrix_sem_pc_gp = (input$treatment_setting_0_16_matrix_sem[1 , 1]) / 100,
      treatment_setting_0_16_matrix_sem_pc_nur = (input$treatment_setting_0_16_matrix_sem[2 , 1]) / 100,
      treatment_setting_0_16_matrix_sem_sc_hgc = (input$treatment_setting_0_16_matrix_sem[3 , 1]) / 100,
      treatment_setting_0_16_matrix_sem_sc_hgn = (input$treatment_setting_0_16_matrix_sem[4 , 1]) / 100,
      treatment_setting_0_16_matrix_sem_com_dia = (input$treatment_setting_0_16_matrix_sem[5 , 1]) / 100,
      treatment_setting_0_16_matrix_sem_com_pha = (input$treatment_setting_0_16_matrix_sem[6 , 1]) / 100,
      monitoring_tests_number_sem = input$monitoring_tests_number_sem,
      monitoring_elf_prop_sem = input$monitoring_elf_prop_sem / 100,
      monitoring_bio_prop_sem = input$monitoring_bio_prop_sem / 100,
      dosage_retention_20_71_sem = input$dosage_retention_20_71_sem / 100,
      semaglutide_20_71_delivery_setting_pc_go = (input$semaglutide_20_71_delivery_setting[1, 1] / 100),
      semaglutide_20_71_delivery_setting_pc_nur = (input$semaglutide_20_71_delivery_setting[2, 1] / 100),
      semaglutide_20_71_delivery_setting_sc_hgc = (input$semaglutide_20_71_delivery_setting[3, 1] / 100),
      semaglutide_20_71_delivery_setting_sc_hgn = (input$semaglutide_20_71_delivery_setting[4, 1] / 100),
      semaglutide_20_71_delivery_setting_com_dia = (input$semaglutide_20_71_delivery_setting[5, 1] / 100),
      semaglutide_20_71_delivery_setting_com_pha = (input$semaglutide_20_71_delivery_setting[6, 1] / 100),
      efficacy_liver_biopsy_prop_sem = input$efficacy_liver_biopsy_prop_sem / 100,
      efficacy_elf_prop_sem = input$efficacy_elf_prop_sem / 100,
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
      retention_surv_0_24 = input$retention_surv_0_24 / 100,
      treatment_setting_0_24_matrix_surv_pc_gp = (input$treatment_setting_0_24_matrix_surv[1, 1]) / 100,
      treatment_setting_0_24_matrix_surv_pc_nur = (input$treatment_setting_0_24_matrix_surv[2, 1]) / 100,
      treatment_setting_0_24_matrix_surv_sc_hgc = (input$treatment_setting_0_24_matrix_surv[3, 1]) / 100,
      treatment_setting_0_24_matrix_surv_sc_hgn = (input$treatment_setting_0_24_matrix_surv[4, 1]) / 100,
      treatment_setting_0_24_matrix_surv_com_dia = (input$treatment_setting_0_24_matrix_surv[5, 1]) / 100,
      treatment_setting_0_24_matrix_surv_com_pha = (input$treatment_setting_0_24_matrix_surv[6, 1]) / 100,
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
  
  
  pre_treat_biopsy_sem <- reactive({
    
    params_sem <- sem_pathway_assumptions()
    treat_pop_sem() |>
      select(c(simulation, treated_total)) |>
      mutate(liv_bio_act = round(treated_total * params_sem$pre_treat_biopsy_sem, 0))
    
  })
  
  pre_treat_biopsy_surv <- reactive({
    
    params_surv <- surv_pathway_assumptions()
    treat_pop_surv() |>
      select(c(simulation, treated_total)) |>
      mutate(liv_bio_act = round(treated_total * params_surv$pre_liver_biopsy_prop_surv, 0))
    
  })
  
  pre_treat_biopsy_res <- reactive({
    
    params_res <- res_pathway_assumptions()
    treat_pop_res() |>
      select(c(simulation, treated_total)) |>
      mutate(liv_bio_act = round(treated_total * params_res$pre_liver_biopsy_prop_res, 0))
    
  })
  
  pre_treat_biopsy_lan <- reactive({
    
    params_lan <- lan_pathway_assumptions()
    treat_pop_lan() |>
      select(c(simulation, treated_total)) |>
      mutate(liv_bio_act = round(treated_total * params_lan$pre_liver_biopsy_prop_lan, 0))
    
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
  
  output$pre_treat_biopsy_sem_DT <- renderDT({
    pre_treat_biopsy_sem_DT <- pre_treat_biopsy_sem()
    datatable(pre_treat_biopsy_sem_DT,
              rownames = FALSE,
              options = list(pageLength = 10,
                             autoWifth = TRUE))
  })
  
  output$pre_treat_biopsy_surv_DT <- renderDT({
    pre_treat_biopsy_surv_DT <- pre_treat_biopsy_surv()
    datatable(pre_treat_biopsy_surv_DT,
              rownames = FALSE,
              options = list(pageLength = 10,
                             autoWifth = TRUE))
  })
  
  output$pre_treat_biopsy_res_DT <- renderDT({
    pre_treat_biopsy_res_DT <- pre_treat_biopsy_res()
    datatable(pre_treat_biopsy_res_DT,
              rownames = FALSE,
              options = list(pageLength = 10,
                             autoWifth = TRUE))
  })
  
  output$pre_treat_biopsy_lan_DT <- renderDT({
    pre_treat_biopsy_lan_DT <- pre_treat_biopsy_lan()
    datatable(pre_treat_biopsy_lan_DT,
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