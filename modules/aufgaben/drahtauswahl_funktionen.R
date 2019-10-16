
###############################################################################
#
# file:         "drahtauswahl_funktionen.R"
#
# date:         2019-09-11
#
# authors:      Group 3
#
# brief:        This script contains DA_x functions used in "drahtauswahl.R".
#               
###############################################################################
#
# Following functions are implemented in this script:
#
# - DA_GetWeibullParametersFromTestData (public use)
# - DA_GetWeibullPlotFromTestData       (public use)
# - DA_getFailureProbabilities          (private use)
#
###############################################################################

DA_GetWeibullParametersFromTestData <- function(test_data){
  
  # convert to data.table
  test_data <- as.data.table(test_data)
  
  # check if necessary columns and entries exist
  col_names <- colnames(test_data)
  
  # remove rows containing NA entries
  test_data <- na.omit(test_data)
  
  # Calculation -------------------------------------------------------------
  
  # estimate failure probabilities (fp) of copper ("Cu") and aluminium ("Al")
  fp_cu <- DA_getFailureProbabilities(test_data, "Cu")
  fp_al <- DA_getFailureProbabilities(test_data, "Al")
  
  # do x on y regression ((log-) location-scale) 
  reg_cu <- rank_regression(x = fp_cu$characteristic,
                            y = fp_cu$prob, event = fp_cu$status)
  
  reg_al <- rank_regression(x = fp_al$characteristic,
                            y = fp_al$prob, event = fp_al$status)
  
  # get Weibull Parameters
  T_cu <- round(reg_cu$coefficients[[1]])
  b_cu <- round(reg_cu$coefficients[[2]], digits = 2)
  
  T_al <- round(reg_al$coefficients[[1]])
  b_al <- round(reg_al$coefficients[[2]], digits = 2)
  
  # calc 50% quantile
  q_50_cu <- round(predict_quantile(0.5, reg_cu$loc_sc_coefficients, 
                                    distribution = "weibull"))
  
  q_50_al <- round(predict_quantile(0.5, reg_al$loc_sc_coefficients, 
                                    distribution = "weibull"))
  
  # create data.table 
  wb_param_dt <- data.table(Material=c("Cu", "Al"), T=c(T_cu, T_al), 
                            b=c(b_cu, b_al), q_50 = c(q_50_cu, q_50_al))
  
  return(wb_param_dt)
}

###############################################################################

DA_GetWeibullPlotFromTestData <- function(test_data) {
  
  # Data Preparation --------------------------------------------------------
  
  # import data containing lifetime data
  test_data <- as.data.table(test_data)
  
  # check if necessary columns and entries exist
  col_names <- colnames(test_data)
  
  # remove rows containing NA entries
  test_data <- na.omit(test_data)
  
  # Calculation -------------------------------------------------------------
  
  # estimate failure probabilities (fp) of copper ("Cu") and aluminium ("Al")
  fp_cu <- DA_getFailureProbabilities(test_data, "Cu")
  fp_al <- DA_getFailureProbabilities(test_data, "Al")
  
  
  # do x on y regression ((log-) location-scale) 
  reg_cu <- rank_regression(x = fp_cu$characteristic,
                            y = fp_cu$prob, event = fp_cu$status)
  
  reg_al <- rank_regression(x = fp_al$characteristic,
                            y = fp_al$prob, event = fp_al$status)
  
  # Plot Creation -----------------------------------------------------------

  wb_plot <- plot_wahrsch_angepasst( x1      = fp_cu$characteristic, 
                           x2      = fp_al$characteristic,
                           y1      = fp_cu$prob, 
                           y2      = fp_al$prob,
                           event1  = fp_cu$status,
                           event2  = fp_al$status,
                           id1     = fp_cu$id,
                           id2     = fp_al$id,
                           merkmal = c("Cu","Al"),
                           distribution = "weibull")
    
  wb_plot <- plot_reg_angepasst( p_obj = wb_plot,
                       x1    = fp_cu$characteristic,
                       x2    = fp_al$characteristic,
                       mrr1  = reg_cu,
                       mrr2  = reg_al,
                       title_trace  = c("Cu","Al"),
                       distribution = "weibull")
  
  return(wb_plot)
}

###############################################################################

DA_getFailureProbabilities <- function(dat, material) {
  #
  # Estimates failure probabilities using mr_method or kaplan_method for 
  # a selected material, depending on existance of survivors
  #
  # args: 
  #   dat (data.table) - the data containing lifetime data of the material
  #   material (char)  - the material to use the method on
  #
  # return:
  #   fp (data.frame) -   A data frame containing id, lifetime characteristic, 
  #                       status of the unit and the estimated failure 
  #                       probabilty (fp). For right censored observations the
  #                       cells of probability column are filled with NA.
  # 
  # references:
  #   https://www.rdocumentation.org/packages/weibulltools/versions/1.0.1
  #   
  
  # filter data for material of interest
  dat_material <- dat[Material == material]
  
  # check if survivors exist
  has_no_survivors <- all(as.logical(dat_material$Status))
  
  # use appropiate method to calculate failure probabilities (fp)
  if (has_no_survivors) {
    fp <- mr_method(x = dat_material$Biegeanzahl, 
                    event = dat_material$Status,
                    id = dat_material$ID)
  } else {
    fp <- kaplan_method(x = dat_material$Biegeanzahl, 
                        event = dat_material$Status,
                        id = dat_material$ID)
  }
  
  return(fp)
}

###############################################################################

