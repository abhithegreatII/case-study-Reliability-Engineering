
###############################################################################
#
# file:         "raffungstest_funktionen.R"
#
# date:         2019-09-11
#
# authors:      Group 3
#
# brief:        This script contains RT_x functions used in "raffungstest.R".
#               
###############################################################################


RT_GetWeibullParametersFromTestData <- function(test_data){
  
  # convert to data.table
  test_data <- as.data.table(test_data)
  
  # check if necessary columns and entries exist
  col_names <- colnames(test_data)
  
  # remove rows containing NA entries
  test_data <- na.omit(test_data)
  
  # Calculation -------------------------------------------------------------
  
  # estimate failure probabilities (fp) of copper ("Cu") and aluminium ("Al")
  fp_45  <- RT_getFailureProbabilities(test_data,  45)
  fp_90  <- RT_getFailureProbabilities(test_data,  90)
  fp_180 <- RT_getFailureProbabilities(test_data, 180)
  
  # do x on y regression ((log-) location-scale) 
  reg_45 <- rank_regression(x = fp_45$characteristic,
                            y = fp_45$prob, event = fp_45$status)
  
  reg_90 <- rank_regression(x = fp_90$characteristic,
                            y = fp_90$prob, event = fp_90$status)
  
  reg_180 <- rank_regression(x = fp_180$characteristic,
                            y = fp_180$prob, event = fp_180$status)
  
  # get Weibull Parameters
  T_45 <- round(reg_45$coefficients[[1]])
  b_45 <- round(reg_45$coefficients[[2]], digits = 2)
  
  T_90 <- round(reg_90$coefficients[[1]])
  b_90 <- round(reg_90$coefficients[[2]], digits = 2)
  
  T_180 <- round(reg_180$coefficients[[1]])
  b_180 <- round(reg_180$coefficients[[2]], digits = 2)
  
  # calc 50% quantile
  q_50_45 <- round(predict_quantile(0.5, reg_45$loc_sc_coefficients, 
                                    distribution = "weibull"))
  q_50_90 <- round(predict_quantile(0.5, reg_90$loc_sc_coefficients, 
                                    distribution = "weibull"))
  q_50_180 <- round(predict_quantile(0.5, reg_180$loc_sc_coefficients, 
                                    distribution = "weibull"))
  
  # create data.table 
  wb_param_dt <- data.table(Biegewinkel=c(45, 90, 180), 
                            T=c(T_45, T_90, T_180), 
                            b=c(b_45, b_90, b_180), 
                            q_50 = c(q_50_45, q_50_90, q_50_180))
  
  return(wb_param_dt)
}

###############################################################################

RT_GetAccelerationFactorFromTestData <- function(test_data){
  
  # convert to data.table
  test_data <- as.data.table(test_data)
  
  # check if necessary columns and entries exist
  col_names <- colnames(test_data)
  
  # remove rows containing NA entries
  test_data <- na.omit(test_data)
  
  # Calculation -------------------------------------------------------------
  
  # estimate failure probabilities (fp) of copper ("Cu") and aluminium ("Al")
  fp_45  <- RT_getFailureProbabilities(test_data,  45)
  fp_90  <- RT_getFailureProbabilities(test_data,  90)
  fp_180 <- RT_getFailureProbabilities(test_data, 180)
  
  # do x on y regression ((log-) location-scale) 
  reg_45 <- rank_regression(x = fp_45$characteristic,
                            y = fp_45$prob, event = fp_45$status)
  
  reg_90 <- rank_regression(x = fp_90$characteristic,
                            y = fp_90$prob, event = fp_90$status)
  
  reg_180 <- rank_regression(x = fp_180$characteristic,
                             y = fp_180$prob, event = fp_180$status)
  
  # get Weibull Parameters
  T_45 <- round(reg_45$coefficients[[1]])
  
  T_90 <- round(reg_90$coefficients[[1]])
  
  T_180 <- round(reg_180$coefficients[[1]])
  
  # create data.table 
  acc_fac_dt <- data.table(Berechnung = 
                             c(
                               "T_45 / T_90",
                               "T_45 / T_180",
                               "T_90 / T_180"),
                           Raffungsfaktor = 
                             c(
                               round(T_45/T_90,  2),
                               round(T_45/T_180, 2),
                               round(T_90/T_180, 2)))
  
  acc_fac_dt <- acc_fac_dt[order(Raffungsfaktor),]
  
  return(acc_fac_dt)
}

###############################################################################

RT_getFailureProbabilities <- function(dat, winkel) {
  #
  # Estimates failure probabilities using mr_method or kaplan_method for 
  # a selected material, depending on existance of survivors
  #
  # args: 
  #   dat (data.table) - the data containing lifetime data of the material
  #   winkel (numeric) - the angle to use the method on
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
  dat_winkel <- dat[Winkel == winkel]
  
  # check if survivors exist
  has_no_survivors <- all(as.logical(dat_winkel$Status))
  
  # use appropiate method to calculate failure probabilities (fp)
  if (has_no_survivors) {
    fp <- mr_method(x = dat_winkel$Biegeanzahl, 
                    event = dat_winkel$Status,
                    id = dat_winkel$ID)
  } else {
    fp <- kaplan_method(x = dat_winkel$Biegeanzahl, 
                        event = dat_winkel$Status,
                        id = dat_winkel$ID)
  }
  
  return(fp)
}


###############################################################################


RT_GetWeibullPlotFromTestData <- function(test_data) {
  #
  # Creates Weibull Plots from test_data for one material and differenct 
  # angles.
  #
  # args: 
  #   test_data (data.frame) - containing test data from excel sheet
  #
  # return:
  #   wb_plot (plotly) - weibull plot of different angles
  # 
  #
  
  # Data Preparation --------------------------------------------------------
  
  # import data containing lifetime data
  test_data <- as.data.table(test_data)
  
  # check if necessary columns and entries exist
  col_names <- colnames(test_data)
  
  # remove rows containing NA entries
  test_data <- na.omit(test_data)
  
  # Calculation -------------------------------------------------------------
  
  # estimate failure probabilities (fp) of copper ("Cu") and aluminium ("Al")
  fp_45  <- RT_getFailureProbabilities(test_data,  45)
  fp_90  <- RT_getFailureProbabilities(test_data,  90)
  fp_180 <- RT_getFailureProbabilities(test_data, 180)
  
  # do x on y regression ((log-) location-scale) 
  reg_45 <- rank_regression(x = fp_45$characteristic,
                            y = fp_45$prob, event = fp_45$status)
  
  reg_90 <- rank_regression(x = fp_90$characteristic,
                            y = fp_90$prob, event = fp_90$status)
  
  reg_180 <- rank_regression(x = fp_180$characteristic,
                             y = fp_180$prob, event = fp_180$status)
  
  # Plot Creation -------------------------------------------------------------
  
  wb_plot <- plot_wahrsch_angepasst( x1      = fp_45$characteristic, 
                           x2      = fp_90$characteristic,
                           x3      = fp_180$characteristic,
                           y1      = fp_45$prob, 
                           y2      = fp_90$prob,
                           y3      = fp_180$prob,
                           event1  = fp_45$status,
                           event2  = fp_90$status,
                           event3  = fp_180$status,
                           id1     = fp_45$id,
                           id2     = fp_90$id,
                           id3     = fp_180$id,
                           merkmal = c("45","90", "180"),
                           distribution = "weibull")

  wb_plot <- plot_reg_angepasst( p_obj = wb_plot,
                       x1    = fp_45$characteristic,
                       x2    = fp_90$characteristic,
                       x3    = fp_180$characteristic,
                       mrr1  = reg_45,
                       mrr2  = reg_90,
                       mrr3  = reg_180,
                       title_trace  = c("45","90", "180"),
                       distribution = "weibull")
  
  return(wb_plot)
  
}

###############################################################################

RT_GetWoehlerPlotFromTestData <- function(test_data) {
  #
  # Creates Weibull Plots from test_data for one material and differenct 
  # angles.
  #
  # args: 
  #   test_data (data.frame) - containing test data from excel sheet
  #
  # return:
  #   woehler_plot (plotly) - weibull plot of different angles
  # 
  
  # Data Preparation --------------------------------------------------------
  
  # import data containing lifetime data
  test_data <- as.data.table(test_data)
  
  # check if necessary columns and entries exist
  col_names <- colnames(test_data)
  
  # remove rows containing NA entries
  test_data <- na.omit(test_data)
  
  # Calculation -------------------------------------------------------------
  
  # estimate failure probabilities (fp) of copper ("Cu") and aluminium ("Al")
  fp_45  <- RT_getFailureProbabilities(test_data,  45)
  fp_90  <- RT_getFailureProbabilities(test_data,  90)
  fp_180 <- RT_getFailureProbabilities(test_data, 180)
  
  # do x on y regression ((log-) location-scale) 
  reg_45 <- rank_regression(x = fp_45$characteristic,
                            y = fp_45$prob, event = fp_45$status)
  
  reg_90 <- rank_regression(x = fp_90$characteristic,
                            y = fp_90$prob, event = fp_90$status)
  
  reg_180 <- rank_regression(x = fp_180$characteristic,
                             y = fp_180$prob, event = fp_180$status)
  
  # Plot Creation -------------------------------------------------------------
  
  # TODO HIER !!
  woehler_plot <- Woehlerlinie(x = test_data$Biegeanzahl,
                               y = test_data$Winkel,
                               mrr1 = reg_45,
                               mrr2 = reg_90,
                               mrr3 = reg_180)

  wb_plot <- RT_GetWeibullPlotFromTestData(test_data)
  woehler_plot <- subplot(wb_plot,woehler_plot ,nrows = 2, shareX = T)
  return(woehler_plot)
}

###############################################################################
