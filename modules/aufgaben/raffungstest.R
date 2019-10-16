
###############################################################################
#
# file:         "raffungstest.R"
#
# date:         2019-09-11
#
# authors:      Group 3
#
# brief:        This script contains the module implementation (shiny) of
#               "Raffungstest"-part of the Case Study (DRE SoSe 2019).
#
# References:   https://rstudio.github.io/shiny/tutorial/#
#               https://shiny.rstudio.com/articles/reactivity-overview.html
#
###############################################################################

library(data.table)

###############################################################################

raffungstest_ui <- function(id) {
  #
  # UI-Implementation of raffungstest-module creates three action buttons.
  #
  # Action Button 1: Add Weibull Parameters
  # Action Button 2: Add Woehler Plot
  #
  ns <- NS(id)
  
  tagList(
    data_selector_ui(
      id = ns("id_data_selector")
    ),
    actionButtonQW(
      inputId = ns("rt_add_weibull_parameter"),
      label = NULL,
      icon = icon("table"),
      tooltip = "Zeige Weibull-Parameter"
    ),
    actionButtonQW(
      inputId = ns("rt_add_woehler_plot"),
      label = NULL,
      icon = icon("area-chart"),
      tooltip = "Öffne Woehler-Plot"
    )
  )
}

###############################################################################

raffungstest <- function(input, output, session, .values) {
  #
  # Server-Implementation of drahtauswahl-module mainly consists of 2 events.
  #
  # Event 1: try to add weibull parameters for drahtauswahl-task in a new tab,
  #          if corresponding action button is pressed
  #
  #
  # Event 2: try to add weibull and woehler plot for raffungstest-task in a 
  #          new tab, if corresponding action buttong is pressed
  #
  ns <- session$ns
  
  
  # Event 1  - Weibull Parameter Button pressed ----------------------------
  observeEvent(input$rt_add_weibull_parameter, {
    
    test_data_name <- ""
    # try to add a new tab with the weibull parameters, catch with error msg
    tryCatch(
      # TRY
      {
        # import test data from selected excel sheet
        test_data <- data_selector_return$data()
        test_data_name <- data_selector_return$name()
        
        # calculate data.table for new tab
        wb_param_dt <- RT_GetWeibullParametersFromTestData(test_data)
        acc_fac_dt  <- RT_GetAccelerationFactorFromTestData(test_data)
        
        
        # set reactive endpoints of 2 data.tables + 2 title
        
        output[[paste("rt_weibull_parameter_title", test_data_name)]] <- 
          renderUI({HTML(paste("", paste("<b>","Weibull-Parameter::","</b>"),
                               sep="<br/>"))})
        
        output[[paste("rt_weibull_parameter", test_data_name)]] <-
          renderDataTable({wb_param_dt}, options = 
            list(dom = "t",
            columnDefs = list(list(className = 'dt-center', targets = 0:ncol(wb_param_dt))),
            ordering = F))
        
        
        output[[paste("rt_acceleration_factor_title", test_data_name)]] <- 
          renderUI({HTML(paste("", paste("", 
                                     paste("<b>","Raffungsfaktoren:","</b>"),
                        sep="<br/>"), sep="<br/>"))})
        
        output[[paste("rt_acceleration_factor", test_data_name)]] <-
          renderDataTable({acc_fac_dt}, options = 
            list(dom = "t",
            columnDefs = list(list(className = 'dt-center', targets = 0:ncol(acc_fac_dt))),
            ordering = F))
        
        
        
        # create new tab in viewer with wb parameters
        
        tab_name <- paste("WB-Parameter Raffungstest für Datensatz:", 
                          data_selector_return$name())
        
        .values$viewer$append_tab(
          tab = tabPanel(
            title = tab_name,
            htmlOutput(
              outputId = ns(paste("rt_weibull_parameter_title", test_data_name))
            ),
            dataTableOutput(
              outputId = ns(paste("rt_weibull_parameter", test_data_name))
            ),
            htmlOutput(
              outputId = ns(paste("rt_acceleration_factor_title", test_data_name))
            ),
            dataTableOutput(
              outputId = ns(paste("rt_acceleration_factor", test_data_name))
            )
          )
        )
        # CATCH
      }, error = function(cond) {
        if (test_data_name == "") {
          showNotification("FEHLER: Es wurde kein Datensatz erkannt! Mögliche
                         Fehlerursache: Inkorrekter bzw. kein Datenimport!", 
                           type = "error", duration = 10)
          
        } else {
          err_msg <- sprintf("FEHLER: Es können keine 
                            Raffungstest-Weibull-Parameter
                            angezeigt werden. Mögliche Fehlerursache: 
                            \"%s\" ist ein inkorrekter Datensatz!",
                             test_data_name)
          
          showNotification(err_msg, type = "error", duration = 10)
        }
      })
  })
  
  # Event 2  - Woehler Plot Button pressed ------------------------------------
  observeEvent(input$rt_add_woehler_plot, {
    
    test_data_name <- ""
    
    # try to add a new tab with woehler plot, catch with error msg
    tryCatch(
      # TRY
      {
        # import test data from selected excel sheet
        test_data <- data_selector_return$data()
        test_data_name <- data_selector_return$name()
        
        # define plot for new tab
        woehler_plot <- RT_GetWoehlerPlotFromTestData(test_data)
        
        # define reactive endpoint of plot
        output[[paste("rt_woehler_plot", test_data_name)]] <-
          renderPlotly({woehler_plot})
        
        # define name for new tab
        tab_name <- paste("Woehler-Plot Raffungstest für Datensatz:",
                          test_data_name)
        
        # create new tab in viewer with woehler_plot
        .values$viewer$append_tab(
          tab = tabPanel(
            title = tab_name,
            
            plotlyOutput(
              outputId = ns(paste("rt_woehler_plot", test_data_name))
            )
          )
        )
        # CATCH
      }, error = function(cond) {
        if (test_data_name == "") {
          showNotification("FEHLER: Es wurde kein Datensatz erkannt! Mögliche
                         Fehlerursache: Inkorrekter bzw. kein Datenimport!",
                           type = "error", duration = 10)
          
        } else {
          err_msg <- sprintf("FEHLER: Es kann kein Raffungstest-Woehler-Plot
                            erstellt werden. Mögliche Fehlerursache:
                            \"%s\" ist ein inkorrekter Datensatz!",
                             test_data_name)
          
          showNotification(err_msg, type = "error", duration = 10)
        }
      })
  })
  
  # ENDE ----------------------------------------------------------------------
  
  data_selector_return <- callModule(
    module = data_selector,
    id = "id_data_selector",
    .values = .values
  )
}