
###############################################################################
#
# file:         "drahtauswahl.R"
#
# date:         2019-09-10
#
# authors:      Group 3
#
# brief:        This script contains the module implementation (shiny) of
#               "Drahtauswahl"-part of the Case Study (DRE SoSe 2019).
#
# References:   https://rstudio.github.io/shiny/tutorial/#
#               https://shiny.rstudio.com/articles/reactivity-overview.html
#
###############################################################################

library(data.table)

###############################################################################

drahtauswahl_ui <- function(id) {
  #
  # UI-Implementation of drahtauswahl-module creates two action buttons.
  #
  # Action Button 1: Add Weibull Parameters 
  # Action Button 2: Add Weibull Plot
  #
  ns <- NS(id)
  
  tagList(
    data_selector_ui(
      id = ns("id_data_selector")
    ),
    actionButtonQW(
      inputId = ns("da_add_weibull_parameter"),
      label = NULL,
      icon = icon("table"),
      tooltip = "Zeige Weibull-Parameter"
    ),
    actionButtonQW(
      inputId = ns("da_add_weibull_plot"),
      label = NULL,
      icon = icon("area-chart"),
      tooltip = "Öffne Weibull-Plot"
    )
  )
}

###############################################################################

drahtauswahl <- function(input, output, session, .values) {
  #
  # Server-Implementation of drahtauswahl-module mainly consists of two events.
  #
  # Event 1: try to add weibull parameters for drahtauswahl-task in a new tab,
  #          if corresponding action button is pressed
  #
  # Event 2: try to add weibull plot for drahtauswahl-task in a new tab,
  #          if corresponding action button is pressed
  #
  ns <- session$ns
  
  # Event 1 - Weibull Parameter Button Pressed --------------------------------
  observeEvent(input$da_add_weibull_parameter, {
    
    test_data_name <- ""
    
    # try to add a new tab with the weibull parameters, catch with error msg
    tryCatch(
      # TRY
      {
        # import test data from selected excel sheet
        test_data <- data_selector_return$data()
        test_data_name <- data_selector_return$name()
        
        # define data.table for new tab
        wb_param_dt <- DA_GetWeibullParametersFromTestData(test_data)
        
        # define reactive endpoint of data.table + title
        
        output[[paste("da_weibull_parameter_title", test_data_name)]] <- 
          renderUI({HTML(paste("", paste("<b>","Weibull-Parameter::","</b>"),
                               sep="<br/>"))})
        
        output[[paste("da_weibull_parameter", test_data_name)]] <-
          renderDataTable({wb_param_dt}, options = list(
            dom = "t",
            columnDefs = list(list(className = 'dt-center', targets = 0:ncol(wb_param_dt))),
            ordering = F))
        
        # create new tab in viewer with wb parameters
        
        tab_name <- paste("WB-Parameter Drahtauswahl für Datensatz:", 
                          data_selector_return$name())
        
        .values$viewer$append_tab(
          tab = tabPanel(
            title = tab_name,
            htmlOutput(
              outputId = ns(paste("da_weibull_parameter_title", test_data_name))
            ),
            dataTableOutput(
              outputId = ns(paste("da_weibull_parameter", test_data_name))
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
                            Drahtauswahl-Weibull-Parameter
                            angezeigt werden. Mögliche Fehlerursache: 
                            \"%s\" ist ein inkorrekter Datensatz!",
                             test_data_name)
          
          showNotification(err_msg, type = "error", duration = 10)
        }
      })
  })
  
  
  # Event 2 - Weibull Plot Button Pressed -------------------------------------
  observeEvent(input$da_add_weibull_plot, {
    
    test_data_name <- ""
    
    # try to add a new tab with weibull plot, catch with error msg
    tryCatch(
    # TRY
    {
      # import test data from selected excel sheet
      test_data <- data_selector_return$data()
      test_data_name <- data_selector_return$name()
      
      # define plot for new tab
      wb_plot <- DA_GetWeibullPlotFromTestData(test_data)
      
      # define reactive endpoint of plot
      output[[paste("da_weibull_plot", test_data_name)]] <- 
        renderPlotly({wb_plot})
      
      # define name for new tab
      tab_name <- paste("WB-Plot Drahtauswahl für Datensatz:", 
                        test_data_name)
      
      # create new tab in viewer with wb_plot
      .values$viewer$append_tab(
        tab = tabPanel(
          title = tab_name,
          
          plotlyOutput(
            outputId = ns(paste("da_weibull_plot", test_data_name))
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
        err_msg <- sprintf("FEHLER: Es kann kein Drahtauswahl-Weibull-Plot 
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

###############################################################################
