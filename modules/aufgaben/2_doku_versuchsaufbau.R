
###############################################################################
#
# file:         "2_doku_versuchsaufbau.R"
#
# date:         2019-09-11
#
# authors:      Group 3
#
# brief:        This script contains the module implementation (shiny) of
#               "Dokumentation - Versuchsaufbau"-part of the Case Study 
#               (DRE SoSe 2019).
#
###############################################################################

doku_versuchsaufbau_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    htmlOutput(ns("text")),
    actionButton(ns("zeige_versuchsaufbau"), "Zeige Versuchsaufbau")
  )
}

###############################################################################

doku_versuchsaufbau <- function(input, output, session, .values) {
  ns <- session$ns
  
  output$text <- renderText({
    
    "__________________ <br/><br/>
    
    Die Biegetests zu Materialwahl und die Raffungstests werden mittels eines
    <b>Roboterarmes (1)</b> durchgeführt. Der <b>Draht (2)</b> wird am Stativ zwischen zwei 
    Blechen mit <b>Schraubzwinge (3)</b> eingespannt. Anschließend wird mit der <b>Bedienoberfläche (4)</b> des Roboters das Biegeprogramm gestartet
    Bei der Einspannung wird auf eine möglichst gleichmäße Vorspannung geachtet. Die scharfen Kanten der Spanbleche werden mit Duct Tape umwickelt.
    
    <br/>__________________<br/><br/>"
  })
  
  
  observeEvent(input$zeige_versuchsaufbau, {
    
    output$versuchsaufbau_header<- 
      renderText(
        "_______________________________________<br/><br/>
        
        <b>Aufbau des Biegeversuchs:</b><br/><br/>
        
        ")
    
    output$versuchsaufbau_text<- 
      renderText(
        " <br/>
          - (1): Roboterarm <br/><br/> 
          - (2): Draht <br/><br/>
          - (3): Spannvorrichtung <br/><br/>
          - (4): Programmier-Panel
        <br/>_______________________________________<br/><br/>")
    
    tab_name <- "Versuchsaufbau"
    
    .values$viewer$append_tab(
      tab = tabPanel(
        title = tab_name,
        htmlOutput(outputId = ns("versuchsaufbau_header")),
        HTML('<img src="versuchsaufbau.png">'),
        htmlOutput(outputId = ns("versuchsaufbau_text"))
      )
    )
  })
}