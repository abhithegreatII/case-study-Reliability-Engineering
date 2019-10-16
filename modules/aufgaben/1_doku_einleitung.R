
###############################################################################
#
# file:         "1_doku_einleitung.R"
#
# date:         2019-09-13
#
# authors:      Group 3
#
# brief:        This script contains the module implementation (shiny) of
#               "Dokumentation - Einleitung"-part of the Case Study 
#               (DRE SoSe 2019).
#
###############################################################################

doku_einleitung_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    htmlOutput(ns("text")),
    
    radioButtons(
      inputId = ns("rb"),
      label = "Optionen für Viewer",
      choiceNames = c(
        "Wissenswertes Büroklammer",
        "Wissenswertes Aluminium",
        "Wissenswertes Kupfer"),
      choiceValues = c(1, 2, 3)
    ),
    
    actionButton(ns("zeige_infos"), "Zeige Infos")
  )
}

###############################################################################

doku_einleitung <- function(input, output, session, .values) {
  
  ns <- session$ns
  
  output$text <- renderText({
    "__________________ <br/><br/>
    
    Die Firma <b>DRE Data Science SE</b> werte mechanische Schadenseinträge aus.
    Im Auftrag der Paperclip GmbH soll ein <b>neues Drahtmaterial für die Herstellung
    von Büroklammern</b> gefunden werden. Das bisher benutzte Material reagiert bei
    Biegebelastungen zu spröde. Im Rahmen dessen wird das Bruchverhalten zwei neuer
    Materialien, <b>Aluminium</b> und <b>Kupfer</b>, mittels Biegetests untersucht. Das Material
    mit den besseren Ermüdungseigenschaften wird im zweiten Teil mittels Raffungstests
    mit dem Ziel untersucht, die Prüfzeiten in Zukunft
    durch höhere Biegewinkel zu verkürzen.<br/><br/>
    
    Diese App ermöglicht es, schon in der Frühphase, die Entwicklung von Büroklammern zu überwachen.
    Dazu werden Versuchsdaten benötigt. <br/>
    __________________<br/><br/>"
  })
  
  observeEvent(input$rb, {
    
    output$viewer_header <- 
      renderText(
        "_______________________________________<br/><br/>")
      
    if (input$rb == 1) {
      
      output$viewer_text <- 
        renderText(
          "_______________________________________<br/><br/>
        
        <b>Wissenswertes zu Büroklammern:</b><br/><br/>
        
        - In den USA wurde im Jahre 1867 das erste Patent für eine Büroklammer eingereicht.<br/><br/>
        
        - Die heute bekannte, spitz zulaufende Form wurde 1919 vom 
        Österreicher Heinrich Sachs entworfen. <br/><br/>
        
        - Als die deutsche Wehrmacht im Zweiten Weltkrieg Norwegen besetzte, 
        dienten Büroklammern als politisches Symbol und Zeichen der Verbundenheit mit dem norwegischen König.<br/><br/> 
        
        - Am 29. Mai feiern die US-Amerikaner den <b>Tag der Büroklammer</b>! :)<br/><br/>
        
        _______________________________________<br/><br/>
        
        Quelle: https://blog.otto-office.com/die-bueroklammer
        
        ")
    } else 
    if (input$rb == 2) {
      
      output$viewer_text <- 
        renderText(
          "_______________________________________<br/><br/>
        
        <b>Wissenswertes zu Aluminium:</b><br/><br/>
        
        - Bezeichnung: Al, Aluminium <br/><br/>
        
        - Dichte: 2,7 g/cm³ <br/><br/>
        
        - E-Modul: 70 GPa <br/><br/>
        
        - Die Gitterstruktur von Aluminium ist kubisch-flächenzentriert (KFZ). <br/><br/>
        
        _______________________________________<br/><br/>
        
        Quelle: https://de.wikipedia.org/wiki/Aluminium
        
        ")
    } else {
      output$viewer_text <- 
        renderText(
          "_______________________________________<br/><br/>
        
        <b>Wissenswertes zu Kupfer:</b><br/><br/>
        
        - Bezeichnung: Cu, Kupfer <br/><br/>
        
        - Dichte: 8,96 g/cm³ <br/><br/>
        
        - E-Modul: 100...130 GPa <br/><br/>
        
        - Die Gitterstruktur von Kupfer ist kubisch-flächenzentriert (KFZ). <br/><br/>
        
        _______________________________________<br/><br/>
        
        Quelle: https://de.wikipedia.org/wiki/Kupfer
        
        ")
    }
    
    
    output$viewer_image <- renderUI({
      if (input$rb == 1) {
        HTML('<img src="paperclip.png">')
      } else
      if (input$rb == 2) {
        HTML('<img src="aluminium.png">')
      } else {
        HTML('<img src="kupfer.png">')
      }
    })
  })
  
  observeEvent(input$zeige_infos, {
    
    
    tab_name <- "Einleitung"
    
    .values$viewer$append_tab(
      tab = tabPanel(
        title = tab_name,
        htmlOutput(outputId = ns("viewer_header")),
        uiOutput(outputId = ns("viewer_image")),
        htmlOutput(outputId = ns("viewer_text"))
      )
    )
  })
}

