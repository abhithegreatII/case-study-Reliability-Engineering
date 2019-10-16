
###############################################################################
#
# file:         "3_doku_kostenkalkulation.R"
#
# date:         2019-09-13
#
# authors:      Group 3
#
# brief:        This script contains the module implementation (shiny) of
#               "Dokumenation - Kostenkalkulation"-part of the Case Study 
#               (DRE SoSe 2019).
#
###############################################################################

doku_kostenkalkulation_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    htmlOutput(ns("text"))
  )
}

###############################################################################

doku_kostenkalkulation <- function(input, output, session, .values) {
  
  ns <- session$ns
  
  output$text <- renderText({
    
    "__________________ <br/><br/>
    
    Für die Untersuchungen steht ein <b>Budget von 600.000 €</b> zur Verfügung.
    Für die Drahtauswahl wurden pro Material 20 Drähte à einer Länge von 33 cm getestet.
    Hinzu kommt jeweils ein Testdraht. Die Materialkosten belaufen sich auf 3.000 € pro Meter.
    Die Versuchskosten eines Drahtes betragen 5.000 €.
    Durch Einrichtung der Maschine (50.000 €) und des Biegewinkels von 45° (10.000 €) entstehen 
    weitere 60.000 € Kosten. Die Kosten des ersten Test betragen somit xx €.
    Für die beiden Raffungstests werden jeweils wieder 21 Drähte gleicher Länge und
    Einzelversuchskosten getestet. Durch den zweimaligen Wechsel der Biegewinkel entstehen 
    zusätzlich 20.000 € Kosten
    
    <br/><br/>__________________<br/><br/>"
  })
}