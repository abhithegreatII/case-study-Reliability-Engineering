
###############################################################################
#
# file:         "4_doku_versuchsdurchfuerhung.R"
#
# date:         2019-09-13
#
# authors:      Group 3
#
# brief:        This script contains the module implementation (shiny) of
#               "Dokumentation - Versuchsdurchführung"-part of the Case Study 
#               (DRE SoSe 2019).
#
###############################################################################


doku_versuchsdurchfuehrung_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    htmlOutput(ns("text")),
    tags$video(id="test_mp4", type = "video/mp4", 
               width="400", src = "test.mp4", controls = "controls"),
    htmlOutput(ns("text2"))
  )
}

###############################################################################

doku_versuchsdurchfuehrung <- function(input, output, session, .values) {
  
  ns <- session$ns
  
  output$text <- renderText({
    
    "__________________ <br/><br/>
    
    Wie in dem Versuchsaufbau beschrieben, wird der Draht eingespannt.
    Dabei werden vorab auf dem Inbus mehrere Drähte gewickelt, um den Wechselprozess zu beschleungigen.
    Beim Einspannen der anderen Seite des Drahtes zwischen den Blechen wird darauf geachtet, die Vorspannung im Draht
    sowie die Klemmkraft der Schraubzwinge mögliches konstant zu halten, um deren Einfluss auf den Versagensprozess nicht zu
    stark variieren zu lassen. <br/><br/>
    
    Anschließend wird die Bewegung des Roboterarmes über das Controllpanel gestartet. Die Biegeanzahl
    bis zum Bruch wird mittels Zähluhr aufgezeichnet und in eine Excel-Tabelle eingetragen.
    Beide Materialien durch 20 Prüfdrähte mit einem Biegewinkel von 45° getestet. Durch eine Auswertung der Weibullparametern wurde das
    Material (Cu) mit dem besseren Betriebsverhalten ausgewählt. <br/><br/>
    
    Mit diesem Material wurden anschließend die Raffungstests durchgeführt. Dabei werden erneut 20 Drähte mit anderen Biegewinkel (90° und 180°) getestet
    und erneut Weibullparameter bestimmt. Die Quotienten der neuen Lageparamter T zum Ausgangstest mit 45° geben die Raffungsfaktoren an. Diese bilden
    die Verkürzung der Versuchszeit bis 63 % aller Drähte ausgefallen sind. <br/><br/>"
    
  })
  
  output$text2 <- renderText({
    "<br/>Durch den Versuchsaufbau ergeben sich mehrere Fehlerquellen, die die Ergebnisse beeinflussen. Es sind zum einen die 
    nicht präzise einstellbare Vorspannung des Drahtes und der Einspannung mit der Schraubzwinge zu nennen. Beide haben einen
    Einfluss auf den Spannungszustand im Draht.
    Außerdem vollzieht der Roboterarm keine perfekte Kreisbewegung. Der Verfahrweg (siehe Video) führt zu einem Reißen am Draht.
    Dies sieht man daran, dass sich das ganze Stativ mitbewegt, wenn der Roboter die Bewegung ausführt. Auch dies führt zu zusätzlichen 
    Belastungen im Draht und verringert vermutlich die gemessene Biege-Schwingfestigkeit. Ein Vergleich zu der Messung mit dem Servo
    gibt eventuelle Einflüsse preis.
     Bei größeren Biegewinkeln wurde beobachtet, dass eine Biegung zu einer starken lokalen
 Verfestigung führen. Beim Rückbiegen wird nicht wieder die gleiche Stelle verformt und belastet. Dies könnte für den hohen
    Exponenten b im 90°-Test erklären. Als Grund dafür ist mangelnde Vorspannung im System zu nennen. 
    
    <br/>__________________<br/><br/>"
    
  })
      
}