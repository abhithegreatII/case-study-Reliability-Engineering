# Falls es an dieser Stelle Fehlermeldungen gibt, müssen die Packages mit
# install.packages installiert werden
library(shiny)
# Dashboard
library(shinydashboard)
# Tidyverse
library(tidyverse)
# DT-Tabellen in shiny
library(DT)
# plotly-Plots
library(plotly)
# wird für source_directory benötigt
library(R.utils)
# Import von .xls- und .xlsx-Dateien
library(readxl)
# Import von .csv-Dateien
library(readr)
# Bearbeiten von Strings
library(stringr)
# Objektorientiertes System; z.B. TabBox ist ein R6-Objekt
library(R6)
# UI
library(shinyWidgets)
# Tooltips
library(shinyBS)
# Weibulltools
library(weibulltools)

# Source source_directory.R
source("./modules/predefined/source_directory.R", encoding = "UTF-8")

# Nutze source_directory, um gesamten Ordner zu sourcen; setze verbose = FALSE,
# um keine Mitteilungen in der Konsole zu sehen
source_directory(
  "./modules", encoding = "UTF-8", modifiedOnly = FALSE, chdir = TRUE, 
  verbose = TRUE, envir = globalenv()
)

# Erzeuge einen Viewer, in dem Plots und Tabellen in einzelnen Tabs dargestellt
# werden können
viewer <- TabBox$new(
  id = "viewer",
  title = "Viewer",
  width = 12
)

# Scrollen in zu breiten DT-Tabellen
options(DT.options = list(scrollX = TRUE))

ui <- div(
  tags$head(
    # Include custom css styles
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "styles.css"
    )
  ),
  dashboardPage(
    dashboardHeader(
      title = "DRE-App"
    ),
    dashboardSidebar(
      sidebarMenu(
        menuItem(
          text = "Dokumentation",
          menuSubItem(
            text = "1. Einleitung",
            tabName = "doku_einleitung"
          ),
          menuSubItem(
            text = "2. Versuchsaufbau",
            tabName = "doku_versuchsaufbau"
          ),
          menuSubItem(
            text = "3. Kostenkalkulation",
            tabName = "doku_kostenkalkulation"
          ),
          menuSubItem(
            text = "4. Versuchsdurchführung",
            tabName = "doku_versuchsdurchfuehrung"
          )
        ),
        menuItem(
          text = "Import",
          tabName = "import"
        ),
        menuItem(
          text = "Auswertungen",
          menuSubItem(
            text = "Drahtauswahl",
            tabName = "drahtauswahl"
          ),
          menuSubItem(
            text = "Raffungstest",
            tabName = "raffungstest"
          )
        )
      )
    ),
    dashboardBody(
      fluidRow(
        column(
          width = 6,
          tabItems(
            tabItem(
              tabName = "import",
              box(
                title = "Import",
                width = 12,
                excel_csv_file_input_ui(
                  id = "id_excel_csv_file_input"
                )
              )
            ),
            tabItem(
              tabName = "drahtauswahl",
              box(
                title = "Drahtauswahl",
                width = 12,
                drahtauswahl_ui(
                  id = "id_drahtauswahl"
                )
              )
            ),
            tabItem(
              tabName = "raffungstest",
              box(
                title = "Raffungstest",
                width = 12,
                raffungstest_ui(
                  id = "id_raffungstest"
                )
              )
            ),
            tabItem(
              tabName = "doku_einleitung",
              box(
                title = "1. Einleitung",
                width = 12,
                doku_einleitung_ui(
                  id = "id_doku_einleitung"
                )
              )
            ),
            tabItem(
              tabName = "doku_versuchsaufbau",
              box(
                title = "2. Versuchaufbau",
                width = 12,
                doku_versuchsaufbau_ui(
                  id = "id_doku_versuchsaufbau"
                )
              )
            ),
            tabItem(
              tabName = "doku_kostenkalkulation",
              box(
                title = "3. Kostenkalkulation",
                width = 12,
                doku_kostenkalkulation_ui(
                  id = "id_doku_kostenkalkulation"
                )
              )
            ),
            tabItem(
              tabName = "doku_versuchsdurchfuehrung",
              box(
                title = "4. Versuchsdurchführung",
                width = 12,
                doku_versuchsdurchfuehrung_ui(
                  id = "id_doku_versuchsdurchfuehrung"
                )
              )
            )
          )
        ),
        column(
          width = 6,
          # Container, in dem die Inhalte des Viewers dargestellt werden
          viewer$tabBox()
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Verknüpfe Viewer mit der session
  viewer$set_session(session)
  
  # Erzeuge eine Liste, die allen Modulen als Argument übergeben wird
  .values <- list(
    data_storage = ObjectStorage$new(),
    viewer = viewer 
  )
  
  # Rufe Module auf
  callModule(
    module = excel_csv_file_input,
    id = "id_excel_csv_file_input",
    .values = .values
  )
  
  callModule(
    module = drahtauswahl,
    id = "id_drahtauswahl",
    .values = .values
  )
  
  callModule(
    module = raffungstest,
    id = "id_raffungstest",
    .values = .values
  )
  
  callModule(
    module = doku_einleitung,
    id = "id_doku_einleitung",
    .values = .values
  )
  
  callModule(
    module = doku_versuchsaufbau,
    id = "id_doku_versuchsaufbau",
    .values = .values
  )
  
  callModule(
    module = doku_kostenkalkulation,
    id = "id_doku_kostenkalkulation",
    .values = .values
  )
  
  callModule(
    module = doku_versuchsdurchfuehrung,
    id = "id_doku_versuchsdurchfuehrung",
    .values = .values
  )

}

# Erzeuge die App
shinyApp(ui, server)