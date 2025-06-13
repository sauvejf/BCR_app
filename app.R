library(shiny)
library(DT)
library(dplyr)
# Sample data.frame loaded on startup
# bcr_data <- data.frame(
#   ID = 1:10,
#   Name = paste("Item", 1:10),
#   Value = round(runif(10, 10, 100), 2),
#   stringsAsFactors = FALSE
# )


# bcr_data <- read_xlsx("Data BCR.xlsx", sheet="Pour_R")
# bcr_conc <- read_xlsx("Data BCR.xlsx", sheet="BCR")
bcr_data <- readRDS("bcr_data.Rds")
bcr_conc <- readRDS("bcr_conc.Rds")

ui <- fluidPage(
  titlePanel("Outil d'identification de bandes de concentration de référence"),
  
  # First Pane: Selection input and action button
  fluidRow(
    column(6,
           selectInput("selected_h",
                       "Phrases de risque de la substance:",
                       choices = bcr_data$Code,
                       multiple = TRUE)
    ),
    column(2,
           br(),
           actionButton("submit", "Calcul de la bande de concentration")
    )
  ),
  
  hr(),
  
  # Second Pane: Output filtered data.frame
  fluidRow(
    column(12,
           DTOutput("filtered_table"),
           br()
    )
  ),
  
  hr(),
  
  fluidRow(
    column(12,
           DTOutput("BCR_table"))
  )
)

server <- function(input, output, session) {
  # Reactive value to store the filtered rows
  selected_data <- reactiveVal(bcr_data[0, ])
  show_output <- reactiveVal(FALSE)
  
  # On button click, filter data and update reactive value
  observeEvent(input$submit, {
    req(input$selected_h)  # Ensure at least one ID is selected
    filtered <- bcr_data[bcr_data$Code %in% input$selected_h, 
                         c("Code", "Libelle_FR_short", "Bande_Danger")]
    # filtered <- arrange(filtered, desc(Bande_Danger))
    selected_data(filtered)
    show_output(TRUE)
    
  })
  
  # Render the table
  output$filtered_table <- renderDT({
    req(show_output())
    datatable(selected_data(),
              caption = "Phrases de risque de la substance",
              options = list(paging = FALSE, searching = FALSE))
  })
  
  # Render the BCR table
  output$BCR_table <- renderDT({
    req(show_output())
    if(nrow(selected_data()>1)){
      max_BCR <- max(selected_data()$Bande_Danger)
      conc_BCR <- bcr_conc$Bande_Conc[bcr_conc$Bande_Danger==max_BCR]
      
      out <- data.frame(Parametre=c("Bande de danger la plus restrictive",
                                    "Bande de concentration associée (gaz/vapeurs)"),
                        Valeur=c(as.character(max_BCR), conc_BCR))
      datatable(out,
                caption = "Bande de danger et bande de concentration",
                options = list(paging = FALSE, searching = FALSE))
    } else{
      selected_data()
    }
  }, options = list(paging = FALSE, searching = FALSE))
  
}

shinyApp(ui, server)