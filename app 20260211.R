library(shiny)
library(DT)
library(dplyr)

bcr_data <- readRDS("bcr_data.Rds")
bcr_conc <- readRDS("bcr_conc.Rds")

ui <- fluidPage(
  titlePanel("Outil d'aide à l'identification de bandes de concentration de référence"),
  
  # First Pane: Selection input, paste area and action button
  fluidRow(
    column(
      6,
      selectInput(
        "selected_h",
        "Mention(s) de danger de la substance:",
        choices = bcr_data$Code,
        multiple = TRUE
      ),
      br(),
      textAreaInput(
        inputId = "hazard_paste",
        label = "Ou collez les codes H3xx / EUHxxx (1 par ligne):",
        placeholder = "Exemple:\nH331\nH373\nEUH014",
        rows = 8,
        width = "100%"
      )
    ),
    column(
      2,
      br(),
      actionButton("submit", "Calcul de la bande de concentration")
    )
  ),
  
  hr(),
  
  # Second Pane: Output filtered data.frame
  fluidRow(
    column(
      12,
      DTOutput("filtered_table"),
      br()
    )
  ),
  
  hr(),
  
  fluidRow(
    column(
      12,
      DTOutput("BCR_table")
    )
  )
)

server <- function(input, output, session) {
  
  # ---- Helper: parse pasted text into codes (Hxxx / EUHxxx) ----
  parse_hazard_codes <- function(txt) {
    if (is.null(txt) || !nzchar(txt)) return(character(0))
    txt_up <- toupper(txt)
    
    # Extract tokens even if pasted "in bulk", e.g. "H260 H314\nEUH014"
    tokens <- unlist(regmatches(txt_up, gregexpr("\\bEUH\\d{3}\\b|\\bH\\d{3}\\b", txt_up)))
    unique(tokens)
  }
  
  allowed_codes <- reactive({
    unique(na.omit(as.character(bcr_data$Code)))
  })
  
  # Guard to avoid infinite loops while syncing
  sync <- reactiveValues(updating = FALSE)
  
  # A) Paste text -> updates selectInput("selected_h")
  observeEvent(input$hazard_paste, {
    req(allowed_codes())
    if (isTRUE(sync$updating)) return()
    
    sync$updating <- TRUE
    on.exit({ sync$updating <- FALSE }, add = TRUE)
    
    pasted <- parse_hazard_codes(input$hazard_paste)
    ok <- intersect(pasted, allowed_codes())
    unknown <- setdiff(pasted, ok)
    
    updateSelectInput(session, "selected_h", selected = ok)
    
    if (length(unknown) > 0) {
      # showNotification(
      #   paste0("Codes inconnus / hors liste ignorés : ", paste(unknown, collapse = ", ")),
      #   type = "warning",
      #   duration = 6
      # )
    }
  }, ignoreInit = TRUE)
  
  # B) selectInput -> updates text area (one code per line)
  observeEvent(input$selected_h, {
    if (isTRUE(sync$updating)) return()
    
    sync$updating <- TRUE
    on.exit({ sync$updating <- FALSE }, add = TRUE)
    
    sel <- input$selected_h
    if (is.null(sel)) sel <- character(0)
    
    updateTextAreaInput(session, "hazard_paste", value = paste(sel, collapse = "\n"))
  }, ignoreInit = TRUE)
  
  
  # Reactive value to store the filtered rows
  selected_data <- reactiveVal(bcr_data[0, ])
  show_output <- reactiveVal(FALSE)
  
  # On button click, filter data and update reactive value
  observeEvent(input$submit, {
    req(input$selected_h)  # Ensure at least one code is selected
    
    filtered <- bcr_data[bcr_data$Code %in% input$selected_h,
                         c("Code", "Libelle_FR_short", "Bande_Danger")]
    
    # Optionnel: trier par bande de danger décroissante
    # filtered <- arrange(filtered, desc(Bande_Danger))
    
    selected_data(filtered)
    show_output(TRUE)
  })
  
  # Render the table
  output$filtered_table <- renderDT({
    req(show_output())
    datatable(
      selected_data(),
      caption = "Mentions de danger prises en compte",
      colnames = c("Code", "Libellé", "Bande de danger"),
      rownames = FALSE,
      options = list(paging = FALSE, 
                     searching = FALSE,
                     info     = FALSE,
                     ordering = FALSE)
    )
  })
  
  # Render the BCR table
  output$BCR_table <- renderDT({
    req(show_output())
    
    if (nrow(selected_data()) >= 1) {
      max_BCR <- max(selected_data()$Bande_Danger)
      conc_ppm <- bcr_conc$Bande_Conc_ppm[bcr_conc$Bande_Danger == max_BCR]
      conc_mgm3 <- bcr_conc$Bande_Conc_mgm3[bcr_conc$Bande_Danger == max_BCR]
      
      out <- data.frame(
        Parametre = c(
          "Bande de danger la plus restrictive",
          "Bande de concentration associée (gaz/vapeurs)",
          "Bande de concentration associée (aérosols)"
        ),
        Valeur = c(as.character(max_BCR), conc_ppm, conc_mgm3)
      )
      
      datatable(
        out,
        caption = "Bande de danger et bandes de concentration correspondantes",
        colnames = c("Paramètre", "Valeur"),
        rownames = FALSE,
        options = list(paging = FALSE, 
                       searching = FALSE,
                       info     = FALSE,
                       ordering = FALSE)
      )
    } else {
      # Si une seule ligne sélectionnée, afficher juste la table filtrée (ou adaptez selon votre souhait)
      datatable(
        selected_data(),
        caption = "Bande de danger (sélection unique)",
        rownames = FALSE,
        options = list(paging = FALSE, 
                       searching = FALSE,
                       info     = FALSE,
                       ordering = FALSE)
      )
    }
  })
}

shinyApp(ui, server)
