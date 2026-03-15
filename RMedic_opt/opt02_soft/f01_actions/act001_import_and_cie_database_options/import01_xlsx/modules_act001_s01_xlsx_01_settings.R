# XLSX files import
module_act001_s01_xlsx_01_settings_ui <- function(id){
  ns <- shiny::NS(id)
  
  div(
    uiOutput(ns("iu_base_selector")),
    uiOutput(ns("ui_action_button"))  # Botón de acción siempre visible
  )
}

#' Server para el módulo de configuración de Excel (XLSX)
#'
#' @param id ID del módulo
#' @param sui_data_source Reactive que indica la fuente seleccionada
#' @return Reactive con la lista de configuración confirmada o NULL
module_act001_s01_xlsx_01_settings_server <- function(id, sui_data_source){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- 1. ESTADOS REACTIVOS ---
    confirmed_data <- reactiveVal(NULL)
    button_state   <- reactiveVal("initial") # initial, confirmed, modified, error
    error_message  <- reactiveVal(NULL)
    
    # Validación de pertenencia a este módulo
    check_ok <- reactive({
      req(sui_data_source())
      sui_data_source() == "source_xlsx"
    })
    
    # --- 2. RENDERIZADO DE INTERFAZ (UI) ---
    
    # Selector de Archivo
    output$iu_base_selector <- renderUI({
      req(check_ok())
      tagList(
        fileInput(ns("selected_input_file"), "Elige un archivo xlsx", 
                  accept = c(".xlsx"), width = "100%"),
        uiOutput(ns("ui_sheet_selector"))
      )
    })
    
    # Selector de Hoja (Dinámico)
    output$ui_sheet_selector <- renderUI({
      req(input$selected_input_file)
      
      # Extraer nombres de hojas sin cargar datos
      tryCatch({
        sheets <- readxl::excel_sheets(input$selected_input_file$datapath)
        selectInput(ns("selected_sheet"), "Selecciona la hoja a importar:",
                    choices = c("Seleccione una..." = "", sheets))
      }, error = function(e) {
        div(style="color:red", "Error al leer las hojas del archivo.")
      })
    })
    
    # Botón de Acción con Bloqueo
    output$ui_action_button <- renderUI({
      req(check_ok())
      
      # Estilo dinámico
      btn_class <- switch(button_state(),
                          "initial"   = "btn-primary",
                          "confirmed" = "btn-success",
                          "modified"  = "btn-primary",
                          "error"     = "btn-danger")
      
      # LÓGICA DE BLOQUEO: Deshabilitar si no hay selección O si ya está confirmado
      is_disabled <- is.null(input$selected_input_file) || 
        is.null(input$selected_sheet) || 
        input$selected_sheet == "" ||
        button_state() == "confirmed"
      
      div(style = "margin-top: 15px;",
          actionButton(
            inputId = ns("confirm_selection"), 
            label   = if(button_state() == "confirmed") "Selección Confirmada" else "Confirmar selección",
            icon    = icon(if(button_state() == "confirmed") "check-double" else "check"), 
            class   = btn_class, 
            width   = "100%",
            disabled = is_disabled
          ),
          
          # Mensajes de guía al usuario
          if (is.null(input$selected_input_file) || is.null(input$selected_sheet) || input$selected_sheet == "") {
            div(style = "margin-top: 10px; color: #e57373; font-style: italic; font-weight: bold",
                icon("arrow-up"), " Sube un archivo y elige una hoja")
          } else if (button_state() == "confirmed") {
            div(style = "margin-top: 10px; color: green; font-weight: bold;", 
                icon("info-circle"), " La base de datos está lista.")
          } else if (button_state() == "modified") {
            div(style = "margin-top: 10px; color: #0275d8; font-style: italic;",
                icon("exclamation-circle"), " Cambios detectados. Confirma para actualizar.")
          }
      )
    })
    
    # --- 3. LÓGICA DE RESET Y CONTROL ---
    
    # RESET INSTANTÁNEO: Si cambia el archivo o la hoja, invalidamos los datos
    # Esto dispara la desaparición de la tabla y la aparición del logo RMedic
    observeEvent(list(input$selected_input_file, input$selected_sheet), {
      confirmed_data(NULL) 
      error_message(NULL)
      
      # Si el usuario cambia algo, el botón deja de estar verde y se habilita de nuevo
      if (button_state() == "confirmed") {
        button_state("modified")
      }
    }, ignoreInit = TRUE)
    
    # Lógica de Confirmación
    observeEvent(input$confirm_selection, {
      req(input$selected_input_file, input$selected_sheet)
      
      path <- input$selected_input_file$datapath
      hoja <- input$selected_sheet
      
      tryCatch({
        # Almacenamos la lista que espera el módulo de Importación
        confirmed_data(list(
          "data_source"         = sui_data_source(),
          "selected_input_file" = input$selected_input_file,
          "selected_sheet"      = hoja,
          "list_extra"          = list(
            "temporal_file_path" = normalizePath(path, winslash = "/", mustWork = FALSE),
            "original_file_name" = input$selected_input_file$name
          )
        ))
        
        button_state("confirmed")
        showNotification(paste("Hoja", hoja, "confirmada."), type = "message")
        
      }, error = function(e) {
        button_state("error")
        showNotification("Error al procesar la selección.", type = "error")
      })
    })
    
    # Reset si cambia la fuente global (ej. de Excel a RData)
    observeEvent(sui_data_source(), {
      confirmed_data(NULL)
      button_state("initial")
    })
    
    # --- 4. RETORNO ---
    return(reactive({
      if (!check_ok()) return(NULL)
      confirmed_data() 
    }))
  })
}
