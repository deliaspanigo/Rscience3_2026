module_act001_s04_Rdata_01_settings_ui <- function(id) {
  ns <- shiny::NS(id)
  
  div(
    uiOutput(ns("iu_base_selector")),
    uiOutput(ns("ui_action_button"))
  )
}

#' Server para el módulo de configuración de RData
#'
#' @param id ID del módulo
#' @param sui_data_source Reactive que indica la fuente seleccionada
#' @return Reactive con la lista de configuración confirmada o NULL
module_act001_s04_Rdata_01_settings_server <- function(id, sui_data_source) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      # --- 1. ESTADOS REACTIVOS ---
      confirmed_data <- reactiveVal(NULL)
      button_state <- reactiveVal("initial")
      
      check_ok <- reactive({
        req(sui_data_source())
        sui_data_source() == "source_Rdata"
      })
      
      # --- 2. RENDERIZADO DE INTERFAZ (UI) ---
      
      output$iu_base_selector <- renderUI({
        req(check_ok())
        
        vector_opt <- c("01 - mtcars"      = "mtcars",
                        "02 - iris"        = "iris",
                        "03 - airquality"  = "airquality")
        
        vector_opt <- c("Seleccione una..." = "", vector_opt)
        
        shiny::selectInput(
          inputId = ns("selected_input_file"),
          label = "Bases de R",
          choices = vector_opt
        )
      })
      
      output$ui_action_button <- renderUI({
        req(check_ok())
        
        # Estilo dinámico del botón
        btn_class <- switch(button_state(),
                            "initial"   = "btn-primary", # Azul
                            "confirmed" = "btn-success", # Verde
                            "modified"  = "btn-primary") # Azul
        
        # LÓGICA DE BLOQUEO:
        # Se inhabilita si: 
        # 1. No hay nada seleccionado.
        # 2. YA está confirmado (button_state == "confirmed").
        is_disabled <- is.null(input$selected_input_file) || 
          input$selected_input_file == "" || 
          button_state() == "confirmed"
        
        div(
          style = "margin-top: 15px;",
          actionButton(
            inputId = ns("confirm_selection"),
            label = if(button_state() == "confirmed") "Selección Confirmada" else "Confirmar selección",
            icon = icon(if(button_state() == "confirmed") "check-double" else "check"),
            class = btn_class,
            width = "100%",
            disabled = is_disabled
          ),
          
          # Mensajes de estado
          if (is.null(input$selected_input_file) || input$selected_input_file == "") {
            div(style = "margin-top: 10px; color: #e57373; font-style: italic; font-weight: bold",
                icon("arrow-up"), " Selecciona una base de datos de R")
          } else if (button_state() == "confirmed") {
            div(style = "margin-top: 10px; color: green; font-weight: bold;",
                icon("check-circle"), " Objeto de R cargado correctamente")
          }
        )
      })
      
      # --- 3. LÓGICA DE CONTROL (EL "RESET") ---
      
      observeEvent(input$selected_input_file, {
        # En cuanto el usuario toca el selector, vaciamos la confirmación
        # Esto dispara el logo de RMedic en el visualizador
        confirmed_data(NULL)
        
        if (is.null(input$selected_input_file) || input$selected_input_file == "") {
          button_state("initial")
        } else {
          # Si cambia la selección, el botón deja de estar en "confirmed" y se habilita
          button_state("modified")
        }
      }, ignoreInit = TRUE)
      
      # OBSERVADOR DE CONFIRMACIÓN
      observeEvent(input$confirm_selection, {
        req(input$selected_input_file, input$selected_input_file != "")
        
        info_to_confirm <- list(
          "data_source"         = sui_data_source(),
          "selected_input_file" = input$selected_input_file,
          "list_extra"          = list("xlsx_file_details" = "No details")
        )
        
        confirmed_data(info_to_confirm)
        button_state("confirmed")
        
        showNotification(
          paste("Confirmado:", input$selected_input_file),
          type = "message"
        )
      })
      
      # Reset por cambio de fuente global
      observeEvent(sui_data_source(), {
        confirmed_data(NULL)
        button_state("initial")
      })
      
      # --- 4. RETORNO DEL MÓDULO ---
      return(reactive({
        if (!check_ok()) return(NULL)
        confirmed_data() 
      }))
    }
  )
}