module_act001_s03_RMedic_01_settings_ui <- function(id){
  ns <- shiny::NS(id)
  
  div(
    uiOutput(ns("iu_base_selector")),
    uiOutput(ns("ui_action_button"))  # Botón de acción siempre visible
  )
}

module_act001_s03_RMedic_01_settings_server <- function(id, sui_data_source){
  
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      # --- 1. ESTADOS REACTIVOS ---
      confirmed_data <- reactiveVal(NULL)
      button_state <- reactiveVal("initial") # initial, confirmed, modified
      
      check_ok <- reactive({
        req(sui_data_source())
        sui_data_source() == "source_RMedic"
      })
      
      # --- 2. LÓGICA DE NOMBRES DE BASES (RMedic Examples) ---
      all_names_database <- reactive({
        req(check_ok())
        # Filtramos las bases que empiezan con "RM" en la lista maestra
        dataset_names <- names(data_list_RMedic)
        dataset_names <- dataset_names[grepl(pattern = "^RM", x = dataset_names)]
        dataset_names
      })
      
      select_opt_database <- reactive({
        req(all_names_database())
        vector_obj <- all_names_database()
        
        # Formateo de números (01, 02, etc.)
        vector_numbers <- 1:length(vector_obj)
        amount_digits <- max(nchar(vector_numbers), 2)
        vector_formatted_numbers <- stringr::str_pad(vector_numbers, width = amount_digits, pad = "0")
        
        vector_visual <- paste0(vector_formatted_numbers, " - ", vector_obj)
        names(vector_obj) <- vector_visual
        vector_obj
      })
      
      # --- 3. RENDERIZADO DE UI ---
      output$iu_base_selector <- renderUI({
        req(check_ok())
        vector_visual <- c("Seleccione una..." = "", select_opt_database())
        
        shiny::selectInput(
          inputId = ns("selected_input_file"),
          label = "Ejemplos de RMedic",
          choices = vector_visual
        )
      })
      
      output$ui_action_button <- renderUI({
        req(check_ok())
        
        btn_class <- switch(button_state(),
                            "initial"   = "btn-primary",
                            "confirmed" = "btn-success",
                            "modified"  = "btn-primary")
        
        # BLOQUEO: Deshabilitar si no hay selección O si ya está confirmado (verde)
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
          if (is.null(input$selected_input_file) || input$selected_input_file == "") {
            div(style = "margin-top: 10px; color: #e57373; font-style: italic; font-weight: bold",
                "Selecciona un ejemplo de RMedic")
          } else if (button_state() == "confirmed") {
            div(style = "margin-top: 10px; color: green; font-weight: bold;",
                icon("check-circle"), "Ejemplo cargado con éxito")
          }
        )
      })
      
      # --- 4. LÓGICA DE DETECCIÓN DE CAMBIOS Y RESET ---
      observeEvent(input$selected_input_file, {
        # Si cambia la selección, invalidamos los datos actuales y liberamos el botón
        confirmed_data(NULL) 
        if (button_state() == "confirmed") {
          button_state("modified")
        }
      }, ignoreInit = TRUE)
      
      observeEvent(check_ok(), {
        if (!check_ok()) {
          confirmed_data(NULL)
          button_state("initial")
        }
      })
      
      # --- 5. CONFIRMACIÓN ---
      observeEvent(input$confirm_selection, {
        req(input$selected_input_file, input$selected_input_file != "")
        
        confirmed_data(list(
          "data_source" = sui_data_source(),
          "selected_input_file" = input$selected_input_file,
          "list_extra" = "No details"
        ))
        
        button_state("confirmed")
        showNotification(paste("Ejemplo confirmado:", input$selected_input_file), type = "message")
      })
      
      # --- 6. RETORNO ---
      return(reactive({
        if (!isTruthy(check_ok())) return(NULL)
        confirmed_data()
      }))
    }
  )
}