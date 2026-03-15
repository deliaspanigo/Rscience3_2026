module_act001_s05_UCC_01_settings_ui <- function(id){
  ns <- shiny::NS(id)
  
  div(
    uiOutput(ns("carrera_selector")),
    uiOutput(ns("iu_base_selector")),
    uiOutput(ns("ui_action_button"))  # Botón de acción siempre visible
  )
}

module_act001_s05_UCC_01_settings_server <- function(id, sui_data_source){
  
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      # --- 1. ESTADOS REACTIVOS ---
      confirmed_data <- reactiveVal(NULL)
      button_state <- reactiveVal("initial") # initial, confirmed, modified
      
      check_ok <- reactive({
        req(sui_data_source())
        sui_data_source() == "source_UCC"
      })
      
      # --- 2. SELECTORES DINÁMICOS ---
      output$carrera_selector <- renderUI({
        req(check_ok())
        vector_opt <- c("Seleccione..." = "", 
                        "Medicina" = "MD", 
                        "Odontología" = "ODT")
        
        selectInput(inputId = ns("the_carrera"), 
                    label = "Carrera", 
                    choices = vector_opt)
      })
      
      all_names_database <- reactive({
        req(check_ok(), input$the_carrera)
        dataset_names <- names(data_list_RMedic)
        my_str <- paste0("^", input$the_carrera)
        dataset_names[grepl(pattern = my_str, x = dataset_names)]
      })
      
      select_opt_database <- reactive({
        req(all_names_database())
        vector_obj <- all_names_database()
        vector_numbers <- 1:length(vector_obj)
        amount_digits <- max(nchar(vector_numbers), 2)
        
        vector_formatted_numbers <- stringr::str_pad(vector_numbers, width = amount_digits, pad = "0")
        vector_visual <- paste0(vector_formatted_numbers, " - ", vector_obj)
        names(vector_obj) <- vector_visual
        vector_obj
      })
      
      output$iu_base_selector <- renderUI({
        req(check_ok(), input$the_carrera)
        vector_visual <- c("Seleccione una..." = "", select_opt_database())
        
        selectInput(inputId = ns("selected_input_file"),
                    label = "UCC Examples",
                    choices = vector_visual)
      })
      
      # --- 3. BOTÓN DE ACCIÓN (CON BLOQUEO) ---
      output$ui_action_button <- renderUI({
        req(check_ok())
        
        btn_class <- switch(button_state(),
                            "initial"   = "btn-primary",
                            "confirmed" = "btn-success",
                            "modified"  = "btn-primary")
        
        # BLOQUEO: Deshabilitado si falta selección o ya está confirmado
        is_disabled <- is.null(input$selected_input_file) || 
          input$selected_input_file == "" || 
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
            if (is_disabled && button_state() != "confirmed") {
              div(style = "margin-top: 10px; color: #e57373; font-style: italic; font-weight: bold",
                  "Selecciona carrera y ejemplo")
            } else if (button_state() == "confirmed") {
              div(style = "margin-top: 10px; color: green; font-weight: bold;",
                  icon("check-circle"), "Datos de UCC listos")
            }
        )
      })
      
      # --- 4. LÓGICA DE RESET Y CAMBIOS ---
      
      # Si cambia la carrera o la base, invalidamos y habilitamos botón
      observeEvent(list(input$the_carrera, input$selected_input_file), {
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
          "list_extra" = list("carrera" = input$the_carrera)
        ))
        
        button_state("confirmed")
        showNotification(paste("UCC Confirmado:", input$selected_input_file), type = "message")
      })
      
      # --- 6. RETORNO ---
      return(reactive({
        if (!isTruthy(check_ok())) return(NULL)
        confirmed_data()
      }))
    }
  )
}