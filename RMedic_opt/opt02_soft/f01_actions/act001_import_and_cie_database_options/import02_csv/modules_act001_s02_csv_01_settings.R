module_act001_s02_csv_01_settings_ui <- function(id){
  ns <- shiny::NS(id)
  
  div(
    uiOutput(ns("iu_base_selector"))

  )
}

module_act001_s02_csv_01_settings_server <- function(id, sui_data_source){
  
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      # --- 1. VALORES REACTIVOS ---
      confirmed_data <- reactiveVal(NULL)
      button_state <- reactiveVal("initial")  # initial, confirmed, modified
      ui_version <- reactiveVal(0)
      
      check_ok <- reactive({
        req(sui_data_source())
        sui_data_source() == "source_csv"
      })
      
      # --- 2. GESTIÓN DE RESET ---
      observeEvent(check_ok(), {
        ui_version(ui_version() + 1)
        if (!check_ok()) {
          confirmed_data(NULL)
          button_state("initial")
        }
      })
      
      # Resetear cuando el usuario cambia cualquier parámetro (limpia la pantalla y habilita botón)
      observeEvent(list(input$selected_input_file, input$header, input$sep, input$dec, input$quote), {
        # Si ya estábamos confirmados y algo cambia, liberamos el botón y limpiamos datos
        if (button_state() == "confirmed") {
          button_state("modified")
          confirmed_data(NULL) # Esto activa el logo de RMedic
        }
      }, ignoreInit = TRUE)
      
      # --- 3. RENDERIZADO DE UI ---
      output$iu_base_selector <- renderUI({
        version <- ui_version()
        req(check_ok())
        
        div(
          fileInput(ns("selected_input_file"), "Elige un archivo CSV",
                    accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
          uiOutput(ns("ui_action_button")),
          tags$hr(),
          radioButtons(ns("header"), "Encabezado", choices = c(Yes = 1, No = 0)), br(),
          radioButtons(ns("sep"), "Separador",
                       choices = c(PuntoYComa = ";", Coma = ",", Tab = "\t"),
                       selected = ";"), br(),
          radioButtons(ns("dec"), "Decimal",
                       choices = c(Dot = ".", Comma = ","),
                       selected = "."), br(),
          radioButtons(ns("quote"), "Comillas",
                       choices = c(Ninguna = "", Doble = '"', Simple = "'"),
                       selected = ""), br()
        )
      })
      
      # --- 4. BOTÓN DE ACCIÓN (CON BLOQUEO) ---
      output$ui_action_button <- renderUI({
        req(check_ok())
        
        btn_class <- switch(button_state(),
                            "initial" = "btn-primary",
                            "confirmed" = "btn-success",
                            "modified" = "btn-primary")
        
        # BLOQUEO: Deshabilitado si no hay archivo O si ya está confirmado (verde)
        is_disabled <- is.null(input$selected_input_file) || button_state() == "confirmed"
        
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
          if (is.null(input$selected_input_file)) {
            div(style = "margin-top: 10px; color: #e57373; font-style: italic; font-weight: bold",
                "Selecciona un archivo CSV")
          } else if (button_state() == "confirmed") {
            div(style = "margin-top: 10px; color: green; font-weight: bold;",
                icon("check-circle"), "Configuración aplicada")
          }
        )
      })
      
      # --- 5. LÓGICA DE DATOS ---
      list_extra <- reactive({
        list(
          "header" = input$header, 
          "sep" = input$sep,
          "dec" = input$dec,
          "quote" = input$quote
        )
      })
      
      observeEvent(input$confirm_selection, {
        req(input$selected_input_file)
        
        # Consolidar datos para enviar al importador
        confirmed_data(list(
          "data_source" = sui_data_source(),
          "selected_input_file" = input$selected_input_file,
          "list_extra" = list_extra()
        ))
        
        button_state("confirmed")
        showNotification("CSV confirmado correctamente", type = "message")
      })
      
      # --- 6. RETORNO ---
      return(reactive({
        if (!isTruthy(check_ok())) return(NULL)
        confirmed_data()
      }))
    }
  )
}

