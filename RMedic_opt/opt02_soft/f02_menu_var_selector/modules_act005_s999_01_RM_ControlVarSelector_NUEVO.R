# Módulo para selección de orden de columnas
mod_orden_columnas_ui <- function(id) {
  
  VECTOR_OPT <- c(
    "Orden Original" = 1,
    "Alfabético Creciente" = 2,
    "Alfabético Decreciente" = 3
  )
  
  ns <- NS(id)
  selectInput(
    inputId = ns("orden"),
    label = "Orden de columnas:",
    choices = VECTOR_OPT 
  )
}

mod_orden_columnas_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    return(reactive({
      req(input$orden)
      input$orden
    }))
  })
}

################################################################################
# Módulo para selección de número de variables
mod_num_variables_ui <- function(id) {
  ns <- NS(id)
  selectInput(
    inputId = ns("num_vars"),
    label = "Cantidad de variables:",
    choices = c("Seleccione..." = "", "1" = 1, "2" = 2)
  )
}

mod_num_variables_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    return(reactive({
      req(input$num_vars)
      as.integer(input$num_vars)
    }))
  })
}

################################################################################
# Módulo para selección de decimales
mod_decimales_ui <- function(id) {
  ns <- NS(id)
  numericInput(
    inputId = ns("decimales"),
    label = "Decimales:",
    value = 2,
    min = 0,
    max = 10,
    step = 1
  )
}

mod_decimales_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    return(reactive({
      req(input$decimales)
      input$decimales
    }))
  })
}

################################################################################
# Módulo para selección de alfa
mod_alfa_ui <- function(id) {
  ns <- NS(id)
  vector_opts <- c("Seleccione..." = "", 
                  "0.01 (1%)" = "0.01",
                  "0.05 (5%)" = "0.05", 
                  "0.10 (10%)" = "0.10")
  selectInput(
    inputId = ns("alfa"),
    label = "Alfa:",
    choices = vector_opts,
    selected = vector_opts[3]
  )
}

mod_alfa_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    return(reactive({
      req(input$alfa)
      as.numeric(as.character(input$alfa))
    }))
  })
}

################################################################################
# Módulo para selección dinámica de variables
mod_seleccion_variables_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("master_box_selection"))
}

# Módulo para selección dinámica de variables
mod_seleccion_variables_server <- function(id, num_variables, columnas_ordenadas) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    VECTOR_INIT <- c("Seleccione..." = "")
    VECTOR_OPT  <- c("Categórica" = 1, "Numérica" = 2)
    
    
    # Generar los controles dinámicamente
    output$master_box_selection <- renderUI({
      req(num_variables(), columnas_ordenadas())
      num_vars <- num_variables()
      
      if(num_vars == "" || is.na(num_vars)) {
        return(NULL)
      }
      
      # Lista para almacenar los controles
      filas_controles <- list()
      
      for(i in 1:num_vars) {
        # Crear una fila para cada variable con sus controles en columnas
        fila <- fluidRow(br(),
          # Selector de variable (primera columna)
          column(
            width = 6,
            selectInput(
              inputId = ns(paste0("var_", i)),
              label = paste0("Variable ", i, ":"),
              choices = c(VECTOR_INIT, columnas_ordenadas())
            )
          ),
          # Selector de tipo de variable (segunda columna)
          column(
            width = 6,
            selectInput(
              inputId = ns(paste0("tipo_var_", i)),
              label = paste0("Tipo de variable ", i, ":"),
              choices = c(VECTOR_INIT, VECTOR_OPT)
            )
          )
        )
        
        filas_controles[[i]] <- fila
        #filas_controles[[i*2-1]] <- fila
        
      }
      
      do.call(tagList, filas_controles)
    })
    
    # Observar cambios en la selección de variables para resetear el tipo
    observe({
      req(num_variables())
      num_vars <- num_variables()
      
      # Esto es reseteo..
      # Si cambia la variable, se resetea el tipo de variable...
      for(i in 1:num_vars) {
        local({
          local_i <- i
          var_id <- paste0("var_", local_i)
          tipo_id <- paste0("tipo_var_", local_i)
          
          # Resetear el tipo cuando cambia la variable
          observeEvent(input[[var_id]], {
            updateSelectInput(session, tipo_id, selected = "")
          }, ignoreInit = TRUE)
        })
      }
      
    })
    
    list_seleccion_variables <- reactive({
      req(num_variables())
      num_vars <- num_variables()
      resultado <- list()
      columnas <- columnas_ordenadas()
      
      for(i in 1:num_vars) {
        var_id <- paste0("var_", i)
        tipo_id <- paste0("tipo_var_", i)
        
        if(!is.null(input[[var_id]]) && input[[var_id]] != "") {
          # Obtener posición y letra
          selected_var <- input[[var_id]]
          col_pos <- which(columnas == selected_var)
          col_letter <- openxlsx::int2col(col_pos)
          
          var_info <- list(
            nombre = input[[var_id]],
            col_pos = unname(col_pos),
            col_letter = col_letter,
            tipo = if(!is.null(input[[tipo_id]]) && input[[tipo_id]] != "") {
              switch(input[[tipo_id]], "1" = names(VECTOR_OPT)[1], "2" = names(VECTOR_OPT)[2])
            } else {
              NA
            },
            system_ref_A = input[[tipo_id]],
            system_ref_B = if(!is.null(input[[tipo_id]]) && input[[tipo_id]] != "") {
              switch(input[[tipo_id]], "1" = "Q", "2" = "C")
            } else {
              NA
            },
            system_ref_C = if(!is.null(input[[tipo_id]]) && input[[tipo_id]] != "") {
              switch(input[[tipo_id]], "1" = "1", "2" = "10")
            } else {
              NA
            }
          )
          resultado[[i]] <- var_info
        } else {
          resultado[[i]] <- list(nombre = NA, tipo = NA, posicion = NA, letra = NA)
        }
      }
      
      resultado
    })
    
    # Retornar información sobre las variables seleccionadas
    return(list_seleccion_variables)
  })
}

################################################################################

mod_resumen_seleccion_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("resumen_texto"))
}

mod_resumen_seleccion_server <- function(id, list_seleccion_variables, num_variables) {
  moduleServer(id, function(input, output, session) {
    output$resumen_texto <- renderUI({
      req(list_seleccion_variables(), num_variables())
      
      num_vars <- num_variables()
      variables <- list_seleccion_variables()
      
      # Si no hay variables seleccionadas aún, mostrar mensaje informativo
      if(num_vars == "" || is.na(num_vars)) {
        return(div(
          style = "margin-top: 10px; color: #757575; font-style: italic;",
          "Selecciona variables para ver el resumen"
        ))
      }
      
      # Crear elementos para cada variable
      items <- lapply(1:num_vars, function(i) {
        var_info <- variables[[i]]
        
        if(!is.na(var_info$nombre)) {
          tipo_texto <- if(!is.na(var_info$tipo)) {
            paste0(" - ", var_info$tipo)
          } else {
            " - Tipo no seleccionado"
          }
          
          div(
            style = "margin-top: 5px;",
            HTML(paste0("<u><b>Variable ", i, "</u>:</b> ", var_info$nombre, tipo_texto))
          )
        } else {
          div(
            style = "margin-top: 5px; color: #9e9e9e; font-style: italic;",
            HTML(paste0("<u><b>Variable ", i, "</u>:</b> No seleccionada"))
          )
        }
      })
      
      # Envolver todos los elementos en un div
      div(
        style = "margin-top: 15px; padding: 10px; background-color: #f5f5f5; border-radius: 4px;",
        #div(style = "font-weight: bold; margin-bottom: 8px;", "Selección de variables"),
        h3_mod("Selección de variables"),
        items
      )
    })
  })
}

################################################################################
mod_my_minibase <- function(id, my_database, list_seleccion_variables, check_botton){
  moduleServer(id, function(input, output, session) {
    
    # Crear minibase reactiva
    return(reactive({
      req(my_database(), list_seleccion_variables(), check_botton())
      
      vector_vars <- sapply(list_seleccion_variables(), function(x) x$nombre)
      vector_QC <- sapply(list_seleccion_variables(), function(x) x$system_ref_B)
      
      check_Q <- vector_QC == "Q"

      my_minibase <- my_database()[, vector_vars, drop = FALSE]
      #my_minibase <- cbind.data.frame("special_id01_database" = 1:nrow(my_minibase), my_minibase)
      my_minibase <- na.omit(my_minibase)
      #my_minibase <- cbind.data.frame("special_id02_minibase" = 1:nrow(my_minibase), my_minibase)

      for(i in 1:length(check_Q)){
        if(check_Q[i]) my_minibase[,vector_vars[i]] <- as.character(my_minibase[,vector_vars[i]])
      }      

            
      return(my_minibase)
    }))
  })
}
################################################################################
# Módulo para mostrar información de selección
mod_info_seleccion_ui <- function(id) {
  ns <- NS(id)
  verbatimTextOutput(ns("info"))
}

mod_info_seleccion_server <- function(id, orden, num_variables, decimales, list_seleccion_variables, my_minibase, check_botton) {
  moduleServer(id, function(input, output, session) {
    
    check_ok <- reactive({
      req(orden(), num_variables(), decimales(), list_seleccion_variables(), my_minibase(), check_botton())
      
      TRUE
    })
    
    list_initial <- reactive({
      req(check_ok())
      
      list(
        "orden" = orden(),
        "orden_str" = switch(orden(),
                             "1" = "Orden Original",
                             "2" = "Alfabético Creciente",
                             "3" = "Alfabético Decreciente"),
        "num_variables" = num_variables(),
        "decimales" = decimales()
      )
      
    })
    
    
    df_info <- reactive({
      req(list_seleccion_variables())
      my_new_df <- do.call(rbind.data.frame, list_seleccion_variables())
      my_new_df
    })
    
    
    batalla_naval <- reactive({
      req(df_info())
      
      vector_vars <- df_info()$"nombre"
      vector_system_ref_B <- df_info()$"system_ref_B"
      fusion_system_ref_B <- paste0(vector_system_ref_B, collapse = "")
      
      vector_spected <- vector_system_ref_B == "C"
      
      vector_obs <- sapply(vector_vars, function(x){
        is.numeric(my_minibase()[,x])
      })
      
      verificacion_general <- identical(unname(vector_spected), unname(vector_obs))
      vector_emite_numeric_alert <- unname(vector_spected) != unname(vector_obs)
      
      ####
      caso_tipo_variables <- NA
      if(fusion_system_ref_B == "Q") caso_tipo_variables <- 1 else
        if(fusion_system_ref_B == "C") caso_tipo_variables <- 2 else
          if(fusion_system_ref_B == "QQ") caso_tipo_variables <- 3 else
            if(fusion_system_ref_B == "CC") caso_tipo_variables <- 4 else
              if(fusion_system_ref_B == "QC") caso_tipo_variables <- 5 else
                if(fusion_system_ref_B == "CQ") caso_tipo_variables <- 5
      
      
      
      
      
      ####
      # Corrige la asignación de variables
      list_output <- list(
        variables = vector_vars,  # Añade el valor que faltaba
        numero_tipo = df_info()$"system_ref_A",
        tipo_variables = df_info()$"tipo",
        caso_tipo_variables = caso_tipo_variables,#as.character(sum(as.numeric(df_info()$"system_ref_A"))),
        vector_system_ref_B = vector_system_ref_B,
        fusion_system_ref_B = fusion_system_ref_B,
        vector_spected = vector_spected, 
        vector_obs = vector_obs,
        verificacion_general = verificacion_general,
        vector_emite_numeric_alert = vector_emite_numeric_alert,
        #verificacion_interna = verificacion_interna, # is.numeric
        #verificacion_general = verificacion_general, # sum() == num_vars
        
        lenguaje_tipo = "Español")
      
      return(list_output)
    })
    
    list_info_seleccion <- reactive({
      
      list_output <- list(
        "list_initial" = list_initial(),
        "variables" = list_seleccion_variables(),
        "df_info" = df_info(),
        "batalla_naval" = batalla_naval()
      )
      
      list_output
    })
    
    return(reactive({list_info_seleccion()}))
  })
}
################################################################################

# Módulo para el botón con estado dinámico
mod_boton_accion_ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("ui_boton_cargar"))
}

mod_boton_accion_server <- function(id, list_seleccion_variables, num_variables, my_database) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
  
    # Estado del botón: initial, confirmed, modified o error
    button_state <- reactiveVal("initial")
    
    # Mensaje de error si hay algún problema
    error_message <- reactiveVal(NULL)
    
    # Verificar si todas las selecciones son válidas
    check_ok <- reactive({
      req(list_seleccion_variables(), num_variables(), my_database)
      
      num_vars <- num_variables()
      variables <- list_seleccion_variables()
      
      
      
      # Verificar que se hayan seleccionado todas las variables y sus tipos
      all_ok <- TRUE
      error_msg <- NULL
      
      for(i in 1:num_vars) {
        if(is.na(variables[[i]]$nombre)) {
          all_ok <- FALSE
          error_msg <- paste0("Debes seleccionar la Variable ", i)
          break
        }
        if(is.na(variables[[i]]$tipo)) {
          all_ok <- FALSE
          error_msg <- paste0("Debes seleccionar el Tipo para la Variable ", i)
          break
        }
       
      }
      
      # Verificar si hay variables duplicadas
      if(all_ok && num_vars > 1) {
        nombres_variables <- sapply(variables, function(v) v$nombre)
        if(length(nombres_variables) != length(unique(nombres_variables))) {
          all_ok <- FALSE
          error_msg <- "No puedes seleccionar la misma variable más de una vez"
        }
      }
      
      # Verificar si hay variables duplicadas
      if(all_ok && num_vars >= 1) {
        # vector_var_names <-     sapply(list_seleccion_variables(), function(x){x$nombre})
        vector_var_names <- sapply(variables, function(v) v$nombre)
        # print(vector_var_names)
        
        vector_system_ref_B <- sapply(list_seleccion_variables(), function(x){x$system_ref_B})
        # print(vector_system_ref_B)
        
        vector_is_numeric <-   sapply(vector_var_names, function(x){is.numeric(my_database()[,x])})
        # print(paste0("vector_is_numeric: ", vector_is_numeric, collapse = " "))
        
        vector_is_C <- vector_system_ref_B == "C"
        # print(paste0("vector_is_C: ", vector_is_C, collapse = " "))
        
        vector_dt_problem <- rep(NA, length(vector_is_numeric))
        vector_dt_problem <- sapply(1:length(vector_dt_problem), function(x){
          if(!vector_is_numeric[x] && vector_is_C[x]) TRUE else FALSE
        })
        
        if(sum(vector_dt_problem) > 0){
          all_ok <- FALSE
          error_msg <- "Una variable numérica solo puede contener números.<br>"
          for(k in 1:length(vector_dt_problem)){
            error_msg <- paste0(error_msg, "La variable '", variables[[i]]$"nombre", "' ha sido detallada como 
                                numérica pero en la columna se hayan también letras o símbolos.<br>")   
          }

        }
      }
      
      # Actualizar mensaje de error
      error_message(error_msg)
      
      return(all_ok)
    })
    
    # Renderizar el botón
    output$ui_boton_cargar <- renderUI({
      # Determinar la clase y estado del botón
      btn_class <- switch(button_state(),
                          "initial" = "btn-primary",    # Azul inicial
                          "confirmed" = "btn-success",  # Verde después de confirmar
                          "modified" = "btn-primary",   # Vuelve a azul si se modifica
                          "error" = "btn-danger")       # Rojo en caso de error
      
      # Determinar si el botón debe estar deshabilitado
      is_disabled <- !check_ok()
      
      div(
        style = "margin-top: 15px;",
        actionButton(
          inputId = ns("btn_cargar"),
          label = "Cargar Selecciones",
          icon = icon("check"),
          class = btn_class,
          width = "100%",
          disabled = is_disabled
        ),
        
        # Mostrar mensaje explicativo si está deshabilitado
        if (is_disabled) {
          div(
            style = "margin-top: 10px; color: #e57373; font-style: italic; font-size: 16px; font-weight: bold",
            "Completa todas las selecciones"
          )
        },
        
        # Mostrar mensaje de confirmación solo si el estado es confirmed
        if (button_state() == "confirmed") {
          div(
            style = "margin-top: 10px; color: green;",
            icon("check-circle"), 
            "Selección confirmada"
          )
        },
        
        # Mostrar mensaje de error si hay uno
        if (!is.null(error_message())) {
          div(
            style = "margin-top: 10px; color: #d32f2f; font-weight: bold;",
            icon("exclamation-triangle"), 
            HTML(error_message())
          )
        }
      )
    })
    
    # Observador para el botón
    observeEvent(input$btn_cargar, {
      # Si la verificación no pasa, establecer estado de error
      if (!check_ok()) {
        button_state("error")
        return(NULL)
      }
      
      # Establecer estado confirmado
      button_state("confirmed")
    })
    
    # Observar cambios en las variables y actualizar el estado del botón
    observe({
      # Cada vez que las variables cambien, si el estado era confirmado, cambiarlo a modificado
      list_seleccion_variables()
      isolate({
        if (button_state() == "confirmed") {
          button_state("modified")
        }
      })
    })
    
    # Retornar valor reactivo para indicar si se ha confirmado la selección
    return(reactive({
      button_state() == "confirmed"
    }))
  })
}
################################################################################


# Módulo para mostrar información de selección
mod_THE_NEWS_ui <- function(id) {
  ns <- NS(id)
  verbatimTextOutput(ns("THE_NEWS"))
}

mod_THE_NEWS_server <- function(id, list_info_seleccion, check_botton, show_dev) {
  moduleServer(id, function(input, output, session) {
    
    output$THE_NEWS <- renderPrint({
      # Solo mostrar información cuando se haya pulsado el botón de carga
      req(show_dev(), list_info_seleccion(), check_botton(), show_dev())
      
      cat("Opciones seleccionadas:\n")
      cat("---------------------\n")
      pretty_print_list(list_info_seleccion())

      # # Aquí simplemente mostraremos información básica en lugar de usar pretty_print_list
      # datos <- list_info_seleccion()
      # 
      # cat("Orden: ", datos$list_initial$orden_str, "\n")
      # cat("Número de variables: ", datos$list_initial$num_variables, "\n")
      # cat("Decimales: ", datos$list_initial$decimales, "\n\n")
      # 
      # cat("Variables seleccionadas:\n")
      # for (i in seq_along(datos$variables)) {
      #   var_info <- datos$variables[[i]]
      #   cat("  Variable ", i, ": ", var_info$nombre, " (", var_info$tipo, ")\n", sep="")
      #   cat("    Posición: ", var_info$col_pos, ", Letra: ", var_info$col_letter, "\n", sep="")
      # }
    })
  })
}

################################################################################

# Módulo principal que integra todos los componentes
mod_principal_SELECTOR_ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("menu_general_selector"))
  
}

mod_principal_SELECTOR_server <- function(id, my_database, show_dev) {
  moduleServer(id, function(input, output, session) {
    # Datos de ejemplo
    
    output$"menu_general_selector" <- renderUI({
      
      ns <- session$ns
      req(my_database())
      
      div(
        #titlePanel("Selector de variables dinámico"),
        
        # Primera fila: opciones juntas (orden, cantidad y decimales)
        fluidRow(
          column(4, mod_orden_columnas_ui(ns("orden"))),
          column(4, mod_num_variables_ui(ns("num_vars"))),
          column(4, mod_decimales_ui(ns("decimales")))
        ),
        # Segunda fila: selección de variables y resumen
        fluidRow(
          column(8, 
                 mod_seleccion_variables_ui(ns("variables")),
                 mod_resumen_seleccion_ui(ns("resumen"))
          ),
          column(4, 
                 br(), br(),
                 mod_boton_accion_ui(ns("boton")))
        ),
        
        # Tercera fila: información de selección
        fluidRow(
          column(12, 
                 mod_THE_NEWS_ui(ns("NEWs"))
          )
        )
      )
      
    })
    check_ok <- reactive({
      req(my_database())
      
    })
    
    columnas <- reactive({
      req(check_ok())
      
      vector_colnames <- colnames(my_database())
      vector_pos <- 1:length(vector_colnames)
      vector_letters <- openxlsx::int2col(vector_pos)
      vector_output <- vector_colnames
      names(vector_output) <- paste0("(", vector_letters, ") - ", vector_colnames)
      vector_output
    })
    
    # Obtener columnas ordenadas
    columnas_ordenadas <- reactive({
      req(check_ok())
      
      
      orden <- orden_columnas()
      cols <- columnas()
      
      switch(orden,
             "1" = cols, # Orden original
             "2" = sort(cols), # Alfabético creciente
             "3" = sort(cols, decreasing = TRUE)) # Alfabético decreciente
    })
    
    # Servidores de los módulos
    ## Básicos
    orden_columnas <- mod_orden_columnas_server("orden")
    num_variables <- mod_num_variables_server("num_vars")
    decimales <- mod_decimales_server("decimales")
    list_seleccion_variables <- mod_seleccion_variables_server("variables", num_variables, columnas_ordenadas)
    mod_resumen_seleccion_server("resumen", list_seleccion_variables, num_variables)
    
    check_botton <- mod_boton_accion_server("boton", list_seleccion_variables, num_variables, my_database)
    
    my_minibase <- mod_my_minibase(id = "mini", my_database, list_seleccion_variables, check_botton)
    
    # Luego, cuando lo pases a mod_info_seleccion_server, asegúrate de que se pase como reactivo:
    list_info_seleccion <- mod_info_seleccion_server("info", 
                                                     orden_columnas, 
                                                     num_variables, 
                                                     decimales, 
                                                     list_seleccion_variables,
                                                     my_minibase,
                                                     check_botton)
    
    # Conectar el módulo de noticias
    mod_THE_NEWS_server(id = "NEWs", 
                        list_info_seleccion = list_info_seleccion, 
                        check_botton = check_botton, 
                        show_dev = show_dev)
    
    list_principal_selector <- reactive({
      req(list_info_seleccion())
      req(list_info_seleccion()$batalla_naval$verificacion_general)
      
      list(
        list_info_seleccion = list_info_seleccion(),
        my_minibase = my_minibase()
      )
    })
    return(list_principal_selector)
  })
}

################################################################################


# Módulo principal que integra todos los componentes
mod_SELECTOR_PRUEBAS_ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("master_menu33"))
  
}

mod_SELECTOR_PRUEBAS_server <- function(id, my_database, show_dev) {
  moduleServer(id, function(input, output, session) {
    # Datos de ejemplo
    
    output$"master_menu33" <- renderUI({
      ns <- session$ns
      req(my_database())
      
      div(
        #titlePanel("Selector de variables dinámico"),
        
        # Primera fila: opciones juntas (orden, cantidad y decimales)
        fluidRow(
          column(3, mod_orden_columnas_ui(ns("orden"))),
          column(3, mod_num_variables_ui(ns("num_vars"))),
          column(3, mod_decimales_ui(ns("decimales"))),
          column(3, mod_alfa_ui(ns("alfa")))
        ),
        # Segunda fila: selección de variables y resumen
        fluidRow(
          column(8, 
                 mod_seleccion_variables_ui(ns("variables")),
                 mod_resumen_seleccion_ui(ns("resumen"))
          ),
          column(4, 
                 br(), br(),
                 mod_boton_accion_ui(ns("boton")))
        ),
        
        # Tercera fila: información de selección
        fluidRow(
          column(12, 
                 mod_THE_NEWS_ui(ns("NEWs"))
          )
        )
      )
      
    })
    columnas <- reactive({
      
      vector_colnames <- colnames(my_database())
      vector_pos <- 1:length(vector_colnames)
      vector_letters <- openxlsx::int2col(vector_pos)
      vector_output <- vector_colnames
      names(vector_output) <- paste0("(", vector_letters, ") - ", vector_colnames)
      vector_output
    })
    
    # Obtener columnas ordenadas
    columnas_ordenadas <- reactive({
      orden <- orden_columnas()
      cols <- columnas()
      
      switch(orden,
             "1" = cols, # Orden original
             "2" = sort(cols), # Alfabético creciente
             "3" = sort(cols, decreasing = TRUE)) # Alfabético decreciente
    })
    
    # Servidores de los módulos
    ## Básicos
    orden_columnas <- mod_orden_columnas_server("orden")
    num_variables <- mod_num_variables_server("num_vars")
    decimales <- mod_decimales_server("decimales")
    alfa <- mod_alfa_server("alfa")
    #observe(print(alfa_value()))
    
    list_seleccion_variables <- mod_seleccion_variables_server("variables", num_variables, columnas_ordenadas)
    mod_resumen_seleccion_server("resumen", list_seleccion_variables, num_variables)
    
    check_botton <- mod_boton_accion_server("boton", list_seleccion_variables, num_variables, my_database)
    
    my_minibase <- mod_my_minibase(id = "mini", my_database, list_seleccion_variables, check_botton)
    
    # Luego, cuando lo pases a mod_info_seleccion_server, asegúrate de que se pase como reactivo:
    list_info_seleccion <- mod_info_seleccion_server("info", 
                                                     orden_columnas, 
                                                     num_variables, 
                                                     decimales, 
                                                     list_seleccion_variables,
                                                     my_minibase,
                                                     check_botton)
    

    
    # Conectar el módulo de noticias
    mod_THE_NEWS_server(id = "NEWs", 
                        list_info_seleccion = list_info_seleccion, 
                        check_botton = check_botton, 
                        show_dev = show_dev)
    
    list_principal_selector <- reactive({
      list(
        list_info_seleccion = list_info_seleccion(),
        my_minibase = my_minibase(),
        alfa = alfa()
      )
    })
    
    return(list_principal_selector)
  })
}

################################################################################
# 
# # UI principal
# ui <- fluidPage(
#   theme = bs_theme(version = 5, bootswatch = "flatly"),
#   # mod_principal_SELECTOR_ui("app")
#   mod_SELECTOR_PRUEBAS_ui("app")
# )
# 
# # Server principal
# server <- function(input, output, session) {
#   # Definir función show_dev como reactiva para que sea accesible en los módulos
#   show_dev <- reactive(TRUE)
# 
#   # the_database <- reactive(mtcars)
#   # list_info_seleccion <- mod_principal_SELECTOR_server("app",
#   #                                                      my_database = the_database,
#   #                                                      show_dev = show_dev)
#   
#   the_database <- reactive(mtcars)
#   list_info_seleccion <- mod_SELECTOR_PRUEBAS_server("app",
#                                                        my_database = the_database,
#                                                        show_dev = show_dev)
#   
#   observe(print(list_info_seleccion()))
# }
# 
# # Crear la aplicación Shiny
# shinyApp(ui = ui, server = server)
