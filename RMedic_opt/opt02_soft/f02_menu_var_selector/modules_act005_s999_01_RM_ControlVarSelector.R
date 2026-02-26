
# UI del módulo
module_act005_s999_01_RM_ControlVarSelector_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("ARMADO_BATALLON"))
}

# SERVER del módulo
module_act005_s999_01_RM_ControlVarSelector_server <- function(input, output, session, 
                                                               base, zocalo_CIE, 
                                                               verbatim = FALSE) {
  
  ns <- session$ns
  
  # Opciones de selección de variables con ordenamiento
  OpcionesColumnas <- reactive({
    req(base(), input$orden)
    
    nombres2_original <- colnames(base())
    opciones_carga2 <- OpcionesDeColumnas(nombres2_original)
    
    # Aplicar el orden seleccionado
    switch(as.character(input$orden),
           "1" = opciones_carga2,                                           # Orden Original
           "2" = opciones_carga2[order(nombres2_original, decreasing = F)], # Orden Creciente
           "3" = opciones_carga2[order(nombres2_original, decreasing = T)], # Orden Decreciente
           opciones_carga2 # Caso por defecto
    )
  })
  
  # Actualizar selectores de variables según cantidad elegida
  observeEvent(input$qtty_var, {
    req(OpcionesColumnas(), input$qtty_var != '')
    
    # Actualizar Variable 1 siempre
    if(input$qtty_var >= 1) {
      freezeReactiveValue(input, "var1")
      updateSelectInput(session, 
                        inputId = "var1",
                        label = "Variable 1: ",
                        choices = c("Seleccione una... " = "", OpcionesColumnas())
      )
    }
    
    # Actualizar Variable 2 si corresponde
    if(input$qtty_var >= 2) {
      freezeReactiveValue(input, "var2")
      updateSelectInput(session, 
                        inputId = "var2",
                        label = "Variable 2: ",
                        choices = c("Seleccione una... " = "", OpcionesColumnas())
      )
    }
  })
  
  # Detectar automáticamente tipo de Variable 1
  observeEvent(c(base(), input$var1), {
    req(base(), input$var1 != "")
    
    tipo_detectado <- ifelse(is.numeric(base()[,input$var1]), 10, 1)
    mis_opciones <- c("Categórica" = 1, "Numérica" = 10)
    
    freezeReactiveValue(input, "tipo_var1")
    updateSelectInput(session, 
                      inputId = "tipo_var1",
                      label = "Tipo Variable 1: ",
                      choices = mis_opciones,
                      selected = mis_opciones[ifelse(tipo_detectado == 10, 2, 1)])
  }, ignoreNULL = TRUE)
  
  # Detectar automáticamente tipo de Variable 2
  observeEvent(c(base(), input$var2), {
    req(base(), input$var2 != "")
    
    tipo_detectado <- ifelse(is.numeric(base()[,input$var2]), 10, 1)
    mis_opciones <- c("Categórica" = 1, "Numérica" = 10)
    
    freezeReactiveValue(input, "tipo_var2")
    updateSelectInput(session, 
                      inputId = "tipo_var2",
                      label = "Tipo Variable 2: ",
                      choices = mis_opciones,
                      selected = mis_opciones[ifelse(tipo_detectado == 10, 2, 1)])
  }, ignoreNULL = TRUE)
  
  # Función para recopilar la selección del usuario
  user_selection <- reactive({
    req(input$qtty_var != '')
    
    if(input$qtty_var == 1) {
      # Validar entradas para 1 variable
      req(input$var1 != "", input$tipo_var1 != "")
      
      # Recopilar información para 1 variable
      variables <- input$var1
      numero_tipo <- as.numeric(input$tipo_var1)
      
    } else if(input$qtty_var == 2) {
      # Validar entradas para 2 variables
      req(input$var1 != "", input$tipo_var1 != "", 
          input$var2 != "", input$tipo_var2 != "")
      
      # Recopilar información para 2 variables
      variables <- c(input$var1, input$var2)
      numero_tipo <- as.numeric(c(input$tipo_var1, input$tipo_var2))
    } else {
      return(NULL)
    }
    
    # Determinar tipo de variables
    tipo_variables <- ifelse(numero_tipo == 1, "Character", "Numeric")
    lenguaje_tipo <- ifelse(numero_tipo == 1, "Categórica", "Numérica")
    
    # Verificar si el tipo elegido es compatible con los datos
    verificacion_interna <- rep(TRUE, length(variables))
    for(i in seq_along(variables)) {
      if(!is.numeric(base()[,variables[i]]) && tipo_variables[i] == "Numeric") {
        verificacion_interna[i] <- FALSE
      }
    }
    
    # Determinar el caso para RMedic (suma de puntos)
    suma_caso <- sum(numero_tipo)
    casos_posibles <- c(1, 10, 2, 20, 11)
    caso_tipo_variables <- which(casos_posibles == suma_caso)
    
    # Verificación general
    verificacion_general <- all(verificacion_interna)
    
    # Retornar resultados
    list(
      variables = variables,
      numero_tipo = numero_tipo,
      tipo_variables = tipo_variables,
      caso_tipo_variables = caso_tipo_variables,
      verificacion_interna = verificacion_interna,
      verificacion_general = verificacion_general,
      lenguaje_tipo = lenguaje_tipo
    )
  })
  
  # Función para organizar la "batalla naval" (reordenamiento para análisis)
  batalla_naval <- reactive({
    req(user_selection(), input$qtty_var != '')
    
    # Para 1 variable, usar user_selection sin cambios
    if(input$qtty_var != 2) return(user_selection())
    
    # Para 2 variables, necesitamos posible reorganización
    resultado <- user_selection()
    
    # Rotar automáticamente si tenemos numérica + categórica para poner categórica primero
    if(identical(resultado$numero_tipo, c(10, 1))) {
      # Reorganizar todos los vectores
      indices_rotados <- c(2, 1)
      resultado$variables <- resultado$variables[indices_rotados]
      resultado$numero_tipo <- resultado$numero_tipo[indices_rotados]
      resultado$tipo_variables <- resultado$tipo_variables[indices_rotados]
      resultado$verificacion_interna <- resultado$verificacion_interna[indices_rotados]
      resultado$lenguaje_tipo <- resultado$lenguaje_tipo[indices_rotados]
    }
    
    return(resultado)
  })
  
  # Preparar información para mostrar al usuario
  zocalo <- reactive({
    req(user_selection())
    
    # Inicializar listas vacías para HTML y descarga
    armado <- list(c(), c())
    names(armado) <- c("Batalla Naval", "Batalla Naval")
    
    # Información para variable 1
    if(input$qtty_var >= 1) {
      req(sum(colnames(base()) == input$var1) > 0)
      
      armado[[1]] <- paste0(
        "<b>Variable 1: </b>", user_selection()$variables[1], 
        " - Columna ", MyLetter(Base = base(), the_col = user_selection()$variables[1]),
        " - ", user_selection()$lenguaje_tipo[1]
      )
    }
    
    # Añadir información para variable 2 si aplica
    if(input$qtty_var >= 2) {
      req(sum(colnames(base()) == input$var2) > 0)
      
      armado[[1]] <- paste0(
        armado[[1]], "<br/>",
        "<b>Variable 2: </b>", user_selection()$variables[2], 
        " - Columna ", MyLetter(Base = base(), the_col = user_selection()$variables[2]),
        " - ", user_selection()$lenguaje_tipo[2]
      )
    }
    
    # Añadir información del zócalo CIE si existe
    if(!is.null(zocalo_CIE()[[1]])) {
      armado[[1]] <- paste0(armado[[1]], "<br/>", zocalo_CIE()[[1]])
      armado[[2]] <- c(armado[[2]], zocalo_CIE()[[2]])
    }
    
    # Convertir a HTML
    armado[[1]] <- HTML(armado[[1]])
    
    return(armado)
  })
  
  # Funciones reactivas para devolver valores
  decimales <- reactive({
    req(input$decimales)
    return(input$decimales)
  })
  
  qtty_var <- reactive({
    req(input$qtty_var != '')
    return(input$qtty_var)
  })
  
  # Salidas renderizadas para mensajes de error/advertencia
  output$message01 <- renderText({
    req(batalla_naval())
    
    if(!batalla_naval()$verificacion_interna[1]) {
      paste0(
        "<b><u>Advertencia:</u> La variable 1 '", input$var1, "' no puede ",
        "ser considerada numérica ya que posee letras y/o símbolos. ",
        "Esto imposibilita la generación de cualquier tipo de tablas, gráficos y test ",
        "estadísticos. Realice un CONTROL sobre esta variable.</b><br/><br/><br/><br/>"
      )
    } else NULL
  })
  
  output$message02 <- renderText({ 
    req(batalla_naval(), length(batalla_naval()$verificacion_interna) >= 2)
    
    if(!batalla_naval()$verificacion_interna[2]) {
      paste0(
        "<b><u>Advertencia:</u> La variable 2 '", input$var2, "' no puede ",
        "ser considerada numérica ya que posee letras y/o símbolos. ",
        "Esto imposibilita la generación de cualquier tipo de tablas, gráficos y test ",
        "estadísticos. Realice un CONTROL sobre esta variable.</b><br/><br/><br/><br/>"
      )
    } else NULL
  })
  
  
 
  output$message03 <- renderText({
    req(batalla_naval(), length(batalla_naval()$variables) == 2)
    
    if(batalla_naval()$variables[1] == batalla_naval()$variables[2]) {
      paste0(
        "<b><u>Advertencia:</u> Ha seleccionado dos veces la misma variable. ",
        "Esto imposibilita la generación de cualquier tipo de tablas, gráficos y test ",
        "estadísticos. Elija dos variables distintas.</b><br/><br/><br/><br/>"
      )
    } else NULL
  })
  
  # Construir la UI de acuerdo a las selecciones del usuario
  output$ARMADO_BATALLON <- renderUI({
    
    if(is.null(base())) return(NULL)
    div(
      fluidRow(
        column(4, 
               selectInput(inputId = ns("orden"), 
                           label = "Orden: ",
                           choices = c("Orden Original" = 1, 
                                       "Albético Creciente" = 2,
                                       "Albatético Decreciente" = 3)
               )
        ),
        column(4,
               selectInput(inputId = ns("qtty_var"),
                           label = "Cantidad de variables",
                           choices = c("Seleccione... " = "", 1, 2)
               )
        ),
        column(4,
               numericInput(inputId = ns("decimales"),
                            label = "Decimales",
                            value = 2,
                            min = 0,
                            max = 10,
                            step = 1)
        )
      ),
      conditionalPanel(condition = "input.qtty_var != ''", ns = ns,
                       br(),
                       fluidRow(
                         conditionalPanel(condition = "input.qtty_var >= 1", ns = ns,
                                          column(4,
                                                 selectInput(inputId = ns("var1"),
                                                             label = "Variable 1: ",
                                                             choices = "")
                                          ),
                                          column(4,
                                                 conditionalPanel(condition = "input.var1 != ''", ns = ns,
                                                                  selectInput(inputId = ns("tipo_var1"),
                                                                              label = "Tipo Variable 1:",
                                                                              choices = "")
                                                 )
                                          )
                         )
                       ),
                       br(),
                       fluidRow(
                         conditionalPanel(condition = "input.qtty_var>= 2", ns = ns,
                                          column(4,
                                                 selectInput(inputId = ns("var2"),
                                                             label = "Variable 2: ",
                                                             choices = "")
                                          ),
                                          column(4,
                                                 conditionalPanel(condition = "input.var2 != ''", ns = ns,
                                                                  selectInput(inputId = ns("tipo_var2"),
                                                                              label = "Tipo Variable 2:",
                                                                              choices = "")
                                                 )
                                          )
                         )
                       ),
                       fluidRow(span(htmlOutput(ns("message01")), style="color:red")),
                       fluidRow(span(htmlOutput(ns("message02")), style="color:red")),
                       fluidRow(span(htmlOutput(ns("message03")), style="color:red")),
                       htmlOutput(ns("zocalo")),
      )
    )
    
  })
  
  # Renderizar el zócalo informativo
  output$zocalo <- renderUI({
    # Corregido de renderText a renderUI para manejar contenido HTML correctamente
    req(zocalo())
    div(
      h3_mod(names(zocalo())[1]), 
      h4(zocalo()[[1]])
    )
  })
  
  # Retornar valores necesarios para otros módulos
  list(
    variables = reactive({
      req(batalla_naval())
      batalla_naval()$variables
    }),
    
    tipo_vars = reactive({
      req(batalla_naval())
      batalla_naval()$tipo_variables
    }),
    
    numero_tipo = reactive({
      req(batalla_naval())
      batalla_naval()$numero_tipo
    }),
    
    caso_tipo_variables = reactive({
      req(batalla_naval())
      batalla_naval()$caso_tipo_variables
    }),
    
    verificacion_interna = reactive({
      req(batalla_naval())
      batalla_naval()$verificacion_interna
    }),
    
    verificacion_general = reactive({
      req(batalla_naval())
      batalla_naval()$verificacion_general
    }),
    
    zocalo = zocalo,
    decimales = decimales,
    qtty_var = qtty_var,
    batalla_naval = batalla_naval
  )
}