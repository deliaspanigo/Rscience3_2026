


HoQC_11_TestRegLogSimple_UI <- function(id) {
  
  ns <- NS(id)
  
  uiOutput(ns("armado_ho"))
  
  
}






## Segmento del server
HoQC_11_TestRegLogSimple_SERVER <- function(input, output, session, 
                                            minibase, 
                                            decimales,
                                            control_ejecucion,
                                            alfa) {
  
  
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  minibase_mod <- reactive({
    
    if(is.null(input$cambio_columnas)) return(NULL)
    # En este caso...
    # La primera columna es categorica y es X
    # La segunda columna es numerica y es Y
    nueva_base <- minibase()
    
    # La primera columna es numerica y es X
    # La segunda columna es categorica y es Y
    if(input$cambio_columnas == colnames(minibase())[2]){
      
      nueva_base <- minibase()[,c(2,1)]
      
    }
    return(nueva_base)
  })
  
  
  tabla_referencias <- reactive({

    armado <- matrix(NA, 2, 2)
    armado[,2] <- c("0","1") 
    
    #Si X es categorico...
    if(control_qc_reglogsimple()[[4]] == 1) {
    
      if(is.null(input$x0)) return(NULL)
      
    mis_nombres <- c(paste0("Categorías Originales - Variable X (", colnames(minibase_mod())[1] , ")"), "Nuevas categorías")
    estos_levels <- levels(as.factor(as.character(minibase_mod()[,1])))
    if(estos_levels[1] == input$x0) orden <- c(1,2) else
      if(estos_levels[2] == input$x0) orden <- c(2,1)
    
    salida_estos_levels <- estos_levels[orden]
    armado[,1] <- salida_estos_levels
    # Si Y es categorico...
    } else
    if(control_qc_reglogsimple()[[4]] == 2) {
      
      if(is.null(input$y0)) return(NULL)
      
      mis_nombres <- c(paste0("Categorías Originales - Variable Y (", colnames(minibase_mod())[2] , ")"), "Nuevas categorías")
      estos_levels <- levels(as.factor(as.character(minibase_mod()[,2])))
      if(estos_levels[1] == input$y0) orden <- c(1,2) else
        if(estos_levels[2] == input$y0) orden <- c(2,1)
      
      salida_estos_levels <- estos_levels[orden]
      
    }
    
    # Colocamos lo correspondiente
    colnames(armado) <- mis_nombres
    armado[,1] <- salida_estos_levels
    
    return(armado)
  })
  
  valores_importantes <- reactive({
    
    # Para la X si es numerica
    if(is.numeric(minibase_mod()[,1])) {
      
      minimo <- min(minibase_mod()[,1])
      maximo <- max(minibase_mod()[,1])
      media <-  round2(mean(minibase_mod()[,1]), 2)
      rango <- (maximo - minimo)/100
      
      salida <- c(minimo, maximo, media, rango)
      
      # Para X si es categorica
    } else salida <- c(0,1,0,1)
      
      
    
    return(salida)
    
  })
  
  # observe(output$tabla_referencias <- renderTable(align = "c",{
  #   
  #   tabla_referencias()
  # }))
  # 
  control_qc_reglogsimple <- reactive({
    
    if(is.null(minibase_mod())) return(NULL)
    
   #  Control03_QC_RegLogSimple(minibase())
    Control03_QC_RegLogSimple(minibase_mod())
  })
  
  # Control interno 01
  control_interno01 <- reactive({
    
    if(is.null(control_qc_reglogsimple())) return(FALSE) #############
    control_interno <- FALSE
    
    if(is.null(control_ejecucion())) control_interno <- FALSE
    if(!is.null(control_ejecucion())) control_interno <- control_ejecucion()
    if(!control_qc_reglogsimple()[[3]]) control_interno <- FALSE
    
    else return(control_interno)
  })
  
  
  
  # Frase 2: Odd Ratio
  observe(output$frase_control_qc <- renderUI({
    # or <- The_Test()$"Odd ratio redondeado"
    # pendiente <- The_Test()$"Tabla Regresion Redondeada y completa"[2,1]
    
    frase <- control_qc_reglogsimple()[[2]]
    HTML(frase)
  }))
  
  
  observe(output$menu_cambios01 <- renderUI({
    
   # if(control_interno01()) {
      # Caso particular 1: X es categorica y Y es numerica
 if(control_qc_reglogsimple()[[4]] == 1) {
        
        # Caso de que X tiene dos categorias
        if(control_qc_reglogsimple()[[1]] <= 2) {
        div(
          h3("Referencia eje X"),
          selectInput(inputId = ns("x0"), 
                      label = paste0("Referencia '0' de la variable categórica X (", colnames(minibase_mod())[1], ")"), 
                      choices = levels(as.factor(as.character(minibase_mod()[,1]))), multiple = FALSE),
        )
        } else return(NULL)
 } else return(NULL)
  #  } else return(NULL)
    
  }))
  
  
  observe(output$menu_cambios02 <- renderUI({
    
 #   if(control_interno01()) {
      # Caso particular 2: X es numerica y Y es categorica
 if(control_qc_reglogsimple()[[4]] == 2) {
        
        # Caso de que Y tiene dos categorias
        if(control_qc_reglogsimple()[[1]] <= 2) {
        div(
          h3("Referencia eje Y"),
          selectInput(inputId = ns("y0"), 
                      label = paste0("Referencia '0' de la variable categórica Y (", colnames(minibase_mod())[2], ")"), 
                      choices = levels(as.factor(as.character(minibase_mod()[,2]))), 
                      multiple = FALSE)
        )
        } else return(NULL)
      } else return(NULL)
  #  } else return(NULL)
    
  }))
  
  
  x0_interno <- reactive({
    
    if(control_interno01()) {
      if(control_qc_reglogsimple()[[4]] == 1) {
        input$x0
      } else return(NULL)
    } else return(NULL)
  })
  
  
  y0_interno <- reactive({
    if(control_interno01()) {
      if(control_qc_reglogsimple()[[4]] == 2) {
        input$y0
      } else return(NULL)
    } else return(NULL)
  })
  
  
  
  ##################################################
  
  
  
  # 
  
  
  
  # # # # #
  # 2C - 07 - Test de Homogeneidad de Varianzas de Fisher
  
  
  
  # Test de Proporciones
  The_Test <- reactive({
    
   # if(!control_interno01()) return(NULL)
    if(is.null(minibase())) return(NULL)
    #   if(is.null(input$tipo_prueba_ho)) return(NULL)
    #  if(is.null(input$valor_bajo_ho)) return(NULL)
    if(is.null(decimales())) return(NULL)
    if(is.null(alfa())) return(NULL)
    
    RegLogGeneral( base = minibase_mod(),
                   columnas = c(1,2),
                   decimales = decimales(),
                   alfa = alfa(),
                   valor_x = input$valor_nuevo06,
                   x0 = x0_interno(),
                   y0 = y0_interno())
    
    
    
    
    
  })
  # #######################################################
  # 
  
  
  observeEvent(input$valor_nuevo06,{
    updateSliderInput(session,
                      inputId = "valor_nuevo05", 
                      label = "Valor a predecir ", 
                      min = valores_importantes()[1], 
                      max = valores_importantes()[2], 
                      value = input$valor_nuevo06,
                      step = valores_importantes()[4]
    )
    
  })
  
  
  observeEvent(input$valor_nuevo05,{
    updateNumericInput(session,
                       inputId = "valor_nuevo06",
                       label = "Valor a predecir ", 
                       min =  valores_importantes()[1], 
                       max = valores_importantes()[2], 
                       value = input$valor_nuevo05,
                       step = valores_importantes()[4]
    )
  })
  
  
  my_ratio <- reactive({
    
    ratio <- (max(minibase_mod()[,1]) - min(minibase_mod()[,1]))/100
    ratio
    
  })
  
  # Tabla Resumen
  observe( output$tabla_resumen <- renderTable(rownames = TRUE, digits=decimales(), align = "c",{
    
    The_Test()$"Tabla Regresion Redondeada y completa"
    
  }))
  
  # Tabla IC
  observe( output$tabla_IC <- renderTable(rownames = TRUE, digits=decimales(), align = "c",{
    
    The_Test()$"Tabla IC"
    
  }))
  
  # Grafico Regresion
  observe( output$grafico_regresion <- renderPlot({
    
    GraficoRegLog(base = minibase_mod(),
                  columnas = c(1,2),
                  decimales = decimales(),
                  alfa = alfa(),
                  logic_obs = input$logic_obs,
                  logic_esp = input$logic_esp,
                  logic_funcion = input$logic_funcion,
                  col_obs = input$color_obs,
                  col_esp = input$color_esp,
                  col_funcion = input$color_funcion,
                  logic_prediccion = input$logic_nuevo,#T,
                  valor_x = input$valor_nuevo06,
                  x0 = x0_interno(),
                  y0 = y0_interno())
    
  }))
  
  
  # Frase 1: Sobre AIC
  observe(output$frase_aic <- renderUI({
    HTML(paste0("El valor de IAC es: ", The_Test()$AIC, "."))
  }))
  
  
  # Frase 2: Odd Ratio
  observe(output$frase_odd_ratio <- renderUI({
    or <- The_Test()$"Odd ratio redondeado"
    pendiente <- The_Test()$"Tabla Regresion Redondeada y completa"[2,1]
    
    frase <- paste0("Odd Ratio = exp(", pendiente, ") = ", or)
    HTML(frase)
  }))
  
  
  # Frase 3: Ordenada
  observe(output$frase_ordenada <- renderUI({
    HTML(The_Test()$"Frase para la ordenada")
  }))
  
  
  # Frase 4: Pendiente
  observe(output$frase_pendiente <- renderUI({
    HTML(The_Test()$"Frase para la pendiente")
  }))
  
  
  # Frase 5: Frase Prediccion
  observe(output$frase_prediccion <- renderUI({
    HTML(The_Test()$"Frase Predicho")
  }))
  
  # Frase 6: Frase Hipotesis Pendiente
  observe(output$"frase_juego_hipotesis_pendiente" <- renderUI({
    HTML(The_Test()$"HipotesisPendiente")
  }))
  
  # Frase 7: Frase Prediccion
  observe(output$"frase_juego_hipotesis_ordenada" <- renderUI({
    HTML(The_Test()$"HipotesisOrdenada")
  }))
  
  
  
  
  # Armado/Salida del test de Proporciones 1Q
  output$armado_ho_1 <- renderUI({
    
    
    div(
      h2("Test de Regresión Logística Simple"),
      "Nota: para la utilización del test de Regresión Logística Simple la variable 'Y' debe tener solo dos valores.", br(),
      "Los valores de la variable categórica podrán ser referenciados a '0' y '1'.", br(),
      "Los valores de la variable numérica serán tomados literalmente",
      br(),
      br(),
      span(htmlOutput(ns("frase_control_qc")), style="color:red")
      # htmlOutput(ns("frase_control_qc"))
    )
  })# ACAAAAAAAAAAAAAAAAAAAAAAAAAA
  
  
  output$armado_ho_2 <- renderUI({
    
    fluidRow(
      column(6,
             h3("Valores de Referencia"),
             uiOutput(ns("menu_cambios01")), 
             uiOutput(ns("menu_cambios02"))
      ),
      column(6,
             br(), br(),
             h2("Seleccione de cada variable las categorías que serán consideradas
                como '0' y haga clic en 'Obtener Análisis'"),
             actionButton(ns("button"), "Obtener Análisis"))
    )
    
    div(
      h3("Selección de Variable X"),
      selectInput(inputId = ns("cambio_columnas"), label = "Eje X", choices = colnames(minibase()),
                  selected = colnames(minibase())[1]),
      br()
    )
  })
  

  
  # Armado/Salida del test de Proporciones 1Q
  output$armado_ho_3 <- renderUI({
    
    if(!control_interno01()) return(NULL)
    div(
      h3("Juego de Hipótesis de la Pendiente"),
      htmlOutput(ns("frase_juego_hipotesis_pendiente")),
      br(),
      h3("Juego de Hipótesis de la Ordenada"),
      htmlOutput(ns("frase_juego_hipotesis_ordenada"))
    )
    #  h3("Elecciones del usuario"),
    #  uiOutput(ns("opciones_ho")),
  })
  
  
  output$armado_ho_4 <- renderUI({
    
    if(!control_interno01()) return(NULL)
    fluidRow(
      column(6,
             #h3("Valores de Referencia"),
             uiOutput(ns("menu_cambios01")), 
             uiOutput(ns("menu_cambios02"))
      ),
      column(6,
             br(), br(),
             h2("Seleccione de cada variable las categorías que serán consideradas
                como '0' y haga clic en 'Obtener Análisis'"),
             actionButton(ns("button"), "Obtener Análisis"))
    )
  })
  

      
      
    # Armado/Salida del test de Proporciones 1Q
    output$armado_ho_5 <- renderUI({
      
      if(!control_interno01()) return(NULL)
 
      div(
        conditionalPanel(condition = "input.button != 0", ns = ns,
      #input$x0, input$y0, br(),
      #tableOutput(ns("aver")), br(),
      # Mensaje de advertencia por redondeo
      span(htmlOutput(ns("frase_redondeo")), style="color:red"),
      br(),
      # h3("Juego de Hipótesis"),
      # htmlOutput(ns("frase_juego_hipotesis")),
      # br(),
      # h3("Tabla Resumen del test de Regresión Logística Simple"),
      # tableOutput(ns("tabla_resumen")),
      h3("Ajuste del modelo"),
      htmlOutput(ns("frase_aic")),
      br(), br(),
      h3("Frase de la Pendiente"),
      htmlOutput(ns("frase_pendiente")),
      br(), br(),
      h3("Frase de la Ordenada"),
      htmlOutput(ns("frase_ordenada")),
      br(), br(),
      h3("Odd Ratio"),
      htmlOutput(ns("frase_odd_ratio")),
      br(), br(),
      h3("Tabla de Intervalos de Confianza"),
      tableOutput(ns("tabla_IC")),
      br(), br(),

      # h3("Frases y conclusiones"),
      # htmlOutput(ns("frase_estadistica")),
      # br(), br()
      h3("Gráfico de Regresión Logística Simple"),
      plotOutput(ns("grafico_regresion")),
      fluidRow(
        column(2,
               h3("Agregar al gráfico:"),
               checkboxInput(ns("logic_obs"), "Observados", TRUE),
               checkboxInput(ns("logic_esp"), "Esperados", TRUE),
               checkboxInput(ns("logic_funcion"), "Función", TRUE)
        ),
        column(3,
               h3("Elección de colores"),
               colourpicker::colourInput(inputId = ns("color_obs"),
                                         label = "Color valores observados", 
                                         value = "red"),
               br(),br(),
               colourpicker::colourInput(inputId = ns("color_esp"),
                                         label = "Color valores observados", 
                                         value = "green"),
               br(),br(),
               colourpicker::colourInput(inputId = ns("color_funcion"),
                                         label = "Color valores observados", 
                                         value = "black")
        ),
        column(1),
        column(6,
               h3("Agregar predicción"),
               checkboxInput(ns("logic_nuevo"), "Agregar predicción", FALSE),
               br(),br(),
               sliderInput(ns("valor_nuevo05"), 
                           label = "Valor a predecir:",
                           min = valores_importantes()[1], 
                           max = valores_importantes()[2], 
                           value = valores_importantes()[3],
                           step = valores_importantes()[4],
                           width = '400px'), 
               br(),
               numericInput(inputId = ns("valor_nuevo06"), 
                            label = "Valor a predecir ", 
                            min = valores_importantes()[1], 
                            max = valores_importantes()[2], 
                            value = valores_importantes()[3],
                            step = valores_importantes()[4],
                            ),
               br(),
               htmlOutput(ns("frase_prediccion"))
        ),
      ),
      br()#
    )
      )
    
    
  })
  
    
  # Armado/Salida del test de Proporciones 1Q
  output$armado_ho <- renderUI({
    
    
    div(
      uiOutput(ns("armado_ho_1")), br(),
      uiOutput(ns("armado_ho_2")), br(),
      uiOutput(ns("armado_ho_3")), br(),
      uiOutput(ns("armado_ho_4")), br(), 
      uiOutput(ns("armado_ho_5"))
    )
    
  })
  
  
  
  
  
  
  
}


