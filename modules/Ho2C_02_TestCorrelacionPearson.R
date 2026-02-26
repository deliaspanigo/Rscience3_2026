

Ho2C_02_TestCorrelacionPearson_UI <- function(id) {
  
  ns <- NS(id)
  
  div(
    
    uiOutput(ns("armado_ho"))
  )
  
  
}






## Segmento del server
Ho2C_02_TestCorrelacionPearson_SERVER <- function(input, output, session, 
                                    minibase, 
                                    decimales,
                                    control_ejecucion,
                                    alfa) {
  
  
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  # Control interno 01
  control_interno01 <- reactive({
    
    if(is.null(control_ejecucion())) return(FALSE)
    else return(control_ejecucion())
  })
  
  
  ##### 2025
  output$tabla22 <- renderTable({
    
    mi_tabla <- data.frame(
      "Detalle" = c("X1", "X2"),
      "Variables" = colnames(minibase())
    )
    
    mi_tabla <- mi_tabla[vector_orden()]
    mi_tabla
  })
  
  output$menu_cambio <- renderUI({
    req(minibase())
    
    vector_choices <- c("no invertir", "invertir")
    names(vector_choices) <- colnames(minibase())
    div(
      fluidRow(
        column(4, selectInput(inputId = ns("invertir"), label = "Elegir X1", choices = vector_choices)),
        # column(4, actionButton(inputId = ns("activate"), label = "Aplicar")),
        column(4, tableOutput(ns("tabla22")))
      )
    )
  })
  
  
  vector_orden <- reactive({
    req(input$invertir)
    vector_orden <- c(1,2)
    if(input$invertir == "invertir") vector_orden <- c(2,1)
    vector_orden
  })
  
  minibase_mod <- reactiveVal()
  observe({
    mi_vector_cambio <- vector_orden() # 2025
    minibase_mod(minibase()[mi_vector_cambio])
  })
  ####2025##############################################
  
  

  
  # Menu del opciones para el test de proporciones
  output$opciones_ho <- renderUI({
    
    
    div(
      # Seleccion de una categoria
      fluidRow(
        column(4,
               # Seleccion del valor bajo H0
               numericInput(inputId = ns("valor_bajo_ho"),
                            label = "Media poblacional (Valor esperado bajo hipótesis): ",
                            min = NA,  max = NA, step = 0.01, value = 0)
        ),
        column(4,
               
               # Seleccion del tipo de prueba
               radioButtons(ns("tipo_prueba_ho"), "Tipo de Prueba de Hipótesis:",
                            choices = c("Bilateral" = "two.sided",
                                        "Unilateral izquierda" = "less",
                                        "Unilateral derecha" = "greater")
               )
        )
        
        
      )
    )
    
    
    
  })
  
  # Test de Proporciones
  The_Test <- reactive({
    
    # req(input$activate)  ### 2025!
    
    if(!control_interno01()) return(NULL)
    if(is.null(minibase())) return(NULL)
    if(is.null(minibase_mod())) return(NULL)
    
 #   if(is.null(input$tipo_prueba_ho)) return(NULL)
  #  if(is.null(input$valor_bajo_ho)) return(NULL)
    if(is.null(decimales())) return(NULL)
    if(is.null(alfa())) return(NULL)
    
    mi_vector_cambio <- vector_orden() # 2025
    
    Test_2C_TestCorrelacion_Pearson( input_base = minibase_mod(),
                                        input_decimales = decimales(),
                                        input_alfa = alfa())
    
    
    
    
    
    
  })
  # #######################################################
  # 
  
  # Tabla Resumen
  observe( output$tabla_resumen <- renderTable(rownames = FALSE, digits=decimales(), align = "c",{
    
    The_Test()$tabla_resumen
    
  }))
  
  
  # Tabla Requisitos
  observe( output$tabla_requisitos <- renderTable(rownames = FALSE, digits=decimales(), align = "c",{
    
    The_Test()$tabla_requisitos
    
  }))
  
  # Frase 1: Sobre los requisitos
  observe(output$frase_requisitos <- renderUI({
    HTML(The_Test()$frase_requisitos)
  }))
  
  # Frase 2: Explicacion Estadistica
  observe(output$frase_estadistica <- renderUI({
    HTML(The_Test()$frase_estadistica)
  }))
  
  
  # Frase 3: Advertencia por redondeo
  observe(output$frase_redondeo <- renderUI({
    HTML(The_Test()$frase_redondeo)
  }))
  
  
  # Frase 4: Juego de Hipotesis
  observe(output$frase_juego_hipotesis <- renderUI({
    HTML(The_Test()$frase_juego_hipotesis)
  }))
  
  # Armado/Salida del test de Proporciones 1Q
  output$armado_ho <- renderUI({
    
    # req(input$activate)   ### 2025
    
    div(
      h2_mod("Test de Correlación de Pearson"),
      "Nota: para la utilización del test de correlación de Pearson 
             ambas variables debe ser numéricas cuantitativas. No debe ser aplicarse 
             el test de Correlación de Pearson sobre variables ordinales representadas 
             con números. En tal caso debe utilizarse el test de Correlación de Spearman.", 
      br(),
      br(),
      uiOutput(ns("menu_cambio")),
    #  h3("Elecciones del usuario"),
    #  uiOutput(ns("opciones_ho")),
      br(),
      # Mensaje de advertencia por redondeo
      span(htmlOutput(ns("frase_redondeo")), style="color:red"),
      br(),
      h3_mod("Tabla de Requisitos del test de Correlación de Pearson"),
      tableOutput(ns("tabla_requisitos")),
      htmlOutput(ns("frase_requisitos")),
      br(),
      h3_mod("Juego de Hipótesis"),
      htmlOutput(ns("frase_juego_hipotesis")),
      br(),
      h3_mod("Tabla Resumen del test de Correlación de Pearson"),
      tableOutput(ns("tabla_resumen")),
      br(),
      h3_mod("Frases y conclusiones"),
      htmlOutput(ns("frase_estadistica")),
      br(), br()
    )
    
  })
  
  
  
  
  
  
  
}


