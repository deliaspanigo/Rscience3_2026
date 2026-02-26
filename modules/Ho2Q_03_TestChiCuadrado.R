


Ho2Q_03_TestChiCuadrado_UI <- function(id) {
  
  ns <- NS(id)
  
  uiOutput(ns("armado_ho"))
  
  
}






## Segmento del server
Ho2Q_03_TestChiCuadrado_SERVER <- function(input, output, session, 
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
      "Detalle" = c("Filas", "Columnas"),
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
        column(4, selectInput(inputId = ns("invertir"), label = "En filas...", choices = vector_choices)),
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
  
  
  ##################################################
  
  
  
  

  # Test de Proporciones
  The_Test <- reactive({
    
    if(!control_interno01()) return(NULL)
    if(is.null(minibase())) return(NULL)
    if(is.null(minibase_mod())) return(NULL)
    
        
    
    if(is.null(decimales())) return(NULL)
    if(is.null(alfa())) return(NULL)

    
    Test_2Q_ChiCuadrado(base = minibase_mod(),
                        columnas = c(1,2),
                        decimales = decimales(),
                        alfa = alfa())
    
    
    
    
    
    
  })
  # #######################################################
  # 
  
  # Frase 1: Frase hipotesis
  observe(output$frase_hipotesis <- renderUI({
    HTML(The_Test()[[3]])
  }))
  
  # Tabla de Requisitos del test clasico
  observe( output$tabla_requisitos <- renderTable(rownames = FALSE, digits=decimales(), align = "c",{
    
    The_Test()[[1]][[1]]
    
  }))
  
 
  # Frase 1: Explicacion de requisitos test clasico
  observe(output$frase_requisitos <- renderUI({
    HTML(The_Test()[[1]][[2]])
  }))
  
  
  # Tabla del Test Clasico
  observe( output$tabla_test01 <- renderTable(rownames = FALSE, digits=decimales(), align = "c",{
    
    The_Test()[[1]][[3]]
    
  }))
  
  
  # Frase del test clasico
  observe(output$frase_test01 <- renderUI({
    HTML(The_Test()[[1]][[4]])
  }))
  
  #############################################################################
  
  
  # Tabla del Test Montecarlo
  observe( output$tabla_test02 <- renderTable(rownames = FALSE, digits=decimales(), align = "c",{
    
    The_Test()[[2]][[1]]
    
  }))
  
  
  # Frase del test montecarlo
  observe(output$frase_test02 <- renderUI({
    HTML(The_Test()[[2]][[2]])
  }))
  
  ##############################################################################
  # Armado/Salida del test de Proporciones 1Q
  output$armado_ho <- renderUI({
    
    div(
      h2_mod("Test Chi Cuadrado"),
      "El test Chi Cuadrado puede realizarse en dos variantes: Clásica y Montecarlo.", br(),
      "En ambos casos el juego de hipótesis es el mismo.", 
      br(),
      br(),
      uiOutput(ns("menu_cambio")),
      br(),
      h2_mod("Juego de Hipótesis"),
      htmlOutput(ns("frase_hipotesis")),
      br(),
      br(),
      tabsetPanel(
        tabPanel("Clásico", 
                  h2_mod("Requisitos del test Chi Cuadrado Clásico"),
                  tableOutput(ns("tabla_requisitos")),
                  htmlOutput(ns("frase_requisitos")),
                  br(), 
                  h2_mod("Tabla - Test Chi Cuadrado Clásico"),
                  tableOutput(ns("tabla_test01")),
                  htmlOutput(ns("frase_test01")),
                  br(),
                  br()
        ),
        tabPanel("Montecarlo", 
                 h2_mod("Tabla - Test Chi Cuadrado Montecarlo"),
                 tableOutput(ns("tabla_test02")),
                 htmlOutput(ns("frase_test02")),
                 br(),
                 br()
        )
      )
    )
    
  })
  
  
  
  
  
  
  
}


