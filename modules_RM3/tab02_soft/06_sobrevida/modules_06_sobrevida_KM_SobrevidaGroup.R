


modules_06_sobrevida_KM_SobrevidaGroup_UI <- function(id) {
  
  ns <- NS(id)
  
  div(
    br(),
    fluidRow(span(htmlOutput(ns("texto_contorl_KM2")), style="color:red")),
    uiOutput(ns("armado_grafico")),
    br()
  )
  
  
}






## Segmento del server
modules_06_sobrevida_KM_SobrevidaGroup_SERVER <- function(input, output, session, 
                                      minibase, 
                                      decimales,
                                      alfa,
                                      control_ejecucion) {
  
  
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  
  control_internoKM2 <- reactive({
    
    if(is.null(control_ejecucion())) return(list(FALSE, ""))
    
    
    # Return exitoso
    if(!is.null(minibase())){
      if(ncol(minibase()) > 0) {
        if(nrow(minibase()) > 0) {
          
          control_KM2(base = minibase())
          
        }
      }
    } 
    
    
  })
  
  
  # Control interno 01
  control_interno01 <- reactive({
    
    if(is.null(control_ejecucion())) return(FALSE)
    if(is.null(control_internoKM2())) return(FALSE) 
    
    ambos <- c(control_ejecucion(), control_internoKM2()[[1]])
    
    if(sum(ambos) == 2) return(TRUE) else return(FALSE)
    
  })
  
  
  
  
  categorias <- reactive({
    
    if(!control_interno01()) return(NULL)
    
    categorias <- levels(as.factor(minibase()[,3]))
    categorias
    
  })
  
  
  
  cantidad <- reactive({
    
    if(!control_interno01()) return(NULL)
    
    cantidad <- length(categorias())
    
  })
  
  
  
  
  output$texto_contorl_KM2 <- renderText({
    
    control_internoKM2()[[2]]
  })
  
  
  
  
  
  
  
  
  
  
  
  
  # Salida de colores
  output$MODcolor_KM_Grupos <- renderUI({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    label_armado <- "Color..."
    cantidad_colores <- cantidad()
    colores_internos <- rainbow(cantidad_colores)
    label_armado <- paste0("Color ", c(1:cantidad()), " - Categoria: ", categorias())
    
    lapply(1:cantidad_colores, function(i) {
      
      nombre_input <- paste("col_KM_Grupos", i, sep="_")
      div(
        colourpicker::colourInput(inputId = ns(nombre_input),
                                  label = label_armado[i], 
                                  value = colores_internos[i]), br()
      )
      
    })
    
  })
  
  
  colores_usuario_KM_Grupos <- reactive({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    # Solo un color en KM General
    
    cantidad <- cantidad()
    
    # Creamos un vector vacio
    mis_colores <- rep(NA, cantidad)
    
    
    if(length(mis_colores) == 0) return(NULL)
    
    
    for(i in 1:cantidad){ 
      nombre_input <- paste("col_KM_Grupos", i, sep="_")
      if(is.null(input[[nombre_input]])) return(NULL)
      
      mis_colores[i] <- input[[nombre_input]]
      
    }
    
    return(mis_colores)
  })
  
  
  # # output$averaver <- renderTable(minibase())
  
  
  output$tablaKM_Grupos <- renderTable(rownames = FALSE, align = "c",{
    
    if(!control_interno01()) return(NULL)
    KM_Tabla_Grupos(base = minibase(), alfa = alfa())[[1]]
    
  })
  
  output$tabla_LogRank <- renderTable(rownames = FALSE, align = "c",{
    
    if(!control_interno01()) return(NULL)
    KM_TestLogRank(base = minibase(), alfa = alfa())[[1]]
    
  })
  
  
  output$texto_LogRank <- renderText({
    
    if(!control_interno01()) return(NULL)
    KM_TestLogRank(base = minibase(), alfa = alfa())[[2]]
    
  })
  
  
  output$graficoKM_Grupos <- renderPlot({
    
    if(!control_interno01()) return(NULL)
    
    # https://r-charts.com/es/r-base/ejes/
    
    objeto_KM <- KM_Tabla_Grupos(base = minibase(), alfa = alfa())[[2]]
    
    if (is.null(objeto_KM)) return(NULL)
    if (is.null(colores_usuario_KM_Grupos())) return(NULL)
    
    forma1 <- rep(1, cantidad())
    forma2 <- rep(c(1,3,3), cantidad())
    
    forma_elegida <- forma1
    if(input$agregado01) forma_elegida <- forma2
    
    plot(objeto_KM, conf.int = input$agregado01, 
         mark.time = TRUE, lty = forma_elegida, 
         col = colores_usuario_KM_Grupos(), 
         xlab= "Tiempo", ylab= "Probabilidad de Sobrevida", 
         main= "Sobrevida por Grupos", yaxt = "n")
    
    axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), las = 1) 
    
    
    
  })
  
  
  output$armado_grafico <- renderUI({
    
    if(!control_interno01()) return(NULL)
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    div(
      #tableOutput(ns("averaver")),
      #br(), br(),
      h2("Tabla Resumen de Sobrevida por Grupos de Kaplan-Meier"),
      tableOutput(ns("tablaKM_Grupos")), br(), br(),
      
      fluidRow(
        h2("Gráfico de Sobrevida por Grupos de Kaplan-Meier"),
        column(6,
               plotOutput(ns("graficoKM_Grupos")), br(), br(),
        ),
        column(4, 
               # Color
               br(),
               #Intervalos
               checkboxInput(inputId = ns("agregado01"),
                             label = "Agregar intervalo de confianza",
                             value = FALSE), 
               br(),
               uiOutput(ns("MODcolor_KM_Grupos"))
        )
      ),
      fluidRow(
        h2("Test de LogRank"),
        tableOutput(ns("tabla_LogRank")),
        htmlOutput(ns("texto_LogRank"))
      )
      
      
    )
  })
  
}


