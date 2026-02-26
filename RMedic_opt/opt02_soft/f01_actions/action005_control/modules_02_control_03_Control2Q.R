## Segmento del UI
modules_02_control_03_Control2Q_UI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("SeccionControl2Q"))
  
  
}




## Segmento del server
modules_02_control_03_Control2Q_SERVER <- function(input, output, session, 
                             base, 
                             batalla_naval,
                             decimales) {
  
  
  
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  # Caso 3: 2Q
  casoRMedic <- reactive({
    
    if(is.null(batalla_naval())) return(NULL)
    if(is.null(batalla_naval()[[4]])) return(NULL)
    if(length(batalla_naval()[[4]]) == 0) return(NULL)
    if(batalla_naval()[[4]] == '') return(NULL)
    casoRMedic <- batalla_naval()[[4]]
    #casoRMedic <- as.numeric(as.character(as.vector(batalla_naval()[[4]])))
    casoRMedic
    
  })
  
  # Todas las tablas 1Q
  Reactive_control_2q_RMedic <- reactive({
    
    if(is.null(casoRMedic())) return(NULL)
    if(casoRMedic() != 3) return(NULL)
    
    
    salida <-  control_2q_RMedic(base = base(), columna = batalla_naval()[[1]])
    
    
    
    
    # Return Exitoso
    return(salida)
    
    
  })  
  
  
  
  # Control 1Q - Tabla 01      
  output$Tabla_Control01 <- renderTable(rownames = FALSE, align= "c",{
    Reactive_control_2q_RMedic()[[1]]
  })
  
  # Control 1Q - Texto 01 
  output$Texto_Control01 <- renderText({
    Reactive_control_2q_RMedic()[[2]]
  })
  
  # Control 1Q - Tabla 02      
  output$Tabla_Control02 <- renderTable(align= "c",{
    Reactive_control_2q_RMedic()[[3]]
  })
  
  # Control 1Q - Texto 02 
  output$Texto_Control02 <- renderText({
    Reactive_control_2q_RMedic()[[4]]
  })
  
  
  
  
  
  output$SeccionControl2Q <- renderUI({
    
    # Especificaciones de cumplimiento
    if(is.null(casoRMedic())) return(NULL)
    if(casoRMedic() != 3) return(NULL)
    
    
    # Si es el caso 1, seguimos!
    div(
      h2_mod("RMedic - Control para 2 Variables Categóricas"),
      h4("- El control sobre dos variables categóricas se lleva a cabo solo sobre las filas que 
      poseen simultáneamente datos de ambas variables."),
      h4("- Corroborar que en ambas variables sus categorías y la cantidad de categorías debe tener sentido en el marco de la experiencia."), 
      h4("- Corroborar la presencia de celdas vacías."),
      br(),
      h3_mod("Parte 1 de 2 - Combinación de Categorías"),
      h4(htmlOutput(ns("Texto_Control01"))),
      br(),
      tableOutput(ns("Tabla_Control01")),
      br(),
      br(),
      h3_mod("Parte 2 de 2 - Celdas vacías"),
      h4(htmlOutput(ns("Texto_Control02"))),
      br(),
      tableOutput(ns("Tabla_Control02"))
      
    )
  })
  
  
  
  
  
  
}


