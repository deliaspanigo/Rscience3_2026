## Segmento del UI
modules_02_control_04_Control2C_UI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("SeccionControl2C"))
  
  
}




## Segmento del server
modules_02_control_04_Control2C_SERVER <- function(input, output, session, 
                             base, 
                             batalla_naval,
                             decimales) {
  
  
  
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  # Caso 4: 2C
  casoRMedic <- reactive({
    
    if(is.null(batalla_naval())) return(NULL)
    if(is.null(batalla_naval()[[4]])) return(NULL)
    if(length(batalla_naval()[[4]]) == 0) return(NULL)
    if(batalla_naval()[[4]] == '') return(NULL)
    casoRMedic <- batalla_naval()[[4]]
    #casoRMedic <- as.numeric(as.character(as.vector(batalla_naval()[[4]])))
    casoRMedic
    
  })
  
  # Todas las tablas 2C
  Reactive_control_2c_RMedic <- reactive({
    
    if(is.null(casoRMedic())) return(NULL)
    if(casoRMedic() != 4) return(NULL)
    
    
    salida <-  control_2c_RMedic(base = base(), columna = batalla_naval()[[1]],
                                 decimales = 2)
    
    
    
    
    # Return Exitoso
    return(salida)
    
    
  })  
  
  
  
  # Control 2C - Tabla 01      
  output$Tabla_Control01 <- renderTable(rownames = FALSE, align= "c",{
    Reactive_control_2c_RMedic()[[1]]
  })
  
  # Control 2C - Texto 01 
  output$Texto_Control01 <- renderText({
    Reactive_control_2c_RMedic()[[2]]
  })
  
  # Control 2C - Tabla 02      
  output$Tabla_Control02 <- renderTable(align= "c",{
    Reactive_control_2c_RMedic()[[3]]
  })
  
  # Control 2C - Texto 02 
  output$Texto_Control02 <- renderText({
    Reactive_control_2c_RMedic()[[4]]
  })
  
  # Control 2C - Tabla 03      
  output$Tabla_Control03 <- renderTable(align= "c",{
    Reactive_control_2c_RMedic()[[5]]
  })
  
  # Control 2C - Texto 03 
  output$Texto_Control03 <- renderText({
    Reactive_control_2c_RMedic()[[6]]
  })
  
  
  
  output$SeccionControl2C <- renderUI({
    
    # Especificaciones de cumplimiento
    if(is.null(casoRMedic())) return(NULL)
    if(casoRMedic() != 4) return(NULL)
    
    
    # Si es el caso 4, seguimos!
    div(
      h2_mod("RMedic - Control para 2 Variable Numéricas"),
      h4("- El control sobre dos variables numéricas se lleva a cabo solo sobre las filas que 
      poseen simultáneamente datos de ambas variables."),
      h4("- Los valores mínimo y máximo de cada variable deben tener sentido en el marco de la experiencia."), 
      h4("- Corroborar la presencia de celdas vacías."),
      br(),
      br(),
      h3_mod(paste0("Parte 1 de 3 - Mínimo y Máximo - Variable 1: ", batalla_naval()[[1]][1])),
      h4(htmlOutput(ns("Texto_Control01"))),
      br(),
      tableOutput(ns("Tabla_Control01")),
      br(),
      br(),
      h3_mod(paste0("Parte 2 de 3 - Mínimo y Máximo - Variable 2: ", batalla_naval()[[1]][2])),
      h4(htmlOutput(ns("Texto_Control02"))),
      br(),
      tableOutput(ns("Tabla_Control02")),
      br(),
      br(),
      h3_mod("Parte 3 de 3 - Celdas vacías"),
      h4(htmlOutput(ns("Texto_Control03"))),
      br(),
      tableOutput(ns("Tabla_Control03"))
      
    )
  })
  
  
  
  
  
  
}


