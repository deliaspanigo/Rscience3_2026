## Segmento del UI
BatallaNavalUI4 <- function(id) {
  ns <- NS(id)
  
  
  uiOutput(ns("ARMADO_BATALLON2"))
  
}




## Segmento del server
BatallaNavalSERVER4 <- function(input, output, session,
                                batalla_naval,
                                OpcionesColumnas) {
  
  ns <- session$ns
  
  
 # observe(cat("AVer2","\n", OpcionesColumnas(), "\n"))
  if(is.null(OpcionesColumnas)) return(NULL)
  
  # return(
  #   list(
  #     frec_input = reactive({ input$frec_input }),
  #     max_input = reactive({ input$max_input })
  #   )
  # )
  
  
  
  
  
 
  
 
  
  
  
 
  
  output$ARMADO_BATALLON2 <- renderUI({
    
    if(is.null(OpcionesColumnas())) return(NULL)
    div(
      fluidRow(
      column(4,
             selectInput(inputId = ns("var3"),
                         label = "Grupo (Variable 3): ",
                         choices = c("Seleccione una... " = "", OpcionesColumnas()
      )
    )
      )
      )
    )
    
    
  })
  
  
  var_grupo <- reactive({
    
    if(is.null(input$var3)) return(NULL)
    if(is.na(input$var3)) return(NULL)
    if(input$var3 == "") return(NULL)
    
    # Returno exitoso
    return(input$var3)
    
    })
  
 
  
  
  # Final Return of the Modul!
  return(
    var_grupo
  )
  
  # return(
  #   list(
  #     batalla_naval =  batalla_naval
  # 
  #   )
  # )
}


