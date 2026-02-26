## Segmento del UI
MiniBaseUI2 <- function(id) {
  ns <- NS(id)
  
  div(tableOutput(ns("MiniBase2")))
  
}




## Segmento del server
MiniBaseSERVER2 <- function(input, output, session, base, 
                           batalla_naval, var_grupo, verbatim) {
  
  
  minibase <- reactive({
    
    if(is.null(base())) return(NULL)
    if(is.null(batalla_naval())) return(NULL)
    if(is.null(var_grupo())) return(NULL)
    
    vars <- c(batalla_naval()[[1]], var_grupo())

    # The minibase
    minibase <- na.omit(base()[vars])
    minibase[,3] <- as.character(minibase[,3])
    
    
    return(minibase)
  })
  
  output$MiniBase2 <- renderTable({
    
   # if(is.null(verbatim)) return(NULL)
  #  if(!verbatim) return(NULL)
    
    minibase()
  })
  
  # Modul Return!!!
  return(minibase)
  
  # return(
  #   list(
  #     frec_input = reactive({ 3333 }),
  #     max_input = reactive({ 4444 })
  #   )
  # )
  
  
  
  
  
}


