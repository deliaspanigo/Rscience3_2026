


h4_mod <- function(text){
  
  new_str <- paste0("<u><b>", text, "</b></u>")
  
  new_str <- shiny::h4(shiny::HTML(new_str))
  
  return(new_str)
  
}



h3_mod <- function(text){
  
  new_str <- paste0("<u><b>", text, "</b></u>")
  
  new_str <- shiny::h3(shiny::HTML(new_str))
  
  return(new_str)
  
}



h2_mod <- function(text){
  
  new_str <- paste0("<u><b>", text, "</b></u>")
  
  new_str <- shiny::h2(shiny::HTML(new_str))
  
  return(new_str)
  
}


h1_mod <- function(text){
  
  new_str <- paste0("<u><b>", text, "</b></u>")
  
  new_str <- shiny::h1(shiny::HTML(new_str))
  
  return(new_str)
  
}


# Función para crear una línea horizontal personalizada
hr_mod <- function(color = "#000000", grosor = "3px") {
  tags$hr(style = paste("border-top:", grosor, "solid", color, ";"))
}

