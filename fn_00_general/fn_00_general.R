
# # # fn:00_general.R
# Es un conjunto de funciones de uso general.


##########################################################################################

# Num 2 let
# Le das un vector de numeros y lo pasa a letras
num2let <- function(n) {
  
  openxlsx::int2col(n)

}


##########################################################################################

let2num <- function(x) {
  
  openxlsx::col2int(x)
  
}


#helper function (convert vector to named list)
namel <-function (vec){
  tmp<-as.list(vec)
  names(tmp)<-as.character(unlist(vec))
  tmp
}

OpcionesDeColumnas <- function(my_names = ""){
  
  # Letras
  letras_elegidas <- paste0("(", num2let(c(1:length(my_names))), ")")
  
  # Visual del usuario
  visual_usuario <- paste0(letras_elegidas, " - ", my_names)
  
  
  # Armamos el vector de salida
  vector_salida <- my_names
  names(vector_salida) <- visual_usuario
  
  return(vector_salida)
}



MyLetter <- function(Base = NULL, the_col = NULL) {
  
  
  
  if(is.null(Base)) return(NULL)
  if(is.null(the_col)) return(NULL)
  if(the_col == "") return(NULL) 
  if(sum(colnames(Base) == the_col) == 0) return(NULL)
  
  dt_col <- colnames(Base) == the_col
  pos_col <- c(1:length(dt_col))
  the_col <- pos_col[dt_col]
  my_letter <- num2let(the_col)
  
  return(my_letter)
}


