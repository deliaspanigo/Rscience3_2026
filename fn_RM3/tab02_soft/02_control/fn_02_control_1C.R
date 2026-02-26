# Sector01
## title01_fixed    - bk_control_1c_s01_title01_fixed()
### text01_01_fixed - bk_control_1c_s01_text01_fixed()

# Sector02
## Title01          - bk_control_1c_s02_title01_fixed()
### text01_fixed    - bk_control_1c_s02_text01_fixed()
### df01            - bk_control_1c_s02_df01()

# Sector03
## Title03
### text03_01_movile
### df03_01

bk_control_1c_FULL <- function(database, selected_col, selected_lang = "ESP"){

  the_section01 <- list()
  the_section01[["title01_fixed"]] <- list(
    "render" = "renderText_title",
    "output" = "textOutput", 
    "content" = bk_control_1c_s01_title01_fixed(selected_lang),
    "modifiers" = "h2_mod",
    "after_br" = 0)
  
  
  the_section01[["text01_fixed"]] <- list(
    "render" = "renderText_list",
    "output" = "htmlOutput",
    "content" = bk_control_1c_s01_text01_fixed(selected_lang),
    "modifiers" = "h4",
    "after_br" = 1)
  
  
  the_section02 <- list()
  the_section02[["title01_fixed"]] <- list(
    "render" = "renderText_title",
    "output" = "textOutput", 
    "content" = bk_control_1c_s02_title01_fixed(selected_lang),
    "modifiers" = "h2_mod",
    "after_br" = 0)
  
  
  the_section02[["text01_fixed"]] <- list(
    "render" = "renderText",
    "output" = "textOutput", 
    "content" = bk_control_1c_s02_text01_fixed(selected_lang),
    "modifiers" = "h4",
    "after_br" = 1)
  
  
  the_section02[["df01"]] <- list(
    "render" = "renderTable",
    "output" = "tableOutput", 
    "content" = bk_control_1c_s02_df01(database, selected_col, selected_lang),
    "modifiers" = "",
    "after_br" = 1)
  
  

  the_section03 <- list()
  the_section03[["title01_fixed"]] <- list(
    "render" = "renderText_title",
    "output" = "textOutput", 
    "content" = bk_control_1c_s03_title01_fixed(selected_lang),
    "modifiers" = "h2_mod",
    "after_br" = 0)
  
  
  the_section03[["text01_fixed"]] <- list(
    "render" = "renderText",
    "output" = "textOutput", 
    "content" = bk_control_1c_s03_text01_fixed(database, selected_col, selected_lang),
    "modifiers" = "h4",
    "after_br" = 1)
  
  
  the_section03[["df01"]] <- list(
    "render" = "renderTable",
    "output" = "tableOutput", 
    "content" = bk_control_1c_s03_df01(database, selected_col, selected_lang),
    "modifiers" = "",
    "after_br" = 1)
  
  
  
  output_list <- list(
    "s01" = the_section01,
    "s02" = the_section02,
    "s03" = the_section03
  )
  
  return(output_list)
  
  
}


# Section 01 - fn ----------------------------------------------------------------------------------------

bk_control_1c_s01_title01_fixed <- function(selected_lang = "ESP"){
  
  list_output <- list()
  
  
  list_output[["ESP"]] <- "RMedic - Control para 1 Variable Numérica"
  list_output[["ENG"]] <- "RMedic - Control para 1 Variable Numérica"
  
  selected_list_output <- list_output[[selected_lang]]
  return(selected_list_output)

}


bk_control_1c_s01_text01_fixed <- function(selected_lang = "ESP"){
  
  output_list <- list()
  
  output_list[["ESP"]] <- list(
    "sub01" = "- Los valores mínimo y máximo deben tener sentido en el marco de la experiencia.",
    "sub02" = "- Corroborar la presencia o no de celdas vacías en la columna seleccionada."
  )

  output_list[["ENG"]] <- list(
    "text01" = "- The minimum and maximum values should make sense within the framework of the experience.",
    "text02" = "- Verify the presence or absence of empty cells on selected column."
  )
  
  vector_output <- output_list[[selected_lang]]
  vector_space <- c(rep("", length(vector_output)-1, ""))
  
  vector_output <- paste0(vector_output, vector_space)
  
  return(vector_output)
  
}


# Section 02 - fn ----------------------------------------------------------------------------------------
bk_control_1c_s02_title01_fixed <- function(selected_lang = "ESP"){
  
  list_output <- list()
  list_output[["ESP"]] <- "Parte 1 de 2 - Mínimo y Máximo dentro de lo esperado"
  list_output[["ENG"]] <- "Part 1 of 2 - Minimum and Maximum within expectations"
  
  
  selected_list_output <- list_output[[selected_lang]]
  return(selected_list_output)
  
}

bk_control_1c_s02_text01_fixed <- function(selected_lang = "ESP"){
  
  output_list <- list()
  
  output_list[["ESP"]] <- "Los valores mínimo y máximo deben tener sentido en el marco de la experiencia."
  output_list[["ENG"]] <- "The minimum and maximum values ​​should make sense within the framework of the experience."
  
  
  return(output_list[[selected_lang]])
  
}

bk_control_1c_s02_df01 <- function(database, selected_col, selected_lang = "ESP"){
  
  my_df <- analyze_min_max_1c(database, selected_col, selected_lang)
  return(my_df)
  
}


# Crea una función que calcule el mínimo, su cantidad y las posiciones en una selected_col
analyze_min_1c <- function(database, selected_col, selected_lang = "ESP") {
  
  standard_colnames <- list()
  standard_colnames[["ESP"]] <- c("Variable", "Detalle", "Valor", "Cantidad", "Posión en filas")
  standard_colnames[["ENG"]] <- c("Variable", "Detail",  "Value", "Amount",   "Position rows")
  
  standard_details <- list()
  standard_details[["ESP"]] <- "Mínimo"
  standard_details[["ENG"]] <- "Minimun"
  
  standard_text <- list()
  standard_text[["ESP"]] <-  " ... (Solo las primeras 5 posiciones)"
  standard_text[["ENG"]] <-  " ... (First 5 positions only)"
    
  # -------------------------------------------------------------------------------------------------
  # Selected stanrdard
  selected_standard_colnames <- standard_colnames[[selected_lang]]
  selected_standard_details  <- standard_details[[selected_lang]]
  selected_standard_text     <- standard_text[[selected_lang]]
  
  # -------------------------------------------------------------------------------------------------
  
  
  # Selected column only
  col_data <- database[[selected_col]]
  
  # Minimun value
  value_estimated <- min(col_data, na.rm = TRUE)
  
  # Amount of minimun
  amount_estimated <- sum(col_data == value_estimated, na.rm = TRUE)
  
  # Determina las posiciones del mínimo
  vector_position <- which(col_data == value_estimated)
  
  # Verifica si hay más de 5 posiciones
  if (length(vector_position) > 5) {
    # Solo toma las primeras 5 posiciones y crea el texto correspondiente
    vector_selected_position <- vector_position[1:5]
    text_position <- paste(vector_selected_position, collapse = ", ")
    text_extra <- selected_standard_text
  } else {
    text_position <- paste(vector_position, collapse = ", ")
    text_extra <- ""
  }
  
  # Crea un database con los resultados
  df_output <- data.frame(
    Variable = selected_col, 
    Detalle = selected_standard_details,  # Minimun or Maximun 
    Valor = value_estimated,
    Cantidad = amount_estimated,
    Posiciones = paste0(text_position, text_extra),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  colnames(df_output) <-  selected_standard_colnames

  return(df_output)
}

# Crea una función que calcule el máximo, su cantidad y las posiciones en una selected_col
analyze_max_1c <- function(database, selected_col, selected_lang = "ESP") {
  
  standard_colnames <- list()
  standard_colnames[["ESP"]] <- c("Variable", "Detalle", "Valor", "Cantidad", "Posión en filas")
  standard_colnames[["ENG"]] <- c("Variable", "Detail",  "Value", "Amount",   "Position rows")
  
  standard_details <- list()
  standard_details[["ESP"]] <- "Máximo"
  standard_details[["ENG"]] <- "Maximun"
  
  standard_text <- list()
  standard_text[["ESP"]] <-  " ... (Solo las primeras 5 posiciones)"
  standard_text[["ENG"]] <-  " ... (First 5 positions only)"
  
  # -------------------------------------------------------------------------------------------------
  # Selected stanrdard
  selected_standard_colnames <- standard_colnames[[selected_lang]]
  selected_standard_details  <- standard_details[[selected_lang]]
  selected_standard_text     <- standard_text[[selected_lang]]
  
  # -------------------------------------------------------------------------------------------------
  
  
  # Selected column only
  col_data <- database[[selected_col]]
  
  # Maximun value
  value_estimated <- max(col_data, na.rm = TRUE)
  
  # Amount of minimun
  amount_estimated <- sum(col_data == value_estimated, na.rm = TRUE)
  
  # Determina las posiciones del maximo
  vector_position <- which(col_data == value_estimated)
  
  # Verifica si hay más de 5 posiciones
  if (length(vector_position) > 5) {
    # Solo toma las primeras 5 posiciones y crea el texto correspondiente
    vector_selected_position <- vector_position[1:5]
    text_position <- paste(vector_selected_position, collapse = ", ")
    text_extra <- selected_standard_text
  } else {
    text_position <- paste(vector_position, collapse = ", ")
    text_extra <- ""
  }
  
  # Crea un database con los resultados
  df_output <- data.frame(
    Variable = selected_col, 
    Detalle = selected_standard_details,  # Minimun or Maximun 
    Valor = value_estimated,
    Cantidad = amount_estimated,
    Posiciones = paste0(text_position, text_extra),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  colnames(df_output) <-  selected_standard_colnames
  
  return(df_output)
}

# La funcion hace lo anterior para el minimo y para el maximo
analyze_min_max_1c <- function(database, selected_col, selected_lang = "ESP") {
  
  new_cols <- ""
  df_min <- analyze_min_1c(database, selected_col, selected_lang)
  df_max <- analyze_max_1c(database, selected_col, selected_lang)
  
  df_output <- rbind.data.frame(df_min, df_max)
  
  return(df_output)
}


# Section 03 - fn ----------------------------------------------------------------------------------------

bk_control_1c_s03_title01_fixed <- function(selected_lang = "ESP"){
  
  list_output <- list()
  list_output[["ESP"]] <- "Parte 2 de 2 - Celdas vacías"
  list_output[["ENG"]] <- "Part 2 of 2 - Empty cells"
  
  
  selected_list_output <- list_output[[selected_lang]]
  return(selected_list_output)
  
}

bk_control_1c_s03_text01_fixed <- function(database, selected_col, selected_lang = "ESP"){
  
  output_list <- list()
  
  df_n <- analyze_n_df_1c(database, selected_col, selected_lang)
  
  my_text <- analyze_n_phrase_1c(df_n, selected_lang)
  
  return(my_text)
  
}

bk_control_1c_s03_df01 <- function(database, selected_col, selected_lang = "ESP"){

  df_output <- analyze_n_df_1c(database, selected_col, selected_lang = "ESP")
  return(df_output)
  
}
  

analyze_n_df_1c <- function(database, selected_col, selected_lang = "ESP"){

  # -------------------------------------------------------------------------------------------------
  
  standard_colnames <- list()
  standard_colnames[["ESP"]] <- c("Variable", "Total de filas", "Celdas con datos", "Celdas vacías")
  standard_colnames[["ENG"]] <- c("Variable", "Total rows",  "Cells with data",   "Empty cells")
  
  # -------------------------------------------------------------------------------------------------

  selected_colnames <- standard_colnames[[selected_lang]]
  
  # -------------------------------------------------------------------------------------------------
  
  
  n_base <- nrow(database)
  
  minibase <- na.omit(database)

  n_minibase <- nrow(minibase)
  
  n_na <- n_base - n_minibase
  
  df_output <- data.frame(
    "Variable" = selected_col, 
    "Total de filas" = n_base,
    "Filas con datos" = n_minibase,
    "Filas con celdas vacías" = n_na,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  colnames(df_output) <- selected_colnames
  
  return(df_output)
}

analyze_n_phrase_1c <- function(df_n, selected_lang = "ESP"){
  
  # -------------------------------------------------------------------------------------------------
  
  standard_phrase <- list()
  standard_phrase[["ESP"]][["phrase01"]] <- "La variable seleccionada '_selected_col_' no presenta celdas vacias. Al utilizar esta variable el 'n' será _n_base_."
  standard_phrase[["ESP"]][["phrase02"]] <- "La variable seleccionada '_selected_col_' presenta _n_na_ celdas vacías. Al utilizar esta variable el 'n' será _n_minibase_."
  
  
  standard_phrase[["ENG"]][["phrase01"]] <- "The selected variable '_selected_col_' does not display empty cells. When using this variable the 'n' will be _n_base_."
  standard_phrase[["ENG"]][["phrase02"]] <- "The selected variable '_selected_col_' returns _n_na_ empty cells. Using this variable the 'n' will be _n_minibase_."
  
  
  # -------------------------------------------------------------------------------------------------
  
  selected_phrase01 <- standard_phrase[[selected_lang]][["phrase01"]]
  selected_phrase02 <- standard_phrase[[selected_lang]][["phrase02"]]
  
  
  # -------------------------------------------------------------------------------------------------
  
  
  selected_var <- df_n[1,1]
  n_base       <- df_n[1,2]
  n_minibase   <- df_n[1,3]
  n_na         <- df_n[1,4]
  

  phrase_output <- ifelse(n_na == 0, selected_phrase01, selected_phrase02)
  phrase_output <- gsub(pattern = "_n_base_", n_base, phrase_output)
  phrase_output <- gsub(pattern = "_n_minibase_", n_minibase, phrase_output)
  phrase_output <- gsub(pattern = "_selected_col_", selected_var, phrase_output)
  
  return(phrase_output)
}


