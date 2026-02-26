

# Libreries
library(testthat)

# Carpeta con funciones y carga
vector_files_fn_RM3 <- list.files(path = "../../../../fn_RM3", full.names = T, recursive = T, pattern = "\\.R$")
lapply(vector_files_fn_RM3, source)



# Testing datasets folder
special_folder <- "../../../../../data_test"


## Test 01
file01 <- "control_1c_base001.csv"
selected_path01 <- file.path(special_folder, file01)
my_dataset01 <- read.csv(file = selected_path01, header = T, sep = ",", dec = ".")
selected_col01 <- "mpg"


# Tabla 1c para el valor minimo
df_test_min_SPECTED <- data.frame(
  "Detalle" = "Mìnimo",
  "Valor" = 10.4,
  "Cantidad" = 2L,
  "Posiciones" = "15, 16",
  stringsAsFactors = FALSE,
  check.names = FALSE)




# Tabla 1c para el valor maximo
df_test_max_SPECTED <- data.frame(
  "Detalle" = "Máximo",
  "Valor" = 33.9,
  "Cantidad" = 1L,
  "Posiciones" = "20",
  stringsAsFactors = FALSE,
  check.names = FALSE)




# Tabla 1c para el valor mininimo y maximo
df_test_min_max_SPECTED <- rbind.data.frame(df_test_min_SPECTED, df_test_max_SPECTED)



####----------------------------------------------------------------------------------------------

# Test 01
## Chequeo para el dataset01 que sera utilizado para el test 01
test_that("Control - 1C - Set de Prueba - databaset01 - OK", {



    
    
    # Verificar que my_dataset01 no sea NULL
    expect_false(is.null(my_dataset01), info = "my_dataset01 no debe ser NULL.")
    
    
    
    
    
    # Verificar que es un DataFrame
    expect_true(is.data.frame(my_dataset01), info = "my_dataset01 debe ser un objeto de tipo data.frame.")
    
    
    
    
    
    # Verificar dimensiones del DataFrame
    expect_equal(ncol(my_dataset01), 11, info = "El objeto my_dataset01 debe tener exactamente 11 columnas.")
    expect_equal(nrow(my_dataset01), 32, info = "El objeto my_dataset01 debe tener exactamente 32 filas.")
    
    
    
    
    
    # Verificar que selected_col01 sea un vector de un elemento y de que pertenece a la base
    expect_true(is.vector(selected_col01), info = "selected_col01 debe ser un vector.")
    expect_equal(length(selected_col01), 1, info = "selected_col01 debe tener exactamente 1 elemento.")
    expect_true(selected_col01 %in% colnames(my_dataset01), info = "selected_col01 debe ser un nombre de columna de my_dataset01.")
    
    
    
    
    
    # Verificar que el DataFrame seleccionado tenga las propiedades correctas
    selected_dataframe <- my_dataset01[selected_col01]
    expect_true(is.data.frame(selected_dataframe), info = "El resultado de my_dataset01[selected_col01] debe ser un data.frame.")
    expect_equal(ncol(selected_dataframe), 1, info = "El DataFrame seleccionado en selected_dataframe debe tener exactamente 1 columna.")
    expect_equal(nrow(selected_dataframe), 32, info = "El DataFrame seleccionado en selected_dataframe debe tener exactamente 32 filas.")  





})



####----------------------------------------------------------------------------------------------

test_that("Control - 1C - Prueba 01 - analyze_min_1c()", {

  # OBSERVED
  df_output_min_OBSERVED <- analyze_min_1c(dataframe = my_dataset01, selected_col01)

  # Verificar que df_output_OBSERVED no sea NULL.
  expect_false(is.null(df_output_min_OBSERVED), info = "El objeto df_output_min_OBSERVED no debe ser NULL.")


  # Verificar que df_output_OBSERVED es un dataframe.
  expect_true(is.data.frame(df_output_min_OBSERVED), info = "El objeto df_output_min_OBSERVED debe ser un objeto de tipo data.frame.")

  
  # Verificar que df_output_min_OBSERVED tiene exactamente 4 columnas
  expect_equal(ncol(df_output_min_OBSERVED), 4, info = "El objeto df_output_min_OBSERVED debe tener exactamente 4 columnas.")


  # Verificar que df_output_min_OBSERVED tiene exactamente 1 fila
  expect_equal(nrow(df_output_min_OBSERVED), 1, info = "El objeto df_output_min_OBSERVED debe tener exactamente 1 fila.")


  # Verificar que el objeto observado y esperado tienen la misma cantidad de columnas
  expect_equal(ncol(df_output_min_OBSERVED), ncol(df_test_min_SPECTED), info = "El objeto df_output_min_OBSERVED y el esperado deben tener la misma cantidad de columnas.")
  
  
  # Verificar que el objeto observado y esperado tienen la msima cantidad de filas
  expect_equal(nrow(df_output_min_OBSERVED), nrow(df_test_min_SPECTED), info = "El objeto df_output_min_OBSERVED y el esperado deben tener la misma cantidad de filas.")
  


  # Verificar que el objeto observado y esperado son identicos
  expect_identical(df_output_min_OBSERVED, df_test_min_SPECTED, info = "El DataFrame min observado y esperado deben ser identicos en todo su contenido.")


})


####----------------------------------------------------------------------------------------------




test_that("Control - 1C - Prueba 01 - analyze_max_1c()", {
  
  # OBSERVED
  df_output_max_OBSERVED <- analyze_max_1c(dataframe = my_dataset01, selected_col01)
  
  # Verificar que df_output_OBSERVED no sea NULL
  expect_false(is.null(df_output_max_OBSERVED), info = "El objeto df_output_max_OBSERVED no debe ser NULL.")
  
  
  # Verificar que df_output_max_OBSERVED es un dataframe
  expect_true(is.data.frame(df_output_max_OBSERVED), info = "El objeto df_output_max_OBSERVED debe ser un objeto de tipo data.frame.")
  
  
  # Verificar que df_output_max_OBSERVED tiene exactamente 4 columnas
  expect_equal(ncol(df_output_max_OBSERVED), 4, info = "El objeto df_output_max_OBSERVED debe tener exactamente 4 columnas.")
  
  
  # Verificar que df_output_max_OBSERVED tiene exactamente 1 fila
  expect_equal(nrow(df_output_max_OBSERVED), 1, info = "El objeto df_output_max_OBSERVED debe tener exactamente 1 fila.")
  
  
  # Verificar que el observado y el esperado tiene exactamente la misma cantidad de columnas que el objeto esperado correspondiente
  expect_equal(ncol(df_output_max_OBSERVED), ncol(df_test_max_SPECTED), info = "El objeto df_output_max_OBSERVED y el esperado deben tener la misma cantidad de columnas.")
  
  
  # Verificar que el observado y el esperado tienen exactamente la misma cantidad de filas
  expect_equal(nrow(df_output_max_OBSERVED), nrow(df_test_max_SPECTED), info = "El objeto df_output_max_OBSERVED y el esperado deben tener la misma cantidad de filas.")
  
  
  
  # Verificar que el objeto observado y esperado son identicos
  expect_identical(df_output_max_OBSERVED, df_test_max_SPECTED, info = "El objeto df_output_max_OBSERVED y el esperado deben ser identicos en todo su contenido.")
  
  
})


####----------------------------------------------------------------------------------------------




test_that("Control - 1C - Prueba 01 - analyze_min_y_max_1c()", {
  
  # OBSERVED
  df_output_min_max_OBSERVED <- analyze_min_y_max_1c(dataframe = my_dataset01, selected_col01)
  
  # Verificar que df_output_OBSERVED no sea NULL
  expect_false(is.null(df_output_min_max_OBSERVED), info = "El objeto df_output_min_max_OBSERVED no debe ser NULL.")
  
  
  # Verificar que df_output_min_max_OBSERVED es un dataframe
  expect_true(is.data.frame(df_output_min_max_OBSERVED), info = "El objeto df_output_min_max_OBSERVED debe ser un objeto de tipo data.frame.")
  
  
  # Verificar que df_output_min_max_OBSERVED tiene exactamente 4 columnas
  expect_equal(ncol(df_output_min_max_OBSERVED), 4, info = "El objeto df_output_min_max_OBSERVED debe tener exactamente 4 columnas.")
  
  
  # Verificar que df_output_min_max_OBSERVED tiene exactamente 2 filas
  expect_equal(nrow(df_output_min_max_OBSERVED), 2, info = "El objeto df_output_min_max_OBSERVED debe tener exactamente 2 filas.")
  
  
  # Verificar que tiene exactamente la misma cantidad de columnas que el objeto esperado correspondiente
  expect_equal(ncol(df_output_min_max_OBSERVED), ncol(df_test_min_max_SPECTED), info = "El DataFrame min_max observado y esperado deben tener la misma cantidad de columnas.")
  
  
  # Verificar que tiene exactamente 1 fila
  expect_equal(nrow(df_output_min_max_OBSERVED), nrow(df_test_min_max_SPECTED), info = "El DataFrame min_max observado y esperado deben tener la misma cantidad de filas.")
  
  
  
  # Verificar que df_output es idéntico a df_test_min
  expect_identical(df_output_min_max_OBSERVED, df_test_min_max_SPECTED, info = "El DataFrame min_max observado y esperado deben ser identicos en todo su contenido.")
  
  
})



####----------------------------------------------------------------------------------------------

