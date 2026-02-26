# Instalar el paquete readxl si no está instalado
if (!require(readxl)) {
  install.packages("readxl")
}

if (!require(openxlsx)) {
  install.packages("openxlsx")
}

# Cargar el paquete readxl
library(readxl)
library(openxlsx)

# Obtener la lista de archivos .xlsx en la carpeta data-raw
xlsx_files <- list.files("data-raw", pattern = "\\.xlsx$", full.names = TRUE)

# Función para convertir .xlsx a .rds
convert_xlsx_to_rds <- function(file) {
  # Leer el archivo .xlsx
  #df <- read_excel(file)
  df <- openxlsx::read.xlsx(xlsxFile = file, sheet = 1)
  
  # Crear el nombre del archivo .rds
  rds_file <- gsub("data-raw/", "data/", gsub("\\.xlsx$", ".rds", file))
  
  # Guardar el dataframe como un archivo .rds
  saveRDS(df, file = rds_file)
}

# Aplicar la función a cada archivo .xlsx
lapply(xlsx_files, convert_xlsx_to_rds)