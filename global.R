# Activa renv
#library(renv)
#renv::activate()

options(encoding = "UTF-8")
options(shiny.maxRequestSize = 500*1024^2)

# remotes::install_github("RinteRface/fullPage")
# library(fullPage)

library("colourpicker")
library("shinycssloaders")
library(shiny)
library(shinyjs)
library(shinyBS)
library(bslib)
library(datasets)
library(DT)
library(htmltools)
library(openxlsx)
library(stringr)
library(vioplot)
library(coin)
library(car)
library(agricolae)
library(gplots)
library(xtable)
library(survival)
library(testthat)
# library(epitools)
library(fmsb)
library(stringi)
library(ggplot2)
library(readxl)
library(usethis)
library(readxl)

#
#
library(httr)
library(jsonlite)

# Función para verificar la conexión a Internet
check_internet <- function() {
  tryCatch({
    res <- httr::GET("http://www.google.com")
    return(res$status_code == 200)
  }, error = function(e) {
    return(FALSE)
  })
}

# Función para obtener la ubicación del usuario
get_user_location <- function() {
  if (!check_internet()) {
    return(NULL)
  }
  
  res <- httr::GET("https://ipinfo.io/json")
  if (res$status_code == 200) {
    content <- httr::content(res, as = "text")
    json <- jsonlite::fromJSON(content)
    return(json)
  } else {
    return(NULL)
  }
}

# # Ejemplo de uso
# location <- get_user_location()
# if (!is.null



# Obtener la lista de archivos .rds en la carpeta data
rds_files_RMedic <- list.files("data", pattern = "\\.rds$", full.names = TRUE)

# Crear una lista para almacenar los dataframes
data_list_RMedic <- lapply(rds_files_RMedic, readRDS)

# Asignar nombres a los elementos de la lista basados en los nombres de los archivos
names(data_list_RMedic) <- gsub("\\.rds$", "", basename(rds_files_RMedic))

# Guardar la lista en un archivo .rds para su uso posterior
#saveRDS(data_list, file = "data/data_list.rds")

########################################
source("uiCode.R")

source("lib.R")
source("functionsHo.R")

source("fn_00_general/fn_00_general.R")
source("fn_shiny/fn_shiny.R")
########################################
super_source <- function(vector_paths){

  lapply(vector_paths, function(path) {
    tryCatch({
      source(path)
      cat("✓ Cargado:", path, "\n")
    }, error = function(e) {
      cat("✗ Error en", path, ":", e$message, "\n")
    })
  })
  
}

vector_files_module_opt <- list.files(path = "RMedic_opt", full.names = T, recursive = T, pattern = "^module_.*\\.R$")
super_source(vector_files_module_opt)

# Aplicar source a cada path

vector_files_modules_opt <- list.files(path = "RMedic_opt", full.names = T, recursive = T, pattern = "^modules_.*\\.R$")
if(length(vector_files_modules_opt) > 0) lapply(vector_files_modules_opt, source)

vector_files_fn_opt <- list.files(path = "RMedic_opt", full.names = T, recursive = T, pattern = "^fn_.*\\.R$")
if(length(vector_files_fn_opt) > 0) lapply(vector_files_fn_opt, source)

########################################
# Obtener la lista de archivos .R en la carpeta especificada
vector_files_modules_RM3 <- list.files(path = "modules_RM3", full.names = T, recursive = T, pattern = "\\.R$")



# Cargar cada archivo .R
lapply(vector_files_modules_RM3, source)
########################################
# Obtener la lista de archivos .R en la carpeta especificada
vector_files_fn_RM3 <- list.files(path = "fn_RM3", full.names = T, recursive = T, pattern = "\\.R$")



# Cargar cada archivo .R
lapply(vector_files_fn_RM3, source)
########################################
# Tabs for all
#source("modules_01_tabs/homeTab.R")
#source("modules_01_tabs/RMedicTab.R")

# RMedic modules
source("modules_01_03_RM_01_side/SideBarBase.R")


source("modules/SideBarBase.R")
#source("modules/BatallaNaval.R")
#source("modules/BatallaNaval2.R")
#source("modules/BatallaNaval3.R")
#source("modules/BatallaNaval4.R")
source("modules/MiniBase.R")
source("modules/MiniBase2.R")

#source("modules/Tablas1Q.R")
#source("modules/Tablas1C.R")
#source("modules/Tablas2Q.R")
#source("modules/Tablas2C.R")
#source("modules/TablasQC.R")

#source("modules/Graficos1Q_ALL.R")
source("modules/Graficos1Q_01_RMedicHelp.R")
source("modules/Graficos1Q_02_Barras.R")
source("modules/Graficos1Q_03_Tortas.R")

#source("modules/Graficos1C_ALL.R")
source("modules/Graficos1C_01_RMedicHelp.R")
source("modules/Graficos1C_02_MediaDesvioEstandard.R")
source("modules/Graficos1C_03_MediaErrorEstandard.R")
source("modules/Graficos1C_04_Boxplot.R")
source("modules/Graficos1C_05_Violinplot.R")
source("modules/Graficos1C_06_Histograma.R")
source("modules/Graficos1C_07_Dispersion.R")
source("modules/Graficos1C_08_Puntos.R")

# Graficos 2Q
#source("modules/Graficos2Q_ALL.R")
source("modules/Graficos2Q_01_RMedicHelp.R")
source("modules/Graficos2Q_02_Barras.R")


# Graficos 2C
#source("modules/Graficos2C_ALL.R")
source("modules/Graficos2C_01_RMedicHelp.R")
source("modules/Graficos2C_02_XY.R")
source("modules/Graficos2C_03_MediaDesvioEstandard.R")
source("modules/Graficos2C_04_MediaErrorEstandard.R")
source("modules/Graficos2C_05_Boxplot.R")
source("modules/Graficos2C_06_Violinplot.R")
source("modules/Graficos2C_07_Dispersion.R")
source("modules/Graficos2C_08_Conectores.R")



# Graficos QC
#source("modules/GraficosQC_ALL.R")
source("modules/GraficosQC_01_RMedicHelp.R")
source("modules/GraficosQC_02_MediaDesvioEstandard.R")
source("modules/GraficosQC_03_MediaErrorEstandard.R")
source("modules/GraficosQC_04_Boxplot.R")
source("modules/GraficosQC_05_Violinplot.R")
source("modules/GraficosQC_06_Dispersion.R")

#####################################################

source("modules/Ho1Q_ALL.R")
source("modules/Ho1Q_01_RMedicHelp.R")
source("modules/Ho1Q_02_TestDeUnaProporcion.R")
source("modules/Ho1Q_03_TestDeUniformidad.R")


source("modules/Ho1C_ALL.R")
source("modules/Ho1C_01_RMedicHelp.R")
source("modules/Ho1C_02_TestTUnaMuestra.R")
source("modules/Ho1C_03_TestWilcoxonUnaMuestra.R")
source("modules/Ho1C_04_TestNormalidadShapiroWilk.R")
source("modules/Ho1C_05_TestChiCuadradoUnaMuestra.R")
# source("modules/Ho1Q_03_TestDeUniformidad.R")

# Ho 2Q
source("modules/Ho2Q_ALL.R")
source("modules/Ho2Q_01_RMedicHelp.R")
source("modules/Ho2Q_02_TestDeDosProporciones.R")
source("modules/Ho2Q_03_TestChiCuadrado.R")
source("modules/Ho2Q_04_TestRegLogSimple.R")
source("modules/Ho2Q_05_Otros.R")

# Ho 2C
source("modules/Ho2C_ALL.R")
source("modules/Ho2C_01_RMedicHelp.R")
source("modules/Ho2C_02_TestCorrelacionPearson.R")
source("modules/Ho2C_04_TestRegresionLinealSimple.R")
source("modules/Ho2C_03_TestCorrelacionSpearman.R")
source("modules/Ho2C_05_TestTApareado.R")
source("modules/Ho2C_06_TestWilcoxonApareado.R")
source("modules/Ho2C_07_TestHomogenedadDeVarianzasFisher.R")
source("modules/Ho2C_08_TestHomogenedadDeVarianzasBartlett.R")
source("modules/Ho2C_09_TestHomogenedadDeVarianzasLevene.R")
source("modules/Ho2C_10_TestRegLogSimple.R")

#####################################################

# Ho QC
source("modules/HoQC_ALL.R")
source("modules/HoQC_01_RMedicHelp.R")
source("modules/HoQC_02_TestTDosMuestrasIndependientes.R")
source("modules/HoQC_03_TestWilcoxonDosMuestrasIndependientes.R")
source("modules/HoQC_04_TestAnova1Factor.R")
source("modules/HoQC_05_TestKruskalWallis.R")
source("modules/HoQC_07_TestHomogeneidadDeVarianzasFisher.R")
source("modules/HoQC_08_TestHomogeneidadDeVarianzasBartlett.R")
source("modules/HoQC_09_TestHomogeneidadDeVarianzasLevene.R")
source("modules/HoQC_10_TestNormalidadShapiroWilkParticionado.R")
source("modules/HoQC_11_TestRegLogSimple.R")

# KaplanMeier
#source("modules/KM_SobrevidaGeneral.R")
#source("modules/KM_SobrevidaGrupos.R")

source("modules2/ModuleBase.R")
#source("modules2/ModuleControl.R")
#source("modules2/ModuleTablas.R")
#source("modules2/ModuleGraficos.R")
#source("modules2/ModuleHo.R")
#source("modules2/ModuleSobrevida.R")

# Control
#source("modules/Control1Q.R")
#source("modules/Control1C.R")
#source("modules/Control2Q.R")
#source("modules/Control2C.R")
#source("modules/ControlQC.R")

###########################################
source("modules/Z_Distribuciones.R")
source("modules/Z_Distribuciones2.R")
