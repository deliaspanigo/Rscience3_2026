#' UI para el módulo de importación de datos
#'
#' @param id ID del módulo
#' @return UI para el módulo
module_act001_s999_03_RM_import_ui <- function(id) {
  ns <- shiny::NS(id)
  
  bslib::card(
    bslib::card_header("Importación de datos"),
    bslib::card_body(
      uiOutput(ns("salida_general"))
    ),
    full_screen = TRUE
  )
}

#' Server para el módulo de importación de datos
#'
#' @param id ID del módulo
#' @param list_sui_settings Lista reactiva con configuraciones (viene del módulo s01_xlsx)
#' @return Lista con datos importados y detalles
#' Server para el módulo de importación de datos
#'
#' @param id ID del módulo
#' @param list_sui_settings Lista reactiva con configuraciones (viene de los módulos settings)
#' @return Lista con datos importados y detalles
#' Server para el módulo de importación de datos
#' 
#' @param id ID del módulo
#' @param list_sui_settings Lista reactiva con configuraciones de los módulos de settings
#' @return Lista reactiva con la base de datos y metadatos
module_act001_s999_03_RM_import_server <- function(id, list_sui_settings) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      # --- VALORES REACTIVOS ---
      data_source        <- reactiveVal("No details")
      database           <- reactiveVal(NULL)
      temporal_file_path <- reactiveVal("No details")
      original_file_name <- reactiveVal("No details")
      str_import         <- reactiveVal("No details")
      error_message      <- reactiveVal(NULL)
      
      # --- FUNCIÓN AUXILIAR DE ERROR ---
      show_error_popup <- function(error_msg) {
        showModal(modalDialog(
          title = "Error de importación",
          HTML(error_msg),
          easyClose = TRUE,
          footer = modalButton("Cerrar")
        ))
        error_message(error_msg)
        database(NULL) 
      }
      
      # --- LÓGICA PRINCIPAL ---
      observe({
        settings <- list_sui_settings()
        
        # RESET TOTAL
        if (is.null(settings)) {
          database(NULL)
          error_message(NULL)
          data_source("No details")
          original_file_name("No details")
          return()
        }
        
        error_message(NULL)
        data_source(settings$data_source)
        
        # --- CASO A: RData ---
        if(data_source() == "source_Rdata") {
          tryCatch({
            selected_obj <- settings$selected_input_file
            df_loaded <- get(selected_obj)
            database(as.data.frame(df_loaded))
            original_file_name(selected_obj)
            str_import(paste0("data <- ", selected_obj))
          }, error = function(e) show_error_popup(e$message))
        } 
        
        # --- CASO B: Excel ---
        else if(data_source() == "source_xlsx") {
          tryCatch({
            path_xlsx  <- settings$list_extra$temporal_file_path
            name_xlsx  <- settings$list_extra$original_file_name
            sheet_xlsx <- settings$selected_sheet 
            req(path_xlsx, sheet_xlsx)
            
            cmd <- sprintf("readxl::read_excel(path = '%s', sheet = '%s')", path_xlsx, sheet_xlsx)
            str_import(cmd)
            database(as.data.frame(eval(parse(text = cmd))))
            original_file_name(name_xlsx)
          }, error = function(e) show_error_popup(e$message))
        }
        
        # --- CASO C: CSV (¡ESTE ES EL QUE FALTABA!) ---
        else if(data_source() == "source_csv") {
          tryCatch({
            file_info <- settings$selected_input_file
            req(file_info)
            
            # Extraer parámetros de settings
            header_val <- as.logical(as.numeric(settings$list_extra$header))
            sep_val    <- settings$list_extra$sep
            dec_val    <- settings$list_extra$dec
            quote_val  <- settings$list_extra$quote
            
            # Construcción del comando de lectura
            cmd <- sprintf(
              "read.csv(file = '%s', header = %s, sep = '%s', dec = '%s', quote = '%s')",
              file_info$datapath, header_val, sep_val, dec_val, quote_val
            )
            
            str_import(cmd)
            # Ejecutar importación
            imported_data <- read.csv(
              file = file_info$datapath,
              header = header_val,
              sep = sep_val,
              dec = dec_val,
              quote = quote_val,
              stringsAsFactors = FALSE
            )
            
            database(as.data.frame(imported_data))
            original_file_name(file_info$name)
            
          }, error = function(e) show_error_popup(paste("Error al leer CSV:", e$message)))
        }
        
        # --- CASO D: RMedic / UCC ---
        else if(data_source() %in% c("source_RMedic", "source_UCC")) {
          tryCatch({
            selected_obj <- settings$selected_input_file
            data_list <- get("data_list_RMedic", envir = .GlobalEnv)
            database(as.data.frame(data_list[[selected_obj]]))
            original_file_name(selected_obj)
          }, error = function(e) show_error_popup(e$message))
        }
      })
      
      # RENDERIZADO DE INFORMACIÓN (UI)
      output$salida_general <- renderUI({
        if (!is.null(error_message())) return(div(class="alert alert-danger", icon("exclamation-circle"), HTML(error_message())))
        if (is.null(database())) return(div(class="text-center", style="padding: 20px; color: #666;", icon("clock"), " Esperando confirmación..."))
        
        bslib::layout_column_wrap(
          width = 1/2,
          bslib::value_box(title = "Origen", value = data_source(), showcase = bsicons::bs_icon("filetype-csv"), theme = "primary"),
          bslib::value_box(title = "Archivo", value = original_file_name(), showcase = bsicons::bs_icon("file-earmark-text"), theme = "info"),
          bslib::card(bslib::card_header("Estructura"), bslib::card_body(verbatimTextOutput(ns("str_output"))))
        )
      })
      
      output$str_output <- renderPrint({ req(database()); str(database()) })
      
      return(reactive({
        list(
          "data_source"        = data_source(),
          "database"           = database(),
          "original_file_name" = original_file_name(),
          "str_import"         = str_import(),
          "error_message"      = error_message()
        )
      }))
    }
  )
}
