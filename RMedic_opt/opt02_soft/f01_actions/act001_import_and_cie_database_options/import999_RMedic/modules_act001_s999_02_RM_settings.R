

module_act001_s999_02_RM_settings_ui <- function(id){
  ns <- shiny::NS(id)
  
  div(
    module_act001_s01_xlsx_01_settings_ui(id =   ns("Aspace02_database_01")),
    module_act001_s02_csv_01_settings_ui(id =    ns("Aspace02_database_02")),
    module_act001_s03_RMedic_01_settings_ui(id = ns("Aspace02_database_03")),
    module_act001_s04_Rdata_01_settings_ui(id =  ns("Aspace02_database_04")),
    module_act001_s05_UCC_01_settings_ui(id =  ns("Aspace02_database_05"))
    
  )
  
}




module_act001_s999_02_RM_settings_server <- function(id, sui_data_source){
  
  moduleServer(
    id,
    function(input, output, session) {
      
      # ns para el server!
      ns <- session$ns
      
      
      # 1.2 - Settings para cada fuente de datos.
      the_01_xlsx_settings   <- module_act001_s01_xlsx_01_settings_server(id =   "Aspace02_database_01", sui_data_source)
      the_02_csv_settings    <- module_act001_s02_csv_01_settings_server(id =    "Aspace02_database_02", sui_data_source)
      the_03_RMedic_settings <- module_act001_s03_RMedic_01_settings_server(id = "Aspace02_database_03", sui_data_source)
      the_04_Rdata_settings  <- module_act001_s04_Rdata_01_settings_server(id =  "Aspace02_database_04", sui_data_source)
      the_05_UCC_settings    <- module_act001_s05_UCC_01_settings_server(id =  "Aspace02_database_05", sui_data_source)
      
      # https://gallery.shinyapps.io/assistant/?_gl=1*slchuy*_ga*MTQ4NTM0MTYxMC4xNzQ0ODMzMDEy*_ga_2C0WZ1JHG0*czE3NDQ4OTg3NDEkbzIkZzEkdDE3NDQ4OTkwNTkkajAkbDAkaDA.#
      control_only_one_alive <- reactive({
        
        vector_status <- c(!is.null(the_01_xlsx_settings()),
                           !is.null(the_02_csv_settings()),
                           !is.null(the_03_RMedic_settings()),
                           !is.null(the_04_Rdata_settings()),
                           !is.null(the_05_UCC_settings()))
        
        check_only_one <- sum(vector_status) <= 1
        
        return(check_only_one)
        
      })  
      
      
      # Lista con el settings para realizar la importacion
      list_sui_settings <- reactive({
        req(sui_data_source(), control_only_one_alive())
        
        
        
        el_elegido <- switch(sui_data_source(),
                             "source_xlsx"   = the_01_xlsx_settings(),
                             "source_csv"    = the_02_csv_settings(),
                             "source_RMedic" = the_03_RMedic_settings(),
                             "source_Rdata"  = the_04_Rdata_settings(),
                             "source_UCC"  = the_05_UCC_settings(),
                             # Valor por defecto o manejo de error
                             stop("Fuente de datos no reconocida:", sui_data_source())
        )
        
        
        return(el_elegido)
        # NULL
      })
      
      
      return(list_sui_settings)
      
    }
  )
}



#######################################################################################
