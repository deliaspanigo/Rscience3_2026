

module_action006_s999_tablas_GENERAL_ui <- function(id) {
  
  ns <- NS(id)
  uiOutput(ns("ui_menuTABLAS"))
  
  
  
  
}




module_action006_s999_tablas_GENERAL_server <- function(id, list_principal_selector_01_TABLAS) {
  moduleServer(id, function(input, output, session) {
    
  
    
    # NameSpaceasing for the session
    ns <- session$ns
    
    # Crear un reactiveValues para almacenar todos los valores calculados
    valores_internos <- reactiveValues(
      check_ok = NULL,
      my_case  = NULL,
      my_list  = NULL)
    
    check_ok <- reactive({
     # print("HOLA 01")
      req(list_principal_selector_01_TABLAS(), list_principal_selector_01_TABLAS()$my_minibase)
      
      mis_columnas  <- colnames(list_principal_selector_01_TABLAS()$my_minibase)
      mis_variables <- list_principal_selector_01_TABLAS()$list_info_seleccion$batalla_naval$variables
      the_check <- all(mis_variables %in% mis_columnas)
      
      req(the_check)
      
      
      TRUE
    })
    
    # Modules Soft - Tabs mapping
    tablas_server_modules <- list(
      "1" = "modules_03_tablas_01_Tablas1Q_SERVER",
      "2" = "modules_03_tablas_02_Tablas1C_SERVER",
      "3" = "modules_03_tablas_03_Tablas2Q_SERVER",
      "4" = "modules_03_tablas_04_Tablas2C_SERVER",
      "5" = "modules_03_tablas_05_TablasQC_SERVER"
    )
    
    super_id <- reactiveValues(id = NULL)
    
    run_internal <- reactiveValues(run = FALSE)
    observeEvent(list(check_ok(), list_principal_selector_01_TABLAS()), {
      
      super_id$id <- NULL
      run_internal$run <- FALSE
      
     # print("HOLA 02")
      valores_internos$check_ok <- NULL
      valores_internos$my_case  <- NULL
      valores_internos$my_list  <- NULL 
      
      super_id$id <- paste0("tablas", valores_internos$my_case, "_", round(runif(1, 1, 100000)))
      
      req(list(check_ok(), list_principal_selector_01_TABLAS()))
      valores_internos$check_ok <- check_ok()
      valores_internos$my_case  <- as.character(list_principal_selector_01_TABLAS()$list_info_seleccion$batalla_naval$caso_tipo_variables)
      valores_internos$my_list  <- list_principal_selector_01_TABLAS()
      
      #print("HOLA 03")
      selected_module_name <- tablas_server_modules[[valores_internos$my_case]]
      
      vector_selected_vars       <- valores_internos$my_list$list_info_seleccion$batalla_naval[[1]]
      vector_fusion_system_ref_B <- valores_internos$my_list$list_info_seleccion$batalla_naval$fusion_system_ref_B
      if(vector_fusion_system_ref_B == "CQ") vector_selected_vars <- vector_selected_vars[c(2,1)]
      
      #print("HOLA 04")
      callModule(module = get(selected_module_name),
                 id =  super_id$id, #paste0("tablas", valores_internos$my_case), super_id$id,
                 minibase = reactive(list_principal_selector_01_TABLAS()$my_minibase[vector_selected_vars]),
                 batalla_naval = reactive(list_principal_selector_01_TABLAS()$list_info_seleccion$batalla_naval),
                 decimales = reactive(list_principal_selector_01_TABLAS()$list_initial$decimales)) 
      
      #print("HOLA 05")
      run_internal$run <- TRUE
      
    
      
    })
    
    
    output$ui_menuTABLAS <- renderUI({
      
      req()
      #print("HOLA 06")
      
      # Si no hay orden de salir a la cancha... Nadie sale...
      req(valores_internos$check_ok)
      req(check_ok(), list_principal_selector_01_TABLAS())
      req(run_internal$run)
      
      fluidRow(
        column(1),
        column(10,
               modules_03_tablas_01_Tablas1Q_UI(ns(super_id$id)), #ns("tablas1")),
               modules_03_tablas_02_Tablas1C_UI(ns(super_id$id)), #ns(super_id$id)
               modules_03_tablas_03_Tablas2Q_UI(ns(super_id$id)), #ns("tablas3")),
               modules_03_tablas_04_Tablas2C_UI(ns(super_id$id)), #ns("tablas4")),
               modules_03_tablas_05_TablasQC_UI(ns(super_id$id))  #ns("tablas5"))
        ),
        column(1)
      )
      
      
      
      
      # tabs
      
    })
    

    
    
   
  })
  
}