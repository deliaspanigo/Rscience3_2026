

module_03_tablas_GENERAL_UI <- function(id) {
  
  ns <- NS(id)
  uiOutput(ns("ui_menuTABLAS"))
  
  
  
  
}



module_03_tablas_GENERAL_SERVER <-  function(input, output, session, base,
                                RMedic_general, status_BaseSalida,
                                zocalo_CIE) { 
  
  observe({
    
    req(RMedic_general(), status_BaseSalida())
    
    # NameSpaceasing for the session
    ns <- session$ns
    
    
    UserSelection <- callModule(module = module_03_tablas_VarSelector_SERVER, 
                                id =  "var_selection_tablas",
                                base = base,
                                zocalo_CIE = zocalo_CIE,
                                verbatim = FALSE)
    
    
    req(UserSelection)
    
    
    # Determinamos los 5 casos para RMedic
    # Caso 1) 1Q =  1 puntos
    # Caso 2) 1C = 10 puntos
    # Caso 3) 2Q =  2 puntos
    # Caso 4) 2C = 20 puntos
    # Caso 5) QC o CQ = 11 puntos
    
    
    MiniBase <- callModule(module = MiniBaseSERVER, id =  "minibase_tablas",
                           base = base,
                           batalla_naval = UserSelection$batalla_naval,
                           verbatim = FALSE)
    
    
    my_case <- reactive({
      
      UserSelection$batalla_naval()$caso_tipo_variables
      
    })
    
    
    # Modules Soft - Tabs mapping
    tablas_server_modules <- list(
      "1" = "modules_03_tablas_Tablas1Q_SERVER",
      "2" = "modules_03_tablas_Tablas1C_SERVER",
      "3" = "modules_03_tablas_Tablas2Q_SERVER",
      "4" = "modules_03_tablas_Tablas2C_SERVER",
      "5" = "modules_03_tablas_TablasQC_SERVER"
    )
    
    observeEvent(my_case(), {
      req(base, UserSelection)
      
      selected_module_name <- tablas_server_modules[[my_case()]]
      req(selected_module_name)
      
      callModule(module = get(selected_module_name),
                 id = paste0("tablas", my_case()),
                 minibase = MiniBase,
                 batalla_naval = UserSelection$batalla_naval,
                 decimales = UserSelection$decimales)
      
    })
    
    
    

    
    output$ui_menuTABLAS <- renderUI({
      
      # Si no hay orden de salir a la cancha... Nadie sale...
      if(is.null(RMedic_general())) return(NULL)
      if(!RMedic_general()) return(NULL)
      
      # Si no hay status de BaseSalida(), nos vamos...
      if(is.null(status_BaseSalida())) return(NULL)
      if(!status_BaseSalida()) return(NULL)
      
      
      fluidRow(
        column(1),
        column(10,
               h3_mod("Menú para Tablas"),
               module_03_tablas_VarSelector_UI(ns("var_selection_tablas")),
               MiniBaseUI(ns("minibase_tablas")),
               hr_mod(),
               br(),
               modules_03_tablas_Tablas1Q_UI(ns("tablas1")),
               modules_03_tablas_Tablas1C_UI(ns("tablas2")),
               modules_03_tablas_Tablas2Q_UI(ns("tablas3")),
               modules_03_tablas_Tablas2C_UI(ns("tablas4")),
               modules_03_tablas_TablasQC_UI(ns("tablas5"))
        ),
        column(1)
      )
      
      
      
      
      # tabs
      
    })
    
    
   
  })
  
}