

module_action008_s999_pruebas_GENERAL_ui <- function(id) {
  
  ns <- NS(id)
  
  uiOutput(ns("ui_menuHO"))
  
  
}

module_action008_s999_pruebas_GENERAL_server <-  function(id, list_principal_selector_01_PRUEBAS) {
  moduleServer(id, function(input, output, session) {


    # observe({print("ADENTRO! de module_action008_s999_pruebas_GENERAL_SERVER!!!")})
    #observe({print(list_principal_selector_01_PRUEBAS())})
    # NameSpaceasing for the session
    ns <- session$ns
    
    # My case - Entre 1 y 5
    # Q = 1 - C = 2 - QQ = 3 - CC = 4 - QC = 5 - CQ = 5
    casoRMedic <- reactive({
      
      req(list_principal_selector_01_PRUEBAS())
      aver <- as.character(list_principal_selector_01_PRUEBAS()$list_info_seleccion$batalla_naval$caso_tipo_variables)
      print(aver)
      aver
    })

    check_ok <- reactive({
      req(casoRMedic(), list_principal_selector_01_PRUEBAS())
      TRUE
    })
    
    ##########################################################################
    
    MiniBase <- reactive({
      req(list_principal_selector_01_PRUEBAS())
      
      vector_selected_vars <- list_principal_selector_01_PRUEBAS()$list_info_seleccion$batalla_naval[[1]]
      vector_fusion_system_ref_B <- list_principal_selector_01_PRUEBAS()$list_info_seleccion$batalla_naval$fusion_system_ref_B
      if(vector_fusion_system_ref_B == "CQ") vector_selected_vars <- vector_selected_vars[c(2,1)]
      
      # # #
      minibase <- list_principal_selector_01_PRUEBAS()$my_minibase[vector_selected_vars]
      minibase
    })
    
    batalla_naval <- reactive({
      check_ok()
      list_principal_selector_01_PRUEBAS()$list_info_seleccion$batalla_naval
    })
    
    decimales <- reactive({
      check_ok()
      the_value <- list_principal_selector_01_PRUEBAS()$list_info_seleccion$list_initial$decimales
      the_value <- as.numeric(as.character(the_value))
      the_value
    })
    
    alfa <- reactive({
      check_ok()
      
      as.numeric(as.character(list_principal_selector_01_PRUEBAS()$alfa))
    })
    ##########################################################################
    
    
    # UserSelection <- callModule(module = modules_05_ho_VarSelector_SERVER, 
    #                             id =  "var_selection_graficos",
    #                             base = base,
    #                             zocalo_CIE = zocalo_CIE,
    #                             verbatim = FALSE)
    # 
    # batalla_naval <- UserSelection$batalla_naval
    # 
    # decimales <- UserSelection$decimales
    # alfa <- UserSelection$alfa
    # 
    # # observe(cat("casoRMedic()1: ", casoRMedic(), "\n"))
    # 
    # MiniBase <- callModule(module = MiniBaseSERVER, 
    #                        id =  "minibase_ho",
    #                        base = base,
    #                        batalla_naval = UserSelection$batalla_naval,
    #                        verbatim = FALSE)
    
    
    
    
    
    # Caso 1 - 1Q
    observeEvent(list(casoRMedic(), list_principal_selector_01_PRUEBAS(),
                      MiniBase(), batalla_naval(), decimales(), alfa()),{
      
      req(list(casoRMedic(), list_principal_selector_01_PRUEBAS(),
               MiniBase(), batalla_naval(), decimales(), alfa()))
                        
      if(casoRMedic() == 1){
        
        # print("ADENTRO! de LA ANTESALA!!!")
        # print("Decimales:")
        # print(decimales)
        # print(decimales())
        # print("\n")
        # Caso 1: 1Q
        callModule(module = Ho1Q_SERVER, 
                   id =  "ho1",
                   minibase = MiniBase,
                   casoRMedic = casoRMedic,
                   caso = 1,
                   decimales = decimales,
                   alfa = alfa,
                   batalla_naval = batalla_naval)
        
        
      }
    })
    
    
    # Caso 2 - 1C
    observeEvent(list(casoRMedic(), list_principal_selector_01_PRUEBAS(),
                      MiniBase(), batalla_naval(), decimales(), alfa()),{
                        
      req(list(casoRMedic(), list_principal_selector_01_PRUEBAS(),
               MiniBase(), batalla_naval(), decimales(), alfa()))

      if(casoRMedic() == 2){
        
        
        #Caso 2 : 1C
        callModule(module = Ho1C_SERVER,
                   id =  "ho2",
                   minibase = MiniBase,
                   casoRMedic = casoRMedic,
                   caso = 2,
                   decimales = decimales,
                   alfa = alfa,
                   batalla_naval = batalla_naval)
        
      }
    })
    
    
    # Caso 3 - 2Q
    observeEvent(list(casoRMedic(), list_principal_selector_01_PRUEBAS(),
                      MiniBase(), batalla_naval(), decimales(), alfa()),{
                        
        req(list(casoRMedic(), list_principal_selector_01_PRUEBAS(),
                 MiniBase(), batalla_naval(), decimales(), alfa()))
        
      
      if(casoRMedic() == 3){
        
        
        # # 
        # 
        # # Caso 3: 2Q
        callModule(module = Ho2Q_SERVER, id =  "ho3",
                   minibase = MiniBase,
                   casoRMedic = casoRMedic,
                   caso = 3,
                   decimales = decimales,
                   alfa = alfa,
                   batalla_naval = batalla_naval)
        
      }
    })
    
    # Caso 4 - 2C
    observeEvent(list(casoRMedic(), list_principal_selector_01_PRUEBAS(),
                      MiniBase(), batalla_naval(), decimales(), alfa()),{
                        
        req(list(casoRMedic(), list_principal_selector_01_PRUEBAS(),
                 MiniBase(), batalla_naval(), decimales(), alfa()))
        
      
      if(casoRMedic() == 4){
        
        
        # Caso 4: 2C
        callModule(module = Ho2C_SERVER, id =  "ho4",
                   minibase = MiniBase,
                   casoRMedic = casoRMedic,
                   caso = 4,
                   decimales = decimales,
                   alfa = alfa,
                   batalla_naval = batalla_naval)
      }
    })
    
    # Caso 5 - QC
    observeEvent(list(casoRMedic(), list_principal_selector_01_PRUEBAS(),
                      MiniBase(), batalla_naval(), decimales(), alfa()),{
                        
          req(list(casoRMedic(), list_principal_selector_01_PRUEBAS(),
                   MiniBase(), batalla_naval(), decimales(), alfa()))
                        
      
      if(casoRMedic() == 5){
        
        # 
        # 
        # Caso 5: 2Q
        callModule(module = HoQC_SERVER, 
                   id =  "ho5",
                   minibase = MiniBase,
                   casoRMedic = casoRMedic,
                   caso = 5,
                   decimales = decimales,
                   alfa = alfa,
                   batalla_naval = batalla_naval)
        # 
        
      }
    })
    
    
    
    ###################################################################### 
    
    
    my_case <- reactive({
      
      req(batalla_naval)
      #UserSelection$batalla_naval()$caso_tipo_variables
      batalla_naval()$caso_tipo_variables
    })
    
    
    # Modules Soft - Tabs mapping
    tablas_server_modules <- list(
      "1" = "modules_03_tablas_01_Tablas1Q_SERVER",
      "2" = "modules_03_tablas_02_Tablas1C_SERVER",
      "3" = "modules_03_tablas_03_Tablas2Q_SERVER",
      "4" = "modules_03_tablas_04_Tablas2C_SERVER",
      "5" = "modules_03_tablas_05_TablasQC_SERVER"
    )
    
    observeEvent(my_case(), {
      #req(base, UserSelection)
      
      selected_module_name <- tablas_server_modules[[my_case()]]
      req(selected_module_name)
      
      callModule(module = get(selected_module_name),
                 id = paste0("tablas_ho", my_case()),
                 minibase = MiniBase,
                 batalla_naval = batalla_naval,
                 decimales = decimales)
      
    })
    
    
    
    
    output$ui_menuHO <- renderUI({
      req(check_ok())
      
      # # Si no hay orden de salir a la cancha... Nadie sale...
      # if(is.null(RMedic_general())) return(NULL)
      # if(!RMedic_general()) return(NULL)
      # 
      # # Si no hay status de BaseSalida(), nos vamos...
      # if(is.null(status_BaseSalida())) return(NULL)
      # if(!status_BaseSalida()) return(NULL)
      
      fluidRow(
        #column(1),
        column(12,
               #h3_mod("Menú para Prueba de Hipótesis"),
               #modules_05_ho_VarSelector_UI(ns("var_selection_graficos")),
               #MiniBaseUI(ns("minibase_ho")),
               #hr_mod(),
               Ho1Q_UI(ns("ho1")),
               Ho1C_UI(ns("ho2")),
               Ho2Q_UI(ns("ho3")),
               Ho2C_UI(ns("ho4")),
               HoQC_UI(ns("ho5")),
               # Graficos1C_UI(ns("graficos04")),
               # Graficos2Q_UI(ns("graficos05")),
               # Graficos2C_UI(ns("graficos06")),
               # GraficosQC_UI(ns("graficos07")),
               br(), br(), br(), br(), br(),
               modules_03_tablas_01_Tablas1Q_UI(ns("tablas_ho1")),
               modules_03_tablas_02_Tablas1C_UI(ns("tablas_ho2")),
               modules_03_tablas_03_Tablas2Q_UI(ns("tablas_hot3")),
               modules_03_tablas_04_Tablas2C_UI(ns("tablas_ho4")),
               modules_03_tablas_05_TablasQC_UI(ns("tablas_ho5"))            )
#        column(1)
      )
      
      
    })
    
    
    
  })
  
  
}

