

module_action007_s999_graficos_GENERAL_ui <- function(id) {
  
  ns <- NS(id)
  
  uiOutput(ns("ui_menuGRAFICOS"))
  
  
  
  
}



module_action007_s999_graficos_GENERAL_server <-  function(id, list_principal_selector_01_GRAFICOS) {
  moduleServer(id, function(input, output, session) {
  
    # NameSpaceasing for the session
    ns <- session$ns
    
    
    # My case - Entre 1 y 5
    # Q = 1 - C = 2 - QQ = 3 - CC = 4 - QC = 5 - CQ = 5
    casoRMedic <- reactive({
      
      req(list_principal_selector_01_GRAFICOS())
      aver <- as.character(list_principal_selector_01_GRAFICOS()$list_info_seleccion$batalla_naval$caso_tipo_variables)
      #print(aver)
      aver
    })
    
    
    ##########################################################################
    
    MiniBase <- reactive({
      req(list_principal_selector_01_GRAFICOS())
      
      vector_selected_vars <- list_principal_selector_01_GRAFICOS()$list_info_seleccion$batalla_naval[[1]]
      vector_fusion_system_ref_B <- list_principal_selector_01_GRAFICOS()$list_info_seleccion$batalla_naval$fusion_system_ref_B
      if(vector_fusion_system_ref_B == "CQ") vector_selected_vars <- vector_selected_vars[c(2,1)]
      
      # # #
      minibase <- list_principal_selector_01_GRAFICOS()$my_minibase[vector_selected_vars]
      minibase
    })
    
    batalla_naval <- reactive({
      req(list_principal_selector_01_GRAFICOS())
      list_principal_selector_01_GRAFICOS()$list_info_seleccion$batalla_naval
      })
    
    decimales <- reactive({
      req(list_principal_selector_01_GRAFICOS())
      
      list_principal_selector_01_GRAFICOS()$list_initial$decimales
      })
    ##########################################################################
    
    # Caso 1: 1Q    " ESTADO OK"
    observeEvent(list(casoRMedic(), list_principal_selector_01_GRAFICOS()),{
      req(casoRMedic(), list_principal_selector_01_GRAFICOS())
      
      
      
      
      
      
      if(casoRMedic() == 1){
        callModule(module = modules_04_graficos_01_Graficos1Q_SERVER, id =  "graficos1",
                   minibase = MiniBase,
                   casoRMedic = casoRMedic,
                   caso = 1,
                   decimales = decimales,
                   batalla_naval = batalla_naval)
      }
    })
    

    # Caso 2 : 1C
    observeEvent(list(casoRMedic(), batalla_naval()),{
      req(casoRMedic())
      if(casoRMedic() == 2){
        
        callModule(module = modules_04_graficos_02_Graficos1C_SERVER, id =  "graficos2",
                   minibase = MiniBase,
                   casoRMedic = casoRMedic,
                   caso = 2,
                   decimales = decimales,
                   batalla_naval = batalla_naval)
      }
    })
    

    # Caso 3: 2Q
    observeEvent(list(casoRMedic(), batalla_naval()),{
      req(casoRMedic())
      if(casoRMedic() == 3){
        
        callModule(module = modules_04_graficos_03_Graficos2Q_SERVER, id =  "graficos3",
                   minibase = MiniBase,
                   casoRMedic = casoRMedic,
                   caso = 3,
                   decimales = decimales,
                   batalla_naval = batalla_naval)

      }
    })
    
    
    # Caso 4: 2C
    observeEvent(list(casoRMedic(), batalla_naval()),{
      req(casoRMedic())
      if(casoRMedic() == 4){
        


        callModule(module = modules_04_graficos_04_Graficos2C_SERVER, id =  "graficos4",
                   minibase = MiniBase,
                   casoRMedic = casoRMedic,
                   caso = 4,
                   decimales = decimales,
                   batalla_naval = batalla_naval)

      }
    })
    
    
    # Caso 5: QC
    observeEvent(list(casoRMedic(), batalla_naval()),{
      req(casoRMedic())
      if(casoRMedic() == 5){
        
        
      

          callModule(module = modules_04_graficos_05_GraficosQC_SERVER, id =  "graficos5",
                     minibase = MiniBase,
                     casoRMedic = casoRMedic,
                     caso = 5,
                     decimales = decimales,
                     batalla_naval = batalla_naval)
          
      }
    })
    
    
    ###################################################################### 
    
    
    my_case2 <- reactive({
      
      req(list_principal_selector_01_GRAFICOS())
      aver <- as.character(list_principal_selector_01_GRAFICOS()$list_info_seleccion$batalla_naval$caso_tipo_variables)
      #print(aver)
      aver
    })
    
    
    # Modules Soft - Tabs mapping
    tablas_server_modules <- list(
      "1" = "modules_03_tablas_01_Tablas1Q_SERVER",
      "2" = "modules_03_tablas_02_Tablas1C_SERVER",
      "3" = "modules_03_tablas_03_Tablas2Q_SERVER",
      "4" = "modules_03_tablas_04_Tablas2C_SERVER",
      "5" = "modules_03_tablas_05_TablasQC_SERVER"
    )
    
    observeEvent(my_case2(), {
      req(list_principal_selector_01_GRAFICOS())
      
      selected_module_name <- tablas_server_modules[[my_case2()]]
      req(selected_module_name)
      
      vector_selected_vars <- list_principal_selector_01_GRAFICOS()$list_info_seleccion$batalla_naval[[1]]
      vector_fusion_system_ref_B <- list_principal_selector_01_GRAFICOS()$list_info_seleccion$batalla_naval$fusion_system_ref_B
      if(vector_fusion_system_ref_B == "CQ") vector_selected_vars <- vector_selected_vars[c(2,1)]
      
      
      callModule(module = get(selected_module_name),
                 id = paste0("tablas_plot", my_case2()),
                 minibase = reactive(list_principal_selector_01_GRAFICOS()$my_minibase[vector_selected_vars]),
                 batalla_naval = reactive(list_principal_selector_01_GRAFICOS()$list_info_seleccion$batalla_naval),
                 decimales = reactive(list_principal_selector_01_GRAFICOS()$list_initial$decimales))
      
    })
    
    
    
    # # TABLAS!
    # callModule(module = modules_03_tablas_Tablas1Q_SERVER, id =  "graficos08",
    #            minibase = MiniBase,
    #            batalla_naval = UserSelection$batalla_naval,
    #            decimales = decimales)
    # 
    # 
    # callModule(module = modules_03_tablas_Tablas1C_SERVER, id =  "graficos09",
    #            minibase = MiniBase,
    #            batalla_naval = UserSelection$batalla_naval,
    #            decimales = decimales)
    # 
    # 
    # callModule(module = modules_03_tablas_Tablas2Q_SERVER, id =  "graficos10",
    #            minibase = MiniBase,
    #            batalla_naval = UserSelection$batalla_naval,
    #            decimales = decimales)
    # 
    # callModule(module = modules_03_tablas_Tablas2C_SERVER, id =  "graficos11",
    #            minibase = MiniBase,
    #            batalla_naval = UserSelection$batalla_naval,
    #            decimales = decimales)
    # 
    # callModule(module = modules_03_tablas_TablasQC_SERVER, id =  "graficos12",
    #            minibase = MiniBase,
    #            batalla_naval = UserSelection$batalla_naval,
    #            decimales = decimales)
    
    
    output$ui_menuGRAFICOS <- renderUI({
      
      req(list_principal_selector_01_GRAFICOS())
      
      
      fluidRow(
        column(1),
        column(10,
               modules_04_graficos_01_Graficos1Q_UI(ns("graficos1")),
               modules_04_graficos_02_Graficos1C_UI(ns("graficos2")),
               modules_04_graficos_03_Graficos2Q_UI(ns("graficos3")),
               modules_04_graficos_04_Graficos2C_UI(ns("graficos4")),
               modules_04_graficos_05_GraficosQC_UI(ns("graficos5")),
               br(), br(), br(), br(), br(),
               modules_03_tablas_01_Tablas1Q_UI(ns("tablas_plot1")),
               modules_03_tablas_02_Tablas1C_UI(ns("tablas_plot2")),
               modules_03_tablas_03_Tablas2Q_UI(ns("tablas_plot3")),
               modules_03_tablas_04_Tablas2C_UI(ns("tablas_plot4")),
               modules_03_tablas_05_TablasQC_UI(ns("tablas_plot5"))
        ),
        column(1)
      )
      
      
      
    })
    
   
    
  })
}