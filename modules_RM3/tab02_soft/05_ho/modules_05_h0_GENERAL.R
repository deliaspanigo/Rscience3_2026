

module_05_ho_GENERAL_UI <- function(id) {
  
  ns <- NS(id)
  
  uiOutput(ns("ui_menuHO"))
  
  
}



module_05_ho_GENERAL_SERVER <-  function(input, output, session, base,
                            RMedic_general, status_BaseSalida,
                            zocalo_CIE) { 
  
  observe({
    
    req(RMedic_general(), status_BaseSalida())
    
    # NameSpaceasing for the session
    ns <- session$ns
    
    
    UserSelection <- callModule(module = modules_05_ho_VarSelector_SERVER, 
                                id =  "var_selection_graficos",
                                base = base,
                                zocalo_CIE = zocalo_CIE,
                                verbatim = FALSE)
    
    batalla_naval <- UserSelection$batalla_naval
    casoRMedic <- reactive({
      
      if(is.null(batalla_naval())) return(NULL)
      if(is.null(batalla_naval()[[4]])) return(NULL)
      if(length(batalla_naval()[[4]]) == 0) return(NULL)
      if(batalla_naval()[[4]] == '') return(NULL)
      casoRMedic <- batalla_naval()[[4]]
      #casoRMedic <- as.numeric(as.character(as.vector(batalla_naval()[[4]])))
      casoRMedic
      
    })
    decimales <- UserSelection$decimales
    alfa <- UserSelection$alfa
    
    # observe(cat("casoRMedic()1: ", casoRMedic(), "\n"))
    
    MiniBase <- callModule(module = MiniBaseSERVER, 
                           id =  "minibase_ho",
                           base = base,
                           batalla_naval = UserSelection$batalla_naval,
                           verbatim = FALSE)
    
    
    
    
    

    observeEvent(list(casoRMedic(), batalla_naval()),{
      req(casoRMedic())
      if(casoRMedic() == 1){
        
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
    
    
    observeEvent(list(casoRMedic(), batalla_naval()),{
      req(casoRMedic())
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
    
    
    observeEvent(list(casoRMedic(), batalla_naval()),{
      req(casoRMedic())
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
    
    
    observeEvent(list(casoRMedic(), batalla_naval()),{
      req(casoRMedic())
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
    
    
    observeEvent(list(casoRMedic(), batalla_naval()),{
      req(casoRMedic())
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
                 id = paste0("tablas_ho", my_case()),
                 minibase = MiniBase,
                 batalla_naval = UserSelection$batalla_naval,
                 decimales = UserSelection$decimales)
      
    })
    
    
    
    
    output$ui_menuHO <- renderUI({
      
      # Si no hay orden de salir a la cancha... Nadie sale...
      if(is.null(RMedic_general())) return(NULL)
      if(!RMedic_general()) return(NULL)
      
      # Si no hay status de BaseSalida(), nos vamos...
      if(is.null(status_BaseSalida())) return(NULL)
      if(!status_BaseSalida()) return(NULL)
      
      fluidRow(
        column(1),
        column(10,
               h3_mod("Menú para Prueba de Hipótesis"),
               modules_05_ho_VarSelector_UI(ns("var_selection_graficos")),
               MiniBaseUI(ns("minibase_ho")),
               hr_mod(),
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
               modules_03_tablas_Tablas1Q_UI(ns("tablas_ho1")),
               modules_03_tablas_Tablas1C_UI(ns("tablas_ho2")),
               modules_03_tablas_Tablas2Q_UI(ns("tablas_hot3")),
               modules_03_tablas_Tablas2C_UI(ns("tablas_ho4")),
               modules_03_tablas_TablasQC_UI(ns("tablas_ho5"))            ),
        column(1)
      )
      
      
    })
    
    
    
  })
  
  
}