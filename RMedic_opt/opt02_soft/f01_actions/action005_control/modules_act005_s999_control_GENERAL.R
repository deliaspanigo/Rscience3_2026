module_action005_s999_control_GENERAL_ui <- function(id) {
  
  ns <- NS(id)
  uiOutput(ns("ui_menuCONTROL"))
  
}


module_action005_s999_control_GENERAL_server <- function(id, list_principal_selector_01_CONTROL) {
  moduleServer(id, function(input, output, session) {
    
  
    # NameSpaceasing for the session
    ns <- session$ns
    
    check_ok <- reactive({
      req(list_principal_selector_01_CONTROL(), list_principal_selector_01_CONTROL()$my_minibase)
      
      mis_columnas  <- colnames(list_principal_selector_01_CONTROL()$my_minibase)
      mis_variables <- list_principal_selector_01_CONTROL()$list_info_seleccion$batalla_naval$variables
      the_check <- all(mis_variables %in% mis_columnas)
      
      req(the_check)
      

      TRUE
    })
    # My case - Entre 1 y 5
    # Q = 1 - C = 2 - QQ = 3 - CC = 4 - QC = 5 - CQ = 5
    my_case <- reactive({
      
      req(check_ok())
      
      aver <- as.character(list_principal_selector_01_CONTROL()$list_info_seleccion$batalla_naval$caso_tipo_variables)
      #print(aver)
      aver
    })
    
    
    # Modules Soft - Tabs mapping
    control_server_modules <- list(
      "1" = "modules_02_control_01_Control1Q_SERVER",
      "2" = "modules_02_control_02_Control1C_SERVER",
      "3" = "modules_02_control_03_Control2Q_SERVER",
      "4" = "modules_02_control_04_Control2C_SERVER",
      "5" = "modules_02_control_05_ControlQC_SERVER"
    )
    
    
    observeEvent(list(check_ok(), my_case(), list_principal_selector_01_CONTROL()), {
      req(check_ok(), my_case(), list_principal_selector_01_CONTROL())
      
      selected_module_name <- control_server_modules[[my_case()]]
      req(selected_module_name)
      
      callModule(module = get(selected_module_name),
                 id = paste0("control", my_case()),
                 base = reactive(list_principal_selector_01_CONTROL()$my_minibase),
                 batalla_naval = reactive(list_principal_selector_01_CONTROL()$list_info_seleccion$batalla_naval),
                 decimales = list_principal_selector_01_CONTROL()$list_initial$decimales) 
      
    })
    
    
    
    
    
    
    output$ui_menuCONTROL <- renderUI({
      
      req(check_ok(), list_principal_selector_01_CONTROL())
      
      
      fluidRow(
        column(1),
        column(10,
               modules_02_control_01_Control1Q_UI(ns("control1")),
               modules_02_control_02_Control1C_UI(ns("control2")),
               modules_02_control_03_Control2Q_UI(ns("control3")),
               modules_02_control_04_Control2C_UI(ns("control4")),
               modules_02_control_05_ControlQC_UI(ns("control5"))
        ),
        column(1)
      )
      
    })
    
    
    
  })
  
}


########################################################################################
