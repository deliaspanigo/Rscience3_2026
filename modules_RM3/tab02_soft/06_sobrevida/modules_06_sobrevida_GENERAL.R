

module_06_sobrevida_GENERAL_UI <- function(id) {
  
  ns <- NS(id)
  
  uiOutput(ns("ui_menuSOBREVIDA"))
  
  
}



module_06_sobrevida_GENERAL_SERVER <-  function(input, output, session, base,
                                   RMedic_general, status_BaseSalida,
                                   zocalo_CIE) { 
  
  
  observe({
    
    req(RMedic_general(), status_BaseSalida())
    
    # NameSpaceasing for the session
    ns <- session$ns
    
    
    # Sobrevida General
    UserSelection <- callModule(module = modules_06_sobrevida_VarSelector_SERVER, 
                                id =  "sobrevida01",
                                base = base,
                                zocalo_CIE = zocalo_CIE,
                                verbatim = FALSE)
    
    OpcionesColumnas <- UserSelection$OpcionesColumnas
    
    
    
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
    caso <- 4
    # Control ejecucion 01
    control_ejecucion <- reactive({
      
      ejecucion <- FALSE
      if(is.null(casoRMedic())) return(ejecucion)
      if(is.null(caso)) return(ejecucion)
      
      if(casoRMedic() == caso) {
        
        if(batalla_naval()[[6]]) ejecucion <- TRUE else ejecucion <- FALSE 
        
      } else ejecucion <- FALSE
      
      
      return(ejecucion)
      
    })
    
    
    decimales <- UserSelection$decimales
    
    alfa <- UserSelection$alfa
    
    # var_general <- UserSelection$batalla_naval[[1]]
    
    minibase <- callModule(module = MiniBaseSERVER, 
                           id =  "sobrevida02",
                           base = base,
                           batalla_naval = UserSelection$batalla_naval,
                           verbatim = FALSE)
    
    
    
    
    callModule(module = modules_06_sobrevida_KM_SobrevidaGeneral_SERVER,
               id =  "sobrevida03",
               minibase = minibase,
               decimales = decimales,
               alfa = alfa,
               control_ejecucion = control_ejecucion)
    
    
    
    #############################################################################
    
    # Sobrevida por grupos
    
    var_grupo <- callModule(module = modules_06_sobrevida_GroupSelector_SERVER,
                            id =  "sobrevidagrupo01",
                            OpcionesColumnas = OpcionesColumnas)
    
    minibase2 <- callModule(module = MiniBaseSERVER2, 
                            id =  "sobrevidagrupo02",
                            base = base,
                            batalla_naval = batalla_naval,
                            var_grupo = var_grupo,
                            verbatim = FALSE)
    
    
    callModule(module = modules_06_sobrevida_KM_SobrevidaGroup_SERVER,
               id =  "sobrevidagrupo03",
               minibase = minibase2,
               decimales = decimales,
               alfa = alfa,
               control_ejecucion = control_ejecucion)
    
    output$ui_menuSOBREVIDA <- renderUI({
      
      # Si no hay orden de salir a la cancha... Nadie sale...
      if(is.null(RMedic_general())) return(NULL)
      if(!RMedic_general()) return(NULL)
      
      # Si no hay status de BaseSalida(), nos vamos...
      if(is.null(status_BaseSalida())) return(NULL)
      if(!status_BaseSalida()) return(NULL)
      
      
      fluidRow(
        column(1),
        column(10,
               h3_mod("Menú para Sobrevida"),
               modules_06_sobrevida_VarSelector_UI(ns("sobrevida01")),
               MiniBaseUI(ns("sobrevida02")),
               hr_mod(),
               br(),
               div(
                 tabsetPanel(
                   tabPanel("Sobrevida General", 
                            modules_06_sobrevida_KM_SobrevidaGeneral_UI(ns("sobrevida03"))),
                   tabPanel("Sobrevida por Grupos",
                            modules_06_sobrevida_GroupSelector_UI(ns("sobrevidagrupo01")),
                            modules_06_sobrevida_KM_SobrevidaGroup_UI(ns("sobrevidagrupo03")),
                            br(), br(), br(), br(), br()
                   )
                 )
                 
                 
                 
               )
        ),
        column(1)
      )
      
      
    })
    
    
    
   
  })
}