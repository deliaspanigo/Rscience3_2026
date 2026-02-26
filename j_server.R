
# rm(list = ls())

function(input, output, session) {
  
  
 
  
  observeEvent(input$showpanel, {
    
    if(input$showpanel == TRUE) {
      removeCssClass("Main", "col-sm-12")
      addCssClass("Main", "col-sm-8")
      shinyjs::show(id = "Sidebar")
      shinyjs::enable(id = "Sidebar")
    }
    else {
      removeCssClass("Main", "col-sm-8")
      addCssClass("Main", "col-sm-12")
      shinyjs::hide(id = "Sidebar")
    }
  })
  
  
  
  

  

  
  
  ##############################################
   
  
  
  
  ########################################################
  
 
  
  # 1 - Base ------------------------------------------------
  {
  ###
    
  # Modulo01... Nos otorga
  # 1) BaseSalida
  # 2) zocalo_CIE
  # 3) RMedic_general
  # 4) status_BaseSalida
 Modulo01 <- callModule(module = SideBarBaseSERVER, id =  "base01")
 zocalo_CIE <- Modulo01$zocalo_CIE
 RMedic_general <- Modulo01$RMedic_general
 status_BaseSalida <- Modulo01$status_BaseSalida
  
 

 # observe(cat(zocalo_CIE()))

 menuBASE <- reactive({


   tabs <- list()

   tabs[[1]] <-    tabPanel(title = "Base de Datos",
                            # icon = icon("user-md"),
                            value = 1,
                            br(),
                            fluidRow(

                              MiBase01_UI("base01")#,

                            ),
                            br(), br()
   )

   tabs

 })
 
  ###
  }
  ###########################################################
  
  
  # 2 - Control ----------------------------------------------
  {
  ###
  
    menuCONTROL <- callModule(module = ModuleControlSERVER, 
                              id =  "menuCONTROL",
                              base = Modulo01$BaseSalida,
                              RMedic_general = RMedic_general,
                              status_BaseSalida = status_BaseSalida,
                              zocalo_CIE = zocalo_CIE)
  
  
  
  ###
  }
  ###########################################################
  
  
  
  
  
  # 3 - Tablas ----------------------------------------------
  {
    ###
    
    menuTABLAS <- callModule(module = ModuleTablasSERVER, 
                             id =  "menuTABLAS",
                             base = Modulo01$BaseSalida,
                             RMedic_general = RMedic_general,
                             status_BaseSalida = status_BaseSalida,
                             zocalo_CIE = zocalo_CIE)
    
    
    
    ###
  }
  ###########################################################
  
  
 
  # 4 - Graficos ----------------------------------------------
  {
    ###
    
    menuGRAFICOS <- callModule(module = ModuleGraficosSERVER, 
                              id =  "menuGRAFICOS",
                              base = Modulo01$BaseSalida,
                              RMedic_general = RMedic_general,
                              status_BaseSalida = status_BaseSalida,
                              zocalo_CIE = zocalo_CIE)
    
    
    
    ###
  }
  ###########################################################
  
  
 
  # 5 - Ho ----------------------------------------------
  {
    ###
    
    menuHO <- callModule(module = ModuleHoSERVER, 
                         id =  "menuHO",
                         base = Modulo01$BaseSalida,
                         RMedic_general = RMedic_general,
                         status_BaseSalida = status_BaseSalida,
                         zocalo_CIE = zocalo_CIE)
    
    
    
    ###
  }
  ###########################################################
  
  
  
 
  # 6 - Sobrevida ----------------------------------------------
  {
    ###
    
    menuSOBREVIDA <- callModule(module = ModuleSobrevidaSERVER, 
                             id =  "menuSOBREVIDA",
                             base = Modulo01$BaseSalida,
                             RMedic_general = RMedic_general,
                             status_BaseSalida = status_BaseSalida,
                             zocalo_CIE = zocalo_CIE)
    
    
    
    ###
  }
  ###########################################################
  
 
  
  observe(output[["RMedicSoft"]] <- renderUI({
    
    
    # do.call(tabsetPanel,  c(id="goku", tabs1,tabs2, tabs3, tabs4, tabs5, tabs6))
    do.call(tabsetPanel,  c(id = "PanelRMedic", 
                            menuBASE(),
                            menuCONTROL() ,
                            menuTABLAS() ,
                            menuGRAFICOS() ,
                            menuHO(),
                            menuSOBREVIDA()
    )
    )
    
  }))
  
#################################################################################### 
  
  
 
  juntos <-   callModule(module = Menu_DistribucionGeneral01_SERVER, 
                id =  "distribucion01.general",
                carpeta_distribuciones = "009App/002_Distribucion_de_Probabilidades")
  
  
 
  
  callModule(module = SideBarDistribucionElegida01_SERVER, 
             id =  "espacio_elegido01",
            # la_distribucion = "001_Normal")
             la_distribucion = juntos)
  
  callModule(module = MainPanelDistribucionElegida01_SERVER, 
             id =  "espacio_elegido01",
             # la_distribucion = "001_Normal")
             la_distribucion = juntos)
  
  callModule(module = ServerDistribucionElegida01_SERVER, 
             id =  "espacio_elegido01",
             # la_distribucion = "001_Normal")
             la_distribucion = juntos)
  
  # Distribucion Normal
  callModule(module = Server01_Normal_Server, 
             id =  "aver01A",
             # la_distribucion = "001_Normal")
             la_distribucion = juntos)
  
  # # Distribucion t
  # callModule(module = Server02_t_Server, 
  #            id =  "aver01B",
  #            # la_distribucion = "001_Normal")
  #            la_distribucion = juntos)
  # 
  # # Distribucion Chi
  # callModule(module = Server03_chi_Server, 
  #            id =  "aver01C",
  #            # la_distribucion = "001_Normal")
  #            la_distribucion = juntos)
  # 
  # # Distribucion F
  # callModule(module = Server04_f_Server, 
  #            id =  "aver01D",
  #            # la_distribucion = "001_Normal")
  #            la_distribucion = juntos)
  
  #####################################################################################
  juntos2 <-   callModule(module = Menu_DistribucionGeneral02_SERVER, 
                         id =  "distribucion02.general",
                         carpeta_distribuciones = "009App/002_Distribucion_de_Probabilidades")
  
  
  
  
  callModule(module = SideBarDistribucionElegida02_SERVER, 
             id =  "espacio_elegido02",
             # la_distribucion = "001_Normal")
             la_distribucion = juntos2)
  
  callModule(module = MainPanelDistribucionElegida02_SERVER, 
             id =  "espacio_elegido02",
             # la_distribucion = "001_Normal")
             la_distribucion = juntos2)
  
  callModule(module = ServerDistribucionElegida02_SERVER, 
             id =  "espacio_elegido02",
             # la_distribucion = "001_Normal")
             la_distribucion = juntos2)
  
  # Distribucion Normal
  callModule(module = Server01_Normal_Server02, 
             id =  "aver02A",
             # la_distribucion = "001_Normal")
             la_distribucion = juntos2)
  

  # Distribucion t
  callModule(module = Server02_t_Server02,
             id =  "aver02B",
             # la_distribucion = "001_Normal")
             la_distribucion = juntos2)
  
  # Distribucion Chi
  callModule(module = Server03_chi_Server02,
             id =  "aver02C",
             # la_distribucion = "001_Normal")
             la_distribucion = juntos2)

  # Distribucion F
  callModule(module = Server04_f_Server02,
             id =  "aver02D",
             # la_distribucion = "001_Normal")
             la_distribucion = juntos2)
  
}


