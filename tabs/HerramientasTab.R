

# tabPanel(title = "RMedic",
div(
  # titlePanel("R+Medic"),
  # 
  # br(), br(),
  selectInput(inputId = "selector_general",
              label = h2("Lista de Herramientas"), 
              choices = c("Distribución de Probabilidades" = "opc01",
                          "Tablas de Probabilidades" = "opc02",
                          "Otras Herramientas" = "opc03")
  ), br(), br(),
  conditionalPanel("input.selector_general == 'opc01'",
                   Menu_DistribucionGeneral01_UI("distribucion01.general")
                   ),
  conditionalPanel("input.selector_general == 'opc02'",
                   Menu_DistribucionGeneral02_UI("distribucion02.general")
                   ),
                   br(), br(),
  sidebarLayout(
    div(id = "MySidebar2",
        
  
        sidebarPanel(id = "Sidebar2", 

       conditionalPanel("input.selector_general == 'opc01'",
                     #SideBarDistribucionGeneral_UI("aver2"),
                     #br(), br(),
                     SideBarDistribucionElegida01_UI("espacio_elegido01")
                     ),
       conditionalPanel("input.selector_general == 'opc02'",
                        # SideBarDistribucionGeneral2_UI("aver3"),
                        # br(), br(),
                        SideBarDistribucionElegida02_UI("espacio_elegido02")
       )
       

        )
      ),
    mainPanel(id = "Main2",
              # uiOutput("RMedicSoft"),
              conditionalPanel("input.selector_general == 'opc01'",
              MainPanelDistribucionElegida01_UI("espacio_elegido01")
              ),
              conditionalPanel("input.selector_general == 'opc02'",
                               MainPanelDistribucionElegida02_UI("espacio_elegido02")
              ),
              br(), br(), br(),br(),br(),br(),br()
              
    )
    # End MainPanel ------------------------------------------
  ) 
)