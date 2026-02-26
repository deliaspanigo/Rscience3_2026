

# tabPanel(title = "RMedic",
div(
         titlePanel("R+Medic"),

         br(), br(),
         fluidRow(
           column(1),
           column(4,
                  bsButton("showpanel", "Ocultar/Mostrar Carga de Datos", 
                           type = "toggle", value = TRUE,
                           icon("bars"), style = "primary", size = "large"
                  )
           )
         ),
         br(), br(),
         
         sidebarLayout(
           div(id = "MySidebar",
               
               sidebarPanel(id = "Sidebar", 
                            SideBarBaseUI("base01"))),
           mainPanel(id = "Main",
                     uiOutput("RMedicSoft"),
                     br(), br(), br(),br(),br(),br(),br()
                     
           )
           # End MainPanel ------------------------------------------
         ) 
)