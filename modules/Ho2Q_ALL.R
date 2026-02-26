## Segmento del UI
Ho2Q_UI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("SeccionHo2Q"))
  
  
}




## Segmento del server
Ho2Q_SERVER <- function(input, output, session, 
                              minibase,
                              casoRMedic,
                              caso,
                              decimales,
                              alfa,
                              batalla_naval) {
  
  
  
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  
  # Control ejecucion 01
  control_ejecucion <- reactive({
    
    
    if(is.null(casoRMedic())) return(FALSE)
    if(is.null(caso)) return(FALSE)
    
    if(casoRMedic() == caso) return(TRUE) else return(FALSE)
    
  })
  
  
  
  tablas_2q <-  reactive({
    # Control interno 01
    if(!control_ejecucion()) return(NULL)
    
    RMedic_2q_tablas(minibase(), decimales())
    })
  
  
  callModule(module = Ho2Q_01_RMedicHelp_SERVER,
             id =  "ho05A",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion,
             tablas_2q = tablas_2q,
             alfa = alfa)

  
  callModule(module = Ho2Q_02_TestDeDosProporciones_SERVER,
             id =  "ho05B",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion,
             tablas_2q = tablas_2q,
             alfa = alfa)

  
  callModule(module = Ho2Q_03_TestChiCuadrado_SERVER,
             id =  "ho05C",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion,
             alfa = alfa)

  
  callModule(module = Ho2Q_04_TestRegLogSimple_SERVER,
             id =  "ho05D",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion,
             alfa = alfa)
 
  
  callModule(module = Ho2Q_05_Otros_SERVER,
             id =  "ho05E",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion,
             alfa = alfa)
  
  output$SeccionHo2Q <- renderUI({
    
    # Especificaciones de cumplimiento
    if(is.null(control_ejecucion())) return(NULL)
    if(!control_ejecucion()) return(NULL)
    
    
    
    # Si es el caso 4, seguimos!
    div(
      h2("RMedic - Prueba de hipótesis para 2 Variables Categóricas"),
      tabsetPanel(id = ns("Ho_2q"),
                  tabPanel(title = "RMedic Help!", value = 1,
                           Ho2Q_01_RMedicHelp_UI(ns("ho05A"))
                          ) ,
                   tabPanel(title = "Test de Dos Proporciones", value = 2,
                            Ho2Q_02_TestDeDosProporciones_UI(ns("ho05B"))
                            ),
                  tabPanel(title = "Test Chi Cuadrado", value = 3,
                           Ho2Q_03_TestChiCuadrado_UI(ns("ho05C"))
                  ),
                  tabPanel(title = "Regresión Logística Simple", value = 4,
                           Ho2Q_04_TestRegLogSimple_UI(ns("ho05D"))
                  ),
                  tabPanel(title = "Otros", value = 5,
                           Ho2Q_05_Otros_UI(ns("ho05E"))
                  )
      )
    )
    
    
    
  })
  
  
  
  
  
  
}


