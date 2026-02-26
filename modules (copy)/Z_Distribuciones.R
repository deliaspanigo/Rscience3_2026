## Segmento del UI
Menu_DistribucionGeneral01_UI <- function(id) {
  ns <- NS(id)
  
  
 
  uiOutput(ns("outMe_distribucion01_general"))
  
  
}



 ## Segmento del server
Menu_DistribucionGeneral01_SERVER <- function(input, output, session,
                                              carpeta_distribuciones) {
   
  # NameSpaceasing for the session
  ns <- session$ns
  
   output$outMe_distribucion01_general <- renderUI({
     
     las_distribuciones <- list.files(carpeta_distribuciones)[1]
     
     # Este es el sidebarpanel completo
     div(  
       selectInput(inputId = ns("distribuciones"), 
                   label = 'Distribución:',
                   choices = las_distribuciones, 
                   selectize = T)
     )
   })
   

  la_elegida <- reactive({
     
     if(is.null(input$distribuciones)) return(NULL)
     return(input$distribuciones)
     
   })
   
   # la_salida <- list(distribucionb_elegida = la_elegida)
    la_salida <- la_elegida
    
   #  observe(cat( la_elegida()))
    
    
   return(la_salida)
 }
 
 
########################################################################################


## Segmento del UI
SideBarDistribucionElegida01_UI <- function(id) {
  ns <- NS(id)
  
  
  
  uiOutput(ns("outMe_sidebar_elegida"))
  
  
}

## Segmento del UI
MainPanelDistribucionElegida01_UI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("outMe_mainmenu_elegida"))

  
 
}

## Segmento del server
SideBarDistribucionElegida01_SERVER <- function(input, output, session,
                                              la_distribucion) {
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  output$outMe_sidebar_elegida <- renderUI({
    if(is.null(la_distribucion())) return(NULL)
    
    if (la_distribucion() == "001_Normal") return(SideBar01_Normal("aver01A")) else return(NULL)
      
    # if (la_distribucion() == "001_Normal") return(SideBar01_Normal("aver01A")) else
    #   if (la_distribucion() == "002_t_de_Student") return(SideBar02_t("aver01B")) else
    #     if (la_distribucion() == "003_Chi_Cuadrado") return(SideBar03_chi("aver01C")) else
    #       if (la_distribucion() == "004_F_de_Fisher") return(SideBar04_f("aver01D")) else
    #   return(NULL)
    

  })
  
  

}


## Segmento del server
MainPanelDistribucionElegida01_SERVER <- function(input, output, session,
                                              la_distribucion) {
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  output$outMe_mainmenu_elegida <- renderUI({
    if(is.null(la_distribucion())) return(NULL)
    
    if (la_distribucion() == "001_Normal") return(MainPanel01_Normal("aver01A")) else return(NULL)
    # if (la_distribucion() == "001_Normal") return(MainPanel01_Normal("aver01A")) else
    #   if (la_distribucion() == "002_t_de_Student") return(MainPanel02_t("aver01B")) else
    #     if (la_distribucion() == "003_Chi_Cuadrado") return(MainPanel03_chi("aver01C")) else
    #       if (la_distribucion() == "004_F_de_Fisher") return(MainPanel04_f("aver01D")) else
    #  return(NULL)
    
    
  })
  
  

}

ServerDistribucionElegida01_SERVER <- function(input, output, session,
                                              la_distribucion) {
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  
 # POR AHORA QUEDA VACIO!
  

}


########################################################################################

SideBar01_Normal <- function(id) {
  ns <- NS(id)
  
  
  
  div(
    h3("Distribución Normal"),
    actionButton(inputId = ns("calcular01"), label = "Calcular!"),br(),
    br(),
    br(),
    fluidRow(
      column(6,
             selectInput(inputId = ns("color_variable"), 
                         label = "Color:",
                         choices = c("Naranja" = "orange",
                                     "Rojo" = "red",
                                     "Verde" = "green",
                                     "Azul" = "blue", 
                                     "Amarillo" = "yellow", 
                                     "Negro" = "black",
                                     "Celeste" = "skyblue"), 
                         multiple = FALSE)
      ),
      column(6,
             numericInput(inputId = ns("decimals"),
                          label = "Decimales", 
                          min = 0,
                          step = 1,
                          value = 2)
      )
    ),
    br(),
    radioButtons(inputId = ns("info"), label = "Parámetros conocidos", 
                 choices = c("Media y Varianza" = 1, "Media y Desvío" = 2)
    ), br(),
    numericInput(inputId = ns("media_poblacional"),
                 label = "Media Poblacional:",
                 step= 0.01,
                 value= 106),
    conditionalPanel(condition = "input.info == 1", ns = ns, 
                     numericInput(inputId = ns("varianza_poblacional"),
                                  label = "Varianza Poblacional: ",
                                  min =0,
                                  step = 0.01,  
                                  value = 64)),
    conditionalPanel(condition = "input.info == 2", ns = ns, 
                     numericInput(inputId = ns("desvio_poblacional"),
                                  label = "Desvío Poblacional: ",
                                  min = 0,
                                  step = 0.01,  
                                  value = 8)),
    br(),
    radioButtons(inputId = ns("opciones"), 
                 label = "Datos conocidos:",
                 choices =  c("Valor de la variable" = "original",
                              "Valor Z" = "valor_z",
                              "Probabilidad" = "probabilidad",
                              "Porcentaje" = "porcentaje")),
    br(),
    radioButtons(inputId = ns("intervalo"), 
                 label = "Intervalo de cálculo:",
                 choices = c("Menores que..." = "menor",
                             "Mayores que..." = "mayor",
                             "Entre..." = "entre")
                 
    ),
    br(),
    
    conditionalPanel('input.opciones == "original"', ns = ns,
                     conditionalPanel('input.intervalo == "menor"', ns = ns,
                                      numericInput(inputId = ns("var1"), 
                                                   label = "Valor de la Variable Original:",
                                                   step= 0.01, 
                                                   value= 90)),
                     
                     conditionalPanel('input.intervalo == "mayor"', ns = ns,
                                      numericInput(inputId = ns("var2"),
                                                   label = "Valor de la Variable Original:",
                                                   step= 0.01, 
                                                   value= 110)),
                     
                     conditionalPanel('input.intervalo == "entre"', ns = ns,
                                      numericInput(inputId = ns("var3"), 
                                                   label = "Valor Izquierdo de la Variable Original:",
                                                   step= 0.01, 
                                                   value= 90),
                                      numericInput(inputId = ns("var4"),
                                                   label = "Valor Derecho de la Variable Original:",
                                                   step= 0.01, 
                                                   value= 110))),  
    
    conditionalPanel('input.opciones == "valor_z"', ns = ns,
                     conditionalPanel('input.intervalo == "menor"', ns = ns,
                                      numericInput(inputId = ns("z_var1"),
                                                   label = "Valor Z:", 
                                                   step= 0.01, 
                                                   value= -1.96)),
                     
                     conditionalPanel('input.intervalo == "mayor"', ns = ns,
                                      numericInput(inputId = ns("z_var2"),
                                                   label = "Valor Z:", 
                                                   step= 0.01, 
                                                   value= 1)),
                     
                     conditionalPanel('input.intervalo == "entre"', ns = ns,
                                      numericInput(inputId = ns("z_var3"),
                                                   label = "Valor Z Izquierdo :", 
                                                   step= 0.01, value= -1.96),
                                      numericInput(inputId = ns("z_var4"),
                                                   label = "Valor Z Derecho:", 
                                                   step= 0.01, 
                                                   value= 1))  
    ),
    conditionalPanel('input.opciones == "probabilidad"', ns = ns,
                     numericInput(inputId = ns("prob_var1"),
                                  label = "Probabilidad (Un valor entre 0 y 1):",
                                  min=0,  max=1, step= 0.01, value= 0.25)),
    conditionalPanel('input.opciones == "porcentaje"', ns = ns,
                     numericInput(inputId = ns("porcentaje_var1"),
                                  label = "Porcentaje (Un valor entre 0 y 100):",
                                  min=0,  max=100, step= 0.01, value= 25)),
    br(), br(),
    actionButton(inputId = ns("calcular02"), label = "Calcular!"),
    br(),br()
  )
  
  
  
}


MainPanel01_Normal <- function(id){
  
  ns <- NS(id)
  
  
  div(textOutput(ns("texto_aviso")),
      uiOutput(ns("armado01"))
  )
  
  
}


Server01_Normal_Server <- function(input, output, session,
                                   la_distribucion) {
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  # los_parametros <- reactive({
  
  result <- reactiveVal(0)
  # result <- reactiveVal() #Esto lo hace NULL al inicio
  observeEvent(input$calcular01, { result(result() + 1) })
  observeEvent(input$calcular02, { result(result() + 1) })
  
  los_parametros <- eventReactive(result(), {
    
    if(is.null(input$info)) return(NULL)
    
    media_poblacional <- NA
    varianza_poblacional <- NA
    desvio_poblacional <- NA
    opciones <- NA
    intervalo <- NA
    color_variable <- NA
    decimals <- NA
    var1 <- NA
    var2 <- NA
    var3 <- NA
    var4 <- NA
    z1 <- NA
    z2 <- NA
    z3 <- NA
    z4 <- NA
    probabilidad_externo <- NA
    porcentaje_externo <- NA
    
    color_variable <- input$color_variable
    media_poblacional <- input$media_poblacional
    
    if(input$info == 1){
      varianza_poblacional <- input$varianza_poblacional
      desvio_poblacional <- sqrt(varianza_poblacional)
    } else
      if(input$info == 2){
        desvio_poblacional <- input$desvio_poblacional
        varianza_poblacional <- desvio_poblacional^2
        
      }
    
    opciones <- input$opciones
    intervalo <- input$intervalo
    decimals <- input$decimals
    
    var1 <- input$var1
    var2 <- input$var2
    var3 <- input$var3
    var4 <- input$var4
    
    z1 <- input$z_var1
    z2 <- input$z_var2
    z3 <- input$z_var3
    z4 <- input$z_var4
    
    probabilidad_externo <- input$prob_var1
    porcentaje_externo <- input$porcentaje_var1
    
    
    los_parametros <- Hmisc::llist(media_poblacional, varianza_poblacional, desvio_poblacional,
                                   opciones, intervalo, color_variable,
                                   var1, var2, var3, var4, 
                                   z1, z2, z3, z4, 
                                   probabilidad_externo, porcentaje_externo, decimals)
    
    return(los_parametros)
    
    
    
  })
  
  RMedic_Info <- reactive({
    
    if(is.null(los_parametros())) return(NULL)
    
    mis_parametros <- los_parametros()
    
    out <-  do.call(Distribucion.Normal, mis_parametros) # works 
    out <- out$RMedic
    
    return(out)
  })
  
  
  
  
  output$tabla01 <- renderTable(align = "c", {
    
    if(is.null(RMedic_Info())) return(NULL)
    out <- RMedic_Info()$tabla_externa01
    out <- CharacterALL(out)
    out
  })
  
  output$tabla02 <- renderTable(align = "c",{
    
    if(is.null(RMedic_Info())) return(NULL)
    out <- RMedic_Info()$tabla_externa02
    out <- CharacterALL(out)
    out
    # tabla_externa02()
  })
  
  output$frase_final01 <- renderUI({
    
    if(is.null(RMedic_Info())) return(NULL)
    out <- RMedic_Info()$frase01
    HTML(out)
  })
  
  output$frase_final02 <- renderUI({
    
    if(is.null(RMedic_Info())) return(NULL)
    out <- RMedic_Info()$frase02
    HTML(out)
  })
  
  # Gráfico 1
  observe(output$distPlot1 <- renderPlot({
    
    if(is.null(RMedic_Info())) return(NULL)
    RMedic_Info()$grafico01
    
  }))
  
  
  output$distPlot2 <- renderPlot({
    
    if(is.null(RMedic_Info())) return(NULL)
    RMedic_Info()$grafico02
    
    
  })
  
  output$texto_aviso <- renderText({
    
    if(is.null(result())) return("Seleccione y de clic en 'Calcular!'") else
      return(NULL)
  })
  
  
  output$armado01 <- renderUI({
    
    if(is.null(result())) return(NULL)
    # if(is.null(los_parametros())) return("Seleccione y de clic en 'Calcular!'")
    # if (la_distribucion() != "001_Normal") return(NULL)
    
    div(h2("Distribución de la Variable"),
        plotOutput(ns("distPlot2")),
        br(),
        h2("Distribución Normal Estándar (Z)"),
        plotOutput(ns("distPlot1")), br(),
        br(),
        br(),
        
        fluidRow(
          column(6, 
                 h2("Parámetros"),
                 tableOutput(ns("tabla01")),
                 br(),
                 h2("Cálculos"),
                 tableOutput(ns("tabla02"))),
          column(6, 
                 h2("Detalles"),
                 htmlOutput(ns("frase_final01")),
                 br(),br(),
                 h2("Frase Explicativa"),
                 htmlOutput(ns("frase_final02")),
                 tags$head(tags$style(
                   paste0("#", ns("frase_final01"), "{color: black;
                                 font-size: 20px;
                                 font-style: italic;
                                 }")
                 )
                 ),
                 tags$head(tags$style(
                   paste0("#", ns("frase_final02"), "{color: black;
                                 font-size: 20px;
                                 font-style: italic;
                                 }")
                 )
                 )
                 
          )
        )
        , br(),
        # span(htmlOutput(ns("frase_control_qc")), style="color:red")
        
        br()
        
    )
  })
}


# ########################################################################################
# 
# # t 
# # # SidePanelNormal
# ## Segmento del UI
# SideBar02_t <- function(id) {
#   ns <- NS(id)
#   
#   
#   
#   div(
#     h3("Distribución t de Student"),
#     actionButton(inputId = ns("calcular01"), label = "Calcular!"),br(),
#     br(),
#     br(),
#     fluidRow(
#       column(6,
#              selectInput(inputId = ns("color_variable"), 
#                          label = "Color:",
#                          choices = c("Naranja" = "orange",
#                                      "Rojo" = "red",
#                                      "Verde" = "green",
#                                      "Azul" = "blue", 
#                                      "Amarillo" = "yellow", 
#                                      "Negro" = "black",
#                                      "Celeste" = "skyblue"), 
#                          multiple = FALSE)
#       ),
#       column(6,
#              numericInput(inputId = ns("decimals"),
#                           label = "Decimales", 
#                           min = 0,
#                           step = 1,
#                           value = 2)
#       )
#     ),
#     br(),
#     radioButtons(inputId = ns("info"), label = "Estimadores conocidos", 
#                  choices = c("Media y Varianza" = 1, "Media y Desvío" = 2)
#     ), br(),
#     numericInput(inputId = ns("media_muestral"),
#                  label = "Media Muestral:",
#                  step= 0.01,
#                  value= 106),
#     conditionalPanel(condition = "input.info == 1", ns = ns, 
#                      numericInput(inputId = ns("varianza_muestral"),
#                                   label = "Varianza Muestral: ",
#                                   min =0,
#                                   step = 0.01,  
#                                   value = 64)),
#     conditionalPanel(condition = "input.info == 2", ns = ns, 
#                      numericInput(inputId = ns("desvio_muestral"),
#                                   label = "Desvío Muestral: ",
#                                   min = 0,
#                                   step = 0.01,  
#                                   value = 8)),
#     numericInput(inputId = ns("gl"),
#                  label = "Grados de Libertad (n-1):",
#                  step= 1,
#                  value= 19),
#     br(),
#     radioButtons(inputId = ns("opciones"), 
#                  label = "Datos conocidos:",
#                  choices =  c("Valor de la variable" = "original",
#                               "Valor t" = "valor_t",
#                               "Probabilidad" = "probabilidad",
#                               "Porcentaje" = "porcentaje")),
#     br(),
#     radioButtons(inputId = ns("intervalo"), 
#                  label = "Intervalo de cálculo:",
#                  choices = c("Menores que..." = "menor",
#                              "Mayores que..." = "mayor",
#                              "Entre..." = "entre")
#                  
#     ),
#     br(),
#     
#     conditionalPanel('input.opciones == "original"', ns = ns,
#                      conditionalPanel('input.intervalo == "menor"', ns = ns,
#                                       numericInput(inputId = ns("var1"), 
#                                                    label = "Valor de la Variable Original:",
#                                                    step= 0.01, 
#                                                    value= 90)),
#                      
#                      conditionalPanel('input.intervalo == "mayor"', ns = ns,
#                                       numericInput(inputId = ns("var2"),
#                                                    label = "Valor de la Variable Original:",
#                                                    step= 0.01, 
#                                                    value= 110)),
#                      
#                      conditionalPanel('input.intervalo == "entre"', ns = ns,
#                                       numericInput(inputId = ns("var3"), 
#                                                    label = "Valor Izquierdo de la Variable Original:",
#                                                    step= 0.01, 
#                                                    value= 90),
#                                       numericInput(inputId = ns("var4"),
#                                                    label = "Valor Derecho de la Variable Original:",
#                                                    step= 0.01, 
#                                                    value= 110))),  
#     
#     conditionalPanel('input.opciones == "valor_t"', ns = ns,
#                      conditionalPanel('input.intervalo == "menor"', ns = ns,
#                                       numericInput(inputId = ns("t_var1"),
#                                                    label = "Valor t:", 
#                                                    step= 0.01, 
#                                                    value= -1.96)),
#                      
#                      conditionalPanel('input.intervalo == "mayor"', ns = ns,
#                                       numericInput(inputId = ns("t_var2"),
#                                                    label = "Valor t:", 
#                                                    step= 0.01, 
#                                                    value= 1)),
#                      
#                      conditionalPanel('input.intervalo == "entre"', ns = ns,
#                                       numericInput(inputId = ns("t_var3"),
#                                                    label = "Valor t Izquierdo :", 
#                                                    step= 0.01, value= -1.96),
#                                       numericInput(inputId = ns("t_var4"),
#                                                    label = "Valor t Derecho:", 
#                                                    step= 0.01, 
#                                                    value= 1))  
#     ),
#     conditionalPanel('input.opciones == "probabilidad"', ns = ns,
#                      numericInput(inputId = ns("prob_var1"),
#                                   label = "Probabilidad (Un valor entre 0 y 1):",
#                                   min=0,  max=1, step= 0.01, value= 0.25)),
#     conditionalPanel('input.opciones == "porcentaje"', ns = ns,
#                      numericInput(inputId = ns("porcentaje_var1"),
#                                   label = "Porcentaje (Un valor entre 0 y 100):",
#                                   min=0,  max=100, step= 0.01, value= 25)),
#     br(), br(),
#     actionButton(inputId = ns("calcular02"), label = "Calcular!"),
#     br(),br()
#   )
#   
#   
#   
# }
# 
# 
# MainPanel02_t <- function(id){
#   
#   ns <- NS(id)
#   
#   
#   div(textOutput(ns("texto_aviso")),
#       uiOutput(ns("armado01"))
#   )
#   
#   
# }
# 
# 
# Server02_t_Server <- function(input, output, session,
#                               la_distribucion) {
#   
#   # NameSpaceasing for the session
#   ns <- session$ns
#   
#   # los_parametros <- reactive({
#   
#   result <- reactiveVal(0)
#   # result <- reactiveVal() #Esto lo hace NULL al inicio
#   observeEvent(input$calcular01, { result(result() + 1) })
#   observeEvent(input$calcular02, { result(result() + 1) })
#   
#   los_parametros <- eventReactive(result(), {
#     
#     if(is.null(input$info)) return(NULL)
#     
#     media_muestral <- NA
#     varianza_muestral <- NA
#     desvio_muestral <- NA
#     gl <- NA
#     opciones <- NA
#     intervalo <- NA
#     color_variable <- NA
#     decimals <- NA
#     var1 <- NA
#     var2 <- NA
#     var3 <- NA
#     var4 <- NA
#     t1 <- NA
#     t2 <- NA
#     t3 <- NA
#     t4 <- NA
#     probabilidad_externo <- NA
#     porcentaje_externo <- NA
#     
#     color_variable <- input$color_variable
#     media_muestral <- input$media_muestral
#     
#     if(input$info == 1){
#       varianza_muestral <- input$varianza_muestral
#       desvio_muestral <- sqrt(varianza_muestral)
#     } else
#       if(input$info == 2){
#         desvio_muestral <- input$desvio_muestral
#         varianza_muestral <- desvio_muestral^2
#         
#       }
#     
#     gl <- input$gl
#     opciones <- input$opciones
#     intervalo <- input$intervalo
#     decimals <- input$decimals
#     
#     var1 <- input$var1
#     var2 <- input$var2
#     var3 <- input$var3
#     var4 <- input$var4
#     
#     t1 <- input$t_var1
#     t2 <- input$t_var2
#     t3 <- input$t_var3
#     t4 <- input$t_var4
#     
#     probabilidad_externo <- input$prob_var1
#     porcentaje_externo <- input$porcentaje_var1
#     
#     
#     los_parametros <- Hmisc::llist(media_muestral, varianza_muestral, desvio_muestral,
#                                    gl,
#                                    opciones, intervalo, color_variable,
#                                    var1, var2, var3, var4, 
#                                    t1, t2, t3, t4, 
#                                    probabilidad_externo, porcentaje_externo, decimals)
#     
#     return(los_parametros)
#     
#     
#     
#   })
#   
#   RMedic_Info <- reactive({
#     
#     if(is.null(los_parametros())) return(NULL)
#     
#     mis_parametros <- los_parametros()
#     
#     out <-  do.call(Distribucion.t, mis_parametros) # works 
#     out <- out$RMedic
#     
#     return(out)
#   })
#   
#   
#   
#   
#   output$tabla01 <- renderTable(align = "c", {
#     
#     if(is.null(RMedic_Info())) return(NULL)
#     out <- RMedic_Info()$tabla_externa01
#     out <- CharacterALL(out)
#     out
#   })
#   
#   output$tabla02 <- renderTable(align = "c",{
#     
#     if(is.null(RMedic_Info())) return(NULL)
#     out <- RMedic_Info()$tabla_externa03
#     out <- CharacterALL(out)
#     out
#     # tabla_externa02()
#   })
#   
#   output$frase_final01 <- renderUI({
#     
#     if(is.null(RMedic_Info())) return(NULL)
#     out <- RMedic_Info()$frase01
#     HTML(out)
#   })
#   
#   output$frase_final02 <- renderUI({
#     
#     if(is.null(RMedic_Info())) return(NULL)
#     out <- RMedic_Info()$frase02
#     HTML(out)
#   })
#   
#   # Gráfico 1
#   observe(output$distPlot1 <- renderPlot({
#     
#     if(is.null(RMedic_Info())) return(NULL)
#     RMedic_Info()$grafico01
#     
#   }))
#   
#   
#   output$distPlot2 <- renderPlot({
#     
#     if(is.null(RMedic_Info())) return(NULL)
#     RMedic_Info()$grafico02
#     
#     
#   })
#   
#   output$texto_aviso <- renderText({
#     
#     if(is.null(result())) return("Seleccione y de clic en 'Calcular!'") else
#       return(NULL)
#   })
#   
#   
#   output$armado01 <- renderUI({
#     
#     if(is.null(result())) return(NULL)
#     # if(is.null(los_parametros())) return("Seleccione y de clic en 'Calcular!'")
#     # if (la_distribucion() != "001_Normal") return(NULL)
#     
#     div(h2("Distribución de la Variable"),
#         plotOutput(ns("distPlot2")),
#         br(),
#         h2("Distribución t Estándar (t)"),
#         plotOutput(ns("distPlot1")), br(),
#         br(),
#         br(),
#         
#         fluidRow(
#           column(6, 
#                  h2("Parámetros"),
#                  tableOutput(ns("tabla01")),
#                  br(),
#                  h2("Cálculos"),
#                  tableOutput(ns("tabla02"))),
#           column(6, 
#                  h2("Detalles"),
#                  htmlOutput(ns("frase_final01")),
#                  br(),br(),
#                  h2("Frase Explicativa"),
#                  htmlOutput(ns("frase_final02")),
#                  tags$head(tags$style(
#                    paste0("#", ns("frase_final01"), "{color: black;
#                                  font-size: 20px;
#                                  font-style: italic;
#                                  }")
#                  )
#                  ),
#                  tags$head(tags$style(
#                    paste0("#", ns("frase_final02"), "{color: black;
#                                  font-size: 20px;
#                                  font-style: italic;
#                                  }")
#                  )
#                  )
#                  
#           )
#         )
#         , br(),
#         # span(htmlOutput(ns("frase_control_qc")), style="color:red")
#         
#         br()
#         
#     )
#   })
# }
# 
# ################################################################################
# 
# # https://virtual.uptc.edu.co/ova/estadistica/docs/libros/estadistica1/cap03b.html
# 
# SideBar03_chi <- function(id) {
#   ns <- NS(id)
#   
#   
#   
#   div(
#     h3("Distribución Chi Cuadrado"),
#     actionButton(inputId = ns("calcular01"), label = "Calcular!"),br(),
#     br(),
#     br(),
#     fluidRow(
#       column(6,
#              selectInput(inputId = ns("color_variable"), 
#                          label = "Color:",
#                          choices = c("Naranja" = "orange",
#                                      "Rojo" = "red",
#                                      "Verde" = "green",
#                                      "Azul" = "blue", 
#                                      "Amarillo" = "yellow", 
#                                      "Negro" = "black",
#                                      "Celeste" = "skyblue"), 
#                          multiple = FALSE)
#       ),
#       column(6,
#              numericInput(inputId = ns("decimals"),
#                           label = "Decimales", 
#                           min = 0,
#                           step = 1,
#                           value = 2)
#       )
#     ),
#     br(),
#     radioButtons(inputId = ns("info"), label = "Parámetros conocidos", 
#                  choices = c("Media y Varianza" = 1, "Media y Desvío" = 2)
#     ), br(),
#     numericInput(inputId = ns("media_poblacional"),
#                  label = "Media Poblacional:",
#                  step= 0.01,
#                  value= 106),
#     conditionalPanel(condition = "input.info == 1", ns = ns, 
#                      numericInput(inputId = ns("varianza_poblacional"),
#                                   label = "Varianza Poblacional: ",
#                                   min =0,
#                                   step = 0.01,  
#                                   value = 64)),
#     conditionalPanel(condition = "input.info == 2", ns = ns, 
#                      numericInput(inputId = ns("desvio_poblacional"),
#                                   label = "Desvío Poblacional: ",
#                                   min = 0,
#                                   step = 0.01,  
#                                   value = 8)),
#     br(),
#     radioButtons(inputId = ns("opciones"), 
#                  label = "Datos conocidos:",
#                  choices =  c("Valor de la variable" = "original",
#                               "Valor Z" = "valor_z",
#                               "Probabilidad" = "probabilidad",
#                               "Porcentaje" = "porcentaje")),
#     br(),
#     radioButtons(inputId = ns("intervalo"), 
#                  label = "Intervalo de cálculo:",
#                  choices = c("Menores que..." = "menor",
#                              "Mayores que..." = "mayor",
#                              "Entre..." = "entre")
#                  
#     ),
#     br(),
#     
#     conditionalPanel('input.opciones == "original"', ns = ns,
#                      conditionalPanel('input.intervalo == "menor"', ns = ns,
#                                       numericInput(inputId = ns("var1"), 
#                                                    label = "Valor de la Variable Original:",
#                                                    step= 0.01, 
#                                                    value= 90)),
#                      
#                      conditionalPanel('input.intervalo == "mayor"', ns = ns,
#                                       numericInput(inputId = ns("var2"),
#                                                    label = "Valor de la Variable Original:",
#                                                    step= 0.01, 
#                                                    value= 110)),
#                      
#                      conditionalPanel('input.intervalo == "entre"', ns = ns,
#                                       numericInput(inputId = ns("var3"), 
#                                                    label = "Valor Izquierdo de la Variable Original:",
#                                                    step= 0.01, 
#                                                    value= 90),
#                                       numericInput(inputId = ns("var4"),
#                                                    label = "Valor Derecho de la Variable Original:",
#                                                    step= 0.01, 
#                                                    value= 110))),  
#     
#     conditionalPanel('input.opciones == "valor_z"', ns = ns,
#                      conditionalPanel('input.intervalo == "menor"', ns = ns,
#                                       numericInput(inputId = ns("z_var1"),
#                                                    label = "Valor Z:", 
#                                                    step= 0.01, 
#                                                    value= -1.96)),
#                      
#                      conditionalPanel('input.intervalo == "mayor"', ns = ns,
#                                       numericInput(inputId = ns("z_var2"),
#                                                    label = "Valor Z:", 
#                                                    step= 0.01, 
#                                                    value= 1)),
#                      
#                      conditionalPanel('input.intervalo == "entre"', ns = ns,
#                                       numericInput(inputId = ns("z_var3"),
#                                                    label = "Valor Z Izquierdo :", 
#                                                    step= 0.01, value= -1.96),
#                                       numericInput(inputId = ns("z_var4"),
#                                                    label = "Valor Z Derecho:", 
#                                                    step= 0.01, 
#                                                    value= 1))  
#     ),
#     conditionalPanel('input.opciones == "probabilidad"', ns = ns,
#                      numericInput(inputId = ns("prob_var1"),
#                                   label = "Probabilidad (Un valor entre 0 y 1):",
#                                   min=0,  max=1, step= 0.01, value= 0.25)),
#     conditionalPanel('input.opciones == "porcentaje"', ns = ns,
#                      numericInput(inputId = ns("porcentaje_var1"),
#                                   label = "Porcentaje (Un valor entre 0 y 100):",
#                                   min=0,  max=100, step= 0.01, value= 25)),
#     br(), br(),
#     actionButton(inputId = ns("calcular02"), label = "Calcular!"),
#     br(),br()
#   )
#   
#   
#   
# }
# 
# 
# MainPanel03_chi <- function(id){
#   
#   ns <- NS(id)
#   
#   
#   div(textOutput(ns("texto_aviso")),
#       uiOutput(ns("armado01"))
#   )
#   
#   
# }
# 
# 
# Server03_chi_Server <- function(input, output, session,
#                                    la_distribucion) {
#   
#   # NameSpaceasing for the session
#   ns <- session$ns
#   
#   # los_parametros <- reactive({
#   
#   result <- reactiveVal(0)
#   # result <- reactiveVal() #Esto lo hace NULL al inicio
#   observeEvent(input$calcular01, { result(result() + 1) })
#   observeEvent(input$calcular02, { result(result() + 1) })
#   
#   los_parametros <- eventReactive(result(), {
#     
#     if(is.null(input$info)) return(NULL)
#     
#     media_poblacional <- NA
#     varianza_poblacional <- NA
#     desvio_poblacional <- NA
#     opciones <- NA
#     intervalo <- NA
#     color_variable <- NA
#     decimals <- NA
#     var1 <- NA
#     var2 <- NA
#     var3 <- NA
#     var4 <- NA
#     z1 <- NA
#     z2 <- NA
#     z3 <- NA
#     z4 <- NA
#     probabilidad_externo <- NA
#     porcentaje_externo <- NA
#     
#     color_variable <- input$color_variable
#     media_poblacional <- input$media_poblacional
#     
#     if(input$info == 1){
#       varianza_poblacional <- input$varianza_poblacional
#       desvio_poblacional <- sqrt(varianza_poblacional)
#     } else
#       if(input$info == 2){
#         desvio_poblacional <- input$desvio_poblacional
#         varianza_poblacional <- desvio_poblacional^2
#         
#       }
#     
#     opciones <- input$opciones
#     intervalo <- input$intervalo
#     decimals <- input$decimals
#     
#     var1 <- input$var1
#     var2 <- input$var2
#     var3 <- input$var3
#     var4 <- input$var4
#     
#     z1 <- input$z_var1
#     z2 <- input$z_var2
#     z3 <- input$z_var3
#     z4 <- input$z_var4
#     
#     probabilidad_externo <- input$prob_var1
#     porcentaje_externo <- input$porcentaje_var1
#     
#     
#     los_parametros <- Hmisc::llist(media_poblacional, varianza_poblacional, desvio_poblacional,
#                                    opciones, intervalo, color_variable,
#                                    var1, var2, var3, var4, 
#                                    z1, z2, z3, z4, 
#                                    probabilidad_externo, porcentaje_externo, decimals)
#     
#     return(los_parametros)
#     
#     
#     
#   })
#   
#   RMedic_Info <- reactive({
#     
#     if(is.null(los_parametros())) return(NULL)
#     
#     mis_parametros <- los_parametros()
#     
#     out <-  do.call(Distribucion.Normal, mis_parametros) # works 
#     out <- out$RMedic
#     
#     return(out)
#   })
#   
#   
#   
#   
#   output$tabla01 <- renderTable(align = "c", {
#     
#     if(is.null(RMedic_Info())) return(NULL)
#     out <- RMedic_Info()$tabla_externa01
#     out <- CharacterALL(out)
#     out
#   })
#   
#   output$tabla02 <- renderTable(align = "c",{
#     
#     if(is.null(RMedic_Info())) return(NULL)
#     out <- RMedic_Info()$tabla_externa02
#     out <- CharacterALL(out)
#     out
#     # tabla_externa02()
#   })
#   
#   output$frase_final01 <- renderUI({
#     
#     if(is.null(RMedic_Info())) return(NULL)
#     out <- RMedic_Info()$frase01
#     HTML(out)
#   })
#   
#   output$frase_final02 <- renderUI({
#     
#     if(is.null(RMedic_Info())) return(NULL)
#     out <- RMedic_Info()$frase02
#     HTML(out)
#   })
#   
#   # Gráfico 1
#   observe(output$distPlot1 <- renderPlot({
#     
#     if(is.null(RMedic_Info())) return(NULL)
#     RMedic_Info()$grafico01
#     
#   }))
#   
#   
#   output$distPlot2 <- renderPlot({
#     
#     if(is.null(RMedic_Info())) return(NULL)
#     RMedic_Info()$grafico02
#     
#     
#   })
#   
#   output$texto_aviso <- renderText({
#     
#     if(is.null(result())) return("Seleccione y de clic en 'Calcular!'") else
#       return(NULL)
#   })
#   
#   
#   output$armado01 <- renderUI({
#     
#     if(is.null(result())) return(NULL)
#     # if(is.null(los_parametros())) return("Seleccione y de clic en 'Calcular!'")
#     # if (la_distribucion() != "001_Normal") return(NULL)
#     
#     div(h2("Distribución de la Variable"),
#         plotOutput(ns("distPlot2")),
#         br(),
#         h2("Distribución Chi Cuadrado"),
#         plotOutput(ns("distPlot1")), br(),
#         br(),
#         br(),
#         
#         fluidRow(
#           column(6, 
#                  h2("Parámetros"),
#                  tableOutput(ns("tabla01")),
#                  br(),
#                  h2("Cálculos"),
#                  tableOutput(ns("tabla02"))),
#           column(6, 
#                  h2("Detalles"),
#                  htmlOutput(ns("frase_final01")),
#                  br(),br(),
#                  h2("Frase Explicativa"),
#                  htmlOutput(ns("frase_final02")),
#                  tags$head(tags$style(
#                    paste0("#", ns("frase_final01"), "{color: black;
#                                  font-size: 20px;
#                                  font-style: italic;
#                                  }")
#                  )
#                  ),
#                  tags$head(tags$style(
#                    paste0("#", ns("frase_final02"), "{color: black;
#                                  font-size: 20px;
#                                  font-style: italic;
#                                  }")
#                  )
#                  )
#                  
#           )
#         )
#         , br(),
#         # span(htmlOutput(ns("frase_control_qc")), style="color:red")
#         
#         br()
#         
#     )
#   })
# }
# 
# 
# 
# 
# 
# 
# 
# ################################################################################
# 
# 
# SideBar04_f <- function(id) {
#   ns <- NS(id)
#   
#   
#   
#   div(
#     h3("Distribución F de Fisher"),
#     actionButton(inputId = ns("calcular01"), label = "Calcular!"),br(),
#     br(),
#     br(),
#     fluidRow(
#       column(6,
#              selectInput(inputId = ns("color_variable"), 
#                          label = "Color:",
#                          choices = c("Naranja" = "orange",
#                                      "Rojo" = "red",
#                                      "Verde" = "green",
#                                      "Azul" = "blue", 
#                                      "Amarillo" = "yellow", 
#                                      "Negro" = "black",
#                                      "Celeste" = "skyblue"), 
#                          multiple = FALSE)
#       ),
#       column(6,
#              numericInput(inputId = ns("decimals"),
#                           label = "Decimales", 
#                           min = 0,
#                           step = 1,
#                           value = 2)
#       )
#     ),
#     br(),
#     radioButtons(inputId = ns("info"), label = "Parámetros conocidos", 
#                  choices = c("Media y Varianza" = 1, "Media y Desvío" = 2)
#     ), br(),
#     numericInput(inputId = ns("media_poblacional"),
#                  label = "Media Poblacional:",
#                  step= 0.01,
#                  value= 106),
#     conditionalPanel(condition = "input.info == 1", ns = ns, 
#                      numericInput(inputId = ns("varianza_poblacional"),
#                                   label = "Varianza Poblacional: ",
#                                   min =0,
#                                   step = 0.01,  
#                                   value = 64)),
#     conditionalPanel(condition = "input.info == 2", ns = ns, 
#                      numericInput(inputId = ns("desvio_poblacional"),
#                                   label = "Desvío Poblacional: ",
#                                   min = 0,
#                                   step = 0.01,  
#                                   value = 8)),
#     br(),
#     radioButtons(inputId = ns("opciones"), 
#                  label = "Datos conocidos:",
#                  choices =  c("Valor de la variable" = "original",
#                               "Valor Z" = "valor_z",
#                               "Probabilidad" = "probabilidad",
#                               "Porcentaje" = "porcentaje")),
#     br(),
#     radioButtons(inputId = ns("intervalo"), 
#                  label = "Intervalo de cálculo:",
#                  choices = c("Menores que..." = "menor",
#                              "Mayores que..." = "mayor",
#                              "Entre..." = "entre")
#                  
#     ),
#     br(),
#     
#     conditionalPanel('input.opciones == "original"', ns = ns,
#                      conditionalPanel('input.intervalo == "menor"', ns = ns,
#                                       numericInput(inputId = ns("var1"), 
#                                                    label = "Valor de la Variable Original:",
#                                                    step= 0.01, 
#                                                    value= 90)),
#                      
#                      conditionalPanel('input.intervalo == "mayor"', ns = ns,
#                                       numericInput(inputId = ns("var2"),
#                                                    label = "Valor de la Variable Original:",
#                                                    step= 0.01, 
#                                                    value= 110)),
#                      
#                      conditionalPanel('input.intervalo == "entre"', ns = ns,
#                                       numericInput(inputId = ns("var3"), 
#                                                    label = "Valor Izquierdo de la Variable Original:",
#                                                    step= 0.01, 
#                                                    value= 90),
#                                       numericInput(inputId = ns("var4"),
#                                                    label = "Valor Derecho de la Variable Original:",
#                                                    step= 0.01, 
#                                                    value= 110))),  
#     
#     conditionalPanel('input.opciones == "valor_z"', ns = ns,
#                      conditionalPanel('input.intervalo == "menor"', ns = ns,
#                                       numericInput(inputId = ns("z_var1"),
#                                                    label = "Valor Z:", 
#                                                    step= 0.01, 
#                                                    value= -1.96)),
#                      
#                      conditionalPanel('input.intervalo == "mayor"', ns = ns,
#                                       numericInput(inputId = ns("z_var2"),
#                                                    label = "Valor Z:", 
#                                                    step= 0.01, 
#                                                    value= 1)),
#                      
#                      conditionalPanel('input.intervalo == "entre"', ns = ns,
#                                       numericInput(inputId = ns("z_var3"),
#                                                    label = "Valor Z Izquierdo :", 
#                                                    step= 0.01, value= -1.96),
#                                       numericInput(inputId = ns("z_var4"),
#                                                    label = "Valor Z Derecho:", 
#                                                    step= 0.01, 
#                                                    value= 1))  
#     ),
#     conditionalPanel('input.opciones == "probabilidad"', ns = ns,
#                      numericInput(inputId = ns("prob_var1"),
#                                   label = "Probabilidad (Un valor entre 0 y 1):",
#                                   min=0,  max=1, step= 0.01, value= 0.25)),
#     conditionalPanel('input.opciones == "porcentaje"', ns = ns,
#                      numericInput(inputId = ns("porcentaje_var1"),
#                                   label = "Porcentaje (Un valor entre 0 y 100):",
#                                   min=0,  max=100, step= 0.01, value= 25)),
#     br(), br(),
#     actionButton(inputId = ns("calcular02"), label = "Calcular!"),
#     br(),br()
#   )
#   
#   
#   
# }
# 
# 
# MainPanel04_f <- function(id){
#   
#   ns <- NS(id)
#   
#   
#   div(textOutput(ns("texto_aviso")),
#       uiOutput(ns("armado01"))
#   )
#   
#   
# }
# 
# 
# Server04_f_Server <- function(input, output, session,
#                                 la_distribucion) {
#   
#   # NameSpaceasing for the session
#   ns <- session$ns
#   
#   # los_parametros <- reactive({
#   
#   result <- reactiveVal(0)
#   # result <- reactiveVal() #Esto lo hace NULL al inicio
#   observeEvent(input$calcular01, { result(result() + 1) })
#   observeEvent(input$calcular02, { result(result() + 1) })
#   
#   los_parametros <- eventReactive(result(), {
#     
#     if(is.null(input$info)) return(NULL)
#     
#     media_poblacional <- NA
#     varianza_poblacional <- NA
#     desvio_poblacional <- NA
#     opciones <- NA
#     intervalo <- NA
#     color_variable <- NA
#     decimals <- NA
#     var1 <- NA
#     var2 <- NA
#     var3 <- NA
#     var4 <- NA
#     z1 <- NA
#     z2 <- NA
#     z3 <- NA
#     z4 <- NA
#     probabilidad_externo <- NA
#     porcentaje_externo <- NA
#     
#     color_variable <- input$color_variable
#     media_poblacional <- input$media_poblacional
#     
#     if(input$info == 1){
#       varianza_poblacional <- input$varianza_poblacional
#       desvio_poblacional <- sqrt(varianza_poblacional)
#     } else
#       if(input$info == 2){
#         desvio_poblacional <- input$desvio_poblacional
#         varianza_poblacional <- desvio_poblacional^2
#         
#       }
#     
#     opciones <- input$opciones
#     intervalo <- input$intervalo
#     decimals <- input$decimals
#     
#     var1 <- input$var1
#     var2 <- input$var2
#     var3 <- input$var3
#     var4 <- input$var4
#     
#     z1 <- input$z_var1
#     z2 <- input$z_var2
#     z3 <- input$z_var3
#     z4 <- input$z_var4
#     
#     probabilidad_externo <- input$prob_var1
#     porcentaje_externo <- input$porcentaje_var1
#     
#     
#     los_parametros <- Hmisc::llist(media_poblacional, varianza_poblacional, desvio_poblacional,
#                                    opciones, intervalo, color_variable,
#                                    var1, var2, var3, var4, 
#                                    z1, z2, z3, z4, 
#                                    probabilidad_externo, porcentaje_externo, decimals)
#     
#     return(los_parametros)
#     
#     
#     
#   })
#   
#   RMedic_Info <- reactive({
#     
#     if(is.null(los_parametros())) return(NULL)
#     
#     mis_parametros <- los_parametros()
#     
#     out <-  do.call(Distribucion.Normal, mis_parametros) # works 
#     out <- out$RMedic
#     
#     return(out)
#   })
#   
#   
#   
#   
#   output$tabla01 <- renderTable(align = "c", {
#     
#     if(is.null(RMedic_Info())) return(NULL)
#     out <- RMedic_Info()$tabla_externa01
#     out <- CharacterALL(out)
#     out
#   })
#   
#   output$tabla02 <- renderTable(align = "c",{
#     
#     if(is.null(RMedic_Info())) return(NULL)
#     out <- RMedic_Info()$tabla_externa02
#     out <- CharacterALL(out)
#     out
#     # tabla_externa02()
#   })
#   
#   output$frase_final01 <- renderUI({
#     
#     if(is.null(RMedic_Info())) return(NULL)
#     out <- RMedic_Info()$frase01
#     HTML(out)
#   })
#   
#   output$frase_final02 <- renderUI({
#     
#     if(is.null(RMedic_Info())) return(NULL)
#     out <- RMedic_Info()$frase02
#     HTML(out)
#   })
#   
#   # Gráfico 1
#   observe(output$distPlot1 <- renderPlot({
#     
#     if(is.null(RMedic_Info())) return(NULL)
#     RMedic_Info()$grafico01
#     
#   }))
#   
#   
#   output$distPlot2 <- renderPlot({
#     
#     if(is.null(RMedic_Info())) return(NULL)
#     RMedic_Info()$grafico02
#     
#     
#   })
#   
#   output$texto_aviso <- renderText({
#     
#     if(is.null(result())) return("Seleccione y de clic en 'Calcular!'") else
#       return(NULL)
#   })
#   
#   
#   output$armado01 <- renderUI({
#     
#     if(is.null(result())) return(NULL)
#     # if(is.null(los_parametros())) return("Seleccione y de clic en 'Calcular!'")
#     # if (la_distribucion() != "001_Normal") return(NULL)
#     
#     div(h2("Distribución de la Variable"),
#         plotOutput(ns("distPlot2")),
#         br(),
#         h2("Distribución F"),
#         plotOutput(ns("distPlot1")), br(),
#         br(),
#         br(),
#         
#         fluidRow(
#           column(6, 
#                  h2("Parámetros"),
#                  tableOutput(ns("tabla01")),
#                  br(),
#                  h2("Cálculos"),
#                  tableOutput(ns("tabla02"))),
#           column(6, 
#                  h2("Detalles"),
#                  htmlOutput(ns("frase_final01")),
#                  br(),br(),
#                  h2("Frase Explicativa"),
#                  htmlOutput(ns("frase_final02")),
#                  tags$head(tags$style(
#                    paste0("#", ns("frase_final01"), "{color: black;
#                                  font-size: 20px;
#                                  font-style: italic;
#                                  }")
#                  )
#                  ),
#                  tags$head(tags$style(
#                    paste0("#", ns("frase_final02"), "{color: black;
#                                  font-size: 20px;
#                                  font-style: italic;
#                                  }")
#                  )
#                  )
#                  
#           )
#         )
#         , br(),
#         # span(htmlOutput(ns("frase_control_qc")), style="color:red")
#         
#         br()
#         
#     )
#   })
# }




