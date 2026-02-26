## Segmento del UI
Menu_DistribucionGeneral02_UI <- function(id) {
  ns <- NS(id)
  
  
  
  uiOutput(ns("outMe_distribucion02_general"))
  
  
}



## Segmento del server
Menu_DistribucionGeneral02_SERVER <- function(input, output, session,
                                              carpeta_distribuciones) {
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  output$outMe_distribucion02_general <- renderUI({
    
    las_distribuciones <- list.files(carpeta_distribuciones)
    
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
SideBarDistribucionElegida02_UI <- function(id) {
  ns <- NS(id)
  

  uiOutput(ns("outMe_sidebar02_elegida"))
  
  
}

## Segmento del UI
MainPanelDistribucionElegida02_UI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("outMe_mainmenu02_elegida"))
  
  
}

## Segmento del server
SideBarDistribucionElegida02_SERVER <- function(input, output, session,
                                              la_distribucion) {
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  output$outMe_sidebar02_elegida <- renderUI({
    if(is.null(la_distribucion())) return(NULL)
    
    if (la_distribucion() == "001_Normal") return(SideBar01_Normal02("aver02A")) else
      if (la_distribucion() == "002_t_de_Student") return(SideBar02_t02("aver02B")) else
        if (la_distribucion() == "003_Chi_Cuadrado") return(SideBar03_chi02("aver02C")) else
          if (la_distribucion() == "004_F_de_Fisher") return(SideBar04_f02("aver02D")) else
            return(NULL)
    
    
  })
  
  

}


## Segmento del server
MainPanelDistribucionElegida02_SERVER <- function(input, output, session,
                                                  la_distribucion) {
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  output$outMe_mainmenu02_elegida <- renderUI({
    if(is.null(la_distribucion())) return(NULL)
    
    if (la_distribucion() == "001_Normal") return(MainPanel01_Normal02("aver02A")) else
      if (la_distribucion() == "002_t_de_Student") return(MainPanel02_t02("aver02B")) else
        if (la_distribucion() == "003_Chi_Cuadrado") return(MainPanel03_chi02("aver02C")) else
          if (la_distribucion() == "004_F_de_Fisher") return(MainPanel04_f02("aver02D")) else
            return(NULL)
    
    
  })
  
  
  
}

ServerDistribucionElegida02_SERVER <- function(input, output, session,
                                             la_distribucion) {
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  
  # POR AHORA QUEDA VACIO!
  
  
}


########################################################################################

# Normal 
# # SidePanelNormal
## Segmento del UI

SideBar01_Normal02 <- function(id) {
  ns <- NS(id)
  
  
  
  div(
    withMathJax(),
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
    h2("Parámetros Poblacionales"),
    h3("Media Poblacional"),
    h2("$$\\mu = 0$$"),br(),
    h3("Varianza Poblacional"),
    h2("$$\\sigma^{2} = 1$$"),
    br(),
    radioButtons(inputId = ns("opciones"), 
                 label = "Datos conocidos:",
                 choices =  c("Valor Z" = "valor_z",
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


MainPanel01_Normal02 <- function(id){
  
  ns <- NS(id)
  
  
  div(textOutput(ns("texto_aviso")),
      uiOutput(ns("armado01"))
  )
  
  
}


Server01_Normal_Server02 <- function(input, output, session,
                                   la_distribucion) {
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  # los_parametros <- reactive({
  
  result <- reactiveVal(0)
  # result <- reactiveVal() #Esto lo hace NULL al inicio
  observeEvent(input$calcular01, { result(result() + 1) })
  observeEvent(input$calcular02, { result(result() + 1) })
  
  los_parametros <- eventReactive(result(), {
    

    media_poblacional <- 0
    varianza_poblacional <- 1
    desvio_poblacional <- 1
    # media_poblacional <- NA
    # varianza_poblacional <- NA
    # desvio_poblacional <- NA
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
    out <- RMedic_Info()$tabla_externa03
    out <- CharacterALL(out)
    # out <- out[,-1]
    out
    # tabla_externa02()
  })
  
  output$frase_final01 <- renderUI({
    
    if(is.null(RMedic_Info())) return(NULL)
    out <- RMedic_Info()$frase03
    HTML(out)
  })
  
  output$frase_final02 <- renderUI({
    
    if(is.null(RMedic_Info())) return(NULL)
    out <- RMedic_Info()$frase04
    HTML(out)
  })
  
  # # Gráfico 1
  # observe(output$distPlot1 <- renderPlot({
  #   
  #   if(is.null(RMedic_Info())) return(NULL)
  #   RMedic_Info()$grafico01
  #   
  # }))
  # 
  # 
  output$distPlot1 <- renderPlot({
    
    if(is.null(RMedic_Info())) return(NULL)
    RMedic_Info()$grafico01
    
    
  })
  
  output$texto_aviso <- renderText({
    
    if(is.null(result())) return("Seleccione y de clic en 'Calcular!'") else
      return(NULL)
  })
  
  
  output$armado01 <- renderUI({
    
    if(is.null(result())) return(NULL)
    # if(is.null(los_parametros())) return("Seleccione y de clic en 'Calcular!'")
    # if (la_distribucion() != "001_Normal") return(NULL)
    
    div(h2("Distribución Normal Estándar (Z)"),
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



#################################################################################


# t
SideBar02_t02 <- function(id) {
  ns <- NS(id)
  
  
  
  div(
    withMathJax(),
    h3("Distribución t de Student"),
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
   
    # h2("Estimadores Muestrales"),
    # h3("Media Muestral"),
    # h2("$$\\bar{x} = 0$$"),br(),
    # h3("Varianza Muestral"),
    # h2("$$S^{2} = 1$$"),
    # br(),
    
    br(),
    radioButtons(inputId = ns("opciones"), 
                 label = "Datos conocidos:",
                 choices =  c("Valor t y Grados de Libertad" = "valor_t",
                              "Probabilidad y Grados de Libertad" = "probabilidad",
                              "Porcentaje y Grados de Libertad" = "porcentaje")),
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
    
    conditionalPanel('input.opciones == "valor_t"', ns = ns,
                     conditionalPanel('input.intervalo == "menor"', ns = ns,
                                      numericInput(inputId = ns("t_var1"),
                                                   label = "Valor t:", 
                                                   step= 0.01, 
                                                   value= -1.96)),
                     
                     conditionalPanel('input.intervalo == "mayor"', ns = ns,
                                      numericInput(inputId = ns("t_var2"),
                                                   label = "Valor t:", 
                                                   step= 0.01, 
                                                   value= 1)),
                     
                     conditionalPanel('input.intervalo == "entre"', ns = ns,
                                      numericInput(inputId = ns("t_var3"),
                                                   label = "Valor t Izquierdo :", 
                                                   step= 0.01, value= -1.96),
                                      numericInput(inputId = ns("t_var4"),
                                                   label = "Valor t Derecho:", 
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
    numericInput(inputId = ns("gl"),
                 label = "Grados de Libertad (n-1):",
                 step= 1,
                 value= 19),
    br(), br(),
    actionButton(inputId = ns("calcular02"), label = "Calcular!"),
    br(),br()
  )
  
  
  
}


MainPanel02_t02 <- function(id){
  
  ns <- NS(id)
  
  
  div(textOutput(ns("texto_aviso")),
      uiOutput(ns("armado01"))
  )
  
  
}


Server02_t_Server02 <- function(input, output, session,
                              la_distribucion) {
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  # los_parametros <- reactive({
  
  result <- reactiveVal(0)
  # result <- reactiveVal() #Esto lo hace NULL al inicio
  observeEvent(input$calcular01, { result(result() + 1) })
  observeEvent(input$calcular02, { result(result() + 1) })
  
  los_parametros <- eventReactive(result(), {
    

    # media_muestral <- NA
    # varianza_muestral <- NA
    # desvio_muestral <- NA
    media_muestral <- 0
    varianza_muestral <- 1
    desvio_muestral <- 1
    gl <- NA
    opciones <- NA
    intervalo <- NA
    color_variable <- NA
    decimals <- NA
    var1 <- NA
    var2 <- NA
    var3 <- NA
    var4 <- NA
    t1 <- NA
    t2 <- NA
    t3 <- NA
    t4 <- NA
    probabilidad_externo <- NA
    porcentaje_externo <- NA
    
    color_variable <- input$color_variable

  
    gl <- input$gl
    opciones <- input$opciones
    intervalo <- input$intervalo
    decimals <- input$decimals
    
    var1 <- input$var1
    var2 <- input$var2
    var3 <- input$var3
    var4 <- input$var4
    
    t1 <- input$t_var1
    t2 <- input$t_var2
    t3 <- input$t_var3
    t4 <- input$t_var4
    
    probabilidad_externo <- input$prob_var1
    porcentaje_externo <- input$porcentaje_var1
    
    
    los_parametros <- Hmisc::llist(media_muestral, varianza_muestral, desvio_muestral,
                                   gl,
                                   opciones, intervalo, color_variable,
                                   var1, var2, var3, var4, 
                                   t1, t2, t3, t4, 
                                   probabilidad_externo, porcentaje_externo, decimals)
    
    return(los_parametros)
    
    
    
  })
  
  RMedic_Info <- reactive({
    
    if(is.null(los_parametros())) return(NULL)
    
    mis_parametros <- los_parametros()
    
    out <-  do.call(Distribucion.t, mis_parametros) # works 
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
    out <- RMedic_Info()$tabla_externa03
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
  
  
  
  output$texto_aviso <- renderText({
    
    if(is.null(result())) return("Seleccione y de clic en 'Calcular!'") else
      return(NULL)
  })
  
  
  output$armado01 <- renderUI({
    
    if(is.null(result())) return(NULL)
    # if(is.null(los_parametros())) return("Seleccione y de clic en 'Calcular!'")
    # if (la_distribucion() != "001_Normal") return(NULL)
    
    div(h2("Distribución t Estándar (t)"),
        plotOutput(ns("distPlot1")), br(),
        br(),
        br(),
        
        fluidRow(
          column(6, 
                 # h2("Parámetros"),
                 # tableOutput(ns("tabla01")),
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


#################################################################################

# Chi Cuadrado
SideBar03_chi02 <- function(id) {
  ns <- NS(id)
  
  
  
  div(
    withMathJax(),
    h3("Distribución Chi Cuadrado"),
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
  
    br(),
    radioButtons(inputId = ns("opciones"), 
                 label = "Datos conocidos:",
                 choices =  c("Valor Chi y Grados de Libertad" = "valor_chi",
                              "Probabilidad y Grados de Libertad" = "probabilidad",
                              "Porcentaje y Grados de Libertad" = "porcentaje")),
    br(),
    radioButtons(inputId = ns("intervalo"), 
                 label = "Intervalo de cálculo:",
                 choices = c("Menores que..." = "menor",
                             "Mayores que..." = "mayor",
                             "Entre..." = "entre")
                 
    ),
    br(),
    
   
  
    conditionalPanel('input.opciones == "valor_chi"', ns = ns,
                     conditionalPanel('input.intervalo == "menor"', ns = ns,
                                      numericInput(inputId = ns("chi_var1"),
                                                   label = "Valor Chi:", 
                                                   step= 0.01,
                                                   min = 0,
                                                   value= 5)),
                     
                     conditionalPanel('input.intervalo == "mayor"', ns = ns,
                                      numericInput(inputId = ns("chi_var2"),
                                                   label = "Valor Chi:", 
                                                   step= 0.01,
                                                   min = 0,
                                                   value= 7)),
                     
                     conditionalPanel('input.intervalo == "entre"', ns = ns,
                                      numericInput(inputId = ns("chi_var3"),
                                                   label = "Valor Chi Izquierdo :", 
                                                   step= 0.01,
                                                   min = 0,
                                                   value= 5),
                                      numericInput(inputId = ns("chi_var4"),
                                                   label = "Valor Chi Derecho:", 
                                                   step= 0.01,
                                                   min = 0,
                                                   value= 6))  
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
    numericInput(inputId = ns("gl"),
                 label = "Grados de Libertad (n-2)", 
                 step = 1,
                 min = 1,
                 value = 4),
    br(),
    actionButton(inputId = ns("calcular02"), label = "Calcular!"),
    br(),br()
  )
  
  
  
}


MainPanel03_chi02 <- function(id){
  
  ns <- NS(id)
  
  
  div(textOutput(ns("texto_aviso")),
      uiOutput(ns("armado01"))
  )
  
  
}


Server03_chi_Server02 <- function(input, output, session,
                                     la_distribucion) {
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  # los_parametros <- reactive({
  
  result <- reactiveVal(0)
  # result <- reactiveVal() #Esto lo hace NULL al inicio
  observeEvent(input$calcular01, { result(result() + 1) })
  observeEvent(input$calcular02, { result(result() + 1) })
  
  los_parametros <- eventReactive(result(), {
    
    

    gl <- NA

    opciones <- NA
    intervalo <- NA
    color_variable <- NA
    decimals <- NA

    chi1 <- NA
    chi2 <- NA
    chi3 <- NA
    chi4 <- NA
    probabilidad_externo <- NA
    porcentaje_externo <- NA
    
    ########################################################
    
    gl <- input$gl
    varianza_poblacional <- input$varianza_poblacional
    color_variable <- input$color_variable
    
    
    opciones <- input$opciones
    intervalo <- input$intervalo
    decimals <- input$decimals
    
   
    chi1 <- input$chi_var1
    chi2 <- input$chi_var2
    chi3 <- input$chi_var3
    chi4 <- input$chi_var4
    
    probabilidad_externo <- input$prob_var1
    porcentaje_externo <- input$porcentaje_var1
    
    
    los_parametros <- Hmisc::llist(gl, 
                                   opciones, intervalo, color_variable,
                                   chi1, chi2, chi3, chi4, 
                                   probabilidad_externo, porcentaje_externo, decimals)
    
    return(los_parametros)
    
    
    
  })
  
  RMedic_Info <- reactive({
    
    if(is.null(los_parametros())) return(NULL)
    
    mis_parametros <- los_parametros()
    
    out <-  do.call(Tabla.Chi, mis_parametros) # works
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
    out <- RMedic_Info()$tabla_externa03
    out <- CharacterALL(out)
    # out <- out[,-1]
    out
    # tabla_externa02()
  })
  
  output$frase_final01 <- renderUI({
    
    if(is.null(RMedic_Info())) return(NULL)
    out <- RMedic_Info()$frase01
    out <- paste0(out, "<br/>", RMedic_Info()$frase02)
    HTML(out)
  })
  
  output$frase_final02 <- renderUI({
    
    if(is.null(RMedic_Info())) return(NULL)
    out <- RMedic_Info()$frase03
    out <- paste0(out, "<br/>", RMedic_Info()$frase04)
    HTML(out)
  })
  
  # # Gráfico 1
  # observe(output$distPlot1 <- renderPlot({
  #   
  #   if(is.null(RMedic_Info())) return(NULL)
  #   RMedic_Info()$grafico01
  #   
  # }))
  # 
  # 
  output$distPlot1 <- renderPlot({
    
    if(is.null(RMedic_Info())) return(NULL)
    RMedic_Info()$grafico01
    
    
  })
  
  output$texto_aviso <- renderText({
    
    if(is.null(result())) return("Seleccione y de clic en 'Calcular!'") else
      return(NULL)
  })
  
  
  output$armado01 <- renderUI({
    
    if(is.null(result())) return(NULL)
    # if(is.null(los_parametros())) return("Seleccione y de clic en 'Calcular!'")
    # if (la_distribucion() != "001_Normal") return(NULL)
    
    div(h2("Distribución Chi Cuadrado"),
        plotOutput(ns("distPlot1")), br(),
        br(),
        br(),
        
        fluidRow(
          column(6, 
                # h2("Parámetros"),
                # tableOutput(ns("tabla01")),
                # br(),
                 h2("Parámetros y Cálculos"),
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


#################################################################################

# Chi Cuadrado
SideBar04_f02 <- function(id) {
  ns <- NS(id)
  
  
  
  div(
    withMathJax(),
    h3("Distribución F"),
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
    
    br(),
    radioButtons(inputId = ns("opciones"), 
                 label = "Datos conocidos:",
                 choices =  c("Valor F y Grados de Libertad" = "valor_f",
                              "Probabilidad y Grados de Libertad" = "probabilidad",
                              "Porcentaje y Grados de Libertad" = "porcentaje")),
    br(),
    radioButtons(inputId = ns("intervalo"), 
                 label = "Intervalo de cálculo:",
                 choices = c("Menores que..." = "menor",
                             "Mayores que..." = "mayor",
                             "Entre..." = "entre")
                 
    ),
    br(),
    
    
    
    conditionalPanel('input.opciones == "valor_f"', ns = ns,
                     conditionalPanel('input.intervalo == "menor"', ns = ns,
                                      numericInput(inputId = ns("f_var1"),
                                                   label = "Valor F:", 
                                                   step= 0.01,
                                                   min = 0,
                                                   value= 5)),
                     
                     conditionalPanel('input.intervalo == "mayor"', ns = ns,
                                      numericInput(inputId = ns("f_var2"),
                                                   label = "Valor F:", 
                                                   step= 0.01,
                                                   min = 0,
                                                   value= 7)),
                     
                     conditionalPanel('input.intervalo == "entre"', ns = ns,
                                      numericInput(inputId = ns("f_var3"),
                                                   label = "Valor F Izquierdo:", 
                                                   step= 0.01,
                                                   min = 0,
                                                   value= 5),
                                      numericInput(inputId = ns("f_var4"),
                                                   label = "Valor F Derecho:", 
                                                   step= 0.01,
                                                   min = 0,
                                                   value= 6))  
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
    numericInput(inputId = ns("gl1"),
                 label = "Grados de Libertad (n-2) del numerador:", 
                 step = 1,
                 min = 1,
                 value = 4),
    br(),
    numericInput(inputId = ns("gl2"),
                 label = "Grados de Libertad (n-2) del denominador:", 
                 step = 1,
                 min = 1,
                 value = 5),
    br(),
    actionButton(inputId = ns("calcular02"), label = "Calcular!"),
    br(),br()
  )
  
  
  
}


MainPanel04_f02 <- function(id){
  
  ns <- NS(id)
  
  
  div(textOutput(ns("texto_aviso")),
      uiOutput(ns("armado01"))
  )
  
  
}


Server04_f_Server02 <- function(input, output, session,
                                  la_distribucion) {
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  # los_parametros <- reactive({
  
  result <- reactiveVal(0)
  # result <- reactiveVal() #Esto lo hace NULL al inicio
  observeEvent(input$calcular01, { result(result() + 1) })
  observeEvent(input$calcular02, { result(result() + 1) })
  
  los_parametros <- eventReactive(result(), {
    
    
    
    gl1 <- NA
    gl2 <- NA
    
    opciones <- NA
    intervalo <- NA
    color_variable <- NA
    decimals <- NA
    
    f1 <- NA
    f2 <- NA
    f3 <- NA
    f4 <- NA
    probabilidad_externo <- NA
    porcentaje_externo <- NA
    
    ########################################################
    
    gl1 <- input$gl1
    gl2 <- input$gl2
    # varianza_poblacional <- input$varianza_poblacional
    color_variable <- input$color_variable
    
    
    opciones <- input$opciones
    intervalo <- input$intervalo
    decimals <- input$decimals
    
    
    f1 <- input$f_var1
    f2 <- input$f_var2
    f3 <- input$f_var3
    f4 <- input$f_var4
    
    probabilidad_externo <- input$prob_var1
    porcentaje_externo <- input$porcentaje_var1
    
    
    los_parametros <- Hmisc::llist(gl1, gl2, 
                                   opciones, intervalo, color_variable,
                                   f1, f2, f3, f4, 
                                   probabilidad_externo, porcentaje_externo, decimals)
    
    return(los_parametros)
    
    
    
  })
  
  RMedic_Info <- reactive({
    
    if(is.null(los_parametros())) return(NULL)
    
    mis_parametros <- los_parametros()
    
    out <-  do.call(Tabla.F, mis_parametros) # works
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
    out <- RMedic_Info()$tabla_externa03
    out <- CharacterALL(out)
    # out <- out[,-1]
    out
    # tabla_externa02()
  })
  
  output$frase_final01 <- renderUI({
    
    if(is.null(RMedic_Info())) return(NULL)
    out <- RMedic_Info()$frase01
    out <- paste0(out, "<br/>", RMedic_Info()$frase02)
    HTML(out)
  })
  
  output$frase_final02 <- renderUI({
    
    if(is.null(RMedic_Info())) return(NULL)
    out <- RMedic_Info()$frase03
    out <- paste0(out, "<br/>", RMedic_Info()$frase04)
    HTML(out)
  })
  
  # # Gráfico 1
  # observe(output$distPlot1 <- renderPlot({
  #   
  #   if(is.null(RMedic_Info())) return(NULL)
  #   RMedic_Info()$grafico01
  #   
  # }))
  # 
  # 
  output$distPlot1 <- renderPlot({
    
    if(is.null(RMedic_Info())) return(NULL)
    RMedic_Info()$grafico01
    
    
  })
  
  output$texto_aviso <- renderText({
    
    if(is.null(result())) return("Seleccione y de clic en 'Calcular!'") else
      return(NULL)
  })
  
  
  output$armado01 <- renderUI({
    
    if(is.null(result())) return(NULL)
    # if(is.null(los_parametros())) return("Seleccione y de clic en 'Calcular!'")
    # if (la_distribucion() != "001_Normal") return(NULL)
    
    div(h2("Distribución F"),
        plotOutput(ns("distPlot1")), br(),
        br(),
        br(),
        
        fluidRow(
          column(6, 
                 # h2("Parámetros"),
                 # tableOutput(ns("tabla01")),
                 # br(),
                 h2("Parámetros y Cálculos"),
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

