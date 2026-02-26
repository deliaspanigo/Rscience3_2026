## Segmento del UI
modules_03_tablas_03_Tablas2Q_UI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("SeccionTablas2Q"))
  
  
}




## Segmento del server
modules_03_tablas_03_Tablas2Q_SERVER <- function(input, output, session, 
                            minibase, 
                            batalla_naval,
                            decimales) {
  
  
  
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  
  check_ok <- reactive({
    
    req(minibase(), batalla_naval(), decimales())
    req(batalla_naval()[[4]], batalla_naval()[[4]] == 3) # QQ es 3
    
    ########
    mis_columnas  <- colnames(minibase())
    mis_variables <- batalla_naval()$variables
    the_check <- all(mis_variables %in% mis_columnas)
    
    req(the_check)
    ####################33
    TRUE
    
  })
  
  # Caso 3: 2Q
  casoRMedic <- reactive({
    
    req(batalla_naval())
    casoRMedic <- batalla_naval()[[4]]
    #casoRMedic <- as.numeric(as.character(as.vector(batalla_naval()[[4]])))
    casoRMedic
    
  })
  
  
  Original_2q_Tablas <- reactive({
    
    if(is.null(casoRMedic())) return(NULL)
    if(casoRMedic() != 3) return(NULL)
    
    tryCatch({
      
      req(minibase())

      if(is.null(casoRMedic())) return(NULL)

      general <- RMedic_2q_tablas(minibase(), decimales())
      
      general$df02[[1]][[1]] <- as.matrix(general$df02[[1]][[1]])
      general$df02[[1]][[1]][1,1] <- as.character(general$df02[[1]][[1]][1,1])
      
      general$df02[[1]][[2]] <- as.matrix(general$df02[[1]][[2]])
      general$df02[[1]][[2]][1,1] <- as.character(general$df02[[1]][[2]][1,1])
      
      general$df02[[2]][[1]] <- as.matrix(general$df02[[2]][[1]])
      general$df02[[2]][[1]][1,1] <- as.character(general$df02[[2]][[1]][1,1])
      
      general$df02[[3]][[1]] <- as.matrix(general$df02[[3]][[1]])
      general$df02[[3]][[1]][1,1] <- as.character(general$df02[[3]][[1]][1,1])
      
      general$df02[[4]][[1]] <- as.matrix(general$df02[[4]][[1]])
      general$df02[[4]][[1]][1,1] <- as.character(general$df02[[4]][[1]][1,1])    
      
      general$df02[[5]][[1]] <- as.matrix(general$df02[[5]][[1]])
      general$df02[[5]][[1]][1,1] <- as.character(general$df02[[5]][[1]][1,1])    
      
      general$df02[[6]][[1]] <- as.matrix(general$df02[[6]][[1]])
      general$df02[[6]][[1]][1,1] <- as.character(general$df02[[6]][[1]][1,1])
      
      
      # Return Exitoso
      return(general)
    }, error = function(e) {
      # Si ocurre cualquier error, simplemente devolver ""
      return("")
    })
    
    
    
    

  })
  
  Tablas_Chi <- reactive({
    
    req(minibase())
    test <- chisq.test(table(minibase()))
    tabla01 <- test[[7]]
    tabla02 <- tabla01 # test[[8]]
    tabla03 <-tabla01 # test[[9]]
    
    for(k1 in 1:nrow(tabla02)) {  
      for (k2 in 1:ncol(tabla01)) {
        
        
        tabla02[k1, k2] <- test[[8]][k1, k2]
        tabla03[k1, k2] <- test[[9]][k1, k2]
      }
    }
    salida <- list(tabla01, tabla02, tabla03)
    salida
  })
  
  ##########################################
  
  # Tabla Esperados
  observe( output$tabla_especial01 <- renderTable(digits = decimales(), align= "c",rownames = T,{
    
    # Tablas_Chi()$expected
    Tablas_Chi()[[1]]
    
  }))
  
  # Tabla Residuos
  observe( output$tabla_especial02 <- renderTable(digits = decimales(), align= "c",rownames = T,{
    
    #Tablas_Chi()$residuals
    Tablas_Chi()[[2]]
  }))
  
  # Tabla Residuos Estudentizados
  observe( output$tabla_especial03 <- renderTable(digits = decimales(), align= "c",rownames = T,{
    
    #Tablas_Chi()$stdres
    Tablas_Chi()[[3]]
  }))
  
  ##########################################
  Referencias_var_2q <- reactive({
    
    if(is.null(casoRMedic())) return(NULL)
    if(casoRMedic() != 3) return(NULL)
    
    referencias <- Original_2q_Tablas()$referencias
    
    armado <- paste0(referencias[1], "<br/>", 
                     referencias[2])
    
    armado <- HTML(armado)
    
    return(armado)
  })
  
  
  # Todas las tablas 2Q
  Reactive_tabla_2q_RMedic <- reactive({
    
    req(Original_2q_Tablas())
    if(is.null(casoRMedic())) return(NULL)
    if(casoRMedic() != 3) return(NULL)
    
    
    general <- Original_2q_Tablas()$df02
    
    
    
    salida <- list()
    
    conteo <- 0
    for (k1 in 1:length(general)) {
      for(k2 in 1:length(general[[k1]])){
        
        conteo <- conteo + 1
        salida[[conteo]] <- list(names(general[[k1]])[k2],
                                 general[[k1]][[k2]])
      }
    }
    
    
    
    # Return Exitoso
    return(salida)
    
    
  })  
  
  
  
  # Cantidad de tablas
  cantidad_tablas <- reactive({
    
    if(is.null(Reactive_tabla_2q_RMedic)) return(NULL)
    
    # Return Exitoso
    return(length(Reactive_tabla_2q_RMedic()))
  })
  
  # Create all renderTables!!!     
  observe(
    lapply(c(1:cantidad_tablas()), function(i) {
      
      nombre_fusion1 <- paste0('Salida_texto_2q_RMedic_', CifrasPerfectas(i))
      nombre_fusion2 <- paste0('Salida_tabla_2q_RMedic_', CifrasPerfectas(i))
      
      # El rotulo de cada tabla       
      output[[nombre_fusion1]] <- renderText({
        # names(Reactive_tabla_2q_RMedic())[i]
        Reactive_tabla_2q_RMedic()[[i]][[1]]
      })
      
      # Cada tabla
      output[[nombre_fusion2]] <- renderTable(digits = decimales(), align= "c",rownames = T,{
        Reactive_tabla_2q_RMedic()[[i]][[2]]
      })
      
      
      
    })
  ) 
  
  
  
  
  
  output$SeccionTablas2Q <- renderUI({
    
    # Especificaciones de cumplimiento
    if(is.null(casoRMedic())) return(NULL)
    if(casoRMedic() != 3) return(NULL)
    
    
    
    # Si es el caso 1, seguimos!
    div(
      h2_mod("RMedic - Tablas para 2 Variables Categóricas"),
      tabsetPanel(id = ns("Tablas_2q"),
                  tabPanel(title = "RMedic Help!", 
                           value = 1,
                           fluidRow(
                             column(4, 
                                    radioButtons(inputId = "help_tablas_2q",
                                                 label = h3("Selección de Ayuda Automática"),
                                                 choices = c("RMedic Here!" = 1,
                                                             "Resumen de Tablas de Doble Entrada" = 2,
                                                             "Clásico" = 3,
                                                             "Al total" = 4, 
                                                             "Por filas" = 5, 
                                                             "Por columnas" = 6,
                                                             "Simple Entrada" = 7,
                                                             "Otras Tablas" = 8)
                                    )
                             ),
                             column(8,
                                    br(),
                                    conditionalPanel(condition = "input.help_tablas_2q == 1",
                                                     div(
                                                       h3_mod("RMedic Here!"),
                                                       HTML(
                                                         "Para presentan y resumir información de dos variables categóricas
                                      una de las formas más utilizadas son las <b>Tablas de Doble Entrada</b>.<br/>
                                      Estas son tablas de doble entrada, con diferentes posibilidades: 
                                      frecuencias absolutas (FA), cociente, frecuencias relativas (FR), y porcentajes.
                                      A su vez estos 4 tipos de tablas pueden ser especificadas con detalles al total, 
                                      por filas, por columnas o ambos. El total de tablas presentadas en esta sección
                                      es de 19 <b>Tablas de Doble Entrada</b> que abarcan todas las posibilidades.<br/>
                                      RMedic agrega a demás la opción de una tabla de contingencia presentada en el formato de simple entrada.<br/><br/>
                                      Se utiliza en caso de tener 2 variables categóricas.<br/>
                                      Cada una de las variables debe contener al menos una categoría.<br/>
                                      Solo son utilizados las filas en las cuales ambas variables poseen de manera simultánea un dato.<br/><br/>
                                      Las categorías de una variable se coloca en filas y las cateogirís de la otra en columnas.<br/>
                                      El cambio en el orden de las variables solo genera una rotación de la tabla, pero no altera los resultados obtenidos.<br/><br/>
                                      En algunos contextos en el cual existe una asociación causa/efecto entre las variables, puede ser interesante
                                      visualizar las tablas colocando a la variable cuasa en filas y a la variable efecto en columnas.<br/><br/>
                                      En contextos en donde quiere comparar un Gold Estándard con una nueva categorización, suele colocarse
                                      al Gold Estándard en filas y a la nueva recategorización en columnas.
                                      "
                                                       )
                                                     )
                                    ),
                                    conditionalPanel(condition = "input.help_tablas_2q == 2",
                                                     div(
                                                       h3("Resumen de Tablas de Doble Entrada"),
                                                       HTML(
                                                         "Son 3 las <b>Tablas de Doble Entrada</b> más utilizada:<br/>
                                    1) Frecuencias Absolutas.<br/>
                                    2) Frecuencias Absolutas y total por fila.<br/>
                                    3) Porcentajes por filas.<br/><br/>
                                    El cambio en el orden de las variables solo genera una rotación de la tabla, pero no altera los resultados obtenidos.<br/><br/>
                                    En algunos contextos en el cual existe una asociación causa/efecto entre las variables, puede ser interesante
                                    visualizar las tablas colocando a la variable cuasa en filas y a la variable efecto en columnas.<br/><br/>
                                    En contextos en donde quiere comparar un Gold Estándard con una nueva categorización, suele colocarse
                                    al Gold Estándard en filas y a la nueva recategorización en columnas.
                                ")
                                                     )
                                    ),
                                    conditionalPanel(condition = "input.help_tablas_2q == 3",
                                                     div(
                                                       h3("Clásico"),
                                                       HTML("El formato clásico es también el más básico. Se presentan todas
                                                        las <b>Tablas de Doble Entrada</b> en su estructura más simple.")
                                                     )
                                    ),
                                    conditionalPanel(condition = "input.help_tablas_2q == 4",
                                                     div(
                                                       h3("Al total"),
                                                       HTML("Se agregan tres aspectos sobre el formato clásico: la suma por filas, las sumas por columnas y la suma total sobre
                                                            la tabla de contingencia de frecuencias absolutas. Esto trae también mayores especificaciones
                                                            en el resto de tablas asociadas."
                                                       )
                                                     )
                                    ),
                                    conditionalPanel(condition = "input.help_tablas_2q == 5",
                                                     div(
                                                       h3("Por filas"),
                                                       HTML("Se agrega al formato clásico la suma por filas a la tabla de contingencia de frecuencias absolutas. Esto trae
                                                       aparejado especificaciones por filas en el resto de las tablas asociadas."
                                                       )
                                                     )
                                    ),
                                    conditionalPanel(condition = "input.help_tablas_2q == 6",
                                                     div(
                                                       h3("Por columnas"),
                                                       HTML("Se agrega al formato clásico la suma por columnas a la tabla de contingencia de frecuencias absolutas. Esto trae
                                                       aparejado especificaciones por columnas en el resto de las tablas asociadas."
                                                       )
                                                     )
                                    ),
                                    conditionalPanel(condition = "input.help_tablas_2q == 7",
                                                     div(
                                                       h3("Simple Entrada"),
                                                       HTML("Se desdobla la información de las <b>Tablas de Doble Entrada</b> a una una sola gran tabla de simple entrada
                                                            en la cual se detallarán en filas todas las combinaciones posibles entre las categorías de ambas variables y
                                                            en columnas los diferentes tipos de información: frecuencias absolutas (FA), total, cociente al total, frecuencia
                                                            relativa al total (FR) y porcentaje al total."
                                                       )
                                                     )
                                    ),
                                    conditionalPanel(condition = "input.help_tablas_2q == 8",
                                                     div(
                                                       h3("Otras Tablas"),
                                                       HTML("Se presenta las <b>Tablas de Doble Entrada</b> de valores esperados, residuos y residuos studentizados."
                                                       )
                                                     )
                                    )
                             )
                           )
                  ),
                  tabPanel(title = "Resumen de Tablas de Doble Entrada", value = 2,
                           h3_mod(textOutput(ns("Salida_texto_2q_RMedic_01"))),
                           Referencias_var_2q(),
                           tableOutput(ns("Salida_tabla_2q_RMedic_01")),
                           br(),
                           h3_mod(textOutput(ns("Salida_texto_2q_RMedic_02"))),
                           Referencias_var_2q(),
                           tableOutput(ns("Salida_tabla_2q_RMedic_02")),
                           br(),
                           h3_mod(textOutput(ns("Salida_texto_2q_RMedic_03"))),
                           Referencias_var_2q(),
                           tableOutput(ns("Salida_tabla_2q_RMedic_03")),
                           br()
                  ),
                  tabPanel(title = "Clásico", value = 3,
                           h3_mod(textOutput(ns("Salida_texto_2q_RMedic_04"))),
                           Referencias_var_2q(),
                           tableOutput(ns("Salida_tabla_2q_RMedic_04")),
                           br(),
                           h3_mod(textOutput(ns("Salida_texto_2q_RMedic_05"))),
                           Referencias_var_2q(),
                           tableOutput(ns("Salida_tabla_2q_RMedic_05")),
                           br(),
                           h3_mod(textOutput(ns("Salida_texto_2q_RMedic_06"))),
                           Referencias_var_2q(),
                           tableOutput(ns("Salida_tabla_2q_RMedic_06")),
                           br(),
                           h3_mod(textOutput(ns("Salida_texto_2q_RMedic_07"))),
                           Referencias_var_2q(),
                           tableOutput(ns("Salida_tabla_2q_RMedic_07")),
                           br(),
                           h3_mod(textOutput(ns("Salida_texto_2q_RMedic_08"))),
                           Referencias_var_2q(),
                           tableOutput(ns("Salida_tabla_2q_RMedic_08")),
                           br()
                  ),
                  tabPanel(title = "Al total", value = 4,
                           h3_mod(textOutput(ns("Salida_texto_2q_RMedic_09"))),
                           Referencias_var_2q(),
                           tableOutput(ns("Salida_tabla_2q_RMedic_09")),
                           br(),
                           h3_mod(textOutput(ns("Salida_texto_2q_RMedic_10"))),
                           Referencias_var_2q(),
                           tableOutput(ns("Salida_tabla_2q_RMedic_10")),
                           br(),
                           h3_mod(textOutput(ns("Salida_texto_2q_RMedic_11"))),
                           Referencias_var_2q(),
                           tableOutput(ns("Salida_tabla_2q_RMedic_11")),
                           br(),
                           h3_mod(textOutput(ns("Salida_texto_2q_RMedic_12"))),
                           Referencias_var_2q(),
                           tableOutput(ns("Salida_tabla_2q_RMedic_12")),
                           br(),
                           h3_mod(textOutput(ns("Salida_texto_2q_RMedic_13"))),
                           Referencias_var_2q(),
                           tableOutput(ns("Salida_tabla_2q_RMedic_13")),
                           br()
                           
                  ),
                  tabPanel(title = "Por filas", value = 5,
                           h3_mod(textOutput(ns("Salida_texto_2q_RMedic_14"))),
                           Referencias_var_2q(),
                           tableOutput(ns("Salida_tabla_2q_RMedic_14")),
                           br(),
                           h3_mod(textOutput(ns("Salida_texto_2q_RMedic_15"))),
                           Referencias_var_2q(),
                           tableOutput(ns("Salida_tabla_2q_RMedic_15")),
                           br(),
                           h3_mod(textOutput(ns("Salida_texto_2q_RMedic_16"))),
                           Referencias_var_2q(),
                           tableOutput(ns("Salida_tabla_2q_RMedic_16")),
                           br(),
                           h3_mod(textOutput(ns("Salida_texto_2q_RMedic_17"))),
                           Referencias_var_2q(),
                           tableOutput(ns("Salida_tabla_2q_RMedic_17")),
                           br(),
                           h3_mod(textOutput(ns("Salida_texto_2q_RMedic_18"))),
                           Referencias_var_2q(),
                           tableOutput(ns("Salida_tabla_2q_RMedic_18")),
                           br()
                           
                  ),
                  tabPanel(title = "Por columnas", value = 6,
                           h3_mod(textOutput(ns("Salida_texto_2q_RMedic_19"))),
                           Referencias_var_2q(),
                           tableOutput(ns("Salida_tabla_2q_RMedic_19")),
                           br(),
                           h3_mod(textOutput(ns("Salida_texto_2q_RMedic_20"))),
                           Referencias_var_2q(),
                           tableOutput(ns("Salida_tabla_2q_RMedic_20")),
                           br(),
                           h3_mod(textOutput(ns("Salida_texto_2q_RMedic_21"))),
                           Referencias_var_2q(),
                           tableOutput(ns("Salida_tabla_2q_RMedic_21")),
                           br(),
                           h3_mod(textOutput(ns("Salida_texto_2q_RMedic_22"))),
                           Referencias_var_2q(),
                           tableOutput(ns("Salida_tabla_2q_RMedic_22")),
                           br(),
                           h3_mod(textOutput(ns("Salida_texto_2q_RMedic_23"))),
                           Referencias_var_2q(),
                           tableOutput(ns("Salida_tabla_2q_RMedic_23")),
                           br()
                           
                  ),
                  tabPanel(title = "Simple Entrada", value = 7,
                           h3_mod(textOutput(ns("Salida_texto_2q_RMedic_24"))),
                           Referencias_var_2q(),
                           tableOutput(ns("Salida_tabla_2q_RMedic_24")),
                           br()
                  ),
                  tabPanel(title = "Otras Tablas", value = 8,
                           h3_mod("Frecuencias Esperadas"),
                           Referencias_var_2q(),
                           tableOutput(ns("tabla_especial01")), 
                           br(),
                           h3_mod("Residuos"),
                           Referencias_var_2q(),
                           tableOutput(ns("tabla_especial02")), 
                           br(),
                           h3_mod("Residuos Studentizados"),
                           Referencias_var_2q(),
                           tableOutput(ns("tabla_especial03")), 
                  )
      ),
    )
  })
  
  
  
}


