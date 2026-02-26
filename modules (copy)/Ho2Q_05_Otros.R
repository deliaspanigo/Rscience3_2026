


Ho2Q_05_Otros_UI <- function(id) {
  
  ns <- NS(id)
  
  uiOutput(ns("armado_ho"))
  
  
}






## Segmento del server
Ho2Q_05_Otros_SERVER <- function(input, output, session, 
                                                 minibase,
                                                 decimales,
                                                 control_ejecucion,
                                                 alfa) {
  
  
  
  # library(epitools)
  
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  
  control_02 <- reactive({
    
    control <- FALSE
    
    caso1 <- length(levels(as.character(minibase()[,1]))) == 2
    caso2 <- length(levels(as.character(minibase()[,2]))) == 2
    
    suma <- caso1 + caso2
    
    if(suma == 2) control <- TRUE
    
    return(control)
    
  })
  
  
  output$texto_cancelacion <- renderUI({
    
    texto01 <- ""
    
    texto02 <- "Para desarrollar este conjunto de herramientas cada variable debe
              contener 2 categorías."
    
    if(control_02()) texto <- texto02 else texto <- texto01
    
    HTML(texto)
  })
  
  # Control interno 01
  control_interno01 <- reactive({
    
    if(is.null(control_ejecucion())) return(FALSE)
    if(!control_02()) return(FALSE) else return(control_ejecucion())
  })
  
  
  
  # Lo dejamos creado por que nos va a hacer falta...
  minibase_mod <- reactive({
    
    minibase()
  })
  
  ##################################################
  
  tabla_observada <- reactive({
    
    aver <- RMedic_2q_tablas(minibase(), decimales())
    
    tabla <- aver$df02$Clasico$`Frecuencias Absolutas`
    for(n in 1:ncol(tabla)) tabla[,n] <- as.character(tabla[,n])

    return(tabla)
    
    })
  
  
  tabla_reordenada_interna <- reactive({
    
    if(is.null(tabla_observada())) return(NULL)
    if(is.null(input$cambiador_filas)) return(NULL)
    if(is.null(input$cambiador_columnas)) return(NULL)
    
    tabla <- tabla_observada()
  
    
    cambio_filas <- c(1,2)
    if(input$cambiador_filas == rownames(tabla_observada())[2]) cambio_filas <- c(2,1)

    cambio_columnas <- c(1,2)
    if(input$cambiador_columnas == colnames(tabla_observada())[2]) cambio_columnas <- c(2,1)
    
    tabla <- tabla[cambio_filas, cambio_columnas]
    colnames(tabla) <- paste0(colnames(tabla), c("(0)", "(1)"))
    rownames(tabla) <- paste0(rownames(tabla), c("(0)", "(1)"))
    
    tabla[,1] <- as.numeric(as.character(tabla[,1]))
    tabla[,2] <- as.numeric(as.character(tabla[,2]))
    return(tabla)
  })
  
  
  tabla_reordenada_externa <- reactive({
    
    if(is.null(tabla_reordenada_interna())) return(NULL)
    tabla <- tabla_reordenada_interna()
    
    tabla[,1] <- as.character(as.character(tabla[,1]))
    tabla[,2] <- as.character(as.character(tabla[,2]))
    
    tabla[1,1] <- paste0(tabla[1,1], " (VN)")
    tabla[1,2] <- paste0(tabla[1,2], " (FN)")
    tabla[2,1] <- paste0(tabla[2,1], " (FP)")
    tabla[2,2] <- paste0(tabla[2,2], " (VP)")
    
    return(tabla)
  })
    
  # Tabla de Referencia "Otros"
  observe(output$tabla_referencia <- renderTable(rownames = T, bordered = T,{
    
    
    TABLA <- matrix(NA, 2, 2)
    TABLA[1,] <- c("VN", "FN")
    TABLA[2,] <- c("FP", "VP")
    
    colnames(TABLA) <- c(0, 1)
    rownames(TABLA) <- c(0, 1)
    
    
    TABLA
    
    
    
  }))
  
  
  # Tabla de Referencia "Otros"
observe(output$tabla_observada <- renderTable(rownames = T, bordered = T,{
    
    
    tabla_observada()
    
  }))
  
  
  # Tabla de Referencia "Otros"
observe(output$tabla_reordenada_externa <- renderTable(rownames = T, bordered = T,{
    
    
    tabla_reordenada_externa()
    
  }))
  
  
  
  # Detalle de niveles "cero" de cada variable
observe(  output$cambiadores <- renderUI({
    
      div(
        radioButtons(inputId = ns("cambiador_filas"), 
                     label = paste0("Referencia '0' para Variable 1 (filas) - '", colnames(minibase_mod())[1], "':"), 
                     choices = levels(as.factor(minibase_mod()[,1]))),
        radioButtons(inputId = ns("cambiador_columnas"), 
                     label = paste0("Referencia '0' para Variable 2 (columnas) - '", colnames(minibase_mod())[2], "':"), 
                     choices = levels(as.factor(minibase_mod()[,2])))
        
        
      )
      
    
  }))
  
  
  ############################################################

# ODD RATIOS
{
  ###
  
  # Generacion de Detalles de OddRatios  
  OR_OTROS <- reactive({
    
    if(is.null(tabla_reordenada_interna())) return(NULL)
    
    
            TABLA <- tabla_reordenada_interna()
            DECIMALES <- decimales()
            ALFA <- alfa()
            
            porcentaje <- 1 - ALFA
            porcentaje <- porcentaje*100
            porcentaje <- paste0(porcentaje, "%")
            porcentaje <- paste0("(", porcentaje, ")")
            
            n00 = as.numeric(TABLA[1,1])
            n01 = as.numeric(TABLA[1,2])
            n10 = as.numeric(TABLA[2,1])
            n11 = as.numeric(TABLA[2,2])
            
            #  p1 <- n00 * n11
            #  p2 <- n01*n10
            
            p1 <-  round2(n11/n10, DECIMALES)
            p2 <-  round2(n01/n00, DECIMALES)
            
            #      OR <- (n00 * n11)/(n01 * n10)
            OR <- round2((n11/n10) / (n01/n00), DECIMALES)
            #
            #  Compute the Wald confidence intervals:
            #
            siglog <- sqrt((1/n00) + (1/n01) + (1/n10) + (1/n11))
            zalph <- qnorm(1 - ALFA/2)
            logOR <- log(OR)
            loglo <- logOR - zalph * siglog
            loghi <- logOR + zalph * siglog
            #
            ORlo <- exp(loglo)
            ORhi <- exp(loghi)
            #
            
            oframe <- data.frame(ORlo, OR, ORhi, ALFA)
            colnames(oframe) <- c(paste0("Límite Inferior", porcentaje), "Odd Ratio", paste0("Límite Superior", porcentaje), "Alfa")
            #    oframe
            
            OR_SALIDA <- list()
            #  OR_SALIDA$TABLA <- list()
            #  OR_SALIDA$FRASE1 <- list()
            OR_SALIDA$TABLA <- oframe
            #   OR_SALIDA$FRASE1 <- paste0("OR = (VN * VP)/(FN * FP) = (", n11 , "*", n00, ")/(", n01, "*", n10, ") = ", p1, "/", p2, "=", OR)
            OR_SALIDA$FRASE1 <- paste0("OR = (VP/FP) / (FN/VN) = (", n11 , "/", n10, ") / (", n01, "/", n00, ") = ", p1, " / ", p2, " = ", OR)
            
            
            #  OR_SALIDA$FRASE1 <- "JKA" 
            OR_SALIDA
   
    
  })
  
  # Tabla OddRatios
  observe(output$TABLA_OR <- renderTable(rownames = F, bordered = T, align = "c", {
    
    if(is.null(OR_OTROS())) return(NULL)
    
      TABLA <- OR_OTROS()$TABLA
      
      TABLA
      
  }))
  
  # Frase1 de OddRatios
  observe(output$FRASE1_OR <- renderText({
    
    if(is.null(OR_OTROS())) return(NULL)
    
    
      FRASE <- OR_OTROS()$FRASE1
      
      FRASE
      
  }))
  
  
  #####################################################################################
  
  ###
} # Fin ODD RATIOS
##################################################################################



# RIESGO RELATIVO
{
  ###
  # Detalles de Riesgos Relativos
  RR_OTROS <- reactive({
    

    if(is.null(tabla_reordenada_interna())) return(NULL)   
    
            #    cat("Estoy adentro", "\n")
            TABLA <- tabla_reordenada_interna()
            DECIMALES <- decimales()
            ALFA <- alfa()
            CONFIANZA <- 1 - ALFA
            
            
            porcentaje <- 1 - ALFA
            porcentaje <- porcentaje*100
            porcentaje <- paste0(porcentaje, "%")
            porcentaje <- paste0("(", porcentaje, ")")
            
            n00 = as.numeric(TABLA[1,1])
            n01 = as.numeric(TABLA[1,2])
            n10 = as.numeric(TABLA[2,1])
            n11 = as.numeric(TABLA[2,2])
            
            # Totales por fila
            tf1 <- n00 + n01
            tf2 <- n10 + n11
            
            # Totales por columna
            tc1 <- n00 + n10
            tc2 <- n01 + n11
            
            p1 <- n11/tf2
            p1 <- round2(p1, DECIMALES)
            p2 <- n01/tf1
            p2 <- round2(p2, DECIMALES)
            #      cat((p1/p2), "\n")
            res <- fmsb::riskratio(X = n11, Y = n01, m1 = tf2, m2 = tf1, conf.level = CONFIANZA)
            
            RR_OBS <- p1/p2
            RR_OBS <- round2(RR_OBS, DECIMALES)
            
            RR <- res$estimate   #
            RR <- round2(RR, DECIMALES)
            RRlo <- res$conf.int[1]
            RRhi <- res$conf.int[2]
            p_interno <- res$p.value
            if (p_interno < 0.01) p_externo <- "<<0.01" else p_externo <- p_interno
            if (p_interno < ALFA) decision <- "Rechazo Ho" else decision <- "No Rechazo Ho"
            
            
            oframe <- data.frame(RRlo, paste0(RR, "(*)"), RRhi, p_interno, ALFA, decision)
            colnames(oframe) <- c(paste0("Límite Inferior", porcentaje), "Riesgo Relativo", paste0("Límite Superior", porcentaje), "Valor p", "Alfa", "Decisión")
            
            
            RR_SALIDA <- list()
            #  OR_SALIDA$TABLA <- list()
            #  OR_SALIDA$FRASE1 <- list()
            RR_SALIDA$TABLA <- oframe
            RR_SALIDA$FRASE1 <- paste0("RR = (VP/(VP+FP)) / (FN/(FN+VN))) = (", n11 , "/(",n11, "+", n10, ")) / (", n01, "/(", n01, "+", n00,")) = (", n11, "/", tf2, ") / (", n01, "/", tf1, ") = ", p1, " / ", p2, " = ", RR_OBS) 
            #  OR_SALIDA$FRASE1 <- "JKA" 
            RR_SALIDA
            
   
    
    
  })
  
  
  # Tabla de Riesgos Relativos
  output$TABLA_RR <- renderTable(rownames = F, bordered = T, align = "c",{
    if(is.null(RR_OTROS())) return(NULL)
      
      TABLA <- RR_OTROS()$TABLA
      
      TABLA
      
  })
  
  
  
  output$FRASE1_RR <- renderText({
    if(is.null(RR_OTROS())) return(NULL)
      FRASE <- RR_OTROS()$FRASE1
      
      FRASE
  
  })
  
  
  ###  
} # Fin RIESGO RELATIVO
################################################################################


# VALORES PREDICTIVOS
{
  ###
  
  # Valores Predictivos
  VP_OTROS <- reactive({
    
    if(is.null(tabla_reordenada_interna())) return(NULL)   
            
            #    cat("Estoy adentro", "\n")
            TABLA <- tabla_reordenada_interna()
            ALFA <- alfa()
            DECIMALES <- decimales()
            
            porcentaje <- 1 - ALFA
            porcentaje <- porcentaje*100
            porcentaje <- paste0(porcentaje, "%")
            porcentaje <- paste0("(", porcentaje, ")")
            
            n00 = as.numeric(TABLA[1,1])
            n01 = as.numeric(TABLA[1,2])
            n10 = as.numeric(TABLA[2,1])
            n11 = as.numeric(TABLA[2,2])
            
            tf1 <- n00 + n01
            tf2 <- n10 + n11
            
            VPP <- n11/tf2
            VPN <- n00/tf1
            
            VPP <- round2(VPP,   DECIMALES)
            VPN <- round2(VPN, DECIMALES)
            nombres <- c("Detalle", "Estimado")
            TABLA <- matrix(NA, 2, 2)
            colnames(TABLA) <- nombres
            TABLA[,1] <- c("VPP", "VPN")
            TABLA[,2] <- c(VPP, VPN)
            
            VP_SALIDA <- list()
            VP_SALIDA$TABLA <- TABLA
            VP_SALIDA$FRASE1 <- paste0("VPP = VP/(VP + FP) = ", n11, " / (", n11, "+", n10, ") = ", n11 , "/", tf2, " = ", VPP)
            VP_SALIDA$FRASE2 <- paste0("VPN = VN/(VN + FN) = ", n00, " / (", n00, "+", n01, ") = ", n00 , "/", tf1, " = ", VPN)
            
            VP_SALIDA

    
  })
  
  
  # Tabla de Valores Predictivos  
  output$TABLA_VP <- renderTable(rownames = F, bordered = T, align = "c",{
    if (is.null(VP_OTROS())) return(NULL)
    
      TABLA <- VP_OTROS()$TABLA
      
      TABLA
      
  })
  
  # Frase 1 Valores Predictivos
  output$FRASE1_VP <- renderText({
    if (is.null(VP_OTROS())) return(NULL)
    
      FRASE <- VP_OTROS()$FRASE1
      
      FRASE
      
  })
  
  # Frase 2 Valores Predictivos
  output$FRASE2_VP <- renderText({
    if (is.null(VP_OTROS())) return(NULL)
      FRASE <- VP_OTROS()$FRASE2
      
      FRASE
      
  })
  
  
  ###  
} # Fin VALORES PREDICTIVOS
###################################################################################



# SENSIBILIDAD Y ESPECIFICIDAD
{
  ###  
  
  
  
  # Sensibilidad y Especificidad
  SE_OTROS <- reactive({
    

    if(is.null(tabla_reordenada_interna())) return(NULL)   
    
            #    cat("Estoy adentro", "\n")
            TABLA <- tabla_reordenada_interna()
            ALFA <- alfa()
            DECIMALES <- decimales()
            
            porcentaje <- 1 - ALFA
            porcentaje <- porcentaje*100
            porcentaje <- paste0(porcentaje, "%")
            porcentaje <- paste0("(", porcentaje, ")")
            
            n00 = as.numeric(TABLA[1,1])
            n01 = as.numeric(TABLA[1,2])
            n10 = as.numeric(TABLA[2,1])
            n11 = as.numeric(TABLA[2,2])
            
            tc1 <- n00 + n10
            tc2 <- n01 + n11
            
            SENSIBILIDAD <- n11/tc2
            ESPECIFICIDAD <- n00/tc1
            
            SENSIBILIDAD <- round2(SENSIBILIDAD,   DECIMALES)
            ESPECIFICIDAD <- round2(ESPECIFICIDAD, DECIMALES)
            nombres <- c("Detalle", "Estimado")
            TABLA <- matrix(NA, 2, 2)
            colnames(TABLA) <- nombres
            TABLA[,1] <- c("Sensibilidad", "Especificidad")
            TABLA[,2] <- c(SENSIBILIDAD, ESPECIFICIDAD)
            
            SE_SALIDA <- list()
            SE_SALIDA$TABLA <- TABLA
            SE_SALIDA$FRASE1 <- paste0("Sensibilidad  = VP/(VP + FN) = ", n11, " / (", n11, "+", n01, ") = ", n11 , "/", tc2, " = ", SENSIBILIDAD)
            SE_SALIDA$FRASE2 <- paste0("Especificidad = VN/(VN + FP) = ", n00, " / (", n00, "+", n10, ") = ", n00 , "/", tc1, " = ", ESPECIFICIDAD)
            
            SE_SALIDA
            
       
    
  })
  
  
  # Tabla SE
  output$TABLA_SE <- renderTable(rownames = F, bordered = T, align = "c", {
    if (is.null(SE_OTROS())) return(NULL)
      TABLA <- SE_OTROS()$TABLA
      
      TABLA
      

  })
  
  # Frase1 SE
  output$FRASE1_SE <- renderText({
    if (is.null(SE_OTROS())) return(NULL)
      FRASE <- SE_OTROS()$FRASE1
      
      FRASE
      

  })
  
  # Frase2 SE
  output$FRASE2_SE <- renderText({
    if (is.null(SE_OTROS())) return(NULL)
      FRASE <- SE_OTROS()$FRASE2
      
      FRASE
      

  })
  
  ###    
} # Fin SENSIBILIDAD Y ESPECIFICIDAD
###################################################################################



  ##############################################################################
  # Armado/Salida del test de Proporciones 1Q
  output$armado_ho <- renderUI({
    
    div(
      span(htmlOutput(ns("texto_cancelacion")), style="color:red"),
      h2("Tabla de Referencia (Teórica)"),
      fluidRow(
        column(6,
      tableOutput(ns("tabla_referencia"))
      ),
      column(6, 
               "VN: Verdadero Negativo", br(),
               "FN: Falso Negativo", br(),
               "FP: Falso Positivo", br(),
               "VP: Verdadero Positivo", br()
             )
      ),
      br(), br(),
      h2("Tabla Observada"),
      tableOutput(ns("tabla_observada")),
      br(), br(),
      h2("Selección de reordenamiento"),
      uiOutput(ns("cambiadores")), br(), br(),
      h2("Tabla Reordenada"),
      tableOutput(ns("tabla_reordenada_externa")),
      br(), br(),
      tabsetPanel(
        tabPanel("OR y RR",
                 br(),
                 h3("Odd Ratios"),
                 tableOutput(ns("TABLA_OR")),
                 br(),  br(),
                 h3("Riesgo Relativo"),
                 tableOutput(ns("TABLA_RR")),
                 "(*)Pueden existir diferencias decimales entre el cálculo de R y el cálculo a manual.",
                 br(),
                 h3("Cálculos manuales"),
                 textOutput(ns("FRASE1_OR")),
                 textOutput(ns("FRASE1_RR"))
                 ),
        tabPanel("VPP y VPN",
                 h3("Valores Predictivos"),
                 tableOutput(ns("TABLA_VP")),
                 textOutput(ns("FRASE1_VP")),
                 textOutput(ns("FRASE2_VP")),
                 br()),
        tabPanel("Sensibilidad y Especificidad",
                 h3("Sensibilidad y Especificidad"),
                 tableOutput(ns("TABLA_SE")),
                 textOutput(ns("FRASE1_SE")),
                 textOutput(ns("FRASE2_SE")),
                 br())
      )
    )
    
  })
  
  
  
  
  
  
  
}


