source("../../lib.R")

library(ggplot2)

Distribucion.t <- function(media_muestral, varianza_muestral = NA, 
                                desvio_muestral = NA, 
                                gl,
                                opciones, 
                                intervalo,
                                color_variable,
                                var1 = NA, var2 = NA, var3 = NA, var4 = NA, 
                                t1 = NA, t2 = NA, t3 = NA, t4 = NA, 
                                probabilidad_externo = NA, porcentaje_externo = NA, decimals = 2){
  
  
  if(is.na(desvio_muestral)) desvio_muestral <- sqrt(varianza_muestral)
  if(is.na(varianza_muestral)) varianza_muestral <- desvio_muestral^2
  
  # opciones puede tomar 4 valores: 
  # 1) "original"
  # 2) "valor_t"
  # 3) "probabilidad"
  # 4) "porcentaje"
  
  
  # intervalo puede tomar 3 valores:
  # 1) "menor"
  # 2) "mayor"
  # 3) "entre
  
  # Tabla interna 01
  tabla_interna01 <- data.frame(media_muestral, varianza_muestral, desvio_muestral,
                                gl,
                                opciones, intervalo, color_variable)
  
  # Tabla Interna 02
  {
    
    # Normal Completa
    min_distribucion <- -4
    max_distribucion <- 4
    h <- (max_distribucion - min_distribucion)/1000
    
    marcas_t <- min_distribucion:max_distribucion
    marcas_variable <- marcas_t*desvio_muestral + media_muestral
    
    if(1 == 1){
      # Si los valores ingresados son de la variable original
      if(opciones == "original") {
        if(intervalo == "menor"){
          
          if(is.na(var1)) stop("Falta ingresar var1")
          
          var_izquierdo <- min(marcas_variable)
          var_derecho <- var1
          
          t_izquierdo <- min_distribucion
          t_derecho <- (var_derecho - media_muestral)/desvio_muestral
          
          la_probabilidad <- pt(q = t_derecho, df = gl, lower.tail = T)
          el_porcentaje <- la_probabilidad*100
          
          frase01 <- "El valor de la variable original es _VariableDerecho_.<br/>
                      El valor estandarizado es t = _t_derecho_."
          
          frase02 <- "La probabilidad de pacientes con valores de la variable original menores a _VariableDerecho_ es _probabilidad_.<br/>
                      La probabilidad de pacientes con valores t menores a _t_derecho_ es _probabilidad_."
          
          frase03 <- "El valor estandarizado es t = _t_derecho_."
          
          frase04 <- "La probabilidad de valores t menores a _t_derecho_ es _probabilidad_."
          
        } else
          if(intervalo == "mayor"){
            
            if(is.na(var2)) stop("Falta ingresar var2")
            var_izquierdo <- var2
            var_derecho <- max(marcas_variable)
            
            t_izquierdo <- (var_izquierdo - media_muestral)/desvio_muestral
            t_derecho <- max_distribucion
            
            la_probabilidad <- pt(q = t_izquierdo, df = gl, lower.tail = F)
            el_porcentaje <- la_probabilidad*100
            
            frase01 <- "El valor de la variable original es _VariableIzquierdo_.<br/>
                        El valor estandarizado es t = _t_izquierdo_."
            
            frase02 <- "La probabilidad de pacientes con valores de la variable original mayores a _VariableIzquierdo_ es _probabilidad_.<br/>
                        La probabilidad de pacientes con valores t mayores a _t_izquierdo_ es _probabilidad_."
            
            frase03 <- "El valor estandarizado es t = _t_izquierdo_."
            
            frase04 <- "La probabilidad de valores t mayores a _t_izquierdo_ es _probabilidad_."
            
          } else
            if(intervalo == "entre"){
              
              if(is.na(var3)) return(NULL)
              if(is.na(var4)) return(NULL)
              
              var_izquierdo <- var3
              var_derecho <- var4
              
              t_izquierdo <- (var_izquierdo - media_muestral)/desvio_muestral
              t_derecho <- (var_derecho - media_muestral)/desvio_muestral
              
              p_der <- pt(q = t_derecho, df = gl, lower.tail = T)
              p_izq <- pt(q = t_izquierdo, df = gl, lower.tail = T)
              
              la_probabilidad <- p_der - p_izq
              el_porcentaje <- la_probabilidad*100
              
              frase01 <- "Los valores de la variable original son _VariableIzquierdo_ y _VariableDerecho_.<br/>
                          Los valores estandarizados son _t_izquierdo_ y _t_derecho_.
                          "
              
              frase02 <- "La probabilidad de pacientes con valores de la variable original entre _VariableIzquierdo_ y _VariableDerecho_ es _probabilidad_.<br/>
                          La probabilidad de pacientes con valores t entre _t_izquierdo_ y _t_derecho_ es _probabilidad_."
              
              frase03 <- "Los valores estandarizados son t1 = _t_izquierdo_ y t2 = _t_derecho_."
              
              frase04 <- "La probabilidad contenida entre los valores estadarizados t de _t_izquierdo_ y _t_derecho_ es _probabilidad_."
            }
      } else
        if(opciones == "valor_t") {
          if(intervalo == "menor"){
            
            if(is.na(t1)) return(NULL)
            
            t_izquierdo <- min_distribucion
            t_derecho <- as.numeric(as.character(t1))
            
            var_izquierdo <- t_izquierdo*desvio_muestral + media_muestral
            var_derecho <- t_derecho*desvio_muestral + media_muestral
            
            la_probabilidad <- pt(q = t_derecho, df = gl, lower.tail = T)
            el_porcentaje <- la_probabilidad*100
            
            frase01 <- "El valor estandarizado es t = _t_derecho_.<br/>
                        El valor de la variable original es _VariableDerecho_."
            
            frase02 <- "La probabilidad de pacientes con valores t menores a _t_derecho_ es _probabilidad_.<br/>
                        La probabilidad de pacientes con valores de la variable original menores a _VariableDerecho_ es _probabilidad_."
            
            frase03 <- "El valor estandarizado es t = _t_derecho_."
            
            frase04 <- "La probabilidad de valores t menores a _t_derecho_ es _probabilidad_."
            
          } else
            if(intervalo == "mayor"){
              
              if(is.na(t2)) return(NULL)
              
              t_izquierdo <- t2
              t_derecho <- max_distribucion
              
              var_izquierdo <- t_izquierdo*desvio_muestral + media_muestral
              var_derecho <- t_derecho*desvio_muestral + media_muestral
              
              la_probabilidad <- pnorm(t_izquierdo, mean = 0, sd = 1, lower.tail = F)
              el_porcentaje <- la_probabilidad*100
              
              frase01 <- "El valor estandarizado es t = _t_izquierdo_.<br/>
                          El valor de la variable original es _VariableIzquierdo_."
              
              frase02 <- "La probabilidad de pacientes con valores t mayores a _t_izquierdo_ es _probabilidad_.<br/>
                          La probabilidad de pacientes con valores de la variable original mayores a _VariableIzquierdo_ es _probabilidad_."
              
              frase03 <- "El valor estandarizado es t = _t_izquierdo_."
              
              frase04 <- "La probabilidad de valores t mayores a _t_izquierdo_ es _probabilidad_."
              
            } else
              if(intervalo == "entre"){
                
                if(is.na(t3)) return(NULL)
                if(is.na(t4)) return(NULL)
                
                t_izquierdo <- t3
                t_derecho <- t4
                
                var_izquierdo <- t_izquierdo*desvio_muestral + media_muestral
                var_derecho <- t_derecho*desvio_muestral + media_muestral
                
                p_der <- pnorm(q = t_derecho, df = gl, lower.tail = T)
                p_izq <- pnorm(q = t_izquierdo, df = gl, lower.tail = T)
                
                la_probabilidad <- p_der - p_izq
                el_porcentaje <- la_probabilidad*100
                
                frase01 <- "Los valores estandarizados son t1 = _t_izquierdo_ y t2 = _t_derecho_.<br/>
                            Los valores de la variable original son _VariableIzquierdo_ y _VariableDerecho_."
                
                frase02 <- "La probabilidad de pacientes con valores t entre _t_izquierdo_ y _t_derecho_ es _probabilidad_.<br/>
                            La probabilidad de pacientes con valores de la variable original entre _VariableIzquierdo_ y _VariableDerecho_ es _probabilidad_." 
                
                frase03 <- "Los valores estandarizados son t1 = _t_izquierdo_ y t2 = _t_derecho_."
                
                frase04 <- "La probabilidad contenida entre los valores estadarizados t de _t_izquierdo_ y _t_derecho_ es _probabilidad_."
              }
        } else
          if(opciones == "probabilidad") {
            if(intervalo == "menor"){
              
              if(is.na(probabilidad_externo)) return(NULL)
              
              la_probabilidad <- as.numeric(as.character(probabilidad_externo))
              
              t_izquierdo <- min_distribucion
              t_derecho <- qt(p = la_probabilidad, df = gl, lower.tail = T)
              
              var_izquierdo <- t_izquierdo*desvio_muestral + media_muestral
              var_derecho <- t_derecho*desvio_muestral + media_muestral
              
              el_porcentaje <- la_probabilidad*100
              
              frase01 <- "La probabilidad es _probabilidad_.<br/>
                          El porcentaje es _porcentaje_%."
              
              frase02 <- "El valor estandarizado t = _t_derecho_ acumula hacia la izquierda un valor de probabilidad de _probabilidad_.<br/>
                          El valor de la variable original _VariableDerecho_ acumula hacia la izquierda un valor de probabilidad de _probabilidad_."
              
              frase03 <- frase01
              
              frase04 <- "El valor estandarizado t = _t_derecho_ acumula hacia la izquierda un valor de probabilidad de _probabilidad_."
              
            } else
              if(intervalo == "mayor"){
                
                if(is.na(probabilidad_externo)) return(NULL)
                
                la_probabilidad <- probabilidad_externo
                
                t_izquierdo <- qnorm(probabilidad_externo, mean = 0, sd = 1, lower.tail = F)
                t_derecho <- max_distribucion
                
                var_izquierdo <- t_izquierdo*desvio_muestral + media_muestral
                var_derecho <- t_derecho*desvio_muestral + media_muestral
                
                el_porcentaje <- la_probabilidad*100
                
                frase01 <- "La probabilidad es _probabilidad_.<br/>
                            El porcentaje es _porcentaje_%."
                
                frase02 <- "El valor estandarizado t = _t_izquierdo_ acumula hacia la derecha una probabilidad de _probabilidad_.<br/>
                            El valor de la variable original _VariableIzquierdo_ acumula hacia la derecha una probabilidad de _probabilidad_."
                
                frase03 <- frase01
                
                frase04 <- "El valor estandarizado t = _t_izquierdo_ acumula hacia la derecha una probabilidad de _probabilidad_."
                
              } else
                if(intervalo == "entre"){
                  
                  if(is.na(probabilidad_externo)) return(NULL)
                  la_probabilidad <- probabilidad_externo
                  el_resto <- 1 - la_probabilidad
                  la_mitad <- el_resto/2
                  
                  t_izquierdo <- qt(p = la_mitad, df = gl, lower.tail = T)
                  t_derecho <- qt(p = la_mitad, df = gl, lower.tail = F)
                  
                  var_izquierdo <- t_izquierdo*desvio_muestral + media_muestral
                  var_derecho <- t_derecho*desvio_muestral + media_muestral
                  
                  el_porcentaje <- la_probabilidad*100
                  
                  frase01 <- "La probabilidad es _probabilidad_.<br/>
                              El porcentaje es _porcentaje_%."
                  
                  frase02 <- "Entre los valores estandarizados t1 = _t_izquierdo_ y t2 = _t_derecho_ se define una probabilidad de _probabilidad_.<br/>
                              Entre los valores de la variable original _VariableIzquierdo_ y _VariableDerecho_ se define una probabilidad de _probabilidad_."
                  
                  frase03 <- frase01
                  
                  frase04 <- "Entre los valores estandarizados t1 = _t_izquierdo_ y t2 = _t_derecho_ se define una probabilidad de _probabilidad_."
                  
                }
          } else
            if(opciones == "porcentaje") {
              if(intervalo == "menor"){
                
                if(is.na(porcentaje_externo)) return(NULL)
                
                el_porcentaje <- porcentaje_externo
                la_probabilidad <- el_porcentaje/100
                
                t_izquierdo <- min_distribucion
                t_derecho <- qt(p = la_probabilidad, df = gl, lower.tail = T)
                
                var_izquierdo <- t_izquierdo*desvio_muestral + media_muestral
                var_derecho <- t_derecho*desvio_muestral + media_muestral
                
                el_porcentaje <- la_probabilidad*100
                
                frase01 <- "El porcentaje es _porcentaje_.<br/>
                            La probabilidad es _probabilidad_.<br/>
                            El valor estandarizado es t = _t_derecho_.<br/>
                            El valor de la variable original es _VariableDerecho_.
                           "
                
                frase02 <- "La probabilidad de pacientes con valores de la variable original menores a _VariableDerecho_ es _probabilidad_.<br/>
                            La probabilidad de pacientes con valores t menores a _t_derecho_ es _probabilidad_."
                
                frase03 <- "El porcentaje es _porcentaje_.<br/>
                            La probabilidad es _probabilidad_.<br/>
                            El valor estandarizado es t = _t_derecho_.<br/>
                           "
                frase04 <- "La probabilidad de pacientes con valores t menores a _t_derecho_ es _probabilidad_."
                
              } else
                if(intervalo == "mayor"){
                  
                  if(is.na(porcentaje_externo)) return(NULL)
                  
                  el_porcentaje <- porcentaje_externo
                  la_probabilidad <- el_porcentaje/100
                  
                  t_izquierdo <- qt(p = probabilidad_externo, df = gl, lower.tail = F)
                  t_derecho <- max_distribucion
                  
                  var_izquierdo <- t_izquierdo*desvio_muestral + media_muestral
                  var_derecho <- t_derecho*desvio_muestral + media_muestral
                  
                  el_porcentaje <- la_probabilidad*100
                  
                  frase01 <- "El porcentaje es _porcentaje_.<br/>
                              La probabilidad es _probabilidad_.<br/>
                              El valor estandarizado es t = _t_izquierdo_.<br/>
                              El valor de la variable original es _VariableIzquierdo_."
                  
                  frase02 <- "La probabilidad de pacientes con valores de la variable original mayores a _VariableIzquierdo_ es _probabilidad_.<br/>
                              La probabilidad de pacientes con valores t mayores a _t_izquierdo_ es _probabilidad_."
                  
                  frase03 <- "El porcentaje es _porcentaje_.<br/>
                              La probabilidad es _probabilidad_.<br/>
                              El valor estandarizado es t = _t_izquierdo_."
                  
                  frase04 <- "La probabilidad de pacientes con valores t mayores a _t_izquierdo_ es _probabilidad_."
                  
                  
                } else
                  if(intervalo == "entre"){
                    
                    if(is.na(porcentaje_externo)) return(NULL)
                    
                    el_porcentaje <- porcentaje_externo
                    la_probabilidad <- el_porcentaje/100
                    el_resto <- 1 - la_probabilidad
                    la_mitad <- el_resto/2
                    
                    t_izquierdo <- qt(p = la_mitad, df = gl, lower.tail = T)
                    t_derecho <- qt(p = la_mitad, df = gl, lower.tail = F)
                    
                    var_izquierdo <- t_izquierdo*desvio_muestral + media_muestral
                    var_derecho <- t_derecho*desvio_muestral + media_muestral
                    
                    el_porcentaje <- la_probabilidad*100
                    
                    frase01 <- "El porcentaje es _porcentaje_.<br/>
                                La probabilidad es _probabilidad_.<br/>
                                Los valores estandarizados son t1 = _t_izquierdo_ y t2 = _t_derecho_.<br/>
                                Los valores de la variable original son _VariableIzquierdo_ y _VariableDerecho_."
                    
                    frase02 <- "La probabilidad de pacientes con valores de la variable original entre _VariableIzquierdo_ y _VariableDerecho_ es _probabilidad_.<br/>
                                La probabilidad de pacientes con valores t entre _t_izquierdo_ y _t_derecho_ es _probabilidad_." 
                    
                    frase03 <- "El porcentaje es _porcentaje_.<br/>
                                La probabilidad es _probabilidad_.<br/>
                                Los valores estandarizados son t1 = _t_izquierdo_ y t2 = _t_derecho_."
                    
                    frase04 <- "La probabilidad de pacientes con valores t entre _t_izquierdo_ y _t_derecho_ es _probabilidad_." 
                    
                    
                  }
            }
      
      
      Pattern <- c("_VariableIzquierdo_", "_VariableDerecho_", "_t_izquierdo_", "_t_derecho_",
                   "_probabilidad_", "_porcentaje_")
      
      Replacement <- c(var_izquierdo, var_derecho, t_izquierdo, t_derecho,
                       la_probabilidad, el_porcentaje)
      
      Replacement <- round2(Replacement, decimals)
      
      frase01 <- stringi::stri_replace_all_fixed(str = frase01,
                                                 pattern = Pattern,
                                                 replacement = Replacement,
                                                 vectorize_all = F)
      
      frase02 <- stringi::stri_replace_all_fixed(str = frase02,
                                                 pattern = Pattern,
                                                 replacement = Replacement,
                                                 vectorize_all = F)
      
      frase03 <- stringi::stri_replace_all_fixed(str = frase03,
                                                 pattern = Pattern,
                                                 replacement = Replacement,
                                                 vectorize_all = F)
      
      frase04 <- stringi::stri_replace_all_fixed(str = frase04,
                                                 pattern = Pattern,
                                                 replacement = Replacement,
                                                 vectorize_all = F)
    }
    
    
    
    las_columnas <- c("Variable", "t", "Probabilidad", "Porcentaje", 
                      "Frase01", "Frase02", "Frase03", "Frase04")
    las_filas <- c("Izquierda", "Derecha")
    
    armado <- as.data.frame(matrix(NA, length(las_filas), length(las_columnas)))
    colnames(armado) <- las_columnas
    rownames(armado) <- las_filas
    
    armado$"Variable" <- c(var_izquierdo, var_derecho)
    armado$"t" <- c(t_izquierdo, t_derecho)
    armado$"Probabilidad" <- c(la_probabilidad, la_probabilidad)
    armado$"Porcentaje" <- c(el_porcentaje, el_porcentaje)
    
    armado <- round2(armado, decimals)
    
    armado$"Frase01"  <- c(frase01, frase01)
    armado$"Frase02"  <- c(frase02, frase02)
    armado$"Frase03"  <- c(frase03, frase03)
    armado$"Frase04"  <- c(frase04, frase04)
    
    tabla_interna02 <- armado
  }
  
  # Tabla Externa 01
  {
    tabla <- tabla_interna01
    tabla <- tabla[c(1:4)]
    
    # tabla <- as.matrix(tabla)
    colnames(tabla) <- c("Media Muestral", "Varianza Muestral", "Desvío Muestral",
                         "Grados de Libertad")
    tabla <- round2(tabla, decimals)
    
    # tabla[,1] <- as.character(tabla[,1])
    
    tabla_externa01 <- tabla
    remove(tabla)
  }
  
  
  # Tabla Externa02 
  {
    tabla <- tabla_interna02
    tabla <- tabla[,-c(ncol(tabla):(ncol(tabla)-3))] # Quitamos las frases01 y frases02
    colnames(tabla)[3] <- c("Probabilidad del intervalo")
    colnames(tabla)[4] <- c("Porcentaje del intervalo")
    tabla[,4] <- paste0(tabla[,4], "%")
    
    # tabla <- as.matrix(tabla)
    # tabla[,1] <- as.character(tabla[,1])
    
    if(intervalo == "menor"){ 
      tabla <- as.data.frame(tabla[2,])
      #return(tabla)
      
    }else
      if(intervalo == "mayor"){ 
        tabla <- as.data.frame(tabla[1,])
        #return(tabla)
        
      }else
        if(intervalo == "entre"){
          
          Posicion <- c("Izquierdo", "Derecho")
          tabla <- cbind(Posicion, tabla)
          tabla[2,4] <- " "
          tabla[2,5] <- " "
          # return(tabla)
        }
    
    
    # tabla <- round2(tabla, decimals)
    
    tabla_externa02 <- tabla
    remove(tabla)
    
  }
  
  # Frase01, Frase02, Frase03 y Frase04
  {
    frase01 <- tabla_interna02$Frase01[1]
    frase02 <- tabla_interna02$Frase02[1]
    frase03 <- tabla_interna02$Frase03[1]
    frase04 <- tabla_interna02$Frase04[1]
  }
  
  
  # grafico01 y grafico02
  {
    
    # media_muestral <- tabla_interna01[1,1]
    # desvio_muestral <- tabla_interna01[1,3]
    # Parametros
    # media_muestral    <- 0
    # desvio_muestral <- 1
    
    color_variable <- tabla_interna01$"color_variable"
    decimals <- decimals
    
    t_izquierdo <- tabla_interna02$"t"[1] 
    t_derecho <- tabla_interna02$"t"[2]
    
    var_izquierdo <- tabla_interna02$"Variable"[1] 
    var_derecho <- tabla_interna02$"Variable"[2]
    
    
    
    
    # Normal Completa
    min_distribucion <- -4
    max_distribucion <- 4
    h <- (max_distribucion - min_distribucion)/10000
    
    marcas_t <- min_distribucion:max_distribucion
    marcas_variable <- (marcas_t*desvio_muestral) + media_muestral
    marcas_variable <- round2(marcas_variable, decimals)
    
    
    # # Rango Acotado
    # lower.x <- -0.0
    # upper.x <-  2.1
    lower.x <- t_izquierdo
    upper.x <-  t_derecho
    
    # La Campana 
    x  <- seq(from = min_distribucion, to = max_distribucion, by = h)
    y <- dt(x = x, df = gl)
    data_distribucion <- data.frame(x = x, y = y)
    
    # Lineas Limites
    linea_izquierda.x <- c(lower.x, lower.x)
    linea_izquierda.y <- c(0, dt( x = lower.x, df = gl))
    data_linea_izquierda <- data.frame(x = linea_izquierda.x, y = linea_izquierda.y)
    
    linea_derecha.x <- c(upper.x, upper.x)
    linea_derecha.y <- c(0, dt( x = upper.x, df = gl))
    data_linea_derecha <- data.frame(x = linea_derecha.x, y = linea_derecha.y)
    
    
    # El poligono
    x_mod <- seq(lower.x, upper.x, by = h)
    y_mod  <- dt(x = x_mod, df = gl)
    data_poligono <- data.frame(x = c(lower.x, x_mod, upper.x), y = c(0, y_mod, 0))
    
    
    
    
    # http://www.sthda.com/english/wiki/ggplot2-axis-ticks-a-guide-to-customize-tick-marks-and-labels
    grafico_general <- ggplot(data = data_distribucion, 
                              aes(x, y, 
                                  xmin = min_distribucion, 
                                  xmax = max_distribucion, 
                                  ymin = 0, 
                                  ymax = 0.5)) + 
      theme_bw() +
      theme(axis.line = element_line(color='black'),
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank()) +
      geom_line(size = 2) + 
      ylab("Frecuencia Relativa") +
      # scale_x_continuous(name = "Variable Z", breaks = marcas_t, labels = marcas_t) +
      geom_polygon(data = data_poligono, fill = color_variable, size=2) + 
      geom_line(data = data_linea_izquierda, colour = color_variable, size=2) + 
      geom_line(data = data_linea_derecha,   colour = color_variable, size=2) + 
      geom_line(size=2)+ theme(text = element_text(size = 20))    
    
    grafico01 <- grafico_general +
      scale_x_continuous(name = "Variable t", breaks = marcas_t, labels = marcas_t)
    
    grafico02 <- grafico_general +
      scale_x_continuous(name = "Variable Original", breaks = marcas_t, labels =marcas_variable)
    

  }
  
  
  # Salida General
  General <- Hmisc::llist(tabla_interna01, tabla_interna02, tabla_externa01, tabla_externa02,
                          frase01, frase02, frase03, frase04, grafico01, grafico02)
  
  RMedic <- Hmisc::llist(tabla_externa01, tabla_externa02, frase01, frase02,
                         frase03, frase04, grafico01, grafico02)
  
  out <- Hmisc::llist(General, RMedic)
  return(out)
}
