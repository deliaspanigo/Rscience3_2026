
################


RMedic_1q_tablas <- function(input_base = NULL, input_decimales = NULL, input_cadena = NULL) {
  
  # Default values
  if (is.null(input_decimales)) input_decimales <- 2
  if (is.null(input_cadena)) input_cadena <- T
  
  # # # Control 1 - input_base
  {
    ###
    # 1- Es nulo
    # 2- No es un data.frame
    # 3- Tiene cero columnas
    # 4- Tienes más de 1 columna
    # 5- No tiene filas (nrow(input_base))
    
    veredicto1 <- control_1q(input_base = input_base, input_cadena = input_cadena)
    output_cadena <- veredicto1
    
    ###  
  } # Fin Control 1 - input_base
  #################################################
  
  
  
  # # # minibase y Control 2 -  base "NO DATA"
  {
    ###
    
    # Si todo va OK...
    if (output_cadena) {
      
      # Creamos "mini"  
      minibase <- na.omit(input_base)
      
      # Vemos que mini tenga filas
      if (nrow(minibase) == 0) {
        cat("Error df01: 'input_base' posee solo celdas vacias.", "\n")
        output_cadena <- FALSE
      }
      
      
      
    } # Fin if Si todo va OK...
    ###################################################
    
    # Si hay algun problema...
    if (output_cadena == FALSE){
      minibase <- as.data.frame(mtcars[2])
      if(!is.factor(minibase[1])) minibase[1] <- as.factor(as.character(minibase[,1]))
      colnames(minibase) <- "No Data"
      cat("Error df01: 'minibase' externo agregado (NO DATA)", "\n")
    } # Fin if si hay problemas
    ##########################################################
    
    
    ### 
  } # Fin Control 2 -  base "NO DATA"
  ################################################################
  
  
  # # # SandBox en caso de no ser valido
  {
    ###
    
    # Si hay algun problema...
    if (output_cadena == FALSE){
      minibase <- as.data.frame(mtcars[2])
      if(!is.factor(minibase[1])) minibase[1] <- as.factor(as.character(minibase[,1]))
      
      colnames(minibase) <- "No Data"
      cat("Error df01: 'minibase' externo agregado (NO DATA)", "\n")
    } # Fin if si hay problemas
    ##########################################################
    
    ###  
  }
  ############################################################################
  
  
  # # # Cohersion como as.factor()
  {
    ###
    
    # Si todo va OK...
    if (output_cadena) {
      
      # Si cumple todo... Falta que sea de tipo factor.
      if(!is.factor(minibase[,1])) minibase[,1] <- as.factor(as.character(minibase[,1]))
      
      lvl1 <- sort(levels(input_base[,1]))
      lvl2 <- sort(levels(minibase[,1]))
      
      if(identical(lvl1, lvl2)) minibase[,1] <- factor(minibase[,1], levels = levels(input_base[,1]))
      
      
    } # Fin if Si todo va OK...
    ###################################################
    
    
    
    
    ### 
  } # Fin Cohersion como as.factor()
  ################################################################
  
  
  
  
  
  
  
  # Tabla 1: Distribucion de Frecuencias
  {
    ###
    
    # Frecuencais Absolutas
    fa <- table(minibase)
    
    # Total Absoluto
    n_total <- sum(fa)
    grupos <- as.character(names(fa))
    salida_n_total <- rep(n_total, length(fa))
    
    # Cociente
    cociente <- paste0(fa, "/", n_total)
    
    # Frecuencias Relativas
    fr <- fa/n_total
    #    fr <- round2((fa/n_total), input_decimales)
    
    # Porcentajes
    porcentaje  <- fr*100
    
    
    # Redondeos y otros detalles (Ahora si!)
    fr <- round2(fr, input_decimales)
    porcentaje <- round2(porcentaje, input_decimales)
    porcentaje  <- paste(porcentaje, "%", sep="")
    
    # Fusion
    fusion <- paste0(fa, " (", porcentaje, ")")
    
    # Tabla de Distribucion de Frecuencias
    tabla01 <- cbind(grupos, fa, salida_n_total, cociente, fr,  
                     porcentaje, fusion)
    
    rotulo <- paste0("Variable: ", colnames(minibase))
  #  tabla01 <- as.data.frame(tabla01)
    tabla01 <- as.matrix(tabla01)
    nombres_tabla01 <- c(rotulo, "Frecuencia Absoluta", "Total", "Cociente", "Frecuencia Relativa", "Porcentaje", "FA (%)")
    colnames(tabla01) <- nombres_tabla01
    
    numericas <- c(2, 3, 5)
    for(n in 1:length(numericas)) tabla01[,numericas[n]] <- as.numeric(as.character(tabla01[,numericas[n]])) 
    
    
    
    ###
  } # Fin Tabla 1
  ##################################
  
  
  # Tabla 2: Intervalos de Confianza
  {
    ###
    
    p <- fr
    q <- rep(NA, length(p))
    for (n in 1:length(q)) q[n] <- sum(p[-n])
    n <- n_total  
    
    alfa <- c(0.10, 0.05, 0.01)
    
    
    tablas2_ic <- list()
    
    for (k in 1:length(alfa)) {
      
      este_alfa <- alfa[k]
      este_alfa_2 <- este_alfa/2
      confianza <- paste0((1 - este_alfa)*100, "%")
      
      Z <- qnorm(1-este_alfa_2)
      Z <- round2(Z, input_decimales)
      
      desvio <- Z*sqrt((p*q)/n)
      # desvio <- round2(desvio, input_decimales)
      
      li_prop <- p - desvio
      ls_prop <- p + desvio
      
      dt_li <- li_prop < 0
      dt_ls <- ls_prop > 1
      
      if (sum(dt_li) > 0) li_prop[dt_li] <- 0
      if (sum(dt_ls) > 0) ls_prop[dt_ls] <- 1
      
      
      li_porcentaje <- li_prop*100
      ls_porcentaje <- ls_prop*100
      
      li_porcentaje <- round2(li_porcentaje, input_decimales)
      ls_porcentaje <- round2(ls_porcentaje, input_decimales)
      
      li_porcentaje <- paste0(li_porcentaje, "%")
      ls_porcentaje <- paste0(ls_porcentaje, "%")
      
      tabla02 <- cbind(grupos, porcentaje, li_porcentaje, ls_porcentaje)
      colnames(tabla02) <- c(rotulo, "Porcentaje", paste0("Límite Inferior ", confianza), paste0("Límite Superior ", confianza))
      tabla02 <- as.data.frame(tabla02)
      rownames(tabla02) <- rownames(tabla01)
      
      
      if (output_cadena == FALSE) {
        
        estas_dimensiones <- dim(tabla02)
        tabla_no_data02 <- as.data.frame(matrix("Sin datos en la Columna", estas_dimensiones[1], estas_dimensiones[2]))
        colnames(tabla_no_data02) <- colnames(tabla02)
        tabla02 <- tabla_no_data02
        
      }
      
      tablas2_ic[[k]] <- tabla02
      
    }
    
    
    
    ###
  } # Fin Tabla 2
  ##################################
  
  
  
  # Mis Tablas
  {
    ###
    mis_tablas <- list(tabla01, tablas2_ic[[1]], tablas2_ic[[2]], tablas2_ic[[3]])
    names(mis_tablas) <- c("Distribución de Frecuencias", 
                           "Intervalos de Confianza del 90% para el Porcentaje",
                           "Intervalos de Confianza del 95% para el Porcentaje", 
                           "Intervalos de Confianza del 99% para el Porcentaje")
    
    ###    
  }
  ##########################################################################
  
  
  
  
  
  
  # # # Cambios "NO DATA" o "Errores"
  {
    ###
    
    
    # Si hay errores... Y estamos en "NO DATA"
    if (output_cadena == FALSE) {
      
      
      # Cambiamos los valores por avisos
      for (n in 1:length(mis_tablas)) {
        
        esta_tabla <- mis_tablas[[n]]
        estas_dimensiones <- dim(esta_tabla)
        tabla_no_data <- as.data.frame(matrix("Sin datos", estas_dimensiones[1], estas_dimensiones[2]))
        colnames(tabla_no_data) <- colnames(esta_tabla)
        esta_tabla <- tabla_no_data
        
        mis_tablas[[n]]  <- esta_tabla
        
      } # Fin for n
      
    } # Fin if si hay errores...
    #############################################################
    
    
    ###   
  } # Fin Cambios "NO DATA" o "Errores"
  ##################################################
  
  
  
  # # # Salida
  {
    ###
    
    return(mis_tablas)
    
    
    ###  
  } # Fin Salida
  ##################
  
  
  
}

########################




control_1q <- function(input_base = NULL, input_cadena = NULL){
  
  # # # Explicacion
  {
    ###
    # La funcion "control_1q()" es una funcion de control sobre input_base.
    # Verifica que input_base sea un dataframe de solo una columna, con al menos una fila.
    # Otorgara avisos si el objeto input_base:
    # 1- Es nulo
    # 2- No es un data.frame
    # 3- Tiene cero columnas
    # 4- Tienes más de 1 columna
    # 5- No tiene filas (nrow(input_base))
    
    # Aclaramos que el punto 5 es ver las filas que tiene input_base, y no 
    # ver si las filas ingresadas son vacias o no.
    
    
    # Si se cumple todo, la funcion devuelve un "TRUE". 
    # Si al menos un detalle falta, devuelve un "FALSE".
    # El control se realiza si el objeto input_cadena es TRUE.
    # Si input_cadena es FALSE quiere decir que ya hay un paso previo de una funcion
    # externa que no se cumple, y por lo tanto carece de sentido realizar las tareas, devolviendo
    # directamente un FALSE control_1q() como resultado.
    
    ###    
  } # Fin Explicacion
  ############################################################
  
  
  # # # Funcionamiento por defecto
  {
    ###
    
    if (is.null(input_cadena)) input_cadena <- TRUE
    
    
    
    
    # Armamos el siguiente eslabon
    output_cadena <- input_cadena
    
    ###
  } # Fin Funcionamiento
  ################################################
  
  
  # # # Controles 1
  {
    ###
    
    # Verificar que input_base no sea nulo
    if(output_cadena && is.null(input_base)) {
      cat("Error control_1q: input_base no debe ser nulo", "\n")
      output_cadena <- FALSE
    }  
    
    # Verificar si es un data frame
    if(output_cadena && !is.data.frame(input_base)) {
      cat("Error control_1q: input_base debe ser un data.frame", "\n")
      output_cadena <- FALSE
    }
    
    # Verificar si tiene al menos una columna
    if(output_cadena && ncol(input_base) == 0) {
      cat("Error control_1q: input_base no tiene columnas.")
      output_cadena <- FALSE
    }
    
    # Verificar si tiene mas de 1 columna
    if(output_cadena && ncol(input_base) > 1) {
      cat("Error control_1q: input_base debe ser solo una columna")
      output_cadena <- FALSE
    }
    
    # Verificar si es no tiene datos
    if(output_cadena && nrow(input_base) == 0) {
      stop("Error control_1q: input_base no presenta filas")
      output_cadena <- FALSE
    }
    
    
    
    ###      
  } # Fin Controles 1
  ###################################################################
  
  # # # Salida
  {
    ###
    
    salida <- output_cadena
    
    names(salida) <- c("output_cadena")
    
    return(salida)
    
    
    ###  
  } # Fin Salida
  ##################
  
  
}


################



round2 <- function(x, n) { 
  posneg <- sign(x) 
  
  z <- abs(x)*10^n 
  z <- z + 0.5 
  z <- trunc(z) 
  z <- z/10^n 
  z*posneg 
} 
#############

CifrasPerfectas <- function(cifras = NULL, digitos = 2){
  
  cifras_perfectas <- rep(NA, length(cifras))
  
  for (k in 1:length(cifras)){
    
    cantidad_cifras <- nchar(cifras[k])
    
    cifras_extra <- digitos - cantidad_cifras
    
    el_extra <- rep(0, cifras_extra)
    
    cifras_perfectas[k] <- paste0(el_extra, cifras[k])
  }
  
  return(cifras_perfectas)
}

#######################


RMedic_1c_tablas <- function(input_base = NULL, input_decimales = NULL, input_cadena = NULL,
                             input_min = NULL, input_max = NULL, input_breaks = NULL,
                             input_side = NULL) {
  
  # Default values
  if (is.null(input_decimales)) input_decimales <- 2
  if (is.null(input_cadena)) input_cadena <- T
  
  
  
  # # # Control 1 - input_base
  {
    ###
    # 1- Es nulo
    # 2- No es un data.frame
    # 3- Tiene cero columnas
    # 4- Tienes más de 1 columna
    # 5- No tiene filas (nrow(input_base))
    
    veredicto1 <- control_1c(input_base = input_base, input_cadena = input_cadena)
    output_cadena <- veredicto1
    
    ###  
  } # Fin Control 1 - input_base
  #################################################
  
  
  
  # # # minibase y Control 2 -  base "NO DATA"
  {
    ###
    
    # Si todo va OK...
    if (output_cadena) {
      
      # Creamos "minibase"  
      minibase <- na.omit(input_base)
      mini_vector <- minibase[,1]
      
      # Vemos que minibase tenga filas
      if (nrow(minibase) == 0) {
        cat("Error df01: 'input_base' posee solo celdas vacias.", "\n")
        output_cadena <- FALSE
      }
      
      # minibase sea numerica
      if (!is.numeric(minibase[,1])) {
        cat("Error df01: 'input_base' debe ser numerico.", "\n")
        output_cadena <- FALSE
      }
      
      
    } # Fin if Si todo va OK...
    ###################################################
    
    # Si hay algun problema...
    if (output_cadena == FALSE){
      minibase <- as.data.frame(mtcars[1])
      colnames(minibase) <- "No Data"
      cat("Error df01: 'minibase' externo agregado (NO DATA)", "\n")
    } # Fin if si hay problemas
    ##########################################################
    
    
    ### 
  } # Fin Control 2 -  base "NO DATA"
  ################################################################
  
  
  # # # SandBox en caso de no ser valido
  {
    ###
    
    # Si hay algun problema...
    if (output_cadena == FALSE){
      minibase <- as.data.frame(mtcars[2])
      mini_vector <- minibase[,1]
      minibase[1] <- as.factor(as.character(minibase[,1]))
      colnames(minibase) <- "No Data"
      cat("Error df01: 'minibase' externo agregado (NO DATA)", "\n")
    } # Fin if si hay problemas
    ##########################################################
    
    ###  
  }
  ############################################################################
  
  
  # More default values
  {
    ###
    
    if(is.null(input_min))  input_min <- min(mini_vector)
    if(is.null(input_max))  input_max <- max(mini_vector)
    if(is.null(input_breaks))  input_breaks <- nclass.Sturges(mini_vector)
    if(is.null(input_side))  input_side <- T
    # 
    ###
  } # Fin More default values
  #################################################
  
  
  # Tabla 1 - Medidas Resumen
  {
    ###
    
    nombres_elementos <- c("Variable", "Media", "Desvío Estándard", "n")
    tabla1 <- as.data.frame(matrix(NA,1, length(nombres_elementos)))
    colnames(tabla1) <- nombres_elementos
    
    
    # Media
    media <- mean(mini_vector)
    media <- round2(media, input_decimales)
    
    # Maximo
    desvio <- sd(mini_vector)
    desvio <- round2(desvio, input_decimales)
    
    # Cantidad de Datos
    n_muestra <- length(mini_vector)
    
    
    tabla1[1,1] <- colnames(minibase)
    tabla1[1,2] <- media
    tabla1[1,3] <- desvio
    tabla1[1,4] <- n_muestra
    
    ###
  } # Fin Tabla 1 - Medidas Resumen
  ############################################################
  
  
  # Tabla 2 - Medidas Posicion
  {
    ###
    
    
    nombres_elementos <- c("Variable", "Mínimo", "Media", "Mediana", "Máximo", "n")
    tabla2 <- as.data.frame(matrix(NA,1, length(nombres_elementos)))
    colnames(tabla2) <- nombres_elementos
    
    
    
    minimo <- min(mini_vector)
    minimo <- round2(minimo, input_decimales)
    
    # Media
    media <- mean(mini_vector)
    media <- round2(media, input_decimales)
    
    # Mediana
    mediana <- median(mini_vector)
    mediana <- round2(mediana, input_decimales)
    
    
    # Maximo
    maximo <- max(mini_vector)
    maximo <- round2(maximo, input_decimales)
    
    # Cantidad de Datos
    n_muestra <- length(mini_vector)
    
    
    tabla2[1,1] <- colnames(minibase)
    tabla2[1,2] <- minimo
    tabla2[1,3] <- media
    tabla2[1,4] <- mediana
    tabla2[1,5] <- maximo
    tabla2[1,6] <- n_muestra
    
    ###
  } # Fin Tabla 2 - Medidas Posicion
  ############################################################
  
  
  # Tabla 3 - Cuartiles
  {
    
    input_busqueda = c(25, 50, 75)
    nombres_columnas <- c("Variable", paste0(c("Q1", "Q2", "Q3"), 
                                             paste0("(", input_busqueda, "%", ")")), "n")
    
    tabla3 <- as.data.frame(matrix(NA, 1, length(nombres_columnas)))
    colnames(tabla3) <- nombres_columnas
    
    percentiles <- quantile(mini_vector,  probs = input_busqueda/100)
    
    tabla3[1,1] <- colnames(minibase)
    tabla3[1, c(2:(ncol(tabla3)-1))] <- percentiles
    tabla3[1,ncol(tabla3)] <- nrow(minibase)
    
  } # End Tabla 3 - Cuartiles
  ###########################################################
  
  
  # Tabla 4 - Deciles
  {
    
    input_busqueda <- c(10, 20, 30, 40, 50, 60, 70, 80, 90)
    nombres_columnas <- c("Variable", paste0( "D",input_busqueda, " (", input_busqueda, "%)"), "n")
    
    tabla4 <- as.data.frame(matrix(NA, 1, length(nombres_columnas)))
    colnames(tabla4) <- nombres_columnas
    
    percentiles <- quantile(mini_vector,  probs = input_busqueda/100)
    
    tabla4[1,1] <- colnames(minibase)
    tabla4[1, c(2:(ncol(tabla4)-1))] <- percentiles
    tabla4[1,ncol(tabla4)] <- nrow(minibase)
    
  } # End Tabla 4 - Deciles
  ###########################################################
  
  
  # Tabla 5 - Percentiles
  {
    
    input_busqueda <- c(1, 5, 10, 25, 50, 75, 90, 95, 99)
    nombres_columnas <- c("Variable", paste0( "P",input_busqueda, " (", input_busqueda, "%)"), "n")
    
    tabla5 <- as.data.frame(matrix(NA, 1, length(nombres_columnas)))
    colnames(tabla5) <- nombres_columnas
    
    percentiles <- quantile(mini_vector,  probs = input_busqueda/100)
    
    tabla5[1,1] <- colnames(minibase)
    tabla5[1, c(2:(ncol(tabla5)-1))] <- percentiles
    tabla5[1,ncol(tabla5)] <- nrow(minibase)
    
  } # End Tabla 5 - Percentiles
  ###########################################################
  
  
  
  # Tabla 6 - Medidas Dispersion 
  {
    ###
    
    
    nombres_elementos <- c("Variable", "Rango", "Varianza", "Desvío Estándard", 
                           "Error Estándard", "Coeficiente de Variación",
                           "Coeficiente de Variación Porcentual", "n")
    
    tabla6 <- as.data.frame(matrix(NA,1, length(nombres_elementos)))
    colnames(tabla6) <- nombres_elementos
    
    # rango
    rango <- abs(max(mini_vector) - min(mini_vector))
    rango <- round2(rango, input_decimales)
    
    # Varianza
    varianza <- var(mini_vector)
    varianza <- round2(varianza, input_decimales)
    
    # Desvio
    desvio <- sd(mini_vector)
    
    # Tamanio muestral
    n_muestra <- length(mini_vector)
    
    # Error estandard
    ee <- desvio/sqrt(n_muestra)
    
    # Media
    media <- mean(mini_vector)
    
    # Coeficiente de Variacion
    cv <- desvio/media
    
    cv_porcentual <- cv*100
    
    # Primero sacamos el desvio...
    # Sin redondear el desvio, lo usamos para sacar el error estandard...
    # Y luego redondeamos los dos.
    # Esto es para sacar mejor al EE... por que sino sacas el DE... lo redondeas...
    # lo usas para sacar el EE y lo volves a redondear.
    # Lo mismo con el CV.
    
    desvio <- round2(desvio, input_decimales)
    ee <- round2(ee, input_decimales)
    cv <- round2(cv, input_decimales)
    cv_porcentual <- paste0(round2(cv_porcentual, input_decimales), "%")
    
    
    tabla6[,1] <- colnames(minibase)
    tabla6[,2] <- rango
    tabla6[,3] <- varianza
    tabla6[,4] <- desvio
    tabla6[,5] <- ee
    tabla6[,6] <- cv
    tabla6[,7] <- cv_porcentual
    tabla6[,8] <- n_muestra
    
    ###
  } # Fin Tabla 6 - Medidas Dispersion 
  ############################################################
  
  
  # Tabla 7 - Desviaciones
  {
    ###
    
    
    nombres_elementos <- c("Variable", "Rango Intercuartílico", "Desviación Intercuartílica", "n")
    
    tabla7 <- as.data.frame(matrix(NA,1, length(nombres_elementos)))
    colnames(tabla7) <- nombres_elementos
    
    
    Q1 <- quantile(mini_vector,  probs = 25/100)
    Q3 <- quantile(mini_vector,  probs = 75/100)
    
    RI <- Q3 - Q1
    DI <- RI/2
    
    # Redondeos
    RI <- round2(RI, input_decimales) 
    DI <- round2(DI, input_decimales) 
    
    
    # Tamanio muestral
    n_muestra <- length(mini_vector)
    
    
    
    
    tabla7[,1] <- colnames(minibase)
    tabla7[,2] <- RI
    tabla7[,3] <- DI
    tabla7[,4] <- n_muestra
    
    
    ###
  } # Fin Tabla 7 - Desviaciones
  ############################################################
  
  
  
  
  
  # Tabla 8: Intervalos de Confianza
  {
    ###
    
    
    alfa <- c(0.10, 0.05, 0.01)
    nombres_columnas <- c("Variable", "Media", "Confianza", "Límite Inferior IC", "Límite Superior IC", "n")
    tabla8 <- as.data.frame(matrix(NA, length(alfa), length(nombres_columnas)))
    colnames(tabla8) <- nombres_columnas
    
    for (n in 1:nrow(tabla8)) {
      
      este_alfa <- alfa[n]
      este_alfa_2 <- este_alfa/2
      gl <- n_muestra - 1
      
      
      desvio <- sd(mini_vector)
      desvio <- round2(desvio, input_decimales)
      
      t_li <- qt(este_alfa_2, df = gl, lower.tail = TRUE)
      t_ld <- qt((1-este_alfa_2), df = gl, lower.tail = TRUE)
      
      brazo <- t_ld*(desvio/sqrt(n_muestra))
      brazo <- round2(brazo, input_decimales)
      
      media_li <- media - brazo
      media_ld <- media + brazo
      
      tabla8[n, 1] <- colnames(input_base)
      tabla8[n, 2] <- media
      tabla8[n, 3] <- paste0((1-este_alfa)*100, "%")
      tabla8[n, 4] <- media_li
      tabla8[n, 5] <- media_ld
      tabla8[n, 6] <- n_muestra
      
    } # Fin for n
    
    
    
    
    
    
    
    ###  
  } # Fin Tabla 8
  #############################################################
  
  
  
  
  # Tabla 9: Tabla de Frecuencias
  {
    ###
    
    diferencia <- input_max - input_min
    rango <- diferencia/input_breaks
    
    cortes <- input_min + c(0:input_breaks)*rango
    
    # VerificacioN!
    # Pasa que si la variable es constante, hay que hacer
    # una sola categoria con todo. La funcion de R aunque
    # la variable sea constante, te tira mas de 1 intervalo.
    # Eso esta mal.
    tabla <- table(mini_vector)
    cantidad_categorias <- length(names(tabla))
    cantidad_cortes <- length(cortes)
    if(cantidad_categorias < cantidad_cortes) cortes <- cantidad_categorias
    
    if(cantidad_categorias > 1) {
      info <- cut(mini_vector, breaks = cortes , right = input_side,
                  include.lowest = T)
      
    } else info <- mini_vector
    
    # Cambio de niveles
    levels(info) <- gsub("[,]", " ; ", levels(info))
    
    dim(info) <- c(length(info), 1)
    info <- as.data.frame(info)
    colnames(info) <- colnames(input_base)
    
    #  tabla9 <- RMedic_1q_tablas(input_base = info, input_decimales = input_decimales)[[1]]
    
    # Pasamos la ultima fila como primera
    #  if(input_side) tabla9 <- tabla9[c(nrow(tabla9), (1:(nrow(tabla9)-1))), ]
    
    # Frecuencais Absolutas
    fa <- table(info)
    
    # Total Absoluto
    n_total <- sum(fa)
    grupos <- as.character(names(fa))
    salida_n_total <- rep(n_total, length(fa))
    
    # Cociente
    cociente <- paste0(fa, "/", n_total)
    
    # Frecuencias Relativas
    fr <- (fa/n_total)
    fr_guardado <- fr
    fr <- round2(fr, input_decimales)
    
    
    
    # Porcentajes
    porcentaje <- fr_guardado*100
    porcentaje <- round2(porcentaje, input_decimales)
    porcentaje  <- paste(porcentaje, "%", sep="")
    
    # Fusion
    fusion <- paste0(fa, " (", porcentaje, ")")
    
    # Tabla de Distribucion de Frecuencias
    tabla9 <- cbind(grupos, fa, salida_n_total, cociente, fr,
                    porcentaje, fusion)
    
    rotulo <- paste0("Variable: ", colnames(info))
    tabla9 <- as.data.frame(tabla9)
    nombres_tabla9 <- c(rotulo, "Frecuancia Absoluta", "Total", "Cociente", "Frecuencia Relativa", "Porcentaje", "FA (%)")
    colnames(tabla9) <- nombres_tabla9
    
    numericas <- c(2, 3, 5)
    for(n in 1:length(numericas)) tabla9[,numericas[n]] <- as.numeric(as.character(tabla9[,numericas[n]]))
    
    ###
  } # Fin Tabla 9 Tabla de Frecuencias
  ######################################################
  
  
  # Mis Tablas
  {
    ###
    mis_tablas <- list(tabla1, tabla2, tabla3, tabla4, tabla5, tabla6, tabla7, 
                       tabla8, tabla9)
    
    # ,  tabla8)
    
    names(mis_tablas) <- c("Medidas Resumen", 
                           "Medidas de Posición",
                           "Cuartiles",
                           "Deciles",
                           "Percentiles",
                           "Medidas de Dispersión", 
                           "Desviaciones",
                           "Intervalo de Confianza (IC) para la media",
                           "Distribución de Frecuencias")
    
    ###    
  }
  ##########################################################################
  
  
  
  
  
  
  # # # Cambios "NO DATA" o "Errores"
  {
    ###
    
    
    # Si hay errores... Y estamos en "NO DATA"
    if (output_cadena == FALSE) {
      
      
      # Cambiamos los valores por avisos
      for (n in 1:length(mis_tablas)) {
        
        esta_tabla <- mis_tablas[[n]]
        estas_dimensiones <- dim(esta_tabla)
        tabla_no_data <- as.data.frame(matrix("Sin datos", estas_dimensiones[1], estas_dimensiones[2]))
        colnames(tabla_no_data) <- colnames(esta_tabla)
        esta_tabla <- tabla_no_data
        
        mis_tablas[[n]]  <- esta_tabla
        
      } # Fin for n
      
    } # Fin if si hay errores...
    #############################################################
    
    
    ###   
  } # Fin Cambios "NO DATA" o "Errores"
  ##################################################
  
  
  
  # # # Salida
  {
    ###
    
    return(mis_tablas)
    
    
    ###  
  } # Fin Salida
  ##################
  
  
  
}


############


control_1c <- function(input_base = NULL, input_cadena = NULL){
  
  # # # Funcionamiento por defecto
  {
    ###
    
    if (is.null(input_cadena)) input_cadena <- TRUE
    
    
    
    
    # Armamos el siguiente eslabon
    output_cadena <- input_cadena
    
    ###
  } # Fin Funcionamiento
  ################################################
  
  
  # # # Controles 1
  {
    ###
    
    # Verificar que input_base no sea nulo
    if(output_cadena && is.null(input_base)) {
      cat("\n", "Error control_1c: input_base no debe ser nulo", "\n")
      output_cadena <- FALSE
    }  
    
    # Verificar si es un data frame
    if(output_cadena && !is.data.frame(input_base)) {
      cat("\n", "Error control_1c: input_base debe ser un data.frame", "\n")
      output_cadena <- FALSE
    }
    
    # Verificar si tiene al menos una columna
    if(output_cadena && ncol(input_base) == 0) {
      cat("\n", "Error control_1c: input_base no tiene columnas")
      output_cadena <- FALSE
    }
    
    # Verificar si tiene mas de 1 columna
    if(output_cadena && ncol(input_base) > 1) {
      cat("\n", "Error control_1c: input_base debe ser solo una columna")
      output_cadena <- FALSE
    }
    
    # Verificar si es no tiene datos
    if(output_cadena && nrow(input_base) == 0) {
      cat("\n", "Error control_1c: input_base no presenta filas")
      output_cadena <- FALSE
    }
    
    # Verificar que sea numerica
    if(output_cadena && !is.numeric(input_base[,1])) {
      cat("\n", "Error control_1c: input_base debe ser numérica.", "\n", "Utilice la solapa 'Control' sobre esta variable.", "\n")
      output_cadena <- FALSE
    }
    
    
    ###      
  } # Fin Controles 1
  ###################################################################
  
  # # # Salida
  {
    ###
    
    salida <- output_cadena
    
    names(salida) <- c("output_cadena")
    
    return(salida)
    
    
    ###  
  } # Fin Salida
  ##################
  
  
}


##############






RMedic_2q_tablas <- function(input_base = NULL, input_decimales = NULL, input_cadena = NULL) {
  
  
  # Input originales 
  {
    ###
    
    input_originales <- list(input_decimales, input_cadena)
    names(input_originales) <- c("input_decimales", "input_cadena")
    
    ###    
  } # Fin Argumentos originales
  ##############################################################################
  
  
  # # # Funcionamiento por defecto 
  {
    ###
    
    # Funcionamiento por defecto
    if (is.null(input_cadena)) input_cadena <- TRUE
    
    # Decimales por defecto
    if (is.null(input_decimales)) input_decimales <- 2
    
    
    # Armamos el siguiente eslabon
    output_cadena <- input_cadena
    
    ###
  } # Fin Funcionamiento
  ##########################################################
  
  
  # # # Control 1 - input_base
  {
    ###
    veredicto1 <- control_2q(input_base = input_base, input_cadena = output_cadena)
    output_cadena <- veredicto1
    
    ###  
  } # Fin Control 1 - input_base
  #################################################
  
  
  # # # Control 2 - input_decimales
  {
    ###
    
    # Hacemos el control de decimales
    veredicto2 <- control_decimales(input_decimales = input_decimales, input_cadena = output_cadena)
    
    # Si no pasa el control numerico, asignamos un nuevo valor para input_decimales
    # pero guardamos el original por si hace falta
    if (veredicto2 == FALSE){
      input_decimales_original <- input_decimales
      input_decimales <- 2
    }
    
    # Si paso el control anterior... guardamos esta nueva opinion
    if (veredicto1) output_cadena <- veredicto2
    
    ###  
  } # Fin Control 2 - input_decimales
  ############################################################################
  
  
  # # # Modificaciones, Controles 2, y base "NO DATA"
  {
    ###
    
    # Si todo va OK...
    if (output_cadena) {
      
      # Si cumple todo... Falta cada columna sea de tipo factor.
      if (is.factor(input_base[,1]) == FALSE) input_base[,1] <- as.factor(as.character(input_base[,1]))
      if (is.factor(input_base[,2]) == FALSE) input_base[,2] <- as.factor(as.character(input_base[,2]))
      
      # Creamos "mini"  
      mini <- na.omit(input_base)
      
      # Vemos que mini tenga filas
      if (nrow(mini) == 0) {
        cat("Error df02: 'mini' sin filas", "\n")
        output_cadena <- FALSE
      }
      
    } # Fin if Si todo va OK...
    ###########################
    
    # Si no hay datos o tienen errores...
    if (output_cadena == FALSE){
      mini <- as.data.frame(mtcars[c(8,2)])
      mini[,1] <- as.factor(as.character(mini[,1]))
      mini[,2] <- as.factor(as.character(mini[,2]))
      colnames(mini) <- "No Data"
      cat("Error df02: 'mini' externo agregado (NO DATA)", "\n")
    }
    ### 
  } # Fin Modificaciones
  ################################################################    
  
  
  # # # Resolucion
  {
    ###
    
    # 1) Marginales al total
    {
      ###
      
      
      
      # Detalles Iniciales y fa
      fa <- table(mini)
      n_total <- sum(fa)
      fa_interno <- fa
      m_col <- colSums(fa)
      m_row <- rowSums(fa)
      
      # fa marginal
      fa_marginal <- cbind(fa, m_row)
      fa_marginal <- rbind(fa_marginal, c(m_col, n_total))
      colnames(fa_marginal)[ncol(fa_marginal)] <- c("Total por Filas")
      rownames(fa_marginal)[nrow(fa_marginal)] <- c("Total por Columnas")
      
      # Cociente al Total con marginales
      cociente_marginal <- fa_marginal
      for(n in 1:ncol(cociente_marginal)) cociente_marginal[,n] <- paste0(cociente_marginal[,n], "/", n_total)
      
      
      # Frecuencias Relativas al Total con Marginales
      fr_marginal <- fa_marginal/n_total
      fr_guardado <- fr_marginal
      fr_marginal <- round2(fr_marginal, input_decimales)
      
      # Si es "NaN" le ponemos un cero... Hay que hacerlo dos veces
      for (n in 1:ncol(fr_marginal)) if (is.nan(fr_marginal[n])) fr_marginal[n,] <- rep(0, nrow(fr_marginal))
      for (k in 1:nrow(fr_marginal)) if (is.nan(fr_marginal[k])) fr_marginal[,k] <- rep(0, ncol(fr_marginal))
      
      m_col2 <- fr_marginal[nrow(fr_marginal),-ncol(fr_marginal)]
      m_row2 <- fr_marginal[-nrow(fr_marginal),ncol(fr_marginal)]
      totales_raros <- c(sum(m_col2), sum(m_row2))
      diferencia <- abs(totales_raros - 1)
      dt_diferencia <- max(diferencia) == diferencia
      orden_diferencia <- c(1,2)[dt_diferencia]
      if (length(orden_diferencia) == 2) orden_diferencia <- orden_diferencia[1]
      
      
      # En "fr"... los marginales al total por filas y columnas deben sumar 1 en cada caso.
      # Por temas de redondeo... puede que uno sea igual a 1 y el otro no... o que ambos sean diferentes de 1.
      # En ese caso deberiamos avisarle que las sumas marginales no estan dando como debieran... y que
      # debe cambiar los decimales para traajar...
      
      # # Deteccion de redondeo incorrecto
      fr_interno <- fr_marginal
      # if (input_aviso == TRUE) if (totales_raros[orden_diferencia] != 1) fr_marginal[nrow(fr_marginal), ncol(fr_marginal)] <- paste0(fr_marginal[nrow(fr_marginal), ncol(fr_marginal)], "(Redondeo Incorrecto)")  
      
      # Porcentajes al Total con marginales
      # # porcentaje_marginal <- fr_interno
      porcentaje_marginal <- fr_guardado
      porcentaje_marginal <- porcentaje_marginal*100
      porcentaje_marginal <- round2(porcentaje_marginal, input_decimales)
      #dim(PORCENTAJE) <- dim(FR_interno)
      
      porcentaje2_marginal <- porcentaje_marginal
      for(n in 1:ncol(porcentaje2_marginal)) for (k in 1:nrow(porcentaje2_marginal)) {
        porcentaje2_marginal[k,n] <- paste(porcentaje2_marginal[k,n], "%", sep="")
      }
      
      # Guardamos unos porcentajes internos...
      porcentaje_interno <- porcentaje_marginal
      
      # # Si tuvo problemas con las "fr" dejamos constancia tambien en la tabla de porcentajes
      # if (input_aviso == TRUE) if (totales_raros[orden_diferencia] != 1) porcentaje_marginal[nrow(porcentaje_marginal), ncol(porcentaje_marginal)] <- paste0(porcentaje_marginal[nrow(porcentaje_marginal), ncol(porcentaje_marginal)], "(Redondeo Incorrecto)")  
      
      # Fusion marginal
      fusion_marginal <- matrix(NA, nrow(fa_marginal), ncol(fa_marginal))
      colnames(fusion_marginal) <- colnames(fa_marginal)
      rownames(fusion_marginal) <- rownames(fa_marginal)
      for (k in 1:nrow(fusion_marginal)) fusion_marginal[k,] <- paste0(fa_marginal[k,], " (", porcentaje2_marginal[k,], ")")
      
      
      ###
    } # Fin Marginales al total
    ###################################################################################
    
    
    # 2) Todo por filas
    {
      
      # FA por filas
      fa_filas <- fa
      total_filas <- rowSums(fa)
      fa_filas <- cbind(fa_filas, total_filas)
      colnames(fa_filas)[ncol(fa_filas)] <- "Total por Filas"
      
      # Cociente por filas
      cociente_filas <- fa_filas
      for (n in 1:nrow(cociente_filas)) cociente_filas[n,] <- paste0(cociente_filas[n,], "/", total_filas[n]) 
      
      # "fr" por filas
      fr_filas <- fa_filas
      fr_filas_interno <- fr_filas
      
      for (n in 1:nrow(cociente_filas)) {
        
        fr_filas[n, ] <- fr_filas[n, ]/total_filas[n]
        
        if (total_filas[n] == 0) fr_filas[n, ] <- rep(0, length(fr_filas[n, ]))
        
        fr_filas_interno[n, ] <- fr_filas[n, ]
        fr_filas[n, ] <- round2(fr_filas[n, ], input_decimales)
      }
      
      
      totales_fr_filas <- fr_filas[,ncol(fr_filas)]
      dt_fr_filas <- totales_fr_filas != 1
      #    if (input_aviso == TRUE) fr_filas[dt_fr_filas] <- paste0(fr_filas[dt_fr_filas], "(Redondeo Incorrecto)")
      
      
      
      # "porcentaje" por filas
      porcentaje_filas <- fr_filas_interno*100
      porcentaje_filas <- round2(porcentaje_filas, input_decimales)
      
      porcentaje2_filas <-  porcentaje_filas
      for (n in 1:nrow(porcentaje2_filas)) porcentaje2_filas[n,] <- paste0(porcentaje2_filas[n,], "%")
      #     porcentaje_filas[dt_fr_filas] <- paste0(porcentaje_filas[dt_fr_filas], "(Redondeo Incorrecto)")
      
      # Fusion por filas
      fusion_filas <- matrix(NA, nrow(fa_filas), ncol(fa_filas))
      colnames(fusion_filas) <- colnames(fa_filas)
      rownames(fusion_filas) <- rownames(fa_filas)
      for (k in 1:nrow(fusion_filas)) fusion_filas[k,] <- paste0(fa_filas[k,], " (", porcentaje2_filas[k,], ")")
      
      
      ###
    } # Todo por filas
    ###################################################################
    
    
    # 3) Todo por columnas
    {
      ###
      
      
      # FA por columnas
      fa_columnas <- fa
      total_columnas <- colSums(fa)
      fa_columnas <- rbind(fa_columnas, total_columnas)
      rownames(fa_columnas)[nrow(fa_columnas)] <- "Total por columnas"
      
      # Cociente por columnas
      cociente_columnas <- fa_columnas
      for (n in 1:ncol(cociente_columnas)) cociente_columnas[,n] <- paste0(cociente_columnas[,n], "/", total_columnas[n]) 
      
      # "fr" por columnas
      fr_columnas <- fa_columnas
      fr_columnas_interno <- fr_columnas
      
      for (n in 1:ncol(cociente_columnas)) {
        
        fr_columnas[,n ] <- fr_columnas[,n ]/total_columnas[n]
        if (total_columnas[n] == 0) fr_columnas[,n ] <- rep(0, length(fr_columnas[,n ]))
        fr_columnas_interno[,n ] <- fr_columnas[,n ]
        fr_columnas[,n ] <- round2(fr_columnas[,n ], input_decimales)
      }
      
      
      totales_fr_columnas <- fr_columnas[nrow(fr_columnas),]
      dt_fr_columnas <- totales_fr_columnas != 1
      #    if (input_aviso == TRUE) fr_columnas[dt_fr_columnas] <- paste0(fr_columnas[dt_fr_columnas], "(Redondeo Incorrecto)")
      
      
      
      # "porcentaje" por columnas
      porcentaje_columnas <- fr_columnas_interno*100
      porcentaje_columnas <- round2(porcentaje_columnas, input_decimales)
      
      porcentaje2_columnas <- porcentaje_columnas
      for (n in 1:ncol(porcentaje2_columnas)) porcentaje2_columnas[,n] <- paste0(porcentaje2_columnas[,n], "%")
      #    if (input_aviso == TRUE)  porcentaje_columnas[dt_fr_columnas] <- paste0(porcentaje_columnas[dt_fr_columnas], "(Redondeo Incorrecto)")
      
      
      # Fusion por columnas
      fusion_columnas <- matrix(NA, nrow(fa_columnas), ncol(fa_columnas))
      colnames(fusion_columnas) <- colnames(fa_columnas)
      rownames(fusion_columnas) <- rownames(fa_columnas)
      for (k in 1:nrow(fusion_columnas)) fusion_columnas[k,] <- paste0(fa_columnas[k,], " (", porcentaje2_columnas[k,], ")")
      
      
      ###
    } # Todo por columnas
    #################################################
    
    
    # 4) Clasico
    {
      ###
      
      # FA Clasico  
      fa_clasico <- fa_marginal[-nrow( fa_marginal), - ncol( fa_marginal)]
      if(is.null(dim(fa_clasico))) {
        
        dim(fa_clasico) <- c((nrow(fa_marginal)-1), (ncol(fa_marginal)-1)) 
        fa_clasico <- as.data.frame(fa_clasico)
        rownames(fa_clasico) <- rownames(fa_marginal)[-nrow(fa_marginal)]
        colnames(fa_clasico) <- colnames(fa_marginal)[-ncol(fa_marginal)]
      }    
      
      # Cociente Clasico
      cociente_clasico <- cociente_marginal[-nrow( fa_marginal), - ncol( fa_marginal)]    
      if(is.null(dim(cociente_clasico))) {
        
        dim(cociente_clasico) <- c((nrow(fa_marginal)-1), (ncol(fa_marginal)-1)) 
        cociente_clasico <- as.data.frame(cociente_clasico)
        rownames(cociente_clasico) <- rownames(fa_marginal)[-nrow(fa_marginal)]
        colnames(cociente_clasico) <- colnames(fa_marginal)[-ncol(fa_marginal)]
      }   
      
      # FR Clasico
      fr_clasico <- fr_marginal[-nrow( fa_marginal), - ncol( fa_marginal)] 
      if(is.null(dim(fr_clasico))) {
        
        dim(fr_clasico) <- c((nrow(fa_marginal)-1), (ncol(fa_marginal)-1)) 
        fr_clasico <- as.data.frame(fr_clasico)
        rownames(fr_clasico) <- rownames(fa_marginal)[-nrow(fa_marginal)]
        colnames(fr_clasico) <- colnames(fa_marginal)[-ncol(fa_marginal)]
      }  
      
      # %
      porcentaje_clasico <- porcentaje_marginal[-nrow( fa_marginal), - ncol( fa_marginal)] 
      if(is.null(dim(porcentaje_clasico))) {
        
        dim(porcentaje_clasico) <- c((nrow(fa_marginal)-1), (ncol(fa_marginal)-1)) 
        porcentaje_clasico <- as.data.frame(porcentaje_clasico)
        rownames(porcentaje_clasico) <- rownames(fa_marginal)[-nrow(fa_marginal)]
        colnames(porcentaje_clasico) <- colnames(fa_marginal)[-ncol(fa_marginal)]
      }  
      
      # %%
      porcentaje2_clasico <- porcentaje2_marginal[-nrow( fa_marginal), - ncol( fa_marginal)] 
      if(is.null(dim(porcentaje2_clasico))) {
        
        dim(porcentaje2_clasico) <- c((nrow(fa_marginal)-1), (ncol(fa_marginal)-1)) 
        porcentaje2_clasico <- as.data.frame(porcentaje2_clasico)
        rownames(porcentaje_clasico) <- rownames(fa_marginal)[-nrow(fa_marginal)]
        colnames(porcentaje_clasico) <- colnames(fa_marginal)[-ncol(fa_marginal)]
      }  
      
      # Fusion
      fusion_clasico <- matrix(NA, nrow(fa_clasico), ncol(fa_clasico))
      colnames(fusion_clasico) <- colnames(fa_clasico)
      rownames(fusion_clasico) <- rownames(fa_clasico)
      for (k in 1:nrow(fusion_clasico)) fusion_clasico[k,] <- paste0(fa_clasico[k,], " (", porcentaje2_clasico[k,], ")")
      ###  
    } # Clasico
    ##################################
    
    
    ###
  } # Fin Resolucion
  ##################################################################
  
  
  # # # Objetos Intermedios
  {
    ###  
    
    #   input_columnas <- c(2,5)  
    
    # Una funcion propia...
    # Para convertir en "character" algunas columnas de varias tablas en una lista
    char_machine <- function(input_lista = NULL, input_columnas = NULL) {
      
      for (k in 1:length(input_columnas)) {
        for (n in 1:ncol(input_lista[[input_columnas[k]]])) {
          
          input_lista[[input_columnas[k]]][,n] <- as.character(input_lista[[input_columnas[k]]][,n])
          
        } # Fin for n
      } # Fin for k
      
      
      return(input_lista)
    } 
    
    
    
    
    
    # Todo lo CLASICO 
    {
      ###
      
      CLASICO <- list()
      CLASICO[[1]] <- as.data.frame(fa_clasico)
      CLASICO[[2]] <- as.data.frame(cociente_clasico)
      CLASICO[[3]] <- as.data.frame(fr_clasico)
      # ELIMINAR! # CLASICO[[4]] <- as.data.frame(porcentaje_clasico) 
      CLASICO[[4]] <- as.data.frame(porcentaje2_clasico)
      CLASICO[[5]] <- fusion_clasico
      names(CLASICO) <- c("Frecuencias Absolutas",
                          "Cociente al total", 
                          "Frecuencias Relativas al total",
                          "Porcentajes al total",
                          "Fusión (Frecuencia Absoluta y Porcentaje)")
      #   CLASICO <- char_machine(CLASICO, input_columnas)
      
      ###
    } # Fin Todo lo CLASICO
    ############################################################################
    
    
    # Todo al TOTAL
    {
      ###
      
      TOTAL <- list()
      TOTAL[[1]] <- as.data.frame(fa_marginal)
      TOTAL[[2]] <- as.data.frame(cociente_marginal)
      TOTAL[[3]] <- as.data.frame(fr_marginal)
      # ELIMINAR! #TOTAL[[4]] <- as.data.frame(porcentaje_marginal)
      TOTAL[[4]] <- as.data.frame(porcentaje2_marginal)
      TOTAL[[5]] <- fusion_marginal
      names(TOTAL) <- c("Frecuencias Absolutas",
                        "Cociente al total", 
                        "Frecuencias Relativas al total",
                        "Porcentajes al total",
                        "Fusión (Frecuencia Absoluta y Porcentaje)")
      #  TOTAL <- char_machine(TOTAL, input_columnas)
      
      ###
    } # Todo al TOTAL
    ############################################################################
    
    
    
    # Todo por FILAS
    {
      ###
      
      
      FILAS <- list()
      FILAS[[1]] <- as.data.frame(fa_filas)
      FILAS[[2]] <- as.data.frame(cociente_filas)
      FILAS[[3]] <- as.data.frame(fr_filas)
      # ELIMINAR! # FILAS[[4]] <- as.data.frame(porcentaje_filas)
      FILAS[[4]] <- as.data.frame(porcentaje2_filas)
      FILAS[[5]] <- fusion_filas
      names(FILAS) <- c("Frecuencias Absolutas",
                        "Cociente por filas", 
                        "Frecuencias Relativas por filas",
                        "Porcentajes por filas",
                        "Fusión por filas (Frecuencia Absoluta y Porcentaje)")
      #  FILAS <- char_machine(FILAS, input_columnas)
      
      ###
    } # Todo por filas
    ############################################################################
    
    
    
    # Todo por COLUMNAS
    {
      ###
      
      COLUMNAS <- list()
      COLUMNAS[[1]] <- as.data.frame(fa_columnas)
      COLUMNAS[[2]] <- as.data.frame(cociente_columnas)
      COLUMNAS[[3]] <- as.data.frame(fr_columnas)
      # ELIMAR! # COLUMNAS[[4]] <- as.data.frame(porcentaje_columnas)
      COLUMNAS[[4]] <- as.data.frame(porcentaje2_columnas)
      COLUMNAS[[5]] <- fusion_columnas
      names(COLUMNAS) <- c("Frecuencias Absolutas",
                           "Cociente por columnas", 
                           "Frecuencias Relativas por columnas",
                           "Porcentajes por columnas",
                           "Fusión por columnas (Frecuencia Absoluta y Porcentaje)")   
      # COLUMNAS <- char_machine(COLUMNAS, input_columnas)
      
      ###
    } # Fin Todo por COLUMNAS
    ############################################################################
    
    
    # SIMPLE
    {
      ###
      SIMPLE <- list()
      
      grupos_filas <- rownames(fa)
      grupos_columnas <- colnames(fa)
      cantidad_filas <- length(grupos_filas)*length(grupos_columnas)
      nombres_columnas <- c(colnames(input_base), "Frecuencia Absoluta", 
                            "Total", "Cociente al Total", "Frecuencia Relativa al Total",
                            "Porcentaje al Total", "FA (%)")
      
      
      tabla_simple <- as.data.frame(matrix(NA, cantidad_filas, length(nombres_columnas)))
      colnames(tabla_simple) <- nombres_columnas
      
      contador_externo <- 0
      for (k in 1:length(grupos_filas)) {
        for (n in 1:length(grupos_columnas)) {
          contador_externo <- contador_externo + 1
          
          tabla_simple[contador_externo,1] <- grupos_filas[k]
          tabla_simple[contador_externo,2] <- grupos_columnas[n]        
          tabla_simple[contador_externo,3] <- TOTAL[[1]][k,n]    
          tabla_simple[contador_externo,4] <- sum(TOTAL[[1]]) 
          tabla_simple[contador_externo,5] <- TOTAL[[2]][k,n]       
          tabla_simple[contador_externo,6] <- TOTAL[[3]][k,n]       
          tabla_simple[contador_externo,7] <- TOTAL[[4]][k,n]  
          tabla_simple[contador_externo,8] <- TOTAL[[5]][k,n]
          # ELIMINAR! SIMPLE[contador_externo,8] <- TOTAL[[5]][k,n] 
          
        } # Fin for n
      } # Fin for k
      
      SIMPLE[[1]] <- tabla_simple
      # 
      # names(SIMPLE) <- "Simple Entrada"
      names(SIMPLE) <- "Simple Entrada"
      
      ###
    } # Fin SIMPLE
    ############################################################################
    
    
    
    
    
    # ARMADO ESPECIAL
    {
      
      # Hay demasiadas tablas... Estas son las que va a mirar
      # el usuario por defecto.
      ARMADO_ESPECIAL <- list()
      ARMADO_ESPECIAL[[1]] <- TOTAL[[1]]
      ARMADO_ESPECIAL[[2]] <- FILAS[[4]]
      ARMADO_ESPECIAL[[3]] <- FILAS[[5]]
      names(ARMADO_ESPECIAL) <- c("Frecuencias Absolutas y Marginales",
                                  "Porcentajes por filas",
                                  "Fusión por filas (Frecuencia Absoluta y Porcentaje)")
    }
    ########################################
    
    
    ###
  } # Fin Objetos Intermedios
  ##############################################################################
  
  
  # # # Mis Tablas y cambios "NO DATA" y "Errores"
  {
    ###
    
    mis_tablas <- list(ARMADO_ESPECIAL, CLASICO, TOTAL, FILAS, COLUMNAS, SIMPLE)
    names(mis_tablas) <- c("Resumen", "Clasico", "Al total", "Por filas", "Por columnas", "Simple Entrada")  
    
    
    
    
    ###  
  } # Fin Mis Tablas
  ########################################################
  
  
  # # # Cambios "NO DATA" o "Errores"
  {
    ###
    
    # Si no es valido trabajar... Y estamos en "NO DATA"
    if (output_cadena == FALSE) {
      
      cambio1 <- "Sin datos en la Columna"
      cambio2 <- "Modificar input_decimales"
      
      # Damos aviso si es algo de los datos o los decimales
      cambio_aplicado <- cambio1
      if (veredicto1 == TRUE && veredicto2 == FALSE) cambio_aplicado <- cambio2
      
      
      
      # Cambiamos los valores por avisos
      for (n in 1:length(mis_tablas)) for (k in 1:length(mis_tablas[[n]])) {
        
        esta_tabla <- mis_tablas[[n]][[k]]
        
        estas_dimensiones <- dim(esta_tabla)
        tabla_no_data <- as.data.frame(matrix(cambio_aplicado, estas_dimensiones[1], estas_dimensiones[2]))
        colnames(tabla_no_data) <- colnames(esta_tabla)
        esta_tabla <- tabla_no_data
        
        mis_tablas[[n]][[k]]  <- esta_tabla
        
      } # Fin for n
      
    } # Fin si no es valido trabajar
    
    ###   
  } # Fin Cambios "NO DATA" o "Errores"
  ##################################################
  
  
  # # # Objetos Finales
  {
    ###
    
    referencias <- list()
    referencias[[1]] <- paste0("En filas: ", colnames(mini)[1])
    referencias[[2]] <- paste0("En columnas: ", colnames(mini)[2])
    
    ###  
  } # Fin Objetos Finales
  ###############################
  
  
  # # # Salida
  {
    ###
    
    salida <- list(mis_tablas, referencias, output_cadena, input_originales)
    names(salida) <- c("df02", "referencias", "output_cadena", "input_originales")
    
    return(salida)
    
    
    ###  
  } # Fin Salida
  ##################
  
  
  
  
  
  
} # Fin function

###########################


control_2q <- function(input_base = NULL, input_cadena = NULL){
  
  # # # Funcionamiento por defecto
  {
    ###
    
    if (is.null(input_cadena)) input_cadena <- TRUE
    
    
    
    
    # Armamos el siguiente eslabon
    output_cadena <- input_cadena
    
    ###
  } # Fin Funcionamiento
  ################################################
  
  
  # # # Controles 1
  {
    ###
    
    # Verificar que input_base no sea nulo
    if(output_cadena && is.null(input_base)) {
      cat("Error control_2q: input_base no debe ser nulo", "\n")
      output_cadena <- FALSE
    }  
    
    # Verificar si es un data frame
    if(output_cadena && !is.data.frame(input_base)) {
      cat("Error control_2q: input_base debe ser un data.frame", "\n")
      output_cadena <- FALSE
    }
    
    # Verificar si tiene al menos una columna
    if(output_cadena && ncol(input_base) == 0) {
      cat("Error control_2q: input_base no tiene columnas")
      output_cadena <- FALSE
    }
    
    # Verificar si tiene mas de 2 columna
    if(output_cadena && ncol(input_base) != 2) {
      cat("Error control_2q: input_base debe ser dos columnas")
      output_cadena <- FALSE
    }
    
    # Verificar si es no tiene datos
    if(output_cadena && nrow(input_base) == 0) {
      stop("Error control_2q: input_base no presenta filas")
      output_cadena <- FALSE
    }
    
    
    
    ###      
  } # Fin Controles 1
  ###################################################################
  
  # # # Salida
  {
    ###
    
    salida <- output_cadena
    
    names(salida) <- c("output_cadena")
    
    return(salida)
    
    
    ###  
  } # Fin Salida
  ##################
  
  
}


#######################


control_decimales <- function(input_decimales = NULL, input_cadena = NULL){
  
  # # # Funcionamiento por defecto
  {
    ###
    
    if (is.null(input_cadena)) input_cadena <- TRUE
    
    
    
    
    # Armamos el siguiente eslabon
    output_cadena <- input_cadena
    
    ###
  } # Fin Funcionamiento
  ################################################
  
  
  # # # Controles  - input_decimales
  {
    ###
    
    # Verificar que input_decimales es un vector
    if(output_cadena && !is.vector(input_decimales)) {
      cat("Error input_decimales: input_decimales debe ser un vector", "\n")
      output_cadena <- FALSE
    }
    
    # Verificar que input_decimales es un solo elemento
    if(output_cadena && length(input_decimales) > 1) {
      cat("Error input_decimales: input_decimales debe ser un solo número", "\n")
      output_cadena <- FALSE
    }
    
    # Verificar que input_decimales es un numero
    if(output_cadena && is.numeric(input_decimales) == FALSE) {
      cat("Error input_decimales: input_decimales debe ser un número", "\n")
      output_cadena <- FALSE
    }
    
    # Verificar que input_decimales no es "NA"
    if(output_cadena && is.na(input_decimales)) {
      cat("Error input_decimales: input_decimales no debe ser 'NA'", "\n")
      output_cadena <- FALSE
    }
    
    
    # Verificar que input_decimales no es "NaN"
    if(output_cadena && is.nan(input_decimales)) {
      cat("Error input_decimales: input_decimales no debe ser 'NaN'", "\n")
      output_cadena <- FALSE
    }
    
    ###      
  } # Fin Controles 
  ###################################################################
  
  
  # # # Salida
  {
    ###
    
    salida <- output_cadena
    
    names(salida) <- c("output_cadena")
    
    return(salida)
    
    
    ###  
  } # Fin Salida
  ##################
  
  
}

#########################

control_2c <- function(input_base = NULL, input_cadena = NULL){
  
  # # # Funcionamiento por defecto
  {
    ###
    
    if (is.null(input_cadena)) input_cadena <- TRUE
    
    
    
    
    # Armamos el siguiente eslabon
    output_cadena <- input_cadena
    
    ###
  } # Fin Funcionamiento
  ################################################
  
  
  # # # Controles 1
  {
    ###
    
    # Verificar que input_base no sea nulo
    if(output_cadena && is.null(input_base)) {
      cat("\n", "Error control_2c: input_base no debe ser nulo", "\n")
      output_cadena <- FALSE
    }  
    
    # Verificar si es un data frame
    if(output_cadena && !is.data.frame(input_base)) {
      cat("\n", "Error control_2c: input_base debe ser un data.frame", "\n")
      output_cadena <- FALSE
    }
    
    # Verificar si tiene al menos una columna
    if(output_cadena && ncol(input_base) == 0) {
      cat("\n", "Error control_1c: input_base no tiene columnas")
      output_cadena <- FALSE
    }
    
    # Verificar si tiene solo 1 columna
    if(output_cadena && ncol(input_base) == 1) {
      cat("\n", "Error control_2c: input_base debe ser dos columnas.")
      output_cadena <- FALSE
    }
    
    # Verificar si tiene mas de 2 columna
    if(output_cadena && ncol(input_base) > 2) {
      cat("\n", "Error control_2c: input_base debe ser solo dos columnas.")
      output_cadena <- FALSE
    }
    
    # Verificar si es no tiene datos
    if(output_cadena && nrow(input_base) == 0) {
      cat("\n", "Error control_2c: input_base no presenta filas")
      output_cadena <- FALSE
    }
    
    # Verificar que sea numerica la columna 1
    if(output_cadena && !is.numeric(input_base[,1])) {
      cat("\n", "Error control_2c: la columna 1 de input_base debe ser numérica.", "\n", "Utilice la solapa 'Control' sobre esta variable.", "\n")
      output_cadena <- FALSE
    }
    
    # Verificar que sea numerica la columna 2
    if(output_cadena && !is.numeric(input_base[,2])) {
      cat("\n", "Error control_2c: la columna 2 de input_base debe ser numérica.", "\n", "Utilice la solapa 'Control' sobre esta variable.", "\n")
      output_cadena <- FALSE
    }
    
    ###      
  } # Fin Controles 1
  ###################################################################
  
  # # # Salida
  {
    ###
    
    salida <- output_cadena
    
    names(salida) <- c("output_cadena")
    
    return(salida)
    
    
    ###  
  } # Fin Salida
  ##################
  
  
}



##########################

RMedic_2c_tablas <- function(input_base = NULL, input_decimales = NULL, input_cadena = NULL,
                             input_min1 = NULL, input_max1 = NULL, input_breaks1 = NULL,
                             input_side1 = NULL,
                             input_min2 = NULL, input_max2 = NULL, input_breaks2 = NULL,
                             input_side2 = NULL) {
  
  
  
  
  # Medidas de posicion y dispersion simultaneas
  # Default values
  if (is.null(input_decimales)) input_decimales <- 2
  if (is.null(input_cadena)) input_cadena <- T
  
  
  
  
  # # # Control 1 - input_base
  {
    ###
    # 1- Es nulo
    # 2- No es un data.frame
    # 3- Tiene cero columnas
    # 4- Tiene 0 columnas
    # 5- Tiene 1 columna
    # 6- Tiene mas de 2 columnas
    # 7- No tiene filas (nrow(input_base))
    
    veredicto1 <- control_2c(input_base = input_base, input_cadena = input_cadena)
    output_cadena <- veredicto1
    
    ###  
  } # Fin Control 1 - input_base
  #################################################
  
  
  
  # # # minibase y Control 2 -  base "NO DATA"
  {
    ###
    
    # Si todo va OK...
    if (output_cadena) {
      
      # Creamos "minibase"  
      minibase_general <- na.omit(input_base)
      minibase1 <- minibase_general[1]
      minibase2 <- minibase_general[2]
      mini_vector1 <- minibase_general[,1]
      mini_vector2 <- minibase_general[,2]
      
      # Vemos que minibase tenga filas
      if (nrow(minibase_general) == 0) {
        cat("Error en RMedic_2c_tablas(): 'input_base' posee solo celdas vacias.", "\n")
        output_cadena <- FALSE
      }
      
      # minibase sea numerica
      if (!is.numeric(mini_vector1)) {
        cat("Error RMedic_2c_tablas(): La columna 1 de 'input_base' debe ser numerica.", "\n")
        output_cadena <- FALSE
      }
      
      # minibase sea numerica
      if (!is.numeric(mini_vector2)) {
        cat("Error RMedic_2c_tablas(): La columna 2 de 'input_base' debe ser numerica.", "\n")
        output_cadena <- FALSE
      }
      
    } # Fin if Si todo va OK...
    ###################################################
    
    # Si hay algun problema...
    if (output_cadena == FALSE){
      minibase_general <- mtcars[c(1,3)]
      minibase1 <- minibase_general[1]
      minibase2 <- minibase_general[2]
      mini_vector1 <- minibase_general[,1]
      mini_vector2 <- minibase_general[,2]
      colnames(minibase_general) <- c("No Data1", "No Data2")
      cat("Error en RMedic_2c_tablas(): 'minibase' externo agregado (NO DATA)", "\n")
    } # Fin if si hay problemas
    ##########################################################
    
    
    ### 
  } # Fin Control 2 -  base "NO DATA"
  ################################################################
  
  
  # # # SandBox en caso de no ser valido
  {
    ###
    
    # Si hay algun problema...
    if (output_cadena == FALSE){
      minibase_general <- mtcars[c(1,3)]
      minibase1 <- minibase_general[1]
      minibase2 <- minibase_general[2]
      mini_vector1 <- minibase_general[,1]
      mini_vector2 <- minibase_general[,2]
      colnames(minibase) <- c("No Data1", "No Data2")
      cat("Error en RMedic_2c_tablas(): 'minibase' externo agregado (NO DATA)", "\n")
    } # Fin if si hay problemas
    ##########################################################
    
    ###  
  }
  ############################################################################
  
  
  # More default values
  {
    ###
    
    # Todo lo de la variable 1
    if(is.null(input_min1))  input_min1 <- min(mini_vector1)
    if(is.null(input_max1))  input_max1 <- max(mini_vector1)
    if(is.null(input_breaks1))  input_breaks1 <- nclass.Sturges(mini_vector1)
    if(is.null(input_side1))  input_side1 <- T
    
    # Todo lo de la variable 2
    if(is.null(input_min2))  input_min2 <- min(mini_vector2)
    if(is.null(input_max2))  input_max2 <- max(mini_vector2)
    if(is.null(input_breaks2))  input_breaks2 <- nclass.Sturges(mini_vector2)
    if(is.null(input_side2))  input_side2 <- T
    # 
    ###
  } # Fin More default values
  #################################################
  
  {
  diferencia1 <- input_max1 - input_min1
  rango1 <- diferencia1/input_breaks1
  
  cortes1 <- input_min1 + c(0:input_breaks1)*rango1
  
  # VerificacioN!
  # Pasa que si la variable es constante, hay que hacer
  # una sola categoria con todo. La funcion de R aunque
  # la variable sea constante, te tira mas de 1 intervalo.
  # Eso esta mal.
  tabla1 <- table(mini_vector1)
  cantidad_categorias1 <- length(names(tabla1))
  cantidad_cortes1 <- length(cortes1)
  if(cantidad_categorias1 < cantidad_cortes1) cortes1 <- cantidad_categorias1
  
  if(cantidad_categorias1 > 1) {
    info1 <- cut(mini_vector1, breaks = cortes1 , right = input_side1,
                include.lowest = T)
    
  } else info1 <- mini_vector1
  
  dim(info1) <- c(length(info1), 1)
  info1 <- as.data.frame(info1)
  colnames(info1) <- colnames(input_base)[1]
  }  
  
  {
    diferencia2 <- input_max2 - input_min2
    rango2 <- diferencia2/input_breaks2
    
    cortes2 <- input_min2 + c(0:input_breaks2)*rango2
    
    # VerificacioN!
    # Pasa que si la variable es constante, hay que hacer
    # una sola categoria con todo. La funcion de R aunque
    # la variable sea constante, te tira mas de 1 intervalo.
    # Eso esta mal.
    tabla2 <- table(mini_vector2)
    cantidad_categorias2 <- length(names(tabla2))
    cantidad_cortes2 <- length(cortes2)
    if(cantidad_categorias2 < cantidad_cortes2) cortes2 <- cantidad_categorias2
    
    if(cantidad_categorias2 > 1) {
      info2 <- cut(mini_vector2, breaks = cortes2 , right = input_side2,
                   include.lowest = T)
      
    } else info2 <- mini_vector2
    
    dim(info2) <- c(length(info2), 1)
    info2 <- as.data.frame(info2)
    colnames(info2) <- colnames(input_base)[2]
  }
  
  segunda_base <- data.frame(info1, info2)
  
  # Tablas para Variable 1
  tablas <- list()
  
  tablas[[1]] <- RMedic_1c_tablas(input_base = minibase1,
                                  input_decimales = input_decimales,
                                  input_cadena = input_cadena,
                                  input_min = input_min1, 
                                  input_max = input_max1,
                                  input_breaks = input_breaks1,
                                  input_side = input_side1)
  
  
  tablas[[2]] <- RMedic_1c_tablas(input_base = minibase2,
                                  input_decimales = input_decimales,
                                  input_cadena = input_cadena,
                                  input_min = input_min2, 
                                  input_max = input_max2,
                                  input_breaks = input_breaks2,
                                  input_side = input_side2)
  
  
  # segunda_base <- mtcars[c(2,8)]
  # otras_tablas <- RMedic_2q_tablas(segunda_base, 2)$df02
  
  otras_tablas <- RMedic_2q_tablas(segunda_base, input_decimales)$df02
  
  
  armado <- list()
  
  directo1 <- c(1:7)
  
  for(k in directo1){
    # Medidas Resumen
    armado[[k]] <- rbind(tablas[[1]][[k]], tablas[[2]][[k]])
    rownames(armado[[k]]) <- c(1:nrow(armado[[k]]))
    names(armado)[k] <- names(tablas[[1]])[k]
  }
  
  directo2 <- c(8,9,10)
  conteo_interno <- 0
  for(k in directo2){
    
    conteo_interno <- conteo_interno + 1
    # Medidas Resumen
    armado[[k]] <- rbind(tablas[[1]][[8]][conteo_interno, ], tablas[[2]][[8]][conteo_interno, ])
    rownames(armado[[k]]) <- c(1:nrow(armado[[k]]))
    # names(armado)[k] <- names(tablas[[1]])[8]
    names(armado)[k] <-  paste0("Intervalo de Confianza (IC) del ",
           armado[[k]][1,3], " para la media") 
  }
  
  # Distribucion de Frecuencias - Var1
  armado[[11]] <- tablas[[1]][[9]]
  names(armado)[11] <- paste0(names(tablas[[1]])[9], " - Variable 1")
  
  # Distribucion de Frecuencias - Var2
  armado[[12]] <- tablas[[2]][[9]]
  names(armado)[12] <- paste0(names(tablas[[1]])[9], " - Variable 2")
  
  
  # Distribución Bivariada
  armado[[13]] <- as.matrix(otras_tablas[[3]][[1]])
  names(armado)[13] <- "Distribución Bivariada"
  
  armado[[14]] <-  as.matrix(otras_tablas[[3]][[4]])
  names(armado)[14] <- "Porcentajes al total"
  
  armado[[15]] <-  as.matrix(otras_tablas[[4]][[4]])
  names(armado)[15] <- "Porcentajes por filas"
  
  armado[[16]] <-  as.matrix(otras_tablas[[5]][[4]])
  names(armado)[16] <- "Porcentajes por columnas"
  
  return(armado)
  
} # Fin function

###############################

control_qc <- function(input_base = NULL, input_cadena = NULL){
  
  # # # Funcionamiento por defecto
  {
    ###
    
    if (is.null(input_cadena)) input_cadena <- TRUE
    
    
    
    
    # Armamos el siguiente eslabon
    output_cadena <- input_cadena
    
    ###
  } # Fin Funcionamiento
  ################################################
  
  
  # # # Controles 1
  {
    ###
    
    # Verificar que input_base no sea nulo
    if(output_cadena && is.null(input_base)) {
      cat("Error control_qc(): input_base no debe ser nulo", "\n")
      output_cadena <- FALSE
    }  
    
    # Verificar si es un data frame
    if(output_cadena && !is.data.frame(input_base)) {
      cat("Error control_qc(): input_base debe ser un data.frame", "\n")
      output_cadena <- FALSE
    }
    
    # Verificar si tiene al menos una columna
    if(output_cadena && ncol(input_base) == 0) {
      cat("Error control_qc(): input_base no tiene columnas")
      output_cadena <- FALSE
    }
    
    # Verificar si tiene una cantidad diferente de 2 columnas
    if(output_cadena && ncol(input_base) != 2) {
      cat("Error control_cq(): input_base debe tener dos columnas")
      output_cadena <- FALSE
    }
    
    # Verificar si es no tiene datos
    if(output_cadena && nrow(input_base) == 0) {
      stop("Error control_qc(): input_base no presenta filas")
      output_cadena <- FALSE
    }
    
    # Verificar si la 2da columan es numerica
    if(output_cadena && !is.numeric(input_base[,2])) {
      stop("Error control_cq(): input_base columna 2 debe ser numérica")
      output_cadena <- FALSE
    }
    
    
    # Nota: No hacemos una verificacion sobre si la 1ra
    #       columna es tipo factor o tipo character, por que
    #       podrian ser numeros representando categorias.
    
    ###      
  } # Fin Controles 1
  ###################################################################
  
  
  # # # Salida
  {
    ###
    
    salida <- output_cadena
    
    names(salida) <- c("output_cadena")
    
    return(salida)
    
    
    ###  
  } # Fin Salida
  ##################
  
  
}

###############################





RMedic_qc_tablas <- function(input_base = NULL, input_decimales = NULL, input_cadena = NULL,
                             input_min = NULL, input_max = NULL, input_breaks = NULL,
                             input_side = NULL) {
  
  
  
  
  # Medidas de posicion y dispersion simultaneas
  # Default values
  if (is.null(input_decimales)) input_decimales <- 2
  if (is.null(input_cadena)) input_cadena <- T
  
  
  
  
  # # # Control 1 - input_base
  {
    ###
    # 1- Es nulo
    # 2- No es un data.frame
    # 3- Tiene cero columnas
    # 4- Tiene 0 columnas
    # 5- Tiene 1 columna
    # 6- Tiene mas de 2 columnas
    # 7- No tiene filas (nrow(input_base))
    
    veredicto1 <- control_qc(input_base = input_base, input_cadena = input_cadena)
    output_cadena <- veredicto1
    
    ###  
  } # Fin Control 1 - input_base
  #################################################
  
  
  
  # # # minibase y Control 2 -  base "NO DATA"
  {
    ###
    
    # Si todo va OK...
    if (output_cadena) {
      
      # Creamos "minibase"  
      minibase <- na.omit(input_base)
      factor <- as.data.frame(as.factor(as.character(minibase[,1])))
      colnames(factor) <- colnames(minibase)[1]
      vr <- minibase[2]
      factor_vector <- factor[,1]
      vr_vector <- vr[,1]
      
      # Vemos que minibase tenga filas
      if (nrow(minibase) == 0) {
        cat("Error en RMedic_2c_tablas(): 'input_base' posee solo celdas vacias.", "\n")
        output_cadena <- FALSE
      }
      
      # minibase sea numerica
      if (!is.numeric(vr_vector)) {
        cat("Error RMedic_qc_tablas(): La columna 2 de 'input_base' debe ser numerica.", "\n")
        output_cadena <- FALSE
      }
      
      # minibase sea numerica
      if (length(levels(factor_vector)) == 0) {
        cat("Error RMedic_qc_tablas(): La columna 1 de 'input_base' debe tener al menos una categoría.", "\n")
        output_cadena <- FALSE
      }
      
    } # Fin if Si todo va OK...
    ###################################################
    
    # Si hay algun problema...
    if (output_cadena == FALSE){
      minibase <- na.omit(mtcars[c(2,1)])
      factor <- as.data.frame(as.factor(as.character(minibase[,1])))
      colnames(factor) <- colnames(minibase)[1]
      vr <- minibase[2]
      factor_vector <- factor[,1]
      vr_vector <- vr[,1]
      
      colnames(minibase) <- c("No Data1", "No Data2")
      cat("Error en RMedic_2c_tablas(): 'minibase' externo agregado (NO DATA)", "\n")
    } # Fin if si hay problemas
    ##########################################################
    
    
    ### 
  } # Fin Control 2 -  base "NO DATA"
  ################################################################
  
  
  # # # SandBox en caso de no ser valido
  {
    ###
    
    # Si hay algun problema...
    if (output_cadena == FALSE){
      minibase <- na.omit(mtcars[c(2,1)])
      factor <- as.data.frame(as.factor(as.character(minibase[,1])))
      colnames(factor) <- colnames(minibase)[1]
      vr <- minibase[2]
      factor_vector <- factor[,1]
      vr_vector <- vr[,1]
      colnames(minibase) <- c("No Data1", "No Data2")
      cat("Error en RMedic_2c_tablas(): 'minibase' externo agregado (NO DATA)", "\n")
    } # Fin if si hay problemas
    ##########################################################
    
    ###  
  }
  ############################################################################
  
  
  # More default values
  {
    ###
    
    # Valores por defecto...
    if(is.null(input_min))  input_min <- min(vr_vector)
    if(is.null(input_max))  input_max <- max(vr_vector)
    if(is.null(input_breaks))  input_breaks <- nclass.Sturges(vr_vector)
    if(is.null(input_side))  input_side <- T
    
    
    ###
  } # Fin More default values
  #################################################
  
  
  
  # Tablas para Variable 1
  tablas <- list()
  
  cantidad_categorias <- length(levels(factor_vector))
  
  cantidad_analisis <- cantidad_categorias + 1
  
  for (k in 1:cantidad_analisis){
    
    # Esto es para cada nivel del factor  
    if (k < cantidad_analisis) dt_categoria <- factor_vector == levels(factor_vector)[k] 
    
    # Esto es para todos los datos juntos...
    if (k == cantidad_analisis) dt_categoria <- rep(T, length(factor_vector))
    
    miniarmado <- as.data.frame(minibase[dt_categoria, 2])
    colnames(miniarmado) <- colnames(minibase)[2]
    
    tablas[[k]] <- RMedic_1c_tablas(input_base = miniarmado,
                                    input_decimales = input_decimales,
                                    input_cadena = input_cadena,
                                    input_min = NULL, 
                                    input_max = NULL,
                                    input_breaks = NULL,
                                    input_side = NULL)
    
  }
  
  
  
  armado_nombres <- c(levels(factor_vector), "Medidas Generales") 
  names(tablas) <- armado_nombres
  
  
  
  
  
  # Armado especial
  armado <- list()
  
  directo1 <- c(1:7)
  
  for(k1 in directo1){
    for (k2 in 1:length(tablas)){
      
      
      if(k2 == 1)  armado[[k1]] <- tablas[[k2]][[k1]] else
        armado[[k1]] <- rbind(armado[[k1]], tablas[[k2]][[k1]])
      
      #rownames(armado[[k1]]) <- c(1:nrow(armado[[k]]))
      #names(armado)[k] <- names(tablas[[1]])[k]
    }
    colnames(armado[[k1]])[1] <- c("Categoría de la variable")
    armado[[k1]][,1] <- armado_nombres
  }
  
  names(armado)[directo1] <- names(tablas[[1]])[directo1]
  
  
  directo2 <- c(8,9,10)
  
  conteo_interno <- 0
  for(k1 in directo2){
    conteo_interno <- conteo_interno + 1
    for (k2 in 1:length(tablas)){
      
      
      # Medidas Resumen
      if(k2 == 1)  armado[[k1]] <- tablas[[k2]][[8]][conteo_interno, ] else
        armado[[k1]] <- rbind(armado[[k1]], tablas[[k2]][[8]][conteo_interno, ])
      
      
    }
    colnames(armado[[k1]])[1] <- c("Categoría de la variable")
    armado[[k1]][,1] <- armado_nombres
    rownames(armado[[k1]]) <- c(1:nrow(armado[[k1]]))
    names(armado)[k1] <- paste0("Intervalo de Confianza (IC) del ",
                                armado[[k1]][1,3], " para la media") 
  }
  
  # Distribucion de Frecuencias - QC
  # Tabla 11 y 12: Distribucion de Frecuencias Bivariada
  {
    ###
    
    diferencia <- input_max - input_min
    rango <- diferencia/input_breaks
    
    cortes <- input_min + c(0:input_breaks)*rango
    
    # VerificacioN!
    # Pasa que si la variable es constante, hay que hacer
    # una sola categoria con todo. La funcion de R aunque
    # la variable sea constante, te tira mas de 1 intervalo.
    # Eso esta mal.
    tabla <- table(vr_vector)
    cantidad_categorias <- length(names(tabla))
    cantidad_cortes <- length(cortes)
    if(cantidad_categorias < cantidad_cortes) cortes <- cantidad_categorias
    
    if(cantidad_categorias > 1) {
      info <- cut(vr_vector, breaks = cortes , right = input_side,
                  include.lowest = T)
      
    } else info <- vr_vector
    
    #  dim(info) <- c(length(info), 1)
    #  info <- as.data.frame(info)
    #  colnames(info) <- paste0("recat_", colnames(input_base)[2])
    
    segunda_minibase <- as.data.frame(cbind(factor, info))
    
    
    nuevas_tablas <- RMedic_2q_tablas(input_base = segunda_minibase,
                                      input_decimales = input_decimales,
                                      input_cadena = NULL)
    
    armado[[11]] <- as.matrix(nuevas_tablas$df02$`Al total`$`Frecuencias Absolutas`)
    
    armado[[12]] <- nuevas_tablas$df02$`Al total`$`Porcentajes al total`
    
    armado[[13]] <- nuevas_tablas$df02$`Por filas`$`Porcentajes por filas`
    
    armado[[14]] <- nuevas_tablas$df02$`Por columnas`$`Porcentajes por columnas`
    
    names(armado)[c(11:14)] <- c("Tabla de Frecuencias Bivariadas",
                                 "Tabla de Porcentajes al total", 
                                 "Tabla de Porcentajes por filas",
                                 "Tabla de Porcentajes por columnas")
    ###
  } # Fin Tabla 9 Tabla de Frecuencias
  ######################################################
  
  
  
  return(armado)
  
} # Fin function

############################

graficos_2c <- function(minibase = NULL, 
                        tipo_grafico = NULL,
                        cols = c("red", "blue"),
                        xlab = NULL,
                        ylab = NULL,
                        xlim = NULL,
                        ylim = NULL,
                        labvar1 = NULL, 
                        labvar2 = NULL
                        ){
  

  # Otros detalles
  cantidad_grupos <- 2
  margen <- 0.5
  
  if(tipo_grafico == "xy") {
    
    # Rotulos
    if(is.null(xlab)) xlab <- colnames(minibase)[1]
    if(is.null(ylab)) ylab <- colnames(minibase)[2]
    
    if(is.null(xlim)) xlim <- c(min(minibase[,1]), max(minibase[,1]))
    if(is.null(ylim)) ylim <- c(min(minibase[,2]), max(minibase[,2]))
    
    
    # Tabla de para 2C
    tablas_2c <- RMedic_2c_tablas(input_base = minibase)
    
    plot(x = minibase[,1], y = minibase[,2],
         col = cols,
         cex = 2,
         pch = 19,
         xlab = xlab,
         ylab = ylab,
         xlim = xlim,
         ylim = ylim
    )
    
  } else
    if(tipo_grafico == "mde") {
    
    # Rotulos de los ejes
    if(is.null(xlab)) xlab <- "Variables"
    if(is.null(ylab)) ylab <- "Rango de ambas variables"
    
    # Limite del eje Y
    if(is.null(ylim)) ylim <- c(min(minibase), max(minibase))
    if(is.null(xlim)) xlim <- c(margen, (cantidad_grupos + margen))
    
    # Rotulos de las variables
    if(is.null(labvar1)) labvar1 <- colnames(minibase)[1]
    if(is.null(labvar2)) labvar2 <- colnames(minibase)[2]
    
    # Tabla de para 2C
    tablas_2c <- RMedic_2c_tablas(input_base = minibase)
    
    # Medias
    media <- as.numeric(as.character(tablas_2c[[1]][,2]))
    names(media) <- tablas_2c[[1]][,1]
    
    # Errores estandard
    desvio_estandard <- as.numeric(as.character(tablas_2c[[1]][,3]))
    names(desvio_estandard) <- tablas_2c[[1]][,1]
    
    
    # Pasamos toda la minibase a un vector
    VR <- as.vector(as.matrix(minibase))
    
    # Armamos la estructura que necesitamos
    FACTOR <- rep(c(1:2), each = nrow(minibase))
    
    # Colores de cada punto... Un vector de colores
    vector_colores <- rep(cols, each = nrow(minibase))
    
    
    # Calculamos los margenes del intervalo
    lista_grafica <- list()
    
    matriz_valores <- matrix(NA, 2, 2)
    matriz_valores[1,] <- media - desvio_estandard 
    matriz_valores[2,] <- media + desvio_estandard
    colnames(matriz_valores) <- colnames(minibase)
    rownames(matriz_valores) <- c("LI", "LS")
    
    
    plot( x = FACTOR[1], y = VR[1], col = "white", 
          xlim = xlim, 
          ylim = ylim,
          xlab = xlab, 
          ylab = ylab,
          xaxt = "n",
          cex = 2,
          pch = 19)
    axis(1, at=1:cantidad_grupos, labels = c(labvar1, labvar2))
    
    
    # Graficamos las medias +- 1 error estandard  
    for (k in 1:ncol(matriz_valores)){
      
      lines(rep(k,nrow(matriz_valores)), matriz_valores[,k], lwd = 3)
      points(k, media[k],  cex = 2,  col = cols[k],  pch=19)  
      
      
    }
    
  } else
      if(tipo_grafico == "mee") {
    
        # Rotulos de los ejes
        if(is.null(xlab)) xlab <- "Variables"
        if(is.null(ylab)) ylab <- "Rango de ambas variables"
        
        # Limite del eje Y
        if(is.null(ylim)) ylim <- c(min(minibase), max(minibase))
        if(is.null(xlim)) xlim <- c(margen, (cantidad_grupos + margen))
        
        # Rotulos de las variables
        if(is.null(labvar1)) labvar1 <- colnames(minibase)[1]
        if(is.null(labvar2)) labvar2 <- colnames(minibase)[2]
    

    # Tabla de para 2C
    tablas_2c <- RMedic_2c_tablas(input_base = minibase)
    
    # Medias
    media <- as.numeric(as.character(tablas_2c[[1]][,2]))
    names(media) <- tablas_2c[[1]][,1]
    
    # Errores estandard
    error_estandard <- as.numeric(as.character(tablas_2c[[6]][,5]))
    names(error_estandard) <- tablas_2c[[1]][,1]
    
    
    # Pasamos toda la minibase a un vector
    VR <- as.vector(as.matrix(minibase))
    
    # Armamos la estructura que necesitamos
    FACTOR <- rep(c(1:2), each = nrow(minibase))
    
    # Colores de cada punto... Un vector de colores
    vector_colores <- rep(cols, each = nrow(minibase))
    
    
    # Calculamos los margenes del intervalo
    lista_grafica <- list()
    
      matriz_valores <- matrix(NA, 2, 2)
      matriz_valores[1,] <- media - error_estandard 
      matriz_valores[2,] <- media + error_estandard
      colnames(matriz_valores) <- colnames(minibase)
      rownames(matriz_valores) <- c("LI", "LS")
      
    
    plot( x = FACTOR[1], y = VR[1], col = "white", 
          xlim = xlim, 
          ylim = ylim,
          xlab = xlab, 
          ylab = ylab,
          xaxt = "n",
          cex = 2,
          pch = 19)
    axis(1, at=1:cantidad_grupos, labels = c(labvar1, labvar2))
    
    
    # Graficamos las medias +- 1 error estandard  
    for (k in 1:ncol(matriz_valores)){
      
      lines(rep(k,nrow(matriz_valores)), matriz_valores[,k], lwd = 3)
      points(k, media[k],  cex = 2,  col = cols[k],  pch=19)  
      
      
    }
    
  } else 
        if(tipo_grafico == "boxplot") {
      
          # Rotulos de los ejes
          if(is.null(xlab)) xlab <- "Variables"
          if(is.null(ylab)) ylab <- "Rango de ambas variables"
          
          # Limite del eje Y
          if(is.null(ylim)) ylim <- c(min(minibase), max(minibase))
          if(is.null(xlim)) xlim <- c(margen, (cantidad_grupos + margen))
          
          # Rotulos de las variables
          if(is.null(labvar1)) labvar1 <- colnames(minibase)[1]
          if(is.null(labvar2)) labvar2 <- colnames(minibase)[2]
      
      
     
      
          # Tabla de para 2C
          tablas_2c <- RMedic_2c_tablas(input_base = minibase)
          
          # Medias
          vector_medias <- as.numeric(as.character(tablas_2c[[1]][,2]))
          names(vector_medias) <- tablas_2c[[1]][,1]
          seleccionados <- 1:length(vector_medias)
          
      boxplot(minibase, 
              col = cols, 
              xlim = xlim, 
              xlab = xlab, 
              ylab = ylab,
              xaxt = "n"
              )
      axis(1, at=1:cantidad_grupos, labels = c(labvar1, labvar2))
   
      #points(x = seleccionados, y = vector_medias , pch = 19, col = "black", cex = 1.5)
      
    } else
          if(tipo_grafico == "violinplot") {
        
            # Rotulos de los ejes
            if(is.null(xlab)) xlab <- "Variables"
            if(is.null(ylab)) ylab <- "Rango de ambas variables"
            
            # Limite del eje Y
            if(is.null(ylim)) ylim <- c(min(minibase), max(minibase))
            if(is.null(xlim)) xlim <- c(margen, (cantidad_grupos + margen))
            
            # Rotulos de las variables
            if(is.null(labvar1)) labvar1 <- colnames(minibase)[1]
            if(is.null(labvar2)) labvar2 <- colnames(minibase)[2]
        
        
        
        library(vioplot)
        
           
        vioplot(minibase, 
                col = cols, 
              #  xlim = xlim,  # Lo cancele, por que sino tira mensaje!
                xlab = xlab, 
                ylab = ylab,
                xaxt = "n"
        
            )
        axis(1, at=1:cantidad_grupos, labels = c(labvar1, labvar2))
        
        
      } else
            if(tipo_grafico == "dispersion") {
          
              # Rotulos de los ejes
              if(is.null(xlab)) xlab <- "Variables"
              if(is.null(ylab)) ylab <- "Rango de ambas variables"
              
              # Limite del eje Y
              if(is.null(ylim)) ylim <- c(min(minibase), max(minibase))
              if(is.null(xlim)) xlim <- c(margen, (cantidad_grupos + margen))
              
              # Rotulos de las variables
              if(is.null(labvar1)) labvar1 <- colnames(minibase)[1]
              if(is.null(labvar2)) labvar2 <- colnames(minibase)[2]
          
         
          
          # Pasamos toda la minibase a un vector
          VR <- as.vector(as.matrix(minibase))
          
          # Armamos la estructura que necesitamos
          FACTOR <- rep(c(1:2), each = nrow(minibase))
          
          # Colores de cada punto... Un vector de colores
          vector_colores <- rep(cols, each = nrow(minibase))
          
          
          
          
          plot( x = FACTOR, y = VR, col = vector_colores, 
                xlim = xlim, 
                ylim = ylim,
                xlab = xlab, 
                ylab = ylab,
                xaxt = "n",
                cex = 2,
                pch = 19)
          axis(1, at=1:cantidad_grupos, labels = c(labvar1, labvar2))
          
          
        
          
        } else
              if(tipo_grafico == "conectores") {
            
                # Rotulos de los ejes
                if(is.null(xlab)) xlab <- "Variables"
                if(is.null(ylab)) ylab <- "Rango de ambas variables"
                
                # Limite del eje Y
                if(is.null(ylim)) ylim <- c(min(minibase), max(minibase))
                if(is.null(xlim)) xlim <- c(margen, (cantidad_grupos + margen))
                
                # Rotulos de las variables
                if(is.null(labvar1)) labvar1 <- colnames(minibase)[1]
                if(is.null(labvar2)) labvar2 <- colnames(minibase)[2]
            
            
            
            # Pasamos toda la minibase a un vector
            VR <- as.vector(as.matrix(minibase))
            
            # Armamos la estructura que necesitamos
            FACTOR <- rep(c(1:2), each = nrow(minibase))
            
            # Colores de cada punto... Un vector de colores
            vector_colores <- rep(cols, each = nrow(minibase))
            
            
            
            
            plot( x = FACTOR, y = VR, col = vector_colores, 
                  xlim = xlim, 
                  ylim = ylim,
                  xlab = xlab, 
                  ylab = ylab,
                  xaxt = "n",
                  cex = 2,
                  pch = 19)
            axis(1, at=1:cantidad_grupos, labels = c(labvar1, labvar2))
            
            for (k in 2:ncol(minibase)) {
              for(m in 1:nrow(minibase))
                lines(c((k-1), k), minibase[m,c((k-1), k)])  
            }
            
            
            
            
            
          }
    
      
  
}


###################################################

graficos_qc <- function(minibase = NULL, 
                        tipo_grafico = NULL,
                        cols = NULL,
                        xlab = NULL,
                        ylab = NULL,
                        ylim = NULL

){
  
  

    if(tipo_grafico == "mde") {
      
      # Rotulos de los ejes
      if(is.null(xlab)) xlab <- colnames(minibase)[1]
      if(is.null(ylab)) ylab <- colnames(minibase)[2]
      
      
      
      # Tablas de QC
      tablas_qc <- RMedic_qc_tablas(input_base =  minibase,
                                    input_decimales = NULL,
                                    input_min = NULL,
                                    input_max = NULL,
                                    input_breaks = NULL,
                                    input_side = NULL
      )
      
      cantidad_categorias <- nrow(tablas_qc[[1]]) - 1
      seleccionados <- c(1:cantidad_categorias)
      categorias <- tablas_qc[[1]][seleccionados,1]
      
      if(is.null(cols)) cols <- rep("red", cantidad_categorias)
      
        
      # Medias
      media <- tablas_qc[[1]][seleccionados,2]
      names(media) <- categorias
      
      # Desvios estandard
      desvio_estandard <- as.numeric(as.character(tablas_qc[[6]][seleccionados,4]))
      names(desvio_estandard) <- categorias
      
      
      
      
      # Matriz de valores   
      matriz_valores <- matrix(NA, 2, cantidad_categorias)
      matriz_valores[1,] <- media - desvio_estandard 
      matriz_valores[2,] <- media + desvio_estandard
      colnames(matriz_valores) <- categorias
      rownames(matriz_valores) <- c("LI", "LS")
      
      minimo_absoluto <- min(minibase[,1], as.vector(matriz_valores))
      maximo_absoluto <- max(minibase[,1], as.vector(matriz_valores))
      
      if(is.null(ylim)) ylim <- c(minimo_absoluto, maximo_absoluto)
      
      
      plot( x = seleccionados, y = media, col = "white", 
            #     xlim = xlim, 
            ylim = ylim,
            xlab = xlab, 
            ylab = ylab,
            xaxt = "n",
            cex = 2,
            pch = 19)
      axis(1, at=1:cantidad_categorias, labels = categorias)
      
      
      # Graficamos las medias +- 1 error estandard  
      for (k in 1:ncol(matriz_valores)){
        
        lines(rep(k,nrow(matriz_valores)), matriz_valores[,k], lwd = 3)
        points(k, media[k],  cex = 2,  col = cols[k],  pch=19)  
        
      }
      
    } else
      if(tipo_grafico == "mee") {
        
        # Rotulos de los ejes
        if(is.null(xlab)) xlab <- colnames(minibase)[1]
        if(is.null(ylab)) ylab <- colnames(minibase)[2]
        
        
        
        # Tablas de QC
        tablas_qc <- RMedic_qc_tablas(input_base =  minibase,
                                      input_decimales = NULL,
                                      input_min = NULL,
                                      input_max = NULL,
                                      input_breaks = NULL,
                                      input_side = NULL
        )
        
        cantidad_categorias <- nrow(tablas_qc[[1]]) - 1
        seleccionados <- c(1:cantidad_categorias)
        categorias <- tablas_qc[[1]][seleccionados,1]
        
        if(is.null(cols)) cols <- rep("red", cantidad_categorias)
        
        
        # Medias
        media <- tablas_qc[[1]][seleccionados,2]
        names(media) <- categorias
        
        # Desvios estandard
        error_estandard <- as.numeric(as.character(tablas_qc[[6]][seleccionados,5]))
        names(error_estandard) <- categorias
        
        
        
        
        # Matriz de valores   
        matriz_valores <- matrix(NA, 2, cantidad_categorias)
        matriz_valores[1,] <- media - error_estandard 
        matriz_valores[2,] <- media + error_estandard
        colnames(matriz_valores) <- categorias
        rownames(matriz_valores) <- c("LI", "LS")
        
        minimo_absoluto <- min(minibase[,1], as.vector(matriz_valores))
        maximo_absoluto <- max(minibase[,1], as.vector(matriz_valores))
        
        if(is.null(ylim)) ylim <- c(minimo_absoluto, maximo_absoluto)
        
        
        plot( x = seleccionados, y = media, col = "white", 
              #     xlim = xlim, 
              ylim = ylim,
              xlab = xlab, 
              ylab = ylab,
              xaxt = "n",
              cex = 2,
              pch = 19)
        axis(1, at=1:cantidad_categorias, labels = categorias)
        
        
        # Graficamos las medias +- 1 error estandard  
        for (k in 1:ncol(matriz_valores)){
          
          lines(rep(k,nrow(matriz_valores)), matriz_valores[,k], lwd = 3)
          points(k, media[k],  cex = 2,  col = cols[k],  pch=19)  
          
        }
        
      } else 
        if(tipo_grafico == "boxplot") {
          
          # Rotulos de los ejes
          if(is.null(xlab)) xlab <- colnames(minibase)[1]
          if(is.null(ylab)) ylab <- colnames(minibase)[2]
          
          # Limite del eje Y
          if(is.null(ylim)) ylim <- c(min(minibase[,2]), max(minibase[,2]))

          # Tablas de QC
          tablas_qc <- RMedic_qc_tablas(input_base =  minibase,
                                        input_decimales = NULL,
                                        input_min = NULL,
                                        input_max = NULL,
                                        input_breaks = NULL,
                                        input_side = NULL
          )
          
          cantidad_categorias <- nrow(tablas_qc[[1]]) - 1
          seleccionados <- c(1:cantidad_categorias)
          categorias <- tablas_qc[[1]][seleccionados,1]
          # Medias
          vector_medias <- tablas_qc[[1]][seleccionados,2]
          names(vector_medias) <- vector_medias
          
          boxplot(minibase[,2] ~ minibase[,1], 
                  col = cols, 
                  ylim = ylim, 
                  xlab = xlab, 
                  ylab = ylab,
                  range = 0
          )
          
         # points(x = seleccionados, y = vector_medias , pch = 19, col = "black", cex = 1.5)
        #  axis(1, at=1:cantidad_grupos, labels = c(labvar1, labvar2))
          
          
        } else
          if(tipo_grafico == "violinplot") {
            
            # Rotulos de los ejes
            if(is.null(xlab)) xlab <- colnames(minibase)[1]
            if(is.null(ylab)) ylab <- colnames(minibase)[2]
            
            # Limite del eje Y
            if(is.null(ylim)) ylim <- c(min(minibase[,2]), max(minibase[,2]))
            
            library(vioplot)
            
            vioplot(minibase[,2] ~ minibase[,1], 
                    col = cols, 
                    ylim = ylim, 
                    xlab = xlab, 
                    ylab = ylab,
                    range = 0
            )
            #  axis(1, at=1:cantidad_grupos, labels = c(labvar1, labvar2))
            
            
          } else
            if(tipo_grafico == "dispersion") {
              
              # Rotulos de los ejes
              if(is.null(xlab)) xlab <- colnames(minibase)[1]
              if(is.null(ylab)) ylab <- colnames(minibase)[2]
              
              # Limite del eje Y
              if(is.null(ylim)) ylim <- c(min(minibase[,2]), max(minibase[,2]))
              
              
              
              # Pasamos toda la minibase a un vector
              VR <- minibase[,2]
              
              # Armamos la estructura que necesitamos
              
              if(is.factor(minibase[,1])) {
              FACTOR <-   minibase[,1]
              FACTOR_pos <- as.numeric(FACTOR) 
              
              } else {
                FACTOR <- as.factor(as.character(minibase[,1]))
                FACTOR_pos <- as.numeric(FACTOR)
                
              }
              # Colores de cada punto... Un vector de colores
              vector_colores <- cols[FACTOR_pos] 
              
              niveles_factor <- levels(FACTOR)
              cantidad_niveles <- length(niveles_factor)
              
              plot( x = FACTOR_pos, y = VR, col = vector_colores, 
                    ylim = ylim,
                    xlab = xlab, 
                    ylab = ylab,
                    cex = 2,
                    pch = 19,
                    xaxt = "n")
              axis(1, at=1:cantidad_niveles, labels = niveles_factor)
              
              
              
              
            } 
  
  
  
}

######################


KM_Tabla_General <- function(base = NULL, alfa = 0.05){
  
  library(xtable)
  library(survival)
  confianza <- 1-alfa
  
  
  minibase <- na.omit(base)
  
  # Separamos los elementos
  #  orden  <-  minibase[,1]
  tiempo <- minibase[,1]
  status <- minibase[,2]
  #  grupo  <-  minibase[,4]
  
  # Sobrevida General con R
  SGen <- survfit(Surv(tiempo, status) ~ 1, conf.type="log", conf.int = confianza,
                  type="kaplan-meier", error="greenwood")
  
  
  
  
  # Guardamos la poca informacion que nos da
  TABLA <- summary(SGen)
  # TABLA
  
  surv_mediana <- TABLA[["surv"]]
  orden_mediana <- c(1:length(surv_mediana))
  
  # Armamos algunos elementos
  TABLA1 <- data.frame(cbind(orden_mediana, TABLA[["surv"]], TABLA[["time"]]))
  colnames(TABLA1) <- c("Orden Mediana", "Proporcion de Sobrevida", "Tiempo")
  
  TABLA2 <- data.frame(cbind(orden_mediana, TABLA[["lower"]], TABLA[["time"]]))
  TABLA3 <- data.frame(cbind(orden_mediana, TABLA[["upper"]], TABLA[["time"]]))
  
  
  # Encontramos la mediana
  TABLA1 <- na.omit(TABLA1)
  mediana_KM <- NA
  if(ncol(TABLA1) > 1)   if (nrow(TABLA1) > 0) if (min(TABLA1[,2]) <= 0.5) {
    
    dt <- TABLA1[,2] <= 0.5
    TABLA1 <- TABLA1[dt,]
    mediana_KM <- TABLA1[1,3]
  }
  #  mediana_KM
  
  
  
  # Encontramos el low de la mediana
  TABLA2 <- na.omit(TABLA2)
  mediana_low <- NA
  if(ncol(TABLA2) > 1)   if (nrow(TABLA2) > 0) if (min(TABLA2[,2]) <= 0.5) {
    
    dt <- TABLA2[,2] <= 0.5
    TABLA2 <- TABLA2[dt,]
    mediana_low <- TABLA2[1,3]
  }
  #  mediana_low
  
  
  # Encontramos el upp de la mediana
  TABLA3 <- na.omit(TABLA3)
  mediana_upp <- NA
  if(ncol(TABLA3) > 1)   if (nrow(TABLA3) > 0) if (min(TABLA3[,2]) <= 0.5) {
    
    dt <- TABLA3[,2] <= 0.5
    TABLA3 <- TABLA3[dt,]
    mediana_upp <- TABLA3[1,3]
  }
  #  mediana_upp
  
  # Armamos una tabla nueva con la informacion que recolectamos
  nombres <- c("n", "Eventos", "Mediana de Sobrevida (50%)", "Límite Inferior", "Límite Superior", 
               "Alfa", "Confianza")
  TABLA_KM <- c(nrow(minibase), sum(status), mediana_KM, mediana_low, mediana_upp,
                alfa, confianza)
  TABLA_KM <- as.character(TABLA_KM)
  dim(TABLA_KM) <- c(1, length(TABLA_KM))
  colnames(TABLA_KM) <- nombres
  TABLA_KM[1,is.na(TABLA_KM[1,])] <- "Sin estimación posible"
  
  # Return Exitoso...
  # # # Sale la tabla y un objeto para graficar
  the_exit <- list(TABLA_KM, SGen)
  return(the_exit)
  
}


######################################





KM_Tabla_Grupos <- function(base = NULL, alfa = 0.05){
  
  library(xtable)
  library(survival)
  confianza <- 1-alfa
  
  
  minibase <- na.omit(base)
  
  # Creacion de Objetos Intermedios
  tiempo <- minibase[,1]
  status <- minibase[,2]
  grupo  <-  as.factor(minibase[,3])
  
  
  # Sobrevida General por Grupos con R
  SGrupo <- survfit(Surv(tiempo, status) ~ grupo, 
                    conf.type = "log", 
                    conf.int = confianza,
                    type="kaplan-meier", error="greenwood")
  SGrupo
  
  
  # La salida anterior, no es un objeto en R.
  # La muestra como texto, pero no se puede utilizar para armar una salida
  # reactiva en RMedic.
  # A si que... la vamos a hacer a mano.
  
  # Guardamos la poca informacion que nos da
  
  # Sobrevida por grupo
  SGrupoPartes <- list()
  nombres <- c("n", "Eventos", "Mediana de Sobrevida (50%)", "Límite Inferior", "Límite Superior")
  TABLA_KMgg <- data.frame(matrix(NA, length(levels(grupo)), length(nombres)))
  colnames(TABLA_KMgg) <- nombres
  rownames(TABLA_KMgg) <- levels(grupo)
  
  
  for (n in 1:length(levels(grupo))) {
    estos <- grupo == levels(grupo)[n]
    t1 <- tiempo[estos]
    s1 <- status[estos]
    g1 <- grupo[estos]
    
    SGrupoPartes[[n]] <- list()
    SGrupoPartes[[n]]  <- survfit(Surv(t1, s1) ~ g1, conf.type="log", conf.int=0.95, type="kaplan-meier", error="greenwood")
    SGrupoPartes[[n]]
    
    # Guardamos la poca informacion que nos da
    TABLA <- summary(SGrupoPartes[[n]])
    surv_mediana <- TABLA[["surv"]]
    orden_mediana <- c(1:length(surv_mediana))
    
    # Armamos algunos elementos
    TABLA1 <- data.frame(cbind(orden_mediana, TABLA[["surv"]], TABLA[["time"]]))
    TABLA2 <- data.frame(cbind(orden_mediana, TABLA[["lower"]], TABLA[["time"]]))
    TABLA3 <- data.frame(cbind(orden_mediana, TABLA[["upper"]], TABLA[["time"]]))
    
    
    # Encontramos la mediana
    TABLA1 <- na.omit(TABLA1)
    mediana_KM <- NA
    if(ncol(TABLA1) > 1) if (nrow(TABLA1) > 0) if (min(TABLA1[,2]) <= 0.5) {
      
      dt <- TABLA1[,2] <= 0.5
      TABLA1 <- TABLA1[dt,]
      mediana_KM <- TABLA1[1,3]
    }
    #    mediana_KM
    
    
    
    # Encontramos el low de la mediana
    TABLA2 <- na.omit(TABLA2)
    mediana_low <- NA
    if(ncol(TABLA2) > 1)  if (nrow(TABLA2) > 0) if (min(TABLA2[,2]) <= 0.5) {
      
      dt <- TABLA2[,2] <= 0.5
      TABLA2 <- TABLA2[dt,]
      mediana_low <- TABLA2[1,3]
    }
    #    mediana_low
    
    
    # Encontramos el upp de la mediana
    TABLA3 <- na.omit(TABLA3)
    mediana_upp <- NA
    if(ncol(TABLA3) > 1)  if (nrow(TABLA3) > 0) if (min(TABLA3[,2]) <= 0.5) {
      
      dt <- TABLA3[,2] <= 0.5
      TABLA3 <- TABLA3[dt,]
      mediana_upp <- TABLA3[1,3]
    }
    #    mediana_upp
    
    # Armamos una tabla nueva con la informacion que recolectamos
    mini <- c(length(s1), sum(s1), mediana_KM, mediana_low, mediana_upp)
    TABLA_KMgg[n,] <- mini
    #  cat("Va el ", n, "\n")
  }
  TABLA_KMgg[,1] <- as.character(TABLA_KMgg[,1])
  TABLA_KMgg[,2] <- as.character(TABLA_KMgg[,2])
  TABLA_KMgg <- cbind(levels(grupo), TABLA_KMgg)
  colnames(TABLA_KMgg)[1] <- "Grupo"
  
  
  for (k in 1:nrow(TABLA_KMgg)) {
    
  TABLA_KMgg[k,is.na(TABLA_KMgg[k,])] <- "Sin estimación posible"
  
  }
  
  # Return Exitoso...
  # # # Sale la tabla y un objeto para graficar
  the_exit <- list(TABLA_KMgg, SGrupo)
  return(the_exit)
  
}


########################################

KM_TestLogRank <- function(base = NULL, alfa = 0.05){
  
  
  # Separamos los elementos
  minibase <- na.omit(base)
  tiempo <- minibase[,1]
  status <- minibase[,2]
  grupo  <-  as.factor(minibase[,3])
  
  
  .S <-survdiff(Surv(tiempo, status) ~ grupo, rho=0.0)
  #  .S
  niveles_grupo <- levels(grupo)
  cantidad_niveles <- length(niveles_grupo)
  
  valor_chi <- .S[["chisq"]]
  valor_chi <- round(valor_chi, 2)
  gl <- length(levels(grupo)) - 1
  #  gl
  valor_p_interno <- pchisq(valor_chi, gl, ncp = 0, lower.tail = FALSE, log.p = FALSE)
  valor_p_interno <- round2(valor_p_interno, 2)
  
  valor_p_externo <- valor_p_interno
  if (valor_p_interno < 0.01) valor_p_externo <- c("<0.01")
  
  frase1 <- c("No Rechazo Ho")
  if (valor_p_interno < alfa) frase1<- c("Rechazo Ho")
  
  frase2A_1 <- c("El valor p es mayor que el valor de alfa.<br/>", "\n",
               "No se rechaza la Hipótesis Nula.<br/>", "\n",
               "No existen diferencias estadísticamente significativas entre los grupos.<br/>", "\n",
               "Los grupos son estadísticamente iguales.")
  
  frase2A_2 <- c("El valor p es mayor que el valor de alfa.<br/>", "\n",
               "No se rechaza la Hipótesis Nula.<br/>", "\n",
               "No existen diferencias estadísticamente significativas entre ambos grupos.<br/>", "\n",
               "Ambos grupos son estadísticamente iguales.")
  
  
  frase2B_1 <- c("El valor p es igual que el valor de alfa.<br/>", "\n",
               "No se rechaza la Hipótesis Nula.<br/>", "\n",
               "No existen diferencias estadísticamente significativas entre los grupos.<br/>", "\n",
               "Los grupos son estadísticamente iguales.")
  
  frase2B_2 <- c("El valor p es igual que el valor de alfa.<br/>", "\n",
                 "No se rechaza la Hipótesis Nula.<br/>", "\n",
                 "No existen diferencias estadísticamente significativas entre ambos grupos.<br/>", "\n",
                 "Ambos grupos son estadísticamente iguales.")
  
  
  frase2C_1 <- c("El valor p es menor que el valor de alfa.<br/>", "\n",
              "Se rechaza la Hipótesis Nula.<br/>", "\n",
              "Existen diferencias estadísticamente significativas.<br/>", "\n",
              "Al menos uno de los grupos son estadísticamente diferentes.<br/>", "\n",
              "En caso de ser solo 2 grupos y existir diferencias estadísticas, los dos grupos son diferentes entre si.<br/>", "\n",
              "En caso de ser 3 o más grupos el test de LogRank solo tiene la capacidad de definir si 
              al menos uno de los grupos es estadísticamente diferente pero no tiene la capacidad de indicar 
              cual o cuales son los grupos estadísticamente diferentes. Rechazar la hipótesis nula no 
              implica que necesariamente todos los grupos son estadísticamente diferentes.")
              
  
  frase2C_2 <- c("El valor p es menor que el valor de alfa.<br/>", "\n",
              "Se rechaza la Hipótesis Nula.<br/>", "\n",
              "Existen diferencias estadísticamente significativas entre ambos grupos.<br/>", "\n",
              "Los dos grupos son estadísticamente diferentes entre si.")
  
  frase2 <- ""
  if(valor_p_interno > alfa && cantidad_niveles == 2) frase2 <- frase2A_2 else
    if(valor_p_interno > alfa && cantidad_niveles > 2) frase2 <- frase2A_1 else 
      if (valor_p_interno == alfa && cantidad_niveles == 2) frase2 <- frase2B_2 else 
        if (valor_p_interno == alfa && cantidad_niveles > 2) frase2 <- frase2B_1 else
          if (valor_p_interno < alfa && cantidad_niveles == 2) frase2 <- frase2C_2 else
            if (valor_p_interno < alfa && cantidad_niveles > 2) frase2 <- frase2C_1
  

  
  # Respuesta
  if (valor_p_interno <= alfa) respuesta <- "Si" else respuesta <- "No"
    
  nombres <- c("Variable 1 (Tiempo)", "Variable 2 (Estatus)", "Variable 3 (Grupo)",
               "n", "Test", "Estadístico (Chi)", "Grados de Libertad", "Valor p", "Alfa", "Decisión", 
               "¿Al menos una de los grupos es diferente?")
  
  salida <- matrix(NA, 1, length(nombres))
  colnames(salida) <- nombres
  
  salida[1,  1] <- colnames(base)[1]
  salida[1,  2] <- colnames(base)[2]
  salida[1,  3] <- colnames(base)[3]
  salida[1,  4] <- nrow(minibase)
  salida[1,  5] <- "Test de LogRank"
  salida[1,  6] <- valor_chi
  salida[1,  7] <- gl
  salida[1,  8] <- valor_p_externo
  salida[1,  9] <- alfa
  salida[1, 10] <- frase1
  salida[1, 11] <- respuesta


  
  # Salida del test  
  KM <- list()
  

  KM$TablaLogRank <- salida
  KM$Frase <- frase2
  
  KM
  
}


###################################################

control_KM1 <- function(base = NULL){

  # Controlador general, empieza TRUE
  controlador <- TRUE
  frase <- ""
  
  # Minibase
  minibase <- na.omit(base)
  columna_tiempo <- paste0("'",colnames(minibase)[1],"'")
  columna_status <- paste0("'",colnames(minibase)[2],"'")
  
  # La base debe contener al menos 2 columnas
  if(ncol(base) < 2) {
    
    controlador <- FALSE
    frase <- "La base debe contener al menos 2 columnas. Revise la base de datos."
  } else  
  
  # La base debe contener al menos 1 fila
  if(nrow(base) == 0){
    controlador <- FALSE
    frase <- "La base de datos debe contener al menos 1 fila. Revise la base de datos."
    
  } else 
  
  # La minibase debe contener dos columnas
  if(ncol(minibase) < 2) {
    
    controlador <- FALSE
    frase <- "La minibase debe contener al menos 2 columnas. Revise la base de datos."
  }  else
  
  # Las dos variables seleccionadas deben ser diferentes  
  if(colnames(minibase)[1] == colnames(minibase)[2]) {
    
    controlador <- FALSE
    frase <- "Ha seleccionado dos veces la misma variable. Seleccione dos variables diferentes, una para 
    'tiempo' y otra para 'status'."
  }  else
  
  # La minibase debe tener al menos una fila
  if(nrow(minibase) == 0){
    controlador <- FALSE
    frase <- "La minibase de datos debe contener al menos 1 fila. Tenga en cuenta que solo 
    son utilizadas las filas que poseen simultáneamente un dato de 'tiempo' y un dato de 'status'.
    Revise la base de datos."
    
  }
  
  # La columna de tiempos no puede tener valores negativos
  if(sum(minibase[,1] < 0) != 0){
    controlador <- FALSE
    frase <- paste0("<b><u>Advertencia:</u> ", "La variable ", columna_tiempo, " asignada como 
                        Tiempo (Variable 1)' solo puede contener medidas de tiempo igual a cero o positivas,
                        o celdas vacías. Se han encontrado en la variable tiempos negativos. Si ha calculado 
              los tiempos a partir de fechas, revise las fechas. Si no es posible verificar o corregir 
              la información de tiempo, deje vacía la celda que presenta tiempos negativos en su base de datos.</b>",
                    "<br/><br/><br/><br/>")
                    

    
  } else
  
  # La columna de status presenta categorias distinta de cero y uno
  if(length(unique(c(levels(as.factor(minibase[,2])), c("0", "1")))) != 2){
      controlador <- FALSE
      frase <-  paste0("<b><u>Advertencia:</u> ", "La variable ", columna_status, " asignada como 
                        Status (Variable 2)' solo puede contener
                        '0' y/o '1' como información. <br/>
                        Se han detectado otras categorías en la variable seleccionada. <br/>
                        Realice un control sobre esta variable y modifique la base de datos.</b>",
                        "<br/><br/><br/><br/>")
    
    }
  

####################################
  # Return Exitoso
  return(list(controlador, frase))
  
  
}


####################################################


control_KM2 <- function(base = NULL){
  
  # Controlador general, empieza TRUE
  controlador <- TRUE
  frase <- ""
  
  # Minibase
  minibase <- na.omit(base)
  columna_tiempo <- paste0("'",colnames(minibase)[1],"'")
  columna_status <- paste0("'",colnames(minibase)[2],"'")
  columna_grupo <- paste0("'",colnames(minibase)[3],"'")
  
  # La base debe contener al menos 2 columnas
  if(ncol(base) < 3) {
    
    controlador <- FALSE
    frase <- "La base debe contener al menos 3 columnas. Revise la base de datos."
  } else  
    
    # La base debe contener al menos 1 fila
    if(nrow(base) == 0){
      controlador <- FALSE
      frase <- "La base de datos debe contener al menos 1 fila. Revise la base de datos."
      
    } else 
      
      # La minibase debe contener dos columnas
      if(ncol(minibase) < 3) {
        
        controlador <- FALSE
        frase <- "La minibase debe contener al menos 3 columnas. Revise la base de datos."
      }  else
        
        # Alguna de las variables esta repetida  
        if(length(names(table(colnames(minibase)))) != 3) {
          
          controlador <- FALSE
          frase <- "Tiempo, Status y Grupo deben ser 3 variables diferentes de la base de datos."
        }  else
          
          # La minibase debe tener al menos una fila
          if(nrow(minibase) == 0){
            controlador <- FALSE
            frase <- "La minibase de datos debe contener al menos 1 fila. Tenga en cuenta que solo 
    son utilizadas las filas que poseen simultáneamente un dato de 'tiempo' y un dato de 'status'.
    Revise la base de datos."
            
          }
  
  # La columna de tiempos no puede tener valores negativos
  if(sum(minibase[,1] < 0) != 0){
    controlador <- FALSE
    frase <- paste0("<b><u>Advertencia:</u> ", "La variable ", columna_tiempo, " asignada como 
                        Tiempo (Variable 1)' solo puede contener medidas de tiempo igual a cero o positivas,
                        o celdas vacías. Se han encontrado en la variable tiempos negativos. Si ha calculado 
              los tiempos a partir de fechas, revise las fechas. Si no es posible verificar o corregir 
              la información de tiempo, deje vacía la celda que presenta tiempos negativos en su base de datos.</b>",
                    "<br/><br/><br/><br/>")
    
    
    
  } else
    
    # La columna de status presenta categorias distinta de cero y uno
    if(length(unique(c(levels(as.factor(minibase[,2])), c("0", "1")))) != 2){
      controlador <- FALSE
      frase <-  paste0("<b><u>Advertencia:</u> ", "La variable ", columna_status, " asignada como 
                        Status (Variable 2)' solo puede contener
                        '0' y/o '1' como información. <br/>
                        Se han detectado otras categorías en la variable seleccionada. <br/>
                        Realice un control sobre esta variable y modifique la base de datos.</b>",
                       "<br/><br/><br/><br/>")
      
    } else
  
  
  # La columna de grupo presenta al menos dos categorias
  if(length(levels(as.factor(minibase[,3]))) < 2){
    controlador <- FALSE
    frase <-  paste0("<b><u>Advertencia:</u> ", "La variable ", columna_grupo, " asignada como 
                        Grupo (Variable 3)' debe contener al menos 2 grupos para poder llevar a cabo 
                        una comparación.<br/>
                        Se ha detectado que la varaible grupo solo posee un grupo.<br/>
                        Realice un control sobre esta variable y modifique la base de datos.</b>",
                     "<br/><br/><br/><br/>")
    
  }
  
  ####################################
  # Return Exitoso
  return(list(controlador, frase))
  
  
}


#################################################################




control_1q_RMedic <- function(base = NULL, columna = NULL){
  
  
  # Minibase
  minibase <- na.omit(base[columna])
  minibase[,1] <- as.character(minibase[,1])
  
  ##############################################################################
  
  # Cantidad de celdas vacias
  cantidad_na <- sum(is.na(base[,columna]))
  filas_base <- nrow(base)
  filas_minibase <- nrow(minibase)
  
  # Tabla para celdas vacias
  columnas01 <- c("Total de filas", "Celdas con datos", "Celdas vacías")
  tabla01 <- matrix(NA, 1, length(columnas01))  
  tabla01[1,] <- c(filas_base, filas_minibase, cantidad_na)
  colnames(tabla01) <- columnas01
  
  # Frases de celdas vacias
  frase_armada01_A <- paste0("Todas las celdas de la columna seleccionada presentan datos.
                              <br/>
                              Al utilizar esta variable el 'n' será ", filas_base, ".")
  
  frase_armada01_B <- paste0("La columna presenta celdas vacías. Al utilizar la columna seleccionada 
                              el 'n' será ", filas_minibase, ".")
  
  
  # Frase elegida segun cantidad de celdas vacias
  if(cantidad_na == 0) frase01 <- frase_armada01_A else frase01 <- frase_armada01_B
  
  ##############################################################################
  
  # Formato de la tabla02  
  columnas02 <- c("Número de categoría", 
                  paste0("Nombre de las categorías de '", colnames(minibase), "'"), 
                  "Frecuencia Absoluta", 
                  "Número de Orden en la base")
  
  tabla_fa <- table(minibase)
  numero_orden_cat <- c(1:length(tabla_fa))
  
  tabla02 <- matrix(NA, length(tabla_fa), length(columnas02))
  colnames(tabla02) <- columnas02
  
  tabla02[,1] <- numero_orden_cat
  tabla02[,2] <- names(tabla_fa)
  tabla02[,3] <- tabla_fa
  
  mostrar <- 5
  
  for(k in 1:length(tabla_fa)) {
    
    orden <- c(1:nrow(base))    
    esta_categoria <- names(tabla_fa)[k]
    dt_categoria <- base[,columna] == esta_categoria
    dt_categoria[is.na(dt_categoria)] <- FALSE
    pos <- orden[dt_categoria]
    
    
    if(length(pos) > mostrar) {
      pos <- pos[1:mostrar]
      texto <- paste0(pos, ", ")
      texto[length(texto)] <- paste0(texto[length(texto)], " ... (Solo se detallan los primeros ", mostrar, " datos)")
      texto <- paste0(texto, collapse = "")
      tabla02[k,4] <- texto
    }  else
      if(length(pos) <= mostrar && length(pos) > 1) {
        
        armado <- c(rep(", ", (length(pos) - 2)), " y ", "")
        
        texto <- paste0(pos, armado)
        texto <- paste0(texto, collapse = "")
        tabla02[k,4] <- texto
      }  else
        if(length(pos) <= mostrar && length(pos) == 1) {
          texto <- pos
          tabla02[k,4] <- texto
        }  
  }
  
  ##############################################################################
  
  frase02 <- "Observe si todas las categorías son correctas para la variable seleccionada."
  
  ##############################################################################
  # Return exitoso
#  salida <- list(tabla01, frase01, tabla02, frase02)
  
  # Cambiamos el orden de la salida
  salida <- list(tabla02, frase02, tabla01, frase01)
  return(salida)
  
  
}

################################################################################




control_1c_RMedic <- function(base = NULL, columna = NULL, decimales = 2){
  
  
  # Minibase
  minibase <- na.omit(base[columna])
  
  ##############################################################################
  
  # Cantidad de celdas vacias
  cantidad_na <- sum(is.na(base[,columna]))
  filas_base <- nrow(base)
  filas_minibase <- nrow(minibase)
  
  # Tabla para celdas vacias
  columnas01 <- c("Total de filas", "Celdas con datos", "Celdas vacías")
  tabla01 <- matrix(NA, 1, length(columnas01))  
  tabla01[1,] <- c(filas_base, filas_minibase, cantidad_na)
  colnames(tabla01) <- columnas01
  
  # Frases de celdas vacias
  frase_armada01_A <- paste0("Todas las celdas de la columna seleccionada presentan datos.
                              <br/>
                              Al utilizar esta variable el 'n' será ", filas_base, ".")
  
  frase_armada01_B <- paste0("La columna presenta celdas vacías. Al utilizar la columna seleccionada 
                              el 'n' será ", filas_minibase, ".")
  
  
  # Frase elegida segun cantidad de celdas vacias
  if(cantidad_na == 0) frase01 <- frase_armada01_A else frase01 <- frase_armada01_B
  
  ##############################################################################
  
  # Formato de la tabla02  
  columnas02 <- c("Detalle",
                  "Valor",
                  "Cantidad de datos detectados",
                  "Número de Orden en la base")
  
  filas02 <- c("Mínimo", "Máximo")
  
  orden <- 1:nrow(base)
  
  minimo <- min(minibase[,1])
  maximo <- max(minibase[,1])
  
  minimo <- round2(minimo, decimales)
  maximo <- round2(maximo, decimales)
  
  dt_min <- minimo == minibase[,1]
  dt_min[is.na(dt_min)] <- FALSE
  
  dt_max <- maximo == minibase[,1]
  dt_max[is.na(dt_max)] <- FALSE
  
  
  cantidad_min <- sum(dt_min)
  cantidad_max <- sum(dt_max)
  
  orden_min <- orden[dt_min]
  orden_max <- orden[dt_max]
  
  
  mostrar <- 5
  
  armado_orden <- list(orden_min, orden_max)
  texto_salida <- list()
  
  
  for(k in 1:length(armado_orden)) {
    
    pos <- armado_orden[[k]]
    texto_salida[[k]] <- c()
    
    # Si hay mas de lo que hay que mostrar...
    if(length(pos) > mostrar) {
      pos <- pos[1:mostrar]
      texto <- paste0(pos, ", ")
      texto[length(texto)] <- paste0(texto[length(texto)], " ... (Solo se detallan los primeros ", mostrar, " datos)")
      texto <- paste0(texto, collapse = "")
    }  else
      if(length(pos) <= mostrar && length(pos) > 1) {
        
        armado <- c(rep(", ", (length(pos) - 2)), " y ", "")
        
        texto <- paste0(pos, armado)
        texto <- paste0(texto, collapse = "")
      }   else
        if(length(pos) <= mostrar && length(pos) == 1) {
          texto <- pos
        } 
    
    texto_salida[[k]] <- texto
  }
  
  
  tabla02 <- matrix(NA, length(filas02), length(columnas02))
  colnames(tabla02) <- columnas02
  #  rownames(tabla02) <- filas02
  
  tabla02[1,] <- c(filas02[1], minimo, cantidad_min, texto_salida[[1]])
  tabla02[2,] <- c(filas02[2], maximo, cantidad_max, texto_salida[[2]])
  
  
  
  
  
  ##############################################################################
  
  frase02 <- "Observe si los valores mínimos y máximos de la variable tienen sentido."
  
  ##############################################################################
  # Return exitoso
  #  salida <- list(tabla01, frase01, tabla02, frase02)
  
  # Cambiamos el orden de la salida
  salida <- list(tabla02, frase02, tabla01, frase01)
  return(salida)
  
  
}


################################################################################



control_2q_RMedic <- function(base = NULL, columna = NULL){
  
  
  # Minibase
  minibase <- na.omit(base[columna])
  minibase[,1] <- as.character(minibase[,1])
  minibase[,2] <- as.character(minibase[,2])
  
  ##############################################################################
  
  # Cantidad de celdas vacias
   
  filas_base <- nrow(base)
  filas_minibase <- nrow(minibase)
  cantidad_na <- filas_base - filas_minibase
  
  # Tabla para celdas vacias
  columnas01 <- c("Total de filas", "Filas con ambos datos", "Filas con una o ambas celdas vacías")
  tabla01 <- matrix(NA, 1, length(columnas01))  
  tabla01[1,] <- c(filas_base, filas_minibase, cantidad_na)
  colnames(tabla01) <- columnas01
  
  # Frases de celdas vacias
  frase_armada01_A <- paste0("Todas las celdas de ambas columnas seleccionadas presentan datos.
                              <br/>
                              Al utilizar ambas variables variable el 'n' será ", filas_base, ".")
  
  frase_armada01_B <- paste0("Solo conforman la muestra los pares de datos completos de ambas variables. 
                              Al utilizar la columna seleccionada el 'n' será ", filas_minibase, ".")
  
  
  # Frase elegida segun cantidad de celdas vacias
  if(cantidad_na == 0) frase01 <- frase_armada01_A else frase01 <- frase_armada01_B
  
  ##############################################################################
  
  # Formato de la tabla02  
  tabla_fa <- table(minibase)
  tabla_fa <- as.data.frame(tabla_fa)
  colnames(tabla_fa) <- c(paste0("Categorías de la variable1 '", colnames(minibase)[1], "'"),
                          paste0("Categorías de la variable2 '", colnames(minibase)[2], "'"),
                          paste0("Frecuencia de la combianción de categorías")
  )
  
  tabla_fa[,1] <- as.character(tabla_fa[,1])
  tabla_fa[,2] <- as.character(tabla_fa[,2])
  
  nuevo_orden <- order(tabla_fa[,1])
  tabla_fa <- tabla_fa[nuevo_orden, ]
  
  
  numero_orden_cat <- c(1:nrow(tabla_fa))
  
  columnas02 <- c("Número de combinación", colnames(tabla_fa), "Número de Orden en la base de datos")
  tabla02 <- matrix(NA, nrow(tabla_fa), length(columnas02))
  colnames(tabla02) <- columnas02
  
  tabla02[,1] <- numero_orden_cat
  tabla02[,2] <- tabla_fa[,1]
  tabla02[,3] <- tabla_fa[,2]
  tabla02[,4] <- tabla_fa[,3]
  
  mostrar <- 5
  
  for(k in 1:nrow(tabla_fa)) {
    
    orden <- c(1:nrow(base))    
    esta_categoria1 <- tabla_fa[k,1]
    esta_categoria2 <- tabla_fa[k,2]
    
    dt_categoria1 <- base[,columna[1]] == esta_categoria1
    dt_categoria1[is.na(dt_categoria1)] <- FALSE
    
    dt_categoria2 <- base[,columna[2]] == esta_categoria2
    dt_categoria2[is.na(dt_categoria2)] <- FALSE
    
    dt_combinacion <- (as.numeric(dt_categoria1) + as.numeric(dt_categoria2)) == 2
    
    
    pos <- orden[dt_combinacion]
    
    if(length(pos) == 0){
      texto <- "No hay filas con esta combinación dentro de la base."
      tabla02[k,5] <- texto
    } else
      if(length(pos) > mostrar) {
        pos <- pos[1:mostrar]
        texto <- paste0(pos, ", ")
        texto[length(texto)] <- paste0(texto[length(texto)], " ... (Solo se detallan los primeros ", mostrar, " datos)")
        texto <- paste0(texto, collapse = "")
        tabla02[k,5] <- texto
      }  else
        if(length(pos) <= mostrar && length(pos) > 1) {
          
          armado <- c(rep(", ", (length(pos) - 2)), " y ", "")
          
          texto <- paste0(pos, armado)
          texto <- paste0(texto, collapse = "")
          tabla02[k,5] <- texto
        }  else
          if(length(pos) <= mostrar && length(pos) == 1) {
            texto <- pos
            tabla02[k,5] <- texto
          }  
  }
  
  ##############################################################################
  
  frase02 <- "Observe si todas las categorías de cada variable y la combinación de categorías son correctas."
  
  ##############################################################################
  # Return exitoso
  #  salida <- list(tabla01, frase01, tabla02, frase02)
  
  # Cambiamos el orden de la salida
  salida <- list(tabla02, frase02, tabla01, frase01)
  return(salida)
  
  
}

################################################################################



control_2c_RMedic <- function(base = NULL, columna = NULL, decimales = 2){
  
  
  # Minibase
  minibase <- na.omit(base[columna])
  
  ##############################################################################
  
  # Cantidad de celdas vacias
  filas_base <- nrow(base)
  filas_minibase <- nrow(minibase)
  cantidad_na <- filas_base - filas_minibase
  
  # Tabla para celdas vacias
  columnas01 <- c("Total de filas", "Filas con ambos datos", "Filas con una o ambas celdas vacías")
  tabla01 <- matrix(NA, 1, length(columnas01))  
  tabla01[1,] <- c(filas_base, filas_minibase, cantidad_na)
  colnames(tabla01) <- columnas01
  
  # Frases de celdas vacias
  frase_armada01_A <- paste0("En las dos columnas seleccionadas todas las filas presentan datos.
                              <br/>
                              Al utilizar ambas variables de manera simultánea el 'n' será ", filas_base, ".")
  
  frase_armada01_B <- paste0("Solo conforman la muestra los pares de datos completos de ambas variables. 
                              Al utilizar ambas columnas de manera simultánea el 'n' será ", filas_minibase, ".")
  
  
  # Frase elegida segun cantidad de celdas vacias
  if(cantidad_na == 0) frase01 <- frase_armada01_A else frase01 <- frase_armada01_B
  
  ##############################################################################
  
  # Formato de la tabla02 y tabla03
  
  mis_tablas <- list()
  
  # Una tabla para cada variable
  for (h in 1:2) {
    
    columnas02 <- c("Detalle",
                    "Valor",
                    "Cantidad de datos detectados",
                    "Número de Orden en la base")
    
    filas02 <- c("Mínimo", "Máximo")
    
    orden <- 1:nrow(base)
    
    minimo <- min(minibase[,h])
    maximo <- max(minibase[,h])
    
    minimo <- round2(minimo, decimales)
    maximo <- round2(maximo, decimales)
    
    dt_min <- minimo == minibase[,h]
    dt_min[is.na(dt_min)] <- FALSE
    
    dt_max <- maximo == minibase[,h]
    dt_max[is.na(dt_max)] <- FALSE
    
    
    cantidad_min <- sum(dt_min)
    cantidad_max <- sum(dt_max)
    
    orden_min <- orden[dt_min]
    orden_max <- orden[dt_max]
    
    
    mostrar <- 5
    
    armado_orden <- list(orden_min, orden_max)
    texto_salida <- list()
    
    
    for(k in 1:length(armado_orden)) {
      
      pos <- armado_orden[[k]]
      texto_salida[[k]] <- c()
      
      # Si hay mas de lo que hay que mostrar...
      if(length(pos) > mostrar) {
        pos <- pos[1:mostrar]
        texto <- paste0(pos, ", ")
        texto[length(texto)] <- paste0(texto[length(texto)], " ... (Solo se detallan los primeros ", mostrar, " datos)")
        texto <- paste0(texto, collapse = "")
      }  else
        if(length(pos) <= mostrar && length(pos) > 1) {
          
          armado <- c(rep(", ", (length(pos) - 2)), " y ", "")
          
          texto <- paste0(pos, armado)
          texto <- paste0(texto, collapse = "")
        }   else
          if(length(pos) <= mostrar && length(pos) == 1) {
            texto <- pos
          } 
      
      texto_salida[[k]] <- texto
    }
    
    
    tabla02 <- matrix(NA, length(filas02), length(columnas02))
    colnames(tabla02) <- columnas02
    #  rownames(tabla02) <- filas02
    
    tabla02[1,] <- c(filas02[1], minimo, cantidad_min, texto_salida[[1]])
    tabla02[2,] <- c(filas02[2], maximo, cantidad_max, texto_salida[[2]])
    
    mis_tablas[[h]] <- tabla02
    
  }
  names(mis_tablas) <- colnames(minibase)
  tabla02 <- mis_tablas[[1]]
  tabla03 <- mis_tablas[[2]]
  ##############################################################################
  
  frase02 <- "Observe si los valores mínimos y máximos de la variable tienen sentido."
  frase03 <- frase02
  ##############################################################################
  # Return exitoso
  #  salida <- list(tabla01, frase01, tabla02, frase02)
  
  # Cambiamos el orden de la salida
  salida <- list(tabla02, frase02, tabla03, frase03, tabla01, frase01)
  return(salida)
  
  
}



################################################################################



control_qc_RMedic <- function(base = NULL, columna = NULL, decimales = 2){
  
  
  # Minibase
  minibase <- na.omit(base[columna])
  
  ##############################################################################
  
  # Cantidad de celdas vacias
  filas_base <- nrow(base)
  filas_minibase <- nrow(minibase)
  cantidad_na <- filas_base - filas_minibase
  
  # Tabla para celdas vacias
  columnas01 <- c("Total de filas", "Filas con ambos datos", "Filas con una o ambas celdas vacías")
  tabla01 <- matrix(NA, 1, length(columnas01))  
  tabla01[1,] <- c(filas_base, filas_minibase, cantidad_na)
  colnames(tabla01) <- columnas01
  
  # Frases de celdas vacias
  frase_armada01_A <- paste0("En las dos columnas seleccionadas todas las filas presentan datos.
                              <br/>
                              Al utilizar ambas variables de manera simultánea el 'n' será ", filas_base, ".")
  
  frase_armada01_B <- paste0("Solo conforman la muestra los pares de datos completos de ambas variables. 
                              Al utilizar ambas columnas de manera simultánea el 'n' será ", filas_minibase, ".")
  
  
  # Frase elegida segun cantidad de celdas vacias
  if(cantidad_na == 0) frase01 <- frase_armada01_A else frase01 <- frase_armada01_B
  
  ##############################################################################
  
  # Formato de la tabla02 y tabla03
  
  if(FALSE){
  mis_tablas <- list()
  
  
  # Una tabla para cada variable
  for (h in 1:2) {
    
    columnas02 <- c("Detalle",
                    "Valor",
                    "Cantidad de datos detectados",
                    "Número de Orden en la base")
    
    filas02 <- c("Mínimo", "Máximo")
    
    orden <- 1:nrow(base)
    
    minimo <- min(minibase[,h])
    maximo <- max(minibase[,h])
    
    minimo <- round2(minimo, decimales)
    maximo <- round2(maximo, decimales)
    
    dt_min <- minimo == minibase[,h]
    dt_min[is.na(dt_min)] <- FALSE
    
    dt_max <- maximo == minibase[,h]
    dt_max[is.na(dt_max)] <- FALSE
    
    
    cantidad_min <- sum(dt_min)
    cantidad_max <- sum(dt_max)
    
    orden_min <- orden[dt_min]
    orden_max <- orden[dt_max]
    
    
    mostrar <- 5
    
    armado_orden <- list(orden_min, orden_max)
    texto_salida <- list()
    
    
    for(k in 1:length(armado_orden)) {
      
      pos <- armado_orden[[k]]
      texto_salida[[k]] <- c()
      
      # Si hay mas de lo que hay que mostrar...
      if(length(pos) > mostrar) {
        pos <- pos[1:mostrar]
        texto <- paste0(pos, ", ")
        texto[length(texto)] <- paste0(texto[length(texto)], " ... (Solo se detallan los primeros ", mostrar, " datos)")
        texto <- paste0(texto, collapse = "")
      }  else
        if(length(pos) <= mostrar && length(pos) > 1) {
          
          armado <- c(rep(", ", (length(pos) - 2)), " y ", "")
          
          texto <- paste0(pos, armado)
          texto <- paste0(texto, collapse = "")
        }   else
          if(length(pos) <= mostrar && length(pos) == 1) {
            texto <- pos
          } 
      
      texto_salida[[k]] <- texto
    }
    
    
    tabla02 <- matrix(NA, length(filas02), length(columnas02))
    colnames(tabla02) <- columnas02
    #  rownames(tabla02) <- filas02
    
    tabla02[1,] <- c(filas02[1], minimo, cantidad_min, texto_salida[[1]])
    tabla02[2,] <- c(filas02[2], maximo, cantidad_max, texto_salida[[2]])
    
    mis_tablas[[h]] <- tabla02
    
  }
  names(mis_tablas) <- colnames(minibase)
  tabla02 <- mis_tablas[[1]]
  tabla03 <- mis_tablas[[2]]
  }
  tabla03 <- NA
  ##############################################################################
  
  frase02 <- "Observe si los valores mínimos de la variable en cada categoría tienen sentido."
  frase03 <- "Observe si los valores máximos de la variable en cada categoría tienen sentido."
  ##############################################################################
  # Return exitoso
  #  salida <- list(tabla01, frase01, tabla02, frase02)
  # # # #NUEVO
  
#  minibase <- database[selected_cols]
#  minibase <- na.omit(minibase)
  
  
  tabla02 <- cbind.data.frame("Cantidad de datos" = tapply(minibase[,2], minibase[,1], length),
                              "Mínimo" = tapply(minibase[,2], minibase[,1], min))
  
  # Agregar columna con la posición del valor mínimo
  tabla02$"Cantidad Mínimo" <- tapply(minibase[,2], minibase[,1], function(x){ 
    mis_valores <- which.min(x)
    length(mis_valores)
  }
  )
  
  
  # Agregar columna con la posición del valor mínimo
  tabla02$"Posición Mínimo" <- tapply(minibase[,2], minibase[,1], function(x){ 
    mis_valores <- which.min(x)
    el_limite <- 5
    dt_muchos <- length(mis_valores)>=el_limite
    pos_max <- ifelse(dt_muchos, 5, length(mis_valores))
    mis_valores <- mis_valores[1:pos_max]
    mis_valores <- paste0(mis_valores, collapse = ", ")
    if(dt_muchos) mis_valores <- paste0(mis_valores, " ... (Solo primeros ", pos_max, "datos.")
    mis_valores
    
    }
    )
  
  
  tabla02 <- cbind.data.frame(rownames(tabla02), tabla02)
  colnames(tabla02)[1] <- "Categorías"
  
  tabla04 <- cbind.data.frame("Cantidad de datos" = tapply(minibase[,2], minibase[,1], length),
                              "Máximo" = tapply(minibase[,2], minibase[,1], max))
  
  
  # Agregar columna con la posición del valor mínimo
  tabla04$"Cantidad Máximo" <- tapply(minibase[,2], minibase[,1], function(x){ 
    mis_valores <- which.max(x)
    length(mis_valores)
  }
  )
  
  
  # Agregar columna con la posición del valor mínimo
  tabla04$"Posición Máximo" <- tapply(minibase[,2], minibase[,1], function(x){ 
    mis_valores <- which.max(x)
    el_limite <- 5
    dt_muchos <- length(mis_valores)>=el_limite
    pos_max <- ifelse(dt_muchos, 5, length(mis_valores))
    mis_valores <- mis_valores[1:pos_max]
    mis_valores <- paste0(mis_valores, collapse = ", ")
    if(dt_muchos) mis_valores <- paste0(mis_valores, " ... (Solo primeros ", pos_max, "datos.")
    mis_valores
    
  }
  )
  
  tabla04 <- cbind.data.frame(rownames(tabla04), tabla04)
  colnames(tabla04)[1] <- "Categorías"
  
  frase04 <- "Observe si los valores máximos de la variable en cada categoría tienen sentido."
  
  # # # # # 
  # Cambiamos el orden de la salida
  salida <- list(tabla02, frase02, tabla01, frase03, tabla03, frase01, tabla04, frase04)
  return(salida)
  
  
}



################################################################################

CharacterALL <- function(the_data_frame){
  
  out <- apply(the_data_frame, 2, as.character, simplify = F)
  out <- do.call(cbind.data.frame, out)
  #if(ncol(out) == 1) out <- t(out)
  #out <- as.data.frame(out)
  return(out)
  
}

##################################################################################

Distribucion.Normal <- function(media_poblacional, varianza_poblacional = NA, 
                                desvio_poblacional = NA, 
                                opciones, 
                                intervalo,
                                color_variable,
                                var1 = NA, var2 = NA, var3 = NA, var4 = NA, 
                                z1 = NA, z2 = NA, z3 = NA, z4 = NA, 
                                probabilidad_externo = NA, porcentaje_externo = NA, decimals = 2){
  
  
  if(is.na(desvio_poblacional)) desvio_poblacional <- sqrt(varianza_poblacional)
  if(is.na(varianza_poblacional)) varianza_poblacional <- desvio_poblacional^2
  
  # opciones puede tomar 4 valores: 
  # 1) "original"
  # 2) "valor_z"
  # 3) "probabilidad"
  # 4) "porcentaje"
  
  
  # intervalo puede tomar 3 valores:
  # 1) "menor"
  # 2) "mayor"
  # 3) "entre
  
  # Tabla interna 01
  tabla_interna01 <- data.frame(media_poblacional, varianza_poblacional, desvio_poblacional,
                                opciones, intervalo, color_variable)
  
  # Tabla Interna 02
  {
    
    # Normal Completa
    min_distribucion <- -4
    max_distribucion <- 4
    h <- (max_distribucion - min_distribucion)/1000
    
    marcas_z <- min_distribucion:max_distribucion
    marcas_variable <- marcas_z*desvio_poblacional + media_poblacional
    
    if(1 == 1){
      # Si los valores ingresados son de la variable original
      if(opciones == "original") {
        if(intervalo == "menor"){
          
          if(is.na(var1)) stop("Falta ingresar var2")
          
          var_izquierdo <- min(marcas_variable)
          var_derecho <- var1
          
          z_izquierdo <- min_distribucion
          z_derecho <- (var_derecho - media_poblacional)/desvio_poblacional
          
          la_probabilidad <- pnorm(z_derecho, mean = 0, sd = 1, lower.tail = T)
          el_porcentaje <- la_probabilidad*100
          
          frase01 <- "El valor de la variable original es _VariableDerecho_.<br/>
                      El valor estandarizado es z = _z_derecho_."
          
          frase02 <- "La probabilidad de pacientes con valores de la variable original menores a _VariableDerecho_ es _probabilidad_.<br/>
                      La probabilidad de pacientes con valores z menores a _z_derecho_ es _probabilidad_."
          
          frase03 <- "El valor estandarizado es z = _z_derecho_."
          
          frase04 <- "La probabilidad de valores z menores a _z_derecho_ es _probabilidad_."
          
        } else
          if(intervalo == "mayor"){
            
            if(is.na(var2)) stop("Falta ingresar var2")
            var_izquierdo <- var2
            var_derecho <- max(marcas_variable)
            
            z_izquierdo <- (var_izquierdo - media_poblacional)/desvio_poblacional
            z_derecho <- max_distribucion
            
            la_probabilidad <- pnorm(z_izquierdo, mean = 0, sd = 1, lower.tail = F)
            el_porcentaje <- la_probabilidad*100
            
            frase01 <- "El valor de la variable original es _VariableIzquierdo_.<br/>
                        El valor estandarizado es z = _z_izquierdo_."
            
            frase02 <- "La probabilidad de pacientes con valores de la variable original mayores a _VariableIzquierdo_ es _probabilidad_.<br/>
                        La probabilidad de pacientes con valores z mayores a _z_izquierdo_ es _probabilidad_."
            
            frase03 <- "El valor estandarizado es z = _z_izquierdo_."
            
            frase04 <- "La probabilidad de valores z mayores a _z_izquierdo_ es _probabilidad_."
            
          } else
            if(intervalo == "entre"){
              
              if(is.na(var3)) return(NULL)
              if(is.na(var4)) return(NULL)
              
              var_izquierdo <- var3
              var_derecho <- var4
              
              z_izquierdo <- (var_izquierdo - media_poblacional)/desvio_poblacional
              z_derecho <- (var_derecho - media_poblacional)/desvio_poblacional
              
              p_der <- pnorm(z_derecho, mean = 0, sd = 1, lower.tail = T)
              p_izq <- pnorm(z_izquierdo, mean = 0, sd = 1, lower.tail = T)
              la_probabilidad <- p_der - p_izq
              el_porcentaje <- la_probabilidad*100
              
              frase01 <- "Los valores de la variable original son _VariableIzquierdo_ y _VariableDerecho_.<br/>
                          Los valores estandarizados son _z_izquierdo_ y _z_derecho_.
                          "
              
              frase02 <- "La probabilidad de pacientes con valores de la variable original entre _VariableIzquierdo_ y _VariableDerecho_ es _probabilidad_.<br/>
                          La probabilidad de pacientes con valores z entre _z_izquierdo_ y _z_derecho_ es _probabilidad_."
              
              frase03 <- "Los valores estandarizados son z1 = _z_izquierdo_ y z2 = _z_derecho_."
              
              frase04 <- "La probabilidad contenida entre los valores estadarizados z de _z_izquierdo_ y _z_derecho_ es _probabilidad_."
            }
      } else
        if(opciones == "valor_z") {
          if(intervalo == "menor"){
            
            if(is.na(z1)) return(NULL)
            
            z_izquierdo <- min_distribucion
            z_derecho <- as.numeric(as.character(z1))
            
            var_izquierdo <- z_izquierdo*desvio_poblacional + media_poblacional
            var_derecho <- z_derecho*desvio_poblacional + media_poblacional
            
            la_probabilidad <- pnorm(z_derecho, mean = 0, sd = 1, lower.tail = T)
            el_porcentaje <- la_probabilidad*100
            
            frase01 <- "El valor estandarizado es z = _z_derecho_.<br/>
                        El valor de la variable original es _VariableDerecho_."
            
            frase02 <- "La probabilidad de pacientes con valores z menores a _z_derecho_ es _probabilidad_.<br/>
                        La probabilidad de pacientes con valores de la variable original menores a _VariableDerecho_ es _probabilidad_."
            
            frase03 <- "El valor estandarizado es z = _z_derecho_."
            
            frase04 <- "La probabilidad de valores z menores a _z_derecho_ es _probabilidad_."
            
          } else
            if(intervalo == "mayor"){
              
              if(is.na(z2)) return(NULL)
              
              z_izquierdo <- z2
              z_derecho <- max_distribucion
              
              var_izquierdo <- z_izquierdo*desvio_poblacional + media_poblacional
              var_derecho <- z_derecho*desvio_poblacional + media_poblacional
              
              la_probabilidad <- pnorm(z_izquierdo, mean = 0, sd = 1, lower.tail = F)
              el_porcentaje <- la_probabilidad*100
              
              frase01 <- "El valor estandarizado es z = _z_izquierdo_.<br/>
                          El valor de la variable original es _VariableIzquierdo_."
              
              frase02 <- "La probabilidad de pacientes con valores z mayores a _z_izquierdo_ es _probabilidad_.<br/>
                          La probabilidad de pacientes con valores de la variable original mayores a _VariableIzquierdo_ es _probabilidad_."
              
              frase03 <- "El valor estandarizado es z = _z_izquierdo_."
              
              frase04 <- "La probabilidad de valores z mayores a _z_izquierdo_ es _probabilidad_."
              
            } else
              if(intervalo == "entre"){
                
                if(is.na(z3)) return(NULL)
                if(is.na(z4)) return(NULL)
                
                z_izquierdo <- z3
                z_derecho <- z4
                
                var_izquierdo <- z_izquierdo*desvio_poblacional + media_poblacional
                var_derecho <- z_derecho*desvio_poblacional + media_poblacional
                
                p_der <- pnorm(z_derecho, mean = 0, sd = 1, lower.tail = T)
                p_izq <- pnorm(z_izquierdo, mean = 0, sd = 1, lower.tail = T)
                la_probabilidad <- p_der - p_izq
                el_porcentaje <- la_probabilidad*100
                
                frase01 <- "Los valores estandarizados son z1 = _z_izquierdo_ y z2 = _z_derecho_.<br/>
                            Los valores de la variable original son _VariableIzquierdo_ y _VariableDerecho_."
                
                frase02 <- "La probabilidad de pacientes con valores z entre _z_izquierdo_ y _z_derecho_ es _probabilidad_.<br/>
                            La probabilidad de pacientes con valores de la variable original entre _VariableIzquierdo_ y _VariableDerecho_ es _probabilidad_." 
                
                frase03 <- "Los valores estandarizados son z1 = _z_izquierdo_ y z2 = _z_derecho_."
                
                frase04 <- "La probabilidad contenida entre los valores estadarizados z de _z_izquierdo_ y _z_derecho_ es _probabilidad_."
              }
        } else
          if(opciones == "probabilidad") {
            if(intervalo == "menor"){
              
              if(is.na(probabilidad_externo)) return(NULL)
              
              la_probabilidad <- as.numeric(as.character(probabilidad_externo))
              
              z_izquierdo <- min_distribucion
              z_derecho <- qnorm(la_probabilidad, mean = 0, sd = 1, lower.tail = T)
              
              var_izquierdo <- z_izquierdo*desvio_poblacional + media_poblacional
              var_derecho <- z_derecho*desvio_poblacional + media_poblacional
              
              el_porcentaje <- la_probabilidad*100
              
              frase01 <- "La probabilidad es _probabilidad_.<br/>
                          El porcentaje es _porcentaje_%."
              
              frase02 <- "El valor estandarizado z = _z_derecho_ acumula hacia la izquierda un valor de probabilidad de _probabilidad_.<br/>
                          El valor de la variable original _VariableDerecho_ acumula hacia la izquierda un valor de probabilidad de _probabilidad_."
              
              frase03 <- frase01
              
              frase04 <- "El valor estandarizado z = _z_derecho_ acumula hacia la izquierda un valor de probabilidad de _probabilidad_."
              
            } else
              if(intervalo == "mayor"){
                
                if(is.na(probabilidad_externo)) return(NULL)
                
                la_probabilidad <- probabilidad_externo
                
                z_izquierdo <- qnorm(probabilidad_externo, mean = 0, sd = 1, lower.tail = F)
                z_derecho <- max_distribucion
                
                var_izquierdo <- z_izquierdo*desvio_poblacional + media_poblacional
                var_derecho <- z_derecho*desvio_poblacional + media_poblacional
                
                el_porcentaje <- la_probabilidad*100
                
                frase01 <- "La probabilidad es _probabilidad_.<br/>
                            El porcentaje es _porcentaje_%."
                
                frase02 <- "El valor estandarizado z = _z_izquierdo_ acumula hacia la derecha una probabilidad de _probabilidad_.<br/>
                            El valor de la variable original _VariableIzquierdo_ acumula hacia la derecha una probabilidad de _probabilidad_."
                
                frase03 <- frase01
                
                frase04 <- "El valor estandarizado z = _z_izquierdo_ acumula hacia la derecha una probabilidad de _probabilidad_."
                
              } else
                if(intervalo == "entre"){
                  
                  if(is.na(probabilidad_externo)) return(NULL)
                  la_probabilidad <- probabilidad_externo
                  el_resto <- 1 - la_probabilidad
                  la_mitad <- el_resto/2
                  
                  z_izquierdo <- qnorm(la_mitad, mean = 0, sd = 1, lower.tail = T)
                  z_derecho <- qnorm(la_mitad, mean = 0, sd = 1, lower.tail = F)
                  
                  var_izquierdo <- z_izquierdo*desvio_poblacional + media_poblacional
                  var_derecho <- z_derecho*desvio_poblacional + media_poblacional
                  
                  el_porcentaje <- la_probabilidad*100
                  
                  frase01 <- "La probabilidad es _probabilidad_.<br/>
                              El porcentaje es _porcentaje_%."
                  
                  frase02 <- "Entre los valores estandarizados z1 = _z_izquierdo_ y z2 = _z_derecho_ se define una probabilidad de _probabilidad_.<br/>
                              Entre los valores de la variable original _VariableIzquierdo_ y _VariableDerecho_ se define una probabilidad de _probabilidad_."
                  
                  frase03 <- frase01
                  
                  frase04 <- "Entre los valores estandarizados z1 = _z_izquierdo_ y z2 = _z_derecho_ se define una probabilidad de _probabilidad_."
                  
                }
          } else
            if(opciones == "porcentaje") {
              if(intervalo == "menor"){
                
                if(is.na(porcentaje_externo)) return(NULL)
                
                el_porcentaje <- porcentaje_externo
                la_probabilidad <- el_porcentaje/100
                
                z_izquierdo <- min_distribucion
                z_derecho <- qnorm(la_probabilidad, mean = 0, sd = 1, lower.tail = T)
                
                var_izquierdo <- z_izquierdo*desvio_poblacional + media_poblacional
                var_derecho <- z_derecho*desvio_poblacional + media_poblacional
                
                el_porcentaje <- la_probabilidad*100
                
                frase01 <- "El porcentaje es _porcentaje_.<br/>
                            La probabilidad es _probabilidad_.<br/>
                            El valor estandarizado es z = _z_derecho_.<br/>
                            El valor de la variable original es _VariableDerecho_.
                           "
                
                frase02 <- "La probabilidad de pacientes con valores de la variable original menores a _VariableDerecho_ es _probabilidad_.<br/>
                            La probabilidad de pacientes con valores z menores a _z_derecho_ es _probabilidad_."
                
                frase03 <- "El porcentaje es _porcentaje_.<br/>
                            La probabilidad es _probabilidad_.<br/>
                            El valor estandarizado es z = _z_derecho_.<br/>
                           "
                frase04 <- "La probabilidad de pacientes con valores z menores a _z_derecho_ es _probabilidad_."
                
              } else
                if(intervalo == "mayor"){
                  
                  if(is.na(porcentaje_externo)) return(NULL)
                  
                  el_porcentaje <- porcentaje_externo
                  la_probabilidad <- el_porcentaje/100
                  
                  z_izquierdo <- qnorm(probabilidad_externo, mean = 0, sd = 1, lower.tail = F)
                  z_derecho <- max_distribucion
                  
                  var_izquierdo <- z_izquierdo*desvio_poblacional + media_poblacional
                  var_derecho <- z_derecho*desvio_poblacional + media_poblacional
                  
                  el_porcentaje <- la_probabilidad*100
                  
                  frase01 <- "El porcentaje es _porcentaje_.<br/>
                              La probabilidad es _probabilidad_.<br/>
                              El valor estandarizado es z = _z_izquierdo_.<br/>
                              El valor de la variable original es _VariableIzquierdo_."
                  
                  frase02 <- "La probabilidad de pacientes con valores de la variable original mayores a _VariableIzquierdo_ es _probabilidad_.<br/>
                              La probabilidad de pacientes con valores z mayores a _z_izquierdo_ es _probabilidad_."
                  
                  frase03 <- "El porcentaje es _porcentaje_.<br/>
                              La probabilidad es _probabilidad_.<br/>
                              El valor estandarizado es z = _z_izquierdo_."
                  
                  frase04 <- "La probabilidad de pacientes con valores z mayores a _z_izquierdo_ es _probabilidad_."
                  
                  
                } else
                  if(intervalo == "entre"){
                    
                    if(is.na(porcentaje_externo)) return(NULL)
                    
                    el_porcentaje <- porcentaje_externo
                    la_probabilidad <- el_porcentaje/100
                    el_resto <- 1 - la_probabilidad
                    la_mitad <- el_resto/2
                    
                    z_izquierdo <- qnorm(la_mitad, mean = 0, sd = 1, lower.tail = T)
                    z_derecho <- qnorm(la_mitad, mean = 0, sd = 1, lower.tail = F)
                    
                    var_izquierdo <- z_izquierdo*desvio_poblacional + media_poblacional
                    var_derecho <- z_derecho*desvio_poblacional + media_poblacional
                    
                    el_porcentaje <- la_probabilidad*100
                    
                    frase01 <- "El porcentaje es _porcentaje_.<br/>
                                La probabilidad es _probabilidad_.<br/>
                                Los valores estandarizados son z1 = _z_izquierdo_ y z2 = _z_derecho_.<br/>
                                Los valores de la variable original son _VariableIzquierdo_ y _VariableDerecho_."
                    
                    frase02 <- "La probabilidad de pacientes con valores de la variable original entre _VariableIzquierdo_ y _VariableDerecho_ es _probabilidad_.<br/>
                                La probabilidad de pacientes con valores z entre _z_izquierdo_ y _z_derecho_ es _probabilidad_." 
                    
                    frase03 <- "El porcentaje es _porcentaje_.<br/>
                                La probabilidad es _probabilidad_.<br/>
                                Los valores estandarizados son z1 = _z_izquierdo_ y z2 = _z_derecho_."
                    
                    frase04 <- "La probabilidad de pacientes con valores z entre _z_izquierdo_ y _z_derecho_ es _probabilidad_." 
                    
                    
                  }
            }
      
      
      Pattern <- c("_VariableIzquierdo_", "_VariableDerecho_", "_z_izquierdo_", "_z_derecho_",
                   "_probabilidad_", "_porcentaje_")
      
      Replacement <- c(var_izquierdo, var_derecho, z_izquierdo, z_derecho,
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
    
    
    
    las_columnas <- c("Variable", "Z", "Probabilidad", "Porcentaje", 
                      "Frase01", "Frase02", "Frase03", "Frase04")
    las_filas <- c("Izquierda", "Derecha")
    
    armado <- as.data.frame(matrix(NA, length(las_filas), length(las_columnas)))
    colnames(armado) <- las_columnas
    rownames(armado) <- las_filas
    
    armado$"Variable" <- c(var_izquierdo, var_derecho)
    armado$"Z" <- c(z_izquierdo, z_derecho)
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
    tabla <- tabla[c(1:3)]
    
    # tabla <- as.matrix(tabla)
    colnames(tabla) <- c("Media Poblacional", "Varianza Poblacional", "Desvío Poblacional")
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
  
  # Tabla Externa03 
  {
    tabla <- tabla_externa02
    if(nrow(tabla) == 1) tabla <-   tabla[-1] else
      if(ncol(tabla) == 2) tabla <-   tabla[-2]
    tabla_externa03 <- tabla
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
    
    # media_poblacional <- tabla_interna01[1,1]
    # desvio_poblacional <- tabla_interna01[1,3]
    # Parametros
    # media_poblacional    <- 0
    # desvio_poblacional <- 1
    
    color_variable <- tabla_interna01$"color_variable"
    decimals <- decimals
    
    z_izquierdo <- tabla_interna02$"Z"[1] 
    z_derecho <- tabla_interna02$"Z"[2]
    
    var_izquierdo <- tabla_interna02$"Variable"[1] 
    var_derecho <- tabla_interna02$"Variable"[2]
    
    
    
    
    # Normal Completa
    min_distribucion <- -4
    max_distribucion <- 4
    h <- (max_distribucion - min_distribucion)/10000
    
    marcas_z <- min_distribucion:max_distribucion
    marcas_variable <- (marcas_z*desvio_poblacional) + media_poblacional
    marcas_variable <- round2(marcas_variable, decimals)
    
    
    # # Rango Acotado
    # lower.x <- -0.0
    # upper.x <-  2.1
    lower.x <- z_izquierdo
    upper.x <-  z_derecho
    
    # La Campana 
    x  <- seq(from = min_distribucion, to = max_distribucion, by = h)
    y <- dnorm(x = x, mean = 0, sd = 1)
    data_distribucion <- data.frame(x = x, y = y)
    
    # Lineas Limites
    linea_izquierda.x <- c(lower.x, lower.x)
    linea_izquierda.y <- c(0, dnorm( x = lower.x, mean = 0, sd = 1))
    data_linea_izquierda <- data.frame(x = linea_izquierda.x, y = linea_izquierda.y)
    
    linea_derecha.x <- c(upper.x, upper.x)
    linea_derecha.y <- c(0, dnorm( x = upper.x, mean = 0, sd = 1))
    data_linea_derecha <- data.frame(x = linea_derecha.x, y = linea_derecha.y)
    
    
    # El poligono
    x_mod <- seq(lower.x, upper.x, by = h)
    y_mod  <- dnorm(x = x_mod, mean = 0, sd = 1)
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
      # scale_x_continuous(name = "Variable Z", breaks = marcas_z, labels = marcas_z) +
      geom_polygon(data = data_poligono, fill = color_variable, size=2) + 
      geom_line(data = data_linea_izquierda, colour = color_variable, size=2) + 
      geom_line(data = data_linea_derecha,   colour = color_variable, size=2) + 
      geom_line(size=2)+ theme(text = element_text(size = 20))    
    
    grafico01 <- grafico_general +
      scale_x_continuous(name = "Variable Z", breaks = marcas_z, labels = marcas_z)
    
    grafico02 <- grafico_general +
      scale_x_continuous(name = "Variable Original", breaks = marcas_z, labels =marcas_variable)
    
    if (1 == 2){
      par(mai=c(1,3,1,1), mar=(c(5,9,4,2)+0.1), mgp=c(4,1,0))
      plot(x, y ,type="l", lwd=4, col="black", 
           xlim = c(min_distribucion, max_distribucion), 
           ylim=c(0,0.5),
           xlab = "Variable Z",
           ylab = "Frecuencia Relativa",
           axes = F,
           cex.lab = 2)
      
      
      axis(side = 2,  las=1, cex.axis=2) # Eje Y
      axis(side = 1, at = marcas_z, labels = marcas_variable , las=1, cex.axis=2) # Eje X
      
      # Grafico Acotado
      x_mod <- seq(lower.x, upper.x, by = h)
      y_mod  <- dnorm(x = x_mod, mean = 0, sd = 1)
      polygon(c(lower.x, x_mod, upper.x), c(0, y_mod, 0), col = color_variable)
      lines(x_mod, y_mod, col="black", lwd=4)
      
    }
    
  }
  
  
  # Salida General
  General <- Hmisc::llist(tabla_interna01, tabla_interna02, tabla_externa01, tabla_externa02,
                          tabla_externa03,
                          frase01, frase02, frase03, frase04, grafico01, grafico02)
  
  RMedic <- Hmisc::llist(tabla_externa01, tabla_externa02, tabla_externa03,
                         frase01, frase02,
                         frase03, frase04, grafico01, grafico02)
  
  out <- Hmisc::llist(General, RMedic)
  return(out)
}




###########################################################################################


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
          
          frase01 <- "El valor estandarizado es t = _t_derecho_."
          
          frase02 <- "La probabilidad de pacientes con valores t menores a _t_derecho_ es _probabilidad_."
          
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
            
            frase01 <- "El valor estandarizado es t = _t_izquierdo_."
            
            frase02 <- "La probabilidad de pacientes con valores t mayores a _t_izquierdo_ es _probabilidad_."
            
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
              
              frase01 <- "Los valores estandarizados son _t_izquierdo_ y _t_derecho_.
                          "
              
              frase02 <- "La probabilidad de pacientes con valores t entre _t_izquierdo_ y _t_derecho_ es _probabilidad_."
              
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
            
            frase01 <- "El valor estandarizado es t = _t_derecho_."
            
            frase02 <- "La probabilidad de pacientes con valores t menores a _t_derecho_ es _probabilidad_."
            
            frase03 <- "El valor estandarizado es t = _t_derecho_."
            
            frase04 <- "La probabilidad de valores t menores a _t_derecho_ es _probabilidad_."
            
          } else
            if(intervalo == "mayor"){
              
              if(is.na(t2)) return(NULL)
              
              t_izquierdo <- t2
              t_derecho <- max_distribucion
              
              var_izquierdo <- t_izquierdo*desvio_muestral + media_muestral
              var_derecho <- t_derecho*desvio_muestral + media_muestral
              
              la_probabilidad <- pt(q = t_izquierdo, df = gl, lower.tail = F)
              el_porcentaje <- la_probabilidad*100
              
              frase01 <- "El valor estandarizado es t = _t_izquierdo_."
              
              frase02 <- "La probabilidad de pacientes con valores t mayores a _t_izquierdo_ es _probabilidad_."
              
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
                
                p_der <- pt(q = t_derecho, df = gl, lower.tail = T)
                p_izq <- pt(q = t_izquierdo, df = gl, lower.tail = T)
                
                la_probabilidad <- p_der - p_izq
                el_porcentaje <- la_probabilidad*100
                
                frase01 <- "Los valores estandarizados son t1 = _t_izquierdo_ y t2 = _t_derecho_."
                
                frase02 <- "La probabilidad de pacientes con valores t entre _t_izquierdo_ y _t_derecho_ es _probabilidad_." 
                
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
              
              frase02 <- "El valor estandarizado t = _t_derecho_ acumula hacia la izquierda un valor de probabilidad de _probabilidad_."
              
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
                
                frase02 <- "El valor estandarizado t = _t_izquierdo_ acumula hacia la derecha una probabilidad de _probabilidad_."
                
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
                  
                  frase02 <- "Entre los valores estandarizados t1 = _t_izquierdo_ y t2 = _t_derecho_ se define una probabilidad de _probabilidad_."
                  
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
                            El valor estandarizado es t = _t_derecho_.
                           "
                
                frase02 <- "La probabilidad de pacientes con valores t menores a _t_derecho_ es _probabilidad_."
                
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
                              El valor estandarizado es t = _t_izquierdo_."
                  
                  frase02 <- "La probabilidad de pacientes con valores t mayores a _t_izquierdo_ es _probabilidad_."
                  
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
                                Los valores estandarizados son t1 = _t_izquierdo_ y t2 = _t_derecho_."
                    
                    frase02 <- "La probabilidad de pacientes con valores t entre _t_izquierdo_ y _t_derecho_ es _probabilidad_." 
                    
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
    
    # tabla <- cbind(tabla, tabla_interna01[4])
    # colnames(tabla)[ncol(tabla)] <- "Grados de Libertad"
    # tabla <- tabla[, c(1,4,2,3)]
    
    
    tabla_externa02 <- tabla
    remove(tabla)
    
  }
  
  # Tabla Externa03 
  {
    tabla <- tabla_externa02
    if(nrow(tabla) == 1) tabla <-   tabla[-1] else
      if(nrow(tabla) == 2) tabla <-   tabla[-2]
    
  tabla$"Grados de Libertad" <- tabla_interna01[1,4]
  # tabla <- cbind(tabla, rep(tabla_interna01[1,4]), nrow(tabla))
  # colnames(tabla)[ncol(tabla)] <- "Grados de Libertad"
   # tabla <- tabla[, c(1,4,2,3)]
    
  if(nrow(tabla) == 1) tabla <- tabla[, c(1,4,2,3)] else
    if(nrow(tabla) == 2) tabla <- tabla[, c(1,2,5,3,4)]
  
 
    tabla_externa03 <- tabla
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
  General <- Hmisc::llist(tabla_interna01, tabla_interna02, 
                          tabla_externa01, tabla_externa02, tabla_externa03, 
                          frase01, frase02, frase03, frase04, grafico01, grafico02)
  
  RMedic <- Hmisc::llist(tabla_externa01, tabla_externa02, tabla_externa03, 
                         frase01, frase02,
                         frase03, frase04, grafico01, grafico02)
  
  out <- Hmisc::llist(General, RMedic)
  return(out)
}


###########################################################################################

# varianza_muestral = 63
# gl = 19

Tabla.Chi <- function(gl,
                      opciones, 
                      intervalo,
                      color_variable,
                      chi1 = NA,
                      chi2 = NA,
                      chi3 = NA,
                      chi4 = NA, 
                      probabilidad_externo = NA, 
                      porcentaje_externo = NA, 
                      decimals = 2){
  
  
  # opciones puede tomar 4 valores: 
  # 1) "valor_chi"
  # 2) "probabilidad"
  # 3) "porcentaje"
  
  
  # intervalo puede tomar 3 valores:
  # 1) "menor"
  # 2) "mayor"
  # 3) "entre
  
  # Tabla interna 01
  tabla_interna01 <- data.frame(chi1, chi2, chi3, chi4, gl,
                                opciones, intervalo, color_variable)
  
  # Tabla Interna 02
  {
    
    # Chi Cuadrado Completa
    min_distribucion <- 0
    max_distribucion <- 20
    
    # Calculamos un ajuste para el valor derecho de la distribucion
    # chi cuadrado. Por que siempre queda mal el grafico.
    valor_maximo <- qchisq(p = 0.99, df = gl, ncp = 0, lower.tail = TRUE)
    valor_maximo <- ceiling(valor_maximo)
    
    # Si el valor
    if(max_distribucion < valor_maximo) max_distribucion <- valor_maximo
    
    h <- (max_distribucion - min_distribucion)/1000
    
    marcas_chi <- min_distribucion:max_distribucion
    # marcas_variable <- marcas_z*desvio_poblacional + media_poblacional
    
    if(1 == 1){
      # Si los valores ingresados son de la variable original
      if(opciones == "valor_chi") {
        if(intervalo == "menor"){
          
          if(is.na(chi1)) return(NULL)
          
          chi_izquierdo <- min_distribucion
          chi_derecho <- as.numeric(as.character(chi1))
          
          la_probabilidad <- pchisq(q = chi_derecho, df = gl, ncp = 0, lower.tail = T)            
          el_porcentaje <- la_probabilidad*100
          
          frase01 <- "El valor chi es _chi_derecho_."
          
          frase02 <- "Los grados de libertad son _gl_."
          
          frase03 <- "La probabilidad de valores chi menores a _chi_derecho_ es _probabilidad_."
          
          frase04 <- "El porcentaje es _porcentaje_%."
          
        } else
          if(intervalo == "mayor"){
            
            if(is.na(chi2)) return(NULL)
            
            chi_izquierdo <- as.numeric(as.character(chi2))
            chi_derecho <- max_distribucion
            
            la_probabilidad <- pchisq(q = chi_izquierdo, df = gl, ncp = 0, lower.tail = T)      
            el_porcentaje <- la_probabilidad*100
            
            frase01 <- "El valor chi es _chi_izquierdo_."
            
            frase02 <- "Los grados de libertad son _gl_."
            
            frase03 <- "La probabilidad de valores chi mayores a _chi_izquierdo_ es _probabilidad_."
            
            frase04 <- "El porcentaje es _porcentaje_%."
            
          } else
            if(intervalo == "entre"){
              
              if(is.na(chi3)) return(NULL)
              if(is.na(chi4)) return(NULL)
              
              chi_izquierdo <- as.numeric(as.character(chi3))
              chi_derecho <- as.numeric(as.character(chi4))
              
              p_der <- pchisq(q = chi_derecho, df = gl, ncp = 0, lower.tail = T)  
              p_izq <- pchisq(q = chi_izquierdo, df = gl, ncp = 0, lower.tail = T)  
              la_probabilidad <- p_der - p_izq
              el_porcentaje <- la_probabilidad*100
              
              frase01 <- "Los valores chi son chi1 = _chi_izquierdo_ y chi2 = _chi_derecho_."
              
              frase02 <- "Los grados de libertad son _gl_."
              
              frase03 <- "La probabilidad de valores chi entre _chi_izquierdo_ y _chi_derecho_ es _probabilidad_."
              
              frase04 <- "El porcentaje es _porcentaje_%."
            }
      } else
        if(opciones == "probabilidad") {
          if(intervalo == "menor"){
            
            if(is.na(probabilidad_externo)) return(NULL)
            
            la_probabilidad <- as.numeric(as.character(probabilidad_externo))
            
            chi_izquierdo <- min_distribucion
            chi_derecho <- qchisq(p = la_probabilidad, df = gl, ncp = 0, lower.tail = T)
            
            el_porcentaje <- la_probabilidad*100
            
            frase01 <- "La probabilidad es _probabilidad_."
            
            frase02 <- "Los grados de libertad son _gl_."
            
            frase03 <- "El valor chi _chi_derecho_ acumula hacia la izquierda un valor de probabilidad de _probabilidad_."
            
            frase04 <- "El porcentaje es _porcentaje_%."
            
          } else
            if(intervalo == "mayor"){
              
              if(is.na(probabilidad_externo)) return(NULL)
              
              la_probabilidad <- probabilidad_externo
              
              chi_izquierdo <- qchisq(p = la_probabilidad,  df = gl, ncp = 0, lower.tail = F)
              chi_derecho <- max_distribucion
              
              el_porcentaje <- la_probabilidad*100
              
              frase01 <- "La probabilidad es _probabilidad_."
              
              frase02 <- "Los grados de libertad son _gl_."
              
              frase03 <- "El valor chi _chi_izquierdo_ acumula hacia la derecha un valor de probabilidad de _probabilidad_."
              
              frase04 <- "El porcentaje es _porcentaje_%." 
              
            } else
              if(intervalo == "entre"){
                
                if(is.na(probabilidad_externo)) return(NULL)
                la_probabilidad <- probabilidad_externo
                el_resto <- 1 - la_probabilidad
                la_mitad <- el_resto/2
                
                chi_izquierdo <- qchisq(p = la_mitad,  df = gl, ncp = 0, lower.tail = T)
                chi_derecho <- qchisq(p = la_mitad,  df = gl, ncp = 0, lower.tail = F)
                
                el_porcentaje <- la_probabilidad*100
                
                frase01 <- "La probabilidad es _probabilidad_."
                
                frase02 <- "Los grados de libertad son _gl_."
                
                frase03 <- "Entre los valores estandarizados chi1 = _chi_izquierdo_ y chi2 = _chi_derecho_ se define una probabilidad de _probabilidad_."
                
                frase04 <- "El porcentaje es _porcentaje_%." 
                
              }
        } else
          if(opciones == "porcentaje") {
            if(intervalo == "menor"){
              
              if(is.na(porcentaje_externo)) return(NULL)
              
              el_porcentaje <- porcentaje_externo
              la_probabilidad <- el_porcentaje/100
              
              
              chi_izquierdo <- min_distribucion
              chi_derecho <- qchisq(p =la_probabilidad,  df = gl, ncp = 0, lower.tail = T)
             
              
              el_porcentaje <- la_probabilidad*100
              
              frase01 <- "El porcentaje es _porcentaje_."
              
              frase02 <- "Los grados de libertad son _gl_."
              
              frase03 <- "La probabilidad es _probabilidad_."
              
              frase04 <- "Los valores chi menores a _chi_derecho_ acumulan una probabilidad de _probabilidad_."
              
            } else
              if(intervalo == "mayor"){
                
                if(is.na(porcentaje_externo)) return(NULL)
                
                el_porcentaje <- porcentaje_externo
                la_probabilidad <- el_porcentaje/100
                
                chi_izquierdo <-  qchisq(p = probabilidad_externo,  df = gl, ncp = 0, lower.tail = F)
                chi_derecho <- max_distribucion
                
                el_porcentaje <- la_probabilidad*100
                
                frase01 <- "El porcentaje es _porcentaje_."
                
                frase02 <- "Los grados de libertad son _gl_."
                
                frase03 <- "La probabilidad es _probabilidad_."
                
                frase04 <- "Los valores chi mayores a _chi_izquierdo_ acumulan una probabilidad de _probabilidad_."
                
                
              } else
                if(intervalo == "entre"){
                  
                  if(is.na(porcentaje_externo)) return(NULL)
                  
                  el_porcentaje <- porcentaje_externo
                  la_probabilidad <- el_porcentaje/100
                  el_resto <- 1 - la_probabilidad
                  la_mitad <- el_resto/2
                  
                  chi_izquierdo <- qchisq(p = la_mitad, df = gl, ncp = 0, lower.tail = T)
                  chi_derecho <- qchisq(p = la_mitad, df = gl, ncp = 0, lower.tail = F)
                  
                  el_porcentaje <- la_probabilidad*100
                  
                  frase01 <- "El porcentaje es _porcentaje_."
                  
                  frase02 <- "Los grados de libertad son _gl_."
                  
                  frase03 <- "La probabilidad es _probabilidad_."
                  
                  frase04 <- "Los valores chi1 = _chi_izquierdo_ y chi2 = _chi_derecho_ contienen una probabilidad de _probabilidad_."
                  
                  
                }
          }
      
      
      Pattern <- c("_chi_izquierdo_", "_chi_derecho_",
                   "_probabilidad_", "_porcentaje_", "_gl_")
      
      Replacement <- c(chi_izquierdo, chi_derecho,
                       la_probabilidad, el_porcentaje, gl)
      
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
    
    
    
    las_columnas <- c("Chi", "G.L.", "Probabilidad", "Porcentaje", 
                      "Frase01", "Frase02", "Frase03", "Frase04")
    las_filas <- c("Izquierda", "Derecha")
    
    armado <- as.data.frame(matrix(NA, length(las_filas), length(las_columnas)))
    colnames(armado) <- las_columnas
    rownames(armado) <- las_filas
    
    #    armado$"Variable" <- c(var_izquierdo, var_derecho)
    armado$"Chi" <- c(chi_izquierdo, chi_derecho)
    armado$"G.L." <- c(gl, gl)
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
    # tabla <- tabla[c(1:3)]
    
    # tabla <- as.matrix(tabla)
    # colnames(tabla) <- c("Media Poblacional", "Varianza Poblacional", "Desvío Poblacional")
    # tabla <- round2(tabla, decimals)
    
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
    
    Posicion <- c("Izquierdo", "Derecho")
    tabla <- cbind(Posicion, tabla)
    tabla[2,3] <- ""
    tabla[2,4] <- ""
    tabla[2,5] <- ""
    
    colnames(tabla)[3] <- "Grados de Libertad"
    
    # tabla <- as.matrix(tabla)
    # tabla[,1] <- as.character(tabla[,1])
    if(intervalo == "mayor"){ 
      # tabla <- as.data.frame(tabla)
      # tabla <- as.data.frame(tabla[1,])
      #return(tabla)
      
    }
    
   
    
   
    tabla_externa02 <- tabla
    remove(tabla)
    
  }
  
  # Tabla Externa03 
  {
    tabla <- tabla_externa02
    if(nrow(tabla) == 1) tabla <-   tabla[-1] else
      if(ncol(tabla) == 2) tabla <-   tabla[-2]
    tabla_externa03 <- tabla
    remove(tabla)
    
  }
  
  # Frase01, Frase02, Frase03 y Frase04
  {
    frase01 <- tabla_interna02$Frase01[1]
    frase02 <- tabla_interna02$Frase02[1]
    frase03 <- tabla_interna02$Frase03[1]
    frase04 <- tabla_interna02$Frase04[1]
  }
  
  
  # grafico01 
  {
    
    # media_poblacional <- tabla_interna01[1,1]
    # desvio_poblacional <- tabla_interna01[1,3]
    # Parametros
    # media_poblacional    <- 0
    # desvio_poblacional <- 1
    
    color_variable <- tabla_interna01$"color_variable"
    decimals <- decimals
    
    chi_izquierdo <- tabla_interna02$"Chi"[1] 
    chi_derecho <- tabla_interna02$"Chi"[2]
    
    # Chi Cuadrado Completa
   # min_distribucion <- 0
   # max_distribucion <- 20 #gl*2
    h <- (max_distribucion - min_distribucion)/10000
    
    marcas_chi <- min_distribucion:max_distribucion
    #    marcas_variable <- (marcas_z*desvio_poblacional) + media_poblacional
    #    marcas_variable <- round2(marcas_variable, decimals)
    
    
    # # Rango Acotado
    # lower.x <- -0.0
    # upper.x <-  2.1
    lower.x <- chi_izquierdo
    upper.x <-  chi_derecho
    
    # La Campana 
    x  <- seq(from = min_distribucion, to = max_distribucion, by = h)
    y  <- dchisq(x = x, df = gl, ncp = 0)
    #    y <- dnorm(x = x, mean = 0, sd = 1)
    data_distribucion <- data.frame(x = x, y = y)
    
    # Lineas Limites
    linea_izquierda.x <- c(lower.x, lower.x)
    linea_izquierda.y <- c(0, dchisq( x = lower.x, df = gl, ncp = 0))
    # linea_izquierda.y <- c(0, dnorm( x = lower.x, mean = 0, sd = 1))
    data_linea_izquierda <- data.frame(x = linea_izquierda.x, y = linea_izquierda.y)
    
    linea_derecha.x <- c(upper.x, upper.x)
    linea_derecha.y <- c(0, dchisq( x = upper.x, df = gl, ncp = 0))
    #    linea_derecha.y <- c(0, dnorm( x = upper.x, mean = 0, sd = 1))
    data_linea_derecha <- data.frame(x = linea_derecha.x, y = linea_derecha.y)
    
    
    # El poligono
    x_mod <- seq(lower.x, upper.x, by = h)
    y_mod  <- dchisq(x = x_mod, df = gl, ncp = 0)
    # y_mod  <- dnorm(x = x_mod, mean = 0, sd = 1)
    data_poligono <- data.frame(x = c(lower.x, x_mod, upper.x), y = c(0, y_mod, 0))
    
    
    chi_mediano <- pchisq(q = 0.5, df = gl, ncp = 0, lower.tail = TRUE)
    limite_derecho <- max_distribucion
    if(chi_mediano > max_distribucion) limite_derecho <- chi_mediano
    
    # http://www.sthda.com/english/wiki/ggplot2-axis-ticks-a-guide-to-customize-tick-marks-and-labels
    grafico_general <- ggplot(data = data_distribucion, 
                              aes(x, y, 
                                  xmin = min_distribucion, 
                                  # xmax = max_distribucion, 
                                  xmax =  limite_derecho,
                                  ymin = 0, 
                                  ymax = 0.20
                                  )
                              ) + 
      theme_bw() +
      theme(axis.line = element_line(color='black'),
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank()) +
      geom_line(size = 2) + 
      ylab("Frecuencia Relativa") +
      # scale_x_continuous(name = "Variable Z", breaks = marcas_z, labels = marcas_z) +
      geom_polygon(data = data_poligono, fill = color_variable, size=2) + 
      geom_line(data = data_linea_izquierda, colour = color_variable, size=2) + 
      geom_line(data = data_linea_derecha,   colour = color_variable, size=2) + 
      geom_line(size=2)+ theme(text = element_text(size = 20))    
    
    grafico01 <- grafico_general +
      scale_x_continuous(name = "Variable Chi", breaks = marcas_chi, labels = marcas_chi)
    
    
    
    
  }
  
  
  # Salida General
  General <- Hmisc::llist(tabla_interna01, tabla_interna02, tabla_externa01, tabla_externa02,
                          tabla_externa03,
                          frase01, frase02, frase03, frase04, grafico01)
  
  
  RMedic <- Hmisc::llist(tabla_externa01, tabla_externa02, tabla_externa03,
                         frase01, frase02,
                         frase03, frase04, grafico01)
  
  out <- Hmisc::llist(General, RMedic)
  return(out)
}

####################################################################################

Tabla.F <- function(gl1,
                    gl2,
                    opciones, 
                    intervalo,
                    color_variable,
                    f1 = NA,
                    f2 = NA,
                    f3 = NA,
                    f4 = NA, 
                    probabilidad_externo = NA, 
                    porcentaje_externo = NA, 
                    decimals = 2){
  
  
  # opciones puede tomar 4 valores: 
  # 1) "valor_f"
  # 2) "probabilidad"
  # 3) "porcentaje"
  
  
  # intervalo puede tomar 3 valores:
  # 1) "menor"
  # 2) "mayor"
  # 3) "entre
  
  # Tabla interna 01
  tabla_interna01 <- data.frame(f1, f2, f3, f4, gl1, gl2,
                                opciones, intervalo, color_variable)
  
  # Tabla Interna 02
  {
    
    # Chi Cuadrado Completa
    min_distribucion <- 0
    max_distribucion <- 20
    
    # Calculamos un ajuste para el valor derecho de la distribucion
    # chi cuadrado. Por que siempre queda mal el grafico.
    valor_maximo <- qf(p = 0.99, df1 = gl1, df2 = gl2, ncp = 0, lower.tail = TRUE)
    valor_maximo <- ceiling(valor_maximo)
    
    # Si el valor
    if(max_distribucion < valor_maximo) max_distribucion <- valor_maximo
    
    h <- (max_distribucion - min_distribucion)/1000
    
    marcas_chi <- min_distribucion:max_distribucion
    # marcas_variable <- marcas_z*desvio_poblacional + media_poblacional
    
    if(1 == 1){
      # Si los valores ingresados son de la variable original
      if(opciones == "valor_f") {
        if(intervalo == "menor"){
          
          if(is.na(f1)) return(NULL)
          
          f_izquierdo <- min_distribucion
          f_derecho <- as.numeric(as.character(f1))
          
          la_probabilidad <- pf(q = f_derecho, df1 = gl1, df2 = gl2, ncp = 0, lower.tail = T)            
          el_porcentaje <- la_probabilidad*100
          
          frase01 <- "El valor F es _f_derecho_."
          
          frase02 <- "Los grados de libertad son _gl1_ (numerador) y _gl2_ (denominador)."
          
          
          frase03 <- "La probabilidad de valores F menores a _f_derecho_ es _probabilidad_."
          
          frase04 <- "El porcentaje es _porcentaje_%."
          
        } else
          if(intervalo == "mayor"){
            
            if(is.na(f2)) return(NULL)
            
            f_izquierdo <- as.numeric(as.character(f2))
            f_derecho <- max_distribucion
            
            la_probabilidad <- pf(q = f_izquierdo, df1 = gl1, df2 = gl2, ncp = 0, lower.tail = T)      
            el_porcentaje <- la_probabilidad*100
            
            frase01 <- "El valor F es _f_izquierdo_."
            
            frase02 <- "Los grados de libertad son _gl1_ (numerador) y _gl2_ (denominador)."
            
            
            frase03 <- "La probabilidad de valores F mayores a _f_izquierdo_ es _probabilidad_."
            
            frase04 <- "El porcentaje es _porcentaje_%."
            
          } else
            if(intervalo == "entre"){
              
              if(is.na(f3)) return(NULL)
              if(is.na(f4)) return(NULL)
              
              f_izquierdo <- as.numeric(as.character(f3))
              f_derecho <- as.numeric(as.character(f4))
              
              p_der <- pf(q = f_derecho, df1 = gl1, df2 = gl2, ncp = 0, lower.tail = T)  
              p_izq <- pf(q = f_izquierdo, df1 = gl1, df2 = gl2, ncp = 0, lower.tail = T)  
              la_probabilidad <- p_der - p_izq
              el_porcentaje <- la_probabilidad*100
              
              frase01 <- "Los valores F son f1 = _f_izquierdo_ y f2 = _f_derecho_."
              
              frase02 <- "Los grados de libertad son _gl1_ (numerador) y _gl2_ (denominador)."
              
              
              frase03 <- "La probabilidad de valores F entre _f_izquierdo_ y _f_derecho_ es _probabilidad_."
              
              frase04 <- "El porcentaje es _porcentaje_%."
            }
      } else
        if(opciones == "probabilidad") {
          if(intervalo == "menor"){
            
            if(is.na(probabilidad_externo)) return(NULL)
            
            la_probabilidad <- as.numeric(as.character(probabilidad_externo))
            
            f_izquierdo <- min_distribucion
            f_derecho <- qf(p = la_probabilidad, df1 = gl1, df2 = gl2, ncp = 0, lower.tail = T)
            
            el_porcentaje <- la_probabilidad*100
            
            frase01 <- "La probabilidad es _probabilidad_."
            
            frase02 <- "Los grados de libertad son _gl1_ (numerador) y _gl2_ (denominador)."
            
            
            frase03 <- "El valor F _f_derecho_ acumula hacia la izquierda un valor de probabilidad de _probabilidad_."
            
            frase04 <- "El porcentaje es _porcentaje_%."
            
          } else
            if(intervalo == "mayor"){
              
              if(is.na(probabilidad_externo)) return(NULL)
              
              la_probabilidad <- probabilidad_externo
              
              f_izquierdo <- qf(p = la_probabilidad,  df1 = gl1, df2 = gl2, ncp = 0, lower.tail = F)
              f_derecho <- max_distribucion
              
              el_porcentaje <- la_probabilidad*100
              
              frase01 <- "La probabilidad es _probabilidad_."
              
              frase02 <- "Los grados de libertad son _gl1_ (numerador) y _gl2_ (denominador)."
              
              
              frase03 <- "El valor F _f_izquierdo_ acumula hacia la derecha un valor de probabilidad de _probabilidad_."
              
              frase04 <- "El porcentaje es _porcentaje_%." 
              
            } else
              if(intervalo == "entre"){
                
                if(is.na(probabilidad_externo)) return(NULL)
                la_probabilidad <- probabilidad_externo
                el_resto <- 1 - la_probabilidad
                la_mitad <- el_resto/2
                
                f_izquierdo <- qf(p = la_mitad,  df1 = gl1, df2 = gl2, ncp = 0, lower.tail = T)
                f_derecho <- qf(p = la_mitad,  df1 = gl1, df2 = gl2, ncp = 0, lower.tail = F)
                
                el_porcentaje <- la_probabilidad*100
                
                frase01 <- "La probabilidad es _probabilidad_."
                
                frase02 <- "Los grados de libertad son _gl1_ (numerador) y _gl2_ (denominador)."
                
                
                frase03 <- "Entre los valores estandarizados f1 = _f_izquierdo_ y f2 = _f_derecho_ se define una probabilidad de _probabilidad_."
                
                frase04 <- "El porcentaje es _porcentaje_%." 
                
              }
        } else
          if(opciones == "porcentaje") {
            if(intervalo == "menor"){
              
              if(is.na(porcentaje_externo)) return(NULL)
              
              el_porcentaje <- porcentaje_externo
              la_probabilidad <- el_porcentaje/100
              
              
              f_izquierdo <- min_distribucion
              f_derecho <- qf(p =la_probabilidad,  df1 = gl1, df2 = gl2, ncp = 0, lower.tail = T)
              
              
              el_porcentaje <- la_probabilidad*100
              
              frase01 <- "El porcentaje es _porcentaje_."
              
              frase02 <- "Los grados de libertad son _gl1_ (numerador) y _gl2_ (denominador)."
              
              frase03 <- "La probabilidad es _probabilidad_."
              
              frase04 <- "Los valores F menores a _f_derecho_ acumulan una probabilidad de _probabilidad_."
              
            } else
              if(intervalo == "mayor"){
                
                if(is.na(porcentaje_externo)) return(NULL)
                
                el_porcentaje <- porcentaje_externo
                la_probabilidad <- el_porcentaje/100
                
                f_izquierdo <-  qf(p = probabilidad_externo,  df1 = gl1, df2 = gl2, ncp = 0, lower.tail = F)
                f_derecho <- max_distribucion
                
                el_porcentaje <- la_probabilidad*100
                
                frase01 <- "El porcentaje es _porcentaje_."
                
                frase02 <- "Los grados de libertad son _gl1_ (numerador) y _gl2_ (denominador)."
                
                
                frase03 <- "La probabilidad es _probabilidad_."
                
                frase04 <- "Los valores F mayores a _f_izquierdo_ acumulan una probabilidad de _probabilidad_."
                
                
              } else
                if(intervalo == "entre"){
                  
                  if(is.na(porcentaje_externo)) return(NULL)
                  
                  el_porcentaje <- porcentaje_externo
                  la_probabilidad <- el_porcentaje/100
                  el_resto <- 1 - la_probabilidad
                  la_mitad <- el_resto/2
                  
                  f_izquierdo <- qf(p = la_mitad, df1 = gl1, df2 = gl2, ncp = 0, lower.tail = T)
                  f_derecho <- qf(p = la_mitad, df1 = gl1, df2 = gl2, ncp = 0, lower.tail = F)
                  
                  el_porcentaje <- la_probabilidad*100
                  
                  frase01 <- "El porcentaje es _porcentaje_."
                  
                  frase02 <- "Los grados de libertad son _gl1_ (numerador) y _gl2_ (denominador)."
                  
                  
                  frase03 <- "La probabilidad es _probabilidad_."
                  
                  frase04 <- "Los valores f1 = _f_izquierdo_ y f2 = _f_derecho_ contienen una probabilidad de _probabilidad_."
                  
                  
                }
          }
      
      
      Pattern <- c("_f_izquierdo_", "_f_derecho_",
                   "_probabilidad_", "_porcentaje_", "_gl1_", "_gl2_")
      
      Replacement <- c(f_izquierdo, f_derecho,
                       la_probabilidad, el_porcentaje, gl1, gl2)
      
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
    
    
    
    las_columnas <- c("F", "G.L.", "Probabilidad", "Porcentaje", 
                      "Frase01", "Frase02", "Frase03", "Frase04")
    las_filas <- c("Izquierda", "Derecha")
    
    armado <- as.data.frame(matrix(NA, length(las_filas), length(las_columnas)))
    colnames(armado) <- las_columnas
    rownames(armado) <- las_filas
    
    #    armado$"Variable" <- c(var_izquierdo, var_derecho)
    armado$"F" <- c(f_izquierdo, f_derecho)
    armado$"G.L." <- c(gl1, gl2)
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
    # tabla <- tabla[c(1:3)]
    
    # tabla <- as.matrix(tabla)
    # colnames(tabla) <- c("Media Poblacional", "Varianza Poblacional", "Desvío Poblacional")
    # tabla <- round2(tabla, decimals)
    
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
    
    Posicion <- c("Izquierdo", "Derecho")
    tabla <- cbind(Posicion, tabla)
    # tabla[2,3] <- ""
    tabla[2,4] <- ""
    tabla[2,5] <- ""
    
    colnames(tabla)[3] <- "Grados de Libertad"
    
    # tabla <- as.matrix(tabla)
    # tabla[,1] <- as.character(tabla[,1])
    if(intervalo == "mayor"){ 
      # tabla <- as.data.frame(tabla)
      # tabla <- as.data.frame(tabla[1,])
      #return(tabla)
      
    }
    
    
    
    
    tabla_externa02 <- tabla
    remove(tabla)
    
  }
  
  # Tabla Externa03 
  {
    tabla <- tabla_externa02
    if(nrow(tabla) == 1) tabla <-   tabla[-1] else
      if(ncol(tabla) == 2) tabla <-   tabla[-2]
    tabla_externa03 <- tabla
    remove(tabla)
    
  }
  
  # Frase01, Frase02, Frase03 y Frase04
  {
    frase01 <- tabla_interna02$Frase01[1]
    frase02 <- tabla_interna02$Frase02[1]
    frase03 <- tabla_interna02$Frase03[1]
    frase04 <- tabla_interna02$Frase04[1]
  }
  
  
  # grafico01 
  {
    
    # media_poblacional <- tabla_interna01[1,1]
    # desvio_poblacional <- tabla_interna01[1,3]
    # Parametros
    # media_poblacional    <- 0
    # desvio_poblacional <- 1
    
    color_variable <- tabla_interna01$"color_variable"
    decimals <- decimals
    
    f_izquierdo <- tabla_interna02$"F"[1] 
    f_derecho <- tabla_interna02$"F"[2]
    
    # Chi Cuadrado Completa
    # min_distribucion <- 0
    # max_distribucion <- 20 #gl*2
    # h <- (max_distribucion - min_distribucion)/10000
    
    # marcas_chi <- min_distribucion:max_distribucion
    #    marcas_variable <- (marcas_z*desvio_poblacional) + media_poblacional
    #    marcas_variable <- round2(marcas_variable, decimals)
    
    
    # # Rango Acotado
    # lower.x <- -0.0
    # upper.x <-  2.1
    lower.x <- f_izquierdo
    upper.x <-  f_derecho
    
    # La Campana 
    x  <- seq(from = min_distribucion, to = max_distribucion, by = h)
    y  <- df(x = x, df1 = gl1, df2 = gl2, ncp = 0)
    #    y <- dnorm(x = x, mean = 0, sd = 1)
    data_distribucion <- data.frame(x = x, y = y)
    
    # Lineas Limites
    linea_izquierda.x <- c(lower.x, lower.x)
    linea_izquierda.y <- c(0, df( x = lower.x, df1 = gl1, df2 = gl2, ncp = 0))
    # linea_izquierda.y <- c(0, dnorm( x = lower.x, mean = 0, sd = 1))
    data_linea_izquierda <- data.frame(x = linea_izquierda.x, y = linea_izquierda.y)
    
    linea_derecha.x <- c(upper.x, upper.x)
    linea_derecha.y <- c(0, df( x = upper.x, df1 = gl1, df2 = gl2, ncp = 0))
    #    linea_derecha.y <- c(0, dnorm( x = upper.x, mean = 0, sd = 1))
    data_linea_derecha <- data.frame(x = linea_derecha.x, y = linea_derecha.y)
    
    
    # El poligono
    x_mod <- seq(lower.x, upper.x, by = h)
    y_mod  <- df(x = x_mod, df1 = gl1, df2 = gl2, ncp = 0)
    # y_mod  <- dnorm(x = x_mod, mean = 0, sd = 1)
    data_poligono <- data.frame(x = c(lower.x, x_mod, upper.x), y = c(0, y_mod, 0))
    
    
    
    limite_derecho <- max_distribucion
    
    # http://www.sthda.com/english/wiki/ggplot2-axis-ticks-a-guide-to-customize-tick-marks-and-labels
    grafico_general <- ggplot(data = data_distribucion, 
                              aes(x, y, 
                                  xmin = min_distribucion, 
                                  # xmax = max_distribucion, 
                                  xmax =  limite_derecho,
                                  ymin = 0, 
                                  ymax = 0.20
                              )
    ) + 
      theme_bw() +
      theme(axis.line = element_line(color='black'),
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank()) +
      geom_line(size = 2) + 
      ylab("Frecuencia Relativa") +
      # scale_x_continuous(name = "Variable Z", breaks = marcas_z, labels = marcas_z) +
      geom_polygon(data = data_poligono, fill = color_variable, size=2) + 
      geom_line(data = data_linea_izquierda, colour = color_variable, size=2) + 
      geom_line(data = data_linea_derecha,   colour = color_variable, size=2) + 
      geom_line(size=2)+ theme(text = element_text(size = 20))    
    
    grafico01 <- grafico_general +
      scale_x_continuous(name = "Variable F", breaks = marcas_chi, labels = marcas_chi)
    
    
    
    
  }
  
  
  # Salida General
  General <- Hmisc::llist(tabla_interna01, tabla_interna02, tabla_externa01, tabla_externa02,
                          tabla_externa03,
                          frase01, frase02, frase03, frase04, grafico01)
  
  
  RMedic <- Hmisc::llist(tabla_externa01, tabla_externa02, tabla_externa03,
                         frase01, frase02,
                         frase03, frase04, grafico01)
  
  out <- Hmisc::llist(General, RMedic)
  return(out)
}
