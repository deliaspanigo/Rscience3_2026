


Ho2C_01_RMedicHelp_UI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    column(4,
           radioButtons(inputId = "help_ho_2c",
                        label = h3("Selección de Ayuda Automática"),
                        choices = c("RMedic Here!" = 1,
                                    "Test de Correlación de Pearson" = 2,
                                    "Test de Correlación de Spearman" = 3,
                                    "Test de Regresión Lineal Simple" = 4,
                                    "Test t (Dos muestras apareadas)" = 5,
                                    "Test Wilcoxon (Dos muestras apareadas)" = 6,
                                    "Test de Homogeneidad de varianzas de Fisher" = 7,
                                    "Test de Homogeneidad de varianzas de Bartlett" = 8,
                                    "Test de Homogeneidad de varianzas de Levene" = 9,
                                    "Test de Regresión Logística Simple" = 10)
           )),
    column(8,
           conditionalPanel(condition = "input.help_ho_2c == 1", 
                            div(
                              h3("RMedic Here!"),
                              HTML(
                                "Los gráficos más utilizados aplicados a una variable numérica son:<br/>
                      - <b>Test de Correlación de Pearson</b>.<br/>
                      - <b>Test de Correlación de Spearman</b>.<br/>
                      - <b>Test de Regresión Lineal Simple</b>.<br/>
                      - <b>Test t (Dos muestras apareadas)</b>.<br/>
                      - <b>Test Wilcoxon (Dos muestras apareadas)</b>.<br/>
                      - <b>Test de Homogeneidad de varianzas de Fisher</b>.<br/>
                      - <b>Test de Homogeneidad de varianzas de Bartlett</b>.<br/>
                      - <b>Test de Homogeneidad de varianzas de Levene</b>.<br/>
                      - <b>Test de Regresión Logística Simple</b>.<br/>
                      Seleccionando la ayuda de cada uno encontrarás un resumen con
                      detalles teóricos y estructura de la base de datos.<br/>
                      Estos te ayudarán a determinar si estas herramientas pueden ser
                      aplicadas en tu trabajo."
                              )
                            )
                            ),
           conditionalPanel(condition = "input.help_ho_2c == 2", 
                            div(
                              h3_mod("Test de Correlación de Pearson"),
                              HTML(
                                "Se aplica sobre dos columnas de la base de datos.<br>  
                                Ambas variables deben ser numéricas.<br>
                                La naturaleza de los datos debe ser cuantitativa.<br>
                                Son requisitos que las variables presenten homogeneidad de varianzas y que
                                cada una posea distribución normal.<br>
                                Estima el valor del índice de correlación lineal de Pearson entre las variables.<br>
                                Plantea si dicha correlación es igual a cero."
                                ),
                              h3_mod("Juego de Hipótesis"),
                              HTML("<u><b>Hipótesis Nula (Ho):</u></b> La correlación lineal de Pearson es igual a cero. <br>
                              <u><b>Hipótesis Alternativa (Hi):</u></b> La correlación lineal de Pearson es distinta a cero."
                                )
                              )
                            ),
           conditionalPanel(condition = "input.help_ho_2c == 3", 
                            div(
                              h3_mod("Test de Correlación de Spearman"),
                              HTML(
                                "Se aplica sobre dos columnas de la base de datos.<br>
                                Ambas variables deben ser numéricas.<br>
                                La naturaleza de los datos puede ser cuantitativa o cualitativa ordinal representada con números.<br>
                                Estima el valor del índice de correlación de Spearman entre las variables.<br>
                                Plantea si dicha correlación es igual a cero."
                              ),
                              h3_mod("Juego de Hipótesis"),
                              HTML("<u><b>Hipótesis Nula (Ho):</u></b> La correlación de Spearman es igual a cero. <br>
                              <u><b>Hipótesis Alternativa (Hi):</u></b> La correlación de Spearman es distinta a cero."
                              )
                            )
           ),
           conditionalPanel(condition = "input.help_ho_2c == 4", 
                            div(
                              h3_mod("Test de Regresión Lineal Simple"),
                              HTML(
                                "Se aplica sobre dos columnas de la base de datos.<br>
                  Ambas variables deben ser numéricas.<br>
                  La naturaleza de los datos debe ser cuantitativa.<br>
                  Regresión Lineal Simple es un modelo de predicción basado en una recta
                  en el que cada variable cumple un rol específico.
                  Una de las variables cumple el rol de variable independiente (X) y la otra el de variable dependiente (Y).
                  El marco de aplicación (y no la estadística) determina cual de las variables
                  es independiente (X) y cual dependiente (Y).<br>
                  Toda recta está definida por una pendiente y una ordenada al origen.<br>
                  Se genera un juego de hipótesis para la pendiente y otro para la ordenada.
                  Por defecto, se plantea si tanto la pendiente como la ordenada son iguales a cero.
                  Por lo general solo el juego de hipótesis de la pendiente tiene relevancia.<br>
                  El modelo tiene requisitos de homogeneidad y distribución normal de los residuos.
                  (Un residuo es la diferencia entre el valor observado de 'Y' y el valor esperado en la recta.)<br>
                  Se agrega un tercer juego de hipótesis sobre el 'R cuadrado ajustado' (R^2 ajustado).
                  'R^2 ajustado' es un valor entre 0 y 1, donde 0 es un ajuste nulo de los datos a la recta
                  y 1 es un ajuste perfecto de todos los datos sobre la recta."
                              ),
                              h3_mod("Juego de Hipótesis"),
                              HTML("<u><b>Pendiente</u></b><br>
                            <u><b>Hipótesis Nula (Ho):</u></b> La pendiente es igual a cero.<br>
                            <u><b>Hipótesis Alternativa (Hi):</u></b> La pendiente es distinta a cero.<br>
                            <br>
                            
                            <u><b>Ordenada</u></b><br>
                            <u><b>Hipótesis Nula (Ho):</u></b> La ordenada es igual a cero.<br>
                            <u><b>Hipótesis Alternativa (Hi):</u></b> La ordenada es distinta a cero.<br>
                            <br>
                            
                            <u><b>R Cuadrado Ajustado</u></b><br>
                            <u><b>Hipótesis Nula (Ho):</u></b> R Cuadrado Ajustado igual a cero. <br>
                            <u><b>Hipótesis Alternativa (Hi):</u></b> R Cuadrado Ajustado diferente a cero. <br>"
                            )
                            )
           ),
           conditionalPanel(condition = "input.help_ho_2c == 5", 
                            div(
          h3_mod("Test t (Dos muestras apareadas)"),
          HTML(
          "Se aplica sobre dos columnas de la base de datos.<br>
          Ambas variables deben ser numéricas.<br>
          La naturaleza de los datos debe ser cuantitativa.<br>
          A partir de cada par de valores de las variables ingresadas (tomados sobre la
          misma unidad o paciente), el análisis internamente obtiene una nueva
          variable que es la diferencia entre cada par de valores.<br>
          La prueba estadística se aplica sobre la variable diferencia.<br>
          El test plantea si la media de estas diferencias es igual a cero.<br>
          Para que los resultados estadístico del Test t apareado sean válidos, es un
          requisito que la nueva variable tenga distribución normal.
                                "
                              ),
            h3_mod("Juego de Hipótesis"),
            HTML("<u><b>Hipótesis Nula (Ho):</u></b> La media de las diferencias es igual a cero.<br>
                  <u><b>Hipótesis Alternativa (Hi):</u></b> La media de las diferencias es diferente a cero.<br>"
                              )
                            )
           ),
           conditionalPanel(condition = "input.help_ho_2c == 6", 
                            div(
                              h3_mod("Test Wilcoxon (Dos muestras apareadas)"),
                              HTML(
                                "Se aplica sobre dos columnas de la base de datos.<br>
          Ambas variables deben ser numéricas.<br>
          La naturaleza de los datos debe ser cuantitativa.<br>
          A partir de cada par de valores de las variables ingresadas (tomados sobre la
          misma unidad o paciente), el análisis internamente obtiene una nueva
          variable que es la diferencia entre cada par de valores.<br>
          La prueba estadística se aplica sobre la variable diferencia.<br>
          El test de Wilcoxon apareado rankea los datos de esta nueva variable y estima la posición de la mediana a partir de la media del ranking.
          Plantea si la mediana de las diferencias es igual a cero. .
                                "
                              ),
                              h3_mod("Juego de Hipótesis"),
                              HTML("<u><b>Hipótesis Nula (Ho):</u></b> La mediana de las diferencias es igual a cero.<br>
                  <u><b>Hipótesis Alternativa (Hi):</u></b> La mediana de las diferencias es diferente a cero.<br>"
                              )
                            )
           ),
           conditionalPanel(condition = "input.help_ho_2c == 7", 
                            div(
                              h3_mod("Test de Homogeneidad de varianzas de Fisher"),
                              HTML(
                                "El Test de Homogeneidad de Fisher posee varias formas de ser utilizado.<br>
                            Las siguientes recomendaciones y juegos de hipótesis corresponden sólo al caso
                            en que ambas variables son numéricas.<br>
                            Se aplica sobre dos columnas de la base de datos.<br>
                            La naturaleza de los datos debe ser cuantitativa.<br>
                            Utiliza una estimación de varianza de cada columna.
                            Plantea si las varianzas poblaciones son iguales entre si.
                                "
                              ),
                              h3_mod("Juego de Hipótesis"),
                              HTML("<u><b>Hipótesis Nula (Ho):</u></b> Las 2 varianzas son iguales.<br>
                  <u><b>Hipótesis Alternativa (Hi):</u></b> Las dos varianzas son diferentes.<br>"
                              )
                            )
           ),
           conditionalPanel(condition = "input.help_ho_2c == 8", 
                            div(
                              h3_mod("Test de Homogeneidad de varianzas de Levene"),
                              HTML(
                            "El Test de Homogeneidad de Levene posee varias formas de ser utilizado.<br>
                            Las siguientes recomendaciones y juegos de hipótesis corresponden sólo al caso
                            en que ambas variables son numéricas.<br>
                            Se aplica sobre dos columnas de la base de datos.<br>
                            La naturaleza de los datos debe ser cuantitativa.<br>
                            Utiliza una estimación de varianza de cada columna.
                            Plantea si las varianzas poblaciones son iguales entre si.
                                "
                              ),
                              h3_mod("Juego de Hipótesis"),
                              HTML("<u><b>Hipótesis Nula (Ho):</u></b> Las 2 varianzas son iguales.<br>
                  <u><b>Hipótesis Alternativa (Hi):</u></b> Las dos varianzas son diferentes.<br>"
                              )
                            )
           ),
           conditionalPanel(condition = "input.help_ho_2c == 9", 
                            div(
                              h3_mod("Test de Homogeneidad de varianzas de Bartlett"),
                              HTML(
                                "El Test de Homogeneidad de Bartlett posee varias formas de ser utilizado.<br>
                            Las siguientes recomendaciones y juegos de hipótesis corresponden sólo al caso
                            en que ambas variables son numéricas.<br>
                            Se aplica sobre dos columnas de la base de datos.<br>
                            La naturaleza de los datos debe ser cuantitativa.<br>
                            Utiliza una estimación de varianza de cada columna.
                            Plantea si las varianzas poblaciones son iguales entre si.
                                "
                              ),
                              h3_mod("Juego de Hipótesis"),
                              HTML("<u><b>Hipótesis Nula (Ho):</u></b> Las 2 varianzas son iguales.<br>
                  <u><b>Hipótesis Alternativa (Hi):</u></b> Las dos varianzas son diferentes.<br>"
                              )
                            )
           ),
           conditionalPanel(condition = "input.help_ho_2c == 10", 
                            div(
                              h3_mod("Test de Regresión Logística Simple"),
                              HTML(
                                "La Regresión Logística Simple posee varias formas de ser utilizada.<br>
          En esta ocasión ambas variables deben ser numéricas.<br>
          Es un modelo de predicción en el que cada variable cumple un rol específico.
          Una variable cumplirá el rol de variable independiente (X) y la otra, variable dependiente (Y).
          El marco de aplicación (y no la estadística) determina cuál de las variables
          es independiente (X) y cuál dependiente (Y).<br>
          Como toda regresión, tendrá un juego de hipótesis para la pendiente y otra para la ordenada al origen.
          Por lo general sólo tiene relevancia el juego de hipótesis de la pendiente.<br>
          La primer variable ingresada será considerada 'X'.<br>
          <br>
          
          Se generan recomendaciones a partir de dos casos posibles:<br>
          <br>
          <u><b>Caso 1</u></b><br>
          Variables (X) e (Y), ambas poseen exclusivamente dos valores: 0 y 1.<br>
          <br>
          <u><b>Caso 2</u></b><br>
          Variable (X) debe ser cuantitativa.<br>
          Variable (Y) debe ser cuantitativa y poseer exclusivamente dos valores: 0 y 1.<br>
          <br>
          En ambos casos, en la variable 'Y', el valor '1' corresponderá a la categoría considerada como 'Éxito' dentro del estudio."
                              ),
                              h3_mod("Juego de Hipótesis"),
                              HTML("<u><b>Pendiente</u></b><br>
                            <u><b>Hipótesis Nula (Ho):</u></b> Pendiente igual a cero (No existe una relación logística entre las variables).<br>
                            <u><b>Hipótesis Alternativa (Hi):</u></b> Pendiente distinta de cero (Existe una relación logística entre las variables)<br> 
                            <br>
                            
                            <u><b>Ordenada</u></b><br>
                            <u><b>Hipótesis Nula (Ho):</u></b> La ordenada es igual a cero.<br>
                            <u><b>Hipótesis Alternativa (Hi):</u></b> La ordenada es distinta a cero.<br>
                            <br>
                            "
                              )
                            )
           )
           )
           )
  
  
  
}






## Segmento del server
Ho2C_01_RMedicHelp_SERVER <- function(input, output, session,
                                            minibase,
                                            decimales,
                                            control_ejecucion) {
  
  
  
  
  
  
  
  
  
  
  
  
}