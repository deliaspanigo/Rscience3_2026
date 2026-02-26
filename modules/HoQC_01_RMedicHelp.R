


HoQC_01_RMedicHelp_UI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    column(4,
           radioButtons(inputId = "help_ho_qc",
                        label = h3("Selección de Ayuda Automática"),
                        choices = c("RMedic Here!" = 1,
                                    "Test t (Dos muestras independientes)" = 2,
                                    "Test Mann-Whitney-Wilcoxon (Dos muestras independientes)" = 3,
                                    "Test Anova a 1 Factor" = 4,
                                    "Test Kruskal-Wallis" = 5,
                                    "Test de Homogeiendiad de varianzas de Fisher" = 6,
                                    "Test de Homogeiendiad de varianzas de Bartlett" = 7,
                                    "Test de Homogeiendiad de varianzas de Levene" = 8,
                                    "Test de Normalidad Shapiro-Wilk (Particionado)" = 9,
                                    "Test de Regresión Logística Simple" = 10)
           )),
    column(8,
           conditionalPanel(condition = "input.help_ho_qc == 1", 
                            div(
                              h3("RMedic Here!"),
                              HTML(
                                "Las pruebas de hipótesis más utilizados aplicados a dos variables siendo 1 categórica y 1 numérica son:<br/>
                      - <b>Test t (Dos muestras independientes)</b>.<br/>
                      - <b>Test Mann-Whitney-Wilcoxon (Dos muestras independientes)</b>.<br/>
                      - <b>Test Anova a 1 Factor</b>.<br/>
                      - <b>Test Kruskal-Wallis</b>.<br/>
                      - <b>Test de Homogeiendiad de varianzas de Fisher</b>.<br/>
                      - <b>Test de Homogeiendiad de varianzas de Bartlett</b>.<br/>
                      - <b>Test de Homogeiendiad de varianzas de Levene</b>.<br/>
                      - <b>Test de Normalidad Shapiro-Wilk (Particionadi)</b>.<br/>
                      - <b>Test de Regresión Logística Simple</b>.<br/>

                      Seleccionando la ayuda de cada uno encontrarás un resumen con
                      detalles teóricos y estructura de la base de datos.<br/>
                      Estos te ayudarán a determinar si estas herramientas pueden ser
                      aplicadas en tu trabajo."
                              )
                            )
                            ),
           conditionalPanel(condition = "input.help_ho_qc == 2", 
                            div(
                              h3_mod("Test t (Dos muestras independientes)"),
                              HTML(
                                "Se aplica sobre dos columnas de la base de datos, una de ellas numérica y otra categórica.
                                La variable numérica debe ser de naturaleza cuantitativa.<br>
                                La variable categórica debe tener solo dos categorías.<br>
                                Cada categoría será considerada un grupo diferente.<br>
                                Utiliza la media de cada grupo.<br>
                                Plantea si las medias de ambos grupos son estadísticamente iguales entre si.<br>
                                Tiene como requisito verificar previamente que ambos grupos poseen distribución normal y homogeneidad de varianza.
                                El incumplimiento de los requisitos invalida los resultados estadísticos obtenidos y las conclusiones.
                                "
                              ),
                              h3_mod("Juego de Hipótesis"),
                              HTML("<u><b>Hipótesis Nula (Ho):</u></b> Las medias de ambos grupos son iguales.<br>
                                    <u><b>Hipótesis Alternativa (Hi):</u></b> Las medias de ambos grupos son diferentes.<br>"
                              )
                            )
           ),
           conditionalPanel(condition = "input.help_ho_qc == 3", 
                            div(
                              h3_mod("Test Mann-Whitney-Wilcoxon (Dos muestras independientes)"),
                              HTML(
                                "Se aplica sobre dos columnas de la base de datos, una de ellas numérica y otra categórica.
                                La variable numérica debe ser de naturaleza cuantitativa.<br>
                                La variable categórica debe tener solo dos categorías.<br>
                                Cada categoría será considerada un grupo diferente.<br>
                                Utiliza la mediana de cada grupo.<br>
                                Internamente el test rankea todos los datos juntos y luego obtiene la media del ranking de cada grupo para determinar la posición de la mediana de cada grupo.<br>
                                Plantea si las medianas de ambos grupos son iguales entre si. 
                                "
                              ),
                              h3_mod("Juego de Hipótesis"),
                              HTML("<u><b>Hipótesis Nula (Ho):</u></b> Las medianas de ambos grupos son iguales.<br>
                  <u><b>Hipótesis Alternativa (Hi):</u></b> Las medianas de ambos grupos son diferentes.<br>"
                              )
                            )
           ),
           conditionalPanel(condition = "input.help_ho_qc == 4", 
                            div(
                              h3_mod("Test Anova a 1 Factor"),
                              HTML(
                                "Se aplica sobre dos columnas de la base de datos, una de ellas numérica y otra categórica.
                                La variable numérica debe ser de naturaleza cuantitativa.<br>
                                La variable categórica debe tener 2 grupos o más.<br>
                                Cada categoría será considerada un grupo diferente.<br>
                                Utiliza la media de cada grupo.<br>
                                Plantea si las medias de todos grupos son estadísticamente iguales entre si.<br>
                                Tiene como requisito verificar previamente los residuos poseen distribución normal y homogeneidad de varianza de los residuos entre los grupos.<br>
                                Se denomina residuo a la diferencia entre el valor observado y el valor de la media del grupo al cual pertence el dato.<br>
                                El incumplimiento de los requisitos invalida los resultados estadísticos obtenidos y las conclusiones.<br>
                                "
                              ),
                              h3_mod("Juego de Hipótesis"),
                              HTML("<u><b>Hipótesis Nula (Ho):</u></b> Las medias de todos los grupos son iguales.<br>
                                    <u><b>Hipótesis Alternativa (Hi):</u></b> Al menos una media de un grupo es estadísticamente diferente.<br>"
                              ),
                              h3_mod("Comparaciones Múltiples"),
                              HTML("Acompaña al test de Anova un test de comparaciones múltiples de las medias: Test de Tukey.<br>
                              El test de Tukey solo es válido si se rechaza la hipótesis nula de Anova."
                              )
                            )
           ),
           conditionalPanel(condition = "input.help_ho_qc == 5", 
                            div(
                              h3_mod("Kruskal-Wallis"),
                              HTML(
                                "Se aplica sobre dos columnas de la base de datos, una de ellas numérica y otra categórica.
                                La variable numérica debe ser de naturaleza cuantitativa.<br>
                                La variable categórica debe tener dos o más categorías.<br>
                                Cada categoría será considerada un grupo diferente.<br>
                                Utiliza la mediana de cada grupo.<br>
                                Internamente el test de Kruskal-Wallisrankea todos los datos juntos y luego obtiene la media del ranking de cada grupo para determinar la posición de la mediana de cada grupo.<br>
                                Plantea si las medianas de ambos grupos son iguales entre si. 
                                "
                              ),
                              h3_mod("Juego de Hipótesis"),
                              HTML("<u><b>Hipótesis Nula (Ho):</u></b> Las medianas de todos los grupos son iguales.<br>
                  <u><b>Hipótesis Alternativa (Hi):</u></b> Al menos una mediana de un grupo es estadísticamente diferente.<br>"
                              ),
                              h3_mod("Comparaciones Múltiples"),
                              HTML("Acompaña al test de Anova un test de comparaciones múltiples de las medias: Test de Dunn.<br>
                              El test de Tukey solo es válido si se rechaza la hipótesis nula del Test de Kruskal-Wallis."
                              )
                            )
           ),
           conditionalPanel(condition = "input.help_ho_qc == 6", 
                            div(
                              h3_mod("Test de Homogeiendiad de varianzas de Fisher"),
                              HTML(
              "Se aplica sobre dos columnas de la base de datos, una de ellas numérica y otra categórica.
              La variable numérica debe ser de naturaleza cuantitativa.<br>
              La variable categórica debe tener exactamente 2 grupos.<br>
              Cada categoría será considerada un grupo diferente.<br>
              Utiliza la varianza de cada grupo.<br>
              Plantea si las varianzas de ambos grupos son estadísticamente iguales entre si.<br>
              Tiene como requisito verificar previamente los residuos poseen distribución normal de los 2 grupos. Ambos deben tener distribución normal.<br>
              El incumplimiento de los requisitos invalida los resultados estadísticos obtenidos y las conclusiones.<br>
                                "
              ),
              h3_mod("Juego de Hipótesis"),
              HTML("<u><b>Hipótesis Nula (Ho):</u></b> Las varianzas de ambos los grupos son iguales.<br>
                    <u><b>Hipótesis Alternativa (Hi):</u></b> Las varianzas de ambos grupos son estadísticamente diferente.<br>"
              )
              )
              ),
           conditionalPanel(condition = "input.help_ho_qc == 7", 
                            div(
                              h3_mod("Test de Homogeiendiad de varianzas de Bartlett"),
                              HTML(
                                "Se aplica sobre dos columnas de la base de datos, una de ellas numérica y otra categórica.
              La variable numérica debe ser de naturaleza cuantitativa.<br>
              La variable categórica debe tener 2 o más grupos.<br>
              Cada categoría será considerada un grupo diferente.<br>
              Utiliza la varianza de cada grupo.<br>
              Plantea si las varianzas de todos los grupos son estadísticamente iguales entre si.<br>
              Tiene como requisito que cada grupo posee distribución normal.<br>
              El incumplimiento de los requisitos invalida los resultados estadísticos obtenidos y las conclusiones.<br>
                                "
                              ),
                              h3_mod("Juego de Hipótesis"),
                              HTML("<u><b>Hipótesis Nula (Ho):</u></b> Las varianzas de todos los grupos son iguales.<br>
                    <u><b>Hipótesis Alternativa (Hi):</u></b> Al menos una varianza de un grupo es estadísticamente diferente.<br>"
                              )
                              )
                            ),
           conditionalPanel(condition = "input.help_ho_qc == 8", 
                            div(
                              h3_mod("Test de Homogeiendiad de varianzas de Levene"),
                              HTML(
                                "Se aplica sobre dos columnas de la base de datos, una de ellas numérica y otra categórica.
              La variable numérica debe ser de naturaleza cuantitativa.<br>
              La variable categórica debe tener 2 o más grupos.<br>
              Cada categoría será considerada un grupo diferente.<br>
              Utiliza la varianza de cada grupo.<br>
              Plantea si las varianzas de todos los grupos son estadísticamente iguales entre si.<br>
              Tiene como requisito que cada grupo posee distribución normal.<br>
              El incumplimiento de los requisitos invalida los resultados estadísticos obtenidos y las conclusiones.<br>
                                "
                              ),
                              h3_mod("Juego de Hipótesis"),
                              HTML("<u><b>Hipótesis Nula (Ho):</u></b> Las varianzas de todos los grupos son iguales.<br>
                    <u><b>Hipótesis Alternativa (Hi):</u></b> Al menos una varianza de un grupo es estadísticamente diferente.<br>"
                              )
                              )
                            ),
           conditionalPanel(condition = "input.help_ho_qc == 9", 
                            div(
                              h3_mod("Test de Normalidad Shapiro-Wilk (Particionado)"),
                              HTML(
                                "Se aplica sobre dos columnas de la base de datos, una de ellas numérica y otra categórica.
              La variable numérica debe ser de naturaleza cuantitativa.<br>
              La variable categórica debe tener 2 o más grupos.<br>
              Cada categoría será considerada un grupo diferente.<br>
              Realiza un test de normalidad sobre cada grupo y detalla todos los resultados juntos.<br>
              Para cada categoría platea una prueba de hipótesis sobre si la distribución de la variable en cada grupo es normal.<br>
              Agrega también un test de normalidad para la variable en su totalidad sin tener en los grupos.
                                "
                              ),
                              h3_mod("Juego de Hipótesis"),
                              HTML("<u><b>Hipótesis Nula (Ho):</u></b> La variable posee distribución normal.<br>
                    <u><b>Hipótesis Alternativa (Hi):</u></b> La variable no posee distribución normal.<br>"
                              )
                            )
           ),
           conditionalPanel(condition = "input.help_ho_qc == 10", 
                            div(
                              h3_mod("Test de Regresión Logística Simple"),
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
HoQC_01_RMedicHelp_SERVER <- function(input, output, session,
                                            minibase,
                                            decimales,
                                            control_ejecucion,
                                            tabla_qc,
                                            alfa) {
  
  
  
  
  
  
  
  
  
  
  
  
}