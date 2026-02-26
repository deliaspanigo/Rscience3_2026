


Ho1C_01_RMedicHelp_UI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    column(4,
           radioButtons(inputId = "help_ho_1c",
                        label = h3("Selección de Ayuda Automática"),
                        choices = c("RMedic Here!" = 1,
                                    "Test t (Una muestra)" = 2,
                                    "Test de Wilcoxon (Una muestra)" = 3,
                                    "Test Normalidad (Shapiro-Wilk)" = 4,
                                    "Test Chi Cuadrado (Una muestra)" = 5)
           )),
    column(8,
           conditionalPanel(condition = "input.help_ho_1c == 1", 
                            div(
                              h3("RMedic Here!"),
                              HTML(
                                "Los gráficos más utilizados aplicados a una variable numérica son:<br/>
                      - <b>Test t (Una muestra)</b>.<br/>
                      - <b>Test de Wilcoxon (Una muestra)</b>.<br/>
                      - <b>Test Normalidad (Shapiro-Wilk)</b>.<br/>
                      - <b>Test Chi Cuadrado (Una muestra)</b>.<br/>
                      Seleccionando la ayuda de cada uno encontrarás un resumen con
                      detalles teóricos y estructura de la base de datos.<br/>
                      Estos te ayudarán a determinar si estas herramientas pueden ser
                      aplicadas en tu trabajo."
                              )
                            )
                            ),
           conditionalPanel(condition = "input.help_ho_1c == 2", 
                            div(
                              h3_mod("Test t (Una muestra)"),
                              HTML(
                                "El 'Test t' posee varias formas de ser utilizado.<br>
                        En este caso, se aplica el test sobre una única columna de la base de datos.<br>
                        La columna debe ser numérica.<br>
                        La naturaleza de los datos debe ser cuantitativa. <br>
                        Tiene como requisito para su utilización verificar previamente que los datos poseen distribución normal.<br>
                        Utiliza la media de los datos ingresados (media observada) y compara esta media con un valor de referencia a elección (media esperada).<br>
                        Plantea si la media observada es igual a la media esperada."                               ),
                              h3_mod("Juego de Hipótesis"),
                              "Hay tres formas de generar hipótesis en el test de proporciones para una muestra.",
                              br(), br(),
                              HTML("<u><b>Prueba Bilateral</u></b><br>
                              <u><b>Hipótesis Nula (Ho):</u></b> La media de la muestra es igual al valor elegido. <br>
                              <u><b>Hipótesis Alternativa (Hi):</u></b> La media de la muestra es diferente al valor elegido.<br>"
                              ),br(), br(),
                              HTML("<u><b>Prueba Unilateral Izquierda</u></b><br>
                              <u><b>Hipótesis Nula (Ho):</u></b> La media de la muestra es igual o mayor al valor elegido. <br>
                              <u><b>Hipótesis Alternativa (Hi):</u></b> La media de la muestra es menor al valor elegido.<br>"
                              ),br(), br(),
                              HTML("<u><b>Prueba Unilateral Derecha</u></b><br>
                              <u><b>Hipótesis Nula (Ho):</u></b> La media de la muestra es igual o menor al valor elegido. <br>
                              <u><b>Hipótesis Alternativa (Hi):</u></b> La media de la muestra es mayor al valor elegido.<br>"
                              )
                              
                            )
           ),
           conditionalPanel(condition = "input.help_ho_1c == 3", 
                            div(
                              h3_mod("Test de Wilcoxon (Una muestra)"),
                              HTML(
                                "Se aplica sobre una columna de la base de datos.<br>
                            La columna debe ser numérica.<br>
                            La naturaleza de los datos puede ser cuantitativa o cualitativa ordinal representada con números.<br>
                            Internamente el test de Wilcoxon estima la posición de la mediana a partir de la media del ranking de los datos.<br>
                            Utiliza la mediana de los datos ingresados (mediana observada) y compara esta mediana con un valor de referencia a elección (mediana esperada).
                            Plantea si la mediana de la muestra es igual al valor seleccionado."                               ),
                              h3_mod("Juego de Hipótesis"),
                              "Hay tres formas de generar hipótesis en el test de proporciones para una muestra.",
                              br(), br(),
                              HTML("<u><b>Prueba Bilateral</u></b><br>
                              <u><b>Hipótesis Nula (Ho):</u></b> La mediana de la muestra es igual al valor elegido. <br>
                              <u><b>Hipótesis Alternativa (Hi):</u></b> La mediana de la muestra es diferente al valor elegido.<br>"
                              ),br(), br(),
                              HTML("<u><b>Prueba Unilateral Izquierda</u></b><br>
                              <u><b>Hipótesis Nula (Ho):</u></b> La mediana de la muestra es igual o mayor al valor elegido. <br>
                              <u><b>Hipótesis Alternativa (Hi):</u></b> La mediana de la muestra es menor al valor elegido.<br>"
                              ),br(), br(),
                              HTML("<u><b>Prueba Unilateral Derecha</u></b><br>
                              <u><b>Hipótesis Nula (Ho):</u></b> La mediana de la muestra es igual o menor al valor elegido. <br>
                              <u><b>Hipótesis Alternativa (Hi):</u></b> La mediana de la muestra es mayor al valor elegido.<br>"
                              )
                              
                            )
           ),
           conditionalPanel(condition = "input.help_ho_1c == 4", 
                            div(
                              h3_mod("Test Normalidad (Shapiro-Wilk)"),
                              HTML(
                              "Se aplica sobre una columna de la base de datos.<br>
                              La columna debe ser numérica.<br>
                              La naturaleza de los datos debe ser cuantitativa.<br>
                              Es un test sobre la distribución de los datos obtenidos.<br>
                              Plantea si la variable posee distribución Normal.<br>"                               ),
                              h3_mod("Juego de Hipótesis"),
                              
                              HTML("
                              <u><b>Hipótesis Nula (Ho):</u></b> La variable posee distribución normal. <br>
                              <u><b>Hipótesis Alternativa (Hi):</u></b> La variable no posee distribución normal.<br>"
                              )
                              
                            )
           ),
           conditionalPanel(condition = "input.help_ho_1c == 5", 
                            div(
                              h3_mod("Test Chi Cuadrado (Una muestra)"),
                              HTML(
                                "El 'Test Chi Cuadrado' posee varias formas de ser utilizado.<br>
                        En este caso, se aplica el test sobre una única columna de la base de datos.<br>
                        La columna debe ser numérica.<br>
                        La naturaleza de los datos debe ser cuantitativa. <br>
                        A partir de los datos de la variable se estima la varianza de la muestra (varianza observada).<br>
                        Se compara la varianza de los datos con un valor de referencia (varianza esperada).<br>
                        Plantea si la varianza observada es igual a la varianza esperada."                               ),
                              h3_mod("Juego de Hipótesis"),
                              "Hay tres formas de generar hipótesis en el test Chi Cuadrado para la varianza de una muestra.",
                              br(), br(),
                              HTML("<u><b>Prueba Bilateral</u></b><br>
                              <u><b>Hipótesis Nula (Ho):</u></b> La varianza de la muestra es igual al valor elegido. <br>
                              <u><b>Hipótesis Alternativa (Hi):</u></b> La varianza de la muestra es diferente al valor elegido.<br>"
                              ),br(), br(),
                              HTML("<u><b>Prueba Unilateral Izquierda</u></b><br>
                              <u><b>Hipótesis Nula (Ho):</u></b> La varianza de la muestra es igual o mayor al valor elegido. <br>
                              <u><b>Hipótesis Alternativa (Hi):</u></b> La varianza de la muestra es menor al valor elegido.<br>"
                              ),br(), br(),
                              HTML("<u><b>Prueba Unilateral Derecha</u></b><br>
                              <u><b>Hipótesis Nula (Ho):</u></b> La varianza de la muestra es igual o menor al valor elegido. <br>
                              <u><b>Hipótesis Alternativa (Hi):</u></b> La varianza de la muestra es mayor al valor elegido.<br>"
                              )
                              
                            )
           )
           )
           )
  
  
  
}






## Segmento del server
Ho1C_01_RMedicHelp_SERVER <- function(input, output, session,
                                            minibase,
                                            decimales,
                                            control_ejecucion,
                                            tablas_1c,
                                            alfa) {
  
  
  
  
  
  
  
  
  
  
  
  
}







