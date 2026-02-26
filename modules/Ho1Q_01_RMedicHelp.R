





Ho1Q_01_RMedicHelp_UI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    column(4, 
           radioButtons(inputId = "help_ho_1q",
                        label = h3("Selección de Ayuda Automática"),
                        choices = c("RMedic Here!" = 1,
                                    "Test de Proporciones" = 2,
                                    "Test de Uniformidad" = 3)
           )
    ),
    column(8,
           br(),
           conditionalPanel(condition = "input.help_ho_1q == 1",
                            div(
                              h3_mod("RMedic Here!"),
                              HTML(
                                "Las pruebas de hipótesis más utilizados aplicados a una variable categórica son:<br/>
                      - <b>Test de proporciones</b>.<br/>
                      - <b>Test de uniformidad</b>.<br/>
                      Seleccionando la ayuda de cada uno encontrarás un resumen con
                      detalles teóricos y estructura de la base de datos.<br/>
                      Estos te ayudarán a determinar si estas herramientas pueden ser
                      aplicadas en tu trabajo."
                              )
                            )
           ),
           conditionalPanel(condition = "input.help_ho_1q == 2",
                            div(
                              h3_mod("Test de proporciones"),
                              HTML(
                                "Se aplica sobre una columna de la base de datos.<br>
                                La variable debe tener solo dos categorías.<br>
                                De las dos categorías presentes en la variable se selecciona una de interés.
                                De esta categoría se calcula el valor de proporción respecto al total de datos (proporción observada). <br>
                                Debe elegirse un valor de proporción poblacional bajo hipótesis (proporción esperada).<br>
                                Plantea si la proporción observada de la categoría seleccionada es igual al valor elegido de proporción esperada (bajo hipótesis)."                               ),
                              h3_mod("Juego de Hipótesis"),
                              "Hay tres formas de generar hipótesis en el test de proporciones para una muestra.",
                              br(), br(),
                              HTML("<u><b>Prueba Bilateral</u></b><br>
                              <u><b>Hipótesis Nula (Ho):</b></u> La diferencia de proporciones es igual a cero. <br>
                              <u><b>Hipótesis Alternativa (Hi):</b></u> La diferencia de proporciones es distinta de cero.<br>"
                              ),br(), br(),
                              HTML("<u><b>Prueba Unilateral Izquierda</b></u><br>
                              <u><b>Hipótesis Nula (Ho):</b></u> La proporción de la categoría seleccionada es igual o mayor al valor elegido. <br>
                              <u><b>Hipótesis Alternativa (Hi):</b></u> La proporción de la categoría seleccionada es menor al valor elegido.<br>"
                              ),br(), br(),
                              HTML("<u><b>Prueba Unilateral Derecha</b></u><br>
                              <u><b>Hipótesis Nula (Ho):</b></u> La proporción de la categoría seleccionada es igual o menor al valor elegido. <br>
                              <u><b>Hipótesis Alternativa (Hi):</b></u> La proporción de la categoría seleccionada es mayor al valor elegido.<br>"
                              )
                             
                            )
           ),
           conditionalPanel(condition = "input.help_ho_1q == 3",
                            div(
                              h3_mod("Test de Uniformidad"),
                              HTML(
                              "Se aplica sobre una columna de la base de datos.<br>
                              La variable debe tener al menos dos categorías.<br>
                              Plantea si todas las categorías de la variable poseen la misma frecuencia.<br>
                              En otras palabras plantea si todas las categorías de la variable poseen la misma proporción.<br>"),
                              h3_mod("Juego de Hipótesis"),
                              HTML("
                              <u><b>Hipótesis Nula (Ho):</u></b> Las frecuencias de todas las categorías son iguales. <br>
                              <u><b>Hipótesis Alternativa (Hi):</u></b> Al menos una categoría presenta una frecuencia diferente.<br>"
                              ),br(), br()
                            )
           ),
    )
  )
  
  
  
}






## Segmento del server

Ho1Q_01_RMedicHelp_SERVER <- function(input, output, session,
                                            minibase,
                                            decimales,
                                            control_ejecucion,
                                            tablas_1q,
                                            alfa = alfa) {
  
  
  
  
  
  
  
  
  
  
  
  
}







