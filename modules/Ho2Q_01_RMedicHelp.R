





Ho2Q_01_RMedicHelp_UI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    column(4, 
           radioButtons(inputId = "help_ho_2q",
                        label = h3("Selección de Ayuda Automática"),
                        choices = c("RMedic Here!" = 1,
                                    "Test de Dos Proporciones" = 2,
                                    "Test Chi Cuadrado" = 3,
                                    "Regresión Logística Simple" = 4,
                                    "Otros" = 5)
           )
    ),
    column(8,
           br(),
           conditionalPanel(condition = "input.help_ho_2q == 1",
                            div(
                              h3("RMedic Here!"),
                              HTML(
                                "Las pruebas de hipótesis más utilizados aplicados a una variable categórica son:<br/>
                      - <b>Test de Dos Proporciones</b>.<br/>
                      - <b>Test Chi Cuadrado</b>.<br/>
                      - <b>Regresión Logística Simple</b>.<br/>
                      - <b>Otros</b>.<br/>
                      Seleccionando la ayuda de cada uno encontrarás un resumen con
                      detalles teóricos y estructura de la base de datos.<br/>
                      Estos te ayudarán a determinar si estas herramientas pueden ser
                      aplicadas en tu trabajo."
                              )
                            )
           ),
           conditionalPanel(condition = "input.help_ho_2q == 2",
                            div(
                              h3_mod("Test de Dos Proporciones"),
                              HTML(
                                "Se aplica sobre dos columnas de la base de datos.<br>
                              Cada variable debe contener solo dos categorías.<br>
                              Una variable determina dos grupos a comparar.<br>
                              La otra variable determina los éxitos dentro de cada grupo.<br>
                              Plantea si las proporciones de éxito de ambos grupos son iguales entre si a través de la siguiente idea:
                              Dos proporciones son iguales si la diferencia entre ellas es cero.<br> "
                              ),
                              h3_mod("Juego de Hipótesis"),
                              HTML("<u><b>Hipótesis Nula (Ho):</u></b> La diferencia de proporciones es igual a cero. <br>
                              <u><b>Hipótesis Alternativa (Hi):</u></b> La diferencia de proporciones es distinta de cero."
                              )
                            )
           ),
           conditionalPanel(condition = "input.help_ho_2q == 3",
                            div(
                              h3_mod("Test Chi Cuadrado"),
                              HTML(
                                "Se aplica sobre dos columnas de la base de datos.<br>
                                Con los pares de datos de ambas columnas se conforma una tabla de doble entrada (contingencia) sobre la cual se realizan diferentes estimaciones.<br>
                                Una de las variables tendrá sus categorías en filas y la otra en columnas.<br>
                                Para el análisis estadístico, el ingreso en filas o en columnas de ambas variables es indistinto ya que se obtienen los mismos resultados.<br>
                                Si el marco del estudio lo permite, se recomienda fuertemente colocar en filas la variable 'Causa' y en columnas 'Efecto' ya que facilita la interpretación de los resultados obtenidos.<br>
                                El test Chi Cuadrado plantea la asociación entre ambas variables. "
                              ),
                              h3_mod("Juego de Hipótesis"),
                              HTML("<u><b>Hipótesis Nula (Ho):</u></b> Las variables son independientes (no asociadas). <br>
                              <u><b>Hipótesis Alternativa (Hi):</u></b> Las variables no son independentes (asociadas)."
                              )
                            )
           ),
           conditionalPanel(condition = "input.help_ho_2q == 4",
                            div(
                              h3_mod("Regresión Logística Simple"),
                              HTML(
                                "La Regresión Logística Simple posee varias formas de ser utilizada.<br>
                      Las siguientes recomendaciones corresponden sólo al caso en que ambas variables son categóricas.<br>
                      Es un requisito que cada variable contenga sólo dos categorías.
                      La regresión logística simple Es un modelo de predicción en el que cada variable cumple un rol específico.
                      Por defecto, la primera variable ingresada es la variable independiente (X) y la segunda es la variable dependiente (Y).
                      Las dos categorías de cada variable serán transformadas a '0' y '1' según se seleccionen las opciones del test.
                      El valor '1' corresponderá a la categoría considerada como 'Éxito' dentro del estudio.<br>
                      El marco de aplicación (y no la estadística) determina cual de las variables
                      es independiente (X) y cual dependiente (Y).<br>
                      Como toda regresión, tendrá un juego de hipótesis para la pendiente y otra para la ordenada al origen.
                      Por lo general sólo tiene relevancia el juego de hipótesis de la pendiente."
                              ),
                              h3_mod("Juego de Hipótesis"),
                              HTML("<u><b>Pendiente</u></b><br>
                            <u><b>Hipótesis Nula (Ho):</u></b> Pendiente igual a cero (No existe una relación logística entre las variables).<br>
                            <u><b>Hipótesis Alternativa (Hi):</u></b> Pendiente distinta de cero (Existe una relación logística entre las variables)<br> 
                            <br>
                            
                            <u><b>Ordenada</u></b><br>
                            </u></b>Hipótesis Nula (Ho):</u></b> La ordenada es igual a cero.<br>
                            <u><b>Hipótesis Alternativa (Hi):</u></b> La ordenada es distinta a cero.<br>
                            <br>
                            "
                              )
                            )
           ),
           conditionalPanel(condition = "input.help_ho_2q == 5",
                            div(
                              h3_mod("Otros"),
                              HTML(
                "La pestaña 'Otros' contiene un conjunto de herramientas aplicables a tablas de contingencia.<br>
                Encontraremos: Odd Ratios (OR), Riego Relativo (RR), Valores Predictivos (VP), Sensibilidad y Especificidad.<br>
                Estas herramientas son aplicadas sobre dos columna de la base de datos.<br>
                Cada variable debe contener solo dos categorías.<br>
                Generalmente estas herramientas suelen aplicarse luego de aplicar un Test Chi Cuadrado.<br>
                <br>
                
                Dentro del contenido de cada herramienta se detalla su fórmula y resolución paso a paso.<br>
                <br>
                
                <u><b>1) Odd Ratios (OR)</u></b><br> 
                Se presenta el estimado y el intervalo de confianza (95%).<br>
                <br>
                <u><b>2) Riego Relativo (RR)</u></b><br>
                Se presenta el estimados y el intervalo de confianza (95%).<br>
                Además presenta la prueba de hipótesis para contrastar si el Riesgo Relativo es igual a 1.<br>
                <br>                        
                &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<u><b>Juego de Hipótesis</u></b><br>
                &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<u><b>Hipótesis Nula (Ho):</u></b> Riesgo Relativo poblacional igual a 1.<br>
                &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<u><b>Hipótesis Alternativa (Hi):</u></b> Riesgo Relativo poblacional diferente de 1.<br>
                <br>        
                <u><b>3) Valores Predictivos (VP)</u></b><br>
                Se presenta el estimado de Valor predictivo positivo y negativo.<br>
                <br>
                <u><b>4) Sensibilidad y Especificidad</u></b><br>
                Se presenta el estimado de sensibilidad y de especificidad.<br>
                <br>"
                              )
                            )
           )
    )
  )
  
  
  
}






## Segmento del server
Ho2Q_01_RMedicHelp_SERVER <- function(input, output, session,
                                            minibase,
                                            decimales,
                                            control_ejecucion,
                                            tablas_2q,
                                            alfa) {
  
  
  
  
  
  
  
  
  
  
  
  
}







