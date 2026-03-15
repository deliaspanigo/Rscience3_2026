
module_action002_show_database_ui <- function(id){
  ns <- shiny::NS(id)
  
  uiOutput(ns("show_df"))

  #
  # div(htmlOutput(ns("TextBase_InfoDataSet_01"),
  #                DTOutput(ns("df_database"))
  # ))
}



#' Server para el módulo de visualización de la base de datos
#'
#' @param id ID del módulo
#' @param output_list_database Lista reactiva proveniente del módulo de importación
module_action002_show_database_server <- function(id, output_list_database) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      # --- 1. VALIDACIÓN ---
      # Esta función asegura que tengamos datos válidos antes de intentar renderizar
      check_ok <- reactive({
        out <- output_list_database()
        # Solo es OK si existe la lista, hay un dataframe y hay un nombre de archivo
        req(out, out$database, out$original_file_name)
        TRUE
      })
      
      # --- 2. COMPONENTES DE TEXTO (Solo se ven si hay datos) ---
      
      output$TextBase_InfoDataSet_01 <- renderText({
        req(check_ok())
        
        df   <- output_list_database()$database
        nome <- output_list_database()$original_file_name
        
        texto_salida <- sprintf(
          "<u><b>Archivo:</b></u> %s <br/>
           <u><b>Variables (Columnas):</b></u> %s variables.<br/>
           <u><b>Unidades (Filas o repeticiones):</b></u> %s unidades.<br/>",
          nome, ncol(df), nrow(df)
        )
        
        paste0("<div style='font-size: 20px;'>", texto_salida, "</div>") %>% HTML()
      })
      
      output$title01 <- renderText({
        req(check_ok())
        "Visualización de la Base de Datos"
      })
      
      # --- 3. RENDERIZADO DE LA TABLA DT ---
      
      output$df_database <- renderDT({
        req(check_ok())
        
        mi_tabla <- output_list_database()$database
        
        DT::datatable(
          mi_tabla,
          rownames = TRUE,
          options = list(
            autowidth = TRUE,
            order = list(list(0, 'asc')),
            pageLength = 10,
            lengthMenu = c(10, 50, 100),
            dom = 'frtip',
            language = list(
              search = "Búsqueda:",
              lengthMenu = "Mostrar _MENU_ registros",
              info = "Mostrando _START_ al _END_ de _TOTAL_ registros",
              paginate = list(previous = "Anterior", `next` = "Siguiente")
            ),
            # Estilos Century Gothic y colores solicitados
            initComplete = JS("
              function(settings, json) {
                $(this.api().table().header()).css({
                  'font-family': 'Century Gothic',
                  'font-size':'125%',
                  'background-color': '#008000',
                  'color': '#fff'
                });
                $('body').css({'font-family': 'Century Gothic'});
              }
            "),
            rowCallback = JS("
              function(row, data, index) {
                if(index % 2 === 0) {
                  $(row).css('background-color', 'lightblue');
                } else {
                  $(row).css('background-color', 'lightgreen');
                }
              }
            ")
          )
        )
      })
      
      # --- 4. ORQUESTADOR DINÁMICO DE LA UI ---
      
      output$show_df <- renderUI({
        # Verificamos si hay base de datos cargada EN ESTE MOMENTO
        # Si el usuario cambió el selector en Settings, esto será NULL inmediatamente
        base_datos <- output_list_database()$database
        
        if (!is.null(base_datos)) {
          
          # CASO A: MOSTRAR LA TABLA (Estado Confirmado)
          div(
            style = "overflow-x: auto; width: 100%;",
            fluidRow(
              column(1),
              column(10, 
                     htmlOutput(ns("TextBase_InfoDataSet_01")),
                     br(),
                     h2(tags$u(tags$b(textOutput(ns("title01"))))),
                     # withSpinner requiere la librería shinycssloaders
                     shinycssloaders::withSpinner(DTOutput(ns("df_database")))
              ),
              column(1)
            )
          )
          
        } else {
          
          # CASO B: MOSTRAR LOGO (Estado Inicial o Cambio de Selección)
          fluidRow(
            column(12, 
                   div(
                     style = "display: flex; flex-direction: column; align-items: center; 
                              justify-content: center; height: 70vh; opacity: 0.4;",
                     tags$img(src = "png/img_01_RMedic.png", width = "250px"),
                     tags$h3("Esperando confirmación de datos...", 
                             style = "color: #888; margin-top: 20px; font-family: 'Century Gothic';")
                   )
            )
          )
        }
      })
      
    }
  )
}


#######################################################################################