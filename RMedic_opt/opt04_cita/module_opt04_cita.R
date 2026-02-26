module_opt04_cita_UI <- function(id) {
  ns <- NS(id)
  
  div(
    tagList(
      tags$head(
        tags$script(type="text/javascript", src = "busy.js"),
        tags$link(rel="shortcut icon", href="./rmediclogo.jpg"),
        tags$script(type="text/javascript", "var switchTo5x=true"),
        tags$script(
          type="text/javascript",
          'stLight.options({
             publisher: "675b3562-a081-470a-9fc4-3dd6a712209d",
             doNotHash: true, doNotCopy: true, hashAddressBar: false
          })'
        )
      )
    ),
    
    div(
      id = ns("home"),
      style = "padding: 40px;",
      
      fluidRow(
        column(
          3,
          img(src = "rmediclogo.jpg", width = 180, height = 180,
              class = "mx-auto d-block mb-3 rounded-circle shadow")
        ),
        
        column(
          9,
          div(
            class = "card shadow-lg p-4 rounded",
            style = "background:#fafafa; border-left:6px solid #ffffff;",
            
            # Título
            h4("¿Puedo usar RMedic para los resultados de mis publicaciones?",
               style="color:#0d47a1; font-weight:bold;"),
            
            # Texto principal
            p(strong("¡Claro que sí!"), " Si lo haces, debes:"),
            
            # Lista con íconos
            tags$ul(
              style="list-style-type:none; padding-left:0;",
              tags$li(icon("check", class="text-success"),
                      " Incluir en 'Materiales y Métodos' a ",
                      strong("RMedic"), " como software estadístico."),
              tags$li(icon("check", class="text-success"),
                      " Citar en tu 'Bibliografía' la siguiente referencia:")
            ),
            
            h3_mod("Materiales y Métodos - RMedic"),
            
            tags$blockquote(
              style="background:#eef3fb; padding:15px; margin:10px 0; 
                     border-left:5px solid #ffffff; font-style:normal;",
              
            "Se utilizó el software estadístico RMedic versión 3.2.4 para todas las tablas, gráficos y análisis estadísticos."
            ),
            tags$button(
              id = "copy_btn04",
              class = "btn btn-outline-primary",
              icon("clipboard"), "Copiar Materiales y Métodos - RMedic",
              onclick = "navigator.clipboard.writeText(Se utilizó el software estadístico RMedic versión 3.2.4 para todas las tablas, gráficos y análisis estadísticos.'); alert('Texto copiado: Materiales y Métodos - RMedic');"
            ),
            br(),
            
            # Cita en bloque estilizado
            h3_mod("Cita Appa 7ma"),
           
            tags$blockquote(
              style="background:#eef3fb; padding:15px; margin:10px 0; 
                     border-left:5px solid #ffffff; font-style:normal;",
              strong("Mangeaud A, Elías Panigo DH. 2018. "),
              em("R-Medic. Un programa de análisis estadísticos sencillo e intuitivo."),
              " Revista Methodo 3(1): 18-22."
            ),
            tags$button(
              id = "copy_btn",
              class = "btn btn-outline-primary",
              icon("clipboard"), "Copiar Cita Appa - RMedic",
              onclick = "navigator.clipboard.writeText('Mangeaud A, Elías Panigo DH. 2018. R-Medic. Un programa de análisis estadísticos sencillo e intuitivo. Revista Methodo 3(1): 18-22.'); alert('Texto copiado: Cita Appa 7ma - RMedic');"
            ),
            br(),
            
            h3_mod("Cita Vancuver"),
            
            tags$blockquote(
              style="background:#eef3fb; padding:15px; margin:10px 0; 
                     border-left:5px solid #ffffff; font-style:normal;",
              strong("Mangeaud A, Elías Panigo DH. (2018) "),
              em("R-Medic. Un programa de análisis estadísticos sencillo e intuitivo."),
              " Revista Methodo 3(1): 18-22."
            ),
            tags$button(
              id = "copy_btn02",
              class = "btn btn-outline-primary",
              icon("clipboard"), "Copiar Cita Vancuver - RMedic",
              onclick = "navigator.clipboard.writeText('Mangeaud A, Elías Panigo DH. (2018). R-Medic. Un programa de análisis estadísticos sencillo e intuitivo. Revista Methodo 3(1): 18-22.'); alert('Texto copiado: Cita Vancuver - RMedic');"
            ),
            # Botón PDF
            br(),
            h3_mod("Descargar artículo"),
            a("📄 Descargar artículo (PDF)",
              href = "RMedic_Cita.pdf",
              target = "_blank",
              class = "btn btn-primary mt-2",
              style="text-decoration:none;")
          )
        )
      )
    )
  )
}





module_opt04_cita_SERVER <- function(id){}