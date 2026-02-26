module_opt05_contacto_UI <- function(id) {
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
    
    # Footer estilizado
    div(
      id = ns("home"),
      style = "background: linear-gradient(135deg, #0d47a1, #1976d2);
               color: white; padding: 40px 0; text-align: center;",
      
      # Logo arriba
      img(src = "rmediclogo.jpg", width = 120, height = 120,
          class = "mx-auto d-block mb-3 rounded-circle shadow"),
      
      # Texto
      h4("RMedic - Software Estadístico"),
      p("Análisis de datos para ciencias de la salud.",
        style = "max-width: 600px; margin: 0 auto 20px; opacity:0.9;"),
      
      # Íconos sociales
      div(
        class = "social-icons",
        style = "margin-bottom:20px;",
        
        tags$a(
          icon("linkedin", class = "fa-2x"),
          href = "https://www.linkedin.com/company/r-medic/",
          target = "_blank",
          style = "color:white; margin:0 15px;",
          style="font-size: 4em;"
        )#,
        # tags$a(
        #   icon("github", class = "fa-2x"),
        #   href = "https://github.com/deliaspanigo/Rscience2",
        #   target = "_blank",
        #   style = "color:white; margin:0 15px;"
        # ),
        # tags$a(
        #   icon("envelope", class = "fa-2x"),
        #   href = "mailto:contacto@rmedic.com",
        #   target = "_blank",
        #   style = "color:white; margin:0 15px;"
        # )
      )#,
      
      # Copyright
      #p("© 2025 RMedic | Universidad Nacional de Córdoba",
      #  style = "font-size: 0.9rem; opacity: 0.7; margin-top:10px;")
    )
  )
}




module_opt05_contacto_SERVER <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    
  })
}