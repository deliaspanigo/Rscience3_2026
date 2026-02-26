module_opt99_who_UI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("aver"))
}

module_opt99_who_SERVER <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$aver <- renderUI({
      
      fluidRow(
        
        # Persona 1
        column(
          width = 6,
          div(
            class = "card shadow-lg p-4 mb-4 rounded text-center",
            style = "background: linear-gradient(135deg, #f0f4ff, #dce7f7); border: none;",
            
            # Foto
            img(src = "ARN.png", height = 180, width = 180,
                class = "mx-auto d-block mb-3 rounded-circle border border-3 border-primary"),
            
            # Nombre
            h3("Dr. Arnaldo Mangeaud", style = "color:#0d47a1; font-weight:bold;"),
            
            # Descripción breve
            p("Asesor estadístico con más de 25 años de experiencia."),
            p("Amplia trayectoria en docencia universitaria en grado y posgrado en ciencias de la salud."),
            
            # Habilidades destacadas
            tags$ul(
              style="text-align:left; display:inline-block;",
              tags$li("✔ Consultoría en Bioestadística."),
              tags$li("✔ Modelado estadístico avanzado."),
              tags$li("✔ Experiencia internacional en investigación clínica.")
            ),
            
            # Footer con iconos
            div(
              style = "margin-top:15px;",
              tags$a(
                icon("linkedin", class = "fa-2x"), 
                href = "https://www.linkedin.com/in/arnaldo-mangeaud-565877108/",
                target = "_blank",
                style = "color:#0a66c2; margin-right:20px;"
              ),
              br(),
              tags$a(
                icon("envelope", class = "fa-2x"), "amangeaud@yahoo.com.ar",
                #href = "mailto:amangeaud@yahoo.com.ar",
                #target = "_blank",
                style = "color:#c62828;"
              )
            )
          )
        ),
        
        # Persona 2
        column(
          width = 6,
          div(
            class = "card shadow-lg p-4 mb-4 rounded text-center",
            style = "background: linear-gradient(135deg, #fff4f0, #f7e3dc); border: none;",
            
            # Foto
            img(src = "DAVID.png", height = 180, width = 180,
                class = "mx-auto d-block mb-3 rounded-circle border border-3 border-danger"),
            
            # Nombre
            h3("Mgter. David Elías Panigo", style = "color:#b71c1c; font-weight:bold;"),
            
            # Descripción breve
            p("Desarrollador R/Shiny y Python."),
            p("Asesor estadístico con 15 años de experiencia."),
            
            # Habilidades destacadas
            tags$ul(
              style="text-align:left; display:inline-block;",
              tags$li("✔ Automatización de reportes para datos clínicos."),
              tags$li("✔ Desarrollo de aplicaciones R/Shiny."),
              tags$li("✔ Análisis de datos en R y Python."),
              tags$li("✔ Innovación en enseñanza de estadística.")
            ),
            
            # Footer con iconos
            div(
              style = "margin-top:15px;",
              tags$a(
                icon("linkedin", class = "fa-2x"),
                href = "https://www.linkedin.com/in/deliaspanigo/",
                target = "_blank",
                style = "color:#0a66c2; margin-right:20px;"
              ), br(),
              tags$a(
                icon("envelope", class = "fa-2x"), "d.eliaspanigo@gmail.com",#
                #href = "mailto:d.eliaspanigo@gmail.com",
                #target = "_blank",
                style = "color:#c62828;"
              )
            )
          )
        )
      )
      
    })
  })
}
