module_opt99_who_SERVER <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$aver <- renderUI({
      fluidRow(
        tags$head(
          tags$style(HTML("
            /* Estenedor de la tarjeta */
            .health-card { 
              border: 2px solid #f0f0f0 !important; 
              border-radius: 25px !important; 
              transition: all 0.4s cubic-bezier(0.165, 0.84, 0.44, 1);
              background: white;
              padding: 30px;
              display: flex;
              flex-direction: column;
              align-items: center; /* Centra todo el contenido */
            }
            
            /* Efecto Hover Pro */
            .card-arnaldo:hover { 
              transform: translateY(-10px); 
              border-color: #0d47a1 !important; 
              box-shadow: 0 15px 30px rgba(13, 71, 161, 0.1) !important;
            }
            .card-david:hover { 
              transform: translateY(-10px); 
              border-color: #b71c1c !important; 
              box-shadow: 0 15px 30px rgba(183, 28, 28, 0.1) !important;
            }

            /* Lista de items centrada pero alineada */
            .expert-list {
              text-align: left;
              display: inline-block;
              margin: 20px 0;
            }
            .expert-item { 
              display: flex; 
              align-items: center; 
              margin-bottom: 12px; 
              font-size: 1.05em; 
              color: #37474f; 
            }
            .expert-icon { margin-right: 15px; font-size: 1.2em; }

            /* Botón de LinkedIn masivo */
            .btn-linkedin {
              display: inline-flex;
              align-items: center;
              padding: 12px 25px;
              background-color: #0a66c2;
              color: white !important;
              border-radius: 50px;
              font-weight: bold;
              text-decoration: none !important;
              transition: background 0.3s;
              margin-top: 15px;
              font-size: 1.1em;
            }
            .btn-linkedin:hover { background-color: #004182; transform: scale(1.05); }
            .btn-linkedin i { margin-right: 10px; }

            .badge-tech {
              background: #eceff1;
              color: #455a64;
              padding: 6px 14px;
              border-radius: 10px;
              font-size: 0.8em;
              font-weight: 700;
              margin: 4px;
              text-transform: uppercase;
            }
          "))
        ),
        
        # Tarjeta 1 - Arnaldo
        column(
          width = 6,
          div(
            class = "card health-card card-arnaldo shadow-sm text-center",
            img(src = "ARN.png", height = 160, width = 160, class = "rounded-circle border border-4 mb-3", style="border-color:#0d47a1;"),
            
            h2("Dr. Arnaldo Mangeaud", style = "color:#0d47a1; font-weight:800; margin-bottom:5px;"),
            span("Senior Biostatistics Consultant", style="color:#90a4ae; font-weight:700; font-size:0.9em; letter-spacing:1px;"),
            
            div(class="expert-list",
                div(class="expert-item", icon("shield-medical", class="expert-icon", style="color:#0d47a1"), "Estrategia en Investigación Clínica"),
                div(class="expert-item", icon("chart-line", class="expert-icon", style="color:#0d47a1"), "Modelado Estadístico Avanzado"),
                div(class="expert-item", icon("microscope", class="expert-icon", style="color:#0d47a1"), "Validación de Protocolos Médicos")
            ),
            
            div(class="mb-3", span(class="badge-tech", "Clinical Trials"), span(class="badge-tech", "R Core")),
            
            tags$a(class="btn-linkedin", href="https://www.linkedin.com/in/arnaldo-mangeaud-565877108/", target="_blank",
                   icon("linkedin"), "Conectar en LinkedIn")
          )
        ),
        
        # Tarjeta 2 - David
        column(
          width = 6,
          div(
            class = "card health-card card-david shadow-sm text-center",
            img(src = "DAVID.png", height = 160, width = 160, class = "rounded-circle border border-4 mb-3", style="border-color:#b71c1c;"),
            
            h2("Mgter. David Elías Panigo", style = "color:#b71c1c; font-weight:800; margin-bottom:5px;"),
            span("Data Systems & Analytics Lead", style="color:#90a4ae; font-weight:700; font-size:0.9em; letter-spacing:1px;"),
            
            div(class="expert-list",
                div(class="expert-item", icon("laptop-code", class="expert-icon", style="color:#b71c1c"), "Arquitectura R / Shiny Expert"),
                div(class="expert-item", icon("gears", class="expert-icon", style="color:#b71c1c"), "Automatización de Datos de Salud"),
                div(class="expert-item", icon("terminal", class="expert-icon", style="color:#b71c1c"), "Desarrollo Python & Cloud")
            ),
            
            div(class="mb-3", span(class="badge-tech", "Full Stack R"), span(class="badge-tech", "Health Tech")),
            
            tags$a(class="btn-linkedin", href="https://www.linkedin.com/in/deliaspanigo/", target="_blank",
                   icon("linkedin"), "Conectar en LinkedIn")
          )
        )
      )
    })
  })
}