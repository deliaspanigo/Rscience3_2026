module_opt99_who_SERVER <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$aver <- renderUI({
      fluidRow(
        tags$head(
          tags$style(HTML("
            .health-card { 
              border: 2px solid #f0f0f0 !important; 
              border-radius: 25px !important; 
              transition: all 0.4s cubic-bezier(0.165, 0.84, 0.44, 1);
              background: white;
              padding: 30px;
              display: flex;
              flex-direction: column;
              align-items: center; 
              height: 100%;
            }
            
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

            /* Estilo Común Botones */
            .btn-profile {
              display: inline-flex;
              align-items: center;
              padding: 10px 20px;
              color: white !important;
              border-radius: 50px;
              font-weight: bold;
              text-decoration: none !important;
              transition: all 0.3s;
              margin-top: 8px;
              font-size: 1em;
              width: 220px; /* Ancho fijo para que queden alineados */
              justify-content: center;
            }
            .btn-profile:hover { transform: scale(1.05); filter: brightness(1.1); }
            .btn-profile i { margin-right: 10px; }

            .btn-linkedin { background-color: #0a66c2; }
            .btn-scholar { background-color: #4285F4; }

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
            
            .mail-footer {
              margin-top: 20px;
              padding-top: 15px;
              border-top: 1px solid #f0f0f0;
              width: 100%;
              color: #37474f; 
              font-family: 'Consolas', monospace;
              font-size: 1em;
              font-weight: 800; 
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
                div(class="expert-item", icon("stethoscope", class="expert-icon", style="color:#0d47a1"), "Estrategia en Investigación Clínica."),
                div(class="expert-item", icon("chart-line", class="expert-icon", style="color:#0d47a1"), "Modelado Estadístico Avanzado."),
                div(class="expert-item", icon("microscope", class="expert-icon", style="color:#0d47a1"), "Metodología de la investigación en Ciencias de la Salud.")
            ),
            
            ### div(class="mb-3", span(class="badge-tech", "Clinical Trials"), span(class="badge-tech", "R Core")),
            
            tags$a(class="btn-profile btn-linkedin", href="https://www.linkedin.com/in/arnaldo-mangeaud-565877108/", target="_blank",
                   icon("linkedin"), "LinkedIn"),
            
            tags$a(class="btn-profile btn-scholar", href="https://scholar.google.com/citations?user=aYmSynEAAAAJ&hl", target="_blank",
                   icon("graduation-cap"), "Google Scholar"),
            
            div(class="mail-footer", icon("envelope", style="color:#0d47a1"), " doctor.arnaldo.m@gmail.com")
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
                div(class="expert-item", icon("laptop-code", class="expert-icon", style="color:#b71c1c"), "Desarrollador R/Shiny y Python."),
                div(class="expert-item", icon("gears", class="expert-icon", style="color:#b71c1c"), "Automatización de análisis de datos y reportes."),
                div(class="expert-item", icon("terminal", class="expert-icon", style="color:#b71c1c"), "Asesor estadístico para ciencias de la salud.")
            ),
            
            ###div(class="mb-3", span(class="badge-tech", "Full Stack R"), span(class="badge-tech", "Health Tech")),
            
            tags$a(class="btn-profile btn-linkedin", href="https://www.linkedin.com/in/deliaspanigo/", target="_blank",
                   icon("linkedin"), "LinkedIn"),
            
            tags$a(class="btn-profile btn-scholar", href="https://scholar.google.be/citations?user=JBwkF9IAAAAJ&hl", target="_blank",
                   icon("graduation-cap"), "Google Scholar"),
            
            div(class="mail-footer", icon("envelope", style="color:#b71c1c"), " d.eliaspanigom@gmail.com")
          )
        )
      )
    })
  })
}