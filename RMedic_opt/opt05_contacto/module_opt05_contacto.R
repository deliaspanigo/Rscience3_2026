module_opt05_contacto_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$script(type="text/javascript", src = "busy.js"),
      tags$link(rel="shortcut icon", href="./rmediclogo.jpg"),
      tags$style(HTML("
        .footer-rmedic {
          background: #0a192f; 
          color: #e6f1ff;
          padding: 80px 20px;
          border-top: 4px solid #1976d2;
          font-family: 'Segoe UI', Roboto, sans-serif;
        }
        .footer-logo {
          transition: all 0.5s ease;
          border: 3px solid rgba(25, 118, 210, 0.2);
        }
        .footer-logo:hover {
          transform: scale(1.08);
          border-color: #1976d2;
        }
        .btn-linkedin-solo {
          display: inline-flex;
          align-items: center;
          padding: 18px 45px;
          background: transparent;
          border: 2px solid #0a66c2;
          color: white !important;
          border-radius: 50px;
          text-decoration: none !important;
          font-weight: 600;
          font-size: 1.2em;
          letter-spacing: 0.5px;
          transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
        }
        .btn-linkedin-solo:hover {
          background: #0a66c2;
          box-shadow: 0 10px 20px rgba(10, 102, 194, 0.3);
          transform: translateY(-3px);
        }
        .btn-linkedin-solo i {
          margin-right: 12px;
          font-size: 1.3em;
        }
        .footer-divider {
          width: 40px;
          height: 3px;
          background: #1976d2;
          margin: 25px auto;
          border-radius: 10px;
        }
      "))
    ),
    
    div(
      class = "footer-rmedic text-center",
      id = ns("home"),
      
      div(
        class = "container",
        
        # Logo
        img(src = "rmediclogo.jpg", width = 120, height = 120,
            class = "footer-logo rounded-circle mb-4 shadow-lg"),
        
        # Títulos
        h1("RMedic", style = "font-weight: 800; letter-spacing: 1.5px; margin-bottom: 0;"),
        h4("Advanced Health Data Analytics", style = "font-weight: 300; opacity: 0.7; margin-top: 8px;"),
        
        div(class = "footer-divider"),
        
        p("Soluciones estadísticas para ciencias de la salud.",
          style = "color: #8892b0; font-size: 1.1em; margin-bottom: 40px;"),
        
        # Botón LinkedIn Único
        tags$a(
          class = "btn-linkedin-solo",
          href = "https://www.linkedin.com/company/r-medic/",
          target = "_blank",
          icon("linkedin"), "LINKEDIN"
        ),
        
        # Copyright Minimalista
        p(style = "font-size: 0.8em; color: #445069; margin-top: 60px; font-weight: 500;",
          paste("©", format(Sys.Date(), "%Y"), "RMEDIC | TECNOLOGÍA PARA LA SALUD"))
      )
    )
  )
}