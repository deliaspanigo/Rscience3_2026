# UI del módulo
module_elegant_cards_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$style(HTML("
        /* Título Principal */
        .home-title {
          text-align: center; 
          margin: 40px 0; 
          font-weight: 800; 
          color: #2c3e50;
          position: relative;
          padding-bottom: 15px;
        }
        .home-title::after {
          content: '';
          position: absolute;
          bottom: 0;
          left: 50%;
          transform: translateX(-50%);
          width: 80px;
          height: 4px;
          background: #4287f5;
          border-radius: 2px;
        }

        /* Estilo Base de las Tarjetas */
        .card-custom {
          border-radius: 20px !important;
          transition: all 0.4s cubic-bezier(0.165, 0.84, 0.44, 1);
          border: none !important;
          overflow: hidden;
        }
        .card-custom:hover {
          transform: translateY(-10px);
          box-shadow: 0 15px 35px rgba(0,0,0,0.15) !important;
        }

        /* Headers Estilizados */
        .card-header-custom {
          padding: 25px !important;
          border: none !important;
          color: white !important;
          display: flex;
          flex-direction: column;
          align-items: center;
          text-align: center;
        }
        .card-header-custom h4 {
          margin-top: 10px;
          font-weight: 700;
          letter-spacing: 0.5px;
          font-size: 1.2rem;
        }
        .card-icon-large {
          font-size: 2.5rem;
          margin-bottom: 5px;
          filter: drop-shadow(1px 2px 4px rgba(0,0,0,0.2));
        }

        /* Contenido */
        .content-paragraph {
          padding: 20px !important;
          font-size: 16px;
          color: #455a64;
          line-height: 1.6;
        }
        .bullet-item {
          display: flex;
          margin-bottom: 12px;
        }
        .bullet-icon {
          color: #4287f5;
          margin-right: 10px;
          margin-top: 5px;
          font-size: 0.8em;
        }

        /* Variantes de Color */
        .card-sce { border-top: 5px solid #4287f5 !important; background: #f8fbff !important; }
        .card-sce .header-bg { background: linear-gradient(135deg, #4287f5, #2159b0); }
        
        .card-clinical { border-top: 5px solid #56ab2f !important; background: #f9fff7 !important; }
        .card-clinical .header-bg { background: linear-gradient(135deg, #56ab2f, #a8e063); }
        
        .card-friendly { border-top: 5px solid #ffb347 !important; background: #fffcf5 !important; }
        .card-friendly .header-bg { background: linear-gradient(135deg, #ffb347, #ffcc33); }
      "))
    ),
    
    h1(class = "home-title", "Análisis estadístico para Ciencias de la Salud"),
    
    layout_columns(
      col_width = 4,
      
      # Tarjeta 1: SCE
      card(
        class = "card-custom card-sce",
        card_header(
          class = "card-header-custom header-bg",
          fa_i(name = "gem", class = "card-icon-large"),
          h4("Statistical Computing Environment (SCE)")
        ),
        div(class = "content-paragraph",
            div(class = "bullet-item", icon("check-circle", class = "bullet-icon"), "Entorno computacional estadístico, basado en R."),
            div(class = "bullet-item", icon("check-circle", class = "bullet-icon"), "Versión estable y estándar de librerías y funciones.")
        )
      ),
      
      # Tarjeta 2: Clinical Reporting
      card(
        class = "card-custom card-clinical",
        card_header(
          class = "card-header-custom header-bg",
          fa_i(name = "file-medical", class = "card-icon-large"),
          h4("Clinical Reporting")
        ),
        div(class = "content-paragraph",
            div(class = "bullet-item", icon("check-circle", class = "bullet-icon", style="color:#56ab2f"), "Análisis desarrollado para ciencias de la salud."),
            div(class = "bullet-item", icon("check-circle", class = "bullet-icon", style="color:#56ab2f"), "Estandarización en la toma de decisiones clínicas.")
        )
      ),
      
      # Tarjeta 3: User Friendly
      card(
        class = "card-custom card-friendly",
        card_header(
          class = "card-header-custom header-bg",
          fa_i(name = "user-md", class = "card-icon-large"),
          h4("User Friendly")
        ),
        div(class = "content-paragraph",
            div(class = "bullet-item", icon("check-circle", class = "bullet-icon", style="color:#ffb347"), "Innovador entorno de interacción con el usuario."),
            div(class = "bullet-item", icon("check-circle", class = "bullet-icon", style="color:#ffb347"), "Interpretación automatizada de valores estadísticos.")
        )
      )
    )
  )
}

module_elegant_cards_SERVER <- function(id) {
  moduleServer(id, function(input, output, session) {})
}