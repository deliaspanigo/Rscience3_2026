# UI del módulo
module_elegant_cards_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$style(HTML("
        .card-custom {
          transition: transform 0.3s ease, box-shadow 0.3s ease;
          margin-bottom: 20px;
        }
        .card-custom:hover {
          transform: translateY(-8px);
          box-shadow: 0 12px 24px rgba(0,0,0,0.25) !important;
        }
        .card-header-custom {
          display: flex;
          align-items: center;
          justify-content: center;
          font-size: 1.3rem;
          letter-spacing: 0.5px;
          text-transform: uppercase;
        }
        .card-icon {
          margin-right: 10px;
          font-size: 1.5rem;
        }
        .content-paragraph {
          border-left: 3px solid rgba(0,0,0,0.1);
          padding-left: 15px;
          margin: 15px 0;
        }
      "))
    ),
    h1(
      "Análisis estadístico para Ciencias de la Salud",
      style = "text-align: center; margin: 30px 0; padding-bottom: 15px; border-bottom: 2px solid #ddd;"
    ),
    layout_columns(
      col_width = 4,
      
      # Tarjeta 1
      card(
        class = "card-custom",
        full_screen = FALSE,
        height = "100%",
        card_header(
          div(
            class = "card-header-custom",
            fa_i(name = "gem", fill = "currentColor", class = "card-icon"),
            h4("Statistical Computing Enviroment (SCE)"),
          ),
          style = "background: linear-gradient(135deg, #4287f5, #6e8efb);
                  color: white;
                  border: none;
                  border-radius: 20px 20px 0 0;
                  font-weight: 500;
                  padding: 20px;
                  text-shadow: 1px 1px 2px rgba(0,0,0,0.2);"
        ),
        style = "background: linear-gradient(to bottom, #f0f7ff, white);
                border: 2px solid #4287f5; /* Más ancho y color */
                border-radius: 20px;
                box-shadow: 0 8px 16px rgba(66, 135, 245, 0.3);",
        p(class = "content-paragraph", 
          style = "font-size: 18px;", 
          "- Entorno computacional estadístico, basado en R.", br(),
          "- Versión estable y standard de librerías y funciones estadísticas.")
      ),
      
      # Tarjeta 2
      card(
        class = "card-custom",
        full_screen = FALSE,
        height = "100%",
        card_header(
          div(
            class = "card-header-custom",
            fa_i(name = "file-medical", fill = "currentColor", class = "card-icon"),
            h4("Clinical Reporting")
          ),
          style = "background: linear-gradient(135deg, #56ab2f, #42f565);
                  color: white;
                  border: none;
                  border-radius: 20px 20px 0 0;
                  font-weight: 500;
                  padding: 20px;
                  text-shadow: 1px 1px 2px rgba(0,0,0,0.2);"
        ),
        style = "background: linear-gradient(to bottom, #f0fff0, white);
                border: 2px solid #42f565; /* Más ancho y color */
                border-radius: 20px;
                box-shadow: 0 8px 16px rgba(66, 245, 101, 0.3);",
        p(class = "content-paragraph",
          style = "font-size: 18px;", 
          "- Análisis estadístico desarrollado específicamente para ciencias de la salud.", br(),
          "- Estandarización en el proceso de análisis y toma de decisiones.", br())
      ),
      
      # Tarjeta 3
      card(
        class = "card-custom",
        full_screen = FALSE,
        height = "100%",
        card_header(
          div(
            class = "card-header-custom",
            fa_i(name = "user-md", fill = "currentColor", class = "card-icon"),
            h4("User friedly")
          ),
          style = "background: linear-gradient(135deg, #ffb347, #f5d742);
                  color: white;
                  border: none;
                  border-radius: 20px 20px 0 0;
                  font-weight: 500;
                  padding: 20px;
                  text-shadow: 1px 1px 2px rgba(0,0,0,0.2);"
        ),
        style = "background: linear-gradient(to bottom, #fffcf0, white);
                border: 2px solid #f5d742; /* Más ancho y color */
                border-radius: 20px;
                box-shadow: 0 8px 16px rgba(245, 215, 66, 0.3);",
        p(class = "content-paragraph", 
          style = "font-size: 18px;", 
          "- Innovador entorno de interacción con el usuario.", br(),
          "- Interpretación automatizada de los valores estadísticos obtenidos.")
      )
    )
  )
}


# Server del módulo
module_elegant_cards_SERVER <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      # No se requiere lógica del servidor para esta aplicación
    }
  )
}