module_opt01_home2_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$script(type="text/javascript", src = "busy.js"),
      tags$link(rel="shortcut icon", href="./rmediclogo.jpg"),
      tags$style(HTML("
          /* Evitar scroll horizontal innecesario */
          body, html { overflow-x: hidden !important; }
          
          /* Contenedor principal de la Home */
          .home-wrapper { padding: 20px; text-align: center; }

          /* Logo Principal RMedic */
          .main-logo { 
            max-width: 100%; 
            height: auto; 
            transition: transform 0.3s ease;
          }
          .main-logo:hover { transform: scale(1.02); }

          /* Logo Rotativo ENIAX */
          .logo-eniax-fijo {
            height: 90px; 
            width: auto;
            margin-bottom: 20px;
            transition: opacity 0.6s ease-in-out;
            display: inline-block;
          }
          
          /* Carrusel de Instituciones */
          .carousel-container {
            width: 100%;
            overflow: hidden;
            white-space: nowrap;
            margin-top: 30px;
            padding: 15px 0;
            background: rgba(255, 255, 255, 0.5);
            border-radius: 15px;
          }
          
          .carousel {
            display: inline-block;
            white-space: nowrap;
            animation: moveSlideshow 60s linear infinite;
          }
          
          .carousel img {
            display: inline-block;
            height: 80px; 
            margin: 0 40px;
            vertical-align: middle;
            filter: drop-shadow(0px 4px 6px rgba(0,0,0,0.1));
            transition: transform 0.3s ease;
          }
          
          .carousel img:hover { transform: scale(1.15); }

          @keyframes moveSlideshow {
            0% { transform: translateX(0); }
            100% { transform: translateX(-50%); }
          }

          /* LinkedIn Icon */
          .social-link {
            font-size: 3.5em; 
            color: #0a66c2; 
            text-decoration: none;
            transition: color 0.3s ease;
          }
          .social-link:hover { color: #004182; }
        "))
    ),
    
    div(id = ns("home"), class = "home-wrapper",
        br(),
        fluidRow(
          # Columna Izquierda: Identidad RMedic
          column(3, 
                 tags$a(href = "https://www.linkedin.com/company/r-medic", target = "_blank", rel = "noopener noreferrer",
                        tags$img(src = "rmediclogo.png", style = "cursor: pointer;",  width = "300", class = "main-logo")),
                 
                 #img(src = "rmediclogo.png",),
                 br(), br(),
                 tags$a(icon("linkedin"), 
                        href = "https://www.linkedin.com/company/r-medic/", 
                        target = "_blank", 
                        class = "social-link")
          ),
          
          # Columna Derecha: Alianzas y Partners
          column(9, 
                 br(),
                 # Area de Logos Rotativos (ENIAX)
                 div(style = "min-height: 110px;", 
                     tags$a(
                       href = "https://eniax.care/", 
                       target = "_blank", 
                       rel = "noopener noreferrer",
                       img(
                         id = ns("eniax_rotativo"), 
                         src = "empresas/img_01_ENIAX.png", 
                         class = "logo-eniax-fijo",
                         style = "cursor: pointer;"
                       )
                     )
                 ),
                 
                 # Carrusel Infinito
                 div(class = "carousel-container",
                     div(class = "carousel",
                         # --- SET 1 ---
                         tags$a(href = "https://www.ucc.edu.ar/", target = "_blank", rel = "noopener noreferrer",
                                tags$img(src = "academia/img01_ucc.png", style = "cursor: pointer;")),
                         
                         tags$a(href = "https://www.ucc.edu.ar/ciencias-salud", target = "_blank", rel = "noopener noreferrer",
                                tags$img(src = "academia/img02_ucc_salud.png", style = "cursor: pointer;")),
                         
                         tags$a(href = "https://curf.com.ar/", target = "_blank", rel = "noopener noreferrer",
                                tags$img(src = "academia/img03_reina_2025.png", style = "cursor: pointer;")),
                         
                         tags$a(href = "https://eniax.care/", target = "_blank", rel = "noopener noreferrer",
                                tags$img(src = "academia/img_02_ENIAX.png", style = "cursor: pointer;")),
                         
                         # --- SET 2 (DUPLICADO PARA EFECTO INFINITO) ---
                         tags$a(href = "https://www.ucc.edu.ar/", target = "_blank",
                                tags$img(src = "academia/img01_ucc.png", style = "cursor: pointer;")),
                         
                         tags$a(href = "https://www.ucc.edu.ar/ciencias-salud", target = "_blank",
                                tags$img(src = "academia/img02_ucc_salud.png", style = "cursor: pointer;")),
                         
                         tags$a(href = "https://curf.com.ar/", target = "_blank",
                                tags$img(src = "academia/img03_reina_2025.png", style = "cursor: pointer;")),
                         
                         tags$a(href = "https://eniax.care/", target = "_blank",
                                tags$img(src = "academia/img_02_ENIAX.png", style = "cursor: pointer;"))
                     )
                 )
          )
        ),
        
        # Módulo de Tarjetas
        module_elegant_cards_UI(id = ns("card_card"))
    ),
    
    # Script de Rotación
    tags$script(HTML(sprintf("
      $(document).ready(function() {
        var images = [
          'empresas/img_01_ENIAX.png',
          'empresas/img_02_ENIAX.png',
          'empresas/img_03_ENIAX.png'
        ];
        var currentIndex = 0;
        var $el = $('#%s');

        setInterval(function() {
          currentIndex = (currentIndex + 1) %% images.length;
          $el.css('opacity', 0);
          
          setTimeout(function() {
            $el.attr('src', images[currentIndex]);
            $el.css('opacity', 1);
          }, 600);
        }, 5000);
      });
    ", ns("eniax_rotativo"))))
  )
}

module_opt01_home2_SERVER <- function(id) {
  moduleServer(id, function(input, output, session) {
    module_elegant_cards_SERVER(id = "card_card")
  })
}