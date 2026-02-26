module_opt01_home2_UI <- function(id) {
  ns <- NS(id)
  # tabPanel(
    # title = "Inicio", icon = icon("home"),
    div(
      tagList(
        tags$head(
          tags$script(type="text/javascript", src = "busy.js"),
          tags$link(rel="shortcut icon", href="./rmediclogo.jpg"),
          tags$script(type="text/javascript", "var switchTo5x=true"),
          tags$script(type="text/javascript",'stLight.options({publisher: "675b3562-a081-470a-9fc4-3dd6a712209d", doNotHash: true, doNotCopy: true, hashAddressBar: false})')
        )
      ),
      div(id = ns("home"),
          br(),
          div(
            style = "text-align: center;",
          fluidRow(
            column(3, img(src = "rmediclogo.png", width = "300", height = "300"),
                       br(),
                       tags$a(
                         fa_i("linkedin"), 
                         href = "https://www.linkedin.com/company/r-medic/", 
                         target = "_blank",
                         class = "social-icon", 
                         style="font-size: 4em;"),
          ),
                   column(9, br(), br(),
                          div(tags$head(
                            tags$style(HTML("
                            body, html {
    overflow-x: hidden !important;  /* Evita scroll horizontal en toda la página */
  }
      .carousel-container {
        width: 100%;
        overflow: hidden;
        white-space: nowrap;
      }
      .carousel {
        display: inline-block;
        white-space: nowrap;
        animation: moveSlideshow 15s linear infinite; /* antes 60s */      }
      .carousel img {
        display: inline-block;
        height: 250px;
        margin-right: 20px;
      }
      
      .carousel-container:hover .carousel {
        animation-play-state: paused;
      }

      @keyframes moveSlideshow {
        0% { transform: translateX(0); }
        100% { transform: translateX(-50%); }
      }
    "))
                          ),
                          class = "carousel-container",
                              div(class = "carousel",
                                  # Primer conjunto de imágenes
                                  tags$img(src = "img01_ucc.png", height = "10%"),  # Imagen
                                  tags$img(src = "img02_ucc_salud.png", height = "10%"),  # Imagen
                                  tags$img(src = "img03_reina_2025.png", height = "10%"),  # Imagen,
                                  
                                  
                                  # Segundo conjunto de imágenes (duplicado)
                                  # tags$img(src = "logo_01_unc_master.png", height = "10%"),  # Imagen
                                  # tags$img(src = "logo_02_fcefyn_master.png", height = "10%"),  # Imagen
                                  # tags$img(src = "logo_03_efadoc_master.png", height = "10%"),  # Imagen
                                  # tags$img(src = "logo_04_rscience_master.png", height = "10%"),  # Imagen,
                                  # tags$img(src = "logo_05_UNT_master.png", height = "10%"),  # Imagen
                                  # tags$img(src = "logo_06_CONICET_master.png", height = "10%"),  # Imagen
                                  # tags$img(src = "logo_07_GULICH_master.png", height = "10%"),
                                  # tags$img(src = "logo_08_NASA_master.png", height = "10%"),
                                  # tags$img(src = "logo_09_UTN_master.png", height = "10%"),
                                  # tags$img(src = "logo_10_INTA_master.png", height = "10%"),
                                  # tags$img(src = "logo_11_CONAE_master.png", height = "10%"),
                                  # tags$img(src = "logo_12_YPF_master.png", height = "10%"),
                                  # tags$img(src = "logo_13_GOOGLE_master.png", height = "10%"),
                                  # tags$img(src = "logo_14_OMS_master.png", height = "10%")
                                  
                              )
                          ))
          )
          ),
          # fluidRow(),
          # fluidRow(),
          module_elegant_cards_UI(id = ns("card_card")),
          fluidRow(
            
          )
      
     
      )
    )
  # )
}



module_opt01_home2_SERVER <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    module_elegant_cards_SERVER(id = "card_card")
    
  })
}

