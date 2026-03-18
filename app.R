# # # 
# # #
# # #

# 1. Definimos la función que abrirá el navegador
onStart <- function() {
  # Agregamos 'cmd /c' al inicio para que reconozca el comando 'start'
  system("cmd /c start msedge --app=http://127.0.0.1:1410 --start-maximized", wait = FALSE)
}

# remotes::install_github("RinteRface/fullPage")
# library(fullPage)
library(shiny)
library(future)
library(promises)
library(bslib)
library(bsicons)
library(shinyWidgets)
library(fontawesome)
library(shiny)
library(bslib)
library("Hmisc")
library("glue")
# Configura el plan de futuro para usar múltiples trabajadores
# plan(multisession)

# # # Loading...
source("global.R")



library(shiny)
library(future)
library(promises)

# Configurar future para usar múltiples núcleos
# plan(multisession, workers = 4)  # Especifica exactamente 4 núcleos
plan(multicore, workers = availableCores())  # Usa todos los núcleos disponibles


# # # User interface - UI
# ui <- shiny::navbarPage(theme = "styles.css",inverse=TRUE,
ui <- shiny::navbarPage(inverse=TRUE,
                        useShinyjs(),
                        tags$head(
                          tags$style(HTML("
                          
  /* Eliminar el guion/subrayado en los enlaces con imágenes */
  a:hover, a:focus {
    text-decoration: none !important;
    outline: none !important;
  }
  
  /* Específicamente para las imágenes dentro de enlaces */
  a img {
    border: none !important;
    text-decoration: none !important;
  }

                          .well {
  
   padding: 12px;
  margin-bottom: 10px;

  background-color:orange;
  
  border-color:black;
  border-radius: 40px;
  border-width: 10px 10px 10px;
  border: 10px solid #d4d4d4;
  
                          }
                          
                          /*  Esto es lo que agregue...*/
.nav-tabs {
  margin-bottom: 10px;

  background-color:orange;
  
  border-color:black;
  border-radius: 40px;
  border-width: 10px 10px 10px;
  border: 10px solid #d4d4d4;
  

}

            .selectize-input, .selectize-dropdown, .select-input, .select-dropdown,
            [type = 'number'], .radio, label, .nav-tabs, table, data.table{
            font-size: 120%;
            }
            
            /* Cambiar el color de fondo de las opciones de SelectInput al pasar el mouse */
.selectize-dropdown-content .option:hover {
  background-color: #68cbd0 !important; /* Color turquesa que coincide con tu tema existente */
  color: white !important; /* Color del texto al hacer hover */
}

/* Personalizar la opción activa/seleccionada del SelectInput */
.selectize-dropdown-content .option.active {
  background-color: #157BA3 !important; /* Color azul más oscuro para la opción seleccionada */
  color: white !important;
}

/* Mejorar la apariencia general del control SelectInput */
.selectize-control.single .selectize-input {
  border-color: #68cbd0 !important; /* Borde del mismo color que usas en otras partes */
  box-shadow: none !important; /* Eliminar la sombra predeterminada */
}

/* Efecto hover en el propio control SelectInput */
.selectize-control.single .selectize-input:hover {
  border-color: #157BA3 !important; /* Borde más oscuro al hacer hover */
}

/* Mejora el estilo del menú desplegable */
.selectize-dropdown {
  border-color: #68cbd0 !important;
  border-radius: 4px !important;
}

/* Hacer que las pestañas del tabset utilicen todo el espacio disponible */
.nav-tabs {
  display: flex !important;
  width: 100% !important;
}

/* Configuración de los elementos li dentro de las pestañas */
.nav-tabs > li {
  flex: 1 !important; /* Distribuye el espacio disponible equitativamente */
  float: none !important; /* Anula el float por defecto */
  display: flex !important; /* Permite centrar el contenido verticalmente */
  align-items: stretch !important; /* Estira el elemento para ocupar la altura completa */
}

/* Control del tamaño de letra y centrado del texto en tabpanels */
.nav-tabs > li > a {
  font-size: 20px !important; 
  padding: 8px 12px !important;
  font-weight: bold !important;
  color: #00336a !important;
  display: flex !important;
  align-items: center !important; /* Centra el contenido verticalmente */
  justify-content: center !important; /* Centra el contenido horizontalmente */
  width: 100% !important;
  min-height: 40px !important; /* Altura mínima para todas las pestañas */
  text-align: center !important;
  height: 100% !important; /* Asegura que ocupe toda la altura disponible */
}

/* Añadir línea separadora entre pestañas del tabset */
.nav-tabs > li:not(:last-child) {
  border-right: 2px solid #ccc !important; /* Línea vertical gris entre pestañas */
  border-bottom: 2px solid #ccc !important; /* Línea inferior gris */

}

/* Mejorar la apariencia de la pestaña activa */
.nav-tabs > li.active > a, 
.nav-tabs > li.active > a:hover, 
.nav-tabs > li.active > a:focus {
  background-color: #f8f8f8 !important;
  color: #00336a !important;
  font-weight: bold !important;
  border-bottom-color: transparent !important;
}

/* Mejora el estilo al pasar el mouse por las pestañas */
.nav-tabs > li > a:hover {
  background-color: #e6e6e6 !important;
  border-color: #ddd #ddd #ddd !important;
  color: #003f80 !important;
}

/* Asegura que el contenido de cada panel tenga suficiente padding */
.tab-pane {
  padding: 15px !important;
}

/* Mejora la transición entre pestañas */
.tab-content > .tab-pane {
  transition: opacity 0.3s ease-in-out !important;
}

  /* Configuración del contenedor de pestañas para permitir múltiples líneas */
      .tabbable .nav-tabs,
      .nav-tabs,
      ul.nav.nav-tabs {
        display: flex !important;
        flex-wrap: wrap !important;
        width: 100% !important;
        margin-bottom: 15px !important;
      }
      
      /* Sobrescribe el comportamiento de flotación por defecto */
      .nav-tabs > li {
        float: none !important;
        display: inline-block !important;
        margin-bottom: 5px !important;
      }

        ")),# En la parte UI de tu aplicación, agrega esto:
                          tags$head(
                            tags$script(HTML("
    $(document).ready(function() {
      // Deshabilita el cursor titilante en selectInput
      $(document).on('mousedown', '.selectize-control', function() {
        $('.selectize-input input').css('caret-color', 'transparent');
      });
      
      // Restaura el cursor solo cuando se está escribiendo
      $(document).on('keydown', '.selectize-input input', function() {
        $(this).css('caret-color', 'auto');
      });
      
      // Vuelve a hacer transparente el cursor al terminar de escribir
      $(document).on('blur', '.selectize-input input', function() {
        $(this).css('caret-color', 'transparent');
      });
    });
  "))
                          ),
                          
                          # Añadimos el script para prevenir la transparencia
                          tags$script(HTML("
          $(document).ready(function() {
            // Prevenir cualquier cambio de opacidad
            var originalCss = $.fn.css;
            $.fn.css = function() {
              if (arguments[0] === 'opacity' && arguments[1] < 1) {
                arguments[1] = 1;
              }
              return originalCss.apply(this, arguments);
            };
            
            // Agregar un indicador de carga alternativo
            $('<div id=\"loading-indicator\" style=\"display:none;position:fixed;top:10px;right:10px;z-index:100000;padding:5px 10px;background-color:rgba(0,0,0,0.7);color:white;border-radius:5px;font-weight:bold;\">Cargando...</div>').appendTo('body');
            
            $(document).on('shiny:busy', function(event) {
              $('#loading-indicator').fadeIn(200);
            });
            
            $(document).on('shiny:idle', function(event) {
              $('#loading-indicator').fadeOut(200);
            });
            
            // Asegurar que no hay cambios de opacidad
            setInterval(function() {
              $('.recalculating, .tab-pane, .tab-content, #RMedicSoft, #opt02_soft-Main, #opt02_soft-Sidebar, .nav-tabs, .shiny-html-output, .shiny-plot-output').css('opacity', 1);
            }, 50);
            
            // Monitorear y corregir cambios de opacidad en elementos específicos
            var observer = new MutationObserver(function(mutations) {
              mutations.forEach(function(mutation) {
                if (mutation.attributeName === 'style') {
                  var el = mutation.target;
                  var opacity = $(el).css('opacity');
                  if (opacity < 1) {
                    $(el).css('opacity', 1);
                  }
                }
              });
            });
            
            // Aplicar el observador a todos los elementos que podrían volverse transparentes
            $('.tab-pane, .tab-content, .nav-tabs').each(function() {
              observer.observe(this, { attributes: true });
            });
          });
        "))
                        ),   
                        
                        title = strong("RMedic 3.3.0"),
                        windowTitle = "RMedic - Medicina y R", 
                        fluid = TRUE, 
                        # header = column(12, ""),
                        footer = column(12,
                                        #, br(), br(),
                                        # Botón de donaciones de PayPal
                                        div( id = "footer",
                                             style = "text-align: center;",
                                             tags$a(href = "https://www.linkedin.com/company/r-medic", target = "_blank", rel = "noopener noreferrer",
                                                    tags$img(src = "rmediclogo.png", style = "cursor: pointer;", height = "50px"))
                                             
                                          #tags$img(src = "rmediclogo.png", height = "50px"),  # Imagen
                                          #tags$img(src = "img01_ucc.png", height = "30px"),  # Imagen
                                          #tags$img(src = "img02_ucc_salud.png", height = "30px"),  # Imagen
                                          #tags$img(src = "img03_reina_2025.png", height = "30px"),  # Imagen
                                          
                                        ),
                                        #br(), 
                                        
                                        div(style = "text-align: center;", HTML('&copy; RMedic (2016-2026)')),
                                        div(style = "text-align: center;", HTML('Marzo 2026'))
                                        
                        ),
                        id = "nav",
                        
                      
                        
                        shiny::tabPanel(title = "Inicio", icon = icon("house"), module_opt01_home2_UI("opt01_home")),
                        shiny::tabPanel(title = "RMedic", module_opt02_soft_UI("opt02_soft")),
                        shiny::tabPanel(title = "Herramientas", source("tabs/HerramientasTab.R", encoding = "UTF-8")$value),
                        shiny::tabPanel(title = "Cita", module_opt04_cita_UI("opt04_cita")),
                        shiny::tabPanel(title = "Contacto", module_opt05_contacto_UI("opt05_contacto")),
                        shiny::tabPanel(title = "Quiénes somos?", module_opt99_who_UI(id = "who99")),
                        shiny::tabPanel(
                          title = "Cronología",
                          tags$head(
                            tags$style(HTML("
      .timeline-container { padding: 20px; position: relative; }
      .timeline-item {
        padding-left: 30px;
        border-left: 3px solid #0d47a1;
        position: relative;
        margin-bottom: 30px;
      }
      .timeline-item::before {
        content: '';
        position: absolute;
        left: -11px;
        top: 0;
        width: 19px;
        height: 19px;
        background: white;
        border: 4px solid #0d47a1;
        border-radius: 50%;
      }
      .version-tag {
        background: #0d47a1;
        color: white;
        padding: 4px 12px;
        border-radius: 50px;
        font-weight: bold;
        font-size: 0.9em;
      }
      .version-date {
        color: #78909c;
        font-family: 'Courier New', monospace;
        font-weight: 600;
        margin-left: 10px;
      }
      .version-current { border-left-color: #2e7d32 !important; }
      .version-current::before { border-color: #2e7d32 !important; }
      .badge-current { background: #2e7d32 !important; }
    "))
                          ),
                          
                          div(class = "timeline-container",
                              h2("Historial de Versiones", style="color:#37474f; font-weight:800; margin-bottom:30px;"),
                              
                              # Versión Actual
                              div(class = "timeline-item version-current",
                                  span(class = "version-tag badge-current", "v3.3.0"),
                                  span(class = "version-date", "2026-03-15"),
                                  h4("Lanzamiento del instalador para Windows", style="margin-top:10px; font-weight:700;"),
                                  tags$ul(
                                    tags$li("Primera versión estable distribuible para entornos Windows."),
                                    tags$li("Nueva arquitectura de lanzamiento profesional (VBS Launcher)."),
                                  )
                              ),
                              
                              
                              div(class = "timeline-item",
                                  span(class = "version-tag", "v3.2.0"),
                                  span(class = "version-date", "2025-11-20"),
                                  h4("Cambios en el diseño", style="margin-top:10px; font-weight:700;"),
                                  tags$ul(
                                    tags$li("Mejoras en la experiencia del usuario.")
                                  )
                              ),
                              
                              div(class = "timeline-item",
                                  span(class = "version-tag", "v3.1.0"),
                                  span(class = "version-date", "2025-06-15"),
                                  h4("Expansión de diseño", style="margin-top:10px; font-weight:700;"),
                                  tags$ul(
                                    tags$li("Mejoras en la experiencia del usuario.")
                                  )
                              ),
                              
                              div(class = "timeline-item",
                                  span(class = "version-tag", "v3.0.0"),
                                  span(class = "version-date", "2025-01-05"),
                                  h4("Consolidación del Software", style="margin-top:10px; font-weight:700;"),
                                  tags$ul(
                                    tags$li("Reestructuración general."),
                                    tags$li("Nuevos detalles de explicativos en los reportes estadísticos.")
                                    
                                  )
                              ),
                              
                              div(class = "timeline-item",
                                  span(class = "version-tag", "v2.0.0"),
                                  span(class = "version-date", "2021-08-12"),
                                  h4("Consolidación del Software", style="margin-top:10px; font-weight:700;"),
                                  tags$ul(
                                    tags$li("Rediseño general."),
                                    tags$li("Nuevas herramientas estadísticas."),
                                    tags$li("Nuevas opciones  gráficas."),
                                    tags$li("Versión disponible solo online."),
                                    
                                    
                                  )
                              ),
                              
                              div(class = "timeline-item",
                                  span(class = "version-tag", "v1.0.0"),
                                  span(class = "version-date", "2016-04-03"),
                                  h4("Nacimiento de RMedic", style="margin-top:10px; font-weight:700;"),
                                  tags$ul(
                                    tags$li("Lanzamiento de la versión original: Estadística para Medicina."),
                                    tags$li("Versión disponible solo online."),
                                    
                                  )
                              )
                          )
                        )
                        #shiny::tabPanel(title = "Donar", module_opt06_donar_UI(id = "donar"))
)




# # # Server - SERVER
server <- function(input, output, session) {
  
  # # # Section 01 - User Location  # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  
  observeEvent(input$donateButton, {
    # URL de PayPal (reemplaza con tu URL de donaciones)
    paypal_url <- "https://www.paypal.com/donate?hosted_button_id=TUVRWUQYUL7B8"
    
    # Redirigir al usuario a la página de PayPal
    js_code <- paste0("window.open('", paypal_url, "', '_blank');")
    runjs(js_code)
  })
  # User location
  user_location <- get_user_location()
  

  if (!is.null(user_location)) {
    # Definir el archivo de registro
    log_file <- "user_locations.log"
    
    # Crear el archivo de registro si no existe
    if (!file.exists(log_file)) {
      file.create(log_file)
    }
    
    # Save info from user on file
    log_entry <- paste(Sys.time(), user_location$ip, user_location$city, user_location$region, user_location$country, sep = ", ")
    write(log_entry, file = log_file, append = TRUE)
    
  }
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  
  
  
  
  # # # Server Modules 01
  
  # 01 - Homepage
  module_opt01_home2_SERVER("opt01_home")
  
  # 02 - RMedic Software
  module_opt02_soft_SERVER("opt02_soft")
  
  # 03 - Tools
  
  # 04 - Cita
  module_opt04_cita_SERVER("opt04_cita")
  
  # 05 - Contacto
  module_opt05_contacto_SERVER("opt05_contacto")
  
  # 99 - who
  module_opt99_who_SERVER(id = "who99")
  
  # 06 - Donar
  module_opt06_donar_SERVER(id = "donar")
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  



  juntos <-   callModule(module = Menu_DistribucionGeneral01_SERVER,
                         id =  "distribucion01.general",
                         carpeta_distribuciones = "009App/002_Distribucion_de_Probabilidades")




  callModule(module = SideBarDistribucionElegida01_SERVER,
             id =  "espacio_elegido01",
             # la_distribucion = "001_Normal")
             la_distribucion = juntos)

  callModule(module = MainPanelDistribucionElegida01_SERVER,
             id =  "espacio_elegido01",
             # la_distribucion = "001_Normal")
             la_distribucion = juntos)

  callModule(module = ServerDistribucionElegida01_SERVER,
             id =  "espacio_elegido01",
             # la_distribucion = "001_Normal")
             la_distribucion = juntos)

  # Distribucion Normal
  callModule(module = Server01_Normal_Server,
             id =  "aver01A",
             # la_distribucion = "001_Normal")
             la_distribucion = juntos)


  #####################################################################################
  juntos2 <-   callModule(module = Menu_DistribucionGeneral02_SERVER,
                          id =  "distribucion02.general",
                          carpeta_distribuciones = "009App/002_Distribucion_de_Probabilidades")




  callModule(module = SideBarDistribucionElegida02_SERVER,
             id =  "espacio_elegido02",
             # la_distribucion = "001_Normal")
             la_distribucion = juntos2)

  callModule(module = MainPanelDistribucionElegida02_SERVER,
             id =  "espacio_elegido02",
             # la_distribucion = "001_Normal")
             la_distribucion = juntos2)

  callModule(module = ServerDistribucionElegida02_SERVER,
             id =  "espacio_elegido02",
             # la_distribucion = "001_Normal")
             la_distribucion = juntos2)

  # Distribucion Normal
  callModule(module = Server01_Normal_Server02,
             id =  "aver02A",
             # la_distribucion = "001_Normal")
             la_distribucion = juntos2)


  # Distribucion t
  callModule(module = Server02_t_Server02,
             id =  "aver02B",
             # la_distribucion = "001_Normal")
             la_distribucion = juntos2)

  # Distribucion Chi
  callModule(module = Server03_chi_Server02,
             id =  "aver02C",
             # la_distribucion = "001_Normal")
             la_distribucion = juntos2)

  # Distribucion F
  callModule(module = Server04_f_Server02,
             id =  "aver02D",
             # la_distribucion = "001_Normal")
             la_distribucion = juntos2)
  
}



# options(shiny.port = 3838)
# options(shiny.host = "0.0.0.0")

# Ejecutar la aplicación Shiny
shinyApp(
  ui = ui, 
  server = server, 
  onStart = onStart  # <--- Esta es la clave
)
#shinyApp(ui = ui, server = server)

