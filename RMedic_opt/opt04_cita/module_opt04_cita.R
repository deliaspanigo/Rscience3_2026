module_opt04_cita_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$style(HTML("
        .citation-container { background: #ffffff; border-radius: 15px; overflow: hidden; }
        .citation-header { background: #f8f9fa; border-bottom: 2px solid #eef2f6; padding: 25px; }
        
        /* Caja de Materiales y Métodos */
        .methodology-box { 
          background: #f0f7ff; 
          border-left: 5px solid #0d47a1; 
          padding: 20px; 
          border-radius: 0 10px 10px 0;
          position: relative;
          font-size: 1.1em;
          color: #1a237e;
        }

        /* Cajas de Cita Estándar */
        .cite-box {
          background: #fdfdfd;
          border: 1px solid #e0e6ed;
          padding: 20px 50px 20px 20px;
          border-radius: 8px;
          position: relative;
          margin-top: 15px;
          color: #37474f;
          line-height: 1.5;
        }

        /* Caja específica para BibTeX (Estilo Código) */
        .bibtex-box {
          background: #282c34;
          color: #abb2bf;
          padding: 15px;
          border-radius: 8px;
          position: relative;
          margin-top: 15px;
          font-family: 'Consolas', 'Monaco', monospace;
          font-size: 0.9em;
        }

        /* Botón de copiar invisible/flotante */
        .copy-icon-btn {
          position: absolute;
          top: 12px;
          right: 12px;
          background: transparent;
          border: none;
          cursor: pointer;
          font-size: 1.2em;
          transition: all 0.2s;
        }
        .copy-icon-btn:hover { transform: scale(1.2); }
        .btn-light-bg { color: #0d47a1; }
        .btn-dark-bg { color: #61afef; }

        /* Ajustes de Tabs */
        .nav-pills .nav-link.active { background-color: #0d47a1 !important; }
        .nav-link { color: #546e7a; font-weight: 600; }
        
        .download-badge {
          background: #e3f2fd;
          color: #0d47a1;
          padding: 12px 25px;
          border-radius: 50px;
          font-weight: 700;
          display: inline-flex;
          align-items: center;
          transition: all 0.3s;
          text-decoration: none !important;
        }
        .download-badge:hover { background: #0d47a1; color: white !important; }
      "))
    ),
    
    div(
      class = "container-fluid", style = "padding: 30px;",
      div(
        class = "citation-container shadow-lg",
        
        # Header
        div(class = "citation-header d-flex align-items-center",
            img(src = "rmediclogo.jpg", width = 80, height = 80, class = "rounded-circle shadow-sm mr-4"),
            div(
              h2("Citación de RMedic", style="color:#0d47a1; font-weight:800; margin:0;"),
              p("Asegura el rigor científico de tus publicaciones científicas", style="color:#607d8b; margin:0;")
            )
        ),
        
        div(class = "p-5",
            # Sección 1
            h4(icon("microscope"), "1. En la sección de Materiales y Métodos", style="color:#37474f; font-weight:700; margin-bottom:15px;"),
            p("Utilice el siguiente texto para describir el procesamiento estadístico:"),
            
            div(class = "methodology-box",
                span("Se utilizó el software estadístico RMedic versión 3.2.5 para todas las tablas, gráficos y análisis estadísticos."),
                tags$button(
                  class = "copy-icon-btn btn-light-bg",
                  onclick = "navigator.clipboard.writeText('Se utilizó el software estadístico RMedic versión 3.2.4 para todas las tablas, gráficos y análisis estadísticos.'); this.style.color='#2e7d32'; setTimeout(()=>this.style.color='#0d47a1', 1000);",
                  icon("copy")
                )
            ),
            
            br(), br(),
            
            # Sección 2
            h4(icon("book"), "2. Referencia Bibliográfica", style="color:#37474f; font-weight:700; margin-bottom:15px;"),
            p("Seleccione el formato de cita bibliográfica:"),
            
            tabsetPanel(
              type = "pills",
              tabPanel("APA 7ma", 
                       div(class = "cite-box",
                           span("Mangeaud A, Elías Panigo DH. (2018). R-Medic. Un programa de análisis estadísticos sencillo e intuitivo. Revista Methodo 3(1): 18-22."),
                           tags$button(class="copy-icon-btn btn-light-bg", onclick="navigator.clipboard.writeText('Mangeaud A, Elías Panigo DH. (2018). R-Medic. Un programa de análisis estadísticos sencillo e intuitivo. Revista Methodo 3(1): 18-22.'); this.style.color='#2e7d32'; setTimeout(()=>this.style.color='#0d47a1', 1000);", icon("copy"))
                       )
              ),
              tabPanel("Vancouver", 
                       div(class = "cite-box",
                           span("Mangeaud A, Elías Panigo DH. R-Medic. Un programa de análisis estadísticos sencillo e intuitivo. Revista Methodo. 2018; 3(1): 18-22."),
                           tags$button(class="copy-icon-btn btn-light-bg", onclick="navigator.clipboard.writeText('Mangeaud A, Elías Panigo DH. R-Medic. Un programa de análisis estadísticos sencillo e intuitivo. Revista Methodo. 2018; 3(1): 18-22.'); this.style.color='#2e7d32'; setTimeout(()=>this.style.color='#0d47a1', 1000);", icon("copy"))
                       )
              ),
              tabPanel("BibTeX", 
                       div(class = "bibtex-box",
                           tags$pre(style="color: #98c379; background: transparent; border: none; padding:0; margin:0;",
                                    "@article{mangeaud2018rmedic,\n  title={R-Medic. Un programa de análisis estadísticos sencillo e intuitivo},\n  author={Mangeaud, Arnaldo and Panigo, David Elías},\n  journal={Revista Methodo},\n  volume={3},\n  number={1},\n  pages={18--22},\n  year={2018}\n}"),
                           tags$button(class="copy-icon-btn btn-dark-bg", 
                                       onclick="navigator.clipboard.writeText('@article{mangeaud2018rmedic, title={R-Medic. Un programa de análisis estadísticos sencillo e intuitivo}, author={Mangeaud, Arnaldo and Panigo, David Elías}, journal={Revista Methodo}, volume={3}, number={1}, pages={18--22}, year={2018}}'); this.style.color='#98c379'; setTimeout(()=>this.style.color='#61afef', 1000);", 
                                       icon("copy"))
                       )
              )
            ),
            
            hr(style="margin-top:40px;"),
            
            # Footer de descarga
            div(class = "text-center mt-5",
                h5("¿Desea adjuntar el artículo original?", style="color:#607d8b; margin-bottom:15px;"),
                a(class = "download-badge", 
                  href = "RMedic_Cita.pdf", 
                  target = "_blank",
                  icon("file-pdf"), "DESCARGAR ARTÍCULO (PDF)"
                )
            )
        )
      )
    )
  )
}

module_opt04_cita_SERVER <- function(id) {
  moduleServer(id, function(input, output, session) {
    # El copiado se maneja por JS en el UI para máxima velocidad
  })
}