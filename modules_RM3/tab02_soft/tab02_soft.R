

tab02_soft_UI <- function(id) {
  ns <- NS(id)
  
  div(
    tags$head(
      tags$style(HTML("
            .nav-tabs {
              width: 100%;
              display: flex;
              justify-content: space-between;
            }
            .nav-tabs > li {
              flex: 1;
              text-align: center;
              font-weight: bold;
              font-family: 'Arial', sans-serif;
            }
            .nav-tabs > li > a {
              font-weight: bold;
              font-size: 18px;
              font-family: 'Arial', sans-serif;
              display: flex;
              height: 50px;
              align-items: center;
              justify-content: center;
              margin: 0; /* Eliminar margen */
              padding: 0; /* Eliminar relleno */
            }
          ")),
      tags$script(HTML("
          $(document).ready(function() {
            var scrollPos = 0;
            $('a[data-toggle=\"tab\"]').on('shown.bs.tab', function(e) {
              window.scrollTo(0, scrollPos);
            });
            $('a[data-toggle=\"tab\"]').on('click', function(e) {
              scrollPos = $(window).scrollTop();
            });
          });
        "))
    ),
    
    titlePanel("R+Medic"),
    
    br(), br(),
    fluidRow(
      column(4, class = "text-center",
             shinyBS::bsButton(ns("showpanel"), "", 
                               type = "toggle", value = TRUE,
                               icon("bars"), style = "primary", size = "large"
             )
      )
    ),
    br(), br(),
    
    sidebarLayout(
      
      #--- RMedic Siderbar
      div(id = "MySidebar",
          
          sidebarPanel(id = ns("Sidebar"), 
             # Data source
             module_pack002_import_s00_general_p01_ui(id = ns("space02_database_00")),
             
             # Data import
             module_pack002_import_s00_general_p02_ui(id = ns("space02_database_00"))
          )
      ),
      
      #--- RMedic Options
      mainPanel(id = ns("Main"),
                uiOutput(ns("RMedicSoft")),
                br(), br(), br(),br(),br(),br(),br()
                
      )
      # End MainPanel ------------------------------------------
    ) 
  )
}



tab02_soft_SERVER <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # # # Show or not show panel button
    observeEvent(input$showpanel, {
      
      if(input$showpanel == TRUE) {
        removeCssClass("Main", "col-sm-12")
        addCssClass("Main", "col-sm-8")
        shinyjs::show(id = "Sidebar")
        shinyjs::enable(id = "Sidebar")
      }
      else {
        removeCssClass("Main", "col-sm-8")
        addCssClass("Main", "col-sm-12")
        shinyjs::hide(id = "Sidebar")
      }
    })
    
    
    
    # 1 - Base -----------------------------------------------------------------
    {
      
      sui_data_source <- module_pack002_import_s00_general_p01_server(id = "space02_database_00")
      output_list_database        <- module_pack002_import_s00_general_p02_server(id = "space02_database_00", sui_data_source)
      
      temporal_file_path <- reactive({output_list_database()$"temporal_file_path"})
      str_import_local <- reactive({output_list_database()$"str_import_local"})
      database <- reactive({output_list_database()$"database"})
      my_name <-  reactive({output_list_database()$"original_file_name"})
      
      # Run Server
      module_pack002_import_s00_general_p03_server(id = "space02_database_00", output_list_database = output_list_database)

    }
    ############################################################################
    
    
    # NOTA: Esto de cada es una zona de control que todavia no esta 
    # desarrollada en su totalidad.
    # pero mas o menos la onda seria ver que la base de datos sea valida.
    # y aca capas iria tambien lo del CIE.
    
    RMedic_general <- reactiveVal(FALSE)
    status_BaseSalida <- reactiveVal(FALSE)
    zocalo_CIE <- reactiveVal(NULL)
    #################################################
    active_tab <- reactiveVal("1")
    observeEvent(input$"custom-tabs", {
      active_tab(input$"custom-tabs")
    })
    #################################################
    
    
    observeEvent(output_list_database()$"database", {
      
      req(output_list_database()$"database")
      RMedic_general(TRUE)
      status_BaseSalida(TRUE)
      active_tab("1")
    })
    
    
    
    # 3 - More tabs: Control- Tablas - Graficos - PH0 - Sobrevida -------------------------
    {
      
    
        # Modules Soft - Tabs mapping
        tab_modules <- list(
          "2" = "module_02_control_GENERAL_SERVER",
          "3" = "module_03_tablas_GENERAL_SERVER",
          "4" = "module_04_graficos_GENERAL_SERVER",
          "5" = "ModuleHoSERVER",
          "6" = "ModuleSobrevidaSERVER"
        )
        
    
        # Run Selected SERVER ------------------------------------------------------
        # All the modules have the same parameters.
        
        observeEvent(output_list_database()$"database", {
          
          req(output_list_database()$"database")
          
          callModule(module = get("module_02_control_GENERAL_SERVER"),
                     id = paste0("menu2"),
                     base = reactive(output_list_database()$"database"),
                     RMedic_general = RMedic_general,
                     status_BaseSalida = status_BaseSalida,
                     zocalo_CIE = zocalo_CIE)
    

        
        
        callModule(module = get("module_03_tablas_GENERAL_SERVER"),
                   id = paste0("menu3"),
                   base = reactive(output_list_database()$"database"),
                   RMedic_general = RMedic_general,
                   status_BaseSalida = status_BaseSalida,
                   zocalo_CIE = zocalo_CIE)
        

        callModule(module = get("module_04_graficos_GENERAL_SERVER"),
                   id = paste0("menu4"),
                   base = reactive(output_list_database()$"database"),
                   RMedic_general = RMedic_general,
                   status_BaseSalida = status_BaseSalida,
                   zocalo_CIE = zocalo_CIE)
        
        callModule(module = get("module_05_ho_GENERAL_SERVER"),
                   id = paste0("menu5"),
                   base = reactive(output_list_database()$"database"),
                   RMedic_general = RMedic_general,
                   status_BaseSalida = status_BaseSalida,
                   zocalo_CIE = zocalo_CIE)
        
        callModule(module = get("module_06_sobrevida_GENERAL_SERVER"),
                   id = paste0("menu6"),
                   base = reactive(output_list_database()$"database"),
                   RMedic_general = RMedic_general,
                   status_BaseSalida = status_BaseSalida,
                   zocalo_CIE = zocalo_CIE)
        
        })
    }
    ############################################################################
    
    
    
    #--- RMedic Software
    output[["RMedicSoft"]] <- renderUI({
      
      tabsetPanel(
        id = ns("custom-tabs"),
        tabPanel(title = "Base de Datos", value = 1,
                 br(),
                 fluidRow(
                   column(1),
                   column(10, module_pack002_import_s00_general_p03_ui(id = ns("space02_database_00"))),
                   column(1)
                 ),
                 br(), br()
        ),
        tabPanel(title = "Control", value = 2,
                 br(),
                 fluidRow(module_02_control_GENERAL_UI(id = ns("menu2"))),
                 br(), br()
        ),
        tabPanel(title = "Tablas", value = 3,
                 br(),
                 fluidRow(module_03_tablas_GENERAL_UI(id = ns("menu3"))),
                 br(), br()
        ),
        tabPanel(title = "Gráficos", value = 4,
                 br(),
                 fluidRow(module_04_graficos_GENERAL_UI(id = ns("menu4"))),
                 br(), br()
        ),
        tabPanel(title = "Pruebas de Hipótesis", value = 5,
                 br(),
                 fluidRow(module_05_ho_GENERAL_UI(id = ns("menu5"))),
                 br(), br()
        ),
        tabPanel(title = "Sobrevida", value = 6,
                 br(),
                 fluidRow(module_06_sobrevida_GENERAL_UI(id = ns("menu6"))),
                 br(), br()
        )
      )
      
    })
    

  })
}

