

module_opt02_soft_UI <- function(id) {
  ns <- NS(id)
  
  div(
    
    # titlePanel("R+Medic"),
    # div(style = "text-align: center;",
    #     div(style = "display: inline-block; margin: 0;",
    #         img(src = "png/img_02_UCC.png", width = "150px", style = "opacity: 1;")),
    #     div(style = "display: inline-block; margin: 0;",
    #         img(src = "png/img_01_RMedic.png", width = "150px", style = "opacity: 1;")),
    #     div(style = "display: inline-block; margin: 0;",
    #         img(src = "png/img_03_ENIAX.png", width = "150px", style = "opacity: 1;"))
    # ),
    # br(), br(),
    fluidRow(
      column(2, class = "text-center",
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
             width = 2,
             # Data source
             module_act001_s999_01_RM_source_options_ui(id = ns("act001_s99_01")),
             module_act001_s999_02_RM_settings_ui(id = ns("act001_s99_02"))
             
          )
      ),
      
      #--- RMedic Options
      mainPanel(id = ns("Main"),
                #module_act001_s999_03_RM_import_ui(id = "act001_s99_03"),
                shinycssloaders::withSpinner(uiOutput(ns("RMedicSoft"))),
                br(), br(), br(),br(),br(),br(),br(),
                br(), br(), br(), br(), br(),br()
                
                
      )
      # End MainPanel ------------------------------------------
    ) 
  )
}



module_opt02_soft_SERVER <- function(id) {
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
    
    
    
    # 1 - Import Base -----------------------------------------------------------------
    {
      
      # 1.1 - Eleccion de la fuente de datos.
      sui_data_source <- module_act001_s999_01_RM_source_options_server(id = "act001_s99_01")
      
      # 1.2 - Settings from data source
      list_sui_settings <- module_act001_s999_02_RM_settings_server(id     = "act001_s99_02", sui_data_source)      

      
      
      # 1.3 - Imported data
      output_list_database  <- module_act001_s999_03_RM_import_server(id   = "act001_s99_03", list_sui_settings) 
      
    }
    ############################################################################
    
    
    # 2 - Show Base -----------------------------------------------------------------
    {
      
    module_action002_show_database_server(id = "action002", output_list_database = output_list_database)  |> bindEvent(output_list_database(), ignoreInit = TRUE, ignoreNULL = TRUE)
    
      
      
      
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



    # 5 - Control- 
    {
      
      # 3.1 - Control - Seleccion de Variables
      #mod_principal_SELECTOR_server(id = "action005", my_database = reactive(output_list_database()$"database")) 
      list_principal_selector_01_CONTROL <- mod_principal_SELECTOR_server(id = "action005_control_01", 
                                                                          my_database = reactive(output_list_database()$"database"), 
                                                                          show_dev = reactive(FALSE))  # |> bindEvent(output_list_database()$"database", ignoreInit = TRUE, ignoreNULL = TRUE)
      # observe(print(list_principal_selector_01_CONTROL()))
      # 3.2 - Control - OutputServer
      module_action005_s999_control_GENERAL_server(id = "action005_control_02", list_principal_selector_01_CONTROL = list_principal_selector_01_CONTROL)  #|> bindEvent(list_principal_selector_01_CONTROL(), ignoreInit = TRUE, ignoreNULL = TRUE)
      
      observeEvent(input$showpanel_control, {
        
        if(input$showpanel_control == TRUE) {
          shinyjs::show(id = "miDivEspecial_control")
          shinyjs::enable(id = "miDivEspecial_control")
        }
        else {
          shinyjs::hide(id = "miDivEspecial_control")
        }
      })
      
      
    }
    ############################################################################
    
    # 6 - Tablas- 
    {
      # 3.1 - Tablas - Seleccion de Variables
      list_principal_selector_01_TABLAS <- mod_principal_SELECTOR_server("action006_tablas_01", my_database = reactive(output_list_database()$"database"), show_dev = reactive(FALSE)) #  |> bindEvent(output_list_database(), ignoreInit = TRUE)
       
      # # 3.2 - Tablas - OutputServer
      module_action006_s999_tablas_GENERAL_server(id = "action006_tablas_02", list_principal_selector_01_TABLAS = list_principal_selector_01_TABLAS)   #|> bindEvent(list_principal_selector_01_TABLAS(), ignoreInit = TRUE)
      # 

      observeEvent(input$showpanel_tablas, {

        if(input$showpanel_tablas == TRUE) {
          shinyjs::show(id = "miDivEspecial_tablas")
          shinyjs::enable(id = "miDivEspecial_tablas")
        }
        else {
          shinyjs::hide(id = "miDivEspecial_tablas")
        }
      })
      
      
    }
    ############################################################################

    # 7 - Graficos- 
    {
      
      # 7.1 - Graficos - Seleccion de Variables
      list_principal_selector_01_GRAFICOS <- mod_principal_SELECTOR_server("action007_graficos_01", my_database = reactive(output_list_database()$"database"), show_dev = reactive(FALSE)) # |> bindEvent(output_list_database(), ignoreInit = TRUE)
      
      # # 7.2 - Graficos - OutputServer
      module_action007_s999_graficos_GENERAL_server(id = "action007_graficos_02", list_principal_selector_01_GRAFICOS = list_principal_selector_01_GRAFICOS)  |> bindEvent(list_principal_selector_01_GRAFICOS(), ignoreInit = TRUE)
      # 
      
      observeEvent(input$showpanel_graficos, {
        
        if(input$showpanel_graficos == TRUE) {
          shinyjs::show(id = "miDivEspecial_graficos")
          shinyjs::enable(id = "miDivEspecial_graficos")
        }
        else {
          shinyjs::hide(id = "miDivEspecial_graficos")
        }
      })
      
      
    }
    ############################################################################

    # 8 - Pruebas- 
    {
      
      # 8.1 - Pruebas - Seleccion de Variables
      list_principal_selector_01_PRUEBAS <- mod_SELECTOR_PRUEBAS_server(id = "action008_pruebas_01", 
                                                                        my_database = reactive(output_list_database()$"database"), 
                                                                        show_dev = reactive(FALSE)) # |> bindEvent(output_list_database(), ignoreInit = TRUE)

      # # 8.2 - Pruebas - OutputServer
      module_action008_s999_pruebas_GENERAL_server(id = "action008_pruebas_02", list_principal_selector_01_PRUEBAS = list_principal_selector_01_PRUEBAS)   |> bindEvent(list_principal_selector_01_PRUEBAS(), ignoreInit = TRUE)
      # 
      
      observeEvent(input$showpanel_pruebas, {
        
        if(input$showpanel_pruebas == TRUE) {
          shinyjs::show(id = "miDivEspecial_pruebas")
          shinyjs::enable(id = "miDivEspecial_pruebas")
        }
        else {
          shinyjs::hide(id = "miDivEspecial_pruebas")
        }
      })
      
      
    }
    ############################################################################
    
    # 9 - Sobrevida
    {
      callModule(module = get("module_action009_sobrevida_03_ALL_server"),
                 id = paste0("action009_sobrevida_01"),
                 base = reactive(output_list_database()$"database"),
                 RMedic_general = reactive(TRUE), #RMedic_general,
                 status_BaseSalida = reactive(TRUE), #status_BaseSalida,
                 zocalo_CIE = reactive(NULL))
      
      observeEvent(input$showpanel_sobrevida, {
        
        if(input$showpanel_sobrevida == TRUE) {
          shinyjs::show(id = "miDivEspecial_sobrevida")
          shinyjs::enable(id = "miDivEspecial_sobrevida")
        }
        else {
          shinyjs::hide(id = "miDivEspecial_sobrevida")
        }
      })
      
    }
    
    
    ######
    # bslib::nav_panel(
    #   title = tab$title,
    #   value = as.character(tab$value),
    #   icon = bsicons::bs_icon(tab$icon),
    #   shinycssloaders::withSpinner(
    #     tab_content,
    #     type = 8,  # Tipo de spinner (valores del 1 al 8)
    #     color = "#0275D8",  # Color del spinner
    #     size = 1  # Tamaño del spinner
    #   )
    # )
    ########################
    #--- RMedic Software
    output[["RMedicSoft"]] <- renderUI({
     
      
      # Contenedor para el conjunto de pestañas
      container_div <- div(
        style = "overflow-x: hidden; width: 100%;",
        bslib::navset_tab(
          id = ns("custom-tabs"),
          selected = "1",
          
          # Pestaña 1: Base de Datos - Definida explícitamente
          bslib::nav_panel(
            title = "Base de Datos",
            value = "1",
            # icon = bsicons::bs_icon("database"),
            div(
              module_action002_show_database_ui(id = ns("action002"))
              
            )
          ),
          
          # Pestaña 2: Control - Definida explícitamente
          bslib::nav_panel(
            title = "Control",
            value = "2",
            # icon = bsicons::bs_icon("database"),
            div(
              fluidRow(
                column(1),
                column(10,
                  h2_mod("Menú para Control"),
                  div(
                    id = ns("miDivEspecial_control"),
                    mod_principal_SELECTOR_ui(id = ns("action005_control_01"))
                    ),
                    hr_mod(),
                  shinycssloaders::withSpinner(
                    ui_element = module_action005_s999_control_GENERAL_ui(id = ns("action005_control_02")),
                    type = 8,  # Tipo de spinner (valores del 1 al 8)
                    color = "#0275D8",  # Color del spinner
                    size = 1  # Tamaño del spinner
                  )
                    
                  ),
                column(1, class = "text-center", br(),
                       shinyBS::bsButton(ns("showpanel_control"), "", 
                                         type = "toggle", value = TRUE,
                                         icon("bars"), style = "primary", size = "large"
                       )
                )
              ),
              br(), br(), br(), br(), br(),br(),
              br(), br(), br(), br(), br(),br()
              # No hay contenido extra para esta pestaña
            )
          ),
          # Pestaña 3: Tablas - Definida explícitamente
          bslib::nav_panel(
            title = "Tablas",
            value = "3",
            # icon = bsicons::bs_icon("database"),
            div(
              fluidRow(
                column(1),
                column(10,
                       h2_mod("Menú para Tablas"),
                       div(
                         id = ns("miDivEspecial_tablas"),
                         mod_principal_SELECTOR_ui(id = ns("action006_tablas_01"))
                       ),
                       hr_mod(),
                       shinycssloaders::withSpinner(
                         ui_element = module_action006_s999_tablas_GENERAL_ui(id = ns("action006_tablas_02")),
                         type = 8,  # Tipo de spinner (valores del 1 al 8)
                         color = "#0275D8",  # Color del spinner
                         size = 1  # Tamaño del spinner
                       )
                ),
                column(1, class = "text-center", br(),
                       shinyBS::bsButton(ns("showpanel_tablas"), "", 
                                         type = "toggle", value = TRUE,
                                         icon("bars"), style = "primary", size = "large"
                       )
                )
              ),
              br(), br(), br(), br(), br(),br(),
              br(), br(), br(), br(), br(),br()
              # No hay contenido extra para esta pestaña
            )
          ),
          # Pestaña 3: Tablas - Definida explícitamente
          bslib::nav_panel(
            title = "Gráficos",
            value = "4",
            # icon = bsicons::bs_icon("database"),
            div(
              fluidRow(
                column(1),
                column(10,
                       h2_mod("Menú para Gráficos"),
                       div(
                         id = ns("miDivEspecial_graficos"),
                         mod_principal_SELECTOR_ui(id = ns("action007_graficos_01"))
                       ),
                       hr_mod(),
                       shinycssloaders::withSpinner(
                         ui_element =  module_action007_s999_graficos_GENERAL_ui(id = ns("action007_graficos_02")),
                         type = 8,  # Tipo de spinner (valores del 1 al 8)
                         color = "#0275D8",  # Color del spinner
                         size = 1  # Tamaño del spinner
                       )
                      
                ),
                column(1, class = "text-center", br(),
                       shinyBS::bsButton(ns("showpanel_graficos"), "", 
                                         type = "toggle", value = TRUE,
                                         icon("bars"), style = "primary", size = "large"
                       )
                )
              ),
              br(), br(), br(), br(), br(),br(),
              br(), br(), br(), br(), br(),br()
              # No hay contenido extra para esta pestaña
            )
          ),
          # Pestaña 3: Tablas - Definida explícitamente
          bslib::nav_panel(
            title = "Pruebas de Hipótesis",
            value = "5",
            # icon = bsicons::bs_icon("database"),
            div(
              fluidRow(
                column(1),
                column(10,
                       h2_mod("Menú para Pruebas de Hipótesis"),
                       div(
                         id = ns("miDivEspecial_pruebas"),
                         mod_SELECTOR_PRUEBAS_ui(id = ns("action008_pruebas_01"))
                       ),
                       hr_mod(),
                       shinycssloaders::withSpinner(
                         ui_element = module_action008_s999_pruebas_GENERAL_ui(id = ns("action008_pruebas_02")),
                         type = 8,  # Tipo de spinner (valores del 1 al 8)
                         color = "#0275D8",  # Color del spinner
                         size = 1  # Tamaño del spinner
                       )
                ),
                column(1, class = "text-center", br(),
                       shinyBS::bsButton(ns("showpanel_pruebas"), "", 
                                         type = "toggle", value = TRUE,
                                         icon("bars"), style = "primary", size = "large"
                       )
                )
              ),
              br(), br(), br(), br(), br(),br(),
              br(), br(), br(), br(), br(),br()
              # No hay contenido extra para esta pestaña
            )
          ),
          bslib::nav_panel(
            title = "Sobrevida",
            value = "6",
            # icon = bsicons::bs_icon("database"),
            div(
              fluidRow(
                column(1),
                column(10,
                       h2_mod("Menú para Sobrevida"),
                       div(
                         id = ns("miDivEspecial_sobrevida"),
                         module_action009_sobrevida_03_ALL_ui(id = ns("action009_sobrevida_01"))
                       ),
                       hr_mod()#,
                       #module_action008_s999_pruebas_GENERAL_ui(id = ns("action009_sobrevida_02"))
                ),
                column(1, class = "text-center", br(),
                       shinyBS::bsButton(ns("showpanel_sobrevida"), "", 
                                         type = "toggle", value = TRUE,
                                         icon("bars"), style = "primary", size = "large"
                       )
                )
              ),
              br(), br(), br(), br(), br(),br(),
              br(), br(), br(), br(), br(),br()
              # No hay contenido extra para esta pestaña
            )
          )
          
          
        )
      )
      
          
          
   
        
          custom_css <- tags$head(
            tags$style(HTML(""))
          )
          
    
      
      # Devolvemos todo junto
      tagList(
        custom_css,
        container_div
      )
    })

    
    
    
    

  })
}

