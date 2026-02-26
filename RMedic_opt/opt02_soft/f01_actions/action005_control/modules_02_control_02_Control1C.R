



## Segmento del UI
modules_02_control_02_Control1C_UI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("SeccionControl1C"))
  
  
}




## Segmento del server
modules_02_control_02_Control1C_SERVER <- function(input, output, session, 
                             base, 
                             batalla_naval,
                             decimales) {
  
  
  
  # Aplicar modificadores usando do.call
  apply_modifiers <- function(content, modifiers) {
    if (length(modifiers) > 0) if(sum(modifiers == "") == 0){
      for (modifier in modifiers) {
        # Verificar que la función exista
        #if (exists(modifier, where = "package:shiny")) {
        
        selected_func <- get(modifier) #, envir = asNamespace("shiny"))
        content <- do.call(selected_func, list(content))  # Llama a la función del modificador
        #}
      }
    }
    return(content)
  }
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  # Caso 1: 1Q
  casoRMedic <- reactive({
    
    if(is.null(batalla_naval())) return(NULL)
    if(is.null(batalla_naval()[[4]])) return(NULL)
    if(length(batalla_naval()[[4]]) == 0) return(NULL)
    if(batalla_naval()[[4]] == '') return(NULL)
    casoRMedic <- batalla_naval()[[4]]
    #casoRMedic <- as.numeric(as.character(as.vector(batalla_naval()[[4]])))
    casoRMedic
    
  })
  
  # Todas las tablas 1C
  output_list <- reactive({
    
    if(is.null(casoRMedic())) return(NULL)
    if(casoRMedic() != 2) return(NULL)
    
    
    # salida <-  control_1c_RMedic(base = base(), columna = batalla_naval()[[1]])
    #salida <-  control_1c_full(dataframe = base(), selected_col = batalla_naval()[[1]])
    
    salida <- bk_control_1c_FULL(database = base(), selected_col = batalla_naval()[[1]], selected_lang = "ESP")
    
    #print(salida)
    # Return Exitoso
    return(salida)
    
    
  })  
  
  #-----------------------------------------------------------------------------
  
  observe({
    lapply(names(output_list()), function(section_name) {
    
      lapply(names(output_list()[[section_name]]), function(obj_name) {
      
        new_special_name <- paste0(section_name, "_", obj_name)
        
        selected_render  <- output_list()[[section_name]][[obj_name]][["render"]]
        selected_content <- output_list()[[section_name]][[obj_name]][["content"]] 
        selected_modifiers <- output_list()[[section_name]][[obj_name]][["modifiers"]]

        # Renderizar texto
        if (selected_render == "renderText_title") {
          
          output[[new_special_name]] <- renderUI({
            my_content <- apply_modifiers(selected_content, selected_modifiers)
            my_content
          })
          
        } 
        
        # Renderizar texto
        if (selected_render == "renderText") {
          
          output[[new_special_name]] <- renderUI({
            my_content <- apply_modifiers(selected_content, selected_modifiers)
            my_content
          })
          
        } 
        
        if (selected_render == "renderText_list") {
          
          output[[new_special_name]] <- renderUI({
            vector_text <- selected_content
            vector_text <- paste0(vector_text, "<br>")
            vector_text <- HTML(vector_text)
            vector_text <- apply_modifiers(vector_text, selected_modifiers)

            vector_text
          })
          
        }
        
        if (selected_render == "renderTable") {
          output[[new_special_name]] <- renderTable({
            my_content <- apply_modifiers(selected_content, selected_modifiers)
            my_content
          }, align = "c")
          
        }  
        
        
      })

    })
  })
  
  
  output$SeccionControl1C <- renderUI({
    
    
    sections_ui <- lapply(names(output_list()), function(section_name) {
      section_ui <- tagList()
      
      lapply(names(output_list()[[section_name]]), function(obj_name) {
        
        new_special_name <- paste0(section_name, "_", obj_name)
        
        selected_render   <- output_list()[[section_name]][[obj_name]][["render"]]
        selected_after_br <- output_list()[[section_name]][[obj_name]][["after_br"]]
        
        if(selected_render == "renderText_title") {
          section_ui <- tagAppendChild(section_ui, uiOutput(ns(new_special_name)))
        } 
        
        # Texto (dependiendo del tipo de render)
        if(selected_render == "renderText") {
          section_ui <- tagAppendChild(section_ui, uiOutput(ns(new_special_name)))
        } 
         
        if(selected_render == "renderText_list") {
          section_ui <- tagAppendChild(section_ui, htmlOutput(ns(new_special_name)))
        }
          
        # Agregar el contenido para la tabla si existe
        if(selected_render == "renderTable") {
          section_ui <- tagAppendChild(section_ui, tableOutput(ns(new_special_name)))
        }

        # Agregar el contenido para la tabla si existe
        if(selected_after_br > 0) {
          for (k in 1:selected_after_br) section_ui <- tagAppendChild(section_ui, br())
        }
        
        
        # Devolver la UI para la sección actual
        section_ui
    })
    })
  })
  
  
  # # Sector01
  # ## title01_fixed - bk_control_1c_s01_title01_fixed()
  # output$s01_title01_fixed <- renderText({
  #   Reactive_control_1c_RMedic()[[1]][[1]][["content"]]
  # })
  # 
  # ### text01_fixed - bk_control_1c_s01_text01_fixed()
  # output$s01_text01_fixed <- renderText({
  #   vector_aver <- Reactive_control_1c_RMedic()[[1]][[2]][["content"]]
  #   vector_aver <- paste0(vector_aver, "<br>")
  #   vector_aver
  # })
  # 
  # #-----------------------------------------------------------------------------
  # 
  # # Sector02
  # ## title01_fixed    - bk_control_1c_s02_title01_fixed()
  # output$s02_title01_fixed <- renderText({
  #   Reactive_control_1c_RMedic()[[2]][[1]][["content"]]
  # })
  # 
  # ### text01_fixed    - bk_control_1c_s02_text01_fixed()
  # output$s02_text01_fixed <- renderText({
  #   Reactive_control_1c_RMedic()[[2]][[2]][["content"]]
  # })
  # 
  # ### df01            - bk_control_1c_s02_df01()
  # output$s02_df01 <- renderTable({
  #   Reactive_control_1c_RMedic()[[2]][[3]][["content"]]
  #   #mtcars
  # })
  # 
  # #-----------------------------------------------------------------------------
  # # Sector03
  # ## title03_fixed
  # output$s03_title01_fixed <- renderText({
  #   Reactive_control_1c_RMedic()[[3]][[1]][["content"]]
  # })
  # 
  # ### text01_fixed    - bk_control_1c_s03_text01_fixed()
  # output$s03_text01_fixed <- renderText({
  #   Reactive_control_1c_RMedic()[[3]][[2]][["content"]]
  # })
  # 
  # ### df01            - bk_control_1c_s03_df01()
  # output$s03_df01 <- renderTable({
  #   Reactive_control_1c_RMedic()[[3]][[3]][["content"]]
  # })
  # 
  # #-----------------------------------------------------------------------------

  # 
  # output$SeccionControl1C <- renderUI({
  #   
  #   # Especificaciones de cumplimiento
  #   if(is.null(casoRMedic())) return(NULL)
  #   if(casoRMedic() != 2) return(NULL)
  #   
  #   
  #   
  # 
  #   # Si es el caso 2, seguimos!
  #   div(
  #     #htmlOutput(ns("Intro")),
  #     fluidRow(
  #       column(12,
  #              h2_mod(textOutput(ns("s01_title01_fixed"))),
  #              h4(htmlOutput(ns("s01_text01_fixed")))
  #              
  #              #h4("- Los valores mínimo y máximo deben tener sentido en el marco de la experiencia."), 
  #              #h4("- Corroborar la presencia o no de celdas vacías.")
  #       )
  #     ),
  #     br(),
  #     br(),
  #     fluidRow(
  #       column(12,
  #              h2_mod(textOutput(ns("s02_title01_fixed"))),
  #              h4(textOutput(ns("s02_text01_fixed"))),
  #              tableOutput(ns("s02_df01"))
  #              )
  #     ),
  #     br(),
  #     br(),
  #     fluidRow(
  #       column(12,
  #              h2_mod(textOutput(ns("s03_title01_fixed"))),
  #              h4(textOutput(ns("s03_text01_fixed"))),
  #              tableOutput(ns("s03_df01"))
  #       )
  #     )
  #     
  #     
  #   )
  # })
  # 
  # 
  
  
  
  
}


