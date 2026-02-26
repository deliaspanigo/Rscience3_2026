library(shiny)
library(ggplot2)
library(bslib)
library(DT)

ui <- page_sidebar(
  title = "Recta Interactiva en Scatterplot",
  sidebar = sidebar(
    width = 300,
    h4("Parámetros de la Recta"),
    verbatimTextOutput("equation"),
    hr(),
    h4("Instrucciones:"),
    tags$ul(
      tags$li("Haz clic en la gráfica para definir dos puntos"),
      tags$li("Haz clic cerca de un punto existente para moverlo"),
      tags$li("Los parámetros se actualizan automáticamente")
    ),
    hr(),
    actionButton("calculate", "Calcular Errores", class = "btn-primary w-100"),
    actionButton("reset", "Reiniciar", class = "btn-outline-secondary w-100 mt-2")
  ),
  
  card(
    full_screen = TRUE,
    card_header("Gráfico Interactivo"),
    card_body(
      plotOutput("scatterPlot", height = "450px", click = "plot_click", 
                 hover = "plot_hover")
    )
  ),
  
  card(
    card_header("Errores de los Puntos Respecto a la Recta"),
    card_body(
      DTOutput("error_table")
    )
  )
)

server <- function(input, output, session) {
  # Generar datos de muestra
  set.seed(123)
  n_points <- 30
  data <- data.frame(
    x = runif(n_points, -5, 5),
    y = runif(n_points, -5, 5)
  )
  
  # Valores reactivos para almacenar los puntos de la línea y sus parámetros
  line_points <- reactiveValues(
    x = numeric(0), 
    y = numeric(0),
    slope = NULL,
    intercept = NULL,
    errors = NULL,
    hover_near_point = FALSE
  )
  
  # Función para actualizar los parámetros de la línea
  updateLineParams <- function() {
    if (length(line_points$x) == 2) {
      line_points$slope <- (line_points$y[2] - line_points$y[1]) / (line_points$x[2] - line_points$x[1])
      line_points$intercept <- line_points$y[1] - line_points$slope * line_points$x[1]
      # Calcular errores
      line_points$errors <- data$y - (line_points$slope * data$x + line_points$intercept)
    }
  }
  
  # Determinar si el ratón está cerca de un punto existente
  observeEvent(input$plot_hover, {
    if (length(line_points$x) > 0) {
      hover_x <- input$plot_hover$x
      hover_y <- input$plot_hover$y
      
      distances <- sqrt((line_points$x - hover_x)^2 + (line_points$y - hover_y)^2)
      min_dist <- min(distances)
      
      line_points$hover_near_point <- min_dist < 0.5
    } else {
      line_points$hover_near_point <- FALSE
    }
  })
  
  # Renderizar el gráfico interactivo
  output$scatterPlot <- renderPlot({
    # Crear el gráfico base
    p <- ggplot(data, aes(x = x, y = y)) +
      geom_point(color = "blue", size = 3, alpha = 0.7) +
      labs(title = "Haz clic para dibujar una recta", x = "X", y = "Y") +
      theme_bw() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        panel.grid.minor = element_line(color = "gray90")
      ) +
      coord_cartesian(xlim = c(-6, 6), ylim = c(-6, 6))
    
    # Cambiar el cursor si estamos cerca de un punto
    if (line_points$hover_near_point) {
      p <- p + labs(title = "Haz clic para mover el punto")
    }
    
    # Agregar los puntos de la línea si existen
    if (length(line_points$x) > 0) {
      line_df <- data.frame(x = line_points$x, y = line_points$y)
      p <- p + geom_point(data = line_df, aes(x = x, y = y), color = "red", size = 4)
      
      # Si tenemos dos puntos, agregar la recta completa
      if (length(line_points$x) == 2) {
        # Ya tenemos los parámetros calculados
        if (is.null(line_points$slope)) {
          updateLineParams()
        }
        
        # Crear datos para la línea extendida
        x_range <- c(-6, 6)
        y_values <- line_points$slope * x_range + line_points$intercept
        line_extended <- data.frame(x = x_range, y = y_values)
        
        # Añadir la línea entre los puntos y la línea extendida
        p <- p +
          geom_line(data = line_df, aes(x = x, y = y), color = "red", size = 1) +
          geom_line(data = line_extended, aes(x = x, y = y), 
                    color = "red", linetype = "dashed", size = 0.8)
      }
    }
    
    return(p)
  })
  
  # Manejar los clics para dibujar/mover la línea
  observeEvent(input$plot_click, {
    x_click <- input$plot_click$x
    y_click <- input$plot_click$y
    
    # Comprobar si el clic está cerca de un punto existente (para moverlo)
    if (length(line_points$x) > 0) {
      distances <- sqrt((line_points$x - x_click)^2 + (line_points$y - y_click)^2)
      closest_point <- which.min(distances)
      
      # Si estamos cerca de un punto existente, moverlo
      if (distances[closest_point] < 0.5) {
        line_points$x[closest_point] <- x_click
        line_points$y[closest_point] <- y_click
        updateLineParams()
        return()
      }
    }
    
    # Si no estamos moviendo un punto, agregar uno nuevo
    if (length(line_points$x) < 2) {
      line_points$x <- c(line_points$x, x_click)
      line_points$y <- c(line_points$y, y_click)
    } else {
      # Si ya tenemos 2 puntos, reemplazar el más cercano
      distances <- sqrt((line_points$x - x_click)^2 + (line_points$y - y_click)^2)
      closest_point <- which.min(distances)
      line_points$x[closest_point] <- x_click
      line_points$y[closest_point] <- y_click
    }
    
    updateLineParams()
  })
  
  # Mostrar la ecuación de la recta
  output$equation <- renderText({
    if (length(line_points$x) == 2 && !is.null(line_points$slope) && !is.null(line_points$intercept)) {
      slope_rounded <- round(line_points$slope, 4)
      intercept_rounded <- round(line_points$intercept, 4)
      
      intercept_sign <- ifelse(intercept_rounded >= 0, "+", "")
      
      paste0("y = ", slope_rounded, "x ", intercept_sign, " ", intercept_rounded)
    } else {
      "Haz clic en dos puntos para dibujar la recta."
    }
  })
  
  # Calcular y mostrar los errores cuando se hace clic en el botón
  observeEvent(input$calculate, {
    req(length(line_points$x) == 2)
    updateLineParams()
  })
  
  # Mostrar tabla de errores
  output$error_table <- renderDT({
    req(length(line_points$x) == 2, !is.null(line_points$errors))
    
    df_errors <- data.frame(
      Punto = 1:nrow(data),
      X = round(data$x, 2),
      Y = round(data$y, 2),
      Y_Predicho = round(line_points$slope * data$x + line_points$intercept, 2),
      Error = round(line_points$errors, 2)
    )
    
    datatable(df_errors, 
              options = list(
                pageLength = 10,
                autoWidth = TRUE,
                dom = 'frtip'
              ),
              rownames = FALSE)
  })
  
  # Botón para reiniciar la recta
  observeEvent(input$reset, {
    line_points$x <- numeric(0)
    line_points$y <- numeric(0)
    line_points$slope <- NULL
    line_points$intercept <- NULL
    line_points$errors <- NULL
  })
}

shinyApp(ui, server)