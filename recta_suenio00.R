library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Dibujar una Recta en un Scatterplot"),
  sidebarLayout(
    sidebarPanel(
      actionButton("calculate", "Calcular Ecuación y Errores"),
      verbatimTextOutput("equation"),
      verbatimTextOutput("errors")
    ),
    mainPanel(
      plotOutput("scatterPlot", click = "plot_click")
    )
  )
)

server <- function(input, output) {
  # Data de ejemplo
  set.seed(123)
  data <- data.frame(x = rnorm(20), y = rnorm(20))
  
  # Variables reactives para la recta
  line_points <- reactiveValues(x = numeric(0), y = numeric(0))
  
  output$scatterPlot <- renderPlot({
    ggplot(data, aes(x, y)) +
      geom_point() +
      geom_abline(slope = ifelse(length(line_points$x) < 2, NA, (line_points$y[2] - line_points$y[1]) / (line_points$x[2] - line_points$x[1])), color = "red") +
      coord_cartesian(xlim = c(min(data$x) - 1, max(data$x) + 1), ylim = c(min(data$y) - 1, max(data$y) + 1)) +
      ggtitle("Click to draw a line")
  })
  
  observeEvent(input$plot_click, {
    # Agregar puntos de la línea
    line_points$x <- c(line_points$x, input$plot_click$x)
    line_points$y <- c(line_points$y, input$plot_click$y)
    
    # Limitar a los primeros 2 puntos
    if (length(line_points$x) > 2) {
      line_points$x <- line_points$x[1:2]
      line_points$y <- line_points$y[1:2]
    }
  })
  
  output$equation <- renderPrint({
    req(length(line_points$x) == 2)
    slope <- (line_points$y[2] - line_points$y[1]) / (line_points$x[2] - line_points$x[1])
    intercept <- line_points$y[1] - slope * line_points$x[1]
    paste("Ecuación de la recta: y =", round(slope, 2), "* x +", round(intercept, 2))
  })
  
  observeEvent(input$calculate, {
    req(length(line_points$x) == 2)
    
    slope <- (line_points$y[2] - line_points$y[1]) / (line_points$x[2] - line_points$x[1])
    intercept <- line_points$y[1] - slope * line_points$x[1]
    
    errors <- data$y - (slope * data$x + intercept)
    output$errors <- renderPrint(errors)
  })
}

shinyApp(ui = ui, server = server)