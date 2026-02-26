library(shiny)
library(bslib)

# UI principal
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  mod_principal_SELECTOR_ui("app")
)

# Server principal
server <- function(input, output, session) {
  the_database <- reactive(mtcars)
  mod_principal_SELECTOR_server("app", my_database = the_database())
}

# Crear la aplicación Shiny
shinyApp(ui = ui, server = server)