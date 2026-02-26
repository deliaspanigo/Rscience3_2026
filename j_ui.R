

# rm(list = ls())

# fluidPage(
#   useShinyjs(),
#   theme = "styles.css", 
  
  shiny::navbarPage(theme = "styles.css",inverse=TRUE,
             useShinyjs(),
             tags$head(tags$style(HTML("
        .selectize-input, .selectize-dropdown, .select-input, .select-dropdown,
        [type = 'number'], .radio, label, .nav-tabs, table, data.table{
          font-size: 120%;
        }
        "))),   
             title = strong("I need RMEDIC here!"),
             windowTitle = "RMedic - Medicina y R", 
             fluid = TRUE, 
             header = column(12, ""),
             footer = column(12,
                             div(id = "footer",
                                 a("Consultoria Bioestadística de la Salud"), br(),
                                 "Contacto: ", a("d.eliaspanigo@gmail.com"),
                                 br(),
                                 HTML('&copy; David Elías Panigo (2016)')
                                 )
                             ),
             #footer = includeHTML("tools/footer.html"),
             # footer =  tags$iframe(src = "tools/footer.html"),
             id = "nav",

  
             # Tab Inicio (HOME)
             # Tab Inicio (HOME)
             shiny::tabPanel(title = "Inicio", icon = icon("house"), source("tabs/homeTab.R", encoding = "UTF-8")$value),
             shiny::tabPanel(title = "RMedic", source("tabs/RMedicTab.R", encoding = "UTF-8")$value),
             shiny::tabPanel(title = "Herramientas", source("tabs/HerramientasTab.R", encoding = "UTF-8")$value)
)
