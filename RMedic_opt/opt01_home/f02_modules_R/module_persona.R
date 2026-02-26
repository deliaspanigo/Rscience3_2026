# Módulo para la UI de una persona
module_persona_UI <- function(id, nombre, titulo, linkedin, correo, imagen, sobre_mi, habilidades) {
  ns <- NS(id)  # Namespace para evitar conflictos de IDs
  
  tagList(
    h3(nombre),
    img(src = imagen, height = 200, width = 200),
    p("Nombre: ", nombre),
    p("Título: ", titulo),
    p("LinkedIn:", a("Perfil de LinkedIn", href = linkedin)),
    p("Correo: ", correo),
    h4("Sobre Mí"),
    p(sobre_mi),
    h4("Habilidades"),
    p(paste("-", habilidades, collapse = "\n"))
  )
}

# Módulo para la lógica del servidor (en este caso no hay lógica adicional)
module_persona_SERVER <- function(id) {
  moduleServer(id, function(input, output, session) {
    # No se necesita lógica adicional en este caso
  })
}
