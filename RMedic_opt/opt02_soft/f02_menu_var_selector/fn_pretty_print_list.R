pretty_print_list <- function(lst, indent = 0, prefix = "") {
  # Función recursiva para imprimir listas con sangría
  indent_space <- paste(rep("  ", indent), collapse = "")
  
  if (is.list(lst) && !is.null(names(lst))) {
    # Es una lista con nombres
    for (name in names(lst)) {
      value <- lst[[name]]
      
      if (is.list(value) && length(value) > 0) {
        # Si el valor es una lista, imprimir el nombre y recorrer recursivamente
        cat(indent_space, prefix, name, ":\n", sep = "")
        pretty_print_list(value, indent + 1)
      } else {
        # Si el valor no es una lista o está vacío, imprimir directamente
        if (length(value) > 1) {
          # Si es un vector con múltiples elementos
          cat(indent_space, prefix, name, ": [", paste(value, collapse = ", "), "]\n", sep = "")
        } else {
          # Si es un valor único
          cat(indent_space, prefix, name, ": ", value, "\n", sep = "")
        }
      }
    }
  } else if (is.list(lst) && is.null(names(lst))) {
    # Es una lista sin nombres (como un array)
    for (i in seq_along(lst)) {
      value <- lst[[i]]
      
      if (is.list(value) && length(value) > 0) {
        # Si el valor es una lista, imprimir el índice y recorrer recursivamente
        cat(indent_space, prefix, "[[", i, "]]:\n", sep = "")
        pretty_print_list(value, indent + 1)
      } else {
        # Si el valor no es una lista o está vacío, imprimir directamente
        if (length(value) > 1) {
          # Si es un vector con múltiples elementos
          cat(indent_space, prefix, "[[", i, "]]: [", paste(value, collapse = ", "), "]\n", sep = "")
        } else {
          # Si es un valor único
          cat(indent_space, prefix, "[[", i, "]]: ", value, "\n", sep = "")
        }
      }
    }
  } else {
    # No es una lista, imprimir directamente
    cat(indent_space, prefix, lst, "\n", sep = "")
  }
}

