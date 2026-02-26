# 1. Limpieza
if ("renv" %in% loadedNamespaces()) unloadNamespace("renv")

# 2. Instalación en la librería base del R-Portable
# Asegúrate de ejecutar esto desde el R que está en .\App\R-Portable
install.packages("renv", lib = .Library)

# 3. Configuración para PORTABILIDAD (Crucial)
Sys.setenv(RENV_CONFIG_SANDBOX_ENABLED = FALSE)
Sys.setenv(RENV_CONFIG_CACHE_ENABLED = FALSE)

# 4. Inicialización
# Entramos a la carpeta del proyecto si no estamos en ella
if (basename(getwd()) != "RMedic3_2026") setwd("RMedic3_2026")

renv::init(bare = TRUE, restart = FALSE)

# 5. Configuración de Snapshot
renv::settings$snapshot.type("explicit")

# 6. Instalación de dependencias
# Esto instalará lo que declare tu archivo DESCRIPTION
renv::install()

renv::restore()

# 7. Snapshot Final
renv::snapshot(prompt = FALSE)

# 8. Prueba manual antes de cerrar
# shiny::runApp(launch.browser = TRUE)