# 1. Agrega todos los cambios al área de preparación
git add .

# 2. Crea el commit con un mensaje descriptivo
git commit -m "Upgrade to v.3.3.0: version for Windws."

# 3. Sube los cambios a la rama principal (usualmente 'main' o 'master')
git push origin main

# 4. Crea una etiqueta anotada para la versión 3.2.6
git tag -a v.3.3.0 -m "Version for Windws"

# 5. Sube la etiqueta a GitHub
git push origin v.3.3.0