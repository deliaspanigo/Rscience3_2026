install.packages("shiny2docker")


library(shiny2docker)

# Generate a Dockerfile in the current directory
shiny2docker(path = ".")

##########

shiny2docker(
  path = ".",
  lockfile = "./renv.lock",
  output = "./Dockerfile2"
)
