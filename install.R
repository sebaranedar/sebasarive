# install.R
# Script de instalación de paquetes para Render.com

options(repos = c(CRAN = "https://cloud.r-project.org"))

paquetes <- c(
  "shiny",
  "dplyr",
  "ggplot2",
  "readxl",
  "leaflet",
  "stringi"
)

instalar_si_faltan <- function(pkgs) {
  instalados <- rownames(installed.packages())
  for (p in pkgs) {
    if (!p %in% instalados) {
      message("Instalando paquete: ", p)
      install.packages(p)
    } else {
      message("Ya está instalado: ", p)
    }
  }
}

instalar_si_faltan(paquetes)
