FROM rocker/shiny:4.4.0

# Carpeta donde vivirá la app
WORKDIR /srv/shiny-server

# Copiar archivos de la app al contenedor
COPY . /srv/shiny-server/

# Instalar paquetes necesarios
RUN R -e "options(repos = c(CRAN='https://cloud.r-project.org')); \
          install.packages(c('shiny','dplyr','ggplot2','readxl','leaflet','stringi'))"

# Exponer puerto
EXPOSE 3838

# Comando para lanzar Shiny
CMD ["R", "-e", "shiny::runApp('.', host='0.0.0.0', port = as.numeric(Sys.getenv('PORT', 3838)))"]
