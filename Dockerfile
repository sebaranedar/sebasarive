FROM rocker/shiny:4.4.0

# Carpeta donde vivirá la app
WORKDIR /srv/shiny-server

# Copiar todo el contenido del repo dentro de la imagen
COPY . /srv/shiny-server

# Instalar paquetes de R necesarios para la app
RUN R -e "options(repos = 'https://cloud.r-project.org'); install.packages(c('shiny','dplyr','ggplot2','readxl','leaflet','stringi'))"

# Puerto que usa Shiny
EXPOSE 3838

# Comando para arrancar la app en el puerto que Render indique
CMD [\"R\", \"-e\", \"shiny::runApp('.', host='0.0.0.0', port = as.numeric(Sys.getenv('PORT', 3838)))\"]
