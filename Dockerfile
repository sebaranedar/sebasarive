FROM rocker/shiny:4.3.1

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c( \
    'shiny', \
    'dplyr', \
    'ggplot2', \
    'readxl', \
    'leaflet', \
    'stringi' \
  ), repos = 'https://cloud.r-project.org')"

WORKDIR /app
COPY . /app

EXPOSE 8080

CMD [\"R\", \"-e\", \"shiny::runApp('/app', host='0.0.0.0', port = as.numeric(Sys.getenv('PORT', 8080)))\"]
