FROM rocker/tidyverse:4.2.0

RUN apt-get update && apt-get install --no-install-recommends -y libjpeg-dev libcairo2-dev libspectre-dev librsvg2-dev libpoppler-glib-dev libv8-dev && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('XML','png','jpeg','base64enc','V8'))"

RUN R -e "devtools::install_github(\"hirenj/grImport2\")"

RUN R -e "devtools::install_github(\"sjp/grConvert\")"