FROM rocker/verse:4.5.2
RUN apt-get update && apt-get install -y  libcurl4-openssl-dev libfontconfig1-dev libfreetype6-dev libfribidi-dev libharfbuzz-dev libicu-dev libjpeg-dev libpng-dev libssl-dev libtiff-dev libwebp-dev libx11-dev libxml2-dev make pandoc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.12.1")'
RUN Rscript -e 'remotes::install_version("scales",upgrade="never", version = "1.4.0")'
RUN Rscript -e 'remotes::install_version("vcfR",upgrade="never", version = "1.16.0")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "3.3.2")'
RUN Rscript -e 'remotes::install_version("spelling",upgrade="never", version = "2.3.2")'
RUN Rscript -e 'remotes::install_version("shinyjs",upgrade="never", version = "2.1.1")'
RUN Rscript -e 'remotes::install_version("shinydisconnect",upgrade="never", version = "0.1.1")'
RUN Rscript -e 'remotes::install_version("shinyWidgets",upgrade="never", version = "0.9.0")'
RUN Rscript -e 'remotes::install_version("bs4Dash",upgrade="never", version = "2.3.5")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.34.0")'
RUN Rscript -e 'remotes::install_version("openxlsx",upgrade="never", version = "4.2.8.1")'
RUN Rscript -e 'remotes::install_version("tidyverse",upgrade="never", version = "2.0.0")'
RUN Rscript -e 'remotes::install_version("viridis",upgrade="never", version = "0.6.5")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.5.1")'
RUN Rscript -e 'remotes::install_version("BIGr",upgrade="never", version = "0.6.2")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone
EXPOSE 80
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');library(familia);familia::run_app()"
