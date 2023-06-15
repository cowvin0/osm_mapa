FROM eddelbuettel/r2u:20.04

RUN apt-get update && apt-get install -y --no-install-recommends \
    pandoc \
    && rm -rf /var/lib/apt/lists/*

RUN install.r shiny rmarkdown flexdashboard dplyr fontawesome reactable leaflet.extras spsComps bspm

RUN addgroup --system app && adduser --system --ingroup app app

WORKDIR /home/prdm0/Dropbox/GitHub/osm_mapa/

COPY . .

RUN chown app:app -R /home/app

USER app

EXPOSE 3838

# Adiciona o comando ssh no final do CMD
CMD R -e "rmarkdown::run(shiny_args = list(port = 3838, host = '0.0.0.0'));"