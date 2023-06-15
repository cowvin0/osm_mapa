# Pare todos os contêineres em execução: docker stop $(docker ps -aq)
# Remova todos os contêineres: docker rm $(docker ps -aq)
# Remova todas as imagens: docker rmi $(docker images -q)
# Limpe o cache do Docker: docker system prune --all --volumes --force
# Compilando imagem: docker build -t osm_mapa .
# Rodando imagem: docker run --name osm_mapa --restart=unless-stopped -p 3838:3838 -d osm_mapa
# Expondo a porta do localhost: ssh -o ServerAliveInterval=60 -R prdm0:80:localhost:3838 serveo.net

FROM eddelbuettel/r2u:20.04

RUN apt-get update && apt-get install -y --no-install-recommends \
    pandoc \
    && rm -rf /var/lib/apt/lists/*

RUN install.r shiny rmarkdown flexdashboard dplyr stringr fontawesome reactable purrr leafem sf leaflet leaflet.extras spsComps bspm mapview

RUN addgroup --system app && adduser --system --ingroup app app

WORKDIR /home/prdm0/Dropbox/GitHub/osm_mapa/

COPY . .

RUN chown app:app -R /home/app

USER app

EXPOSE 3838

# Adiciona o comando ssh no final do CMD
CMD R -e "rmarkdown::run(shiny_args = list(port = 3838, host = '0.0.0.0'));"