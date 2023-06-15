library(mapview)
library(dplyr)

dados <- read.csv(file = "~/Downloads/natal.csv")
df <- dados 

mapa <- function(df = NULL, imovel = NULL){
  renomeando_tipos <- function(x)
    stringr::str_split(x, "_")[[1L]][1L]
  
  if(is.null(df)){
    m <- data.frame(nome = "TecGeo", latitude = -7.1214628, longitude = -34.8655664) |> 
      sf::st_as_sf(coords=c("longitude", "latitude"), crs = 4326) |> 
      mapview::mapview()
    
    
    m <- leafem::addLogo(m, "https://tecnologiageo.com.br/static/images/logo_tecgeo2.png",
                    position = "bottomleft",
                    offset.x = 50,
                    offset.y = 50,
                    width = 140,
                    height = 100)
    return(m)
  }
    
  tipos <- purrr::map_chr(.x = df$tipo, .f = renomeando_tipos)
  
  dados <- 
    df |> 
    sf::st_as_sf(coords=c("longitude", "latitude"), crs = 4326) |> 
    dplyr::select(-z_lat, -z_lon, -url, -bairro_completo) 
  
  dados$tipo <- tipos
  
  if(!is.null(imovel)){
    dados <- 
      dados |> 
      dplyr::filter(tipo == imovel)
    
    mapa <- dados |> 
      mapview::mapview()
  } else {
    mapa <- 
      dados |> 
      dplyr::filter(tipo == imovel) |> 
      mapview::mapview(legend = F, alpha = 0)
  }
  
  mapa <- leafem::addLogo(mapa, "https://tecnologiageo.com.br/static/images/logo_tecgeo2.png",
                       position = "bottomleft",
                       offset.x = 50,
                       offset.y = 50,
                       width = 160,
                       height = 100)
  return(mapa)
}

mapa(df = dados, imovel = "flat")
