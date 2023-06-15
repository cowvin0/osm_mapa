library(mapview)
library(dplyr)

dados <- read.csv(file = "~/Downloads/natal.csv")
df <- dados 

mapa2 <- function(df = NULL, imovel = NULL){
  renomeando_tipos <- function(x)
    stringr::str_split(x, "_")[[1L]][1L]
  
  if(is.null(df))
    return(mapview()@map)

  tipos <- purrr::map_chr(.x = df$tipo, .f = renomeando_tipos)
  
  dados <- 
    df |> 
    sf::st_as_sf(coords=c("longitude", "latitude"), crs = 4326) |> 
    dplyr::select(-z_lat, -z_lon, -url, -bairro_completo) 
  
  dados$tipo <- tipos
  
  if(!is.null(imovel)){
    dados <- 
      dados |> 
      dplyr::filter(tipo == imovel, alpha = 0)
    
    mapa <- dados |> 
      mapview::mapview(al)
  } else {
    mapa <- dados |> 
      mapview::mapview(legend = F, alpha = 0)
  }
  
  return(mapa@map)
}

mapa2()