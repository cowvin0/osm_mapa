library(readr)
library(glue)
library(dplyr)
library(purrr)
library(stringr)
library(cluster)
library(geosphere)
library(pbmcapply)

rm(list=ls())

#####
bairros <- function(uf = "pb", cidade = "João Pessoa") {
  uf <- tolower(uf)
  estado <- case_when(
    uf == "ac" ~ "acre",
    uf == "al" ~ "alagoas",
    uf == "ap" ~ "amapa",
    uf == "am" ~ "amazonas",
    uf == "ba" ~ "bahia",
    uf == "ce" ~ "ceara",
    uf == "df" ~ "distrito-federal",
    uf == "es" ~ "espirito-santo",
    uf == "go" ~ "goias",
    uf == "ma" ~ "maranhao",
    uf == "mt" ~ "mato-grosso",
    uf == "ms" ~ "mato-grosso-do-sul",
    uf == "mg" ~ "minas-gerais",
    uf == "pa" ~ "para",
    uf == "pb" ~ "paraiba",
    uf == "pr" ~ "parana",
    uf == "pe" ~ "pernambuco",
    uf == "pi" ~ "piaui",
    uf == "rj" ~ "rio-de-janeiro",
    uf == "rn" ~ "rio-grande-do-norte",
    uf == "rs" ~ "rio-grande-do-sul",
    uf == "ro" ~ "rondonia",
    uf == "rr" ~ "roraima",
    uf == "sp" ~ "sao-paulo",
    uf == "se" ~ "sergipe",
    uf == "sc" ~ "santa-catarina",
    uf == "to" ~ "tocantins"
  )
  
  estado <- estado |>
    stringr::str_squish() |>
    abjutils::rm_accent() |>
    stringr::str_replace_all(" ", "-") |>
    tolower()
  
  cidade <- cidade |>
    stringr::str_squish() |>
    abjutils::rm_accent() |>
    stringr::str_replace_all(" ", "-") |>
    tolower()
  
  bairros_api <-
    glue::glue("https://cepbrasil.org/{estado}/{cidade}/") |>
    xml2::read_html() |>
    xml2::xml_find_all("(//div[@class='col-md-6 coluna'])//a[@href]") |>
    xml2::xml_text() |>
    stringr::str_squish() |>
    tolower() |>
    abjutils::rm_accent() |>
    stringr::str_replace_all(" ", "-") |>
    suppressWarnings()
  
  path_file <-
    paste0(fs::path("R", "scraping", "correcoes_bairros", cidade),
           "/correcoes.csv")
  
  if (!fs::file_exists(path_file))
    return(bairros_api)
  
  complemento <-
    paste0(fs::path("R", "scraping", "correcoes_bairros", cidade),
           "/correcoes.csv") |>
    readr::read_csv(show_col_types = FALSE)
  
  complemento <- complemento[[1L]]
  
  
  return(c(bairros_api, complemento))
}

correcao_bairros <- function(uf, cidade) {
  
  remove_parentesis <- function(texto) {
    gsub("-\\(.*\\)", "", texto)
  }
  
  vec_bairros <- bairros(uf = uf,  cidade = cidade)
  
  nomes <-
    c(
      "santo",
      "santa",
      "jardim",
      "sao",
      "vila",
      "parque",
      "residencial",
      "cidade",
      "loteamento",
      "habitacional",
      "conjunto",
      "balneario",
      "chacaras"
    )
  
  step <- function(bairro, x) {
    abreviacao <- dplyr::case_when(
      x == "santo" ~ "sto",
      x == "santa" ~ "sta",
      x == "jardim" ~ "jd",
      x == "sao" ~ "s",
      x == "vila" ~ "vl",
      x == "parque" ~ "pq",
      x == "residencial" ~ "resid",
      x == "cidade" ~ "cid",
      x == "loteamento" ~ "lot",
      x == "habitacional" ~ "hab",
      x == "conjunto" ~ "conj",
      x == "balneario" ~ "bal",
      x == "chacaras" ~ "ch"
    )
    
    stringr::str_replace_all(bairro, x, abreviacao)
  }
  
  vec_bairros <- reduce(nomes, step, .init = vec_bairros)
  
  # Algumas correcoes:
  
  if(uf == "rj" && cidade == "marica"){
    vec_complemento <- 
      c("portal-dos-cajueiros",
        "bananal-ponta-negra",
        "cassorotiba-inoa",
        "sta-paula-inoa",
        "s-jose-de-imbassai",
        "spar-inoa",
        "jd-atlantico-leste-itaipuacu",
        "jd-atlantico",
        "ponte-negra",
        "praia-de-itaipuacu-itaipuacu",
        "morada-das-aguias-itaipuacu",
        "rincao-mimoso-itaipuacu",
        "jd-atlantico-leste-itaipuacu",
        "itaipuacu",
        "condado"
      )
    vec_bairros <- c(vec_bairros, vec_complemento)
  }
  
  remove_parentesis(vec_bairros)
}

add_coordenadas <-
  function(df,
           uf = "pb",
           cidade = "joao-pessoa",
           complemento) {
    retornarAntesVirgula <- function(texto) {
      partes <- strsplit(texto, ",")[[1]]
      antesVirgula <- trimws(partes[1])
      return(antesVirgula)
    }
    
    df$endereco <-
      purrr::map_vec(.x = df$endereco, .f = retornarAntesVirgula)
    
    df <- df |>
      dplyr::mutate(
        end_completo = glue::glue("{df$endereco}, {complemento}"),
        .before = bairro
      ) |>
      tidygeocoder::geocode(end_completo,
                            lat = latitude,
                            long = longitude,
                            method = "arcgis") |>
      dplyr::select(-end_completo)
    
    df <- df |>
      dplyr::mutate(
        z_lat = (latitude - mean(latitude)) / sd(latitude),
        z_lon = (longitude - mean(longitude)) / sd(longitude)
      ) 
    
    q_zlat <- df$z_lat |> quantile(probs = c(0.02, 0.98))
    q_zlon <- df$z_lon |> quantile(probs = c(0.02, 0.98))
    
    df |> 
      dplyr::filter(
        z_lat > q_zlat[1], z_lat < q_zlat[2],
        z_lon > q_zlon[1], z_lat < q_zlon[2]
      )
  }

# Aracaju
dados <- vroom::vroom("~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais/aracaju_dados_29_05_2023_14h_47min_32sec.csv") 
dados <- dados |> 
  dplyr::select(-latitude, -longitude)

dados <- add_coordenadas(df = dados, uf = "SE", cidade = "aracaju", complemento = "Aracaju, Sergipe, Brasil")
readr::write_csv(dados, "~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais_ultimas_coordenadas/aracaju.csv")

# Belem
dados <- vroom::vroom("~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais/belem_dados_01_06_2023_16h_43min_40sec.csv")
dados <- dados |> 
  dplyr::select(-latitude, -longitude)

dados <- add_coordenadas(df = dados, uf = "PA", cidade = "belem", complemento = "Belem, Para, Brasil")
readr::write_csv(dados, "~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais_ultimas_coordenadas/belem.csv")

# Belo Horizonte
dados <- vroom::vroom("~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais/belo_horizonte_dados_01_06_2023_05h_20min_37sec.csv") 
dados <- dados |> 
  dplyr::select(-latitude, -longitude)

dados <- add_coordenadas(df = dados, uf = "MG", cidade = "belo-horizonte", complemento = "Belo Horizonte, Minas Gerais, Brasil")
readr::write_csv(dados, "~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais_ultimas_coordenadas/belo_horizonte.csv")

# Boa Vista
dados <- vroom::vroom("~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais/boa_vista_dados_04_06_2023_02h_28min_11sec.csv") 
dados <- dados |> 
  dplyr::select(-latitude, -longitude)

dados <- add_coordenadas(df = dados, uf = "RR", cidade = "boa-vista", complemento = "Boa Vista, Roraima, Brasil")
readr::write_csv(dados, "~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais_ultimas_coordenadas/boa_vista.csv")

# Brasilia
dados <- vroom::vroom("~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais/brasilia_dados_25_05_2023_16h_21min_10sec.csv") 
dados <- dados |> 
  dplyr::select(-latitude, -longitude)

dados <- add_coordenadas(df = dados, uf = "DF", cidade = "brasilia", complemento = "Brasilia, Distrito Federal, Brasil")
readr::write_csv(dados, "~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais_ultimas_coordenadas/brasilia.csv")

# Cabedelo
dados <- vroom::vroom("~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais/cabedelo_dados_18_05_2023_22h_12min_02sec.csv") 
dados <- dados |> 
  dplyr::select(-latitude, -longitude)

dados <- add_coordenadas(df = dados, uf = "PB", cidade = "cabedelo", complemento = "Cabedelo, Paraiba, Brasil")
readr::write_csv(dados, "~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais_ultimas_coordenadas/cabedelo.csv")

# Campo Grande
dados <- vroom::vroom("~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais/campo_grande_dados_26_05_2023_19h_33min_09sec.csv") 
dados <- dados |> 
  dplyr::select(-latitude, -longitude)

dados <- add_coordenadas(df = dados, uf = "MS", cidade = "campo-grande", complemento = "Campo Grande, Mato Grosso do Sul, Brasil")
readr::write_csv(dados, "~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais_ultimas_coordenadas/campo_grande.csv")

# Curitiba
dados <- vroom::vroom("~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais/curitiba_dados_30_05_2023_00h_45min_46sec.csv") 
dados <- dados |> 
  dplyr::select(-latitude, -longitude)

dados <- add_coordenadas(df = dados, uf = "PR", cidade = "curitiba", complemento = "Curitiba, Parana, Brasil")
readr::write_csv(dados, "~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais_ultimas_coordenadas/curitiba.csv")

# Florianopolis
dados <- vroom::vroom("~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais/florianopolis_dados_31_05_2023_17h_43min_22sec.csv") 
dados <- dados |> 
  dplyr::select(-latitude, -longitude)

dados <- add_coordenadas(df = dados, uf = "SC", cidade = "florianopolis", complemento = "Florianopolis, Santa Catarina, Brasil")
readr::write_csv(dados, "~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais_ultimas_coordenadas/florianopolis.csv")

# Fortaleza
dados <- vroom::vroom("~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais/fortaleza_dados_27_05_2023_03h_33min_48sec.csv") 
dados <- dados |> 
  dplyr::select(-latitude, -longitude)

dados <- add_coordenadas(df = dados, uf = "CE", cidade = "fortaleza", complemento = "Fortaleza, Ceara, Brasil")
readr::write_csv(dados, "~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais_ultimas_coordenadas/fortaleza.csv")

# Goiania
dados <- vroom::vroom("~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais/goiana_dados_04_06_2023_20h_07min_17sec.csv") 
dados <- dados |> 
  dplyr::select(-latitude, -longitude)

dados <- add_coordenadas(df = dados, uf = "GO", cidade = "goiania", complemento = "Goiania, Goias, Brasil")
readr::write_csv(dados, "~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais_ultimas_coordenadas/goiania.csv")

# Jaboatao dos Guararapes
dados <- vroom::vroom("~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais/jaboatao_dos_guararapes_dados_19_05_2023_05h_02min_20sec.csv") 
dados <- dados |> 
  dplyr::select(-latitude, -longitude)

dados <- add_coordenadas(df = dados, uf = "PE", cidade = "jaboatao-dos-guararapes", complemento = "Jaboatao dos Guararapes, Pernambuco, Brasil")
readr::write_csv(dados, "~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais_ultimas_coordenadas/jaboatao_dos_guararapes.csv")

# Joao Pessoa
dados <- vroom::vroom("~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais/joao_pessoa_dados_18_05_2023_20h_59min_26sec.csv") 
dados <- dados |> 
  dplyr::select(-latitude, -longitude)

dados <- add_coordenadas(df = dados, uf = "PB", cidade = "joao-pessoa", complemento = "Joao Pessoa, Paraiba, Brasil")
readr::write_csv(dados, "~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais_ultimas_coordenadas/joao_pessoa.csv")

# Macapa

dados <- vroom::vroom("~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais/macapa_dados_04_06_2023_23h_19min_40sec.csv") 
dados <- dados |> 
  dplyr::select(-latitude, -longitude)

dados <- add_coordenadas(df = dados, uf = "AP", cidade = "macapa", complemento = "Macapa, Amapa, Brasil")
readr::write_csv(dados, "~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais_ultimas_coordenadas/macapa.csv")

# Maceio

dados <- vroom::vroom("~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais/maceio_dados_31_05_2023_12h_51min_01sec.csv") 
dados <- dados |> 
  dplyr::select(-latitude, -longitude)

dados <- add_coordenadas(df = dados, uf = "AL", cidade = "maceio", complemento = "Maceio, Alagoas, Brasil")
readr::write_csv(dados, "~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais_ultimas_coordenadas/maceio.csv")

# Manaus

dados <- vroom::vroom("~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais/manaus_dados_31_05_2023_20h_02min_55sec.csv") 
dados <- dados |> 
  dplyr::select(-latitude, -longitude)

dados <- add_coordenadas(df = dados, uf = "AM", cidade = "manaus", complemento = "Manaus, Amazonas, Brasil")
readr::write_csv(dados, "~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais_ultimas_coordenadas/manaus.csv")

# Natal

dados <- vroom::vroom("~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais/natal_dados_24_05_2023_10h_35min_01sec.csv") 
dados <- dados |> 
  dplyr::select(-latitude, -longitude)

dados <- add_coordenadas(df = dados, uf = "RN", cidade = "natal", complemento = "Natal, Rio Grande do Norte, Brasil")
readr::write_csv(dados, "~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais_ultimas_coordenadas/natal.csv")

# Palmas

dados <- vroom::vroom("~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais/palmas_dados_26_05_2023_12h_42min_23sec.csv") 
dados <- dados |> 
  dplyr::select(-latitude, -longitude)

dados <- add_coordenadas(df = dados, uf = "TO", cidade = "palmas", complemento = "Palmas, Tocantins, Brasil")
readr::write_csv(dados, "~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais_ultimas_coordenadas/palmas.csv")

# Porto Alegre

dados <- vroom::vroom("~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais/porto_alegre_dados_04_06_2023_08h_33min_27sec.csv") 
dados <- dados |> 
  dplyr::select(-latitude, -longitude)

dados <- add_coordenadas(df = dados, uf = "RS", cidade = "porto-alegre", complemento = "Porto Alegre, Rio Grande do Sul, Brasil")
readr::write_csv(dados, "~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais_ultimas_coordenadas/porto_alegre.csv")

# Porto Velho

dados <- vroom::vroom("~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais/porto_velho_dados_01_06_2023_18h_50min_32sec.csv") 
dados <- dados |> 
  dplyr::select(-latitude, -longitude)

dados <- add_coordenadas(df = dados, uf = "RO", cidade = "porto-velho", complemento = "Porto Velho, Rondonia, Brasil")
readr::write_csv(dados, "~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais_ultimas_coordenadas/porto_velho.csv")

# Recife

dados <- vroom::vroom("~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais/recife_dados_18_05_2023_05h_29min_58sec.csv") 
dados <- dados |> 
  dplyr::select(-latitude, -longitude)

dados <- add_coordenadas(df = dados, uf = "PE", cidade = "recife", complemento = "Recife, Pernambuco, Brasil")
readr::write_csv(dados, "~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais_ultimas_coordenadas/recife.csv")

# Rio Branco

dados <- vroom::vroom("~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais/rio_branco_dados_04_06_2023_22h_22min_37sec.csv") 
dados <- dados |> 
  dplyr::select(-latitude, -longitude)

dados <- add_coordenadas(df = dados, uf = "AC", cidade = "rio-branco", complemento = "Rio Branco, Acre, Brasil")
readr::write_csv(dados, "~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais_ultimas_coordenadas/rio_branco.csv")

# Salvador

dados <- vroom::vroom("~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais/salvador_dados_29_05_2023_20h_37min_45sec.csv") 
dados <- dados |> 
  dplyr::select(-latitude, -longitude)

dados <- add_coordenadas(df = dados, uf = "BA", cidade = "salvador", complemento = "Salvador, Bahia, Brasil")
readr::write_csv(dados, "~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais_ultimas_coordenadas/salvador.csv")

# Sao Luis

dados <- vroom::vroom("~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais/sao_luis_dados_19_05_2023_03h_37min_34sec.csv") 
dados <- dados |> 
  dplyr::select(-latitude, -longitude)

dados <- add_coordenadas(df = dados, uf = "MA", cidade = "sao-luis", complemento = "Sao Luis, Maranhao, Brasil")
readr::write_csv(dados, "~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais_ultimas_coordenadas/sao_luis.csv")

# Sao Paulo

dados <- vroom::vroom("~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais/sao_paulo_dados_04_06_2023_01h_24min_21sec.csv") 
dados <- dados |> 
  dplyr::select(-latitude, -longitude)

dados <- add_coordenadas(df = dados, uf = "SP", cidade = "sao-paulo", complemento = "Sao Paulo, Sao Paulo, Brasil")
readr::write_csv(dados, "~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais_ultimas_coordenadas/sao_paulo.csv")

# Teresina

dados <- vroom::vroom("~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais/teresina_dados_27_05_2023_07h_04min_15sec.csv") 
dados <- dados |> 
  dplyr::select(-latitude, -longitude)

dados <- add_coordenadas(df = dados, uf = "PI", cidade = "teresina", complemento = "Teresina, Piaui, Brasil")
readr::write_csv(dados, "~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais_ultimas_coordenadas/teresina.csv")

# Vitoria

dados <- vroom::vroom("~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais/vitoria_dados_26_05_2023_11h_17min_48sec.csv") 
dados <- dados |> 
  dplyr::select(-latitude, -longitude)

dados <- add_coordenadas(df = dados, uf = "ES", cidade = "vitoria", complemento = "Vitoria, Espirito Santo, Brasil")
readr::write_csv(dados, "~/Dropbox/consultorias/tecgeo/compartilhar_dados/dados_tecgeo_capitais_ultimas_coordenadas/vitoria.csv")

# dados <- 
#   readr::read_csv(file = "~/Downloads/joao_pessoa.csv", show_col_types = FALSE) |> 
#   dplyr::select(-latitude, -longitude, -z_lat, -z_lon, -bairro_completo)
# 
# dados <- dados |> 
#   add_coordenadas(complemento = "Joao Pessoa, Paraiba, Brasil")
# 
# save(dados, file = "~/Downloads/joao_pessoa_arcgis.RData")
#####

# map2_parallel <- function(.x, .y, .f, ...) {
#   pbmclapply(seq_along(.x), function(i) {
#     .f(.x[[i]], .y[[i]], ...)
#   }, mc.cores = parallel::detectCores())
# }
# 
# try_map2_parallel <- function(...){
#   tryCatch(
#     expr = map2_parallel(...),
#     error = function(e) -1
#   )
# }
# 
# centroide <- function(df, complemento = "joao pessoa, paraiba, brasil"){
#   b <- 
#     df$bairro |> 
#     stringr::str_replace_all(pattern = "_", " ") |> unique()
#   
#   step <- function(x) {
#     tidygeocoder::geo(glue::glue("{x}, {complemento}"), method = "arcgis")
#   }
#   
#   c <- pbmcapply::pbmclapply(X = b, FUN = step, mc.cores = parallel::detectCores()) |> 
#     unlist() |> 
#     matrix(byrow = TRUE, ncol = 3) |> as.data.frame()
#   colnames(c) <- c("bairro", "latitude", "longitude")
#   c
# }
# 
# distancias <- function(df){
#   
#   centroide_bairro <- centroide(df = df)
#   
#   b <- 
#     df$bairro |> 
#     stringr::str_replace_all(pattern = "_", " ") |> unique()
#   
#   step <- function(lat, lon, b){
#     geosphere::distVincentySphere(c(lat, lon), b)
#   }
#   
#   try_step <- function(...){
#     tryCatch(
#       expr = step(...),
#       error = function(e) -1
#     )
#   }
#   
#   f <- function(lat_b, lon_b){
#      id <- purrr::map2(
#         .x = as.numeric(centroide_bairro$latitude),
#         .y = as.numeric(centroide_bairro$longitude),
#         .f = \(x, y) purrr::map2(.x = x, .y = y, .f = try_step, b = c(lat_b, lon_b))
#       ) |> 
#       purrr::flatten() |>
#       unlist() |> 
#       which.min()
#      
#      r <- centroide_bairro[id,]$bairro
#      
#      b_provavel <- strsplit(r, ",")[[1L]][1L] |> 
#        stringr::str_replace_all(" ", "_")
# 
#      lat_provavel = as.double(centroide_bairro[id, "latitude"])
#      lon_provavel = as.double(centroide_bairro[id, "longitude"])
#      
#      list(
#        bairro_provavel = b_provavel,
#        lat_provavel = lat_provavel,
#        lon_provavel = lon_provavel
#      )
#   }
#   
#   r <- try_map2_parallel(.x = df$latitude, .y = df$longitude, .f = f) |> 
#     dplyr::bind_rows()
#   
#   r <- cbind(df, r)
#   
#   vicenty_sphere <- function(i)
#     geosphere::distVincentySphere(c(r$latitude[i], r$longitude[i]), c(r$lat_provavel[i], r$lon_provavel[i]))
#   
#   distancia <- 
#     pbmcapply::pbmclapply(
#       X = 1L:nrow(r),
#       FUN = vicenty_sphere,
#       mc.cores = parallel::detectCores()
#     ) |> unlist() 
#   
#   r$distancia <- distancia
#   r
# }
# 
# load("~/Downloads/joao_pessoa_arcgis.RData")
# 
# d <- distancias(dados)
# 
# suspeitos <- d |> dplyr::arrange(desc(distancia))

