library(cepR)

token <- "33efdb7230265c63c301ced1bc16370b"

cep <- function(
    endereco = "joao-pessoa,rua-francisco-cavalcan, joao pessoa, paraiba, brasil"){
  
  padronizando_string <- function(x){
    stringr::str_split(string = abjutils::rm_accent(tolower(x)),
                       pattern = "\\s+")[[1L]] |> paste0(collapse = "-")
  }

  url <- glue::glue(
    "http://cep.la/{padronizando_string(x = endereco)}"
  ) |> httr::GET()
  
  url <- xml2::read_html(url)

  rvest::html_table(url)[[1L]][1,][[1L]] |> 
    stringr::str_replace("-", "") |> 
    cepR::busca_cep(token = token)
}

cep(endereco = "rua desportista jose de farias")