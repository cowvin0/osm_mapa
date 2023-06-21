library(magick)

newlogo <- image_scale(image_read("img/antiga.png"))
oldlogo <- image_scale(image_read("img/atual.jpg"))
image_resize(c(newlogo, oldlogo), '1920x550!') |> 
  image_background('white') |> 
  image_morph() |> 
  image_animate(fps = 3, loop = 1L, optimize = TRUE, delay = 1/2) |> 
  image_write("animacao.gif")