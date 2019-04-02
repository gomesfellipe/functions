save_flextable <- 
  function (x, png_file = "image.png"){
  
  # Inspirado na pergunta que fiz no stackoverflow;
  # https://stackoverflow.com/questions/50225669/how-to-save-flextable-as-png-in-r
  
    htmltools::save_html(htmltools_value(x), "temp.html")
    img = webshot::webshot("temp.html", png_file, zoom = 2)
    unlink("temp.html")
    magick::image_write(magick::image_trim(magick::image_read(str_c(img))), 
        path = png_file)
  }
