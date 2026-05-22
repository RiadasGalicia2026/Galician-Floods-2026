install.packages(c(
  "tidyverse",
  "httr2",
  "rvest",
  "xml2",
  "stringr",
  "pagedown"
))



library(tidyverse)
library(httr2)
library(rvest)
library(xml2)
library(stringr)
library(pagedown)

# ---------------------------------------------------------
# 1. DIRECTORIOS
# ---------------------------------------------------------

dir.create("pdf", showWarnings = FALSE, recursive = TRUE)

# ---------------------------------------------------------
# 2. LIMPIEZA DE TEXTO
# ---------------------------------------------------------

limpiar_texto <- function(x){
  
  x <- as.character(x)
  
  x <- str_replace_all(x, "\u2032|\u2019|\u2018", "'")
  x <- str_replace_all(x, "\u201c|\u201d", "\"")
  x <- str_replace_all(x, "\u00a0", " ")
  x <- str_replace_all(x, "\n", " ")
  x <- str_squish(x)
  
  return(x)
}

# ---------------------------------------------------------
# 3. BASE DE NOTICIAS (AMPLIADA)
# ---------------------------------------------------------

noticias <- tibble(
  
  fecha = as.Date(c(
    "2026-01-30","2026-02-02","2026-02-03",
    "2026-02-04","2026-02-07","2026-02-10",
    "2026-02-12","2026-02-13","2026-02-19","2026-02-22",
    
    # NUEVAS
    "2026-02-11","2026-02-11","2026-02-11",
    "2026-02-12","2026-02-14"
  )),
  
  medio = c(
    "Teleprensa","Cadena SER","Teleprensa","Cadena SER","Cadena SER",
    "Galicia Press","Europa Press","Cadena SER","Cadena SER","Europa Press",
    
    # NUEVAS
    "El País","Cadena SER","Europa Press",
    "Cadena SER Vigo","AEMET / AS"
  ),
  
  titulo = c(
    "Vientos 120 km/h y rios en seguimiento",
    "Inundaciones sigilosas en Galicia",
    "Seguimiento 15 rios",
    "Desbordamiento Mendo y Mandeo",
    "Trenes suspendidos por inundaciones",
    "Rios en nivel rojo",
    "Borrasca Nils 800 incidencias",
    "40 dias de alertas en Galicia",
    "Obras antiinundaciones",
    "Desactivacion INUNGAL",
    
    # NUEVAS
    "Nils prolonga la inestabilidad en Galicia",
    "Tres ríos en alerta roja por desbordamiento",
    "Más de 300 incidencias por la borrasca Nils",
    "Daños graves en Pontevedra por la borrasca Nils",
    "Nueva borrasca Oriana y continuidad del temporal"
  ),
  
  url = c(
    "https://www.teleprensa.com/articulo/nacional-3/vientos-mas-120-km-h-cauces-rios-seguimiento-transporte-ria-cancelado-galicia/202601301257422328847.html",
    "https://cadenaser.com/galicia/2026/02/02/inundaciones-sigilosas-galicia-refuerza-la-vigilancia-de-los-rios-en-pleno-temporal-radio-coruna/",
    "https://www.teleprensa.com/articulo/nacional-3/galicia-mantiene-seguimiento-caudal-15-rios-riesgo-inundaciones/202602030947382331412.html",
    "https://cadenaser.com/galicia/2026/02/04/los-rios-mendo-y-mandeo-se-desbordan-en-betanzos-radio-coruna/",
    "https://cadenaser.com/galicia/2026/02/07/renfe-aisla-a-vigo-por-tercer-dia-trenes-suspendidos/",
    "https://www.galiciapress.es/articulo/ultima-hora/2026-02-10/5766774-tres-rios-permanecen-nivel-rojo-galicia-ante-riesgo-desbordamiento",
    "https://www.europapress.es/galicia/noticia-borrasca-nils-deja-cerca-800-incidencias-galicia-20260212102422.html",
    "https://cadenaser.com/galicia/2026/02/13/galicia-sobrevive-a-40-dias-de-alertas/",
    "https://cadenaser.com/galicia/2026/02/19/pontevedra-obras-inundaciones/",
    "https://www.europapress.es/galicia/noticia-xunta-desactiva-plan-inungal-20260222140614.html",
    
    # NUEVAS
    "https://elpais.com/el-tiempo/2026-02-11/la-borrasca-nils-no-da-tregua-y-prolonga-la-inestabilidad-en-la-peninsula.html",
    "https://cadenaser.com/galicia/2026/02/11/nils-azota-galicia-con-tres-rios-en-alerta-roja-y-avisos-por-viento-y-lluvia-radio-galicia/",
    "https://www.europapress.es/galicia/noticia-borrasca-nils-deja-cerca-800-incidencias-galicia-mas-300-arboles-carreteras-20260212102422.html",
    "https://cadenaser.com/galicia/2026/02/12/las-consecuencias-de-la-borrasca-nils-deja-incidencias-graves-en-toda-la-provincia-radio-vigo/",
    "https://elpais.com/el-tiempo/2026-02-11/la-borrasca-nils-no-da-tregua-y-prolonga-la-inestabilidad-en-la-peninsula.html"
  )
)

# ---------------------------------------------------------
# 4. SCRAPING
# ---------------------------------------------------------

extraer_noticia <- function(url){
  
  tryCatch({
    
    Sys.sleep(1)
    
    html <- request(url) |>
      req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64)") |>
      req_perform() |>
      resp_body_html()
    
    titulo <- html |>
      html_element("title") |>
      html_text2()
    
    texto <- html |>
      html_elements("p") |>
      html_text2() |>
      paste(collapse = " ")
    
    list(titulo = titulo, texto = texto)
    
  }, error = function(e){
    list(
      titulo = "NO DISPONIBLE",
      texto = "No se pudo extraer contenido"
    )
  })
}

# ---------------------------------------------------------
# 5. HTML
# ---------------------------------------------------------

crear_html <- function(fecha, medio, titulo, texto){
  
  archivo_html <- file.path(
    "pdf",
    paste0(fecha, "_", tolower(gsub(" ", "_", medio)), ".html")
  )
  
  titulo <- limpiar_texto(titulo)
  texto <- limpiar_texto(texto)
  
  html <- paste0(
    "<html><head><meta charset='utf-8'></head><body>",
    "<h1>", titulo, "</h1>",
    "<p><b>Fecha:</b> ", fecha, "</p>",
    "<p><b>Medio:</b> ", medio, "</p>",
    "<hr>",
    "<p>", texto, "</p>",
    "</body></html>"
  )
  
  writeLines(html, archivo_html)
  
  return(archivo_html)
}

# ---------------------------------------------------------
# 6. PDF
# ---------------------------------------------------------

html_a_pdf <- function(html_file){
  
  pdf_file <- sub(".html", ".pdf", html_file)
  
  pagedown::chrome_print(
    input = html_file,
    output = pdf_file
  )
  
  return(pdf_file)
}

# ---------------------------------------------------------
# 7. PIPELINE
# ---------------------------------------------------------

for(i in 1:nrow(noticias)){
  
  info <- extraer_noticia(noticias$url[i])
  
  if(info$titulo == "NO DISPONIBLE") next
  
  html_file <- crear_html(
    noticias$fecha[i],
    noticias$medio[i],
    info$titulo,
    info$texto
  )
  
  pdf_file <- html_a_pdf(html_file)
  
  message("✔ PDF creado: ", pdf_file)
  
  Sys.sleep(2)
}

# ---------------------------------------------------------
# 8. FINAL
# ---------------------------------------------------------

print("✔ REPOSITORIO PDF COMPLETO ACTUALIZADO")



library(tidyverse)
library(pagedown)

# ---------------------------------------------------------
# 1. DIRECTORIOS
# ---------------------------------------------------------

dir.create("pdf", showWarnings = FALSE)

# ---------------------------------------------------------
# 2. EJEMPLO: LISTA DE PDFs GENERADOS

# ---------------------------------------------------------

pdf_files <- list.files("pdf", pattern = "\\.pdf$", full.names = FALSE)

# ---------------------------------------------------------
# 3. URL DEL REPOSITORIO
# ---------------------------------------------------------

base_url <- "https://celiaolabarria.github.io/riadas-galicia-2026/pdf/"

# ---------------------------------------------------------
# 4. CREAR ÍNDICE HTML PÚBLICO
# ---------------------------------------------------------

crear_indice <- function(pdf_files, base_url){
  
  enlaces <- map_chr(pdf_files, function(f){
    
    paste0(
      "<li><a href='", base_url, f, "'>", f, "</a></li>"
    )
    
  })
  
  html <- paste0(
    "<html>",
    "<head><meta charset='utf-8'><title>Repositorio PDF</title></head>",
    "<body>",
    "<h1>📄 Repositorio de informes PDF</h1>",
    "<p>Acceso público a los informes generados automáticamente.</p>",
    "<ul>",
    paste(enlaces, collapse = "\n"),
    "</ul>",
    "</body></html>"
  )
  
  writeLines(html, "index.html")
  
  return("index.html")
}

# ---------------------------------------------------------
# 5. GENERAR ÍNDICE
# ---------------------------------------------------------

indice <- crear_indice(pdf_files, base_url)

message("✔ Índice creado: ", indice)

# ---------------------------------------------------------
# 6. LINK PARA DOCUMENTO
# ---------------------------------------------------------

link_html <- paste0(
  "<p>📁 Accede al ",
  "<a href='", base_url, "'>repositorio de PDFs</a>",
  "</p>"
)

cat(link_html)
