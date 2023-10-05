# Cargamos las bibliotecas necesarias
library(rvest)
library(dplyr)

# Definimos una función para extraer nombres de una página
scrape_nombres <- function(url) {
  # Leer la página web
  page <- read_html(url)
  
  # Extraer los nombres basados en la etiqueta class
  nombres <- page %>%
    html_nodes(".Estilo7.Estilo9") %>%
    html_text()
  
  return(nombres)
}

# Inicializamos una lista para almacenar los nombres
nombres_totales <- list()

# Inicializamos una variable para almacenar el último nombre
ultimo_nombre <- ""

# Recorremos todas las letras de la 'a' a la 'z'
for (letra in letters) {
  offset <- 0
  while (TRUE) {
    # Construir la URL con la letra actual y el offset
    url <- paste0("https://www.ovirtual.cooptl.com.ar/servicios/internet/guia_coop_nombreWP.asp?Nombre=", letra, "&offset=", offset)
    
    # Scraping de la página actual
    nombres <- scrape_nombres(url)
    
    # Si no se encontraron nombres, o el último nombre es igual al último de la página anterior, salimos del bucle
    if (length(nombres) == 0 || nombres[length(nombres)] == ultimo_nombre) break
    
    # Almacenamos el último nombre de la página actual
    ultimo_nombre <- nombres[length(nombres)]
    
    # Agregamos los nombres a la lista total
    nombres_totales[[length(nombres_totales) + 1]] <- nombres
    
    # Incrementamos el offset para la siguiente página
    offset <- offset + 10
  }
}

# Unimos todos los nombres en una lista en un vector
nombres_totales <- unique(unlist(nombres_totales))

# Imprimir los nombres
#cat(nombres_totales, sep = "\n")


nombres_df <- nombres_totales %>% as.data.frame()
colnames(nombres_df) <- "apellido_y_nombre"
nombres_df$apellido_y_nombre <- tolower(nombres_df$apellido_y_nombre)

# Encuentra las filas que contienen "S." en la columna "apellido_y_nombre"
filas_a_eliminar <- grep("s\\.", nombres_df$apellido_y_nombre, ignore.case = TRUE)
nombres_df <- nombres_df[-filas_a_eliminar, , drop = FALSE]

filas_a_eliminar <- grep("srl", nombres_df$apellido_y_nombre, ignore.case = TRUE)
nombres_df <- nombres_df[-filas_a_eliminar, , drop = FALSE]

filas_a_eliminar <- grep("coop", nombres_df$apellido_y_nombre, ignore.case = TRUE)
nombres_df <- nombres_df[-filas_a_eliminar, , drop = FALSE]

filas_a_eliminar <- grep("asoc", nombres_df$apellido_y_nombre, ignore.case = TRUE)
nombres_df <- nombres_df[-filas_a_eliminar, , drop = FALSE]

filas_a_eliminar <- grep("&", nombres_df$apellido_y_nombre, ignore.case = TRUE)
nombres_df <- nombres_df[-filas_a_eliminar, , drop = FALSE]

filas_a_eliminar <- grep("muni", nombres_df$apellido_y_nombre, ignore.case = TRUE)
nombres_df <- nombres_df[-filas_a_eliminar, , drop = FALSE]

filas_a_eliminar <- grep("lauq", nombres_df$apellido_y_nombre, ignore.case = TRUE)
nombres_df <- nombres_df[-filas_a_eliminar, , drop = FALSE]

filas_a_eliminar <- grep("club", nombres_df$apellido_y_nombre, ignore.case = TRUE)
nombres_df <- nombres_df[-filas_a_eliminar, , drop = FALSE]

filas_a_eliminar <- grep("sindicato", nombres_df$apellido_y_nombre, ignore.case = TRUE)
nombres_df <- nombres_df[-filas_a_eliminar, , drop = FALSE]

filas_a_eliminar <- grep("salud", nombres_df$apellido_y_nombre, ignore.case = TRUE)
nombres_df <- nombres_df[-filas_a_eliminar, , drop = FALSE]

filas_a_eliminar <- grep("farmacia", nombres_df$apellido_y_nombre, ignore.case = TRUE)
nombres_df <- nombres_df[-filas_a_eliminar, , drop = FALSE]

filas_a_eliminar <- grep("laboratorio", nombres_df$apellido_y_nombre, ignore.case = TRUE)
nombres_df <- nombres_df[-filas_a_eliminar, , drop = FALSE]


nombres_df <- nombres_df %>% mutate(apellido_y_nombre = trimws(apellido_y_nombre))
nombres_df <- nombres_df %>% mutate(apellido_y_nombre = gsub("\\s+", " ", apellido_y_nombre))

nombres_df <- subset(nombres_df, !grepl("\\..*\\.", nombres_df$apellido_y_nombre))

obtener_apellido <- function(nombre_completo) {
  nombre_completo <- trimws(nombre_completo)  # Elimina espacios en blanco al inicio y al final
  palabras <- unlist(strsplit(nombre_completo, " "))  # Divide la cadena en palabras
  
  # Lista de palabras que deseamos tratar como parte del apellido
  palabras_de_apellido <- c("de", "del", "la", "las", "los", "di", "da", "san")
  
  if (palabras[1] %in% palabras_de_apellido) {
    # Si la primera palabra está en la lista de palabras de apellido, verificamos si la segunda palabra también lo está
    if (length(palabras) >= 2 && palabras[2] %in% palabras_de_apellido) {
      # Si la segunda palabra también está en la lista, concatenamos la primera, la segunda y la tercera palabra como apellido
      apellido <- paste(palabras[1:3], collapse = " ")
    } else {
      # Si solo la primera palabra está en la lista, el apellido es la concatenación de la primera y la segunda palabra
      apellido <- paste(palabras[1:2], collapse = " ")
    }
  } else {
    # Si la primera palabra no está en la lista, el apellido es la primera palabra
    apellido <- palabras[1]
  }
  
  return(apellido)
}




# obtenemos apellido
nombres_df$apellido <- sapply(nombres_df$apellido_y_nombre, obtener_apellido)
nombres_df <- nombres_df %>% group_by(apellido) %>% mutate(n = n()) %>% ungroup()

# grabamos la tabla generada.
saveRDS(nombres_df, file = "data/out/apellidos_guia_telefonica.rds")
rm(nec_df, pagina_web, texto, url_list, css_tag, i, url_base) # eliminamos variables generadas