# Análisis comparativo entre tipo de elección (PASO vs Generales)



# 0. Librerías y carpetas necesarias
#------------------------------------

# librerías a utilizar
libs <- c("readr", "dplyr", "tidyr", "flextable", "stringr", "sf", "ggplot2", "ggrepel", "scales")

installed_libraries <- libs %in% rownames(installed.packages())

if (any(installed_libraries == FALSE)) {
  install.packages(libs[!installed_libraries])
}
invisible(lapply(libs, library, character.only = TRUE))

# carpeta para los gráficos generados
if (!file.exists("plots")) dir.create("plots")

rm(list = c("libs", "installed_libraries"))



# 1. Cargar datos originales
#---------------------------

# lista de nombres de archivos CSV
archivos_csv <- c(
  "agrupacion", "cargo", "circuito", "distrito", "eleccion",
  "establecimiento", "lista", "mesa", "municipio",
  "seccion", "seccionprovincial", "tipovoto", "votacion"
)

# carpeta donde se encuentran los archivos CSV
directorio <- "data/"

# crear lista vacía para almacenar los dataframes
lista_dataframes <- list()

# cargar los archivos CSV en la lista de dataframes
for (archivo in archivos_csv) {
  archivo_path <- file.path(directorio, paste0(archivo, ".csv"))
  df <- readr::read_csv(archivo_path, show_col_types = FALSE)
  lista_dataframes[[archivo]] <- df
}

# asignar los dataframes a variables con nombres adecuados
for (archivo in archivos_csv) {
  assign(archivo, lista_dataframes[[archivo]])
}

# eliminar variables temporales y datasets vacíos
rm(list = c("lista_dataframes", "archivo", "df", "archivo_path", "establecimiento", "municipio", "archivos_csv"))



# 2. Funciones
#-------------

# Función para procesamiento inicial de elección
procesar_eleccion <- function(tabla_elecciones, anio_eleccion, votacion) {
  # Paso 1: Seleccionar identificador de elección a procesar
  elecc_id <- tabla_elecciones |>
    filter(anio == anio_eleccion) |>
    select(eleccion_id)
  
  # Paso 2: Obtener del dataset de votaciones la elección deseada
  data <- votacion |>
    filter(eleccion_id %in% elecc_id$eleccion_id)
  
  # Paso 3: Unir las tablas para obtener nombres de agrupaciones, listas, tipo de voto y cargo
  data <- data |>
    left_join(agrupacion |> select(agrupacion_id, eleccion_id, nombre),
              by = c("agrupacion_id", "eleccion_id")) |>
    rename(agrupacion = nombre)
  
  data <- data |>
    left_join(lista |> select(lista_id, agrupacion_id, eleccion_id, nombre),
              by = c("lista_id", "agrupacion_id", "eleccion_id")) |>
    rename(lista = nombre)
  
  data <- data |>
    left_join(tipovoto |> select(tipovoto_id, nombre),
              by = c("tipovoto_id")) |>
    rename(tipovoto = nombre) # tipo de voto (comando, blanco, impugnado, etc.)
  
  data <- data |>
    left_join(cargo |> select(cargo_id, nombre),
              by = c("cargo_id")) |>
    rename(cargo = nombre)
  
  data <- data |>
    left_join(eleccion |> select(eleccion_id, eleccion_tipo, anio),
              by = c("eleccion_id")) |>
    select(-c(eleccion_id, tipovoto_id, lista_id, agrupacion_id, cargo_id)) # eliminar variables de identificadores
  
  # Paso 4: Reemplazar valores de "agrupacion" según condición
  # El motivo es simplificar la visualización teniendo en una sola vista todos los votos positivos/no positivos
  no_positivos <- c("EN BLANCO", "IMPUGNADO", "NULO", "RECURRIDO")
  data <- data |>
    mutate(agrupacion = ifelse(tipovoto %in% no_positivos, paste0("_", tipovoto), agrupacion))
  
  # Paso 5: Convertir columnas a minúsculas para facilitar comparación
  columnas_a_convertir <- c("agrupacion", "tipovoto", "cargo", "eleccion_tipo", "lista")
  data <- data |>
    mutate_at(vars(all_of(columnas_a_convertir)), tolower)
  
  return(data)
}


# Función que realiza una comparativa entre instancias electorales primaria-general-ballotage
# requiere previamente ejecutar función "procesar_eleccion" y corregir manualmente nombre agrupaciones
calcular_resumen <- function(df, cargo_id, is_ballotage = FALSE, tabla_cargo = cargo) {
  # Filtrar el DataFrame para el cargo específico
  cargo_nombre <- tolower(tabla_cargo$nombre[cargo_id])
  data <- df |>
    filter(cargo == cargo_nombre) |>
    
    # Agrupar por tipo de elección, tipo de voto y agrupación
    group_by(eleccion_tipo, tipovoto, agrupacion) |>
    
    # Resumir los votos sumando
    summarise(n = sum(votos), .groups = "drop") |>
    
    # Ordenar por tipo de elección y agrupación
    arrange(desc(eleccion_tipo), agrupacion) |>
    
    # Eliminar la columna tipovoto
    select(-tipovoto) |>
    
    # Hacer un pivote para tener las columnas de tipo de elección
    pivot_wider(
      names_from = eleccion_tipo,
      values_from = n,
      values_fill = 0
    )
  
  # Calcular el total de votos
  total_sum <- data |>
    summarize(
      paso = sum(paso),
      general = sum(general),
      ballotage = if (is_ballotage) sum(ballotage) else 0
    )
  
  # Si no es ballotage, eliminar la columna
  if (!is_ballotage) {
    total_sum <- total_sum |> select(-ballotage)
  }
  
  # Crear una fila con el total
  total_row <- data.frame(agrupacion = "total", total_sum)
  
  # Calcular la diferencia y ordenar
  data <- data |>
    mutate(
      dif = ifelse(general > 0, general - paso, NA),
      prc_dif = as.numeric(ifelse(paso > 0 & general > 0, format(dif / paso * 100, nsmall = 0, digits = 2), NA))
    ) |>
    select(agrupacion, paso, general, dif, prc_dif, ballotage = matches("ballotage")) |>
    arrange(desc(general))
  
  return(data)
}



# Función para visualizar tablas con resumen comparativo entre paso y general
visualizar_tabla <- function(df, anio, cargo) {
  # Convertir agrupación a título para facilitar la lectura
  df$agrupacion <- str_to_title(df$agrupacion)
  
  # Crear la tabla flextable
  ft <- flextable(df)
  
  # Aplicar formato a las filas cuyos valores de agrupación comienzan con "_"
  ft <- ft |>
    set_table_properties(width = .9, layout = "autofit") |>
    autofit() |>
    bg(i = ~ grepl("^_", agrupacion), bg = "gray90") |>
    color(~ dif < 0, ~dif, color = "red") |>
    color(~ prc_dif < 0, ~prc_dif, color = "red") |>
    set_header_labels(
      agrupacion = "Agrupación",
      paso = "PASO\n(votos)",
      general = "General\n(votos)",
      dif = "Diferencia\n(votos)",
      prc_dif = "% Dif",
      ballotage = "Ballotage\n(votos)"
    ) |>
    colformat_num(
      big.mark = ".",
      decimal.mark = ",",
      na_str = ""
    ) |>
    set_caption(paste0("Seccion: ", seccion_obj, " - Elecciones ", anio, " a ", cargo)) |>
    theme_vanilla()
  
  # Guardar la tabla como imagen
  save_as_image(x = ft, path = paste0("plots/tb_elecciones_", anio, "-", gsub("/", "-", cargo), ".jpg"))
  
  # Imprimir la tabla
  print(ft)
}


# Función para visualizar detalle comparativo de votos POR MESA entre paso y general
generar_visualizacion_mesa <- function(mesaid, cargoid, eleccionesid) {
  
  visualizacion <- votacion_obj %>%
    select(mesa_id, cargo_id, eleccion_id, agrupacion_id, votos) %>%
    filter(mesa_id == mesaid, eleccion_id %in% eleccionesid, cargo_id == cargoid) %>%
    arrange(agrupacion_id) %>%
    left_join(
      agrupacion %>%
        filter(eleccion_id %in% eleccionesid) %>%
        select(eleccion_id, agrupacion_id, nombre),
      by = c("agrupacion_id", "eleccion_id"),
      relationship = "many-to-many"
    ) %>%
    mutate(
      eleccion = ifelse(eleccion_id == 10, "primaria", "general"),
      agrupacion = str_to_title(nombre)
    ) %>%
    select(agrupacion, eleccion, votos) %>%
    filter(!is.na(agrupacion)) %>%
    pivot_wider(names_from = eleccion, values_from = votos) %>%
    arrange(-general) %>%
    flextable() %>%
    set_table_properties(width = .9, layout = "autofit") %>%
    color( ~ (primaria == 0 & general > 0), ~ primaria, color = "red") %>%
    set_header_labels(agrupacion = "Agrupación",
                      primaria = "Primaria",
                      general = "General") %>%
    set_caption(paste("Detalle de Mesa ", mesaid)) %>%
    theme_vanilla()
  
  # Guardar la tabla como imagen
  save_as_image(x = visualizacion, path = paste0("plots/tb_detalle_mesa", mesaid,".jpg"))
  
  return(visualizacion)
}

# Función para usar en las tablas favoreciendo la interpretación de valores numéricos
colores_gradiente_RYG <- function(valores) {
  col_numeric(
    palette = c(
      "firebrick",
      "darkorange1",
      "gold",
      "yellowgreen",
      "forestgreen"
    ),
    domain = c(floor(min(valores)), ceiling(max(valores)))
  )
}


# Función para generar tabla de mesas con alto swing y baja diferencia de votantes
generar_tabla_swing <- function(datos, mesaid) {
  
  swing <- datos |> filter(mesa_id == mesaid) |> pull(swing) |> unique() |> round(digits = 1)
  difvotantes <- datos |> filter(mesa_id == mesaid) |> pull(prc_dif_votantes) |> unique() |> round(digits = 1)
  titulo <- paste0("Mesa N°:",mesaid,". Swing Mesa:", swing,"%. Dif. Electores:", difvotantes, "%")
  
  tabla <- datos |>
    filter(mesa_id == mesaid) |>
    arrange(-general) |>
    mutate(prc_dif = round(prc_dif, 1)) |>
    select(
      agrupacion,
      paso_vot = paso,
      paso_prc = prc_paso,
      general_vot = general,
      general_prc = prc_general,
      difprc = prc_dif
    )
  
  colores <- colores_gradiente_RYG(tabla$difprc)
  
  tabla <- tabla |>
    flextable() |>
    add_header_row(
      values = c(
        "Agrupación",
        "Primaria",
        "Primaria",
        "General",
        "General",
        "Swing Agrup."
      )
    ) %>%
    set_header_labels(
      agrupacion = "Agrupación",
      paso_vot = "(votos)",
      paso_prc = "(%)",
      general_vot = "(votos)",
      general_prc = "(%)",
      difprc = "(%)"
    ) |>
    theme_vanilla() |>
    autofit(part = "all")  |>
    align(align = "center", part = "all") |>
    merge_h(i = 1, part = "header") |>
    merge_v(part = "header") |>
    set_table_properties(width = .6, layout = "autofit") |>
    colformat_double(digits = 2) |>
    set_caption(titulo) |>
    bg(bg = colores,
       j = "difprc",
       part = "body")
  
  save_as_image(
    x = tabla,
    path = paste0("plots/tb_swing_mesa_",mesaid,
                  "_swing_",swing,
                  "_difvotantes_",difvotantes,
                  ".jpg"
                  )
    )
  
  return(tabla)
}

# Función que crea una tabla con los apellidos más frecuentes en la mesa elegida.
generar_tabla_apellido_x_mesa <- function(datos_mesas, datos_apellidos, mesaid) {
  
  # obtener primer elector del padrón
  elector_inicio <- datos_mesas |> filter(mesa_id == mesaid) |> pull(inicio)
  
  # obtener último elector del padrón
  elector_fin <- datos_mesas |> filter(mesa_id == mesaid) |> pull(fin)
  
  # generar tabla conteniendo los apellidos que acumulan el 60% del total de electores en la mesa 
  tabla <- datos_apellidos |>
    filter(apellido_y_nombre >= elector_inicio & apellido_y_nombre <= elector_fin) |>
    group_by(apellido) |>
    summarise(n = n(), .groups = "drop") |>
    arrange(-n) |> 
    mutate(prc = round(n / sum(n) * 100 ,1),
           prc_cum = cumsum(prc),
           apellido = str_to_title(apellido)) |> 
    filter(prc_cum < 60) |> 
    flextable() |> 
    theme_vanilla() |> 
    set_table_properties(width = .8, layout = "autofit") %>%
    set_header_labels(
      apellido = "Apellido",
      n = "Frecuencia",
      prc = "% en la mesa",
      prc_cum = "% acumulado"
    )
  
  save_as_image(
    x = tabla,
    path = paste0("plots/tb_apellidos_mesa_",mesaid,
                  ".jpg"
    )
  )
  
  return(tabla)
}



# 3. Preparación de Datos y Creación de Tabla Resumen con elecciones disponibles
#-------------------------------------------------------------------------------

# Distrito y sección de interés
distrito_obj <- "Buenos Aires"
seccion_obj <- "Trenque Lauquén" # El nombre real es Trenque Lauquen, sin acento

# Obtener identificadores de distrito y sección
distrito_obj_id <- distrito |>
  filter(nombre == distrito_obj) |>
  pull(distrito_id) # Identificador del distrito

seccion_obj_id <- seccion |>
  filter(distrito_id == distrito_obj_id & nombre == seccion_obj) |>
  pull(seccion_id) # Identificador de la sección electoral

# Filtrar datos de votación para el distrito y sección de interés
votacion_obj <- votacion |>
  filter(distrito_id == distrito_obj_id, seccion_id == seccion_obj_id) |>
  select(-c(distrito_id, seccion_id))

# Guardar los datos de votación filtrados
saveRDS(votacion_obj, file = "data/votacion_obj.rds")

# Unificar valores similares en la tabla 'eleccion'
eleccion <- eleccion |>
  mutate(eleccion_tipo = if_else(eleccion_tipo == "GENERALES", "GENERAL", eleccion_tipo))

# Crear una tabla resumen de las elecciones disponibles y su ID asociado
tabla <- eleccion |>
  filter(anio < 2023) |>
  select(anio, eleccion_tipo, eleccion_id) |>
  pivot_wider(names_from = anio, values_from = eleccion_id) |>
  flextable() |>
  set_table_properties(width = .9, layout = "autofit") |>
  set_header_labels(eleccion_tipo = "Tipo de elección") |>
  set_caption("Elecciones disponibles y su ID asociado") |>
  theme_vanilla() |>
  autofit()

# Imprimir la tabla y guardarla como imagen
tabla
save_as_image(x = tabla, path = "plots/tb_elecciones_disponibles.jpg")

rm(list = c("tabla"))



# 4. Procesamiento de los datos
#------------------------------

# cargar archivo de votación objetivo (para retomar desde aquí sin perder tiempo en pasos anteriores)
votacion_obj <- readRDS("data/votacion_obj.rds")

# Ejemplo de procesamiento para elecciones presidenciales 2011, 2015 (c/ballotage) y 2019
# El proceso consta de 3 etapas:
#   1) Generar una tabla donde los identificadores se cambian por los valores para facilitar la lectura humana
#   2) Corregir nombres de las listas ya que no coinciden entre las instancias de las PASO y las generales
#   3) Generar cuadro comparativo final y guardar el resultado

# Paso 1: Procesamiento inicial de eleccion
anio_obj <- 2011
df1 <- procesar_eleccion(eleccion, anio_obj, votacion_obj)

# Paso 2: Corregir manualmente nombre de las agrupaciones
df1 <- df1 |>
  mutate(agrupacion = case_when(
    agrupacion == "alianza unión para el desarrollo social" ~ "alianza union para el desarrollo social - udeso",
    agrupacion == "coalición cívica - afirmación para una república igualitaria ari" ~ "coalicion civica ari",
    TRUE ~ agrupacion # Mantener otros nombres sin cambios
  ))

# Paso 3: Procesamiento final de elección y grabación del resultado
cargo_id <- 1
df2 <- calcular_resumen(df = df1, cargo_id = cargo_id, is_ballotage = FALSE)
file_name <- paste0("data/out/tb_cmp_eleccion_", anio_obj, "_", tolower(gsub("/", "-", cargo$nombre[cargo_id])), ".rds")
saveRDS(df2, file = file_name)


### Idem 2015
# Paso 1: Procesamiento inicial de eleccion
anio_obj <- 2015
df1 <- procesar_eleccion(eleccion, anio_obj, votacion_obj)
# Paso 2: Corregir manualmente nombre de las agrupaciones
df1 <- df1 |>
  mutate(agrupacion = case_when(
    agrupacion == "cambiemos" ~ "alianza cambiemos",
    agrupacion == "frente para la victoria" ~ "alianza frente para la victoria",
    TRUE ~ agrupacion # Mantener otros nombres sin cambios
  ))
# Paso 3: Procesamiento final de elección y grabación del resultado
cargo_id <- 1
df2 <- calcular_resumen(df = df1, cargo_id = cargo_id, is_ballotage = TRUE)
file_name <- paste0("data/out/tb_cmp_eleccion_", anio_obj, "_", tolower(gsub("/", "-", cargo$nombre[cargo_id])), ".rds")
saveRDS(df2, file = file_name)


### Idem 2019
# Paso 1: Procesamiento inicial de eleccion
anio_obj <- 2019
df1 <- procesar_eleccion(eleccion, anio_obj, votacion_obj)
# Paso 2 no es necesario en esta oportunidad porque las agrupaciones coinciden
# Paso 3: Procesamiento final de elección y grabación del resultado
cargo_id <- 1
df2 <- calcular_resumen(df = df1, cargo_id = cargo_id, is_ballotage = FALSE)
file_name <- paste0("data/out/tb_cmp_eleccion_", anio_obj, "_", tolower(gsub("/", "-", cargo$nombre[cargo_id])), ".rds")
saveRDS(df2, file = file_name)

rm(cargo_id, df1, df2, file_name, anio_obj)



# 5. Visualización de circuitos y tablas resúmenes
#-------------------------------------------------

# Graficar circuitos electorales del distrito para dar contexto
circuito_02_sf <- st_read(dsn = "data/circuito_02.shp")

seccion_obj <- "Trenque Lauquen" # distrito de la provincia de Buenos Aires a graficar

# personalizado para el distrito de Trenque Lauquen, modificar por datos propios cuando se refieran a otro distrito
localidades <- data.frame(
  city = c("30 de Agosto", "Beruti", "Girodías", "La Carreta", "Garré", "Pje. F. de Vitoria",
           "Pje. F. Magnano", "Cnia. Martín Fierro", "Trenque\nLauquen", "Trongé", "Pje. La María",
           "La Porteña", "Lértora", "La Zanja", "Est. Primera\nJunta"),
  lat = c(-36.2786, -35.8564, -36.3645, -36.20687, -36.55870, -35.616172, -35.655, -35.714048,
          -35.9731, -36.46407, -36.16795, -36.366224, -35.915716, -36.051953, -35.914497),
  lng = c(-62.5452, -62.5116, -62.3569, -62.23899, -62.60545, -62.699970, -62.5014, -62.783642,
          -62.7335, -62.48382, -62.67149, -62.716671, -62.966928, -62.867220, -62.639837)
)

localidades <- st_as_sf(localidades, remove = FALSE, coords = c("lng", "lat"), crs = 4326)

p <- ggplot(circuito_02_sf |> filter(cabecera == seccion_obj)) +
  geom_sf(aes(fill = factor(circuito)), alpha = 0.5) +
  geom_sf(data = localidades) +
  geom_text_repel(
    data = localidades, aes(x = lng, y = lat, label = city),
    fontface = "bold", nudge_x = c(-0.60, 0.25, 0.20, 0.10, 0.20, -0.30, 0.25, -0.30,
                                   -1.20, 0.20, -0.60, -0.60, -0.30, -0.60, 0.50),
    nudge_y = c(0.00, 0.00, 0.00, -0.10, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00)
  ) +
  coord_sf(
    xlim = c(-63.5, -62.0),
    ylim = c(-35.5, -36.6),
    expand = FALSE
  ) +
  labs(
    x = "",
    y = "",
    title = paste0("Localidades y circuitos en ", seccion_obj),
    caption = "Fuente: Elaboración propia",
    fill = "Circuitos"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Eliminar la grilla
    legend.position = "bottom",    # Mover la leyenda abajo
    axis.text.x = element_blank(), # quitar texto que no aporta información
    axis.text.y = element_blank()
  )

p
ggsave("plots/circuitos_y_localidades.jpg", plot = p, width = 10, height = 10, dpi = 300)


# Visualizar tablas resumen de votos
directorio <- "data/out/" # carpeta donde se encuentran los archivos

# lista de archivos que cumplen con los criterios
archivos <- list.files(directorio, pattern = "^tb_cmp_eleccion.*\\.rds$", full.names = TRUE)

# Cargar y visualizar cada archivo
for (archivo in archivos) {
  # Cargar el dataset desde el archivo
  dataset <- readRDS(archivo)
  
  # Uso de expresiones regulares para extraer los valores
  matches <- str_match(archivo, "tb_cmp_eleccion_(\\d+)_([^\\.]+)\\.rds")
  
  # Obtener los valores extraídos
  anio <- matches[1, 2]
  cargo_obj <- matches[1, 3]
  cargo_obj <- gsub("-", "/", cargo_obj)
  
  # Aplicar la función visualizar_tabla al dataset
  visualizar_tabla(dataset, anio, cargo_obj)
}
rm(list = c("matches", "dataset", "anio", "archivos", "archivo", "p", "circuito_02_sf", "seccion_obj", "localidades", 
            "directorio", "cargo_obj"
            )
   )



# 6. Depuración de mesas
#-----------------------

# Cargo y año de elección a analizar
cargo_obj <- "presidente/a"
anio_obj <- 2019

# Procesamiento de la elección objetivo
df_2019 <- procesar_eleccion(eleccion, anio_obj, votacion_obj)

# Votos por mesa
data <- df_2019 |>
  filter(cargo == cargo_obj) |>
  group_by(eleccion_tipo, tipovoto, agrupacion, mesa_id) |>
  summarise(n = sum(votos), .groups = "drop") |>
  arrange(desc(mesa_id), desc(eleccion_tipo), agrupacion) |>
  select(-tipovoto) |>
  pivot_wider(
    names_from = eleccion_tipo,
    values_from = n,
    values_fill = 0
  ) |>
  mutate(dif = general - paso)

# Resumen de votos por mesa (acumulado)
resumen <- data |>
  group_by(mesa_id) |>
  summarise(
    paso_sum = sum(paso),
    general_sum = sum(general),
    promedio = (paso_sum + general_sum) / 2,
    dif_cum = sum(abs(dif)),
    .groups = "drop"
  )

# Mesas sin datos de PASO o Generales
tabla <- resumen |>
  filter(paso_sum == 0 | general_sum == 0) |>
  select(-c(promedio, dif_cum)) |>
  flextable() |>
  set_table_properties(width = .9, layout = "autofit") |>
  color(~ paso_sum == 0, ~paso_sum, color = "red") |>
  color(~ general_sum == 0, ~general_sum, color = "red") |>
  set_header_labels(
    mesa_id = "Mesa N°",
    paso_sum = "Primaria\n(votos)",
    general_sum = "General\n(votos)"
  ) |>
  set_caption("Mesas a excluir por falta de datos") |>
  theme_vanilla() |>
  autofit()

tabla

# Guardar tabla como imagen
save_as_image(x = tabla, path = "plots/tb_mesas_sin_votos.jpg")

# Mesas excluidas por falta de datos
mesas_excluidas <- resumen |>
  filter(paso_sum == 0 | general_sum == 0) |>
  pull(mesa_id)

# Resumen ajustado al excluir mesas sin datos
resumen_adj <- resumen |>
  filter(!mesa_id %in% mesas_excluidas) |>
  mutate(
    min = ifelse(paso_sum < general_sum, paso_sum, general_sum),
    max = ifelse(paso_sum > general_sum, paso_sum, general_sum)
  )

# Visualización de rangos por mesas
p <-
  ggplot(resumen_adj, aes(x = reorder(as.character(mesa_id), -promedio), y = promedio)) +
  geom_linerange(
    aes(
      ymin = min,
      ymax = max
    ),
    alpha = 0.9,
    linewidth = 1.3
  ) +
  labs(x = "Mesas", y = "Diferencia de votos (rango) entre elección primaria y general") +
  geom_hline(
    yintercept = mean(resumen_adj$promedio),
    color = "firebrick", ,
    linetype = "dashed"
  ) +
  theme_minimal() +
  scale_x_discrete(labels = NULL) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  geom_text(
    x = 40,
    y = median(resumen_adj$promedio) - 105,
    label = paste0(
      "Mediana de la\ndiferencia de votos\npromedio entre\nprimaria y general\n",
      "    (", round(median(resumen_adj$promedio)), " votos)"
    ),
    hjust = 0,
    size = 4,
    color = "firebrick"
  ) +
  coord_flip()

p

# Guardar gráfico como imagen
ggsave("plots/dif_votos_primaria_general.jpg", plot = p, width = 6, height = 8, dpi = 300)

# Mesas con pocos votantes respecto de la mediana de la sección
votos_x_mesa_avg <- median(resumen_adj$promedio) |> floor()

tabla <- resumen_adj |>
  filter(promedio < votos_x_mesa_avg * .8) |>
  arrange(promedio) |>
  select(mesa_id, paso_sum, general_sum) |>
  flextable() |>
  set_table_properties(width = .6, layout = "autofit") |>
  set_header_labels(
    mesa_id = "Mesa N°",
    paso_sum = "Primaria\n(votos)",
    general_sum = "General\n(votos)"
  ) |>
  set_caption("Mesas con promedio de votos inferior al 80% de la mediana del total de mesas") |>
  theme_vanilla() |>
  autofit()

tabla

# Guardar tabla como imagen
save_as_image(x = tabla, path = "plots/tb_mesas_pocos_votantes.jpg")


# Mesas con valores bajos de votos emitidos promediando primaria y general
ggplot(
  data = resumen_adj |> filter(promedio < votos_x_mesa_avg),
  aes(
    x = reorder(as.character(mesa_id), -promedio),
    y = promedio,
    color = factor(promedio < votos_x_mesa_avg * 0.8)
  )
) +
  geom_linerange(
    aes(ymin = min, ymax = max),
    alpha = 0.9,
    linewidth = 1.3
  ) +
  labs(
    x = "N° Mesa",
    y = "Votos",
    title = "Diferencia de votos entre Paso y General",
    caption = "Fuente: Elaboración propia"
  ) +
  geom_hline(
    yintercept = median(resumen_adj$promedio),
    color = "firebrick",
    linetype = "dashed"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black")) +
  coord_flip() +
  geom_text(
    x = 10,
    y = median(resumen_adj$promedio),
    label = "Mediana de votos promedio",
    hjust = 1.2,
    size = 4,
    color = "firebrick"
  )


# Mesas excluidas por bajo promedio de votos respecto de la sección electoral
# debido a que tiene bajo impacto en el resultado
mesas_excluidas <- c(mesas_excluidas, resumen_adj |>
                       filter(promedio < votos_x_mesa_avg * .8) |>
                       arrange(promedio) |>
                       pull(mesa_id))

resumen_adj <- resumen_adj |> filter(!mesa_id %in% mesas_excluidas)



# Verificación casos de mesas con rango extremo

## Calcular mediana del rango del total de mesas a analizar
rango_x_mesa_avg <- resumen_adj |>
  summarise(median = median(dif_cum)) |>
  pull(median) |>
  round()

# Calcular la mediana y el rango intercuartil por mesa
median_dif_cum <- median(resumen_adj$dif_cum)
iqr_dif_cum <- IQR(resumen_adj$dif_cum)

# Calcular el umbral para identificar valores extremos
umbral_mayor <- median_dif_cum + 2 * iqr_dif_cum
umbral_menor <- median_dif_cum - 2 * iqr_dif_cum

ggplot(resumen_adj, aes(
  x = reorder(as.character(mesa_id), -dif_cum),
  y = dif_cum,
  color = factor(dif_cum < umbral_menor | dif_cum > umbral_mayor)
)) +
  geom_point(
    alpha = 0.9,
    size = 1.3
  ) +
  labs(
    x = "N° Mesa",
    y = "Diferencia de votos emitidos",
    title = "Rango de votos emitidos entre instancia Paso y General",
    subtitle = "Referenicia: Color rojo implica diferencia mayor a 2 desviaciones estándares
    respecto del promedio de las mesas",
    caption = "Fuente: Elaboración propia"
  ) +
  geom_hline(
    yintercept = rango_x_mesa_avg,
    color = "firebrick",
    linetype = "dashed"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black")) +
  coord_flip() +
  geom_text(
    x = 100,
    y = rango_x_mesa_avg,
    label = paste0("Diferencia promedio\n    de votos entre\nPrimaria y General\n       (", rango_x_mesa_avg, " votos)"),
    hjust = -0.08,
    vjust = 1,
    size = 4,
    color = "firebrick"
  )

resumen_adj |> filter(dif_cum > umbral_mayor)

## Mesa 4: si bien la mesa 4 posee votos cargados en la primaria, omitieron hacerlo para una de las fuerzas mayoritarias
##         de ahí el rango elevado. Debe ser excluida
mesa_rango_outlier <- generar_visualizacion_mesa(mesaid = 4, cargoid = 1, eleccionesid = c(10, 11))
mesa_rango_outlier |> autofit()

save_as_image(mesa_rango_outlier|> autofit(), path = "plots/tb_mesa_excluida_agrupacion_sin_carga.jpg")

mesas_excluidas <- c(mesas_excluidas, 4)
                     

## Mesa 79: situación OK
mesa_rango_outlier <- generar_visualizacion_mesa(mesaid = 79, cargoid = 1, eleccionesid = c(10, 11))
mesa_rango_outlier

## Mesa 89: situación OK para agrupaciones mayoritarias. ¿Se olvidaron de cargar Consenso Federal en la primaria?
mesa_rango_outlier <- generar_visualizacion_mesa(mesaid = 89, cargoid = 1, eleccionesid = c(10, 11))
mesa_rango_outlier


# Se depura el dataset a considerar en base a los criterios anteriores

data_adj <- data |>
  filter(!mesa_id %in% mesas_excluidas)

rm(cargo_obj, anio_obj, df_2019, resumen, tabla, mesas_excluidas, resumen_adj, p, votos_x_mesa_avg, rango_x_mesa_avg,
   median_dif_cum, iqr_dif_cum, umbral_mayor, umbral_menor, mesa_rango_outlier)



# 7. Swing en los votos
#--------------------------------------------------------------------------
# Se compara el % de votos obtenidos en la PASO y en la General para cada agrupación

# calculamos % de votos recibidos en cada eleccion por las agrupaciones
data_adj <- data_adj |>
  group_by(mesa_id) |>
  mutate(
    prc_paso = paso / sum(paso) * 100,
    prc_general = general / sum(general) * 100,
    prc_dif = prc_general - prc_paso,
    votantes_paso = sum(paso),
    votantes_general = sum(general),
    prc_dif_votantes = (votantes_general - votantes_paso) / votantes_paso * 100
  ) |>
  ungroup()

# eliminamos aquellas agrupaciones que no participaron en 2da. vuelta
data_adj <- data_adj |>
  filter(general > 0 | agrupacion == "_en blanco")

# calculamos el swing por mesa
data_adj <- data_adj |>
  group_by(mesa_id) |>
  mutate(swing = sum(abs(prc_dif) / 2)) |> 
  ungroup()

# distribución de swing en la sección, sin considerar la diferencia de votantes
swing_m = mean(resumen_swing$swing, na.rm = T)
swing_sd = sd(resumen_swing$swing, na.rm = T)

p <- ggplot(data = resumen_swing) +
  geom_histogram(aes(y = ..density.., x = swing),
                 binwidth = 0.8,
                 fill = "steelblue") +
  stat_function(
    data = resumen_swing,
    fun = dnorm,
    args = list(mean = swing_m,
                sd = swing_sd),
    color = "firebrick",
    size = 0.8
  ) +
  theme_minimal() +
  labs(x = "Swing (%)", y = "Densidad", subtitle = "Histograma del porcentaje de swing y curva normal de referencia") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size = 8)) + 
  scale_x_continuous(limits = c(0, max(resumen_swing$swing)*1.1))

p

ggsave("plots/hist_swing_y_normal.jpg", plot = p, width = 8, height = 6, dpi = 300)


resumen_swing <- data_adj |>
  group_by(mesa_id) |>
  select(mesa_id, swing, prc_dif_votantes) |>
  unique() |>
  ungroup()

mediana_swing <- median(resumen_swing$swing)
mediana_prc_dif_votantes <- median(resumen_swing$prc_dif_votantes)

p <- ggplot(data = resumen_swing) +
  geom_line(aes(x = reorder(mesa_id, -prc_dif_votantes), y = swing, group = 1), color = "steelblue") +
  geom_point(aes(
    x = reorder(mesa_id, -prc_dif_votantes),
    y = swing),
    color = ifelse(resumen_swing$swing > mediana_swing,"steelblue", "black"),
    size = ifelse(resumen_swing$swing > mediana_swing, 2, 1)
    ) +
  geom_line(aes(x = reorder(mesa_id, -prc_dif_votantes), y = prc_dif_votantes, group = 1), color = "firebrick") +
  geom_point(aes(
    x = reorder(mesa_id, -prc_dif_votantes),
    y = prc_dif_votantes),
    color = ifelse(resumen_swing$prc_dif_votantes <= mediana_prc_dif_votantes,"firebrick", "black")
    ) +
  geom_hline(yintercept = median(resumen_swing$swing), color = "steelblue", linetype = "dashed") +
  geom_hline(yintercept = median(resumen_swing$prc_dif_votantes), color = "firebrick", linetype = "dashed") +
  geom_text(
    aes(
      x = 37,
      y = median(swing),
      label = paste0("Mediana Swing:", round(mediana_swing, 0),"%"),
      hjust = -0.1,
      vjust = -1
    ),
    color = "blue",
    size = 4
  ) +
  geom_text(
    aes(
      x = 90,
      y = median(prc_dif_votantes),
      label = paste0("Mediana diferencia votantes: ", round(mediana_prc_dif_votantes,0),"%"),
      hjust = 0.5, vjust = -1
    ),
    color = "red",
    size = 4
  ) +
  labs(x = "Mesas de votación", y = "Swing  /  % Diferencia Votantes") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), panel.grid.major.x = element_blank()) +
  scale_y_continuous(breaks = seq(0, max(max(resumen_swing$swing), max(resumen_swing$prc_dif_votantes)), 5))

p

# Guardar gráfico como imagen
ggsave("plots/swing_y_dif_votantes.jpg", plot = p, width = 10, height = 6, dpi = 300)

tabla <- resumen_swing |>
  filter(prc_dif_votantes < mediana_prc_dif_votantes & swing > mediana_swing) |>
  arrange(-swing) |> 
  mutate(
    swing = round(swing, 1),
    prc_dif_votantes = round(prc_dif_votantes, 1)
    )

colourer_swing <- col_numeric(
  palette = c("transparent", "steelblue"),
  domain = c(floor(min(tabla$swing)), ceiling(max(tabla$swing)))
)

colourer_dif_votantes <- col_numeric(
  palette = c("transparent", "firebrick"),
  domain = c(floor(min(tabla$prc_dif_votantes)), ceiling(max(tabla$prc_dif_votantes))),
  reverse = TRUE
)

print("Mesas con alto Swing y baja diferencia de votantes")
tabla <- tabla |> 
  flextable() |> 
  theme_vanilla() |> 
  set_table_properties(width = .6, layout = "autofit") %>%
  set_header_labels(
    mesa_id = "Mesa N°",
    swing = "% Swing",
    prc_dif_votantes = "% Dif. Votantes"
  ) |> 
  align(align = "center", part = "all") %>%
  bg(
    bg = colourer_swing,
    j = "swing",
    part = "body"
    ) |> 
  bg(
    bg = colourer_dif_votantes,
    j = "prc_dif_votantes",
    part = "body"
  )

tabla
# Guardar tabla como imagen
save_as_image(x = tabla, path = "plots/tb_mesas_alto_swing_baja_diferencia_votantes.jpg")

# análisis de casos puntuales
mesaid <- 89
generar_tabla_swing(data_adj, mesaid)

mesaid <- 14
generar_tabla_swing(data_adj, mesaid)

mesaid <- 90
generar_tabla_swing(data_adj, mesaid)

mesaid <- 16
generar_tabla_swing(data_adj, mesaid)

mesaid <- 1
generar_tabla_swing(data_adj, mesaid)

mesaid <- 52
generar_tabla_swing(data_adj, mesaid)





# 8. Distribución de apellidos en mesas con alto swing y baja diferencia de electores
#------------------------------------------------------------------------------------

## Cargar mesa_id, establecimiento, elector inicial y elector final
## fuente: https://datatrenque.com.ar/donde-votas-todo-el-distrito-mesa-por-mesa-con-establecimientos-y-direcciones-2/
mesas_distrito_2019 <- read_delim(
  "data/mesas_distrito_2019.csv",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE,
  locale = locale(encoding = "latin1"), # Especifica el juego de caracteres (charset) como "latin1"
  show_col_types = FALSE
)

# Extraer el identificador de la mesa
mesas_distrito_2019$mesa_id <- as.integer(str_extract(mesas_distrito_2019$escuela, "(?<=MESA )\\d+"))

# cargar dataset con guía telefónica como referencia de distribución de apellidos
# en el caso de estudio la compañía eléctrica también es la de telecomunicaciones
# y junto con el servicio de internet incluye una línea fija gratis, por lo que se
# reduce el sesgo de tipo económico al tomar una guía telefónica como indicador de
# apellidos de la sección electoral
apellidos_df <- readRDS("data/out/apellidos_guia_telefonica.rds")

generar_tabla_apellido_x_mesa(mesas_distrito_2019, apellidos_df, 89) # mesa 89

generar_tabla_apellido_x_mesa(mesas_distrito_2019, apellidos_df, 14) # mesa 14

generar_tabla_apellido_x_mesa(mesas_distrito_2019, apellidos_df, 90) # mesa 90

generar_tabla_apellido_x_mesa(mesas_distrito_2019, apellidos_df, 16) # mesa 16

generar_tabla_apellido_x_mesa(mesas_distrito_2019, apellidos_df, 1)  # mesa 1

generar_tabla_apellido_x_mesa(mesas_distrito_2019, apellidos_df, 52) # mesa 52

