El script se divide en partes, dentro de un único documento, y a continuación se resumen el objetivo de cada una de ellas:

### Parte 0

Se instalar y/o cargan las librerías necesarias para el análisis de los datos. Se genera una carpeta "plots" para guardar los gráficos más representativos
y que se incluyen en este reporte.

El código que facilitó el análisis y generar las imágenes utilizadas en este reporte se encuentran disponibles en el repositorio de Github [https://github.com/marcoscarloseduardo/elecciones-2011-2019](https://github.com/marcoscarloseduardo/elecciones-2011-2019). Para ejecutar el código es necesario descargar y descromprimir en la carpeta "data" el archivo [elecciones.csv.7z](https://socialstats.la/archivos/argentina/elecciones/) compartido por [Mauro Infantino](https://twitter.com/plenque), ya que por su tamaño no es posible subirlo aquí.

Para la representación cartográfica se descargaron los circuitos electorales correspondientes a la provincia de Buenos Aires desde la sección [Mapa Electoral](https://mapa2.electoral.gov.ar/descargas/) de la Cámara Nacional Electoral.

En la carpeta *data* se encuentran estos archivos, que poseen formato csv, de los datos de las elecciones. En la subcarpeta *out* se guardan tablas generadas durante el procesamiento de los datos para poder acceder en un futuro a ella sin necesidad de ejecutar nuevamente el scritp.

Como ya se mencionó, en la carpeta **plots** se guardan todos las imágenes que se generaron al ejecutar el script.

### Parte 1

Se importaron los datos mencionados anteriormente y se les dio formato de dataset.

### Parte 2

Se generaron funciones necesarias para el procesamiento y análisis de los datos, facilitando la interpretación de la línea de análisis de las partes siguientes.

### Parte 3

Se trabajó sobre los datos de manera de vincularlos entre sí y filtrar los mismos para acotar el dataset a la sección electoral a analizar. Se corrigieron inconsistencias de nombres y se generó una tabla resumen de las elecciones disponibles.

### Parte 4

Se realizó el procesamiento de las elecciones corrspondientes a los años 2011, 2015 y 2019 para el cargo de presidente a nivel de agrupación, sin discriminar por lista interna. Se tuvo que corregir manualmente los casos donde el nombre de la agrupación en la instancia primaria no coincidía con el de la general.

### Parte 5

Se visualizan los circuitos electorales correspondiente a la sección analizada y las tablas que resumen los resultados para las elecciones mencionadas.

### Parte 6

Se pone foco en el año 2019 para analizar votación por cada Mesa. Se excluyen las mesas en las que se detecta:
- ausencia de datos para la instancia primaria o general,
- baja cantidad de votos respecto de la mediana de la sección, por carecer de impacto en el resultado de la elección,
- mesas que carecen de datos en la elección primaria en alguna de las agrupaciones mayoritarias

### Parte 7

Se calcula el Swing por Mesa y la diferencia de votantes entre ambas instancias electorales. El motivo es que si hay una alta proporción de votantes en la general, la diferencia en la proporción de votos de las agrupaciones respecto de la primaria podría estar dada por los electores que no sufragaron en la PASO.

Las mesas que resultan de interés es donde la proporción de votantes es similar entre ambas instancias per el swing es alto, lo que favorece que sean los electores de la primaria quienes están cambiando su voto a otra agrupación.

Se visualiza el detalle para 6 mesas que cumplen con esta condición.

### Parte 8

Se analiza la frecuencia de apellidos dentro de las mesas identificadas en la Parte 7 con intención de explorar el grado de exposición tienen las familias en el interior a partir de la implementación de la obligatoriedad de las elecciones primarias.# elecciones-2011-2019
