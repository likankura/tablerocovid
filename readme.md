# Tablero de seguimiento COVID-19

## México



Esta herramienta busca solucionar el problema de comparación de magnitud de infecciones entre distintas regiones, mediante la traslación de la variable tiempo a ciertos puntos claves en cada región (primero 100 casos confirmados, 1er deceso, 190 decesos).

El tablero se encuentra disponible para consulta en: https://itzamna.org/lagaleria/extra/covid19/tablero/

### Fuente de datos



### Adaptación a otros países

Para cambiar los países incluidos es solo necesario modificar el siguiente vector, incluyendo solo los países desados:

`nat <- c('MEX','ECU','ESP','KOR','ITA','USA')`

Para establecer otro páis de referencia distinto a México, unicamente se sustituye `'MEX'` por la clave del país deseado en el vector

`mex <- data$countryterritoryCode=='MEX'`

No es necesario modificar el resto del script.

Ambos vectores se definen en el archivo `Plots1.R`

Estos cambios modifcan las gráficas, pero no el diseño del dashboard, este se puede modficiar en el archivo `Tablero.Rmd`