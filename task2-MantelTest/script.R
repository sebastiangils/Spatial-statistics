# Trabajo 2 - Estadística Espacial
# Geoestadística: El Test de Mantel

# Elaborado por Sebastian Gil Sanchez

# Librerías ----
# install.packages("vegan"); install.packages("geosphere")
library(geosphere)
library(vegan)

# Conjunto de datos tomados: Precipitación liquida en Bogotá durante el 
# año ...



precipitacion <- calidad[calidad$Variable == 'Precipitación Líquida', 
                  c( "Latitud", "Longitud", "Fecha","Departamento",
                     "Nombre.del.municipio","Unidades","Variable","Concentración")]

precip <- precipitacion

# ------------------------ Análisis Exploratorio -------------------------------

##Unidades: milimetros (mm) ----
table(precip$Unidades)

## Departamentos: ----
table(precip$Departamento)

## Municipios: ----
table(precip[precip$Departamento == "CUNDINAMARCA", ]$Nombre.del.municipio)
table(precip[precip$Departamento == "ANTIOQUIA", ]$Nombre.del.municipio)
# Se tomará el departamento de ANTIOQUIA al tener más municipios en el dataset

# ---------------------------- EJEMPLO 2 --------------------------------------
## Filtrado por Departamento = "ANTIOQUIA" ----
precip_a <- precip[precip$Departamento == "ANTIOQUIA", ]

## Fecha: ----
head(precip_a$Fecha)

# Cambiar formato de fecha: Str -> Date
precip_a$Fecha <- as.Date(precip_a$Fecha, format = "%d/%m/%Y")
class(precip_a$Fecha)
head(precip_a$Fecha)

tabla_frecuencias <- table(precip_a$Fecha)
# Encontrar la fecha con el conteo más alto
fecha_con_maximo_conteo <- names(tabla_frecuencias)[which.max(tabla_frecuencias)]
print(fecha_con_maximo_conteo)

## Filtrado de fecha: Primero de enero de 2018 ----
precip_a = precip_a[precip_a$Fecha == '2018-01-01',]

## Filtrado de columnas: Georeferencia y valor de la variable ---- 
names(precip_a)
precip_a <- subset(precip_a, select = c("Latitud", "Longitud", "Fecha",
                                        "Nombre.del.municipio", "Concentración",
                                        "Variable"))
head(precip_a)


## ------------------------- Test de Mantel ------------------------------------
## Aislando los datos ----
### Latitud y longitud ----
geo = data.frame(precip_a$Longitud , precip_a$Latitud)

### Precipitacion en mm ----
con = precip_a$Concentración

## Transformación a matrices de distancias ----
### Latitud y longitud ----
d.geo = distm(geo, fun = distHaversine)
dist.geo = as.dist(d.geo)

### Concentracion ----
dist.con = dist(con, method = "euclidean")

## Test ----
### Concentración vs Geografico
con_geo = mantel(dist.con, dist.geo, method = "spearman", permutations = 9999,
                 na.rm = TRUE)

## -------------------------------- Adicionales -------------------------------
#Primero le indicamos a R que nuestros datos cargados corresponden a matrices de distancia:
matrizgeo <- as.matrix(dist.geo)
matrizcon <- as.matrix(dist.con)

#Graficamos
grafica <- plot(matrizgeo, matrizcon, pch=16, cex=1, col="black",main = "Todos los municipios de Antioquia", xlab="Distancia geográfica (m)", ylab="Disimilitud de precipitación (mm)")
grafica


# ---------------------------------- EJEMPLO 1 --------------------------------
## Filtrado por Departamento = "CUNDINAMARCA" ----
precip_b <- precip[precip$Departamento == "CUNDINAMARCA", ]

## Filtrado de columnas: Georeferencia y valor de la variable ---- 
precip_b <- subset(precip_b, select = c("Latitud", "Longitud", "Fecha",
                                        "Nombre.del.municipio", "Concentración",
                                        "Variable"))
head(precip_b)

# Cambiar formato de fecha: Str -> Date
precip_b$Fecha <- as.Date(precip_b$Fecha, format = "%d/%m/%Y")

tabla_frecuencias <- table(precip_b$Fecha)
# Encontrar la fecha con el conteo más alto
fecha_con_maximo_conteo <- names(tabla_frecuencias)[which.max(tabla_frecuencias)]
print(fecha_con_maximo_conteo)

## Filtrado de fecha: 29 de Septiembre de 2011 ----
precip_b = precip_b[precip_b$Fecha == '2011-09-29',]

## ------------------------ Test de Mantel -----------------------------------
## Aislando los datos ----
### Latitud y longitud ----
geo = data.frame(precip_b$Longitud , precip_b$Latitud)

### Precipitacion en mm ----
con = precip_b$Concentración

## Transformación a matrices de distancias ----
### Latitud y longitud ----
d.geo = distm(geo, fun = distHaversine)
dist.geo = as.dist(d.geo)

### Concentracion ----
dist.con = dist(con, method = "euclidean")

## Test ----
### Concentración vs Geografico
con_geo = mantel(dist.con, dist.geo, method = "spearman", permutations = 9999,
                 na.rm = TRUE)

## -------------------------------- Adicionales -------------------------------
#Primero le indicamos a R que nuestros datos cargados corresponden a matrices de distancia:
matrizgeo <- as.matrix(dist.geo)
matrizcon <- as.matrix(dist.con)

#Graficamos
grafica <- plot(matrizgeo, matrizcon, pch=16, cex=1, col="black",main = "Bogotá", xlab="Distancia geográfica (m)", ylab="Disimilitud de precipitación (mm)")
grafica



