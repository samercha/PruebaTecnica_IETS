##Prueba de conocimientos para el IETS en versión en R
## Instalo paquetes
install.packages("dplyr")
install.packages("readr")
install.packages("stringr")


##Instalo librerías, en este caso, voy a generar la gestión de datos con Dplyr

library(readxl)
library(dplyr)
library(stringr)
library(readr)

##Cargo datos
Prestadores <- read_excel("C:/Users/57315/Documents/Convocatoria IETS/R/Prestadores.xlsx", sheet = "Prestadores")
Municipios<- read_excel("C:/Users/57315/Documents/Convocatoria IETS/R/Municipios.xlsx", sheet = "Sheet1")
View(Prestadores)

##Extraigo los cod_municipios del codigo de prestador de la tabla Prestadores
Prestadores <- Prestadores %>%
  mutate(cod_mpio = substr(codigo_habilitacion, 1, 5))

##Extraigo los cod_Departamento del codigo de prestador de la tabla Prestadores
Prestadores <- Prestadores %>%
  mutate(cod_Dpto = substr(codigo_habilitacion, 1, 2))
###############################################################################

##Proceso de depuración del archivo de municipios 

limpiar_y_capitalizarPal <- function(x) {
  x %>%
    str_to_lower() %>%                              ## Primero pongo todo en minusculas para facilitar la limpieza de las mayusculas dentro de las palabras
    str_replace_all("[\\%\\!\\*#\\?'\"&><]", "") %>% ## Ahora quito los caracteres especiales en las categorías
    str_squish() %>%                                ## Se procede a eliminar los espacios innecesarios
    str_to_title()                                  ## Le pongo mayúscula por palabra
}

# Aplicar a Municipio y Departamento para que limpie las categorias de ambas variables
Municipios <- Municipios %>%
  mutate(
    Municipio = limpiar_y_capitalizarPal(Municipio),
    Departamento = limpiar_y_capitalizarPal(Departamento)
  )
View(Municipios)

##Le agrego un cero delante a DP y a MPIO a Antioquia y a Atlántico
Municipios <- Municipios %>%
  mutate(
    DP = if_else(nchar(as.character(DP)) == 1, paste0("0", DP), as.character(DP)),
    MPIO = if_else(nchar(as.character(MPIO)) == 4, paste0("0", MPIO), as.character(MPIO))
  )
head(Municipios)
glimpse(Municipios)

##Join entre ambas tablas

PrestadoresJoinMunicipio <- Prestadores %>%
  left_join(Municipios, by = c("cod_mpio" = "MPIO"))
View(PrestadoresJoinMunicipio)


##Exportar el archivo con el join entre ambas tablas

write_csv(PrestadoresJoinMunicipio, 
          "C:/Users/57315/Documents/Convocatoria IETS/R/PrestadoresJoinMunicipio.csv")
readLines("C:/Users/57315/Documents/Convocatoria IETS/R/PrestadoresJoinMunicipio.csv", n = 5)

