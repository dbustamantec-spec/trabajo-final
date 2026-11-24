# Se extrajeron 10 bases de datos de casos de dengue del 2015 al 2025 de
# https://portalsivigila.ins.gov.co/Paginas/Buscador.aspx#
# Cada archivo es un xlsx, y uno de ellos pesa más de 100 megas, por lo que
# GitHub no lo admite. Se intentó trabajar con .zip y .csv, pero se sigue
# superando la capacidad máxima. Por tanto, en "crudos" dentro de "datos"
# no están las bases de datos crudas porque el archivo .zip queda muy pesado,
# por esto todo este código está comentado para evitar problemas. Si se desea
# correr este código se debe ingresar a la página de sivigila y descargar las 
# bases de datos. 

#library(dplyr)
#library(lubridate)
#library(tibble)
#library(readxl)
#library(readr)
#library(here)


# Esta función escoge los datos confirmados y crea los conteos agrupando 
# por mes
#conteo_casos_dengue <- function(datos){
#  datos %>% 
#    filter(nom_est_f_caso %in% c("Confirmado por laboratorio",
#                                 "Confirmado por Nexo Epidemiológico")) %>%
#    mutate(fecha = sub(" .*", "", INI_SIN),
#           fecha = as.Date(fecha, format = "%d/%m/%Y"),
#           mes = floor_date(fecha, "month")) %>%
#    group_by(mes) %>%
#    summarise(casos_confirmados = n()) %>%
#    arrange(mes) %>% as_tibble()
#}


#ruta1 <- here("datos", "crudos", "Datos_2015_210.xlsx")
#ruta2 <- here("datos", "crudos", "Datos_2016_210.xlsx")
#ruta3 <- here("datos", "crudos", "Datos_2017_210.xlsx")
#ruta4 <- here("datos", "crudos", "Datos_2018_210.xlsx")
#ruta5 <- here("datos", "crudos", "Datos_2019_210.xlsx")
#ruta6 <- here("datos", "crudos", "Datos_2020_210.xlsx")
#ruta7 <- here("datos", "crudos", "Datos_2021_210.xlsx")
#ruta8 <- here("datos", "crudos", "Datos_2022_210.xlsx")
#ruta9 <- here("datos", "crudos", "Datos_2023_210.xlsx")
#ruta10 <- here("datos", "crudos", "Datos_2024_210.xlsx")
#ruta11 <- here("datos", "crudos", "Datos_2025_210.xlsx")

#b1 <- read_xlsx(ruta1)
#b2 <- read_xlsx(ruta2, guess_max = 100000)
#b3 <- read_xlsx(ruta3)
#b4 <- read_xlsx(ruta4)
#b5 <- read_xlsx(ruta5, guess_max = 120000)
#b6 <- read_xlsx(ruta6)
#b7 <- read_xlsx(ruta7)
#b8 <- read_xlsx(ruta8)
#b9 <- read_xlsx(ruta9, guess_max = 120000)
#b10 <- read_xlsx(ruta10, guess_max = 200000)
#b11 <- read_xlsx(ruta11, guess_max = 100000)

# Se aplica la función conteo_casos_dengue:
#d1 <- conteo_casos_dengue(b1)
#d2 <- conteo_casos_dengue(b2)
#d3 <- conteo_casos_dengue(b3)
#d4 <- conteo_casos_dengue(b4)
#d5 <- conteo_casos_dengue(b5)
#d6 <- conteo_casos_dengue(b6)
#d7 <- conteo_casos_dengue(b7)
#d8 <- conteo_casos_dengue(b8)
#d9 <- conteo_casos_dengue(b9)
#d10 <- conteo_casos_dengue(b10)
#d11 <- conteo_casos_dengue(b11)

# La mayoría de bases de datos tienen información del último mes del año
# anterior y del primer mes del próximo año, el siguiente código descarta 
# esa información, pues se supone que toda la información de casos de dengue
# que ocurrió en un año debe estar en su respectiva base de datos.

#d1 <- d1[-1, ]
#d1 <- d1[-13, ]
#d2 <- d2[-1, ]
#d2 <- d2[-13, ]
#d2 <- d2[-13, ]
#d4 <- d4[-1, ]                
#d5 <- d5[-1, ]
#d6 <- d6[-1, ]
#d6 <- d6[-13, ]
#d7 <- d7[-13, ]
#d10 <- d10[-1, ]
#d11 <- d11[-1, ]
#d11 <- d11[-13, ]


# Se contruye una unión de los anteriores datasets y se crea un archivo csv:
#base_final <- bind_rows(d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11)
#ruta <- here("datos", "procesados", "serie_dengue_2015_2025.csv")
#write_csv(base_final, ruta)
