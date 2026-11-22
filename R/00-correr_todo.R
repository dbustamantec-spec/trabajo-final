library(forecast); library(tidyverse); library(readxl)
library(ggfortify); library(patchwork); library(tibble)
library(urca); library(TSA); library(tseries); library(here)
library(lubridate)

# El siguiente código solo funciona si se corre en informe.qmd
source("../R/01-limpieza.R")
source("../R/02-tabla_comparativa_literatura.R")
source("../R/03-graficas.R")
source("../R/04-transformacion.R")
source("../R/05-diferenciacion.R")
source("../R/06-intercepto.R")
