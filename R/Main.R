library(tidyverse)
library(readxl)
library(foreign)
library(sf)
library(ggrepel)
library(mapsPERU)
library(stringi)
library(stringr)
library(stringdist)
library(writexl)
library("wesanderson")
library(dplyr)
library(purrr)
library(haven)
library(fs)
library(janitor)

# Clean up workspace
rm(list = ls())

getwd()
setwd("C:/Users/esteb/OneDrive/Documents/Esteban/Berlin/General/MA LA Studien/Masterarbeit/tesis_canon/")


# Funciones de ayuda

normalizar_texto <- function(x) {
  x %>%
    as.character() %>%
    str_squish() %>%
    str_to_upper() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    na_if("")
}

short_years <- sprintf("%02d", 4:24)     # "04" to "24"

full_years <- 2004:2024                 # 2004 to 2024


#################################
###       Load Import         ###
#################################

###Transferencias###

Transferencias_Municipales <- read_rds("./Data/Transferencias_Municipales.rds")
Transferencias_Provinciales <- read_rds("./Data/Transferencias_Provinciales.rds")
Transferencias_Regionales <- read_rds("./Data/Transferencias_Regionales.rds")

###ENAHO###

ENAHO_100 <- read_rds("./Data/ENAHO_100.rds")
ENAHO_200 <- read_rds("./Data/ENAHO_200.rds")
ENAHO_300 <- read_rds("./Data/ENAHO_300.rds")
ENAHO_500 <- read_rds("./Data/ENAHO_500.rds")
ENAHO_sumaria <- read_rds("./Data/ENAHO_sumaria.rds")

###Exchange rate USD-PEN###

EX <- read_rds("./Data/EX.rds")

### Mapas ###

Mapa_Provincia <- read_rds("./Data/Mapa_Provincia.rds")
Mapa_Distrito <- read_rds("./Data/Mapa_Distrito.rds")
Mapa_Region <- read_rds("./Data/Mapa_Region.rds")


Ubigeo_Region <- read_rds("./Data/ubigeo_departamento.rds")
Ubigeo_Provincia <- read_rds("./Data/ubigeo_provincia.rds")
Ubigeo_Distrito <- read_rds("./Data/ubigeo_distrito.rds")

### Mining Site with Prices ###

Mining_Site_long <- read_rds("./Data/Mining_Site_long.rds")


###############################
###       Join Data         ###
###############################

