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
setwd("C:/Users/esteb/OneDrive/Documents/Esteban/Berlin/General/MA LA Studien/Masterarbeit/")

short_years <- sprintf("%02d", 4:24)     # "04" to "24"
full_years <- 2004:2024                 # 2004 to 2024


#################################
###       Load Import         ###
#################################

###Transferencias###

Transferencias_Municipales <- read.csv("./Data/Transferencias Canon Minero/Transferencias_Municipales.csv")
Transferencias_Provinciales <- read.csv("./Data/Transferencias Canon Minero/Transferencias_Provinciales.csv")
Transferencias_Regionales <- read.csv("./Data/Transferencias Canon Minero/Transferencias_Regionales.csv")

###ENAHO###

ENAHO_100 <- read_rds("./Data/ENAHO/ENAHO_100.rds")
ENAHO_200 <- read_rds("./Data/ENAHO/ENAHO_200.rds")
ENAHO_300 <- read_rds("./Data/ENAHO/ENAHO_300.rds")
ENAHO_500 <- read_rds("./Data/ENAHO/ENAHO_500.rds")
ENAHO_sumaria <- read_rds("./Data/ENAHO/ENAHO_sumaria.rds")

###Exchange rate USD-PEN###

EX <- read_rds("./Data/Exchange Rate PEN-USD/EX.rds")

### Mapas ###

Mapa_Provincia <- read_rds("./Data/Ubigeo/Mapa_Provincia.rds")
Mapa_Distrito <- read_rds("./Data/Ubigeo/Mapa_Distrito.rds")
Mapa_Region <- read_rds("./Data/Ubigeo/Mapa_Region.rds")


Ubigeo_region <- read_rds("./Data/Ubigeo/ubigeo_departamento.rds")
Ubigeo_provincia <- read_rds("./Data/Ubigeo/ubigeo_provincia.rds")
Ubigeo_distrito <- read_rds("./Data/Ubigeo/ubigeo_distrito.rds")

### Mining Site ###

Mining_Site_path <- "./Data/Mining Site/Produccion Minera MEF"
Mining_Site_distrito <- read_xlsx(file.path(Mining_Site_path, "./Mining_Site_distrito.rds"))
