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


# Función para estandarizar textos

normalizar_texto <- function(x) {
  x %>%
    as.character() %>%
    str_squish() %>%
    str_to_upper() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    na_if("")
}

# Load mapas con nombres e ubigeo
Mapa_Provincia <- map_PROV
Mapa_Distrito <- map_DIST
Mapa_Region <- map_REG #Diferencia entre Lima Provincias y Lima Metropolitana


Ubigeo_region <- read_rds("./Data/Ubigeo/ubigeo_departamento.rds")
Ubigeo_provincia <- read_rds("./Data/Ubigeo/ubigeo_provincia.rds")
Ubigeo_distrito <- read_rds("./Data/Ubigeo/ubigeo_distrito.rds")

################ Transferencias Canon Minero ################

###Load Data###

##Regional##

short_years <- sprintf("%02d", 4:24)     # "04" to "24"
full_years <- 2004:2024                 # 2004 to 2024

regional_transfer_path <- "./Data/Transferencias Canon Minero/Gobiernos Regionales/"

# Loop through each year
for (i in seq_along(full_years)) {
  #year_full <- full_years[i]
  #year_short <- short_years[i]
  
  # Nombre del documento usando paste0 [concatenar] y leer archivo con el nombre correspondiente
  file_name <- paste0(regional_transfer_path, full_years[i], " Gobiernos Regionales.xls")
  df <- read_xls(file_name)
  
  #Asignar nombres para cada data frame y añadirla columna de año
  names(df)[1:4] <- c("code", "name", "authorised", "credited")
  df <- cbind(df[, 1:2], year = full_years[i], df[, 3:ncol(df)])
  df[["authorised"]] <- as.numeric(gsub("[^0-9.-]", "", df[["authorised"]])) / 1000000
  df[["credited"]]   <- as.numeric(gsub("[^0-9.-]", "", df[["credited"]])) / 1000000
  
  # Asignar nuevo nombre al data frame
  assign(paste0("Transferencias_Regionales_", short_years[i]), df)
}


##Limpiar filas
Transferencias_Regionales_04 <- Transferencias_Regionales_04[-c(1:16), ]   
Transferencias_Regionales_05 <- Transferencias_Regionales_05[-c(1:11), ]   
Transferencias_Regionales_06 <- Transferencias_Regionales_06[-c(1:11), ]   
Transferencias_Regionales_07 <- Transferencias_Regionales_07[-c(1:12), ]   
Transferencias_Regionales_08 <- Transferencias_Regionales_08[-c(1:11), ]   
Transferencias_Regionales_09 <- Transferencias_Regionales_09[-c(1:11), ]   
Transferencias_Regionales_10 <- Transferencias_Regionales_10[-c(1:11), ]   
Transferencias_Regionales_11 <- Transferencias_Regionales_11[-c(1:11), ]   
Transferencias_Regionales_12 <- Transferencias_Regionales_12[-c(1:11), ]   
Transferencias_Regionales_13 <- Transferencias_Regionales_13[-c(1:11), ]   
Transferencias_Regionales_14 <- Transferencias_Regionales_14[-c(1:11), ]
Transferencias_Regionales_15 <- Transferencias_Regionales_15[-c(1:11), ]
Transferencias_Regionales_16 <- Transferencias_Regionales_16[-c(1:11), ]
Transferencias_Regionales_17 <- Transferencias_Regionales_17[-c(1:14), ]
Transferencias_Regionales_18 <- Transferencias_Regionales_18[-c(1:11), ]
Transferencias_Regionales_19 <- Transferencias_Regionales_19[-c(1:11), ]
Transferencias_Regionales_20 <- Transferencias_Regionales_20[-c(1:11), ]
Transferencias_Regionales_21 <- Transferencias_Regionales_21[-c(1:11), ]
Transferencias_Regionales_22 <- Transferencias_Regionales_22[-c(1:11), ]
Transferencias_Regionales_23 <- Transferencias_Regionales_23[-c(1:11), ]
Transferencias_Regionales_24 <- Transferencias_Regionales_24[-c(1:11), ]

##Bind all df in one
Transferencias_Regionales <- do.call(rbind, lapply(short_years, function(y) {
  get(paste0("Transferencias_Regionales_", y))
}))

##Remove Transferencias_Regionales_XX
df_remove <- paste0("Transferencias_Regionales_", sprintf("%02d", 4:24))
rm(list = df_remove)

summary(Transferencias_Regionales)

##Municipal##

municipal_transfer_path <- "./Data/Transferencias Canon Minero/Gobiernos Locales/"

# Loop through each year
for (i in seq_along(full_years)) {
  #year_full <- full_years[i]
  #year_short <- short_years[i]
  
  # Nombre del documento usando paste0 [concatenar] y leer archivo con el nombre correspondiente
  file_name <- paste0(municipal_transfer_path, full_years[i], " Gobiernos Locales.xls")
  df <- read_xls(file_name)
  
  #Asignar nombres para cada data frame y añadirla columna de año
  names(df)[1:4] <- c("code", "name", "authorised", "credited")
  df <- cbind(df[, 1:2], year = full_years[i], df[, 3:ncol(df)])
  df[["authorised"]] <- as.numeric(gsub("[^0-9.-]", "", df[["authorised"]])) / 1000000
  df[["credited"]]   <- as.numeric(gsub("[^0-9.-]", "", df[["credited"]])) / 1000000
  
  # Asignar nuevo nombre al data frame
  assign(paste0("Transferencias_Municipales_", short_years[i]), df)
}


##Limpiar filas
for (y in sprintf("%02d", 4:24)) assign(paste0("Transferencias_Municipales_", y),
                                        get(paste0("Transferencias_Municipales_", y))[-c(1:11), ])

##Bind all df in one
Transferencias_Municipales <- do.call(rbind, lapply(short_years, function(y) {
  get(paste0("Transferencias_Municipales_", y))
}))

##Remove Transferencias_Regionales_XX
df_remove <- paste0("Transferencias_Municipales_", sprintf("%02d", 4:24))
rm(list = df_remove)

summary(Transferencias_Municipales)

###Cleaning###

Transferencias_Regionales <- Transferencias_Regionales %>%
  mutate(name = str_remove(name, "^GOBIERNO REGIONAL "),
         name = str_remove(name, "^DEL DEPARTAMENTO DE "),
         name = str_remove(name, "^DE LA PROVINCIA CONSTITUCIONAL DEL "),
         name = str_replace(name, "^MUNICIPALIDAD METROPOLITANA DE LIMA$", "LIMA METROPOLITANA"),
         name = str_replace(name, "^LIMA$", "LIMA PROVINCIAS"))

Transferencias_Municipales <-  Transferencias_Municipales %>% 
  mutate(
    name = str_replace(name, "^MUNICIPALIDAD PROVINCIAL DE LIMA$", "LIMA"),
    name = str_replace(name, "^MUN. PRO. DE LIMA$", "LIMA"),
    name = str_replace(name, "^MUNICIPALIDAD METROPOLITANA DE LIMA$", "LIMA")
    ) #Inconsistent naming for Lima Metropolitana, although all share the same
      #code with "MUNICIPALIDAD METROPOLITANA DE LIMA". For this reason, the
      #name is unified.


Transferencias_Municipales <- Transferencias_Municipales %>%
  mutate(
    province = if_else(
      str_detect(name, "^MUN\\. PRO\\.|^MUNICIPALIDAD PROVINCIAL|^LIMA$"),
      name,
      NA_character_
    )
  ) %>%
  fill(province)  #Para identificar las municipales por provincia se arrastra la 
                  #línea de provincia hasta la siguiente provincia. Están 
                  #organizadas en orden. Igualmente se comprueba con el dataset
                  #de mapa para verificar luego de limpiar nombres.


Transferencias_Municipales <- Transferencias_Municipales %>%
  mutate(
    province = case_when(
      province == "MUNICIPALIDAD PROVINCIAL DE DATEM DEL MARAÑON" & name %in% c(
        "MUNICIPALIDAD DISTRITAL DE JEBEROS",
        "MUNICIPALIDAD DISTRITAL DE LAGUNAS",
        "MUNICIPALIDAD DISTRITAL DE SANTA CRUZ",
        "MUNICIPALIDAD DISTRITAL DE TENIENTE CESAR LOPEZ ROJAS"
      ) ~ "MUNICIPALIDAD PROVINCIAL DEL ALTO AMAZONAS - YURIMAGUAS",
      TRUE ~ province
    )
  ) #Existen algunas observaciones que están marcadas como DATEM DEL MANON
    #cuando pertenecen a ALTO AMAZONAS


Transferencias_Municipales <- Transferencias_Municipales %>%
  filter(grepl("MUN\\. PRO\\.|^MUNICIPALIDAD PROVINCIAL |^LIMA$", province)) %>% 
  mutate(
    province = str_remove(province, "^MUN\\. PRO\\. DE "),
    province = str_remove(province, "^MUN\\. PRO\\. DEL "),
    province = str_remove(province, "^MUN\\. PRO\\. "),
    province = str_remove(province, "^MUNICIPALIDAD PROVINCIAL DEL "),
    province = str_remove(province, "^MUNICIPALIDAD PROVINCIAL DE "),
    province = str_remove(province, "^MUNICIPALIDAD PROVINCIAL "),
    province = str_remove(province, "\\s*-.*")
  ) #Limpiar nombres para poder hacer comparación con Mapa_Distrito


Transferencias_Provinciales <- Transferencias_Municipales %>%
  select(-code, -name) %>%
  group_by(province, year) %>%
  summarise(
    authorised = sum(authorised, na.rm = TRUE),
    credited   = sum(credited, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(name = province)



Transferencias_Municipales <- Transferencias_Municipales %>% 
  mutate(
    name = str_remove(name, "^MUN\\. DIS\\. DE "),
    name = str_remove(name, "^MUN\\. PRO\\. DE "),
    name = str_remove(name, "^MUN\\. PRO\\. DEL "),
    name = str_remove(name, "^MUN\\. PRO\\. "),
    name = str_remove(name, "^MUNICIPALIDAD PROVINCIAL DEL "),
    name = str_remove(name, "^MUNICIPALIDAD PROVINCIAL DE "),
    name = str_remove(name, "^MUNICIPALIDAD PROVINCIAL "),
    name = str_remove(name, "^MUN\\. DIS\\. "),
    name = str_remove(name, "^MUN\\. DIST\\. "),
    name = str_remove(name, "^MUNICIPALIDAD DISTRITAL DE "),
    name = str_remove(name, "^MUNICIPALIDAD DISTRITAL "),
    name = str_remove(name, "^ DE "),
    name = str_remove(name, "^ "),
    name = str_remove(name, "\\s*-.*")
  )

###Validación nombres de provincias en distritos que recibieron transferencia###

Transferencias_Municipales <- Transferencias_Municipales %>%
  mutate(
    province = str_squish(stri_trans_general(str_to_upper(province), "Latin-ASCII")),
    name     = str_squish(stri_trans_general(str_to_upper(name), "Latin-ASCII"))
  )

Mapa_Distrito <- Mapa_Distrito %>%
  mutate(
    PROVINCIA = str_squish(stri_trans_general(str_to_upper(PROVINCIA), "Latin-ASCII")),
    DISTRITO  = str_squish(stri_trans_general(str_to_upper(DISTRITO), "Latin-ASCII"))
  )

Transferencias_Provinciales <- Transferencias_Provinciales %>% 
  mutate(name = case_when(
  name == "NAZCA" ~ "NASCA",
  name == "CAÑETE" ~ "CANETE",
  name == "SANCHEZ CERRO" ~ "GENERAL SANCHEZ CERRO",
  name == "DANIEL CARRION" ~ "DANIEL ALCIDES CARRION",
  name == "CUZCO" ~ "CUSCO",
  name == "QUISPICANCHIS" ~ "QUISPICANCHI",
  name == "MARAÑON" ~ "MARANON",
  name == "FERREÑAFE" ~ "FERRENAFE",
  name == "DANIEL A. CARRION" ~ "DANIEL ALCIDES CARRION",
  name == "DATEM DEL MARAÑON" ~ "DATEM DEL MARANON",
  TRUE ~ name
  ))


Transferencias_Municipales <- Transferencias_Municipales %>% 
  mutate(name = case_when(
    name == "NAZCA" ~ "NASCA",
    name == "CAÑETE" ~ "CANETE",
    name == "SANCHEZ CERRO" ~ "GENERAL SANCHEZ CERRO",
    name == "DANIEL CARRION" ~ "DANIEL ALCIDES CARRION",
    name == "CUZCO" ~ "CUSCO",
    name == "QUISPICANCHIS" ~ "QUISPICANCHI",
    name == "MARAÑON" ~ "MARANON",
    name == "FERREÑAFE" ~ "FERRENAFE",
    name == "DANIEL A. CARRION" ~ "DANIEL ALCIDES CARRION",
    name == "DATEM DEL MARAÑON" ~ "DATEM DEL MARANON",
    name == "SAN ISIDRO DE MAYNO" ~ "SAN ISIDRO DE MAINO",
    name == "IMASA" ~ "IMAZA",
    name == "PIZUQUIA" ~ "PISUQUIA",
    name == "SAN CRISTOBAL DE OLTO" ~ "SAN CRISTOBAL",
    name == "SAN JERONIMO DE PACLAS" ~ "SAN JERONIMO",
    name == "ABELARDO PARDO LAZAMETA" ~ "ABELARDO PARDO LEZAMETA",
    name == "MATACOTA" ~ "MATACOTO",
    name == "SAN MIGUEL CHACCRAMPA" ~ "SAN MIGUEL DE CHACCRAMPA",
    name == "HUAYLLO" ~ "HUAYO",
    name == "QUEQUE¥A" ~ "QUEQUENA",
    name == "HUAC" ~ "HUAC HUAS",
    name == "SAN FRANCISCO DE RAVACAYCO" ~ "SAN FRANCISCO DE RIVACAYCO",
    name == "HUAYA" ~ "HUALLA",
    name == "LOS BA¥OS DEL INCA" ~ "LOS BANOS DEL INCA",
    name == "ACOSTAMBAO" ~ "ACOSTAMBO",
    name == "DANIEL HERNANDES" ~ "DANIEL HERNANDEZ",
    name == "YAHUIMPUQUIO" ~ "NAHUIMPUQUIO",
    name == "SAN MARCOS ROCCHAC" ~ "SAN MARCOS DE ROCCHAC",
    name == "TINGUINA" ~ "LA TINGUINA",
    name == "LEONOR ORDO¥EZ" ~ "LEONOR ORDONEZ",
    name == "PAMPA HERMOZA" ~ "PAMPA HERMOSA",
    name == "HUAY" ~ "HUAY HUAY",
    name == "AYAUCA" ~ "ALLAUCA",
    name == "VILCHAYAL" ~ "VICHAYAL",
    name == "CAPASO" ~ "CAPAZO",
    name == "HUANAHUARA" ~ "HUANUARA",
    name == "CHUCATAMANI" ~ "HEROES ALBARRACIN",
    name == "QUIMBIRI" ~ "KIMBIRI",
    name == "CRESPO Y CASTILLO" ~ "JOSE CRESPO Y CASTILLO",
    name == "CODO DE POZUZO" ~ "CODO DEL POZUZO",
    name == "CHUPAN" ~ "APARICIO POMARES",
    name == "SAN FRANCISCO DEL YESO" ~ "SAN FRANCISCO DE YESO",
    name == "MACHAHUAY" ~ "MACHAGUAY",
    name == "PACAPAUZA" ~ "PACAPAUSA",
    name == "CCORCCA" ~ "CCORCA",
    name == "PACCARECTAMBO" ~ "PACCARICTAMBO",
    name == "KCOSNIPATA" ~ "KOSNIPATA",
    name == "KICHKI" ~ "QUISQUI",
    name == "ZANA" ~ "SANA",
    name == "KANARIS" ~ "CANARIS",
    name == "FUNDICION DE TINYAHUARCO" ~ "TINYAHUARCO",
    name == "HUATTA" ~ "HUATA",
    name == "SAN JOSE DE LOS CHORRILLOS" ~ "CUENCA",
    name == "TAPAYRIHUA" ~ "TAPAIRIHUA",
    name == "PACAICASA" ~ "PACAYCASA",
    name == "ECHARATI" ~ "ECHARATE",
    name == "ANDRES AVELINO CACERES DORREGAY" ~ "ANDRES AVELINO CACERES DORREGARAY",
    name == "EL ARENAL" ~ "ARENAL",
    name == "JALCA GRANDE" ~ "LA JALCA",
    name == "CASTA" ~ "SAN PEDRO DE CASTA",
    name == "SAN PEDRO DE LARAOS" ~ "LARAOS",
    name == "PEVAS" ~ "PEBAS",
    name == "PACCARICTAMBO" ~ "PACCARITAMBO",
    name == "NAZCA" ~ "NASCA",
    name == "CAÑETE" ~ "CANETE",
    name == "SANCHEZ CERRO" ~ "GENERAL SANCHEZ CERRO",
    name == "DANIEL CARRION" ~ "DANIEL ALCIDES CARRION",
    name == "CUZCO" ~ "CUSCO",
    name == "QUISPICANCHIS" ~ "QUISPICANCHI",
    name == "MARAÑON" ~ "MARANON",
    name == "FERREÑAFE" ~ "FERRENAFE",
    name == "DANIEL A. CARRION" ~ "DANIEL ALCIDES CARRION",
    name == "DATEM DEL MARAÑON" ~ "DATEM DEL MARANON",
    name == "LIMA METROPOLITANA" ~ "LIMA",
    TRUE ~ name
  ))


Transferencias_Municipales <- Transferencias_Municipales %>% 
  mutate(province = case_when(
    province == "CASCAS" ~ "GRAN CHIMU",
    province == "HUACHO" ~ "HUAURA",
    province == "SAN JOSE DE SISA" ~ "EL DORADO",
    province == "CALLAO" ~ "PROV. CONST. DEL CALLAO",
    TRUE ~ province
  ))


Transferencias_Municipales <- Transferencias_Municipales %>%
  mutate(
    province_exists = province %in% unique(Mapa_Distrito$PROVINCIA)
  )

any(!Transferencias_Municipales$province_exists) #Ahora todas las municipalidades coinciden con su provincia

Transferencias_Municipales <- Transferencias_Municipales %>%
  select(-province_exists)

###Export data###
write.csv(Transferencias_Municipales, "./Data/Transferencias Canon Minero/Transferencias_Municipales.csv",
          row.names = FALSE)

write.csv(Transferencias_Provinciales, "./Data/Transferencias Canon Minero/Transferencias_Provinciales.csv",
          row.names = FALSE)

write.csv(Transferencias_Regionales, "./Data/Transferencias Canon Minero/Transferencias_Regionales.csv",
          row.names = FALSE)

###Visualizing###

##Evolution year by year##

Regional_by_year <- Transferencias_Regionales %>%
  group_by(year) %>%
  summarise(
    total_authorised = sum(authorised, na.rm = TRUE),
    total_credited   = sum(credited, na.rm = TRUE)
  )

summary(Regional_by_year)


Provincial_by_year <- Transferencias_Provinciales %>% 
  group_by(year) %>% 
  summarise(
    total_authorised = sum(authorised, na.rm = TRUE),
    total_credited = sum(credited, na.rm = TRUE)
  )

summary(Provincial_by_year)

Municipal_by_year <- Transferencias_Municipales %>% 
  group_by(year) %>% 
  summarise(
    total_authorised = sum(authorised, na.rm = TRUE),
    total_credited = sum(credited, na.rm = TRUE)
  )

summary(Municipal_by_year)

#### However check for differences between credited and authorised ####

any((Transferencias_Municipales$authorised - Transferencias_Municipales$credited) != 0)

Transferencias_Municipales %>%
  mutate(diff = authorised - credited) %>%
  filter(diff != 0) %>%
  nrow()

Transferencias_Municipales %>%
  mutate(diff = authorised - credited) %>%
  filter(diff != 0) %>%
  arrange(desc(abs(diff)))

#Difrencias mínimas en caso de región solo un caso (el de Puno que recibió
#mayores transeferencias de lo esperado). Probablemente no sea necesario.
#Además, cuando se mira el Year_by_Year los quantiles son los mismos en todos
#los casos.
######
  
Transfers_by_year <- bind_rows(
  Regional_by_year %>% 
    select(year, total_credited = total_credited) %>% 
    mutate(type = "Regional"),
  Municipal_by_year %>% 
    select(year, total_credited = total_credited) %>% 
    mutate(type = "Municipal")
)
  
ggplot(
  Transfers_by_year, aes(x = year, y = total_credited, color = type)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Canon Minero Transfers by Year",
    subtitle = "Regional and Municipal Comparison",
    x = "Year",
    y = "Transfer (mm.)",
    color = "Type"
  ) +
  scale_x_continuous(breaks = unique(Transfers_by_year$year)) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


##Disparities##

##Regional##

ggplot(
  Transferencias_Regionales,
  aes(x = year, y = credited, group = name)
) +
  geom_point() +
  labs(
    title = "Transfer Disparities by Region",
    x = "Year",
    y = "Log(Amount Credited)"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  ) + 
  theme(legend.position = "none")


Transferencias_Regionales %>%
  group_by(year) %>%
  summarise(
    total = n(),
    over_50 = sum(credited > 50)
  )  #Just a very few Regions receive more than 50 million!
                        

##Municipal##

ggplot(
  Transferencias_Municipales,
  aes(x = year, y = credited, group = name)
) +
  geom_point() +
  labs(
    title = "Transfer Disparities by Municipal District",
    x = "Year",
    y = "Amount (mm.)"
  ) +
  scale_x_continuous(breaks = unique(Transferencias_Municipales$year)) +
 theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  ) + 
  theme(legend.position = "none")

Transferencias_Municipales %>%
  group_by(year) %>%
  summarise(
    total = n(),
    over_50 = sum(credited > 50)
  ) #Just a very few Regions receive more than 50 million!
                    

################### Mapa de Peru ###################

###Departamento###

Mapa_Region <- st_as_sf(Mapa_Region)
Regiones_Mineras <- unique(Transferencias_Regionales$name)

#Para poder comparar con Transferencias_Regionales poner en mayúscula y quitar
#tildes y otros símbolos

Mapa_Region <- Mapa_Region %>%
  mutate(REGION_UPPER = toupper(REGION))

Mapa_Region <- Mapa_Region %>%
  mutate(
    REGION_UPPER = normalizar_texto(REGION_UPPER),
    Mineria = ifelse(REGION_UPPER %in% Regiones_Mineras, "Canon Region", "Non-Canon Region")
  )

ggplot(Mapa_Region) +
  geom_sf(aes(fill = Mineria), color = "white") +
  geom_sf(data = Mapa_Region, fill = NA, color = "black", linewidth = 0.3) +
  scale_fill_manual(values = wes_palette("AsteroidCity3", 2)) +
  geom_text_repel(
    data = Mapa_Region,
    aes(x = coords_x, y = coords_y, label = REGION_UPPER),
    size = 2.5,
    fontface = "bold",
    max.overlaps = 20,
    box.padding = 0.4,
    point.padding = 0.2,
    segment.color = "gray60"
  ) +
  labs(
    title = "Canon vs. Non-Canon Regions in Peru",
    fill = ""
  ) +
  theme_minimal()

setdiff(Mapa_Region$REGION_UPPER, Regiones_Mineras) #No existen departamentos que no reciban Canon!

#Saving#

saveRDS(Mapa_Region, "./Data/Ubigeo/Mapa_Region.rds")
write.csv(Mapa_Region, "./Data/Ubigeo/Mapa_Region.cvs", row.names = FALSE)

###Provincial###

Mapa_Provincia <- st_as_sf(Mapa_Provincia)

#Para poder comparar con Transferencias_Regionales poner en mayúscula y quitar
#tildes y otros símbolos

Mapa_Provincia <- Mapa_Provincia %>%
  mutate(PROVINCIA_UPPER = toupper(PROVINCIA),
         PROVINCIA_UPPER = case_when(
           PROVINCIA_UPPER == "PROV. CONST. DEL CALLAO" ~ "CALLAO",
           TRUE ~ PROVINCIA_UPPER))

Mapa_Provincia <- Mapa_Provincia %>%
  mutate(
    PROVINCIA_UPPER = stri_trans_general(PROVINCIA_UPPER, "Latin-ASCII"))

#Para realizar el mapa, armonizar los nombres de provincias
setdiff(Transferencias_Provinciales$name, Mapa_Provincia$PROVINCIA_UPPER)

Transferencias_Provinciales <- Transferencias_Provinciales %>% 
  mutate(name = case_when(
    name == "NAZCA" ~ "NASCA",
    name == "CAÑETE" ~ "CANETE",
    name == "SANCHEZ CERRO" ~ "GENERAL SANCHEZ CERRO",
    name == "DANIEL CARRION" ~ "DANIEL ALCIDES CARRION",
    name == "CUZCO" ~ "CUSCO",
    name == "QUISPICANCHIS" ~ "QUISPICANCHI",
    name == "MARAÑON" ~ "MARANON",
    name == "FERREÑAFE" ~ "FERRENAFE",
    name == "DANIEL A. CARRION" ~ "DANIEL ALCIDES CARRION",
    name == "DATEM DEL MARAÑON" ~ "DATEM DEL MARANON",
    TRUE ~ name
  ))

Transferencias_Provinciales <- Transferencias_Provinciales %>% 
  mutate(name = case_when(
    name == "CASCAS" ~ "GRAN CHIMU",
    name == "HUACHO" ~ "HUAURA",
    name == "SAN JOSE DE SISA" ~ "EL DORADO",
    TRUE ~ name
  ))


df <- setdiff(Transferencias_Provinciales$name, Mapa_Provincia$PROVINCIA_UPPER)

Provincias_Mineras <- unique(Transferencias_Provinciales$name)

Mapa_Provincia <- Mapa_Provincia %>%
  mutate(
    Mineria = ifelse(PROVINCIA_UPPER %in% Provincias_Mineras, "Canon Province", "Non-Canon Province")
  )

ggplot(Mapa_Provincia) +
  geom_sf(aes(fill = Mineria), color = "white") +
  geom_sf(data = Mapa_Provincia, fill = NA, color = "black", linewidth = 0.3) +
  scale_fill_manual(values = wes_palette("AsteroidCity3", 2)) +
  labs(
    title = "Canon vs. Non-Canon Provinces in Peru",
    fill = ""
  ) +
  theme_minimal()

setdiff(Mapa_Provincia$PROVINCIA_UPPER, Provincias_Mineras) #No existen provincias que no reciban Canon!

#Saving#

saveRDS(Mapa_Provincia, "./Data/Ubigeo/Mapa_Provincia.rds")
write.csv(Mapa_Provincia, "./Data/Ubigeo/Mapa_Provincia.cvs", row.names = FALSE)

###Distrito###

Mapa_Distrito <- st_as_sf(Mapa_Distrito)

Mapa_Distrito <- Mapa_Distrito %>% 
  mutate(
    DISTRITO = str_replace_all(DISTRITO, "_", " "),
    DISTRITO = str_replace_all(DISTRITO, "-", " ")
    )

Mapa_Distrito <- Mapa_Distrito %>%
  mutate(DISTRITO_UPPER = toupper(DISTRITO))

Mapa_Distrito <- Mapa_Distrito %>%
  mutate(
    DISTRITO_UPPER = str_remove(DISTRITO_UPPER, "\\s*\\([^\\)]+\\)"),
    DISTRITO_UPPER = stri_trans_general(DISTRITO_UPPER, "Latin-ASCII"),
  )
    

Transferencias_Municipales <- Transferencias_Municipales %>%
  mutate(
    name = str_remove(name, "\\s*\\([^\\)]+\\)"),
    name = stri_trans_general(name, "Latin-ASCII"),
  )

setdiff(Transferencias_Municipales$name, Mapa_Distrito$DISTRITO_UPPER)


########cambiar nombres de región por nombre de capitales
########se tomó la info antes del "- " en la sección anterior


Transferencias_Municipales <- Transferencias_Municipales %>% 
  mutate(name = case_when(
    name == "BONGARA" ~ "JUMBILLA",
    name == "CONDORCANQUI" ~ "NIEVA",
    name == "RODRIGUEZ DE MENDOZA" ~ "SAN NICOLAS",
    name == "UTCUBAMBA" ~ "BAGUA GRANDE",
    name == "CARLOS FERMIN FITZCARRALD" ~ "SAN LUIS",
    name == "MARISCAL LUZURIAGA" ~ "PISCOBAMBA",
    name == "AYMARAES" ~ "CHALHUANCA",
    name == "GRAU" ~ "CHUQUIBAMBILLA",
    name == "CONDESUYOS" ~ "CHUQUIBAMBA",
    name == "HUAMANGA" ~ "AYACUCHO",
    name == "HUANCA SANCOS" ~ "SANCOS",
    name == "LA MAR" ~ "SAN MIGUEL",
    name == "PARINACOCHAS" ~ "CORACORA",
    name == "PAUCAR DEL SARA SARA" ~ "PAUSA",
    name == "VICTOR FAJARDO" ~ "HUANCAPI",
    name == "ANGARAES" ~ "LIRCAY",
    name == "TAYACAJA" ~ "PAMPAS",
    name == "CHINCHA" ~ "CHINCHA ALTA",
    name == "SANCHEZ CARRION" ~ "HUAMACHUCO",
    name == "CANETE" ~ "SAN VICENTE DE CANETE",
    name == "MARISCAL NIETO" ~ "MOQUEGUA",
    name == "GENERAL SANCHEZ CERRO" ~ "OMATE",
    name == "PASCO" ~ "CHAUPIMARCA",
    name == "DANIEL ALCIDES CARRION" ~ "YANAHUANCA",
    name == "TALARA" ~ "PARINAS",
    name == "CARABAYA" ~ "MACUSANI",
    name == "EL COLLAO" ~ "ILAVE",
    name == "MELGAR" ~ "AYAVIRI",
    name == "SAN ANTONIO DE PUTINA" ~ "PUTINA",
    name == "SAN ROMAN" ~ "JULIACA",
    name == "JORGE BASADRE" ~ "LOCUMBA",
    name == "CANAS" ~ "YANAOCA",
    name == "CHUMBIVILCAS" ~ "SANTO TOMAS",
    name == "CANCHIS" ~ "URCOS",
    name == "LA CONVENCION" ~ "SANTA ANA",
    name == "QUISPICANCHI" ~ "URCOS",
    name == "DOS DE MAYO" ~ "LA UNION",
    name == "HUAMALIES" ~ "LLATA",
    name == "PACHITEA" ~ "PANAO",
    name == "MARANON" ~ "HUACRACHUCO",
    name == "LAURICOCHA" ~ "JESUS",
    name == "YAROWILCA" ~ "CHAVINILLO",
    name == "PACCARICTAMBO" ~ "PACCARITAMBO",
    name == "GRAN CHIMU" ~ "CASCAS",
    name == "EL DORADO" ~ "SAN JOSE DE SISA",
    name == "CONTRALMIRANTE VILLAR" ~ "ZORRITOS",
    name == "MAYNAS" ~ "IQUITOS",
    name == "ALTO AMAZONAS" ~ "YURIMAGUAS",
    name == "DATEM DEL MARANON" ~ "BARRANCA",
    name == "LORETO" ~ "NAUTA",
    name == "MARISCAL RAMON CASTILLA" ~ "RAMON CASTILLA",
    name == "UCAYALI" ~ "CONTAMANA",
    name == "CORONEL PORTILLO" ~ "CALLERIA",
    name == "ATALAYA" ~ "RAYMONDI",
    TRUE ~ name
  ))

setdiff(Transferencias_Municipales$name, Mapa_Distrito$DISTRITO_UPPER)

Distritos_Mineros <- unique(Transferencias_Municipales$name)

Mapa_Distrito <- Mapa_Distrito %>%
  mutate(
    Mineria = ifelse(DISTRITO_UPPER %in% Distritos_Mineros, "Canon District", "Non-Canon District"))

ggplot(Mapa_Distrito) +
  geom_sf(aes(fill = Mineria), color = "white") +
  geom_sf(data = Mapa_Distrito, fill = NA, color = "black", linewidth = 0.3) +
  scale_fill_manual(values = wes_palette("AsteroidCity3", 2)) +
  labs(
    title = "Canon vs. Non-Canon Municipalities in Peru",
    fill = ""
  ) +
  theme_minimal()


length(setdiff(Mapa_Distrito$DISTRITO_UPPER, Distritos_Mineros)) #Existen 27 distritos no mineros

Distritos_Full <- Transferencias_Municipales %>% 
  group_by(name) %>% 
  summarise(presente = n_distinct(year)) %>% 
  filter(presente == length(full_years))

#Al observar cuántos distritos aparecen siempre durante el periodo 2004-2023
#el grupo de comparación aumenta a 419 observaciones.
length(setdiff(Mapa_Distrito$DISTRITO_UPPER, Distritos_Full$name))


#Observar grupo de control por año:
Control_by_year <- sapply(full_years, function(t){
  Municipal_by_year <- Transferencias_Municipales %>%
    filter(year == t) %>%
    pull(name) %>%
    unique()
  length(setdiff(Mapa_Distrito$DISTRITO_UPPER, Municipal_by_year))
})

tibble(
  year = full_years,
  control = Control_by_year
)

#Saving#

saveRDS(Mapa_Distrito, "./Data/Ubigeo/Mapa_Distrito.rds")
write.csv(Mapa_Distrito, "./Data/Ubigeo/Mapa_Distrito.cvs", row.names = FALSE)


######################### ENAHO #########################


###Load Data###

ENAHO_path = "./Data/ENAHO/"


ENAHO_completo <- list()

for (i in seq_along(full_years)) {
  
  year <- as.character(full_years[i])
  work_path <- file.path(ENAHO_path, year)
  
  print(paste("Procesando año:", year))
  
  all_files <- list.files(work_path, full.names = TRUE, recursive = TRUE)
  
  by_year <- list()
  
  for (file in all_files) {
    
    name <- basename(file)
    
    # leer .dta
    if (grepl("\\.dta$", file, ignore.case = TRUE)) {
      print(paste("Reading .dta:", name))
      
      df <- tryCatch(
        haven::read_dta(file),
        error = function(e) {
          message(paste("❌ Error leyendo", name, ":", e$message))
          return(NULL)
        }
      )
      
      if (!is.null(df)) {
        by_year[[name]] <- as.data.frame(df)
      }
    }
    
    # leer .dbf
    else if (grepl("\\.dbf$", file, ignore.case = TRUE)) {
      print(paste("Reading .dbf:", name))
      
      df <- tryCatch(
        foreign::read.dbf(file, as.is = TRUE),
        error = function(e) {
          message(paste("❌ Error leyendo", name, ":", e$message))
          return(NULL)
        }
      )
      
      if (!is.null(df)) {
        by_year[[name]] <- as.data.frame(df)
      }
    }
    
    # leer .csv
    else if (grepl("\\.csv$", file, ignore.case = TRUE)) {
      print(paste("Reading .csv:", name))
      
      df <- tryCatch(
        read.csv(file, stringsAsFactors = FALSE, fileEncoding = "latin1"),
        error = function(e) {
          message(paste("❌ Error leyendo", name, ":", e$message))
          return(NULL)
        }
      )
      
      if (!is.null(df)) {
        by_year[[name]] <- as.data.frame(df)
      }
    }
  }
  
  ENAHO_completo[[year]] <- by_year
}


rm(by_year)


################# Datasets by module #################


################
## Module 100 ##
################

vars_100 <- c(
  "AÑO", "CONGLOME", "VIVIENDA", "HOGAR", "UBIGEO",
  "ESTRATO", "RESULT", "P101", "P106A", "P110",
  "P111", "P111A",
  "NBI1", "NBI2", "NBI3", "NBI4", "NBI5",
  "nbi1", "nbi2", "nbi3", "nbi4", "nbi5"
)

vars_chr <- c("AÑO", "CONGLOME", "VIVIENDA", "HOGAR", "UBIGEO")
vars_num <- c("ESTRATO", "RESULT", "P101", "P106A", "P110", 
              "P111", "P111A",
              "NBI1", "NBI2", "NBI3", "NBI4", "NBI5",
              "nbi1", "nbi2", "nbi3", "nbi4", "nbi5")

extraer_modulo_100 <- function(year_name, year_list, vars_keep) {
  
  file_100_name <- names(year_list)[
    str_detect(names(year_list), regex("(^|[-_])100(\\(|\\.|[-_]|$)", ignore_case = TRUE))
  ][1]
  
  if (is.na(file_100_name)) {
    warning(paste("No se encontró módulo 100 para el año", year_name))
    return(NULL)
  }
  
  df <- as.data.frame(year_list[[file_100_name]])
  
  vars_available <- intersect(vars_keep, names(df))
  
  if (length(vars_available) == 0) {
    warning(paste("Ninguna variable objetivo encontrada en", year_name))
    return(NULL)
  }
  
  df_out <- df %>%
    select(any_of(vars_keep))
  
  if (!"AÑO" %in% names(df_out)) {
    df_out <- df_out %>%
      mutate(AÑO = as.character(year_name), .before = 1)
  }
  
  # Forzar tipos homogéneos
  df_out <- df_out %>%
    mutate(
      across(any_of(vars_chr), as.character),
      across(any_of(vars_num), ~ suppressWarnings(as.numeric(.x)))
    )
  
  df_out
}


#Saving#
ENAHO_100 <- imap_dfr(
  ENAHO_completo,
  ~ extraer_modulo_100(year_name = .y, year_list = .x, vars_keep = vars_100)
)


ENAHO_100 <- ENAHO_100 %>%
  mutate(
    P111 = coalesce(P111, P111A)
  ) %>%
  select(-P111A) #A partir de 2012 la pregunta P111 es P111A. La P111 deja de existir


ENAHO_100 <- ENAHO_100 %>%
  mutate(
    NBI1 = coalesce(NBI1, nbi1)
  ) %>%
  select(-nbi1)


ENAHO_100 <- ENAHO_100 %>%
  mutate(
    NBI2 = coalesce(NBI2, nbi2)
  ) %>%
  select(-nbi2)

ENAHO_100 <- ENAHO_100 %>%
  mutate(
    NBI3 = coalesce(NBI3, nbi3)
  ) %>%
  select(-nbi3)

ENAHO_100 <- ENAHO_100 %>%
  mutate(
    NBI4 = coalesce(NBI4, nbi4)
  ) %>%
  select(-nbi4)

ENAHO_100 <- ENAHO_100 %>%
  mutate(
    NBI5 = coalesce(NBI5, nbi5)
  ) %>%
  select(-nbi5) #Existen periodos en los que NBI se escribe con minúscula o mayúscula


saveRDS(ENAHO_100, "./Data/ENAHO/ENAHO_100.rds")
write.csv(ENAHO_100, "./Data/ENAHO/ENAHO_100.csv", row.names = FALSE)


################
## Module 200 ##
################

vars_200 <- c(
  "AÑO", "CONGLOME", "VIVIENDA", "HOGAR", "CODPERSO",
  "UBIGEO", "ESTRATO",
  "P203", "P204", "P205", "P206",
  "P207", "P208A", "P208A1", "P214"
)

vars_200_chr <- c("AÑO", "CONGLOME", "VIVIENDA", "HOGAR", "CODPERSO", "UBIGEO")
vars_200_num <- c("ESTRATO", "P203", "P204", "P205", "P206", "P207", "P208A", "P208A1", "P214")


extraer_modulo_200 <- function(year_name, year_list, vars_keep) {
  
  file_200_name <- names(year_list)[
    str_detect(names(year_list), regex("(^|[-_])200(\\.|[-_])", ignore_case = TRUE))
  ][1]
  
  if (is.na(file_200_name)) {
    warning(paste("No se encontró módulo 200 para el año", year_name))
    return(NULL)
  }
  
  df <- as.data.frame(year_list[[file_200_name]])
  
  vars_available <- intersect(vars_keep, names(df))
  
  if (length(vars_available) == 0) {
    warning(paste("Ninguna variable objetivo encontrada en", year_name))
    return(NULL)
  }
  
  df_out <- df %>%
    select(any_of(vars_keep))
  
  if (!"AÑO" %in% names(df_out)) {
    df_out <- df_out %>%
      mutate(AÑO = as.character(year_name), .before = 1)
  }
  
  df_out <- df_out %>%
    mutate(
      across(any_of(vars_200_chr), as.character),
      across(any_of(vars_200_num), ~ suppressWarnings(as.numeric(.x)))
    )
  
  df_out
}


#Saving#

ENAHO_200 <- imap_dfr(
  ENAHO_completo,
  ~ extraer_modulo_200(year_name = .y, year_list = .x, vars_keep = vars_200)
) #Para años a partir del 2018 la variable de migración (P208A1) no está disponible

saveRDS(ENAHO_200, "./Data/ENAHO/ENAHO_200.rds")
write.csv(ENAHO_200, "./Data/ENAHO/ENAHO_200.csv", row.names = FALSE)


################
## Module 300 ##
################

vars_300 <- c(
  "AÑO", "CONGLOME", "VIVIENDA", "HOGAR", "CODPERSO",
  "UBIGEO", "ESTRATO", "CODINFOR",
  "P300A", "P301A", "P301B", "P301C",
  "P302", "P306", "P307", "P313", "P313A",
  "P203", "P207", "P208A", "T313", "T313A"
)


vars_300_chr <- c("AÑO", "CONGLOME", "VIVIENDA", "HOGAR", "CODPERSO", "UBIGEO")
vars_300_num <- c(
  "ESTRATO", "CODINFOR",
  "P300A", "P301A", "P301B", "P301C",
  "P302", "P306", "P307", "P313", "P313A",
  "P203", "P207", "P208A", "T313", "T313A"
)


extraer_modulo_300 <- function(year_name, year_list, vars_keep) {
  
  file_300_name <- names(year_list)[
    str_detect(
      names(year_list),
      regex("(^|[-_])300(\\.|[-_])", ignore_case = TRUE)
    )
  ][1]
  
  if (is.na(file_300_name)) {
    warning(paste("No se encontró módulo 300 para el año", year_name))
    return(NULL)
  }
  
  df <- as.data.frame(year_list[[file_300_name]])
  
  vars_available <- intersect(vars_keep, names(df))
  
  if (length(vars_available) == 0) {
    warning(paste("Ninguna variable objetivo encontrada en", year_name))
    return(NULL)
  }
  
  df_out <- df %>%
    select(any_of(vars_keep))
  
  if (!"AÑO" %in% names(df_out)) {
    df_out <- df_out %>%
      mutate(AÑO = as.character(year_name), .before = 1)
  }
  
  df_out <- df_out %>%
    mutate(
      across(any_of(vars_300_chr), as.character),
      across(any_of(vars_300_num), ~ suppressWarnings(as.numeric(.x)))
    )
  
  df_out
}


# Saving #

ENAHO_300 <- imap_dfr(
  ENAHO_completo,
  ~ extraer_modulo_300(year_name = .y, year_list = .x, vars_keep = vars_300)
)


ENAHO_300 <- ENAHO_300 %>%
  mutate(
    P313 = coalesce(P313, P313A)
  ) %>%
  select(-P313A)


ENAHO_300 <- ENAHO_300 %>%
  mutate(
    T313 = coalesce(T313, T313A)
  ) %>%
  select(-T313A)


saveRDS(ENAHO_300, "./Data/ENAHO/ENAHO_300.rds")
write.csv(ENAHO_300, "./Data/ENAHO/ENAHO_300.csv", row.names = FALSE)

################
## Module 500 ##
################

vars_500 <- c(
  "AÑO", "CONGLOME", "VIVIENDA", "HOGAR", "CODPERSO",
  "UBIGEO", "ESTRATO", "CODINFOR",
  "P501", "P502", "P503",
  "P505", "P505B", "P506", "P507",
  "P510A", "P510A1",
  "P513T", "P5118",
  "D524A1", "D530A",
  "P5566A", "P5566B", "P5566C",
  "P524A1", "P530A",
  "P207", "P208A", "P301A",
  "OCU500"
)


vars_500_chr <- c(
  "AÑO", "CONGLOME", "VIVIENDA", "HOGAR", "CODPERSO", "UBIGEO"
)

vars_500_num <- c(
  "ESTRATO", "CODINFOR",
  "P501", "P502", "P503",
  "P505", "P505B", "P506", "P507",
  "P510A", "P510A1",
  "P513T", "P5118",
  "D524A1", "D530A",
  "P5566A", "P5566B", "P5566C",
  "P524A1", "P530A",
  "P207", "P208A", "P301A",
  "OCU500"
)


extraer_modulo_500 <- function(year_name, year_list, vars_keep) {
  
  file_500_names <- names(year_list)[
    str_detect(
      names(year_list),
      regex("(^|[-_])500(\\(|\\.|[-_]|$)", ignore_case = TRUE)
    )
  ]
  
  if (length(file_500_names) == 0) {
    warning(paste("No se encontró módulo 500 para el año", year_name))
    return(NULL)
  }
  
  # leer y preparar cada subarchivo
  dfs_500 <- lapply(file_500_names, function(f) {
    df <- as.data.frame(year_list[[f]])
    names(df) <- toupper(names(df))
    df
  })
  
  # seleccionar en cada uno solo lo que exista
  dfs_500 <- lapply(dfs_500, function(df) {
    vars_available <- intersect(toupper(vars_keep), names(df))
    df %>% select(any_of(vars_available))
  })
  
  # asegurar que AÑO exista
  dfs_500 <- lapply(dfs_500, function(df) {
    if (!"AÑO" %in% names(df)) {
      df <- df %>% mutate(AÑO = as.character(year_name), .before = 1)
    }
    df
  })
  
  # merge progresivo por llave común
  key_vars <- c("AÑO", "CONGLOME", "VIVIENDA", "HOGAR", "CODPERSO")
  
  # usar solo las llaves presentes en todos
  common_keys <- Reduce(intersect, lapply(dfs_500, names))
  join_keys <- intersect(key_vars, common_keys)
  
  if (length(join_keys) < 5) {
    warning(paste("Muy pocas llaves comunes en módulo 500 para el año", year_name))
  }
  
  df_out <- Reduce(function(x, y) full_join(x, y, by = join_keys), dfs_500)
  
  # homogeneizar tipos
  df_out <- df_out %>%
    mutate(
      across(any_of(vars_500_chr), as.character),
      across(any_of(vars_500_num), ~ suppressWarnings(as.numeric(.x)))
    )
  
  # si hay columnas duplicadas por joins raros, revisarlas después
  df_out
}


# Saving #

ENAHO_500 <- imap_dfr(
  ENAHO_completo,
  ~ extraer_modulo_500(year_name = .y, year_list = .x, vars_keep = vars_500)
)


ENAHO_500 <- ENAHO_500 %>%
  mutate(
    P510A = coalesce(P510A, P510A1)
  ) %>%
  select(-P510A1)


saveRDS(ENAHO_500, "./Data/ENAHO/ENAHO_500.rds")
write.csv(ENAHO_500, "./Data/ENAHO/ENAHO_500.csv", row.names = FALSE)

#############
## Sumaria ##
#############

vars_sumaria <- c(
  "CONGLOME", "VIVIENDA", "HOGAR", "UBIGEO",
  "FACTOR07", "DOMINIO",
  "MIEPERHO", "PERCEPHO",
  "INGBRUHD", "INGHOG2D",
  "GASHOG1D", "GASHOG2D",
  "POBREZA"
)

vars_chr_sumaria <- c("CONGLOME", "VIVIENDA", "HOGAR", "UBIGEO")

vars_num_sumaria <- c(
  "FACTOR07", "DOMINIO",
  "MIEPERHO", "PERCEPHO",
  "INGBRUHD", "INGHOG2D",
  "GASHOG1D", "GASHOG2D",
  "POBREZA"
)


extraer_sumaria <- function(year_name, year_list, vars_keep) {
  
  file_candidates <- names(year_list)[
    stringr::str_detect(names(year_list), stringr::regex("sumaria", ignore_case = TRUE))
  ]
  
  # 1. Priorizar archivo anual "sumaria-AAAA" sin sufijo adicional como -12
  file_name <- file_candidates[
    stringr::str_detect(
      file_candidates,
      stringr::regex(paste0("^sumaria[-_]", year_name, "\\.[A-Za-z0-9]+$"), ignore_case = TRUE)
    )
  ][1]
  
  # 2. Si no existe, usar el primero que contenga el año pero no tenga sufijo extra
  if (is.na(file_name)) {
    file_name <- file_candidates[
      stringr::str_detect(file_candidates, year_name) &
        !stringr::str_detect(file_candidates, paste0(year_name, "[-_]\\d+"))
    ][1]
  }
  
  # 3. Si tampoco existe, usar el primero disponible
  if (is.na(file_name)) {
    file_name <- file_candidates[1]
  }
  
  if (is.na(file_name)) {
    warning(paste("No se encontró SUMARIA para el año", year_name))
    return(NULL)
  }
  
  print(paste("Usando archivo:", file_name, "para el año", year_name))
  
  df <- as.data.frame(year_list[[file_name]])
  names(df) <- toupper(names(df))
  vars_keep <- toupper(vars_keep)
  
  # Buscar automáticamente LINPE* y LINEA*
  linpe_vars <- names(df)[stringr::str_detect(names(df), "^LINPE")]
  linea_vars <- names(df)[stringr::str_detect(names(df), "^LINEA")]
  
  vars_finales <- unique(c(vars_keep, linpe_vars, linea_vars))
  vars_available <- intersect(vars_finales, names(df))
  
  if (length(vars_available) == 0) {
    warning(paste("Ninguna variable encontrada en SUMARIA para", year_name))
    return(NULL)
  }
  
  df_out <- df %>%
    dplyr::select(dplyr::any_of(vars_available))
  
  if (!"AÑO" %in% names(df_out)) {
    df_out <- df_out %>%
      dplyr::mutate(AÑO = as.character(year_name), .before = 1)
  }
  
  df_out <- df_out %>%
    dplyr::mutate(
      dplyr::across(dplyr::any_of(vars_chr_sumaria), as.character),
      dplyr::across(-dplyr::any_of(vars_chr_sumaria), ~ suppressWarnings(as.numeric(.x)))
    )
  
  # Unificar LINPE
  if (length(linpe_vars) > 0) {
    df_out$LINPE <- dplyr::coalesce(!!!df_out[linpe_vars])
  } else {
    df_out$LINPE <- NA_real_
  }
  
  # Unificar LINEA
  if (length(linea_vars) > 0) {
    df_out$LINEA <- dplyr::coalesce(!!!df_out[linea_vars])
  } else {
    df_out$LINEA <- NA_real_
  }
  
  # Eliminar columnas originales LINPE* y LINEA* salvo la final
  df_out <- df_out %>%
    dplyr::select(
      -dplyr::any_of(setdiff(linpe_vars, "LINPE")),
      -dplyr::any_of(setdiff(linea_vars, "LINEA"))
    )
  
  df_out
}



# Saving #

ENAHO_sumaria <- imap_dfr(
  ENAHO_completo,
  ~ extraer_sumaria(.y, .x, vars_sumaria)
)


saveRDS(ENAHO_sumaria, "./Data/ENAHO/ENAHO_sumaria.rds")
write.csv(ENAHO_sumaria, "./Data/ENAHO/ENAHO_sumaria.csv", row.names = FALSE)


###################### Tipo de cambio ######################

EX <- read_xlsx("./Data/Exchange Rate PEN-USD/PEN-USD 02-24.xlsx", sheet = "Anuales")

names(EX)[1]<-paste("Año")
names(EX)[2]<-paste("Tipo de cambio")

EX <- EX[-c(1:3), ]

saveRDS(EX, "./Data/Exchange Rate PEN-USD/EX.rds")


###################### Mining Site ######################

# Load #

Mining_Site_path <- "./Data/Mining Site/Produccion Minera MEF"
Mining_Site <- read_xlsx(file.path(Mining_Site_path, "./mining_site_resumen.xlsx"))


# Cleaning #

nombres <- Mining_Site[2, ] %>% unlist() %>% as.character()

nombres <- nombres %>%
  str_squish() %>%
  str_to_upper() %>%
  stringi::stri_trans_general("Latin-ASCII")

names(Mining_Site) <- nombres

Mining_Site <- Mining_Site[-c(1,2,3), ]

Mining_Site <- Mining_Site %>% tibble::as_tibble()


# PROCESO #

Mining_Site <- Mining_Site %>%
  mutate(
    PROCESO = normalizar_texto(PROCESO)
  )


# CLASIFICACION #


Mining_Site <- Mining_Site %>%
  mutate(
    CLASIFICACION = normalizar_texto(CLASIFICACION)
  )


# TITULAR #

Mining_Site <- Mining_Site %>%
  mutate(
    TITULAR = TITULAR,
    TITULAR = TITULAR %>%
      as.character() %>%
      str_squish() %>%
      str_to_upper() %>%
      stringi::stri_trans_general("Latin-ASCII") %>%
      str_replace_all("\\s+", " ") %>%
      na_if("")
  )

Mining_Site <- Mining_Site %>%
  mutate(
    TITULAR = TITULAR %>%
      str_replace_all("S\\s*\\.\\s*A\\s*\\.\\s*A\\s*\\.", "SAA") %>%
      str_replace_all("S\\s*\\.\\s*A\\s*\\.\\s*C\\s*\\.", "SAC") %>%
      str_replace_all("S\\s*\\.\\s*R\\s*\\.\\s*L\\s*\\.", "SRL") %>%
      str_replace_all("S\\s*\\.\\s*A\\s*\\.", "SA") %>%
      str_replace_all("E\\s*\\.\\s*I\\s*\\.\\s*R\\s*\\.\\s*L\\s*\\.", "EIRL") %>%
      str_replace_all("S\\s*\\.\\s*M\\s*\\.\\s*R\\s*\\.\\s*L\\s*\\.", "SMRL") %>%
      str_replace_all("S\\s*\\.\\s*C\\s*\\.\\s*R\\s*\\.\\s*L\\s*\\.", "SRL") %>%
      str_replace_all("SOCIEDAD ANONIMA CERRADA", "SAC") %>%
      str_replace_all("SOCIEDAD ANONIMA", "SA") %>%
      str_replace_all("SOCIEDAD COMERCIAL DE RESPONSABILIDAD LIMITADA", "SRL") %>%
      str_replace_all("EMPRESA INDIVIDUAL DE RESPONSABILIDAD LIMITADA", "EIRL") %>%
      str_replace_all("\\s+", " ") %>%
      str_squish()
  )

Mining_Site <- Mining_Site %>%
  mutate(
    TITULAR = TITULAR %>%
#      str_remove("\\s+(SAA|SAC|SA|SRL|EIRL|LTDA)\\.?$") %>%
      str_remove("\\s+EN LIQUIDACION( EN MARCHA)?$") %>%
      str_remove("\\s+SUCURSAL DEL PERU$") %>%
      str_squish()
  )


Mining_Site %>%
  count(TITULAR, sort = TRUE) %>%
  print(n = 100)


# UNIDAD #

limpiar_unidad_min <- function(x) {
  x %>%
    normalizar_texto() %>%
    
    # unificar signos tipo N°, Nº, N.
    str_replace_all("N[°º\\.]\\s*", "N ") %>%
    
    # quitar anotaciones tipo "a)", "b)", "d)"
    str_remove_all("\\b[A-Z]\\)") %>%
    
    # quitar contenido entre paréntesis
    str_remove_all("\\([^\\)]*\\)") %>%
    
    # normalizar guiones
    str_replace_all("[-–—]+", " ") %>%
    
    # colapsar espacios
    str_squish() %>%
    na_if("")
}


Mining_Site <- Mining_Site %>%
  mutate(
    UNIDAD_CLEAN = limpiar_unidad_min(UNIDAD)
  )

Mining_Site %>%
  distinct(UNIDAD, UNIDAD_CLEAN) %>%
  filter(UNIDAD != UNIDAD_CLEAN) %>%
  arrange(UNIDAD) %>%
  print(n = 100) #Revisar cambios luego de primera limpieza

Mining_Site %>%
  count(UNIDAD_CLEAN, sort = TRUE) %>%
  print(n = 50)


Mining_Site %>%
  distinct(UNIDAD, UNIDAD_CLEAN) %>%
  filter(UNIDAD != UNIDAD_CLEAN) %>%
  arrange(UNIDAD) %>%
  print(n = 100)

# Revisar si la enumeración de minas son relevantes para la identificación de
# la unidad minera

crear_unidad_base <- function(x) {
  x %>%
    # partir de la versión ya limpia
    str_squish() %>%
    
    # quitar anotaciones finales tipo N 1, N 2, etc.
    str_remove("\\s+N\\s+\\d+[A-Z]?$") %>%
    
    # quitar números finales simples
    str_remove("\\s+\\d+[A-Z]?$") %>%
    
    # quitar romanos finales
    str_remove("\\s+(I|II|III|IV|V|VI|VII|VIII|IX|X)$") %>%
    
    # quitar sufijos UNO/DOS/TRES al final
    str_remove("\\s+(UNO|DOS|TRES|CUATRO|CINCO|SEIS|SIETE|OCHO|NUEVE|DIEZ)$") %>%
    
    str_squish() %>%
    na_if("")
}


Mining_Site <- Mining_Site %>%
  mutate(
    UNIDAD_BASE = crear_unidad_base(UNIDAD_CLEAN)
  )


revision_unidades <- Mining_Site %>%
  count(REGION, PROVINCIA, DISTRITO, UNIDAD_BASE, UNIDAD_CLEAN, sort = TRUE)

revision_unidades %>%
  group_by(REGION, PROVINCIA, DISTRITO, UNIDAD_BASE) %>%
  summarise(
    n_variantes = n_distinct(UNIDAD_CLEAN),
    variantes = paste(sort(unique(UNIDAD_CLEAN)), collapse = " | "),
    .groups = "drop"
  ) %>%
  filter(n_variantes > 1) %>%
  arrange(desc(n_variantes), REGION, PROVINCIA, DISTRITO, UNIDAD_BASE)

Mining_Site <- Mining_Site %>%
  mutate(
    UNIDAD_BASE = crear_unidad_base(UNIDAD_CLEAN)
  )

Mining_Site %>%
  group_by(REGION, PROVINCIA, DISTRITO, UNIDAD_BASE) %>%
  summarise(
    n_variantes = n_distinct(UNIDAD_CLEAN),
    variantes = paste(sort(unique(UNIDAD_CLEAN)), collapse = " | "),
    .groups = "drop"
  ) %>%
  filter(n_variantes > 1) %>%
  arrange(desc(n_variantes)) %>%
  print(n = 100)  # La enumeración de las unidades parece ser un factor irrelevante
                  # a nivel distrito-año por lo que se procede a colapsar unidades
                  # similares.

Mining_Site <- Mining_Site %>%
  mutate(
    UNIDAD_CLEAN = UNIDAD_BASE
  )


# REGION #
# Todas las regiones deberían aparecer en el df de Mining_Site ya que se
# registra CM para todas las regiones

unique(Mining_Site$REGION)
unique(Ubigeo_region$departamento)

Mining_Site <- Mining_Site %>%
  mutate(
    REGION_RAW = REGION,
    REGION_STD = normalizar_texto(REGION)
  )

Ubigeo_region <- Ubigeo_region %>%
  mutate(
    departamento_raw = departamento,
    departamento_std = normalizar_texto(departamento)
  )

Regiones_Sin_Mining_Site <- setdiff(
  sort(unique(Ubigeo_region$departamento_std)),
  sort(unique(Mining_Site$REGION_STD))
)


summary(Transferencias_Regionales$authorised) # Las regiones que no se encuentran en el df
                                              # Mining_Site no son regiones que explotan alguno
                                              # de los minerales principales. Aparentemente son
                                              # regiones con transferencias de CM relativamente bajas.
                                              # Se dejarán de lado para simplificar el estudio y
                                              # continuar con observaciones más relevantes.

intersect(
  sort(unique(Mining_Site$REGION_STD)),
  sort(unique(Ubigeo_region$departamento_std))
)


# Contruir mapa con diferencias de transferencias cumulativas para comprobar  #
# relevandia de las regiones que no aparecen en Mining_Site                   #

Transferencias_Acumulado <- Transferencias_Regionales %>%
  group_by(name) %>%
  summarise(
    authorised_acum = sum(authorised, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(REGION_UPPER = name) # Las 6 regiones con menor transferencia acumulada
                              # en el periodo 04-24 son justamente las regiones 
                              # que no cuentan con Mining_Site

Mapa_Region <- Mapa_Region %>%
  left_join(Transferencias_Acumulado, by = "REGION_UPPER")

Mapa_Region <- Mapa_Region %>%
  mutate(
    Mining_Site_Status = case_when(
      REGION_UPPER %in% Regiones_Sin_Mining_Site ~ "No Mining_Site match",
      TRUE ~ "Mining_Site match"
    )
  )

# Mapa #
pal_wes <- colorRampPalette(wes_palette("Zissou1", 5))(100)

ggplot(Mapa_Region) +
  geom_sf(aes(fill = log(authorised_acum)), color = "white", linewidth = 0.2) +
  geom_sf(fill = NA, color = "black", linewidth = 0.25) +
  geom_text_repel(
    data = Mapa_Region %>% 
      st_drop_geometry() %>%
      filter(Mining_Site_Status == "No Mining_Site match"),
    aes(x = coords_x, y = coords_y, label = REGION_UPPER),
    size = 3,
    fontface = "bold",
    max.overlaps = 30,
    box.padding = 0.35,
    point.padding = 0.2,
    segment.color = "gray50"
  ) +
  scale_fill_gradientn(
    colors = pal_wes,
    na.value = "grey90",
    name = "Authorised\nacumulado\n2004–2024"
  ) +
  labs(
    title = "Acumulado regional de transferencias autorizadas del canon en Perú",
    subtitle = "Etiquetas solo para regiones ausentes en Mining_Site"
  ) +
  theme_minimal()



# PROVINCIA #
# Todas las provincias deberían aparecer en el df de Mining_Site ya que se
# registra CM para todas las provincias

unique(Mining_Site$PROVINCIA)
unique(Ubigeo_provincia$provincia)

Mining_Site <- Mining_Site %>%
  mutate(
    PROVINCIA_RAW = PROVINCIA,
    PROVINCIA_STD = normalizar_texto(PROVINCIA)
  )

Ubigeo_provincia <- Ubigeo_provincia %>%
  mutate(
    provincia_raw = provincia,
    provincia_std = normalizar_texto(provincia)
  )


prov_mining <- sort(unique(Mining_Site$PROVINCIA_STD))
prov_ubigeo <- sort(unique(Ubigeo_provincia$provincia_std))


posibles_matches <- expand.grid(
  mining = prov_mining,
  ubigeo = prov_ubigeo,
  stringsAsFactors = FALSE
) %>%
  as_tibble() %>%
  mutate(
    dist = stringdist(mining, ubigeo, method = "jw")
  ) %>%
  filter(dist < 0.15, mining != ubigeo) %>%
  arrange(dist) # No existen diferencias relevantes entre los nombres de los dos df.


posibles_matches %>% print(n = 100)

Provincia_Sin_Mining_Site <- setdiff(
  sort(unique(Ubigeo_provincia$provincia_std)),
  sort(unique(Mining_Site$PROVINCIA_STD))
)


summary(Transferencias_Provinciales$authorised) # Las regiones que no se encuentran en el df
# Mining_Site no son regiones que explotan alguno
# de los minerales principales. Aparentemente son
# regiones con transferencias de CM relativamente bajas.
# Se dejarán de lado para simplificar el estudio y
# continuar con observaciones más relevantes.

intersect(
  sort(unique(Mining_Site$PROVINCIA_STD)),
  sort(unique(Ubigeo_provincia$provincia_std))
)



Transferencias_Provinciales %>%
  mutate(
    provincia_std = normalizar_texto(name)
  ) %>%
  mutate(
    sin_mining = provincia_std %in% Provincia_Sin_Mining_Site
  ) %>%
  group_by(sin_mining) %>%
  summarise(
    n = n(),
    mean = mean(authorised, na.rm = TRUE),
    p50 = median(authorised, na.rm = TRUE),
    p90 = quantile(authorised, 0.9, na.rm = TRUE),
    p99 = quantile(authorised, 0.99, na.rm = TRUE),
    max = max(authorised, na.rm = TRUE)
  ) # En promedio los valores de sin_mining son relativamente pequeños lo que
    # podría ser plausible.



Transferencias_Provinciales %>%
  mutate(provincia_std = normalizar_texto(name)) %>%
  filter(provincia_std %in% Provincia_Sin_Mining_Site) %>%
  arrange(desc(authorised)) %>%
  select(provincia_std, authorised, year) %>%
  head(20)  # Sin embargo Tarata en Tacna es la provincia con transferencias más
            # alta. Determinar si esto puede ser predicho por hat(CM) o si esto
            # afecta la estimación de alguna manera. <- 


# Distristo #

unique(Mining_Site$DISTRITO)
unique(Ubigeo_distrito$distrito)


Mining_Site <- Mining_Site %>%
  mutate(
    DISTRITO_RAW = DISTRITO,
    DISTRITO_STD = normalizar_texto(DISTRITO)
  )

Ubigeo_distrito <- Ubigeo_distrito %>%
  mutate(
    distrito_raw = distrito,
    distrito_std = normalizar_texto(distrito)
  )


dist_mining <- sort(unique(Mining_Site$DISTRITO_STD))
dist_ubigeo <- sort(unique(Ubigeo_distrito$distrito_std))


posibles_matches <- expand.grid(
  mining = dist_mining,
  ubigeo = dist_ubigeo,
  stringsAsFactors = FALSE
) %>%
  as_tibble() %>%
  mutate(
    dist = stringdist(mining, ubigeo, method = "jw")
  ) %>%
  filter(dist < 0.15, mining != ubigeo) %>%
  arrange(dist) 


posibles_matches %>% print(n = 100) # Tras revisar los nombres con alta similitud
                                    # se identificó que arreglar los nombres no es
                                    # necesario. A pesar de la similitud, los nombres
                                    # pertenecen a distritos distintos.


Distrito_Sin_Mining_Site <- setdiff(
  sort(unique(Ubigeo_distrito$distrito_std)),
  sort(unique(Mining_Site$DISTRITO_STD))
)


Distrito_Sin_Mining_Site

summary(Transferencias_Municipales$authorised)  # Las regiones que no se encuentran en el df
                                                # Mining_Site no son regiones que explotan alguno
                                                # de los minerales principales. Aparentemente son
                                                # regiones con transferencias de CM relativamente bajas.
                                                # Se dejarán de lado para simplificar el estudio y
                                                # continuar con observaciones más relevantes.

intersect(
  sort(unique(Mining_Site$DISTRITO_STD)),
  sort(unique(Ubigeo_distrito$distrito_std))
)



Transferencias_Municipales %>%
  mutate(
    distrito_std = normalizar_texto(name)
  ) %>%
  mutate(
    sin_mining = distrito_std %in% Distrito_Sin_Mining_Site
  ) %>%
  group_by(sin_mining) %>%
  summarise(
    n = n(),
    mean = mean(authorised, na.rm = TRUE),
    p50 = median(authorised, na.rm = TRUE),
    p90 = quantile(authorised, 0.9, na.rm = TRUE),
    p99 = quantile(authorised, 0.99, na.rm = TRUE),
    max = max(authorised, na.rm = TRUE)
  ) # En promedio los valores de sin_mining son relativamente pequeños lo que
    # podría ser plausible.



Transferencias_Municipales %>%
  mutate(distrito_std = normalizar_texto(name)) %>%
  filter(distrito_std %in% Distrito_Sin_Mining_Site) %>%
  arrange(desc(authorised)) %>%
  select(distrito_std, authorised, year) %>%
  head(20)  # Adicionalmente, los distritos con transferencias más altas no son
            # distritos con transferencias particularmente altas.


# Meses y Totales #

meses <- c(
  "JANUAR","FEBRUAR","MARZ","APRIL","MAI","JUNI",
  "JULI","AUGUST","SEPTEMBER","OKTOBER","NOVEMBER","DEZEMBER"
)

total_col <- "TOTAL GENERAL"

Mining_Site <- Mining_Site %>%
  mutate(
    across(all_of(c(meses, total_col)), as.numeric),
    across(all_of(c(meses, total_col)), ~ round(., 6))
  )


Mining_Site <- Mining_Site %>%
  mutate(
    TOTAL = rowSums(select(., JANUAR:DEZEMBER), na.rm = TRUE)
  )

summary(Mining_Site$`TOTAL GENERAL` - Mining_Site$TOTAL)


Mining_Site <- Mining_Site %>% 
  select(-`TOTAL GENERAL`)

names(Mining_Site) <- names(Mining_Site) %>%
  recode(
    "JANUAR" = "JAN",
    "FEBRUAR" = "FEB",
    "MARZ" = "MAR",
    "APRIL" = "APR",
    "MAI" = "MAY",
    "JUNI" = "JUN",
    "JULI" = "JUL",
    "AUGUST" = "AUG",
    "SEPTEMBER" = "SEP",
    "OKTOBER" = "OCT",
    "NOVEMBER" = "NOV",
    "DEZEMBER" = "DEC")


Mining_Site_long <- Mining_Site %>%
  pivot_longer(
    cols = JAN:DEC,
    names_to = "MES",
    values_to = "PRODUCCION"
  )

Mining_Site_long <- Mining_Site_long %>%
  mutate(
    MES_NUM = match(MES, c(
      "JAN","FEB","MAR","APR","MAY","JUN",
      "JUL","AUG","SEP","OCT","NOV","DEC"
    ))
  )

Mining_Site_distrito <- Mining_Site_long %>%
  group_by(ANO, DISTRITO_STD) %>%
  summarise(
    produccion_total = sum(PRODUCCION, na.rm = TRUE),
    .groups = "drop"
  )




# Saving #

saveRDS(Mining_Site, file.path(Mining_Site_path,"./Mining_Site.rds"))
write.csv(Mining_Site, file.path(Mining_Site_path,"./Mining_Site.cvs"), row.names = FALSE)

saveRDS(Mining_Site_long, file.path(Mining_Site_path,"./Mining_Site_long.rds"))
write.csv(Mining_Site_long, file.path(Mining_Site_path,"./Mining_Site_long.cvs"), row.names = FALSE)

saveRDS(Mining_Site_distrito, file.path(Mining_Site_path,"./Mining_Site_distrito.rds"))
write.csv(Mining_Site_distrito, file.path(Mining_Site_path,"./Mining_Site_distrito.cvs"), row.names = FALSE)



###################### Mineral Prices ######################

# Load #

Prices_path <- "./Data/Commodity Prices"

Molybdenum <- read_csv(file.path(Prices_path, "./dataset_2026-04-17T09_51_14.139456426Z_DEFAULT_INTEGRATION_IMF.RES_PCPS_9.0.0.csv"))



# Cleaning #




























