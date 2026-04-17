# ==============================================================================
# 01_data_preparation.R
# Master Thesis – Canon Minero and Fiscal Redistribution in Peru
# Author: Esteban
# Description: Loads, cleans, harmonises, and exports all data sources:
#              Canon Minero transfers, ENAHO survey modules, exchange rates,
#              and mining-site production records.
# ==============================================================================

# ---- 0. Setup ----------------------------------------------------------------

## 0.1 Packages ----

library(tidyverse)
library(readxl)
library(foreign)
library(sf)
library(ggrepel)
library(mapsPERU)
library(stringi)
library(stringdist)
library(writexl)
library(wesanderson)
library(haven)
library(fs)
library(janitor)

## 0.2 Environment ----

rm(list = ls())

project_root <- "C:/Users/esteb/OneDrive/Documents/Esteban/Berlin/General/MA LA Studien/Masterarbeit/"
setwd(project_root)

## 0.3 Helper functions ----

#' Standardise text: uppercase, trim whitespace, strip diacritics, NA if empty.
normalizar_texto <- function(x) {
  x %>%
    as.character() %>%
    str_squish() %>%
    str_to_upper() %>%
    stri_trans_general("Latin-ASCII") %>%
    na_if("")
}

## 0.4 Reference data ----

# Shapefiles from mapsPERU
mapa_region    <- st_as_sf(map_REG)   # Distinguishes Lima Provincias / Lima Metropolitana
mapa_provincia <- st_as_sf(map_PROV)
mapa_distrito  <- st_as_sf(map_DIST)

# Ubigeo catalogues
ubigeo_region    <- read_rds("./Data/Ubigeo/ubigeo_departamento.rds")
ubigeo_provincia <- read_rds("./Data/Ubigeo/ubigeo_provincia.rds")
ubigeo_distrito  <- read_rds("./Data/Ubigeo/ubigeo_distrito.rds")


# ==============================================================================
# ---- 1. Canon Minero Transfers ----------------------------------------------
# ==============================================================================

full_years  <- 2004:2024
short_years <- sprintf("%02d", 4:24)

## 1.1 Regional transfers ----

regional_path <- "./Data/Transferencias Canon Minero/Gobiernos Regionales/"

# Rows to skip per year (header junk in the original .xls files).
# Most years have 11 header rows; 2004 has 16 and 2017 has 14.
skip_rows_regional <- setNames(
  c(16, rep(11, 12), 14, rep(11, 7)),
  short_years
)

read_transfer_file <- function(path, year, rows_to_skip) {
  df <- read_xls(path)
  names(df)[1:4] <- c("code", "name", "authorised", "credited")
  
  df <- df %>%
    slice(-(1:rows_to_skip)) %>%
    mutate(
      year       = year,
      authorised = as.numeric(gsub("[^0-9.-]", "", authorised)) / 1e6,
      credited   = as.numeric(gsub("[^0-9.-]", "", credited))   / 1e6,
      .after     = name
    )
  
  df
}

transferencias_regionales <- map2_dfr(
  full_years, short_years,
  function(yr_full, yr_short) {
    file <- paste0(regional_path, yr_full, " Gobiernos Regionales.xls")
    read_transfer_file(file, yr_full, skip_rows_regional[[yr_short]])
  }
)


## 1.2 Municipal transfers ----

municipal_path <- "./Data/Transferencias Canon Minero/Gobiernos Locales/"

# All municipal files share the same 11-row header
transferencias_municipales <- map_dfr(full_years, function(yr) {
  file <- paste0(municipal_path, yr, " Gobiernos Locales.xls")
  read_transfer_file(file, yr, rows_to_skip = 11)
})


## 1.3 Cleaning: regional names ----

transferencias_regionales <- transferencias_regionales %>%
  mutate(
    name = name %>%
      str_remove("^GOBIERNO REGIONAL ") %>%
      str_remove("^DEL DEPARTAMENTO DE ") %>%
      str_remove("^DE LA PROVINCIA CONSTITUCIONAL DEL ") %>%
      str_replace("^MUNICIPALIDAD METROPOLITANA DE LIMA$", "LIMA METROPOLITANA") %>%
      str_replace("^LIMA$", "LIMA PROVINCIAS")
  )


## 1.4 Cleaning: municipal names ----

# Unify inconsistent Lima names (all share the same code)
transferencias_municipales <- transferencias_municipales %>%
  mutate(
    name = case_when(
      name %in% c("MUNICIPALIDAD PROVINCIAL DE LIMA",
                  "MUN. PRO. DE LIMA",
                  "MUNICIPALIDAD METROPOLITANA DE LIMA") ~ "LIMA",
      TRUE ~ name
    )
  )

# Identify province from hierarchical ordering: province headers propagate
# downward until the next province header appears.
transferencias_municipales <- transferencias_municipales %>%
  mutate(
    province = if_else(
      str_detect(name, "^MUN\\. PRO\\.|^MUNICIPALIDAD PROVINCIAL|^LIMA$"),
      name,
      NA_character_
    )
  ) %>%
  fill(province, .direction = "down")

# Fix mis-assigned districts: these belong to Alto Amazonas, not Datem del Marañón
distritos_alto_amazonas <- c(
  "MUNICIPALIDAD DISTRITAL DE JEBEROS",
  "MUNICIPALIDAD DISTRITAL DE LAGUNAS",
  "MUNICIPALIDAD DISTRITAL DE SANTA CRUZ",
  "MUNICIPALIDAD DISTRITAL DE TENIENTE CESAR LOPEZ ROJAS"
)

transferencias_municipales <- transferencias_municipales %>%
  mutate(
    province = if_else(
      province == "MUNICIPALIDAD PROVINCIAL DE DATEM DEL MARAÑON" &
        name %in% distritos_alto_amazonas,
      "MUNICIPALIDAD PROVINCIAL DEL ALTO AMAZONAS - YURIMAGUAS",
      province
    )
  )

# Strip prefixes from province names
transferencias_municipales <- transferencias_municipales %>%
  filter(str_detect(province, "MUN\\. PRO\\.|^MUNICIPALIDAD PROVINCIAL |^LIMA$")) %>%
  mutate(
    province = province %>%
      str_remove("^MUN\\. PRO\\. DEL? ") %>%
      str_remove("^MUN\\. PRO\\. ") %>%
      str_remove("^MUNICIPALIDAD PROVINCIAL DEL? ") %>%
      str_remove("^MUNICIPALIDAD PROVINCIAL ") %>%
      str_remove("\\s*-.*")
  )


## 1.5 Build provincial aggregation ----

# Sum all district-level transfers within each province-year
transferencias_provinciales <- transferencias_municipales %>%
  group_by(province, year) %>%
  summarise(
    authorised = sum(authorised, na.rm = TRUE),
    credited   = sum(credited,   na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  rename(name = province)


## 1.6 Strip prefixes from district names ----

transferencias_municipales <- transferencias_municipales %>%
  mutate(
    name = name %>%
      str_remove("^MUN\\. DIS\\. DE ") %>%
      str_remove("^MUN\\. PRO\\. DEL? ") %>%
      str_remove("^MUN\\. PRO\\. ") %>%
      str_remove("^MUNICIPALIDAD PROVINCIAL DEL? ") %>%
      str_remove("^MUNICIPALIDAD PROVINCIAL ") %>%
      str_remove("^MUN\\. DIS\\. ") %>%
      str_remove("^MUN\\. DIST\\. ") %>%
      str_remove("^MUNICIPALIDAD DISTRITAL DE ") %>%
      str_remove("^MUNICIPALIDAD DISTRITAL ") %>%
      str_remove("^ DE ") %>%
      str_remove("^ ") %>%
      str_remove("\\s*-.*")
  )


## 1.7 Normalise text in transfers and map ----

transferencias_municipales <- transferencias_municipales %>%
  mutate(
    province = normalizar_texto(province),
    name     = normalizar_texto(name)
  )

transferencias_provinciales <- transferencias_provinciales %>%
  mutate(name = normalizar_texto(name))

mapa_distrito <- mapa_distrito %>%
  mutate(
    PROVINCIA = normalizar_texto(PROVINCIA),
    DISTRITO  = normalizar_texto(DISTRITO)
  )


## 1.8 Name harmonisation: province-level ----

# Lookup table for province name corrections (transfers → map standard)
correccion_provincias <- tribble(
  ~original,                ~corregido,
  "NAZCA",                  "NASCA",
  "CAÑETE",                 "CANETE",
  "SANCHEZ CERRO",          "GENERAL SANCHEZ CERRO",
  "DANIEL CARRION",         "DANIEL ALCIDES CARRION",
  "DANIEL A. CARRION",      "DANIEL ALCIDES CARRION",
  "CUZCO",                  "CUSCO",
  "QUISPICANCHIS",          "QUISPICANCHI",
  "MARAÑON",                "MARANON",
  "FERREÑAFE",              "FERRENAFE",
  "DATEM DEL MARAÑON",      "DATEM DEL MARANON",
  "CASCAS",                 "GRAN CHIMU",
  "HUACHO",                 "HUAURA",
  "SAN JOSE DE SISA",       "EL DORADO",
  "CALLAO",                 "PROV. CONST. DEL CALLAO"
)

aplicar_correcciones <- function(df, col, tabla) {
  lookup <- setNames(tabla$corregido, tabla$original)
  df %>%
    mutate(!!sym(col) := ifelse(
      !!sym(col) %in% names(lookup),
      lookup[!!sym(col)],
      !!sym(col)
    ))
}

transferencias_provinciales <- aplicar_correcciones(
  transferencias_provinciales, "name", correccion_provincias
)


## 1.9 Name harmonisation: district-level ----

# Comprehensive district name corrections (transfers → map standard).
# Each entry reflects a verified inconsistency between MEF naming and
# INEI/mapsPERU naming conventions.
correccion_distritos <- tribble(
  ~original,                                    ~corregido,
  "NAZCA",                                      "NASCA",
  "CAÑETE",                                     "CANETE",
  "SANCHEZ CERRO",                              "GENERAL SANCHEZ CERRO",
  "DANIEL CARRION",                             "DANIEL ALCIDES CARRION",
  "DANIEL A. CARRION",                          "DANIEL ALCIDES CARRION",
  "CUZCO",                                      "CUSCO",
  "QUISPICANCHIS",                              "QUISPICANCHI",
  "MARAÑON",                                    "MARANON",
  "FERREÑAFE",                                  "FERRENAFE",
  "DATEM DEL MARAÑON",                          "DATEM DEL MARANON",
  "SAN ISIDRO DE MAYNO",                        "SAN ISIDRO DE MAINO",
  "IMASA",                                      "IMAZA",
  "PIZUQUIA",                                   "PISUQUIA",
  "SAN CRISTOBAL DE OLTO",                      "SAN CRISTOBAL",
  "SAN JERONIMO DE PACLAS",                     "SAN JERONIMO",
  "ABELARDO PARDO LAZAMETA",                    "ABELARDO PARDO LEZAMETA",
  "MATACOTA",                                   "MATACOTO",
  "SAN MIGUEL CHACCRAMPA",                      "SAN MIGUEL DE CHACCRAMPA",
  "HUAYLLO",                                    "HUAYO",
  "QUEQUE¥A",                                   "QUEQUENA",
  "HUAC",                                       "HUAC HUAS",
  "SAN FRANCISCO DE RAVACAYCO",                 "SAN FRANCISCO DE RIVACAYCO",
  "HUAYA",                                      "HUALLA",
  "LOS BA¥OS DEL INCA",                         "LOS BANOS DEL INCA",
  "ACOSTAMBAO",                                 "ACOSTAMBO",
  "DANIEL HERNANDES",                           "DANIEL HERNANDEZ",
  "YAHUIMPUQUIO",                               "NAHUIMPUQUIO",
  "SAN MARCOS ROCCHAC",                         "SAN MARCOS DE ROCCHAC",
  "TINGUINA",                                   "LA TINGUINA",
  "LEONOR ORDO¥EZ",                             "LEONOR ORDONEZ",
  "PAMPA HERMOZA",                              "PAMPA HERMOSA",
  "HUAY",                                       "HUAY HUAY",
  "AYAUCA",                                     "ALLAUCA",
  "VILCHAYAL",                                  "VICHAYAL",
  "CAPASO",                                     "CAPAZO",
  "HUANAHUARA",                                 "HUANUARA",
  "CHUCATAMANI",                                "HEROES ALBARRACIN",
  "QUIMBIRI",                                   "KIMBIRI",
  "CRESPO Y CASTILLO",                          "JOSE CRESPO Y CASTILLO",
  "CODO DE POZUZO",                             "CODO DEL POZUZO",
  "CHUPAN",                                     "APARICIO POMARES",
  "SAN FRANCISCO DEL YESO",                     "SAN FRANCISCO DE YESO",
  "MACHAHUAY",                                  "MACHAGUAY",
  "PACAPAUZA",                                  "PACAPAUSA",
  "CCORCCA",                                    "CCORCA",
  "PACCARECTAMBO",                              "PACCARICTAMBO",
  "KCOSNIPATA",                                 "KOSNIPATA",
  "KICHKI",                                     "QUISQUI",
  "ZANA",                                       "SANA",
  "KANARIS",                                    "CANARIS",
  "FUNDICION DE TINYAHUARCO",                   "TINYAHUARCO",
  "HUATTA",                                     "HUATA",
  "SAN JOSE DE LOS CHORRILLOS",                 "CUENCA",
  "TAPAYRIHUA",                                 "TAPAIRIHUA",
  "PACAICASA",                                  "PACAYCASA",
  "ECHARATI",                                   "ECHARATE",
  "ANDRES AVELINO CACERES DORREGAY",            "ANDRES AVELINO CACERES DORREGARAY",
  "EL ARENAL",                                  "ARENAL",
  "JALCA GRANDE",                               "LA JALCA",
  "CASTA",                                      "SAN PEDRO DE CASTA",
  "SAN PEDRO DE LARAOS",                        "LARAOS",
  "PEVAS",                                      "PEBAS",
  "PACCARICTAMBO",                              "PACCARITAMBO",
  "LIMA METROPOLITANA",                         "LIMA"
)

transferencias_municipales <- aplicar_correcciones(
  transferencias_municipales, "name", correccion_distritos
)

# Also apply province-level corrections to the province column in districts
transferencias_municipales <- aplicar_correcciones(
  transferencias_municipales, "province", correccion_provincias
)

# Validate: all provinces in transfers should match map provinces
stopifnot(
  "Unmatched provinces remain" =
    all(transferencias_municipales$province %in% unique(mapa_distrito$PROVINCIA))
)

# Replace province names that refer to the province rather than the capital
# district. The transfer files use "- CAPITAL" after province names; we took
# the text before the dash, but for provincial capitals the district name
# differs from the province name.
correccion_capitales <- tribble(
  ~original,                         ~corregido,
  "BONGARA",                         "JUMBILLA",
  "CONDORCANQUI",                    "NIEVA",
  "RODRIGUEZ DE MENDOZA",            "SAN NICOLAS",
  "UTCUBAMBA",                       "BAGUA GRANDE",
  "CARLOS FERMIN FITZCARRALD",       "SAN LUIS",
  "MARISCAL LUZURIAGA",              "PISCOBAMBA",
  "AYMARAES",                        "CHALHUANCA",
  "GRAU",                            "CHUQUIBAMBILLA",
  "CONDESUYOS",                      "CHUQUIBAMBA",
  "HUAMANGA",                        "AYACUCHO",
  "HUANCA SANCOS",                   "SANCOS",
  "LA MAR",                          "SAN MIGUEL",
  "PARINACOCHAS",                    "CORACORA",
  "PAUCAR DEL SARA SARA",            "PAUSA",
  "VICTOR FAJARDO",                  "HUANCAPI",
  "ANGARAES",                        "LIRCAY",
  "TAYACAJA",                        "PAMPAS",
  "CHINCHA",                         "CHINCHA ALTA",
  "SANCHEZ CARRION",                 "HUAMACHUCO",
  "CANETE",                          "SAN VICENTE DE CANETE",
  "MARISCAL NIETO",                  "MOQUEGUA",
  "GENERAL SANCHEZ CERRO",           "OMATE",
  "PASCO",                           "CHAUPIMARCA",
  "DANIEL ALCIDES CARRION",          "YANAHUANCA",
  "TALARA",                          "PARINAS",
  "CARABAYA",                        "MACUSANI",
  "EL COLLAO",                       "ILAVE",
  "MELGAR",                          "AYAVIRI",
  "SAN ANTONIO DE PUTINA",           "PUTINA",
  "SAN ROMAN",                       "JULIACA",
  "JORGE BASADRE",                   "LOCUMBA",
  "CANAS",                           "YANAOCA",
  "CHUMBIVILCAS",                    "SANTO TOMAS",
  "CANCHIS",                         "URCOS",
  "LA CONVENCION",                   "SANTA ANA",
  "QUISPICANCHI",                    "URCOS",
  "DOS DE MAYO",                     "LA UNION",
  "HUAMALIES",                       "LLATA",
  "PACHITEA",                        "PANAO",
  "MARANON",                         "HUACRACHUCO",
  "LAURICOCHA",                      "JESUS",
  "YAROWILCA",                       "CHAVINILLO",
  "PACCARICTAMBO",                   "PACCARITAMBO",
  "GRAN CHIMU",                      "CASCAS",
  "EL DORADO",                       "SAN JOSE DE SISA",
  "CONTRALMIRANTE VILLAR",           "ZORRITOS",
  "MAYNAS",                          "IQUITOS",
  "ALTO AMAZONAS",                   "YURIMAGUAS",
  "DATEM DEL MARANON",               "BARRANCA",
  "LORETO",                          "NAUTA",
  "MARISCAL RAMON CASTILLA",         "RAMON CASTILLA",
  "UCAYALI",                         "CONTAMANA",
  "CORONEL PORTILLO",                "CALLERIA",
  "ATALAYA",                         "RAYMONDI"
)

transferencias_municipales <- aplicar_correcciones(
  transferencias_municipales, "name", correccion_capitales
)

# Final validation against map
unmatched_districts <- setdiff(
  unique(transferencias_municipales$name),
  unique(mapa_distrito$DISTRITO)
)
if (length(unmatched_districts) > 0) {
  warning("Unmatched districts: ", paste(unmatched_districts, collapse = ", "))
}


## 1.10 Export transfers ----

write.csv(transferencias_municipales,
          "./Data/Transferencias Canon Minero/Transferencias_Municipales.csv",
          row.names = FALSE)
write.csv(transferencias_provinciales,
          "./Data/Transferencias Canon Minero/Transferencias_Provinciales.csv",
          row.names = FALSE)
write.csv(transferencias_regionales,
          "./Data/Transferencias Canon Minero/Transferencias_Regionales.csv",
          row.names = FALSE)


# ==============================================================================
# ---- 2. Descriptive Visualisations ------------------------------------------
# ==============================================================================

## 2.1 Yearly aggregation ----

regional_by_year <- transferencias_regionales %>%
  group_by(year) %>%
  summarise(
    total_authorised = sum(authorised, na.rm = TRUE),
    total_credited   = sum(credited,   na.rm = TRUE)
  )

municipal_by_year <- transferencias_municipales %>%
  group_by(year) %>%
  summarise(
    total_authorised = sum(authorised, na.rm = TRUE),
    total_credited   = sum(credited,   na.rm = TRUE)
  )

# Differences between authorised and credited are negligible: only Puno
# received more than authorised, and quantile distributions are identical.
transferencias_municipales %>%
  mutate(diff = authorised - credited) %>%
  filter(diff != 0) %>%
  arrange(desc(abs(diff)))


## 2.2 Time-series comparison ----

transfers_by_year <- bind_rows(
  regional_by_year  %>% transmute(year, total_credited, type = "Regional"),
  municipal_by_year %>% transmute(year, total_credited, type = "Municipal")
)

ggplot(transfers_by_year, aes(x = year, y = total_credited, colour = type)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = unique(transfers_by_year$year)) +
  labs(
    title    = "Canon Minero Transfers by Year",
    subtitle = "Regional vs. Municipal",
    x = "Year", y = "Transfer (millions PEN)", colour = "Level"
  ) +
  theme_minimal() +
  theme(
    plot.title   = element_text(face = "bold"),
    axis.text.x  = element_text(angle = 45, hjust = 1)
  )


## 2.3 Disparity dot-plots ----

ggplot(transferencias_regionales, aes(x = year, y = credited)) +
  geom_point(alpha = 0.5) +
  labs(title = "Transfer Disparities by Region", x = "Year", y = "Amount (millions PEN)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Very few regions receive > 50 million in any year
transferencias_regionales %>%
  group_by(year) %>%
  summarise(total = n(), over_50m = sum(credited > 50))

ggplot(transferencias_municipales, aes(x = year, y = credited)) +
  geom_point(alpha = 0.3) +
  scale_x_continuous(breaks = full_years) +
  labs(title = "Transfer Disparities by District", x = "Year", y = "Amount (millions PEN)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))


# ==============================================================================
# ---- 3. Maps of Peru --------------------------------------------------------
# ==============================================================================

## 3.1 Regional map ----

mapa_region <- mapa_region %>%
  mutate(
    REGION_UPPER = normalizar_texto(REGION),
    mineria = if_else(
      REGION_UPPER %in% unique(transferencias_regionales$name),
      "Canon Region", "Non-Canon Region"
    )
  )

# All regions receive canon — verified:
stopifnot(length(setdiff(mapa_region$REGION_UPPER,
                         unique(transferencias_regionales$name))) == 0)

ggplot(mapa_region) +
  geom_sf(aes(fill = mineria), colour = "white") +
  geom_sf(fill = NA, colour = "black", linewidth = 0.3) +
  scale_fill_manual(values = wes_palette("AsteroidCity3", 2)) +
  geom_text_repel(
    aes(x = coords_x, y = coords_y, label = REGION_UPPER),
    size = 2.5, fontface = "bold", max.overlaps = 20,
    box.padding = 0.4, segment.color = "gray60"
  ) +
  labs(title = "Canon vs. Non-Canon Regions in Peru", fill = "") +
  theme_minimal()

saveRDS(mapa_region, "./Data/Ubigeo/Mapa_Region.rds")


## 3.2 Provincial map ----

mapa_provincia <- mapa_provincia %>%
  mutate(
    PROVINCIA_UPPER = normalizar_texto(PROVINCIA),
    PROVINCIA_UPPER = if_else(
      PROVINCIA_UPPER == "PROV. CONST. DEL CALLAO", "CALLAO", PROVINCIA_UPPER
    )
  )

provincias_mineras <- unique(transferencias_provinciales$name)

mapa_provincia <- mapa_provincia %>%
  mutate(mineria = if_else(
    PROVINCIA_UPPER %in% provincias_mineras,
    "Canon Province", "Non-Canon Province"
  ))

# All provinces receive canon
stopifnot(length(setdiff(mapa_provincia$PROVINCIA_UPPER, provincias_mineras)) == 0)

ggplot(mapa_provincia) +
  geom_sf(aes(fill = mineria), colour = "white") +
  geom_sf(fill = NA, colour = "black", linewidth = 0.3) +
  scale_fill_manual(values = wes_palette("AsteroidCity3", 2)) +
  labs(title = "Canon vs. Non-Canon Provinces in Peru", fill = "") +
  theme_minimal()

saveRDS(mapa_provincia, "./Data/Ubigeo/Mapa_Provincia.rds")


## 3.3 District map ----

mapa_distrito <- mapa_distrito %>%
  mutate(
    DISTRITO = str_replace_all(DISTRITO, "[_-]", " "),
    DISTRITO_UPPER = normalizar_texto(DISTRITO),
    DISTRITO_UPPER = str_remove(DISTRITO_UPPER, "\\s*\\([^)]+\\)")
  )

distritos_mineros <- unique(transferencias_municipales$name)

mapa_distrito <- mapa_distrito %>%
  mutate(mineria = if_else(
    DISTRITO_UPPER %in% distritos_mineros,
    "Canon District", "Non-Canon District"
  ))

# 27 districts do not appear in the transfer data
n_no_canon <- length(setdiff(mapa_distrito$DISTRITO_UPPER, distritos_mineros))
message(n_no_canon, " districts without canon transfers")

# Districts present in ALL years (full panel)
distritos_full_panel <- transferencias_municipales %>%
  group_by(name) %>%
  summarise(years_present = n_distinct(year), .groups = "drop") %>%
  filter(years_present == length(full_years))

# Potential control group grows to ~419 when requiring full panel presence
n_potential_control <- length(setdiff(mapa_distrito$DISTRITO_UPPER,
                                      distritos_full_panel$name))
message("Potential control-group size (missing from full panel): ", n_potential_control)

# Control group size by year
control_by_year <- tibble(
  year = full_years,
  n_missing = sapply(full_years, function(t) {
    present <- transferencias_municipales %>%
      filter(year == t) %>%
      pull(name) %>%
      unique()
    length(setdiff(mapa_distrito$DISTRITO_UPPER, present))
  })
)
print(control_by_year)

saveRDS(mapa_distrito, "./Data/Ubigeo/Mapa_Distrito.rds")


# ==============================================================================
# ---- 4. ENAHO Survey Data ---------------------------------------------------
# ==============================================================================

enaho_path <- "./Data/ENAHO/"

## 4.1 Load all ENAHO modules into nested list ----

enaho_completo <- list()

for (year in as.character(full_years)) {
  
  work_path <- file.path(enaho_path, year)
  message("Processing year: ", year)
  
  all_files <- list.files(work_path, full.names = TRUE, recursive = TRUE)
  by_year   <- list()
  
  for (file in all_files) {
    fname <- basename(file)
    
    reader <- case_when(
      grepl("\\.dta$",  file, ignore.case = TRUE) ~ "dta",
      grepl("\\.dbf$",  file, ignore.case = TRUE) ~ "dbf",
      grepl("\\.csv$",  file, ignore.case = TRUE) ~ "csv",
      TRUE ~ NA_character_
    )
    
    if (is.na(reader)) next
    
    df <- tryCatch({
      switch(reader,
             dta = haven::read_dta(file),
             dbf = foreign::read.dbf(file, as.is = TRUE),
             csv = read.csv(file, stringsAsFactors = FALSE, fileEncoding = "latin1")
      ) %>% as.data.frame()
    }, error = function(e) {
      message("  Error reading ", fname, ": ", e$message)
      NULL
    })
    
    if (!is.null(df)) by_year[[fname]] <- df
  }
  
  enaho_completo[[year]] <- by_year
}

rm(by_year)


## 4.2 Generic module extractor ----

#' Extract a single ENAHO module from the nested list for one year.
#' @param year_name Character year (e.g. "2010")
#' @param year_list Named list of data frames for that year
#' @param module_regex Regex to identify the module file(s)
#' @param vars_keep Character vector of desired variable names
#' @param vars_chr Character variables (forced to character)
#' @param vars_num Numeric variables (forced to numeric)
#' @param merge_multiple If TRUE, merge multiple matching files by key_vars
#' @param key_vars Join keys for merging
extraer_modulo <- function(year_name, year_list, module_regex,
                           vars_keep, vars_chr, vars_num,
                           merge_multiple = FALSE,
                           key_vars = c("AÑO", "CONGLOME", "VIVIENDA",
                                        "HOGAR", "CODPERSO")) {
  
  file_matches <- names(year_list)[str_detect(names(year_list), module_regex)]
  
  if (length(file_matches) == 0) {
    warning("Module not found for year ", year_name)
    return(NULL)
  }
  
  read_and_select <- function(fname) {
    df <- as.data.frame(year_list[[fname]])
    names(df) <- toupper(names(df))
    
    available <- intersect(toupper(vars_keep), names(df))
    if (length(available) == 0) return(NULL)
    
    df <- df %>% select(any_of(available))
    
    if (!"AÑO" %in% names(df)) {
      df <- df %>% mutate(AÑO = as.character(year_name), .before = 1)
    }
    
    df %>% mutate(
      across(any_of(toupper(vars_chr)), as.character),
      across(any_of(toupper(vars_num)), ~ suppressWarnings(as.numeric(.x)))
    )
  }
  
  if (!merge_multiple) {
    return(read_and_select(file_matches[1]))
  }
  
  # Multiple files: read each and merge progressively
  dfs <- map(file_matches, read_and_select) %>% compact()
  
  if (length(dfs) == 0) return(NULL)
  if (length(dfs) == 1) return(dfs[[1]])
  
  common_keys <- Reduce(intersect, map(dfs, names))
  join_keys   <- intersect(key_vars, common_keys)
  
  if (length(join_keys) < 3) {
    warning("Too few common keys for year ", year_name)
  }
  
  Reduce(function(x, y) full_join(x, y, by = join_keys), dfs)
}


## 4.3 Module 100: Housing characteristics & NBI ----

vars_100     <- c("AÑO", "CONGLOME", "VIVIENDA", "HOGAR", "UBIGEO",
                  "ESTRATO", "RESULT", "P101", "P106A", "P110",
                  "P111", "P111A",
                  "NBI1", "NBI2", "NBI3", "NBI4", "NBI5",
                  "nbi1", "nbi2", "nbi3", "nbi4", "nbi5")
vars_100_chr <- c("AÑO", "CONGLOME", "VIVIENDA", "HOGAR", "UBIGEO")
vars_100_num <- setdiff(vars_100, vars_100_chr)

enaho_100 <- imap_dfr(enaho_completo, function(year_list, year_name) {
  extraer_modulo(year_name, year_list,
                 regex("(^|[-_])100(\\(|\\.|[-_]|$)", ignore_case = TRUE),
                 vars_100, vars_100_chr, vars_100_num)
})

# From 2012 onward P111 is renamed P111A; unify
enaho_100 <- enaho_100 %>%
  mutate(P111 = coalesce(P111, P111A)) %>%
  select(-any_of("P111A"))

# NBI variables appear in lowercase in some years; unify
for (i in 1:5) {
  upper <- paste0("NBI", i)
  lower <- paste0("nbi", i)  # already toupper'd, but coalesce handles it
  if (all(c(upper, lower) %in% names(enaho_100))) {
    enaho_100[[upper]] <- coalesce(enaho_100[[upper]], enaho_100[[lower]])
    enaho_100[[lower]] <- NULL
  }
}

saveRDS(enaho_100, "./Data/ENAHO/ENAHO_100.rds")
write.csv(enaho_100, "./Data/ENAHO/ENAHO_100.csv", row.names = FALSE)


## 4.4 Module 200: Household members & demographics ----

vars_200     <- c("AÑO", "CONGLOME", "VIVIENDA", "HOGAR", "CODPERSO",
                  "UBIGEO", "ESTRATO",
                  "P203", "P204", "P205", "P206",
                  "P207", "P208A", "P208A1", "P214")
vars_200_chr <- c("AÑO", "CONGLOME", "VIVIENDA", "HOGAR", "CODPERSO", "UBIGEO")
vars_200_num <- setdiff(vars_200, vars_200_chr)

# Note: P208A1 (migration) unavailable from 2018 onward
enaho_200 <- imap_dfr(enaho_completo, function(year_list, year_name) {
  extraer_modulo(year_name, year_list,
                 regex("(^|[-_])200(\\.|[-_])", ignore_case = TRUE),
                 vars_200, vars_200_chr, vars_200_num)
})

saveRDS(enaho_200, "./Data/ENAHO/ENAHO_200.rds")
write.csv(enaho_200, "./Data/ENAHO/ENAHO_200.csv", row.names = FALSE)


## 4.5 Module 300: Education ----

vars_300     <- c("AÑO", "CONGLOME", "VIVIENDA", "HOGAR", "CODPERSO",
                  "UBIGEO", "ESTRATO", "CODINFOR",
                  "P300A", "P301A", "P301B", "P301C",
                  "P302", "P306", "P307", "P313", "P313A",
                  "P203", "P207", "P208A", "T313", "T313A")
vars_300_chr <- c("AÑO", "CONGLOME", "VIVIENDA", "HOGAR", "CODPERSO", "UBIGEO")
vars_300_num <- setdiff(vars_300, vars_300_chr)

enaho_300 <- imap_dfr(enaho_completo, function(year_list, year_name) {
  extraer_modulo(year_name, year_list,
                 regex("(^|[-_])300(\\.|[-_])", ignore_case = TRUE),
                 vars_300, vars_300_chr, vars_300_num)
})

enaho_300 <- enaho_300 %>%
  mutate(P313 = coalesce(P313, P313A),
         T313 = coalesce(T313, T313A)) %>%
  select(-any_of(c("P313A", "T313A")))

saveRDS(enaho_300, "./Data/ENAHO/ENAHO_300.rds")
write.csv(enaho_300, "./Data/ENAHO/ENAHO_300.csv", row.names = FALSE)


## 4.6 Module 500: Employment ----

vars_500     <- c("AÑO", "CONGLOME", "VIVIENDA", "HOGAR", "CODPERSO",
                  "UBIGEO", "ESTRATO", "CODINFOR",
                  "P501", "P502", "P503", "P505", "P505B", "P506", "P507",
                  "P510A", "P510A1", "P513T", "P5118",
                  "D524A1", "D530A", "P5566A", "P5566B", "P5566C",
                  "P524A1", "P530A", "P207", "P208A", "P301A", "OCU500")
vars_500_chr <- c("AÑO", "CONGLOME", "VIVIENDA", "HOGAR", "CODPERSO", "UBIGEO")
vars_500_num <- setdiff(vars_500, vars_500_chr)

# Module 500 sometimes split across multiple files; merge them
enaho_500 <- imap_dfr(enaho_completo, function(year_list, year_name) {
  extraer_modulo(year_name, year_list,
                 regex("(^|[-_])500(\\(|\\.|[-_]|$)", ignore_case = TRUE),
                 vars_500, vars_500_chr, vars_500_num,
                 merge_multiple = TRUE)
})

enaho_500 <- enaho_500 %>%
  mutate(P510A = coalesce(P510A, P510A1)) %>%
  select(-any_of("P510A1"))

saveRDS(enaho_500, "./Data/ENAHO/ENAHO_500.rds")
write.csv(enaho_500, "./Data/ENAHO/ENAHO_500.csv", row.names = FALSE)


## 4.7 Sumaria: Household income, expenditure, poverty ----

vars_sumaria     <- c("CONGLOME", "VIVIENDA", "HOGAR", "UBIGEO",
                      "FACTOR07", "DOMINIO", "MIEPERHO", "PERCEPHO",
                      "INGBRUHD", "INGHOG2D", "GASHOG1D", "GASHOG2D",
                      "POBREZA")
vars_sumaria_chr <- c("CONGLOME", "VIVIENDA", "HOGAR", "UBIGEO")
vars_sumaria_num <- setdiff(vars_sumaria, vars_sumaria_chr)

extraer_sumaria <- function(year_name, year_list, vars_keep) {
  
  candidates <- names(year_list)[str_detect(names(year_list),
                                            regex("sumaria", ignore_case = TRUE))]
  
  # Priority: exact annual file > any file with year > first available
  file_name <- candidates[str_detect(
    candidates,
    regex(paste0("^sumaria[-_]", year_name, "\\.[A-Za-z0-9]+$"), ignore_case = TRUE)
  )][1]
  
  if (is.na(file_name)) {
    file_name <- candidates[
      str_detect(candidates, year_name) &
        !str_detect(candidates, paste0(year_name, "[-_]\\d+"))
    ][1]
  }
  
  if (is.na(file_name)) file_name <- candidates[1]
  
  if (is.na(file_name)) {
    warning("SUMARIA not found for year ", year_name)
    return(NULL)
  }
  
  message("  Using: ", file_name, " for year ", year_name)
  
  df <- as.data.frame(year_list[[file_name]])
  names(df) <- toupper(names(df))
  
  # Automatically detect LINPE* and LINEA* columns (poverty lines vary by year)
  linpe_vars <- names(df)[str_detect(names(df), "^LINPE")]
  linea_vars <- names(df)[str_detect(names(df), "^LINEA")]
  
  vars_final    <- unique(c(toupper(vars_keep), linpe_vars, linea_vars))
  vars_available <- intersect(vars_final, names(df))
  
  if (length(vars_available) == 0) {
    warning("No variables found in SUMARIA for ", year_name)
    return(NULL)
  }
  
  df_out <- df %>%
    select(any_of(vars_available)) %>%
    mutate(AÑO = as.character(year_name), .before = 1) %>%
    mutate(
      across(any_of(toupper(vars_sumaria_chr)), as.character),
      across(-any_of(c("AÑO", toupper(vars_sumaria_chr))),
             ~ suppressWarnings(as.numeric(.x)))
    )
  
  # Unify LINPE and LINEA into single columns
  if (length(linpe_vars) > 0) {
    df_out$LINPE <- coalesce(!!!df_out[linpe_vars])
    df_out <- df_out %>% select(-any_of(setdiff(linpe_vars, "LINPE")))
  }
  
  if (length(linea_vars) > 0) {
    df_out$LINEA <- coalesce(!!!df_out[linea_vars])
    df_out <- df_out %>% select(-any_of(setdiff(linea_vars, "LINEA")))
  }
  
  df_out
}

enaho_sumaria <- imap_dfr(enaho_completo, ~ extraer_sumaria(.y, .x, vars_sumaria))

saveRDS(enaho_sumaria, "./Data/ENAHO/ENAHO_sumaria.rds")
write.csv(enaho_sumaria, "./Data/ENAHO/ENAHO_sumaria.csv", row.names = FALSE)


# ==============================================================================
# ---- 5. Exchange Rate -------------------------------------------------------
# ==============================================================================

exchange_rate <- read_xlsx(
  "./Data/Exchange Rate PEN-USD/PEN-USD 02-24.xlsx",
  sheet = "Anuales"
) %>%
  slice(-(1:3)) %>%
  rename(year = 1, pen_usd = 2)

saveRDS(exchange_rate, "./Data/Exchange Rate PEN-USD/EX.rds")


# ==============================================================================
# ---- 6. Mining Site Production -----------------------------------------------
# ==============================================================================

mining_path <- "./Data/Mining Site/Produccion Minera MEF"

## 6.1 Load and set column names ----

mining_site_raw <- read_xlsx(file.path(mining_path, "mining_site_resumen.xlsx"))

# Row 2 contains the actual headers
col_names <- mining_site_raw[2, ] %>%
  unlist() %>%
  as.character() %>%
  normalizar_texto()

names(mining_site_raw) <- col_names
mining_site <- mining_site_raw %>%
  slice(-(1:3)) %>%
  as_tibble()


## 6.2 Clean text columns ----

mining_site <- mining_site %>%
  mutate(
    PROCESO       = normalizar_texto(PROCESO),
    CLASIFICACION = normalizar_texto(CLASIFICACION)
  )

# Standardise company names: normalise legal suffixes
mining_site <- mining_site %>%
  mutate(
    TITULAR = TITULAR %>%
      as.character() %>%
      str_squish() %>%
      str_to_upper() %>%
      stri_trans_general("Latin-ASCII") %>%
      # Collapse variant legal abbreviations
      str_replace_all("S\\s*\\.\\s*A\\s*\\.\\s*A\\s*\\.", "SAA") %>%
      str_replace_all("S\\s*\\.\\s*A\\s*\\.\\s*C\\s*\\.", "SAC") %>%
      str_replace_all("S\\s*\\.\\s*R\\s*\\.\\s*L\\s*\\.", "SRL") %>%
      str_replace_all("S\\s*\\.\\s*A\\s*\\.",              "SA")  %>%
      str_replace_all("E\\s*\\.\\s*I\\s*\\.\\s*R\\s*\\.\\s*L\\s*\\.", "EIRL") %>%
      str_replace_all("S\\s*\\.\\s*M\\s*\\.\\s*R\\s*\\.\\s*L\\s*\\.", "SMRL") %>%
      str_replace_all("SOCIEDAD ANONIMA CERRADA",                      "SAC")  %>%
      str_replace_all("SOCIEDAD ANONIMA",                              "SA")   %>%
      str_replace_all("SOCIEDAD COMERCIAL DE RESPONSABILIDAD LIMITADA","SRL")  %>%
      str_replace_all("EMPRESA INDIVIDUAL DE RESPONSABILIDAD LIMITADA","EIRL") %>%
      # Remove liquidation / branch suffixes
      str_remove("\\s+EN LIQUIDACION( EN MARCHA)?$") %>%
      str_remove("\\s+SUCURSAL DEL PERU$") %>%
      str_squish() %>%
      na_if("")
  )


## 6.3 Clean mining unit names ----

limpiar_unidad <- function(x) {
  x %>%
    normalizar_texto() %>%
    str_replace_all("N[°º\\.]\\s*", "N ") %>%
    str_remove_all("\\b[A-Z]\\)") %>%
    str_remove_all("\\([^)]*\\)") %>%
    str_replace_all("[-–—]+", " ") %>%
    str_squish() %>%
    na_if("")
}

# Collapse unit numbering (N 1, N 2, roman numerals, etc.) — irrelevant at
# district-year level per manual inspection.
colapsar_unidad <- function(x) {
  x %>%
    str_squish() %>%
    str_remove("\\s+N\\s+\\d+[A-Z]?$") %>%
    str_remove("\\s+\\d+[A-Z]?$") %>%
    str_remove("\\s+(I|II|III|IV|V|VI|VII|VIII|IX|X)$") %>%
    str_remove("\\s+(UNO|DOS|TRES|CUATRO|CINCO|SEIS|SIETE|OCHO|NUEVE|DIEZ)$") %>%
    str_squish() %>%
    na_if("")
}

mining_site <- mining_site %>%
  mutate(
    UNIDAD_CLEAN = limpiar_unidad(UNIDAD) %>% colapsar_unidad()
  )


## 6.4 Standardise geographic columns ----

mining_site <- mining_site %>%
  mutate(
    REGION_RAW    = REGION,
    REGION_STD    = normalizar_texto(REGION),
    PROVINCIA_RAW = PROVINCIA,
    PROVINCIA_STD = normalizar_texto(PROVINCIA),
    DISTRITO_RAW  = DISTRITO,
    DISTRITO_STD  = normalizar_texto(DISTRITO)
  )

ubigeo_region <- ubigeo_region %>%
  mutate(departamento_std = normalizar_texto(departamento))

ubigeo_provincia <- ubigeo_provincia %>%
  mutate(provincia_std = normalizar_texto(provincia))

ubigeo_distrito <- ubigeo_distrito %>%
  mutate(distrito_std = normalizar_texto(distrito))


## 6.5 Regions without mining sites ----

# The 6 regions absent from mining_site are precisely the 6 with the lowest
# cumulative canon transfers 2004–2024. They can be excluded without
# material impact on the analysis.
regiones_sin_mining <- setdiff(
  sort(unique(ubigeo_region$departamento_std)),
  sort(unique(mining_site$REGION_STD))
)
message("Regions without mining sites: ", paste(regiones_sin_mining, collapse = ", "))

# Cumulative regional transfers for map
transferencias_acumulado <- transferencias_regionales %>%
  group_by(name) %>%
  summarise(authorised_acum = sum(authorised, na.rm = TRUE), .groups = "drop") %>%
  rename(REGION_UPPER = name)

mapa_region <- mapa_region %>%
  left_join(transferencias_acumulado, by = "REGION_UPPER") %>%
  mutate(mining_site_match = if_else(
    REGION_UPPER %in% regiones_sin_mining,
    "No match", "Match"
  ))

pal_wes <- colorRampPalette(wes_palette("Zissou1", 5))(100)

ggplot(mapa_region) +
  geom_sf(aes(fill = log(authorised_acum)), colour = "white", linewidth = 0.2) +
  geom_sf(fill = NA, colour = "black", linewidth = 0.25) +
  geom_text_repel(
    data = mapa_region %>% st_drop_geometry() %>%
      filter(mining_site_match == "No match"),
    aes(x = coords_x, y = coords_y, label = REGION_UPPER),
    size = 3, fontface = "bold", max.overlaps = 30, segment.color = "gray50"
  ) +
  scale_fill_gradientn(
    colours = pal_wes, na.value = "grey90",
    name = "log(Authorised\nacum. 2004-2024)"
  ) +
  labs(
    title    = "Cumulative Canon Transfers by Region",
    subtitle = "Labels: regions absent from mining-site data"
  ) +
  theme_minimal()


## 6.6 Provinces & districts without mining sites ----

# Provinces: Tarata (Tacna) has high transfers despite no mining site.
# Worth investigating whether hat(CM) can predict this. <- TODO
provincias_sin_mining <- setdiff(
  unique(ubigeo_provincia$provincia_std),
  unique(mining_site$PROVINCIA_STD)
)

transferencias_provinciales %>%
  mutate(provincia_std = normalizar_texto(name)) %>%
  filter(provincia_std %in% provincias_sin_mining) %>%
  arrange(desc(authorised)) %>%
  select(provincia_std, authorised, year) %>%
  head(20)

# Districts: those absent from mining_site have relatively low transfers
distritos_sin_mining <- setdiff(
  unique(ubigeo_distrito$distrito_std),
  unique(mining_site$DISTRITO_STD)
)

transferencias_municipales %>%
  mutate(distrito_std = normalizar_texto(name)) %>%
  filter(distrito_std %in% distritos_sin_mining) %>%
  arrange(desc(authorised)) %>%
  select(distrito_std, authorised, year) %>%
  head(20)


## 6.7 Parse monthly production and reshape ----

month_names_de <- c("JANUAR", "FEBRUAR", "MARZ", "APRIL", "MAI", "JUNI",
                    "JULI", "AUGUST", "SEPTEMBER", "OKTOBER", "NOVEMBER", "DEZEMBER")
month_abbr_en  <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN",
                    "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")

mining_site <- mining_site %>%
  mutate(across(all_of(c(month_names_de, "TOTAL GENERAL")),
                ~ round(as.numeric(.x), 6)))

# Verify row-sum matches TOTAL GENERAL, then drop it
mining_site <- mining_site %>%
  mutate(TOTAL = rowSums(select(., all_of(month_names_de)), na.rm = TRUE)) %>%
  select(-`TOTAL GENERAL`)

# Rename months from German to English abbreviations
names(mining_site) <- names(mining_site) %>%
  {setNames(month_abbr_en, month_names_de)[.] %|% .}  # replace if match, keep otherwise

# Manual rename since the piped approach above won't work simply — use recode
names(mining_site)[names(mining_site) %in% month_names_de] <- month_abbr_en

# Pivot to long format
mining_site_long <- mining_site %>%
  pivot_longer(
    cols      = all_of(month_abbr_en),
    names_to  = "month",
    values_to = "production"
  ) %>%
  mutate(month_num = match(month, month_abbr_en))

# Aggregate to district-year level
mining_site_distrito <- mining_site_long %>%
  group_by(ANO, DISTRITO_STD) %>%
  summarise(produccion_total = sum(production, na.rm = TRUE), .groups = "drop")


## 6.8 Export mining data ----

saveRDS(mining_site,          file.path(mining_path, "Mining_Site.rds"))
saveRDS(mining_site_long,     file.path(mining_path, "Mining_Site_long.rds"))
saveRDS(mining_site_distrito, file.path(mining_path, "Mining_Site_distrito.rds"))

write.csv(mining_site,          file.path(mining_path, "Mining_Site.csv"), row.names = FALSE)
write.csv(mining_site_long,     file.path(mining_path, "Mining_Site_long.csv"), row.names = FALSE)
write.csv(mining_site_distrito, file.path(mining_path, "Mining_Site_distrito.csv"), row.names = FALSE)


# ==============================================================================
# End of 01_data_preparation.R
# ==============================================================================