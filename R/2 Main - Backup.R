library(tidyverse)
library(readxl)
library(fixest)     # efectos fijos + IV (reemplaza plm/AER)
library(acid)       # para Gini con pesos
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
library(ivmodel)

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

pad6 <- function(x) str_pad(as.character(x), width = 6, side = "left", pad = "0")

short_years <- sprintf("%02d", 4:24)     # "04" to "24"

full_years <- 2004:2024                 # 2004 to 2024
pre_years  <- 2004:2006

gini_weighted <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0 & x >= 0
  if (sum(ok) < 2) return(NA_real_)
  x <- x[ok]; w <- w[ok]  # Elimina hogares con ingreso o peso faltante, 
                          # peso cero, o ingreso negativo. Si quedan menos de 2
                          # hogares válidos, devuelve NA (no se puede calcular Gini)
  ord <- order(x); x <- x[ord]; w <- w[ord] # Ordenar por ingreso de menor a mayor
                                            # Para Lorenz-Kurve
  W  <- sum(w); XW <- sum(x * w)  # W normaliza la distribución de población
                                  # y XW normaliza la distribución de ingreso
  if (W == 0 || XW == 0) return(NA_real_)
  cw  <- cumsum(w) / W  # fracción acumulada de población (eje X)
  cxw <- cumsum(x * w) / XW   # # fracción acumulada de ingreso (eje Y)
  area <- sum(diff(c(0, cw)) * (c(0, cxw[-length(cxw)]) + cxw) / 2)
  1 - 2 * area
} # Survey-weighted Gini coefficient via Lorenz-curve trapezoidal rule


##################################
###         Load Data          ###
##################################

###Transferencias###

Transferencias_Municipales <- read_rds("./Data/Transferencias_Municipales.rds")
Transferencias_Provinciales <- read_rds("./Data/Transferencias_Provinciales.rds")
Transferencias_Regionales <- read_rds("./Data/Transferencias_Regionales.rds")

###ENAHO###

ENAHO_100 <- read_rds("./Data/ENAHO_100.rds")         # NBI, servicios
ENAHO_200 <- read_rds("./Data/ENAHO_200.rds")         # demografía
ENAHO_300 <- read_rds("./Data/ENAHO_300.rds")         # educación
ENAHO_500 <- read_rds("./Data/ENAHO_500.rds")         # empleo
ENAHO_sumaria <- read_rds("./Data/ENAHO_sumaria.rds") # ingreso, gasto, pobreza

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
Mining_Site <- read_rds("./Data/Mining_Site.rds")


### All Mineral Prices ###

All_Prices_long <- read_rds("./Data/All_Prices_long.rds")

# ==============================================================================
# ---- JOIN DATA: Ubigeo matching — Transferencias ----------------------------
# ==============================================================================

# ---- Ubigeo_Master (base compartida por los tres matches) -------------------

Ubigeo_Master <- Mapa_Distrito %>%
  st_drop_geometry() %>%
  transmute(
    ubigeo6       = pad6(COD_DISTRITO),
    cod_provincia = pad6(COD_PROVINCIA),
    cod_region    = pad6(COD_REGION),
    region_std    = normalizar_texto(REGION),
    provincia_std = normalizar_texto(PROVINCIA),
    distrito_std  = normalizar_texto(DISTRITO)
  ) %>%
  distinct()

stopifnot(
  sum(duplicated(Ubigeo_Master$ubigeo6)) == 0,
  sum(nchar(Ubigeo_Master$ubigeo6) != 6)  == 0
)

# ==============================================================================
# ---- 1. Transferencias_Municipales → ubigeo6 (3 pasos) ----------------------
# ==============================================================================

transf_keys <- Transferencias_Municipales %>%
  select(-any_of(c("ubigeo6", "match_method", "provincia_real", "depto_real"))) %>%
  mutate(
    code          = as.character(code),
    year          = as.integer(year),
    region_std     = normalizar_texto(depto),
    provincia_std = normalizar_texto(province),
    distrito_std  = normalizar_texto(name)
  )

# Paso 1 — match fuerte (provincia, distrito)
m1 <- transf_keys %>%
  left_join(
    Ubigeo_Master %>% select(ubigeo6, region_std, provincia_std, distrito_std),
    by = c("region_std", "provincia_std", "distrito_std")
  ) %>%
  mutate(match_method = if_else(!is.na(ubigeo6), "prov_dist", NA_character_))

# Paso 2 — distrito único en Perú
dist_unique <- Ubigeo_Master %>%
  count(distrito_std, name = "n") %>%
  filter(n == 1) %>%
  left_join(Ubigeo_Master %>% select(ubigeo6, distrito_std), by = "distrito_std") %>%
  select(distrito_std, ubigeo6_uniq = ubigeo6)

m2 <- m1 %>%
  left_join(dist_unique, by = "distrito_std") %>%
  mutate(
    ubigeo6      = coalesce(ubigeo6, ubigeo6_uniq),
    match_method = coalesce(match_method,
                            if_else(!is.na(ubigeo6_uniq), "dist_unique", NA_character_))
  ) %>%
  select(-ubigeo6_uniq)

# Paso 3 — code crosswalk (solo filas donde provincia cruda = provincia real
#          del ubigeo, para excluir contaminación de fill-bugs como TARATA 2015)
code_xwalk <- m2 %>%
  filter(!is.na(ubigeo6)) %>%
  left_join(
    Ubigeo_Master %>% select(ubigeo6, true_prov = provincia_std),
    by = "ubigeo6"
  ) %>%
  filter(provincia_std == true_prov) %>%
  count(code, ubigeo6, name = "n_obs") %>%
  group_by(code) %>%
  slice_max(n_obs, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(code, ubigeo6_code = ubigeo6)

m3 <- m2 %>%
  left_join(code_xwalk, by = "code") %>%
  mutate(
    ubigeo6      = coalesce(ubigeo6, ubigeo6_code),
    match_method = coalesce(match_method,
                            if_else(!is.na(ubigeo6_code), "code_xwalk", "unmatched"))
  ) %>%
  select(-ubigeo6_code) %>%
  left_join(
    Ubigeo_Master %>% select(ubigeo6,
                             region_real = region_std,
                             provincia_real = provincia_std,
                             distrito_real     = distrito_std),
    by = "ubigeo6"
  )

Transferencias_Municipales <- m3 %>%
  transmute(
    ubigeo6,
    year,
    credited,
    authorised,
    code,
    region_real,
    provincia_real,
    distrito_real,
    distrito_raw  = name,
    provincia_raw = province,
    match_method
  )

# ==============================================================================
# ---- 2. Transferencias_Regionales → cod_region (corregido) ------------------
# ==============================================================================

# Lookup directo: no colapsar Lima — Ubigeo_Master ya distingue
# LIMA METROPOLITANA (metro) y LIMA PROVINCIAS (resto), que son receptores
# separados del canon regional.
region_lookup <- Ubigeo_Master %>%
  distinct(cod_region, region_std)

Transferencias_Regionales <- Transferencias_Regionales %>%
  select(-any_of(c("cod_region", "region_std_clean", "match_method"))) %>%
  mutate(
    region_std_clean = normalizar_texto(name)
  ) %>%
  left_join(region_lookup, by = c("region_std_clean" = "region_std")) %>%
  mutate(
    match_method = if_else(!is.na(cod_region), "name_match", "unmatched")
  )

# QA Regional
qa_reg <- Transferencias_Regionales %>%
  group_by(match_method) %>%
  summarise(
    n          = n(),
    n_regiones = n_distinct(name),
    credited   = sum(credited, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  mutate(pct_credited = credited / sum(credited))
print(qa_reg)

Transferencias_Regionales %>%
  filter(match_method == "unmatched") %>%
  distinct(name, region_std_clean) %>%
  print()


# ==============================================================================
# ---- 3. Transferencias_Provinciales → cod_provincia -------------------------
# ==============================================================================

prov_lookup <- Ubigeo_Master %>%
  distinct(cod_provincia, provincia_std, region_std)

prov_dupes <- prov_lookup %>% count(provincia_std) %>% filter(n > 1)

Transferencias_Provinciales <- Transferencias_Provinciales %>%
  select(-any_of(c("cod_provincia", "region_std_clean", "match_method"))) %>%
  mutate(
    prov_std_clean   = normalizar_texto(name),
    region_std_clean = normalizar_texto(depto)   # viene de Import.R
  ) %>%
  left_join(
    prov_lookup,
    by = c("prov_std_clean" = "provincia_std",
           "region_std_clean" = "region_std")    # doble clave elimina homónimos
  ) %>%
  mutate(
    match_method = if_else(!is.na(cod_provincia), "name_region_match", "unmatched")
  )

# ==============================================================================
# ---- QA: los tres datasets --------------------------------------------------
# ==============================================================================

cat("=== QA MUNICIPAL ===\n")

qa_mun <- Transferencias_Municipales %>%
  group_by(match_method) %>%
  summarise(
    n          = n(),
    n_ubigeos  = n_distinct(ubigeo6),
    credited   = sum(credited, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  mutate(pct_credited = credited / sum(credited))
print(qa_mun)

# Pendientes
Transferencias_Municipales %>%
  filter(match_method == "unmatched") %>%
  count(provincia_raw, distrito_raw, sort = TRUE) %>%
  print(n = 20)

# Unicidad (gates: 0 filas)
stopifnot(
  nrow(Transferencias_Municipales %>%
         distinct(code, ubigeo6) %>% count(code) %>% filter(n > 1)) == 0,
  nrow(Transferencias_Municipales %>%
         count(ubigeo6, year) %>% filter(n > 1)) == 0
)
cat("Unicidad Municipal: OK\n")

# Consistencia temporal por departamento
district_coverage <- Transferencias_Municipales %>%
  filter(match_method != "unmatched") %>%
  group_by(region_real, year) %>%
  summarise(n_districts = n_distinct(ubigeo6), .groups = "drop") %>%
  group_by(region_real) %>%
  mutate(
    median_n      = median(n_districts),
    pct_of_median = n_districts / median_n
  ) %>%
  ungroup()

cat("\nDepartamentos con cobertura < 70% de su mediana (excl. datos fuente MEF):\n")
district_coverage %>%
  filter(pct_of_median < 0.70, median_n > 3) %>%
  arrange(pct_of_median) %>%
  print(n = 20)

cat("\n=== QA REGIONAL ===\n")

qa_reg <- Transferencias_Regionales %>%
  group_by(match_method) %>%
  summarise(
    n          = n(),
    n_regiones = n_distinct(name),
    credited   = sum(credited, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  mutate(pct_credited = credited / sum(credited))
print(qa_reg)

unmatched_reg <- Transferencias_Regionales %>%
  filter(match_method == "unmatched") %>%
  distinct(name, region_std_clean)
if (nrow(unmatched_reg) > 0) {
  cat("Regiones sin match:\n"); print(unmatched_reg)
} else {
  cat("Sin unmatched regionales.\n")
}

cat("\n=== QA PROVINCIAL ===\n")

if (nrow(prov_dupes) > 0) {
  cat("AVISO — Provincias homónimas (pueden crear duplicados de panel):\n")
  print(prov_dupes)
}

qa_prov <- Transferencias_Provinciales %>%
  group_by(match_method) %>%
  summarise(
    n            = n(),
    n_provincias = n_distinct(name),
    credited     = sum(credited, na.rm = TRUE),
    .groups      = "drop"
  ) %>%
  mutate(pct_credited = credited / sum(credited))
print(qa_prov)

Transferencias_Provinciales %>%
  filter(match_method == "unmatched") %>%
  distinct(name, prov_std_clean) %>%
  { if (nrow(.) > 0) { cat("Provincias sin match:\n"); print(.) } else cat("Sin unmatched provinciales.\n") }

dup_prov_panel <- Transferencias_Provinciales %>%
  count(name, year) %>% filter(n > 1)
if (nrow(dup_prov_panel) > 0) {
  cat("AVISO — Duplicados de panel por homónimos:\n"); print(dup_prov_panel)
}

# ==============================================================================
# ---- Guardar ----------------------------------------------------------------
# ==============================================================================

#saveRDS(Transferencias_Municipales,  "./Data/Transferencias_Municipales.rds")
#saveRDS(Transferencias_Regionales,   "./Data/Transferencias_Regionales.rds")
#saveRDS(Transferencias_Provinciales, "./Data/Transferencias_Provinciales.rds")

#cat("\nGuardado — Municipal:", nrow(Transferencias_Municipales), "filas\n")
#cat("Guardado — Regional:",  nrow(Transferencias_Regionales),  "filas\n")
  cat("Guardado — Provincial:", nrow(Transferencias_Provinciales), "filas\n")


# ==============================================================================
# ---- Mining_Site_long → ubigeo6 (triple llave) ------------------------------
# ==============================================================================

# ubigeo_mining: colapsa Lima Metropolitana + Lima Provincias → LIMA
# (Mining_Site usa "LIMA" para toda la región Lima)
ubigeo_mining <- Ubigeo_Master %>%
  mutate(
    region_std   = if_else(
      region_std %in% c("LIMA METROPOLITANA", "LIMA PROVINCIAS"),
      "LIMA", region_std
    ),
    distrito_std = str_remove(distrito_std, "\\s*\\([^)]+\\)") %>%
      str_replace_all("-", " ") %>%
      str_squish()
  ) %>%
  distinct(ubigeo6, region_std, provincia_std, distrito_std)

prov_alias_mining <- c("NAZCA" = "NASCA")

mining_match <- Mining_Site_long %>%
  mutate(
    year     = as.integer(ANO),
    reg_std  = normalizar_texto(REGION_STD),
    prov_std = normalizar_texto(PROVINCIA_STD) %>% recode(!!!prov_alias_mining),
    dist_std = normalizar_texto(DISTRITO_STD) %>%
      str_remove("\\s*\\([^)]+\\)") %>%
      str_replace_all("-", " ") %>%
      str_squish(),
    # Correcciones de nombre de distrito
    dist_std = case_when(
      prov_std == "ESPINAR" & dist_std == "YAURI" ~ "ESPINAR",  # nombre histórico
      prov_std == "NASCA"   & dist_std == "NAZCA" ~ "NASCA",    # ortografía MEF
      TRUE                                        ~ dist_std
    ),
    # Corrección de provincia: OYON está en prov. OYON, no en CAJATAMBO
    prov_std = case_when(
      reg_std == "LIMA" & prov_std == "CAJATAMBO" & dist_std == "OYON" ~ "OYON",
      TRUE                                                              ~ prov_std
    )
  ) %>%
  left_join(
    ubigeo_mining %>% select(ubigeo6, region_std, provincia_std, distrito_std),
    by = c("reg_std"  = "region_std",
           "prov_std" = "provincia_std",
           "dist_std" = "distrito_std")
  ) %>%
  mutate(match_method = if_else(!is.na(ubigeo6), "triple_key", "unmatched"))

# ---- QA Mining --------------------------------------------------------------

qa_mining <- mining_match %>%
  group_by(match_method) %>%
  summarise(
    n_rows      = n(),
    n_distritos = n_distinct(paste(reg_std, prov_std, dist_std)),
    revenue     = sum(revenue_usd, na.rm = TRUE),
    .groups     = "drop"
  ) %>%
  mutate(pct_revenue = revenue / sum(revenue))
print(qa_mining)

unmatched_mining <- mining_match %>%
  filter(match_method == "unmatched") %>%
  group_by(reg_std, prov_std, dist_std) %>%
  summarise(n = n(), revenue = sum(revenue_usd, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(revenue))
print(unmatched_mining, n = 50)


# ==============================================================================
# ---- Decisión de ETAPA para instrumento Bartik ------------------------------
# ==============================================================================
# Se usa ÚNICAMENTE Concentración para construir el instrumento Bartik.
# Justificación:
#   1. Concentración representa el 82.4% del revenue total y cubre 239 de 241
#      distritos mineros — es la etapa geográficamente representativa.
#   2. Refinación (10.6%) y Fundición (7.0%) operan en un número reducido de
#      plantas industriales (23 y 10 distritos respectivamente) y corresponden
#      al mismo mineral contabilizado más adelante en la cadena productiva.
#   3. El check siguiente confirma que 58 combinaciones distrito × año × mineral
#      aparecen en más de una etapa: agregar todas las ETAPAs inflaría el revenue.

# --- Check de double-counting entre ETAPAs -----------------------------------
n_doble_conteo <- Mining_Site_long %>%
  distinct(DISTRITO_STD, PROVINCIA_STD, ANO, mineral, ETAPA) %>%
  count(DISTRITO_STD, PROVINCIA_STD, ANO, mineral) %>%
  filter(n > 1) %>%
  nrow()
stopifnot(n_doble_conteo > 0)   # confirma que el problema existe
cat("Combinaciones distrito×año×mineral con >1 ETAPA:", n_doble_conteo, "\n")

# --- Cobertura por etapa (referencia) ----------------------------------------
Mining_Site_long %>%
  group_by(ETAPA) %>%
  summarise(
    n_distritos = n_distinct(paste(DISTRITO_STD, PROVINCIA_STD)),
    revenue     = sum(revenue_usd, na.rm = TRUE),
    .groups     = "drop"
  ) %>%
  mutate(pct_revenue = revenue / sum(revenue) * 100) %>%
  arrange(desc(revenue)) %>%
  print()


# ==============================================================================
# ---- Guardar ----------------------------------------------------------------
# ==============================================================================

Mining_Site <- mining_match %>%
  transmute(
    ubigeo6,
    year,
    ETAPA,
    mineral,
    revenue_usd,
    match_method,
    reg_raw   = REGION_STD,
    prov_raw  = PROVINCIA_STD,
    dist_raw  = DISTRITO_STD
  )

saveRDS(Mining_Site, "./Data/Mining_Site.rds")

# QA final
cat("Filas totales:", nrow(Mining_Site), "\n")
cat("Distritos únicos:", n_distinct(Mining_Site$ubigeo6), "\n")
cat("Unmatched:", sum(Mining_Site$match_method == "unmatched"), "\n")
Mining_Site %>% count(ETAPA)


# ==============================================================================
# ---- ENAHO: diagnóstico de cobertura ubigeo ---------------------------------
# ==============================================================================

# pad6 lo tenemos de Main.R
pad6 <- function(x) str_pad(as.character(x), 6, "left", "0")

enaho_s <- readRDS("./Data/ENAHO_sumaria.rds")

cat("=== Columnas de ENAHO_sumaria ===\n")
print(names(enaho_s))

cat("\n=== Años disponibles ===\n")
print(table(enaho_s$AÑO))

cat("\n=== Muestra de UBIGEO raw ===\n")
print(head(sort(unique(enaho_s$UBIGEO)), 20))

# Padear a 6 dígitos
enaho_s <- enaho_s %>%
  mutate(
    ubigeo6 = pad6(UBIGEO),
    year    = as.integer(AÑO)
  )

cat("\n=== Verificación: todos ubigeo6 tienen 6 chars? ===\n")
print(table(nchar(enaho_s$ubigeo6)))

# ¿Cuántos ubigeos distintos hay en ENAHO?
cat("\n=== Ubigeos únicos en ENAHO ===\n")
cat("Total:", n_distinct(enaho_s$ubigeo6), "\n")

# Cruzar con Ubigeo_Master
cat("\n=== Match contra Ubigeo_Master ===\n")
cat("En Ubigeo_Master:", n_distinct(Ubigeo_Master$ubigeo6), "distritos\n")
cat("ENAHO ubigeos en Ubigeo_Master:", 
    sum(unique(enaho_s$ubigeo6) %in% Ubigeo_Master$ubigeo6), "\n")
cat("ENAHO ubigeos NO en Ubigeo_Master:", 
    sum(!unique(enaho_s$ubigeo6) %in% Ubigeo_Master$ubigeo6), "\n")

# ¿Cuántos distritos-año en ENAHO?
enaho_cobertura <- enaho_s %>%
  group_by(ubigeo6, year) %>%
  summarise(n_hogares = n(), .groups = "drop")

cat("\n=== Cobertura distrital por año (cuántos distritos tienen datos ENAHO) ===\n")
enaho_cobertura %>%
  count(year, name = "n_distritos") %>%
  print(n = 30)

# ---- 1. Identificar los 5 ubigeos no matcheados ----------------------------
enaho_sin_match <- enaho_s %>%
  filter(!ubigeo6 %in% Ubigeo_Master$ubigeo6) %>%
  group_by(ubigeo6) %>%
  summarise(
    n_hogares = n(),
    years     = paste(sort(unique(year)), collapse = ", "),
    .groups   = "drop"
  )
print(enaho_sin_match)

# ---- 2. Agregar ENAHO a nivel distrito × año --------------------------------
# Usar FACTOR07 como peso (diseño muestral ENAHO)
ENAHO_panel <- enaho_s %>%
  filter(ubigeo6 %in% Ubigeo_Master$ubigeo6) %>%          # excluir 5 sin match
  group_by(ubigeo6, year) %>%
  summarise(
    n_hogares        = n(),
    ingbruhd_mean    = weighted.mean(INGBRUHD,  FACTOR07, na.rm = TRUE),
    inghog2d_mean    = weighted.mean(INGHOG2D,  FACTOR07, na.rm = TRUE),
    gashog2d_mean    = weighted.mean(GASHOG2D,  FACTOR07, na.rm = TRUE),
    pct_pobre        = weighted.mean(POBREZA == 1, FACTOR07, na.rm = TRUE),
    pct_pobre_ext    = weighted.mean(POBREZA == 2, FACTOR07, na.rm = TRUE),
    .groups          = "drop"
  )

# QA rápido
cat("Filas ENAHO_panel:", nrow(ENAHO_panel), "\n")
cat("Distritos únicos:", n_distinct(ENAHO_panel$ubigeo6), "\n")
cat("Años únicos:", n_distinct(ENAHO_panel$year), "\n")

# Cobertura: % de distrito-año con al menos 10 hogares
cat("Celdas con >= 10 hogares:",
    mean(ENAHO_panel$n_hogares >= 10) * 100, "%\n")

# Check: distribución de hogares por celda
quantile(ENAHO_panel$n_hogares, c(0.05, 0.25, 0.5, 0.75, 0.95))

# 5 ubigeos sin match:
# 120699 (824 hogares, 2013-2020): sufijo "99" en ENAHO = código comodín/no
# asignado del diseño muestral. No es un distrito real.
# 160109, 160203/04/07 (14-43 hogares, 2004-2012): códigos de Ucayali que
# desaparecen antes de 2013 — reorganizaciones territoriales INEI. Irrelevantes.

# ==============================================================================
# ---- 5. Construcción del Panel ----------------------------------------------
# ==============================================================================

# ---- 5.1 ENAHO sumaria → distrito × año (ponderado FACTOR07) ---------------
ENAHO_panel <- ENAHO_sumaria %>%
  mutate(
    ubigeo6 = pad6(UBIGEO),
    year    = as.integer(AÑO)
  ) %>%
  filter(ubigeo6 %in% Ubigeo_Master$ubigeo6) %>%
  group_by(ubigeo6, year) %>%
  summarise(
    n_hogares     = n(),
    ingbruhd_mean = weighted.mean(INGBRUHD,  FACTOR07, na.rm = TRUE),
    inghog2d_mean = weighted.mean(INGHOG2D,  FACTOR07, na.rm = TRUE),
    gashog2d_mean = weighted.mean(GASHOG2D,  FACTOR07, na.rm = TRUE),
    pct_pobre     = weighted.mean(POBREZA == 1, FACTOR07, na.rm = TRUE),
    pct_pobre_ext = weighted.mean(POBREZA == 2, FACTOR07, na.rm = TRUE),
    gini          = gini_weighted(INGHOG2D, FACTOR07),
    .groups = "drop"
  )

# ---- 5.2 Mining → distrito × año (sólo Concentración) ----------------------
Mining_dist_year <- Mining_Site %>%
  filter(ETAPA == "Concentración") %>%
  group_by(ubigeo6, year) %>%
  summarise(
    revenue_conc_usd = sum(revenue_usd, na.rm = TRUE),
    n_minerales      = n_distinct(mineral),
    .groups = "drop"
  )

# ---- 5.3 Transferencias → distrito × año -----------------------------------
Transf_dist_year <- Transferencias_Municipales %>%
  filter(!is.na(ubigeo6)) %>%
  mutate(year = as.integer(year)) %>%
  group_by(ubigeo6, year) %>%
  summarise(
    canon_credited_mpen = sum(credited,   na.rm = TRUE),
    canon_auth_mpen     = sum(authorised, na.rm = TRUE),
    .groups = "drop"
  )

# ---- 5.4 Tipo de cambio ----------------------------------------------------
EX_clean <- EX %>%
  rename(year = 1, pen_usd = 2) %>%
  mutate(year = as.integer(year), pen_usd = as.numeric(pen_usd))

# ---- 5.5 Skeleton + joins --------------------------------------------------
Panel <- expand_grid(
  ubigeo6 = Ubigeo_Master$ubigeo6,
  year    = full_years
) %>%
  left_join(
    Ubigeo_Master %>%
      select(ubigeo6, region_std, region_std, provincia_std, distrito_std,
             cod_region, cod_provincia),
    by = "ubigeo6"
  ) %>%
  left_join(Transf_dist_year, by = c("ubigeo6", "year")) %>%
  left_join(ENAHO_panel,      by = c("ubigeo6", "year")) %>%
  left_join(Mining_dist_year, by = c("ubigeo6", "year")) %>%
  left_join(EX_clean,         by = "year") %>%
  mutate(
    canon_credited_mpen = replace_na(canon_credited_mpen, 0),
    canon_auth_mpen     = replace_na(canon_auth_mpen,     0),
    revenue_conc_usd    = replace_na(revenue_conc_usd,    0),
    treated             = as.integer(canon_credited_mpen > 0),
    mining_district     = as.integer(revenue_conc_usd    > 0),
    enaho_reliable      = as.integer(!is.na(n_hogares) & n_hogares >= 10)
  )

# ==============================================================================
# ---- 6. Deflactores ---------------------------------------------------------
# ==============================================================================

# ---- 6.1 Peru CPI desde LINPE (base 2010 = 1) ------------------------------
# El archivo BCRP contiene VARIACIONES PORCENTUALES anuales fin de período.
# Convertimos a niveles via producto cumulado y normalizamos a 2010 = 1.

ipc_bcrp_raw <- read_excel(
  "./Data/IPC BCRP 02-24.xlsx",
  sheet = "Anuales",
  skip = 2,
  col_names = c("year", "variacion_pct")
) %>%
  filter(!is.na(year)) %>%
  mutate(year = as.integer(year),
         variacion_pct = as.numeric(variacion_pct))

peru_cpi <- ipc_bcrp_raw %>%
  arrange(year) %>%
  mutate(factor    = 1 + variacion_pct / 100,
         cpi_level = cumprod(factor)) %>%
  mutate(cpi_peru = cpi_level / cpi_level[year == 2010]) %>%
  filter(year %in% full_years) %>%
  select(year, cpi_peru)

saveRDS(peru_cpi,  "./Data/peru_cpi.rds")


# ---- 6.2 US CPI desde FRED (CPIAUCNS mensual → anual, base 2010 = 1) ---------
us_cpi <- read_csv(
  "./Data/CPI US.csv",
  show_col_types = FALSE
) %>%
  rename(date = observation_date, cpi_monthly = CPIAUCNS) %>%
  mutate(year = as.integer(format(date, "%Y"))) %>%
  group_by(year) %>%
  summarise(cpi_annual = mean(cpi_monthly, na.rm = TRUE), .groups = "drop") %>%
  filter(year %in% full_years) %>%
  mutate(cpi_us = cpi_annual / cpi_annual[year == 2010]) %>%
  select(year, cpi_us)

stopifnot(
  abs(peru_cpi$cpi_peru[peru_cpi$year == 2010] - 1) < 1e-10,
  abs(us_cpi$cpi_us[us_cpi$year == 2010] - 1) < 1e-10
)

saveRDS(us_cpi,  "./Data/us_cpi.rds")


# ---- 6.3 Aplicar al panel --------------------------------------------------
Panel <- Panel %>%
  left_join(peru_cpi %>% select(year, cpi_peru), by = "year") %>%
  left_join(us_cpi,                                    by = "year") %>%
  mutate(
    canon_credited_rusd = (canon_credited_mpen / pen_usd) / cpi_us,
    inghog2d_real       = inghog2d_mean       / cpi_peru,
    ingbruhd_real       = ingbruhd_mean       / cpi_peru,
    gashog2d_real       = gashog2d_mean       / cpi_peru,
    log_canon_r         = log(canon_credited_rusd + 1),
    log_ingreso_r       = log(inghog2d_real       + 1),
    log_gasto_r         = log(gashog2d_real       + 1),
    log_revenue_conc    = log(revenue_conc_usd    + 1)
  )

# ==============================================================================
# ---- 7. Construcción de instrumentos ----------------------------------------
# ==============================================================================

# ---- 7.1 Índice de precios (P_mt / P_m_pre) --------------------------------
price_base <- All_Prices_long %>%
  filter(year %in% pre_years) %>%
  group_by(mineral) %>%
  summarise(price_base = mean(price, na.rm = TRUE), .groups = "drop")

price_index <- All_Prices_long %>%
  mutate(year = as.integer(year)) %>%
  left_join(price_base, by = "mineral") %>%
  mutate(price_idx = price / price_base) %>%
  select(year, mineral, price_idx)

# ---- 7.2 Bartik regional (alternativo) -------------------------------------
ubigeo_geo <- Ubigeo_Master %>% distinct(ubigeo6, cod_region, cod_provincia)

shares_reg <- Mining_Site %>%
  filter(ETAPA == "Concentración", year %in% pre_years) %>%
  left_join(ubigeo_geo %>% select(ubigeo6, cod_region), by = "ubigeo6") %>%
  group_by(cod_region, mineral) %>%
  summarise(revenue_pre = sum(revenue_usd, na.rm = TRUE), .groups = "drop") %>%
  group_by(cod_region) %>%
  mutate(share_reg = revenue_pre / sum(revenue_pre)) %>%
  ungroup() %>%
  select(cod_region, mineral, share_reg)

Bartik <- ubigeo_geo %>%
  select(ubigeo6, cod_region) %>%
  left_join(shares_reg,  by = "cod_region",  relationship = "many-to-many") %>%
  left_join(price_index, by = "mineral",     relationship = "many-to-many") %>%
  group_by(ubigeo6, year) %>%
  summarise(
    bartik     = sum(share_reg * price_idx,      na.rm = TRUE),
    log_bartik = sum(share_reg * log(price_idx), na.rm = TRUE),
    .groups    = "drop"
  )

Panel <- Panel %>%
  select(-any_of(c("bartik", "log_bartik"))) %>%
  left_join(Bartik, by = c("ubigeo6", "year"))

# ---- 7.3 Canon simulado (principal) — ACUMULACIÓN POR CANALES -------------
# Por Ley 27506 un distrito puede acumular hasta tres canales:
#   Productor:  10% directo + 25% provincial + 40% regional
#   Provincial: 25% provincial + 40% regional
#   Regional:   solo 40% regional

mineros_directos <- Mining_Site %>%
  filter(ETAPA == "Concentración") %>%
  distinct(ubigeo6) %>% pull(ubigeo6)

clasificacion_canon <- Ubigeo_Master %>%
  mutate(minero_directo = as.integer(ubigeo6 %in% mineros_directos)) %>%
  left_join(
    Mining_Site %>% filter(ETAPA == "Concentración") %>%
      distinct(ubigeo6) %>%
      left_join(Ubigeo_Master %>% select(ubigeo6, cod_provincia), by = "ubigeo6") %>%
      distinct(cod_provincia) %>% mutate(provincia_minera = 1L),
    by = "cod_provincia"
  ) %>%
  left_join(
    Mining_Site %>% filter(ETAPA == "Concentración") %>%
      distinct(ubigeo6) %>%
      left_join(Ubigeo_Master %>% select(ubigeo6, cod_region), by = "ubigeo6") %>%
      distinct(cod_region) %>% mutate(region_minera = 1L),
    by = "cod_region"
  ) %>%
  mutate(
    provincia_minera = replace_na(provincia_minera, 0L),
    region_minera    = replace_na(region_minera,    0L),
    tipo_receptor    = case_when(
      minero_directo   == 1 ~ "directo",
      provincia_minera == 1 ~ "provincia",
      region_minera    == 1 ~ "region",
      TRUE                  ~ "no_receptor"
    )
  )

ingreso_regional_simulado <- Mining_Site %>%
  filter(ETAPA == "Concentración") %>%
  left_join(Ubigeo_Master %>% select(ubigeo6, cod_region), by = "ubigeo6") %>%
  group_by(cod_region, mineral) %>%
  mutate(rev_pre = mean(revenue_usd[year %in% pre_years], na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(price_index, by = c("mineral", "year")) %>%
  mutate(revenue_simulado = rev_pre * price_idx) %>%
  group_by(cod_region, year) %>%
  summarise(ingreso_minero_sim = sum(revenue_simulado, na.rm = TRUE), .groups = "drop")

n_por_tipo <- clasificacion_canon %>%
  group_by(cod_region, tipo_receptor) %>%
  summarise(n_tipo = n(), .groups = "drop")

canon_simulado <- expand_grid(
  ubigeo6 = clasificacion_canon$ubigeo6,
  year    = full_years
) %>%
  left_join(clasificacion_canon %>% select(ubigeo6, cod_region, tipo_receptor),
            by = "ubigeo6") %>%
  left_join(ingreso_regional_simulado, by = c("cod_region", "year")) %>%
  left_join(n_por_tipo, by = c("cod_region", "tipo_receptor")) %>%
  mutate(
    peso_formula  = case_when(
      tipo_receptor == "directo"   ~ 0.10,
      tipo_receptor == "provincia" ~ 0.25,
      tipo_receptor == "region"    ~ 0.40,
      TRUE                         ~ 0
    ),
    canon_sim_d   = ingreso_minero_sim * 0.50 * peso_formula / n_tipo,
    log_canon_sim = log(canon_sim_d + 1)
  ) %>%
  arrange(ubigeo6, year) %>%
  group_by(ubigeo6) %>%
  mutate(
    log_canon_sim_l1 = lag(log_canon_sim, 1),
    log_canon_sim_l2 = lag(log_canon_sim, 2)
  ) %>%
  ungroup()

# ==============================================================================
# ---- 8. Muestras de regresión -----------------------------------------------
# ==============================================================================

panel_base <- Panel %>%
  filter(enaho_reliable == 1) %>%
  arrange(ubigeo6, year) %>%
  group_by(ubigeo6) %>%
  mutate(
    log_bartik_l1 = lag(log_bartik, 1),
    log_bartik_l2 = lag(log_bartik, 2)
  ) %>%
  ungroup() %>%
  left_join(
    canon_simulado %>% select(ubigeo6, year, log_canon_sim_l1, log_canon_sim_l2),
    by = c("ubigeo6", "year")
  )

panel_sim    <- panel_base %>% filter(!is.na(log_canon_sim_l1), !is.na(log_ingreso_r))
panel_bartik <- panel_base %>% filter(!is.na(log_bartik_l1),    !is.na(log_ingreso_r))

# ==============================================================================
# ---- 9. Primeras etapas -----------------------------------------------------
# ==============================================================================

fs_sim_l1 <- feols(log_canon_r ~ log_canon_sim_l1 | ubigeo6 + year,
                   data = panel_sim, cluster = ~ ubigeo6 + cod_provincia)
fs_bar_l1 <- feols(log_canon_r ~ log_bartik_l1    | ubigeo6 + year,
                   data = panel_bartik, cluster = ~ ubigeo6 + cod_region)

etable(fs_sim_l1, fs_bar_l1,
       headers = c("Sim L1", "Bartik L1"),
       fitstat = ~ r2 + n + ivwald,
       title   = "First Stage Comparison")

# ==============================================================================
# ---- 10. Forma reducida -----------------------------------------------------
# ==============================================================================

m_rf_ingreso <- feols(log_ingreso_r ~ log_canon_sim_l1 | ubigeo6 + year,
                      data = panel_sim, cluster = ~ ubigeo6 + cod_provincia)
m_rf_gasto   <- feols(log_gasto_r   ~ log_canon_sim_l1 | ubigeo6 + year,
                      data = panel_sim, cluster = ~ ubigeo6 + cod_provincia)
m_rf_pobre   <- feols(pct_pobre     ~ log_canon_sim_l1 | ubigeo6 + year,
                      data = panel_sim, cluster = ~ ubigeo6 + cod_provincia)

etable(m_rf_ingreso, m_rf_gasto, m_rf_pobre,
       headers = c("log Ingreso", "log Gasto", "% Pobre"),
       fitstat = ~ r2 + n,
       title   = "Reduced Form: Simulated Canon → Outcomes")

# ==============================================================================
# ---- 11. RESULTADOS PRINCIPALES (IV con Canon simulado) ---------------------
# ==============================================================================

m_iv_ingreso <- feols(log_ingreso_r ~ 1 | ubigeo6 + year | log_canon_r ~ log_canon_sim_l1,
                      data = panel_sim, cluster = ~ ubigeo6 + cod_provincia)
m_iv_gasto   <- feols(log_gasto_r   ~ 1 | ubigeo6 + year | log_canon_r ~ log_canon_sim_l1,
                      data = panel_sim, cluster = ~ ubigeo6 + cod_provincia)
m_iv_pobre   <- feols(pct_pobre     ~ 1 | ubigeo6 + year | log_canon_r ~ log_canon_sim_l1,
                      data = panel_sim, cluster = ~ ubigeo6 + cod_provincia)

etable(m_iv_ingreso, m_iv_gasto, m_iv_pobre,
       se.below = TRUE,
       keep_raw = "fit_log_canon_r",
       headers  = c("log Ingreso", "log Gasto", "% Pobre"),
       fitstat  = ~ r2 + n + ivf1 + ivwald,
       title    = "Main Results — 2SLS IV Simulated Canon (Provincial Cluster)")

# ==============================================================================
# ---- 12. ROBUSTEZ -----------------------------------------------------------
# ==============================================================================

# ---- 12.A Bartik como instrumento alternativo ------------------------------
m_iv_bar_ingreso <- feols(log_ingreso_r ~ 1 | ubigeo6 + year | log_canon_r ~ log_bartik_l1,
                          data = panel_bartik, cluster = ~ ubigeo6 + cod_region)
m_iv_bar_gasto   <- feols(log_gasto_r   ~ 1 | ubigeo6 + year | log_canon_r ~ log_bartik_l1,
                          data = panel_bartik, cluster = ~ ubigeo6 + cod_region)
m_iv_bar_pobre   <- feols(pct_pobre     ~ 1 | ubigeo6 + year | log_canon_r ~ log_bartik_l1,
                          data = panel_bartik, cluster = ~ ubigeo6 + cod_region)

# ---- 12.B Excluir productores directos -------------------------------------
panel_indirectos <- panel_sim %>%
  mutate(minero_directo = as.integer(ubigeo6 %in% mineros_directos)) %>%
  filter(minero_directo == 0)

m_iv_indir_ingreso <- feols(log_ingreso_r ~ 1 | ubigeo6 + year | log_canon_r ~ log_canon_sim_l1,
                            data = panel_indirectos, cluster = ~ ubigeo6 + cod_provincia)
m_iv_indir_gasto   <- feols(log_gasto_r   ~ 1 | ubigeo6 + year | log_canon_r ~ log_canon_sim_l1,
                            data = panel_indirectos, cluster = ~ ubigeo6 + cod_provincia)
m_iv_indir_pobre   <- feols(pct_pobre     ~ 1 | ubigeo6 + year | log_canon_r ~ log_canon_sim_l1,
                            data = panel_indirectos, cluster = ~ ubigeo6 + cod_provincia)

# ---- 12.C Control por producción minera local ------------------------------
m_iv_ctrl_ingreso <- feols(log_ingreso_r ~ log_revenue_conc | ubigeo6 + year |
                             log_canon_r ~ log_canon_sim_l1,
                           data = panel_sim, cluster = ~ ubigeo6 + cod_provincia)
m_iv_ctrl_gasto   <- feols(log_gasto_r   ~ log_revenue_conc | ubigeo6 + year |
                             log_canon_r ~ log_canon_sim_l1,
                           data = panel_sim, cluster = ~ ubigeo6 + cod_provincia)

# ---- 12.D Clustering regional ----------------------------------------------
m_iv_reg_ingreso <- feols(log_ingreso_r ~ 1 | ubigeo6 + year | log_canon_r ~ log_canon_sim_l1,
                          data = panel_sim, cluster = ~ ubigeo6 + cod_region)
m_iv_reg_gasto   <- feols(log_gasto_r   ~ 1 | ubigeo6 + year | log_canon_r ~ log_canon_sim_l1,
                          data = panel_sim, cluster = ~ ubigeo6 + cod_region)

# ---- Tablas comparativas ---------------------------------------------------
etable(m_iv_ingreso, m_iv_bar_ingreso, m_iv_indir_ingreso, m_iv_ctrl_ingreso, m_iv_reg_ingreso,
       headers  = c("Main", "Bartik", "Indirect", "Mining ctrl", "Region cl."),
       fitstat  = ~ n + ivwald,
       title    = "Robustness — Income Equation")

etable(m_iv_gasto, m_iv_bar_gasto, m_iv_indir_gasto, m_iv_ctrl_gasto, m_iv_reg_gasto,
       headers  = c("Main", "Bartik", "Indirect", "Mining ctrl", "Region cl."),
       fitstat  = ~ n + ivwald,
       title    = "Robustness — Expenditure Equation")

# ==============================================================================
# ---- 13. Guardar resultados -------------------------------------------------
# ==============================================================================

saveRDS(Panel,           "./Data/Panel.rds")
saveRDS(canon_simulado,  "./Data/canon_simulado.rds")
saveRDS(panel_sim,       "./Data/panel_sim.rds")
saveRDS(
  list(
    main      = list(ingreso = m_iv_ingreso, gasto = m_iv_gasto, pobre = m_iv_pobre),
    bartik    = list(ingreso = m_iv_bar_ingreso, gasto = m_iv_bar_gasto, pobre = m_iv_bar_pobre),
    indirect  = list(ingreso = m_iv_indir_ingreso, gasto = m_iv_indir_gasto, pobre = m_iv_indir_pobre),
    mining_ct = list(ingreso = m_iv_ctrl_ingreso, gasto = m_iv_ctrl_gasto),
    region_cl = list(ingreso = m_iv_reg_ingreso,  gasto = m_iv_reg_gasto)
  ),
  "./Data/results_iv.rds"
)

cat("\n=== Pipeline completo ===\n")
cat("Panel:", nrow(Panel), "filas\n")
cat("Muestra IV:", nrow(panel_sim), "filas |",
    n_distinct(panel_sim$ubigeo6), "distritos\n")

