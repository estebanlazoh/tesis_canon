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
# ---- 3. Transferencias_Provinciales — reconstruida desde Municipales --------
# ==============================================================================
# Se agrega Transferencias_Municipales (ya con ubigeo6 → cod_provincia) para
# evitar todos los errores de strings del MEF: fill-bugs (ej. TARATA 2015 en
# HUANCA HUANCA), homónimos de provincia y cabeceras provinciales sin match.

prov_lookup <- Ubigeo_Master %>%
  distinct(cod_provincia, provincia_std, region_std)

Transferencias_Provinciales <- Transferencias_Municipales %>%
  filter(!is.na(ubigeo6)) %>%
  left_join(
    Ubigeo_Master %>% distinct(ubigeo6, cod_provincia),
    by = "ubigeo6"
  ) %>%
  group_by(cod_provincia, year) %>%
  summarise(
    authorised = sum(authorised, na.rm = TRUE),
    credited   = sum(credited,   na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  left_join(
    prov_lookup %>% rename(name = provincia_std, depto = region_std),
    by = "cod_provincia"
  ) %>%
  mutate(
    match_method = if_else(!is.na(name), "ubigeo_agg", "unmatched")
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

qa_prov <- Transferencias_Provinciales %>%
  group_by(match_method) %>%
  summarise(
    n            = n(),
    n_provincias = n_distinct(cod_provincia),
    credited     = sum(credited, na.rm = TRUE),
    .groups      = "drop"
  ) %>%
  mutate(pct_credited = credited / sum(credited))
print(qa_prov)

Transferencias_Provinciales %>%
  filter(match_method == "unmatched") %>%
  distinct(cod_provincia) %>%
  { if (nrow(.) > 0) { cat("Provincias sin match:\n"); print(.) } else cat("Sin unmatched provinciales.\n") }

dup_prov_panel <- Transferencias_Provinciales %>%
  count(cod_provincia, year) %>% filter(n > 1)
if (nrow(dup_prov_panel) > 0) {
  cat("AVISO — Duplicados de panel (cod_provincia × year):\n"); print(dup_prov_panel)
} else {
  cat("Sin duplicados provinciales.\n")
}

# ==============================================================================
# ---- Guardar ----------------------------------------------------------------
# ==============================================================================

#saveRDS(Transferencias_Municipales,  "./Data/Transferencias_Municipales.rds")
#saveRDS(Transferencias_Regionales,   "./Data/Transferencias_Regionales.rds")
#saveRDS(Transferencias_Provinciales, "./Data/Transferencias_Provinciales.rds")

#cat("\nGuardado — Municipal:", nrow(Transferencias_Municipales), "filas\n")
#cat("Guardado — Regional:",  nrow(Transferencias_Regionales),  "filas\n")
#cat("Guardado — Provincial:", nrow(Transferencias_Provinciales), "filas\n")


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
# ---- Decisión de ETAPA para instrumentos -------------------------------------
# ==============================================================================
# Se usa ÚNICAMENTE Concentración para construir los intrumentos.
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


# ==============================================================================
# ---- 5. Construcción del Panel ----------------------------------------------
# ==============================================================================

# ---- Identificar los 5 ubigeos no matcheados ---------------------------------
enaho_sin_match <- enaho_s %>%
  filter(!ubigeo6 %in% Ubigeo_Master$ubigeo6) %>%
  group_by(ubigeo6) %>%
  summarise(
    n_hogares = n(),
    years     = paste(sort(unique(year)), collapse = ", "),
    .groups   = "drop"
  )
print(enaho_sin_match)

# ---- 5.1 ENAHO sumaria → distrito × año (ponderado FACTOR07) ---------------
ENAHO_panel <- ENAHO_sumaria %>%
  mutate(
    ubigeo6 = pad6(UBIGEO),
    year    = as.integer(AÑO)
  ) %>%
  filter(ubigeo6 %in% Ubigeo_Master$ubigeo6) %>%
  group_by(ubigeo6, year) %>%
  summarise(
    n_hogares      = n(),
    ingbruhd_mean  = weighted.mean(INGBRUHD,  FACTOR07, na.rm = TRUE),
    inghog2d_mean  = weighted.mean(INGHOG2D,  FACTOR07, na.rm = TRUE),
    gashog2d_mean  = weighted.mean(GASHOG2D,  FACTOR07, na.rm = TRUE),
    pct_pobre      = weighted.mean(POBREZA == 1, FACTOR07, na.rm = TRUE),
    pct_pobre_ext  = weighted.mean(POBREZA == 2, FACTOR07, na.rm = TRUE),
    gini           = gini_weighted(INGHOG2D, FACTOR07),
    tam_hogar_mean = weighted.mean(MIEPERHO, FACTOR07, na.rm = TRUE),
    pop_proxy      = sum(FACTOR07 * MIEPERHO, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(log_pop_proxy = log(pop_proxy + 1)) %>%
  select(-pop_proxy)

# ---- 5.1b Helper: pesos de hogar para módulos de personas ------------------
# ENAHO_100/200/300/500 NO tienen FACTOR07 — se obtiene de ENAHO_sumaria.
# Llave: CONGLOME + VIVIENDA + HOGAR + AÑO (normalizada a integer).
hh_weights <- ENAHO_sumaria %>%
  mutate(
    year      = as.integer(AÑO),
    CONGLOME  = as.integer(CONGLOME),
    VIVIENDA  = as.integer(VIVIENDA),
    HOGAR_int = as.integer(HOGAR)
  ) %>%
  select(CONGLOME, VIVIENDA, HOGAR_int, year, FACTOR07)

# ---- 5.1c Educación (ENAHO_300) → pct_sin_educ -----------------------------
# P301A: nivel educativo (1 = sin nivel; 2-11 = inicial a postgrado).
ENAHO_edu <- ENAHO_300 %>%
  mutate(
    ubigeo6   = pad6(UBIGEO),
    year      = as.integer(AÑO),
    CONGLOME  = as.integer(CONGLOME),
    VIVIENDA  = as.integer(VIVIENDA),
    HOGAR_int = as.integer(HOGAR),
    sin_educ  = as.integer(!is.na(P301A) & P301A <= 1)
  ) %>%
  left_join(hh_weights, by = c("CONGLOME", "VIVIENDA", "HOGAR_int", "year")) %>%
  filter(ubigeo6 %in% Ubigeo_Master$ubigeo6, !is.na(FACTOR07)) %>%
  group_by(ubigeo6, year) %>%
  summarise(pct_sin_educ = weighted.mean(sin_educ, FACTOR07, na.rm = TRUE),
            .groups = "drop")

# ---- 5.1d NBI (ENAHO_100) → pct_nbi y desagregados -------------------------
# NBI1: vivienda inadecuada; NBI2: hacinamiento; NBI3: sin servicios higiénicos;
# NBI4: niños sin escuela; NBI5: alta dependencia económica.
# Limitación: son estimaciones muestrales (no censales), error de medición alto
# en distritos con pocos hogares. Reportar como aproximación.
ENAHO_nbi <- ENAHO_100 %>%
  mutate(
    ubigeo6   = pad6(UBIGEO),
    year      = as.integer(AÑO),
    CONGLOME  = as.integer(CONGLOME),
    VIVIENDA  = as.integer(VIVIENDA),
    HOGAR_int = as.integer(HOGAR),
    nbi_any   = as.integer(
      (!is.na(NBI1) & NBI1 == 1) | (!is.na(NBI2) & NBI2 == 1) |
        (!is.na(NBI3) & NBI3 == 1) | (!is.na(NBI4) & NBI4 == 1) |
        (!is.na(NBI5) & NBI5 == 1)
    )
  ) %>%
  left_join(hh_weights, by = c("CONGLOME", "VIVIENDA", "HOGAR_int", "year")) %>%
  filter(ubigeo6 %in% Ubigeo_Master$ubigeo6, !is.na(FACTOR07)) %>%
  group_by(ubigeo6, year) %>%
  summarise(
    pct_nbi      = weighted.mean(nbi_any,      FACTOR07, na.rm = TRUE),
    pct_nbi1_viv = weighted.mean(NBI1 == 1,    FACTOR07, na.rm = TRUE),
    pct_nbi2_hac = weighted.mean(NBI2 == 1,    FACTOR07, na.rm = TRUE),
    pct_nbi3_sss = weighted.mean(NBI3 == 1,    FACTOR07, na.rm = TRUE),
    pct_nbi4_edu = weighted.mean(NBI4 == 1,    FACTOR07, na.rm = TRUE),
    pct_nbi5_dep = weighted.mean(NBI5 == 1,    FACTOR07, na.rm = TRUE),
    .groups = "drop"
  )

# ---- 5.1e Migración (ENAHO_200) → pct_migrante -----------------------------
# P208A1: 1 = nació en el distrito, 2 = no nació (migrante).
# Solo miembros presentes del hogar (P204 == 1 & P206 == 1).
ENAHO_mig <- ENAHO_200 %>%
  mutate(
    ubigeo6   = pad6(UBIGEO),
    year      = as.integer(AÑO),
    CONGLOME  = as.integer(CONGLOME),
    VIVIENDA  = as.integer(VIVIENDA),
    HOGAR_int = as.integer(HOGAR),
    migrante  = as.integer(!is.na(P208A1) & P208A1 == 2)
  ) %>%
  left_join(hh_weights, by = c("CONGLOME", "VIVIENDA", "HOGAR_int", "year")) %>%
  filter(
    ubigeo6 %in% Ubigeo_Master$ubigeo6,
    !is.na(FACTOR07),
    !is.na(P204) & P204 == 1,
    !is.na(P206) & P206 == 1
  ) %>%
  group_by(ubigeo6, year) %>%
  summarise(pct_migrante = weighted.mean(migrante, FACTOR07, na.rm = TRUE),
            .groups = "drop")

# ---- 5.1f Empleo minero + transferencias públicas (ENAHO_500) --------------
# P506: código CIIU del sector.
#   CIIU rev 3 (hasta ~2009): minas/canteras = 10-14 (2 dígitos)
#   CIIU rev 4 (desde ~2010): minas/canteras = 05-09 (2 dígitos)
# P501/P502: indicadores de ocupado.
# P5566A: receptor de otras transferencias institucionales (Juntos, Pensión 65, etc.)
ENAHO_emp <- ENAHO_500 %>%
  mutate(
    ubigeo6   = pad6(UBIGEO),
    year      = as.integer(AÑO),
    CONGLOME  = as.integer(CONGLOME),
    VIVIENDA  = as.integer(VIVIENDA),
    HOGAR_int = as.integer(HOGAR),
    p506_code = as.integer(
      substr(str_pad(as.character(as.integer(P506)), 4, "left", "0"), 1, 2)
    ),
    empleado  = as.integer((!is.na(P501) & P501 == 1) | (!is.na(P502) & P502 == 1)),
    emp_minero = as.integer(
      empleado == 1 & !is.na(p506_code) &
        (p506_code %in% 10:14 | p506_code %in% 5:9)
    ),
    recibe_transf = as.integer(!is.na(P5566A) & P5566A == 1)
  ) %>%
  left_join(hh_weights, by = c("CONGLOME", "VIVIENDA", "HOGAR_int", "year")) %>%
  filter(ubigeo6 %in% Ubigeo_Master$ubigeo6, !is.na(FACTOR07)) %>%
  group_by(ubigeo6, year) %>%
  summarise(
    pct_emp_minero  = weighted.mean(emp_minero,    FACTOR07, na.rm = TRUE),
    pct_transf_publ = weighted.mean(recibe_transf, FACTOR07, na.rm = TRUE),
    .groups = "drop"
  )

# ---- 5.1g Consolidar todos los controles en ENAHO_panel --------------------
ENAHO_panel <- ENAHO_panel %>%
  left_join(ENAHO_edu, by = c("ubigeo6", "year")) %>%
  left_join(ENAHO_nbi, by = c("ubigeo6", "year")) %>%
  left_join(ENAHO_mig, by = c("ubigeo6", "year")) %>%
  left_join(ENAHO_emp, by = c("ubigeo6", "year"))

cat("\n=== ENAHO_panel: controles construidos ===\n",
    "  Filas totales:    ", nrow(ENAHO_panel),                       "\n",
    "  tam_hogar_mean:   ", sum(!is.na(ENAHO_panel$tam_hogar_mean)), "\n",
    "  log_pop_proxy:    ", sum(!is.na(ENAHO_panel$log_pop_proxy)),  "\n",
    "  pct_sin_educ:     ", sum(!is.na(ENAHO_panel$pct_sin_educ)),   "\n",
    "  pct_nbi:          ", sum(!is.na(ENAHO_panel$pct_nbi)),        "\n",
    "  pct_migrante:     ", sum(!is.na(ENAHO_panel$pct_migrante)),   "\n",
    "  pct_emp_minero:   ", sum(!is.na(ENAHO_panel$pct_emp_minero)), "\n",
    "  pct_transf_publ:  ", sum(!is.na(ENAHO_panel$pct_transf_publ)),"\n",
    "  gini (sumaria):   ", sum(!is.na(ENAHO_panel$gini)),           "\n")


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
      select(ubigeo6, region_std, provincia_std, distrito_std,
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
    treated             = as.integer(canon_credited_mpen > 0),  # ¿recibió algún canon este año?
    mining_district     = as.integer(revenue_conc_usd    > 0),  # ¿hubo extracción minera este año?
    enaho_reliable      = as.integer(!is.na(n_hogares) & n_hogares >= 10)
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

# ---- Diagnóstico de cobertura ENAHO y canon por región ---------------------
cobertura_region <- Panel %>%
  group_by(region_std) %>%
  summarise(
    n_distritos_total    = n_distinct(ubigeo6),
    n_con_enaho          = n_distinct(ubigeo6[enaho_reliable == 1]),
    n_con_canon          = n_distinct(ubigeo6[canon_credited_mpen > 0]),
    pct_enaho_confiable  = mean(enaho_reliable) * 100,
    .groups = "drop"
  ) %>%
  arrange(pct_enaho_confiable)

print(cobertura_region, n = 30)

# ---- Mapa: cobertura ENAHO por distrito ------------------------------------
Panel %>%
  group_by(ubigeo6) %>%
  summarise(
    n_anios_enaho = sum(enaho_reliable, na.rm = TRUE),
    n_anios_canon = sum(canon_credited_mpen > 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  right_join(
    Mapa_Distrito %>% mutate(ubigeo6 = pad6(COD_DISTRITO)),
    by = "ubigeo6"
  ) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = n_anios_enaho), color = NA) +
  scale_fill_gradient(
    low = "grey90", high = "#1A5276",
    na.value = "grey90",
    name = "Años con\nENAHO ≥10 hogares"
  ) +
  labs(title = "Cobertura ENAHO confiable por distrito (2004-2024)",
       subtitle = "Gris = nunca muestreado o n_hogares < 10") +
  theme_void()

# ---- Verificar pad6 -----------------------------------------------------------
# Confirmar que el join ENAHO-Panel funciona revisando cuántos distritos matchean
cat("Ubigeos en Ubigeo_Master:", n_distinct(Ubigeo_Master$ubigeo6), "\n")
cat("Ubigeos en ENAHO_panel:  ", n_distinct(ENAHO_panel$ubigeo6), "\n")
cat("ENAHO en Ubigeo_Master:  ",
    sum(unique(ENAHO_panel$ubigeo6) %in% Ubigeo_Master$ubigeo6), "\n")
cat("ENAHO NO en Ubigeo_Master:",
    sum(!unique(ENAHO_panel$ubigeo6) %in% Ubigeo_Master$ubigeo6), "\n")

# ---- Revisar Amazonas específicamente (cobertura < 70% en algunos años) ----
Panel %>%
  filter(region_std == "AMAZONAS") %>%
  group_by(year) %>%
  summarise(
    n_distritos_total = n_distinct(ubigeo6),
    n_con_canon       = n_distinct(ubigeo6[canon_credited_mpen > 0]),
    n_con_enaho       = sum(enaho_reliable, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print(n = 21)

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

saveRDS(peru_cpi, "./Data/peru_cpi.rds")


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

saveRDS(us_cpi, "./Data/us_cpi.rds")


# ---- 6.3 Aplicar al panel --------------------------------------------------
Panel <- Panel %>%
  left_join(peru_cpi, by = "year") %>%
  left_join(us_cpi,   by = "year") %>%
  mutate(
    canon_credited_rusd = (canon_credited_mpen / pen_usd) / cpi_us,
    inghog2d_real       = inghog2d_mean / cpi_peru,
    ingbruhd_real       = ingbruhd_mean / cpi_peru,
    gashog2d_real       = gashog2d_mean / cpi_peru,
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

# ---- 7.2 Canon simulado (principal) — Ley 27506 ----------------------------
# Productor:  10% directo + 25% provincial + 40% regional
# Provincial: 25% provincial + 40% regional
# Regional:   solo 40% regional

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

canon_simulado <- canon_simulado %>%
  arrange(ubigeo6, year) %>%
  group_by(ubigeo6) %>%
  mutate(
    log_canon_sim_f1 = lead(log_canon_sim, 1),   # placebo (futuro)
    log_canon_sim_f2 = lead(log_canon_sim, 2)
  ) %>%
  ungroup()

# Placebo: canon simulado del año SIGUIENTE
# Pregunta: ¿el canon que Áncash va a recibir el año que viene ya está prediciendo
# el ingreso de los hogares de Áncash este año?

# Si la respuesta es SÍ (β significativo) → hay un problema de identificación.
# Significa que los distritos que van a crecer en canon ya están creciendo en
# ingreso antes de recibirlo. Esto indicaría que la relación no es causal sino
# que ambas variables responden a algún factor común previo (pre-trend).

# Si la respuesta es NO (β ≈ 0, n.s.) → el instrumento no está prediciendo
# outcomes antes de actuar. La identificación es válida.


# ==============================================================================
# ---- 8. Muestras de regresión -----------------------------------------------
# ==============================================================================
panel_base <- Panel %>%
  filter(enaho_reliable == 1) %>%   # solo celulas con ≥10 hogares ENAHO
  arrange(ubigeo6, year) %>%
  left_join(
    canon_simulado %>%
      select(ubigeo6, year, log_canon_sim_l1, log_canon_sim_l2,
             log_canon_sim_f1, log_canon_sim_f2),
    by = c("ubigeo6", "year")
  )

panel_sim <- panel_base %>%
  filter(!is.na(log_canon_sim_l1), !is.na(log_ingreso_r))


# ==============================================================================
# ---- 9. Primera etapa --------------------------------------------------------
# ==============================================================================
fs_sim_l1 <- feols(log_canon_r ~ log_canon_sim_l1 | ubigeo6 + year,
                   data = panel_sim, cluster = ~ ubigeo6 + cod_provincia)

etable(fs_sim_l1,
       headers = c("Sim L1"),
       fitstat = ~ r2 + n + ivwald,
       title   = "First Stage — Simulated Canon (L1)")

# ==============================================================================
# ---- 10. Forma reducida -----------------------------------------------------
# ==============================================================================

# ---- 10.A Sin controles ----------------------------------------------------
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

# ---- 10.B Con vector de controles X_dt -------------------------------------
# X_dt = { tam_hogar_mean, log_pop_proxy, pct_migrante, pct_emp_minero,
#          pct_transf_publ, pct_sin_educ }
# Los FE absorben heterogeneidad fija; los controles absorben cambios intra-
# distrito en composición demográfica, empleo y acceso a programas sociales.
panel_rf_Xdt <- panel_sim %>%
  filter(if_all(c(tam_hogar_mean, log_pop_proxy, pct_migrante,
                  pct_emp_minero, pct_transf_publ, pct_sin_educ),
                ~ !is.na(.)))

m_rf_Xdt_ingreso <- feols(
  log_ingreso_r ~ log_canon_sim_l1 + tam_hogar_mean + log_pop_proxy +
    pct_migrante + pct_emp_minero + pct_transf_publ + pct_sin_educ |
    ubigeo6 + year,
  data = panel_rf_Xdt, cluster = ~ ubigeo6 + cod_provincia)

m_rf_Xdt_gasto <- feols(
  log_gasto_r ~ log_canon_sim_l1 + tam_hogar_mean + log_pop_proxy +
    pct_migrante + pct_emp_minero + pct_transf_publ + pct_sin_educ |
    ubigeo6 + year,
  data = panel_rf_Xdt, cluster = ~ ubigeo6 + cod_provincia)

m_rf_Xdt_pobre <- feols(
  pct_pobre ~ log_canon_sim_l1 + tam_hogar_mean + log_pop_proxy +
    pct_migrante + pct_emp_minero + pct_transf_publ + pct_sin_educ |
    ubigeo6 + year,
  data = panel_rf_Xdt, cluster = ~ ubigeo6 + cod_provincia)

# ---- 10.C Con X_dt + NBI ---------------------------------------------------
panel_rf_Xdt_nbi <- panel_sim %>%
  filter(if_all(c(tam_hogar_mean, log_pop_proxy, pct_migrante,
                  pct_emp_minero, pct_transf_publ, pct_sin_educ, pct_nbi),
                ~ !is.na(.)))

m_rf_nbi_ingreso <- feols(
  log_ingreso_r ~ log_canon_sim_l1 + tam_hogar_mean + log_pop_proxy +
    pct_migrante + pct_emp_minero + pct_transf_publ + pct_sin_educ + pct_nbi |
    ubigeo6 + year,
  data = panel_rf_Xdt_nbi, cluster = ~ ubigeo6 + cod_provincia)

m_rf_nbi_gasto <- feols(
  log_gasto_r ~ log_canon_sim_l1 + tam_hogar_mean + log_pop_proxy +
    pct_migrante + pct_emp_minero + pct_transf_publ + pct_sin_educ + pct_nbi |
    ubigeo6 + year,
  data = panel_rf_Xdt_nbi, cluster = ~ ubigeo6 + cod_provincia)

m_rf_nbi_pobre <- feols(
  pct_pobre ~ log_canon_sim_l1 + tam_hogar_mean + log_pop_proxy +
    pct_migrante + pct_emp_minero + pct_transf_publ + pct_sin_educ + pct_nbi |
    ubigeo6 + year,
  data = panel_rf_Xdt_nbi, cluster = ~ ubigeo6 + cod_provincia)

# ---- 10.D Tabla comparativa de forma reducida ------------------------------
etable(m_rf_ingreso, m_rf_Xdt_ingreso, m_rf_nbi_ingreso,
       m_rf_gasto,   m_rf_Xdt_gasto,   m_rf_nbi_gasto,
       m_rf_pobre,   m_rf_Xdt_pobre,   m_rf_nbi_pobre,
       se.below = TRUE,
       keep     = "log_canon_sim_l1",
       headers  = c("Ing sin", "Ing X_dt", "Ing +NBI",
                    "Gas sin", "Gas X_dt", "Gas +NBI",
                    "Pob sin", "Pob X_dt", "Pob +NBI"),
       fitstat  = ~ r2 + n,
       title    = "Forma reducida — Estabilidad ante controles X_dt y NBI")

# ==============================================================================
# ---- 11. RESULTADOS PRINCIPALES (IV con Canon simulado L1) ------------------
# ==============================================================================

# ---- 11.A Sin controles ----------------------------------------------------
m_iv_ingreso <- feols(log_ingreso_r ~ 1 | ubigeo6 + year |
                        log_canon_r ~ log_canon_sim_l1,
                      data = panel_sim, cluster = ~ ubigeo6 + cod_provincia)
m_iv_gasto   <- feols(log_gasto_r ~ 1 | ubigeo6 + year |
                        log_canon_r ~ log_canon_sim_l1,
                      data = panel_sim, cluster = ~ ubigeo6 + cod_provincia)
m_iv_pobre   <- feols(pct_pobre ~ 1 | ubigeo6 + year |
                        log_canon_r ~ log_canon_sim_l1,
                      data = panel_sim, cluster = ~ ubigeo6 + cod_provincia)

etable(m_iv_ingreso, m_iv_gasto, m_iv_pobre,
       se.below = TRUE,
       headers  = c("log Ingreso", "log Gasto", "% Pobre"),
       fitstat  = ~ r2 + n + ivf1 + ivwald,
       title    = "Main Results — 2SLS IV Canon Simulado L1 (sin controles)")

# ---- 11.B Con composición del hogar y población ----------------------------
panel_iv_h <- panel_sim %>%
  filter(!is.na(tam_hogar_mean), !is.na(log_pop_proxy))

m_iv_h_ingreso <- feols(log_ingreso_r ~ tam_hogar_mean + log_pop_proxy |
                          ubigeo6 + year |
                          log_canon_r ~ log_canon_sim_l1,
                        data = panel_iv_h, cluster = ~ ubigeo6 + cod_provincia)
m_iv_h_gasto   <- feols(log_gasto_r ~ tam_hogar_mean + log_pop_proxy |
                          ubigeo6 + year |
                          log_canon_r ~ log_canon_sim_l1,
                        data = panel_iv_h, cluster = ~ ubigeo6 + cod_provincia)
m_iv_h_pobre   <- feols(pct_pobre ~ tam_hogar_mean + log_pop_proxy |
                          ubigeo6 + year |
                          log_canon_r ~ log_canon_sim_l1,
                        data = panel_iv_h, cluster = ~ ubigeo6 + cod_provincia)

# ---- 11.C + Migración y empleo minero --------------------------------------
panel_iv_mig <- panel_sim %>%
  filter(!is.na(tam_hogar_mean), !is.na(log_pop_proxy),
         !is.na(pct_migrante),   !is.na(pct_emp_minero))

m_iv_mig_ingreso <- feols(
  log_ingreso_r ~ tam_hogar_mean + log_pop_proxy + pct_migrante + pct_emp_minero |
    ubigeo6 + year | log_canon_r ~ log_canon_sim_l1,
  data = panel_iv_mig, cluster = ~ ubigeo6 + cod_provincia)
m_iv_mig_gasto <- feols(
  log_gasto_r ~ tam_hogar_mean + log_pop_proxy + pct_migrante + pct_emp_minero |
    ubigeo6 + year | log_canon_r ~ log_canon_sim_l1,
  data = panel_iv_mig, cluster = ~ ubigeo6 + cod_provincia)
m_iv_mig_pobre <- feols(
  pct_pobre ~ tam_hogar_mean + log_pop_proxy + pct_migrante + pct_emp_minero |
    ubigeo6 + year | log_canon_r ~ log_canon_sim_l1,
  data = panel_iv_mig, cluster = ~ ubigeo6 + cod_provincia)

# ---- 11.D Vector X_dt completo ---------------------------------------------
panel_iv_Xdt <- panel_sim %>%
  filter(if_all(c(tam_hogar_mean, log_pop_proxy, pct_migrante,
                  pct_emp_minero, pct_transf_publ, pct_sin_educ),
                ~ !is.na(.)))

m_iv_Xdt_ingreso <- feols(
  log_ingreso_r ~ tam_hogar_mean + log_pop_proxy + pct_migrante +
    pct_emp_minero + pct_transf_publ + pct_sin_educ | ubigeo6 + year |
    log_canon_r ~ log_canon_sim_l1,
  data = panel_iv_Xdt, cluster = ~ ubigeo6 + cod_provincia)

m_iv_Xdt_gasto <- feols(
  log_gasto_r ~ tam_hogar_mean + log_pop_proxy + pct_migrante +
    pct_emp_minero + pct_transf_publ + pct_sin_educ | ubigeo6 + year |
    log_canon_r ~ log_canon_sim_l1,
  data = panel_iv_Xdt, cluster = ~ ubigeo6 + cod_provincia)

m_iv_Xdt_pobre <- feols(
  pct_pobre ~ tam_hogar_mean + log_pop_proxy + pct_migrante +
    pct_emp_minero + pct_transf_publ + pct_sin_educ | ubigeo6 + year |
    log_canon_r ~ log_canon_sim_l1,
  data = panel_iv_Xdt, cluster = ~ ubigeo6 + cod_provincia)

# ---- 11.E X_dt completo + NBI ----------------------------------------------
panel_iv_nbi <- panel_sim %>%
  filter(if_all(c(tam_hogar_mean, log_pop_proxy, pct_migrante,
                  pct_emp_minero, pct_transf_publ, pct_sin_educ, pct_nbi),
                ~ !is.na(.)))

m_iv_nbi_ingreso <- feols(
  log_ingreso_r ~ tam_hogar_mean + log_pop_proxy + pct_migrante +
    pct_emp_minero + pct_transf_publ + pct_sin_educ + pct_nbi |
    ubigeo6 + year | log_canon_r ~ log_canon_sim_l1,
  data = panel_iv_nbi, cluster = ~ ubigeo6 + cod_provincia)

m_iv_nbi_gasto <- feols(
  log_gasto_r ~ tam_hogar_mean + log_pop_proxy + pct_migrante +
    pct_emp_minero + pct_transf_publ + pct_sin_educ + pct_nbi |
    ubigeo6 + year | log_canon_r ~ log_canon_sim_l1,
  data = panel_iv_nbi, cluster = ~ ubigeo6 + cod_provincia)

m_iv_nbi_pobre <- feols(
  pct_pobre ~ tam_hogar_mean + log_pop_proxy + pct_migrante +
    pct_emp_minero + pct_transf_publ + pct_sin_educ + pct_nbi |
    ubigeo6 + year | log_canon_r ~ log_canon_sim_l1,
  data = panel_iv_nbi, cluster = ~ ubigeo6 + cod_provincia)

# ---- 11.F Tabla de estabilidad — coeficiente del canon a través de specs ---
cat("\nTamaños de muestra por especificación:\n",
    "  Sin ctrl (panel_sim):    ", nrow(panel_sim),    "\n",
    "  + Hogar/Pob:             ", nrow(panel_iv_h),   "\n",
    "  + Migrac/Min:            ", nrow(panel_iv_mig), "\n",
    "  X_dt completo:           ", nrow(panel_iv_Xdt), "\n",
    "  X_dt + NBI:              ", nrow(panel_iv_nbi), "\n")

etable(m_iv_ingreso, m_iv_h_ingreso, m_iv_mig_ingreso,
       m_iv_Xdt_ingreso, m_iv_nbi_ingreso,
       se.below = TRUE,
       keep     = "fit_log_canon_r",
       headers  = c("Sin ctrl", "+ Hogar/Pob", "+ Migrac/Min",
                    "X_dt", "X_dt + NBI"),
       fitstat  = ~ r2 + n + ivf1 + ivwald,
       title    = "Estabilidad del β_IV — log Ingreso")

etable(m_iv_gasto, m_iv_h_gasto, m_iv_mig_gasto,
       m_iv_Xdt_gasto, m_iv_nbi_gasto,
       se.below = TRUE,
       keep     = "fit_log_canon_r",
       headers  = c("Sin ctrl", "+ Hogar/Pob", "+ Migrac/Min",
                    "X_dt", "X_dt + NBI"),
       fitstat  = ~ r2 + n + ivf1 + ivwald,
       title    = "Estabilidad del β_IV — log Gasto")

etable(m_iv_pobre, m_iv_h_pobre, m_iv_mig_pobre,
       m_iv_Xdt_pobre, m_iv_nbi_pobre,
       se.below = TRUE,
       keep     = "fit_log_canon_r",
       headers  = c("Sin ctrl", "+ Hogar/Pob", "+ Migrac/Min",
                    "X_dt", "X_dt + NBI"),
       fitstat  = ~ r2 + n + ivf1 + ivwald,
       title    = "Estabilidad del β_IV — % Pobre")

# ---- 11.G Resultados completos con X_dt + NBI (especificación más rica) ----
etable(m_iv_nbi_ingreso, m_iv_nbi_gasto, m_iv_nbi_pobre,
       se.below = TRUE,
       headers  = c("log Ingreso", "log Gasto", "% Pobre"),
       fitstat  = ~ r2 + n + ivf1 + ivwald,
       title    = "Resultados principales — IV con X_dt + NBI")

# ==============================================================================
# ---- 12. ROBUSTEZ -----------------------------------------------------------
# ==============================================================================

# ---- 12.A Excluir productores directos -------------------------------------
# Identifica si el efecto sobreviene por la actividad minera local o por la
# transferencia. Si el coef. es similar (o mayor) sin productores directos, la
# transferencia fiscal — no la producción — es el mecanismo activo.

# RESULTADO: β = 0.269* (SE 0.108), F-Wald = 35.1 — el efecto CRECE al excluir
# los distritos con minería propia. Confirma que el mecanismo es la transferencia
# fiscal (canon), no la actividad económica minera directa. Es el resultado más
# importante para la narrativa causal del paper.

panel_indirectos <- panel_sim %>%
  mutate(minero_directo = as.integer(ubigeo6 %in% mineros_directos)) %>%
  filter(minero_directo == 0)

m_iv_indir_ingreso <- feols(log_ingreso_r ~ 1 | ubigeo6 + year |
                              log_canon_r ~ log_canon_sim_l1,
                            data = panel_indirectos,
                            cluster = ~ ubigeo6 + cod_provincia)
m_iv_indir_gasto   <- feols(log_gasto_r ~ 1 | ubigeo6 + year |
                              log_canon_r ~ log_canon_sim_l1,
                            data = panel_indirectos,
                            cluster = ~ ubigeo6 + cod_provincia)
m_iv_indir_pobre   <- feols(pct_pobre ~ 1 | ubigeo6 + year |
                              log_canon_r ~ log_canon_sim_l1,
                            data = panel_indirectos,
                            cluster = ~ ubigeo6 + cod_provincia)

# ---- 12.B Control por producción minera local ------------------------------
# RESULTADO: β = 0.211* (SE 0.091), coef. revenue_conc = 8.85e-5 (n.s.).
# El efecto del canon sobre ingreso no cambia al controlar por el nivel de
# extracción local. Descarta que el resultado sea un shock de demanda local
# (ej. contratistas, proveedores mineros) en lugar del canal fiscal.

m_iv_ctrl_ingreso <- feols(log_ingreso_r ~ log_revenue_conc | ubigeo6 + year |
                             log_canon_r ~ log_canon_sim_l1,
                           data = panel_sim, cluster = ~ ubigeo6 + cod_provincia)
m_iv_ctrl_gasto   <- feols(log_gasto_r ~ log_revenue_conc | ubigeo6 + year |
                             log_canon_r ~ log_canon_sim_l1,
                           data = panel_sim, cluster = ~ ubigeo6 + cod_provincia)
m_iv_ctrl_pobre   <- feols(pct_pobre ~ log_revenue_conc | ubigeo6 + year |
                             log_canon_r ~ log_canon_sim_l1,
                           data = panel_sim, cluster = ~ ubigeo6 + cod_provincia)

# ---- 12.C Clustering regional (en lugar de provincial) ---------------------
# RESULTADO: β = 0.211 (SE 0.151), F-Wald = 9.25 — pierde significancia.
# El SE casi se duplica con clustering regional vs provincial. Esto es esperable:
# hay menos clusters (26 regiones vs ~196 provincias) y más correlación intra-
# cluster. El clustering provincial es la unidad correcta porque la fórmula
# del canon opera a nivel provincial (Ley 27506, Art. 5). Se reporta como
# honestidad sobre la sensibilidad al nivel de agrupación.

m_iv_reg_ingreso <- feols(log_ingreso_r ~ 1 | ubigeo6 + year |
                            log_canon_r ~ log_canon_sim_l1,
                          data = panel_sim, cluster = ~ ubigeo6 + cod_region)
m_iv_reg_gasto   <- feols(log_gasto_r ~ 1 | ubigeo6 + year |
                            log_canon_r ~ log_canon_sim_l1,
                          data = panel_sim, cluster = ~ ubigeo6 + cod_region)
m_iv_reg_pobre   <- feols(pct_pobre ~ 1 | ubigeo6 + year |
                            log_canon_r ~ log_canon_sim_l1,
                          data = panel_sim, cluster = ~ ubigeo6 + cod_region)

# ---- 12.D Lag alternativo (L2) ---------------------------------------------
# Si el efecto del canon es persistente, debería aparecer también con dos años
# de retraso. Sirve para descartar que el resultado sea un artefacto de la
# elección específica del rezago.

# RESULTADO: β = 0.221* (SE 0.106), F-Wald = 18.0 — el efecto es robusto con
# dos años de rezago. La magnitud es similar al L1 (0.211), lo que sugiere
# persistencia del efecto del canon sobre el ingreso más allá del año de
# recepción. Descarta que el resultado sea artefacto de la elección específica
# de un rezago.

panel_sim_l2 <- panel_base %>%
  filter(!is.na(log_canon_sim_l2), !is.na(log_ingreso_r))

m_iv_l2_ingreso <- feols(log_ingreso_r ~ 1 | ubigeo6 + year |
                           log_canon_r ~ log_canon_sim_l2,
                         data = panel_sim_l2, cluster = ~ ubigeo6 + cod_provincia)
m_iv_l2_gasto   <- feols(log_gasto_r ~ 1 | ubigeo6 + year |
                           log_canon_r ~ log_canon_sim_l2,
                         data = panel_sim_l2, cluster = ~ ubigeo6 + cod_provincia)
m_iv_l2_pobre   <- feols(pct_pobre ~ 1 | ubigeo6 + year |
                           log_canon_r ~ log_canon_sim_l2,
                         data = panel_sim_l2, cluster = ~ ubigeo6 + cod_provincia)

# ---- 12.E Excluir las top-3 regiones mineras -------------------------------
# Áncash + Cajamarca + Pasco (o las que el revenue identifique) concentran la
# mayor parte del canon. Esta robustez muestra que el resultado no depende de
# unos pocos receptores extremos.

# RESULTADO: β_ingreso = 0.285** (SE 0.090), F-Wald = 44.1 — el efecto es MÁS
# fuerte y preciso sin las tres regiones dominantes. Nuevo hallazgo: el efecto
# sobre gasto se vuelve significativo (0.155*, SE 0.073). Esto sugiere que las
# regiones top-mineras tienen efectos compensatorios (enfermedad holandesa,
# desigualdad local, captura política del gasto) que atenúan la trasmisión
# canon→consumo. Para distritos en regiones menos dependientes del recurso,
# la transferencia se traduce más efectivamente en bienestar.

top_mining_regions <- Mining_Site %>%
  filter(ETAPA == "Concentración") %>%
  left_join(Ubigeo_Master %>% select(ubigeo6, cod_region), by = "ubigeo6") %>%
  group_by(cod_region) %>%
  summarise(rev_total = sum(revenue_usd, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(rev_total)) %>%
  slice_head(n = 3) %>%
  pull(cod_region)

cat("Top-3 regiones mineras excluidas (cod_region):", top_mining_regions, "\n")

panel_sin_top <- panel_sim %>% filter(!cod_region %in% top_mining_regions)

m_iv_sintop_ingreso <- feols(log_ingreso_r ~ 1 | ubigeo6 + year |
                               log_canon_r ~ log_canon_sim_l1,
                             data = panel_sin_top,
                             cluster = ~ ubigeo6 + cod_provincia)
m_iv_sintop_gasto   <- feols(log_gasto_r ~ 1 | ubigeo6 + year |
                               log_canon_r ~ log_canon_sim_l1,
                             data = panel_sin_top,
                             cluster = ~ ubigeo6 + cod_provincia)
m_iv_sintop_pobre   <- feols(pct_pobre ~ 1 | ubigeo6 + year |
                               log_canon_r ~ log_canon_sim_l1,
                             data = panel_sin_top,
                             cluster = ~ ubigeo6 + cod_provincia)

# ---- 12.F Outcomes alternativos: ingreso bruto y desigualdad (gini) -------
# Ingreso bruto (INGBRUHD): β = 0.002 (SE 0.241) n.s. — no interpretable,
# la medición de ingresos brutos en ENAHO tiene mayor ruido que el ingreso
# neto del hogar. Gini: β = 0.021 (SE 0.020) n.s. — el canon no afecta
# la desigualdad intra-distrito. Consistent con un efecto ingreso que se
# distribuye proporcionalmente en el hogar promedio.

panel_alt <- panel_sim %>%
  mutate(log_ingbruhd_r = log(ingbruhd_real + 1))

m_alt_ingbru <- feols(log_ingbruhd_r ~ 1 | ubigeo6 + year |
                        log_canon_r ~ log_canon_sim_l1,
                      data = panel_alt, cluster = ~ ubigeo6 + cod_provincia)
m_alt_gini   <- feols(gini ~ 1 | ubigeo6 + year |
                        log_canon_r ~ log_canon_sim_l1,
                      data = panel_alt %>% filter(!is.na(gini)),
                      cluster = ~ ubigeo6 + cod_provincia)

# ---- 12.G Placebo: leads del instrumento (test de pre-tendencias) ----------
# Si el canon FUTURO predice ingreso PRESENTE, hay un problema de identificación
# (anticipación o tendencias previas). Coeficiente esperado: ≈ 0.

# RESULTADO: coefs ≈ 0 y n.s. para los tres outcomes (ingreso: 0.009,
# gasto: -0.008, pobreza: 0.003). El canon simulado del AÑO SIGUIENTE no
# predice el ingreso presente. Ausencia de pre-tendencias: la identificación
# es válida. Este es el test más importante de exogeneidad del instrumento.

m_placebo_ingreso <- feols(log_ingreso_r ~ log_canon_sim_f1 | ubigeo6 + year,
                           data = panel_sim %>% filter(!is.na(log_canon_sim_f1)),
                           cluster = ~ ubigeo6 + cod_provincia)
m_placebo_gasto   <- feols(log_gasto_r ~ log_canon_sim_f1 | ubigeo6 + year,
                           data = panel_sim %>% filter(!is.na(log_canon_sim_f1)),
                           cluster = ~ ubigeo6 + cod_provincia)
m_placebo_pobre   <- feols(pct_pobre ~ log_canon_sim_f1 | ubigeo6 + year,
                           data = panel_sim %>% filter(!is.na(log_canon_sim_f1)),
                           cluster = ~ ubigeo6 + cod_provincia)

# ---- 12.H Pre-Period classification: Exogeneity of the treatment post 2006 --
# Clasificación basada SOLO en pre-período (2004-2006)
mineros_directos_pre <- Mining_Site %>%
  filter(ETAPA == "Concentración", year %in% pre_years) %>%
  distinct(ubigeo6) %>% pull(ubigeo6)

clasificacion_canon_pre <- Ubigeo_Master %>%
  mutate(minero_directo = as.integer(ubigeo6 %in% mineros_directos_pre)) %>%
  left_join(
    Mining_Site %>% filter(ETAPA == "Concentración", year %in% pre_years) %>%
      distinct(ubigeo6) %>%
      left_join(Ubigeo_Master %>% select(ubigeo6, cod_provincia), by = "ubigeo6") %>%
      distinct(cod_provincia) %>% mutate(provincia_minera = 1L),
    by = "cod_provincia"
  ) %>%
  left_join(
    Mining_Site %>% filter(ETAPA == "Concentración", year %in% pre_years) %>%
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

# Comparar cuántos distritos cambian de categoría
left_join(
  clasificacion_canon %>% select(ubigeo6, tipo_full = tipo_receptor),
  clasificacion_canon_pre %>% select(ubigeo6, tipo_pre = tipo_receptor),
  by = "ubigeo6"
) %>%
  count(tipo_full, tipo_pre) %>%
  filter(tipo_full != tipo_pre)   # distritos que cambian de categoría

# ---- Tablas comparativas ---------------------------------------------------
etable(m_iv_ingreso, m_iv_indir_ingreso, m_iv_ctrl_ingreso,
       m_iv_reg_ingreso, m_iv_l2_ingreso, m_iv_sintop_ingreso,
       headers  = c("Main", "Indirect", "Mining ctrl",
                    "Region cl.", "Lag L2", "Sin top 3 reg."),
       fitstat  = ~ n + ivwald,
       title    = "Robustness — Income Equation")

etable(m_iv_gasto, m_iv_indir_gasto, m_iv_ctrl_gasto,
       m_iv_reg_gasto, m_iv_l2_gasto, m_iv_sintop_gasto,
       headers  = c("Main", "Indirect", "Mining ctrl",
                    "Region cl.", "Lag L2", "Sin top 3 reg."),
       fitstat  = ~ n + ivwald,
       title    = "Robustness — Expenditure Equation")

etable(m_iv_pobre, m_iv_indir_pobre, m_iv_ctrl_pobre,
       m_iv_reg_pobre, m_iv_l2_pobre, m_iv_sintop_pobre,
       headers  = c("Main", "Indirect", "Mining ctrl",
                    "Region cl.", "Lag L2", "Sin top 3 reg."),
       fitstat  = ~ n + ivwald,
       title    = "Robustness — Poverty Equation")

etable(m_alt_ingbru, m_alt_gini,
       se.below = TRUE,
       headers  = c("log Ingreso bruto", "Gini"),
       fitstat  = ~ n + ivwald,
       title    = "Outcomes alternativos — IV Canon Simulado")

etable(m_placebo_ingreso, m_placebo_gasto, m_placebo_pobre,
       se.below = TRUE,
       headers  = c("log Ingreso (t)", "log Gasto (t)", "% Pobre (t)"),
       fitstat  = ~ r2 + n,
       title    = "Placebo — Lead t+1 del Canon Simulado (esperado: ≈ 0)")

# ==============================================================================
# ---- 13. Guardar resultados -------------------------------------------------
# ==============================================================================
saveRDS(Panel,           "./Data/Panel.rds")
saveRDS(canon_simulado,  "./Data/canon_simulado.rds")
saveRDS(panel_sim,       "./Data/panel_sim.rds")
saveRDS(
  list(
    main      = list(ingreso = m_iv_ingreso,      gasto = m_iv_gasto,      pobre = m_iv_pobre),
    indirect  = list(ingreso = m_iv_indir_ingreso, gasto = m_iv_indir_gasto, pobre = m_iv_indir_pobre),
    mining_ct = list(ingreso = m_iv_ctrl_ingreso, gasto = m_iv_ctrl_gasto,  pobre = m_iv_ctrl_pobre),
    region_cl = list(ingreso = m_iv_reg_ingreso,  gasto = m_iv_reg_gasto,   pobre = m_iv_reg_pobre),
    lag_l2    = list(ingreso = m_iv_l2_ingreso,   gasto = m_iv_l2_gasto,    pobre = m_iv_l2_pobre),
    sin_top   = list(ingreso = m_iv_sintop_ingreso, gasto = m_iv_sintop_gasto, pobre = m_iv_sintop_pobre),
    alt_out   = list(ingbru = m_alt_ingbru,       gini  = m_alt_gini),
    placebo   = list(ingreso = m_placebo_ingreso, gasto = m_placebo_gasto, pobre = m_placebo_pobre)
  ),
  "./Data/results_iv.rds"
)

cat("\n=== Pipeline completo ===\n")
cat("Panel:", nrow(Panel), "filas\n")
cat("Muestra IV (L1):", nrow(panel_sim), "filas |",
    n_distinct(panel_sim$ubigeo6), "distritos\n")
cat("Muestra IV (L2):", nrow(panel_sim_l2), "filas\n")
cat("Muestra sin top-3 mineras:", nrow(panel_sin_top), "filas\n")


# ==============================================================================
# ---- 14. QAs adicionales -----------------------------------------------------
# ==============================================================================

# ---- QA macro: trayectoria nacional del canon -------------------------------
canon_anual <- Transferencias_Municipales %>%
  filter(!is.na(ubigeo6)) %>%
  group_by(year) %>%
  summarise(
    credited_total_mpen = sum(credited, na.rm = TRUE),
    n_distritos_recep   = n_distinct(ubigeo6[credited > 0]),
    .groups = "drop"
  )
print(canon_anual, n = 21)

# ---- QA macro: dependencia minera por región -------------------------------
revenue_region <- Mining_Site %>%
  filter(ETAPA == "Concentración") %>%
  left_join(Ubigeo_Master %>% select(ubigeo6, region_std), by = "ubigeo6") %>%
  group_by(region_std) %>%
  summarise(revenue_total = sum(revenue_usd, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(revenue_total))
print(revenue_region, n = 26)

# ---- QA macro: tipología de receptor según Ley 27506 -----------------------
clasificacion_canon %>%
  count(tipo_receptor) %>%
  mutate(pct = n / sum(n) * 100) %>%
  print()

# ---- QA: variación intra-distrito (within-variation) -----------------------
within_var <- Panel %>%
  filter(canon_credited_mpen > 0) %>%
  group_by(ubigeo6) %>%
  summarise(
    cv_canon = sd(log(canon_credited_mpen + 1)) / mean(log(canon_credited_mpen + 1)),
    .groups  = "drop"
  )
cat("CV intra-distrito del log-canon (mediana):",
    median(within_var$cv_canon, na.rm = TRUE), "\n")
