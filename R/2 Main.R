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


#################################
###       Load Import         ###
#################################

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
    depto_std     = normalizar_texto(DEPARTAMENTO),
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
    provincia_std = normalizar_texto(province),
    distrito_std  = normalizar_texto(name)
  )

# Paso 1 — match fuerte (provincia, distrito)
m1 <- transf_keys %>%
  left_join(
    Ubigeo_Master %>% select(ubigeo6, provincia_std, distrito_std),
    by = c("provincia_std", "distrito_std")
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
                             provincia_real = provincia_std,
                             depto_real     = depto_std),
    by = "ubigeo6"
  )

Transferencias_Municipales <- m3 %>%
  transmute(
    ubigeo6,
    year,
    credited,
    authorised,
    code,
    provincia_real,
    depto_real,
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
  distinct(cod_provincia, provincia_std, depto_std)

# Detectar homónimos antes del join (si los hay, el join creará filas dobles)
prov_dupes <- prov_lookup %>% count(provincia_std) %>% filter(n > 1)

Transferencias_Provinciales <- Transferencias_Provinciales %>%
  select(-any_of(c("cod_provincia", "depto_std", "match_method"))) %>%
  mutate(prov_std_clean = normalizar_texto(name)) %>%
  left_join(
    prov_lookup %>% select(cod_provincia, provincia_std, depto_std),
    by = c("prov_std_clean" = "provincia_std")
  ) %>%
  mutate(
    match_method = if_else(!is.na(cod_provincia), "name_match", "unmatched")
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
  group_by(depto_real, year) %>%
  summarise(n_districts = n_distinct(ubigeo6), .groups = "drop") %>%
  group_by(depto_real) %>%
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

saveRDS(Transferencias_Municipales,  "./Data/Transferencias_Municipales.rds")
saveRDS(Transferencias_Regionales,   "./Data/Transferencias_Regionales.rds")
saveRDS(Transferencias_Provinciales, "./Data/Transferencias_Provinciales.rds")

cat("\nGuardado — Municipal:", nrow(Transferencias_Municipales), "filas\n")
cat("Guardado — Regional:",  nrow(Transferencias_Regionales),  "filas\n")
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
# ---- Panel: construcción ----------------------------------------------------
# ==============================================================================

# ---- Mining → distrito × año ------------------------------------------------
# Se usa solo Concentración para evitar doble conteo en operaciones 
# verticalmente integradas. 58 combinaciones distrito×año×mineral aparecen en 
# más de una etapa; Concentración representa el 82.4% del revenue y 239/241 
# distritos mineros.
Mining_dist_year <- Mining_Site %>%
  filter(ETAPA == "Concentración") %>%
  group_by(ubigeo6, year) %>%
  summarise(
    revenue_conc_usd = sum(revenue_usd, na.rm = TRUE),
    n_minerales      = n_distinct(mineral),
    .groups          = "drop"
  )

# ---- Transferencias → distrito × año ----------------------------------------
stopifnot("ubigeo6" %in% names(Transferencias_Municipales))

Transf_dist_year <- Transferencias_Municipales %>%
  filter(!is.na(ubigeo6)) %>%
  mutate(year = as.integer(year)) %>%
  group_by(ubigeo6, year) %>%
  summarise(
    canon_credited_mpen = sum(credited,   na.rm = TRUE),
    canon_auth_mpen     = sum(authorised, na.rm = TRUE),
    .groups             = "drop"
  )

# ---- Exchange rate ----------------------------------------------------------
EX_clean <- EX %>%
  rename(year = 1, pen_usd = 2) %>%
  mutate(year = as.integer(year), pen_usd = as.numeric(pen_usd))

# ---- Skeleton + joins -------------------------------------------------------
Panel <- expand_grid(
  ubigeo6 = Ubigeo_Master$ubigeo6,
  year    = 2004L:2024L
) %>%
  left_join(
    Ubigeo_Master %>%
      select(ubigeo6, region_std, depto_std, provincia_std, distrito_std,
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
    canon_credited_musd = canon_credited_mpen / pen_usd,
    log_canon           = log(canon_credited_mpen + 1),
    asinh_canon         = asinh(canon_credited_mpen),
    log_revenue         = log(revenue_conc_usd + 1),
    enaho_reliable      = as.integer(!is.na(n_hogares) & n_hogares >= 10),
    log_ingreso         = log(inghog2d_mean + 1),
    log_gasto           = log(gashog2d_mean + 1)
  )


# ---- QA ---------------------------------------------------------------------
cat("=== QA Panel ===\n")
cat("Filas:", nrow(Panel),
    "| Distritos:", n_distinct(Panel$ubigeo6),
    "| Años:",     n_distinct(Panel$year), "\n")

Panel %>%
  count(treated) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  print()

cat("ENAHO celdas con datos:            ", sum(!is.na(Panel$inghog2d_mean)), "\n")
cat("ENAHO celdas confiables (>=10 hog):", sum(Panel$enaho_reliable == 1, na.rm = TRUE), "\n")

Panel %>%
  summarise(across(
    c(canon_credited_mpen, inghog2d_mean, pct_pobre, revenue_conc_usd),
    ~ sum(is.na(.))
  )) %>%
  print()


# ==============================================================================
# ---- Deflactores de precios --------------------------------------------------
# ==============================================================================

# ---- Peru CPI: construido desde LINPE de ENAHO (canasta base INEI) ----------
# LINPE = línea de pobreza mensual per cápita en soles nominales
# Ratio LINPE_t / LINPE_2010 = deflactor implícito de precios al consumidor

linpe_deflator <- ENAHO_sumaria %>%
  mutate(year = as.integer(AÑO)) %>%
  group_by(year) %>%
  summarise(
    linpe_nac = weighted.mean(LINPE * 12, FACTOR07, na.rm = TRUE),  # anualizar
    .groups   = "drop"
  )

linpe_2010 <- linpe_deflator %>%
  filter(year == 2010) %>%
  pull(linpe_nac)

linpe_deflator <- linpe_deflator %>%
  mutate(cpi_peru = linpe_nac / linpe_2010)   # 1.0 en 2010

# Verificar
print(linpe_deflator)

# ---- US CPI: BLS CPI-U, base 2010 = 1.0 ------------------------------------
# Fuente: BLS CPI-U All Items, annual average. Verificar en bls.gov.
us_cpi <- tibble(
  year   = 2004:2024,
  cpi_us = c(0.881, 0.912, 0.940, 0.969, 1.005, 0.999,
             1.000, 1.032, 1.053, 1.068, 1.086, 1.087,
             1.101, 1.124, 1.152, 1.172, 1.187, 1.243,
             1.342, 1.405, 1.451)
)

# ==============================================================================
# ---- Aplicar deflactores al Panel -------------------------------------------
# ==============================================================================

Panel <- Panel %>%
  left_join(linpe_deflator %>% select(year, cpi_peru), by = "year") %>%
  left_join(us_cpi,                                    by = "year") %>%
  mutate(
    # --- Canon: soles nominales → USD reales 2010 ----------------------------
    # Paso 1: soles nominales → USD nominales (÷ tipo de cambio anual)
    # Paso 2: USD nominales → USD reales 2010 (÷ US CPI)
    canon_credited_rusd = (canon_credited_mpen / pen_usd) / cpi_us,
    canon_auth_rusd     = (canon_auth_mpen     / pen_usd) / cpi_us,
    
    # --- ENAHO: soles nominales → soles reales 2010 -------------------------
    inghog2d_real = inghog2d_mean / cpi_peru,
    ingbruhd_real = ingbruhd_mean / cpi_peru,
    gashog2d_real = gashog2d_mean / cpi_peru,
    
    # --- Log variables reales (para regresión) -------------------------------
    log_canon_r   = log(canon_credited_rusd + 1),
    log_ingreso_r = log(inghog2d_real       + 1),
    log_gasto_r   = log(gashog2d_real       + 1)
  )

# ---- QA: verificar deflactores ----------------------------------------------
cat("=== CPI Peru (2010 = 1.0) ===\n")
Panel %>%
  distinct(year, cpi_peru) %>%
  arrange(year) %>%
  print(n = 25)

cat("\n=== Estadísticas Canon real (USD 2010) ===\n")
Panel %>%
  filter(canon_credited_rusd > 0) %>%
  summarise(
    n       = n(),
    mean    = mean(canon_credited_rusd),
    median  = median(canon_credited_rusd),
    p90     = quantile(canon_credited_rusd, 0.9),
    max     = max(canon_credited_rusd)
  ) %>% print()

cat("\n=== Ingreso real vs nominal (2010) ===\n")
Panel %>%
  filter(year == 2010, !is.na(inghog2d_mean)) %>%
  summarise(
    ingreso_nominal = mean(inghog2d_mean),
    ingreso_real    = mean(inghog2d_real)
  ) %>% print()  # Deberían ser iguales en 2010


# ==============================================================================
# ---- 5. Instrumento Bartik y Regresión IV ------------------------------------
# ==============================================================================

pre_years <- 2004:2006

# ---- 5.1 Índice de precios internacionales (base = promedio 2004–2006) -------
price_base <- All_Prices_long %>%
  filter(year %in% pre_years) %>%
  group_by(mineral) %>%
  summarise(price_base = mean(price, na.rm = TRUE), .groups = "drop")

price_index <- All_Prices_long %>%
  mutate(year = as.integer(year)) %>%
  left_join(price_base, by = "mineral") %>%
  mutate(price_idx = price / price_base) %>%
  select(year, mineral, price_idx)

# ---- 5.2 Shares pre-período a nivel regional --------------------------------
# Canon distribuye 40% a toda la región → instrumento cubre todos los
# distritos de regiones mineras, no solo los productores directos.

ubigeo_geo <- Ubigeo_Master %>%
  distinct(ubigeo6, cod_region)

shares_reg <- Mining_Site %>%
  filter(ETAPA == "Concentración", year %in% pre_years) %>%
  left_join(ubigeo_geo, by = "ubigeo6") %>%
  group_by(cod_region, mineral) %>%
  summarise(revenue_pre = sum(revenue_usd, na.rm = TRUE), .groups = "drop") %>%
  group_by(cod_region) %>%
  mutate(share_reg = revenue_pre / sum(revenue_pre)) %>%
  ungroup() %>%
  select(cod_region, mineral, share_reg)

# ---- 5.3 Bartik = Σ_m share_{r,m,pre} × (P_{m,t} / P_{m,pre}) --------------
Bartik <- ubigeo_geo %>%
  left_join(shares_reg,  by = "cod_region",  relationship = "many-to-many") %>%
  left_join(price_index, by = "mineral",     relationship = "many-to-many") %>%
  group_by(ubigeo6, year) %>%
  summarise(
    bartik     = sum(share_reg * price_idx,       na.rm = TRUE),
    log_bartik = sum(share_reg * log(price_idx),  na.rm = TRUE),
    .groups    = "drop"
  )

cat("Regiones con shares:", n_distinct(shares_reg$cod_region), "\n")
cat("Distritos con Bartik > 0:", sum(Bartik$bartik > 0), "\n")

Panel <- Panel %>%
  select(-any_of(c("bartik", "log_bartik"))) %>%
  left_join(Bartik, by = c("ubigeo6", "year"))

# ---- 5.4 Muestra de regresión -----------------------------------------------
panel_reg <- Panel %>%
  filter(enaho_reliable == 1, !is.na(log_bartik), !is.na(log_ingreso_r))

cat("Filas en muestra:", nrow(panel_reg), "\n")
cat("Distritos:", n_distinct(panel_reg$ubigeo6), "\n")

# ---- 5.5 Primer estadio -----------------------------------------------------
fs <- feols(log_canon_r ~ log_bartik | ubigeo6 + year,
            data    = panel_reg,
            cluster = ~ubigeo6)

cat("\n=== Primer estadio ===\n")
print(coeftable(fs))
cat("F-stat instrumento:", fitstat(fs, "ivwald")[[1]], "\n")

# ---- 5.6 Regresiones IV (2SLS-TWFE) ----------------------------------------
m_ingreso <- feols(log_ingreso_r ~ 1 | ubigeo6 + year | log_canon_r ~ log_bartik,
                   data    = panel_reg,
                   cluster = ~ubigeo6)

m_gasto   <- feols(log_gasto_r   ~ 1 | ubigeo6 + year | log_canon_r ~ log_bartik,
                   data    = panel_reg,
                   cluster = ~ubigeo6)

m_pobre   <- feols(pct_pobre     ~ 1 | ubigeo6 + year | log_canon_r ~ log_bartik,
                   data    = panel_reg,
                   cluster = ~ubigeo6)

# ---- 5.7 Tabla ---------------------------------------------------------------
etable(m_ingreso, m_gasto, m_pobre,
       se.below  = TRUE,
       keep_raw  = "fit_log_canon_r",
       headers   = c("log Income", "log Expenditure", "Poverty Rate"),
       fitstat   = ~ r2 + n + ivwald)


# ---- Bartik con rezago de 1 y 2 años ----------------------------------------
# Canon año t ≈ basado en precios año t-2 (procesamiento MEF ~2 años)

panel_reg <- panel_reg %>%
  arrange(ubigeo6, year) %>%
  group_by(ubigeo6) %>%
  mutate(
    log_bartik_l1 = lag(log_bartik, 1),
    log_bartik_l2 = lag(log_bartik, 2)
  ) %>%
  ungroup()

# Primer estadio con L1
fs_l1 <- feols(log_canon_r ~ log_bartik_l1 | ubigeo6 + year,
               data = panel_reg %>% filter(!is.na(log_bartik_l1)),
               cluster = ~ubigeo6)

# Primer estadio con L2
fs_l2 <- feols(log_canon_r ~ log_bartik_l2 | ubigeo6 + year,
               data = panel_reg %>% filter(!is.na(log_bartik_l2)),
               cluster = ~ubigeo6)

cat("=== Primer estadio L1 ===\n")
print(coeftable(fs_l1))
cat("F-stat L1:", fitstat(fs_l1, "ivwald")[[1]], "\n\n")

cat("=== Primer estadio L2 ===\n")
print(coeftable(fs_l2))
cat("F-stat L2:", fitstat(fs_l2, "ivwald")[[1]], "\n")


# ==============================================================================
# ---- 5.8 Regresión IV con instrumento rezagado (L1) -------------------------
# ==============================================================================
# Justificación L1: Canon año t ≈ función de tributos mineros año t-1, que a su
# vez reflejan precios año t-1. Bartik contemporáneo falla (F=0.33);
# Bartik(t-1) tiene relevancia estadística fuerte (F≈26.5).

# Errores estándar clusterizados a NIVEL REGIONAL (Adão, Kolesár & Morales 2019)
# porque los shares se construyen al nivel de region, no de distrito.
# Esto es más conservador que clusterizar solo a nivel de distrito.

panel_iv <- panel_reg %>% filter(!is.na(log_bartik_l1))

# IV con instrumento L1, cluster doble (ubigeo6 + cod_region)
m_iv_ingreso <- feols(
  log_ingreso_r ~ 1 | ubigeo6 + year | log_canon_r ~ log_bartik_l1,
  data    = panel_iv,
  cluster = ~ ubigeo6 + cod_region
)

m_iv_gasto <- feols(
  log_gasto_r   ~ 1 | ubigeo6 + year | log_canon_r ~ log_bartik_l1,
  data    = panel_iv,
  cluster = ~ ubigeo6 + cod_region
)

m_iv_pobre <- feols(
  pct_pobre     ~ 1 | ubigeo6 + year | log_canon_r ~ log_bartik_l1,
  data    = panel_iv,
  cluster = ~ ubigeo6 + cod_region
)

# Diagnóstico: F-stat del primer estadio (ahora con sintaxis IV correcta)
cat("=== Primer estadio (sintaxis IV) ===\n")
print(summary(m_iv_ingreso, stage = 1))

cat("\n=== Tabla resultados IV ===\n")
etable(m_iv_ingreso, m_iv_gasto, m_iv_pobre,
       se.below  = TRUE,
       keep_raw  = "fit_log_canon_r",
       headers   = c("log Income", "log Expenditure", "Poverty Rate"),
       fitstat   = ~ r2 + n + ivf1 + ivwald)































































































































































































































































































































# ==============================================================================
# ---- 7. Export --------------------------------------------------------------
# ==============================================================================

saveRDS(master_distrito, file.path(DIR_DERIVED, "master_distrito.rds"))
saveRDS(panel,           file.path(DIR_PANEL,   "panel_distrito.rds"))
write.csv(panel,         file.path(DIR_PANEL,   "panel_distrito.csv"),
          row.names = FALSE)

cat("✅ Pipeline completo. Archivos derivados en", DIR_DERIVED, "\n")
cat("✅ Panel en", DIR_PANEL, "\n")

# ==============================================================================
# Fin de Main.R
# ==============================================================================


































##################################
###       Descriptives         ###
##################################




























#################################
###       First Stage         ###
#################################




























###################################
###       IV Estimation         ###
###################################




























#######################################
###       Robustness Checks         ###
#######################################




























