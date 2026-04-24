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


###############################
###       Join Data         ###
###############################

# ---- Ubigeo Master ----

Ubigeo_Master <- Mapa_Distrito %>%
  st_drop_geometry() %>%
  transmute(
    ubigeo6      = pad6(COD_DISTRITO),
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
  sum(nchar(Ubigeo_Master$ubigeo6) != 6) == 0
)


# ==============================================================================
# ---- Transferencias Municipales: match en 3 pasos ----------------------------
# ==============================================================================

# ---- Paso 0 — alias y pares manuales (sin cambios) --------------------------

prov_alias <- c(
  "CUZCO"             = "CUSCO",
  "DANIEL CARRION"    = "DANIEL ALCIDES CARRION",
  "DANIEL A. CARRION" = "DANIEL ALCIDES CARRION",
  "QUISPICANCHIS"     = "QUISPICANCHI",
  "NAZCA"             = "NASCA",
  "SANCHEZ CERRO"     = "GENERAL SANCHEZ CERRO"
)

manual_pairs <- tribble(
  ~provincia_from,     ~distrito_from,   ~provincia_to,      ~distrito_to,
  "PUTUMAYO",          "SAN ANTONIO",    "MARISCAL NIETO",   "SAN ANTONIO",
  "HUARAZ",            "PAMPAS",         "HUARAZ",           "PAMPAS GRANDE",
  "PUTUMAYO",          "SAN MIGUEL",     "SAN ROMAN",        "SAN MIGUEL",
  "ZARUMILLA",         "SAN MIGUEL",     "SAN ROMAN",        "SAN MIGUEL",
  "PUTUMAYO",          "EL PORVENIR",    "CHINCHEROS",       "EL PORVENIR",
  "TARATA",            "PAMPAS",         "HUARAZ",           "PAMPAS",
  "PUTUMAYO",          "NINABAMBA",      "LA MAR",           "NINABAMBA",
  "HUANUCO",           "QUISQUI",        "HUANUCO",          "QUISQUI (KICHKI)",
  "ZARUMILLA",         "PUEBLO NUEVO",   "LEONCIO PRADO",    "PUEBLO NUEVO",
  "ZARUMILLA",         "EL PORVENIR",    "CHINCHEROS",       "EL PORVENIR",
  "PUTUMAYO",          "COCHABAMBA",     "TAYACAJA",         "COCHABAMBA",
  "PUTUMAYO",          "PUEBLO NUEVO",   "LEONCIO PRADO",    "PUEBLO NUEVO",
  "PUTUMAYO",          "SANTA LUCIA",    "TOCACHE",          "SANTA LUCIA",
  "TARATA",            "EL PORVENIR",    "CHINCHEROS",       "EL PORVENIR",
  "TARATA",            "QUISQUI",        "TARATA",           "QUISQUI (KICHKI)",
  "PADRE ABAD",        "AGUAYTIA",       "PADRE ABAD",       "PADRE ABAD"
)

# ---- transf_base -------------------------------------------------------------

transf_base <- Transferencias_Municipales %>%
  rename(distrito_raw = name, provincia_raw = province) %>%
  mutate(
    code = as.character(code),
    year = as.integer(year),
    
    # Paso 0a: correcciones por código municipal — ANTES de normalizar.
    # Cada código que generaba code→ubigeo no-único se ancla aquí al nombre
    # oficial, de modo que el pipeline posterior converge a un único ubigeo6.
    distrito_raw = case_when(
      code == "01-301371"                  ~ "HUACHO",
      code == "03-300939"                  ~ "MOLINO",
      code == "06-300022"                  ~ "BAGUA",
      code == "07-300292"                  ~ "IHUAYLLO",
      TRUE                                 ~ distrito_raw
    ),
    provincia_raw = case_when(
      code == "02-301128"                  ~ "TRUJILLO",
      code == "06-301764" & year == 2015L  ~ "SAN MARTIN",
      code == "18-301069" & year == 2015L  ~ "JAUJA",
      TRUE                                 ~ provincia_raw
    ),
    
    # Paso 0b: normalización
    distrito_std  = normalizar_texto(distrito_raw) %>%
      str_remove("\\s*\\([^)]+\\)") %>%
      str_squish(),
    provincia_std = normalizar_texto(provincia_raw) %>%
      recode(!!!prov_alias),
    
    # Paso 0c: correcciones de nombre estandarizado (capital provincial, typos)
    distrito_std = case_when(
      provincia_std == "CARLOS FERMIN FITZCARRALD" &
        distrito_std == "CARLOS FERMIN FITZCARRALD"  ~ "SAN LUIS",
      provincia_std == "SAN ANTONIO DE PUTINA"       &
        distrito_std == "SAN ANTONIO DE PUTINA"      ~ "PUTINA",
      provincia_std == "HUAMANGA"                    &
        distrito_std == "HUAMANGA"                   ~ "AYACUCHO",
      provincia_std == "CONTRALMIRANTE VILLAR"        &
        distrito_std == "CONTRALMIRANTE VILLAR"      ~ "ZORRITOS",
      provincia_std == "CORONEL PORTILLO"            &
        distrito_std == "CORONEL PORTILLO"           ~ "CALLERIA",
      provincia_std == "DATEM DEL MARANON"           &
        distrito_std == "DATEM DEL MARANON"          ~ "BARRANCA",
      provincia_std == "MARISCAL RAMON CASTILLA"     &
        distrito_std == "MARISCAL RAMON CASTILLA"    ~ "RAMON CASTILLA",
      distrito_std == "VITARTE"                      ~ "ATE",
      distrito_std == "AGUAITIA"                     ~ "AGUAYTIA",
      provincia_std == "LUCANAS" &
        distrito_std == "HUAS"                       ~ "HUAC HUAS",
      TRUE                                           ~ distrito_std
    )
  ) %>%
  # Paso 0d: ajustes manuales por par (provincia_std, distrito_std)
  left_join(
    manual_pairs,
    by = c("provincia_std" = "provincia_from",
           "distrito_std"  = "distrito_from")
  ) %>%
  mutate(
    provincia_std      = coalesce(provincia_to, provincia_std),
    distrito_std       = coalesce(distrito_to,  distrito_std),
    manual_fix_applied = !is.na(provincia_to) | !is.na(distrito_to)
  ) %>%
  select(-provincia_to, -distrito_to)

# ---- Paso 1: match fuerte (provincia, distrito) ------------------------------

m1 <- transf_base %>%
  left_join(
    Ubigeo_Master %>% select(ubigeo6, provincia_std, distrito_std, region_std),
    by = c("provincia_std", "distrito_std")
  ) %>%
  mutate(match_method = if_else(!is.na(ubigeo6), "prov_dist", NA_character_))

# ---- Paso 2: fallback — distrito único en Perú -------------------------------

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

# ---- Paso 3: crosswalk por code (cubre años con provincia errónea) -----------

code_xwalk <- m2 %>%
  filter(!is.na(ubigeo6)) %>%
  count(code, ubigeo6, match_method, name = "n_obs") %>%
  group_by(code) %>%
  slice_max(n_obs, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(code, ubigeo6_code = ubigeo6, src_code = match_method)

m3 <- m2 %>%
  left_join(code_xwalk, by = "code") %>%
  mutate(
    ubigeo6      = coalesce(ubigeo6, ubigeo6_code),
    match_method = coalesce(match_method,
                            if_else(!is.na(ubigeo6_code),
                                    paste0("code_xwalk[", src_code, "]"),
                                    "unmatched"))
  ) %>%
  select(-ubigeo6_code, -src_code)

# ---- QA ----------------------------------------------------------------------

qa_final <- m3 %>%
  summarise(
    n_total            = n(),
    n_match            = sum(match_method != "unmatched"),
    n_unmatch          = sum(match_method == "unmatched"),
    pct_match_rows     = n_match / n_total,
    pct_match_credited = sum(credited[match_method != "unmatched"], na.rm = TRUE) /
      sum(credited, na.rm = TRUE),
    n_manual_fix       = sum(manual_fix_applied, na.rm = TRUE)
  )
print(qa_final)

unmatched_transf <- m3 %>%
  filter(match_method == "unmatched") %>%
  group_by(provincia_std, distrito_std) %>%
  summarise(n = n(), credited = sum(credited, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(credited))
print(unmatched_transf, n = 100)

# Chequeos de unicidad — deben devolver 0 filas
m3 %>% distinct(code, ubigeo6) %>% count(code) %>% filter(n > 1)
m3 %>% count(ubigeo6, year) %>% filter(n > 1)
























# ==============================================================================
# ---- Mining Site Long: match por triple llave --------------------------------
# ==============================================================================

# Ubigeo_Master sin paréntesis (para join con Mining donde nombres son más limpios)
ubigeo_mining <- Ubigeo_Master %>%
  mutate(
    distrito_std = str_remove(distrito_std, "\\s*\\([^)]+\\)") %>% str_squish()
  ) %>%
  distinct(ubigeo6, region_std, provincia_std, distrito_std)

mining_match <- Mining_Site_long %>%
  mutate(
    year     = as.integer(ANO),
    dist_std = normalizar_texto(DISTRITO_STD) %>%
      str_replace_all("-", " ") %>%
      str_squish(),
    prov_std = normalizar_texto(PROVINCIA_STD) %>%
      recode("NAZCA" = "NASCA"),
    reg_std  = normalizar_texto(REGION_STD)
  ) %>%
  left_join(
    ubigeo_mining %>% select(ubigeo6, region_std, provincia_std, distrito_std),
    by = c("reg_std" = "region_std",
           "prov_std" = "provincia_std",
           "dist_std" = "distrito_std")
  ) %>%
  mutate(match_method = if_else(!is.na(ubigeo6), "triple_key", "unmatched"))

# ---- QA Mining ----

qa_mining <- mining_match %>%
  group_by(match_method) %>%
  summarise(
    n_rows      = n(),
    n_distritos = n_distinct(paste(reg_std, prov_std, dist_std)),
    revenue     = sum(revenue_usd, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(pct_revenue = revenue / sum(revenue, na.rm = TRUE))

print(qa_mining)

unmatched_mining <- mining_match %>%
  filter(match_method == "unmatched") %>%
  group_by(reg_std, prov_std, dist_std) %>%
  summarise(n = n(), revenue = sum(revenue_usd, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(revenue))

print(unmatched_mining, n = 50)
























































































































































































































































































































# ==============================================================================
# ---- 3. Resolución de Llaves por Fuente + QA --------------------------------
# ==============================================================================

## 3.1 ENAHO sumaria: UBIGEO directo (pad a 6) ----

# ENAHO trae UBIGEO de 5 dígitos (sin cero inicial para dptos. 01–09).
enaho_keys <- ENAHO_sumaria %>%
  mutate(
    ubigeo6_raw = as.character(UBIGEO),
    ubigeo6     = pad6(ubigeo6_raw),
    year        = as.integer(AÑO)
  ) %>%
  left_join(
    master_distrito %>% select(ubigeo6, region_std, provincia_std, distrito_std),
    by = "ubigeo6"
  ) %>%
  mutate(
    match_method = if_else(!is.na(distrito_std), "direct_ubigeo", "unmatched"),
    source       = "ENAHO_sumaria"
  )

qa_enaho <- enaho_keys %>%
  summarise(
    n         = n(),
    n_match   = sum(match_method != "unmatched"),
    n_unmatch = sum(match_method == "unmatched"),
    pct_match = n_match / n
  )
print(qa_enaho)

## 3.2 Transferencias municipales: join por provincia+distrito normalizados ----

# OJO: Transferencias_Municipales$code tiene formato "01-300001" — NO es ubigeo INEI.
# No usarlo como llave. Join por nombre después de limpieza (Import_Claude.R).

transf_keys <- Transferencias_Municipales %>%
  mutate(
    distrito_std  = normalize_name(name),
    provincia_std = normalize_name(province),
    year          = as.integer(year)
  ) %>%
  left_join(
    master_distrito %>% select(ubigeo6, provincia_std, distrito_std, region_std),
    by = c("provincia_std", "distrito_std")
  ) %>%
  mutate(
    match_method = if_else(!is.na(ubigeo6), "name_province", "unmatched"),
    source       = "Transferencias_Municipales"
  )

# Detectar ambigüedad (homónimos dentro de misma provincia)
amb_transf <- transf_keys %>%
  filter(!is.na(ubigeo6)) %>%
  distinct(provincia_std, distrito_std, ubigeo6) %>%
  group_by(provincia_std, distrito_std) %>%
  summarise(n_ubigeo = n(), .groups = "drop") %>%
  filter(n_ubigeo > 1)

qa_transf <- transf_keys %>%
  summarise(
    n         = n(),
    n_match   = sum(match_method != "unmatched"),
    n_unmatch = sum(match_method == "unmatched"),
    pct_match = n_match / n,
    n_amb     = nrow(amb_transf)
  )
print(qa_transf)

## 3.3 Mining site distrito: triple llave (distrito + provincia + region) ----

# Mining_Site_distrito agregado solo trae DISTRITO_STD + ANO. Para desambiguar
# homónimos, recuperamos provincia/region desde Mining_Site antes de agregar.

mining_dist_full <- Mining_Site %>%
  transmute(
    distrito_std  = normalize_name(DISTRITO_STD),
    provincia_std = normalize_name(PROVINCIA_STD),
    region_std    = normalize_name(REGION_STD),
    year          = as.integer(ANO)
  ) %>%
  distinct()

mining_keys <- mining_dist_full %>%
  left_join(
    master_distrito %>% select(ubigeo6, region_std, provincia_std, distrito_std),
    by = c("region_std", "provincia_std", "distrito_std")
  ) %>%
  mutate(
    match_method = case_when(
      !is.na(ubigeo6) ~ "triple_key",
      TRUE            ~ "unmatched"
    ),
    source = "Mining_Site"
  )

qa_mining <- mining_keys %>%
  summarise(
    n         = n(),
    n_match   = sum(match_method != "unmatched"),
    n_unmatch = sum(match_method == "unmatched"),
    pct_match = n_match / n
  )
print(qa_mining)

## 3.4 Consolidar casos de revisión manual ----

manual_review <- bind_rows(
  enaho_keys  %>% filter(match_method == "unmatched") %>%
    distinct(source, ubigeo6_raw, ubigeo6),
  transf_keys %>% filter(match_method == "unmatched") %>%
    distinct(source, provincia_std, distrito_std, ubigeo6),
  mining_keys %>% filter(match_method == "unmatched") %>%
    distinct(source, region_std, provincia_std, distrito_std, ubigeo6)
)

## 3.5 Persistir QA (para trazabilidad) ----

write.csv(qa_master,         file.path(DIR_DERIVED, "qa_master.csv"),         row.names = FALSE)
write.csv(qa_master_vs_map,  file.path(DIR_DERIVED, "qa_master_vs_map.csv"),  row.names = FALSE)
write.csv(qa_enaho,          file.path(DIR_DERIVED, "qa_enaho.csv"),          row.names = FALSE)
write.csv(qa_transf,         file.path(DIR_DERIVED, "qa_transferencias.csv"), row.names = FALSE)
write.csv(qa_mining,         file.path(DIR_DERIVED, "qa_mining.csv"),         row.names = FALSE)
write.csv(amb_transf,        file.path(DIR_DERIVED, "qa_transf_ambiguous.csv"), row.names = FALSE)
write.csv(manual_review,     file.path(DIR_DERIVED, "manual_review_keys.csv"), row.names = FALSE)

## 3.6 Gate de calidad: exige >= 98% de match en todas las fuentes ----

MATCH_THRESHOLD <- 0.98
if (qa_enaho$pct_match  < MATCH_THRESHOLD ||
    qa_transf$pct_match < MATCH_THRESHOLD ||
    qa_mining$pct_match < MATCH_THRESHOLD) {
  warning(sprintf(
    "Match rate por debajo de %.0f%% — revisa manual_review_keys.csv antes del panel.\n  ENAHO: %.3f  Transf: %.3f  Mining: %.3f",
    MATCH_THRESHOLD * 100,
    qa_enaho$pct_match, qa_transf$pct_match, qa_mining$pct_match
  ))
}


# ==============================================================================
# ---- 4. Panel Distrito × Año ------------------------------------------------
# ==============================================================================

## 4.1 Esqueleto canónico ----

panel_skeleton <- expand_grid(
  ubigeo6 = master_distrito$ubigeo6,
  year    = full_years
)

## 4.2 Transferencias agregadas por ubigeo6-año ----

transfers_panel <- transf_keys %>%
  filter(!is.na(ubigeo6), !is.na(year)) %>%
  group_by(ubigeo6, year) %>%
  summarise(
    canon_credited_mpen   = sum(credited,   na.rm = TRUE),
    canon_authorised_mpen = sum(authorised, na.rm = TRUE),
    .groups = "drop"
  )

## 4.3 ENAHO agregada con peso muestral ----

enaho_panel <- enaho_keys %>%
  filter(!is.na(ubigeo6), !is.na(year)) %>%
  group_by(ubigeo6, year) %>%
  summarise(
    n_households     = n(),
    ingbruhd_mean    = weighted.mean(INGBRUHD, FACTOR07, na.rm = TRUE),
    inghog2d_mean    = weighted.mean(INGHOG2D, FACTOR07, na.rm = TRUE),
    gashog2d_mean    = weighted.mean(GASHOG2D, FACTOR07, na.rm = TRUE),
    pct_poor         = weighted.mean(POBREZA == 1, FACTOR07, na.rm = TRUE),
    pct_extreme_poor = weighted.mean(POBREZA == 2, FACTOR07, na.rm = TRUE),
    .groups = "drop"
  )

## 4.4 Mining site agregado distrito-año ----

# production total (kg ó TMF según clasificación) por distrito-año
mining_panel <- Mining_Site_long %>%
  transmute(
    distrito_std  = normalize_name(DISTRITO_STD),
    provincia_std = normalize_name(PROVINCIA_STD),
    region_std    = normalize_name(REGION_STD),
    year          = as.integer(ANO),
    production    = as.numeric(production)
  ) %>%
  left_join(
    master_distrito %>% select(ubigeo6, region_std, provincia_std, distrito_std),
    by = c("region_std", "provincia_std", "distrito_std")
  ) %>%
  filter(!is.na(ubigeo6), !is.na(year)) %>%
  group_by(ubigeo6, year) %>%
  summarise(produccion_total = sum(production, na.rm = TRUE), .groups = "drop")

## 4.5 Covariables time-invariant desde master ----

ubigeo_chars <- master_distrito %>%
  select(ubigeo6, departamento_std, provincia_std, distrito_std, region_std,
         altitude, latitude, longitude, superficie, pob_densidad_2020,
         idh_2019, ivfa, pct_pobreza_total, pct_pobreza_extrema)

## 4.6 Ensamblar panel ----

panel <- panel_skeleton %>%
  left_join(transfers_panel, by = c("ubigeo6", "year")) %>%
  left_join(enaho_panel,     by = c("ubigeo6", "year")) %>%
  left_join(mining_panel,    by = c("ubigeo6", "year")) %>%
  left_join(ubigeo_chars,    by = "ubigeo6") %>%
  left_join(EX,              by = "year") %>%
  mutate(
    canon_credited_mpen   = replace_na(canon_credited_mpen,   0),
    canon_authorised_mpen = replace_na(canon_authorised_mpen, 0),
    produccion_total      = replace_na(produccion_total,      0),
    canon_credited_musd   = canon_credited_mpen / pen_usd
  )

# QA del panel
qa_panel <- tibble(
  n_rows       = nrow(panel),
  n_distritos  = n_distinct(panel$ubigeo6),
  n_years      = n_distinct(panel$year),
  n_dup_key    = sum(duplicated(panel[, c("ubigeo6", "year")]))
)
print(qa_panel)
stopifnot(qa_panel$n_dup_key == 0)


# ==============================================================================
# ---- 5. Variables de Tratamiento (DiD / IV-Bartik) --------------------------
# ==============================================================================

## 5.1 Intensive / extensive margin ----

panel <- panel %>%
  mutate(
    treated       = as.integer(canon_credited_mpen > 0),
    log_canon_pen = log(canon_credited_mpen + 1),
    log_canon_usd = log(canon_credited_musd + 1),
    log_prod      = log(produccion_total    + 1)
  )

## 5.2 Ever-treated y timing de tratamiento ----

ever_treated <- panel %>%
  group_by(ubigeo6) %>%
  summarise(ever_treated = as.integer(any(treated == 1)), .groups = "drop")

first_year_treated <- panel %>%
  filter(treated == 1) %>%
  group_by(ubigeo6) %>%
  summarise(first_treat_year = min(year), .groups = "drop")

panel <- panel %>%
  left_join(ever_treated,       by = "ubigeo6") %>%
  left_join(first_year_treated, by = "ubigeo6") %>%
  mutate(
    time_to_treat = if_else(!is.na(first_treat_year),
                            year - first_treat_year, NA_integer_)
  )

## 5.3 Shares pre-período para Bartik (shift-share IV) ----

# Shares de producción minera por distrito en el período pre-reforma (2004–2006)
# Ajusta el cutoff pre_window según el diseño de identificación.

pre_window <- 2004:2006

mining_shares_pre <- panel %>%
  filter(year %in% pre_window) %>%
  group_by(ubigeo6) %>%
  summarise(prod_pre = sum(produccion_total, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    prod_pre_total = sum(prod_pre, na.rm = TRUE),
    share_pre      = if_else(prod_pre_total > 0, prod_pre / prod_pre_total, 0)
  ) %>%
  select(ubigeo6, share_pre)

# Shift nacional: precio o producción agregada (placeholder; conectar con IV real)
national_shift <- panel %>%
  group_by(year) %>%
  summarise(prod_nacional = sum(produccion_total, na.rm = TRUE), .groups = "drop")

bartik <- expand_grid(
  ubigeo6 = unique(panel$ubigeo6),
  year    = full_years
) %>%
  left_join(mining_shares_pre, by = "ubigeo6") %>%
  left_join(national_shift,    by = "year") %>%
  mutate(bartik_iv = share_pre * prod_nacional)

panel <- panel %>%
  left_join(bartik %>% select(ubigeo6, year, share_pre, bartik_iv),
            by = c("ubigeo6", "year"))


# ==============================================================================
# ---- 6. Descriptivos --------------------------------------------------------
# ==============================================================================

## 6.1 Coverage ----

cat("Panel dims:",        nrow(panel), "rows,", ncol(panel), "cols\n")
cat("Distritos:",          n_distinct(panel$ubigeo6), "\n")
cat("Años:",               n_distinct(panel$year), "\n")
cat("Ever-treated:",       sum(ever_treated$ever_treated), "\n")
cat("Never-treated:",      sum(ever_treated$ever_treated == 0), "\n")
cat("ENAHO con ingreso:",  sum(!is.na(panel$ingbruhd_mean)), "\n")
cat("Mining>0:",           sum(panel$produccion_total > 0, na.rm = TRUE), "\n")

## 6.2 Transferencias totales por año ----

transfers_by_year <- panel %>%
  group_by(year) %>%
  summarise(
    total_canon_mpen = sum(canon_credited_mpen, na.rm = TRUE),
    n_treated        = sum(treated),
    .groups = "drop"
  )

ggplot(transfers_by_year, aes(x = year, y = total_canon_mpen)) +
  geom_col(fill = "#3B7CB4") +
  scale_x_continuous(breaks = full_years) +
  labs(title = "Canon Minero total transferido a municipalidades",
       x = "Año", y = "Total acreditado (millones PEN)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 6.3 Distribución log-transfers entre tratados ----

panel %>%
  filter(ever_treated == 1, year == 2010) %>%
  ggplot(aes(x = log_canon_pen)) +
  geom_histogram(bins = 40, fill = "#3B7CB4", colour = "white") +
  labs(title = "Distribución de log Canon (tratados, 2010)",
       x = "log(canon + 1, millones PEN)", y = "Frecuencia") +
  theme_minimal()


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




























