# ==============================================================================
# ---- 3 Descriptive Statistics.R ---------------------------------------------
# ==============================================================================

library(tidyverse)
library(wesanderson)
library(dplyr)
library(ggplot2)
library(tidyr)
library(fixest)


Transferencias_Regionales   <- read_rds("./Data/Transferencias_Regionales.rds")
Transferencias_Provinciales <- read_rds("./Data/Transferencias_Provinciales.rds")
Transferencias_Municipales  <- read_rds("./Data/Transferencias_Municipales.rds")

# ---- Totales por año --------------------------------------------------------

Regional_by_year <- Transferencias_Regionales %>%
  group_by(year) %>%
  summarise(total_authorised = sum(authorised, na.rm = TRUE),
            total_credited   = sum(credited,   na.rm = TRUE), .groups = "drop")

Provincial_by_year <- Transferencias_Provinciales %>%
  group_by(year) %>%
  summarise(total_authorised = sum(authorised, na.rm = TRUE),
            total_credited   = sum(credited,   na.rm = TRUE), .groups = "drop")

Municipal_by_year <- Transferencias_Municipales %>%
  group_by(year) %>%
  summarise(total_authorised = sum(authorised, na.rm = TRUE),
            total_credited   = sum(credited,   na.rm = TRUE), .groups = "drop")

# ---- Diferencias authorised vs credited -------------------------------------

Transferencias_Municipales %>%
  mutate(diff = authorised - credited) %>%
  filter(diff != 0) %>%
  arrange(desc(abs(diff)))

# ---- Plot 1: Regional vs Municipal por año ----------------------------------

Transfers_by_year <- bind_rows(
  Regional_by_year  %>% select(year, total_credited) %>% mutate(type = "Regional"),
  Municipal_by_year %>% select(year, total_credited) %>% mutate(type = "Municipal")
)

ggplot(Transfers_by_year, aes(x = year, y = total_credited, color = type)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(title = "Canon Minero Transfers by Year",
       subtitle = "Regional and Municipal Comparison",
       x = "Year", y = "Transfer (mm.)", color = "Type") +
  scale_x_continuous(breaks = unique(Transfers_by_year$year)) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

# ---- Plot 2: Disparidades regionales ----------------------------------------

ggplot(Transferencias_Regionales, aes(x = year, y = credited, group = name)) +
  geom_point() +
  labs(title = "Transfer Disparities by Region",
       x = "Year", y = "Amount Credited (mm.)") +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

Transferencias_Regionales %>%
  group_by(year) %>%
  summarise(total = n(), over_50 = sum(credited > 50))

# ---- Plot 3: Disparidades municipales ---------------------------------------

ggplot(Transferencias_Municipales, aes(x = year, y = credited, group = ubigeo6)) +
  geom_point() +
  labs(title = "Transfer Disparities by Municipal District",
       x = "Year", y = "Amount (mm.)") +
  scale_x_continuous(breaks = unique(Transferencias_Municipales$year)) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

Transferencias_Municipales %>%
  group_by(year) %>%
  summarise(total = n(), over_50 = sum(credited > 50))
















# ==============================================================================
# ---- 6. Descriptivos --------------------------------------------------------
# ==============================================================================

## 6.1 Coverage ----

## 6.1 Coverage del panel ----
cat("Panel dims:",            nrow(Panel), "filas,", ncol(Panel), "cols\n")
cat("Distritos únicos:",      n_distinct(Panel$ubigeo6), "\n")
cat("Años:",                  n_distinct(Panel$year), "\n")
cat("Distrito-año tratados:", sum(Panel$treated), "\n")
cat("ENAHO confiable (≥10 hogares):", sum(Panel$enaho_reliable, na.rm = TRUE), "\n")
cat("Distritos mineros (revenue>0):", sum(Panel$mining_district), "\n")
cat("Muestra IV final:", nrow(panel_sim), "obs |",
    n_distinct(panel_sim$ubigeo6), "distritos\n")

# ==============================================================================
# ---- DESCRIPTIVA: tablas y figuras para la sección de datos -----------------
# ==============================================================================

library(scales)

# ---- Tabla 1: Summary statistics del panel de regresión --------------------
# Calcula n, media, sd, mediana y percentil 99 del canon real, ingreso real, gasto real, pobreza y porcentaje tratado sobre panel_sim
# Caracterización de la muestra de regresión
# "La muestra de estimación cubre N distritos × T años. El canon real anual promedio es X USD, mediana Y, con una cola larga (P99 = Z)."
tabla_summary <- panel_sim %>%
  summarise(
    n_obs               = n(),
    n_distritos         = n_distinct(ubigeo6),
    n_anios             = n_distinct(year),
    canon_real_mean     = mean(canon_credited_rusd, na.rm = TRUE),
    canon_real_sd       = sd(canon_credited_rusd,   na.rm = TRUE),
    canon_real_p50      = median(canon_credited_rusd, na.rm = TRUE),
    canon_real_p99      = quantile(canon_credited_rusd, 0.99, na.rm = TRUE),
    ingreso_real_mean   = mean(inghog2d_real, na.rm = TRUE),
    gasto_real_mean     = mean(gashog2d_real, na.rm = TRUE),
    pct_pobre_mean      = mean(pct_pobre, na.rm = TRUE),
    pct_treated         = mean(treated)
  )
print(tabla_summary)

# ---- Tabla 2: Tipología de distritos receptores ----------------------------
# Cuenta distritos por tipo_receptor (directo / provincia / región / no_receptor)
# Distribución de los 4 tipos de receptor según Ley 27506
# Justifica el diseño del instrumento: "12.5% de distritos son productores directos, 44.1% reciben canon vía provincia, 29.9% vía región, y 13.4% no reciben."
tabla_tipologia <- clasificacion_canon %>%
  count(tipo_receptor, name = "n_distritos") %>%
  mutate(pct = n_distritos / sum(n_distritos) * 100) %>%
  arrange(desc(n_distritos))
print(tabla_tipologia)

# ---- Tabla 3: Top 10 regiones por canon recibido ---------------------------
# Agrega canon total por región desde Transferencias_Municipales
# Concentración geográfica del canon
# "Áncash, Cajamarca y Arequipa concentran X% del canon nacional acumulado 2004-2024."
tabla_top_regiones <- Transferencias_Municipales %>%
  filter(!is.na(ubigeo6)) %>%
  left_join(Ubigeo_Master %>% select(ubigeo6, region_std), by = "ubigeo6") %>%
  group_by(region_std) %>%
  summarise(
    canon_total_mpen  = sum(credited, na.rm = TRUE),
    n_distritos       = n_distinct(ubigeo6),
    .groups = "drop"
  ) %>%
  mutate(pct_total = canon_total_mpen / sum(canon_total_mpen) * 100) %>%
  arrange(desc(canon_total_mpen)) %>%
  slice_head(n = 10)
print(tabla_top_regiones)



# ---- Figura 2: Distribución del canon entre distritos receptores -----------
# Histograma en escala log del canon entre distrito-año tratados
# Heterogeneidad entre receptores
# "La distribución es altamente asimétrica: la mayoría recibe montos modestos, pero una cola minoritaria de productores directos recibe órdenes de magnitud más."
fig_canon_dist <- panel_sim %>%
  filter(treated == 1) %>%
  ggplot(aes(canon_credited_rusd)) +
  geom_histogram(bins = 50, fill = "#A8A878", color = "white") +
  scale_x_log10(labels = label_number(big.mark = ",")) +
  labs(
    title    = "Distribución del canon real anual por distrito × año",
    subtitle = "Sólo distritos-año con canon > 0; escala log",
    x = "Canon real (USD 2010)",
    y = "Frecuencia",
    caption  = "Fuente: MEF deflactado por FRED CPI-U"
  ) +
  theme_minimal(base_size = 11)
print(fig_canon_dist)
ggsave("./Output/fig_canon_dist.png", fig_canon_dist,
       width = 8, height = 5, dpi = 300)

# ---- Figura 3: Mapa de distritos mineros y canon receptores ----------------
# Mapa coroplético del Perú con tipo_receptor
# Geografía de la redistribución
# Muestra visualmente la cobertura nacional y la concentración en la sierra/sur.
mapa_clasif <- Mapa_Distrito %>%
  mutate(ubigeo6 = pad6(COD_DISTRITO)) %>%
  left_join(
    clasificacion_canon %>% select(ubigeo6, tipo_receptor),
    by = "ubigeo6"
  ) %>%
  mutate(tipo_receptor = factor(
    tipo_receptor,
    levels = c("directo", "provincia", "region", "no_receptor")
  ))

fig_mapa <- ggplot(mapa_clasif) +
  geom_sf(aes(fill = tipo_receptor), color = "white", linewidth = 0.05) +
  scale_fill_manual(
    values = c(
      "directo"     = "#A8341F",
      "provincia"   = "#D88C3F",
      "region"      = "#E8C674",
      "no_receptor" = "grey85"
    ),
    name = "Tipo de receptor"
  ) +
  labs(
    title    = "Distritos del Perú según tipo de receptor de canon (Ley 27506)",
    subtitle = "Productores directos, mismo provincia, misma región, no receptores"
  ) +
  theme_void(base_size = 11)
print(fig_mapa)
ggsave("./Output/fig_mapa_receptores.png", fig_mapa,
       width = 7, height = 9, dpi = 300)

# ---- Figura 4: Trayectorias de ingreso real — mineros vs no mineros --------
# Compara ingreso real medio entre productores directos vs resto, por año
# Test descriptivo de paralelismo
# "Los productores directos tienen mayor nivel de ingreso, pero la dinámica temporal es similar — los efectos fijos de distrito (within) capturan esta diferencia de nivel."
fig_trend <- panel_sim %>%
  mutate(grupo = if_else(ubigeo6 %in% mineros_directos,
                         "Productor directo", "No productor")) %>%
  group_by(year, grupo) %>%
  summarise(ingreso_real = mean(inghog2d_real, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(year, ingreso_real, color = grupo, linetype = grupo)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.6) +
  scale_color_manual(values = c("Productor directo" = "#A8341F",
                                "No productor"      = "#3B7080"),
                     name = NULL) +
  scale_linetype_manual(values = c("Productor directo" = "solid",
                                   "No productor"      = "dashed"),
                        name = NULL) +
  scale_x_continuous(breaks = seq(2004, 2024, 2)) +
  labs(
    title    = "Ingreso real promedio del hogar (PEN 2010)",
    subtitle = "Distritos productores directos vs. resto del país",
    x = NULL, y = "Ingreso anual real",
    caption  = "Fuente: ENAHO sumaria, ponderado por FACTOR07"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top")
print(fig_trend)
ggsave("./Output/fig_trend_ingreso.png", fig_trend,
       width = 8, height = 5, dpi = 300)

# ---- Tabla 4: Correlaciones de variables clave -----------------------------#
# Matriz de correlación entre las variables clave del IV
# Diagnóstico ex-ante
# Muestra que log_canon_sim_l1 correlaciona con log_canon_r (primera etapa) pero no fuertemente con outcomes — consistente con la exclusión.
library(psych)
tabla_corr <- panel_sim %>%
  select(log_canon_r, log_canon_sim_l1, log_bartik_l1,
         log_ingreso_r, log_gasto_r, pct_pobre, log_revenue_conc) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(3)
print(tabla_corr)

# ---- Guardar tablas para Typst ---------------------------------------------
writexl::write_xlsx(
  list(
    summary    = tabla_summary,
    tipologia  = tabla_tipologia,
    top_reg    = tabla_top_regiones,
    correl     = as.data.frame(tabla_corr) %>%
      tibble::rownames_to_column("var")
  ),
  "./Output/tablas_descriptivas.xlsx"
)



# ==============================================================================
# ---- 3.2.1. Sample construction and representativeness ----------------------
# ==============================================================================

# `enaho_reliable` coverage funnel#
funnel <- tibble::tibble(
  step = c(
    "Full district-year frame (1,891 x 21)",
    "ENAHO-reliable cells (>= 10 households)",
    "Drop 2004 (one-period instrument lag)",
    "Drop fixest singletons (single-year districts/provinces)",
    "Final estimation sample (panel_sim)"
  ),
  observations = c(
    nrow(Panel),
    sum(Panel$enaho_reliable == 1),
    sum(Panel$enaho_reliable == 1 & Panel$year > 2004),
    nrow(panel_sim),                 # post-singleton-drop object actually estimated
    nrow(panel_sim)
  ),
  districts = c(
    n_distinct(Panel$ubigeo6),
    n_distinct(Panel$ubigeo6[Panel$enaho_reliable == 1]),
    n_distinct(Panel$ubigeo6[Panel$enaho_reliable == 1 &
                               Panel$year > 2004]),
    n_distinct(panel_sim$ubigeo6),
    n_distinct(panel_sim$ubigeo6)
  )
)
funnel

# ---- Canon nacional anual (2004-2024) ----------------------------
# Bar plot de suma anual del canon nacional
# Evolución temporal
# "El canon exhibe un ciclo boom-bust pronunciado: pico en 2007 (3,868 M PEN), valle en 2016 (1,123 M PEN), nuevo máximo en 2022 (5,883 M PEN). Esta variación temporal es la fuente principal de identificación."
fig_canon_anual <- Panel %>%
  group_by(year) %>%
  summarise(
    canon_real_musd = sum(canon_credited_rusd, na.rm = TRUE) / 1e6,
    n_recipients    = sum(canon_credited_rusd > 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(year, canon_real_musd)) +
  geom_line(linewidth = 0.8, colour = wes_palette("Zissou1", 5)[3]) +
  geom_point(size = 1.6,    colour = wes_palette("Zissou1", 5)[3]) +
  geom_vline(xintercept = c(2007, 2012, 2022),
             linetype = "dashed", colour = "grey60") +
  scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
  scale_x_continuous(breaks = seq(2004, 2024, 2)) +
  labs(
    title    = "Canon Minero distributed to municipalities, 2004–2024",
    subtitle = "National sum of credited transfers (constant USD 2010)",
    x        = NULL,
    y        = "Million USD (2010 = 1)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold"),
    axis.title.y     = element_text(margin = margin(r = 10)),
    panel.grid.minor = element_blank()
  )

print(fig_canon_anual)
ggsave("./Output/fig_canon_anual.png", fig_canon_anual,
       width = 8, height = 5, dpi = 300)

# --- Recipient category division + pre-period reclassification ---

# --- Recipient category division + pre-period reclassification ---
# Construye el crosswalk desde las clasificaciones (Sec 7.2 y 12.H),
# no desde Panel (que no contiene tipo_full/tipo_pre).
district_class <- clasificacion_canon %>%
  select(ubigeo6, tipo_full = tipo_receptor) %>%
  left_join(
    clasificacion_canon_pre %>% select(ubigeo6, tipo_pre = tipo_receptor),
    by = "ubigeo6"
  )

typology <- district_class %>%
  count(tipo_full) %>%
  mutate(share = round(100 * n / sum(n), 1))

# Full-period vs pre-period crosswalk (identification-relevant: how many
# districts "gained" a category because mining expanded after the 2007 boom)
reclass <- district_class %>%
  filter(ubigeo6 %in% panel_sim$ubigeo6) %>%          # estimation sample only
  summarise(
    n_total       = n(),
    n_changed     = sum(tipo_full != tipo_pre, na.rm = TRUE),
    share_changed = round(100 * mean(tipo_full != tipo_pre, na.rm = TRUE), 1)
  )

list(typology = typology, reclassification = reclass)


# --- figure:outcomes_trends + tab:descriptive_stats ---
# (a) three-panel outcome trends (pop-weighted across districts)
trends <- panel_sim %>%
  group_by(year) %>%
  summarise(
    `Log real income` = weighted.mean(log_ingreso_r, n_hogares, na.rm = TRUE),
    `Poverty rate`    = weighted.mean(pct_pobre,     n_hogares, na.rm = TRUE),
    `Gini`            = weighted.mean(gini,          n_hogares, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(-year, names_to = "outcome", values_to = "value") %>%
  mutate(outcome = factor(outcome,
                          levels = c("Log real income", "Poverty rate", "Gini")))

ggplot(trends, aes(year, value)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.2) +
  facet_wrap(~ outcome, scales = "free_y", ncol = 3) +
  scale_x_continuous(breaks = seq(2004, 2024, 4)) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 11)

# (b) descriptive statistics for the estimation sample
vars <- c("log_canon_r", "log_canon_sim_l1",
          "log_ingreso_r",
          "pct_pobre", "pct_pobre_ext", "gini",
          "log_pop_proxy", "tam_hogar_mean",
          "pct_migrante", "pct_emp_minero",
          "pct_nbi", "pct_sin_educ")

desc <- panel_sim %>%
  summarise(across(all_of(vars),
                   list(mean = ~mean(.x, na.rm = TRUE),
                        sd   = ~sd(.x,   na.rm = TRUE),
                        p10  = ~quantile(.x, .10, na.rm = TRUE),
                        p50  = ~quantile(.x, .50, na.rm = TRUE),
                        p90  = ~quantile(.x, .90, na.rm = TRUE),
                        n    = ~sum(!is.na(.x))))) %>%
  pivot_longer(everything(),
               names_to = c("variable", ".value"),
               names_pattern = "(.*)_(mean|sd|p10|p50|p90|n)$")
desc

# ==============================================================================
# ---- 4.1. Empirical Objective -----------------------------------------------
# ==============================================================================
# ---- Construir canon_anual con las 3 series del canal de transmisión ----

# 1) Índice de precios anual: promedio ponderado por revenue (base pre=1)
price_idx_year <- Mining_Site %>%
  filter(ETAPA == "Concentración") %>%
  left_join(price_index, by = c("mineral", "year")) %>%
  group_by(year) %>%
  summarise(
    price_idx_mean = weighted.mean(price_idx, w = revenue_usd, na.rm = TRUE),
    .groups = "drop"
  )

# 2) Revenue minero real (proxy de la base del impuesto a la renta minera)
#    Mining income tax = ~30% sobre utilidades, que escalan con revenue.
#    Como normalizamos al máximo, la constante 0.3 se cancela.
revenue_year <- Mining_Site %>%
  filter(ETAPA == "Concentración") %>%
  group_by(year) %>%
  summarise(revenue_usd_total = sum(revenue_usd, na.rm = TRUE), .groups = "drop") %>%
  left_join(us_cpi, by = "year") %>%
  mutate(mining_tax_real = revenue_usd_total / cpi_us) %>%
  select(year, mining_tax_real)

# 3) Canon acreditado, USD reales (millones)
canon_year <- Panel %>%
  group_by(year) %>%
  summarise(
    canon_real_musd = sum(canon_credited_rusd, na.rm = TRUE) / 1e6,
    .groups = "drop"
  )

canon_anual <- price_idx_year %>%
  left_join(revenue_year, by = "year") %>%
  left_join(canon_year,   by = "year")

# ---- Figura: cadena de transmisión (normalizada al máximo de cada serie) ----
trans <- canon_anual %>%
  transmute(
    year,
    `Price index (avg, pre=1)` = price_idx_mean,
    `Mining income tax (real)` = mining_tax_real / max(mining_tax_real, na.rm = TRUE),
    `Credited Canon (real)`    = canon_real_musd / max(canon_real_musd, na.rm = TRUE)
  ) %>%
  pivot_longer(-year)

ggplot(trans, aes(year, value, linetype = name)) +
  geom_line(linewidth = 0.7) +
  scale_x_continuous(breaks = seq(2004, 2024, 4)) +
  labs(x = NULL, y = "Normalised to series maximum", linetype = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")



# ==============================================================================
# ---- 4.2. Identification Strategy -------------------------------------------
# ==============================================================================
# ---- Exogeneity of International Prices -------------------------------------
# Shifter normalizado, en logs, en la ventana de estimación
price_decomp <- All_Prices_long %>%
  filter(year %in% full_years) %>%
  left_join(price_base, by = "mineral") %>%
  mutate(log_price_idx = log(price / price_base))

# Varianza total del shifter
var_total <- var(price_decomp$log_price_idx, na.rm = TRUE)

# Modelo con SOLO efectos fijos de año (el "shock común")
m_year <- feols(log_price_idx ~ 1 | year, data = price_decomp)
var_resid_year <- var(resid(m_year), na.rm = TRUE)

cat(sprintf("Varianza total del log shifter:                %.4f\n", var_total))
cat(sprintf("Varianza residual tras year FE:                %.4f\n", var_resid_year))
cat(sprintf("Share que SOBREVIVE a year FE (cross-mineral): %.1f%%\n",
            100 * var_resid_year / var_total))
cat(sprintf("Share absorbido por year FE (common cycle):    %.1f%%\n",
            100 * (1 - var_resid_year / var_total)))


