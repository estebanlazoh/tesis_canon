# ==============================================================================
# ---- 3 Descriptive Statistics.R ---------------------------------------------
# ==============================================================================

library(tidyverse)
library(wesanderson)

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