library(tidyverse)

phones <- readRDS("dataset definitivo.rds")

battery_df <- phones |>
  select(
    Brand,
    Model,
    Released.Year,
    battery_type,
    battery_capacity_mah,
    charging_power_w,
    wireless_qi,
    wireless_reverse,
    wireless_charging_power_w
  )

summary(battery_df)

battery_df %>%
  summarise(
    n = n(),
    missing_type = sum(is.na(battery_type)),
    missing_capacity = sum(is.na(battery_capacity_mah)),
    missing_charging = sum(is.na(charging_power_w)),
    missing_qi = sum(is.na(wireless_qi)),
    missing_reverse = sum(is.na(wireless_reverse)),
    missing_wireless_power = sum(is.na(wireless_charging_power_w))
  )

battery_df %>%
  filter(Released.Year %in% c(2023, 2024)) %>%
  count(Released.Year, battery_type) %>%
  group_by(Released.Year) %>%
  mutate(
    percentage = n / sum(n) * 100
  ) %>%
  ungroup()

battery_df %>%
  filter(Released.Year %in% c(2023, 2024)) %>%
  group_by(Released.Year, battery_type) %>%
  summarise(
    n = n(),
    mean_mah = mean(battery_capacity_mah),
    median_mah = median(battery_capacity_mah),
    p25_mah = quantile(battery_capacity_mah, 0.25),
    p75_mah = quantile(battery_capacity_mah, 0.75),
    max_mah = max(battery_capacity_mah),
    .groups = "drop"
  )


battery_2024_type <- phones %>%
  filter(Released.Year == 2024) %>%
  filter(!is.na(battery_type), !is.na(battery_capacity_mah)) %>%
  group_by(battery_type) %>%
  summarise(
    n = n(),
    mean_capacity = mean(battery_capacity_mah),
    median_capacity = median(battery_capacity_mah),
    p25 = quantile(battery_capacity_mah, 0.25),
    p75 = quantile(battery_capacity_mah, 0.75),
    max_capacity = max(battery_capacity_mah),
    .groups = "drop"
  )

battery_2024_type

# ============================================================
# BATTERY INNOVATION INDEX - METODOLOGIA DEFINITIVA
# ============================================================
# Principio: il BI misura l'innovazione tecnologica incorporata,
# non la semplice performance.
#
# Silicon-Carbon:
# - tecnologia già presente nel campione 2023;
# - frontiera configurazionale osservata nel 2023 = 5.450 mAh;
# - solo il superamento di tale frontiera genera estensione positiva;
# - le batterie convenzionali ricevono SC = 1.
#
# Wireless charging:
# - frontiera osservata 2023 = 50 W;
# - WP_i = WirelessPower_i / 50;
# - missing wireless power -> WP = 1.
#
# BI baseline:
# BI_i = 1 + 0.80*(SC_i - 1) + 0.20*(WP_i - 1)

SC_FRONTIER_2023 <- 5450
WIRELESS_FRONTIER_2023 <- 50

# ------------------------------------------------------------
# 1. Silicon-Carbon: frontiera osservata 2023
# ------------------------------------------------------------

sc_2023 <- phones %>%
  filter(
    Released.Year == 2023,
    battery_type == "Silicon-Carbon Li-ion"
  ) %>%
  select(
    Brand,
    Model,
    battery_type,
    battery_capacity_mah
  )

sc_2023_capacities <- sc_2023 %>%
  filter(!is.na(battery_capacity_mah)) %>%
  pull(battery_capacity_mah)

if (length(sc_2023_capacities) == 0) {
  stop("Nessuna capacità Silicon-Carbon 2023 disponibile: impossibile costruire la frontiera.")
}

if (length(sc_2023_capacities) != 2) {
  warning(
    paste0(
      "Sono state trovate ", length(sc_2023_capacities),
      " capacità Silicon-Carbon 2023 non-missing; ",
      "la metodologia attesa prevede 2 osservazioni."
    )
  )
}

if (!isTRUE(all.equal(max(sc_2023_capacities), SC_FRONTIER_2023))) {
  stop(
    paste0(
      "La frontiera Silicon-Carbon 2023 osservata nel dataset (",
      max(sc_2023_capacities),
      " mAh) non coincide con il riferimento metodologico di ",
      SC_FRONTIER_2023,
      " mAh."
    )
  )
}

sc_2023_summary <- tibble(
  n_sc_2023 = length(sc_2023_capacities),
  min_sc_2023 = min(sc_2023_capacities),
  max_sc_2023 = max(sc_2023_capacities),
  median_sc_2023 = median(sc_2023_capacities),
  frontier_sc_2023 = SC_FRONTIER_2023
)

# ------------------------------------------------------------
# 2. Silicon-Carbon: estensione della frontiera 2023 nel 2024
# ------------------------------------------------------------

battery_sc_2024 <- phones %>%
  filter(Released.Year == 2024) %>%
  mutate(
    SC_index = case_when(
      battery_type == "Silicon-Carbon Li-ion" &
        !is.na(battery_capacity_mah) ~
        pmax(battery_capacity_mah / SC_FRONTIER_2023, 1),
      TRUE ~ 1
    ),

    HE_SC = SC_index - 1
  ) %>%
  select(
    Brand,
    Model,
    battery_type,
    battery_capacity_mah,
    SC_index,
    HE_SC
  )

sc_2024 <- battery_sc_2024 %>%
  filter(battery_type == "Silicon-Carbon Li-ion")

sc_2024_above_frontier <- sc_2024 %>%
  filter(
    !is.na(battery_capacity_mah),
    battery_capacity_mah > SC_FRONTIER_2023
  ) %>%
  arrange(desc(battery_capacity_mah))

sc_2024_at_or_below_frontier <- sc_2024 %>%
  filter(
    is.na(battery_capacity_mah) |
      battery_capacity_mah <= SC_FRONTIER_2023
  ) %>%
  arrange(desc(battery_capacity_mah))

# Controllo esplicito: le batterie convenzionali non possono avere SC > 1.
conventional_sc_violation <- battery_sc_2024 %>%
  filter(
    battery_type != "Silicon-Carbon Li-ion",
    !is.na(SC_index),
    SC_index > 1 + 1e-12
  )

if (nrow(conventional_sc_violation) > 0) {
  stop("Controllo SC violato: almeno una batteria non Silicon-Carbon ha SC > 1.")
}

# Controllo: nessun Silicon-Carbon <= 5.450 mAh può avere SC > 1.
sc_below_frontier_violation <- battery_sc_2024 %>%
  filter(
    battery_type == "Silicon-Carbon Li-ion",
    !is.na(battery_capacity_mah),
    battery_capacity_mah <= SC_FRONTIER_2023,
    SC_index > 1 + 1e-12
  )

if (nrow(sc_below_frontier_violation) > 0) {
  stop("Controllo SC violato: un Silicon-Carbon <= 5.450 mAh ha SC > 1.")
}

# Controllo generale: SC > 1 solo se la configurazione SC supera la frontiera.
sc_frontier_logic_violation <- battery_sc_2024 %>%
  filter(
    SC_index > 1 + 1e-12,
    battery_type != "Silicon-Carbon Li-ion" |
      is.na(battery_capacity_mah) |
      battery_capacity_mah <= SC_FRONTIER_2023
  )

if (nrow(sc_frontier_logic_violation) > 0) {
  stop("Controllo SC violato: SC > 1 senza superamento della frontiera 2023.")
}

# ------------------------------------------------------------
# 3. Wireless charging: metodologia invariata
# ------------------------------------------------------------
# La colonna wireless può essere character nel dataset originale
# (ad es. valori "NA"). La conversione deve avvenire PRIMA
# di qualsiasi operazione aritmetica sulla variabile.
phones <- phones %>%
  mutate(
    wireless_charging_power_w = case_when(
      is.na(wireless_charging_power_w) ~ NA_real_,
      as.character(wireless_charging_power_w) %in% c("NA", "NA_real_") ~ NA_real_,
      TRUE ~ suppressWarnings(as.numeric(as.character(wireless_charging_power_w)))
    )
  )

if (!is.numeric(phones$wireless_charging_power_w)) {
  stop("Controllo wireless violato: wireless_charging_power_w non è numerica dopo la conversione.")
}

wireless_power_component <- phones %>%
  filter(
    Released.Year == 2024,
    !is.na(wireless_charging_power_w)
  ) %>%
  mutate(
    frontier_2023 = WIRELESS_FRONTIER_2023,
    frontier_excess = pmax(
      0,
      wireless_charging_power_w - frontier_2023
    ),
    frontier_ratio = pmax(
      1,
      wireless_charging_power_w / frontier_2023
    )
  ) %>%
  select(
    Brand,
    Model,
    wireless_charging_power_w,
    frontier_2023,
    frontier_excess,
    frontier_ratio
  ) %>%
  arrange(desc(wireless_charging_power_w))

wireless_power_component |> print(n = Inf) |> clipr::write_clip()

# Missing wireless power = nessuna penalizzazione:
# WP = 1, senza interpretare il missing come assenza della tecnologia.
wireless_power_index <- phones %>%
  filter(Released.Year == 2024) %>%
  mutate(
    wireless_power_frontier_2023 = WIRELESS_FRONTIER_2023,
    wireless_power_index = case_when(
      !is.na(wireless_charging_power_w) ~
        pmax(1, wireless_charging_power_w / wireless_power_frontier_2023),
      TRUE ~ 1
    )
  ) %>%
  select(
    Brand,
    Model,
    wireless_charging_power_w,
    wireless_power_frontier_2023,
    wireless_power_index
  )

wireless_power_summary <- wireless_power_index %>%
  summarise(
    n_2024 = n(),
    n_with_power = sum(!is.na(wireless_charging_power_w)),
    n_missing_power = sum(is.na(wireless_charging_power_w)),
    mean = mean(wireless_power_index, na.rm = TRUE),
    median = median(wireless_power_index, na.rm = TRUE),
    p75 = quantile(wireless_power_index, 0.75, na.rm = TRUE),
    p90 = quantile(wireless_power_index, 0.90, na.rm = TRUE),
    max = max(wireless_power_index, na.rm = TRUE),
    n_wp_gt_1 = sum(wireless_power_index > 1 + 1e-12, na.rm = TRUE),
    n_wp_eq_1 = sum(abs(wireless_power_index - 1) <= 1e-12, na.rm = TRUE)
  )

wireless_frontier_2024 <- wireless_power_index %>%
  summarise(
    frontier_2024 = ifelse(
      all(is.na(wireless_charging_power_w)),
      NA_real_,
      max(wireless_charging_power_w, na.rm = TRUE)
    )
  ) %>%
  pull(frontier_2024)

wireless_he <- ifelse(
  is.na(wireless_frontier_2024),
  NA_real_,
  wireless_frontier_2024 / WIRELESS_FRONTIER_2023 - 1
)

# Controlli wireless richiesti.
wp_80 <- wireless_power_index %>%
  filter(!is.na(wireless_charging_power_w),
         abs(wireless_charging_power_w - 80) <= 1e-12)

if (nrow(wp_80) > 0 &&
    any(abs(wp_80$wireless_power_index - 1.60) > 1e-12)) {
  stop("Controllo wireless violato: 80 W non produce WP = 1.60.")
}

if (any(
  wireless_power_index$wireless_power_index[
    is.na(wireless_power_index$wireless_charging_power_w)
  ] != 1
)) {
  stop("Controllo wireless violato: i missing non producono WP = 1.")
}

# ------------------------------------------------------------
# 4. Battery Innovation Index RAW baseline 80/20
# ------------------------------------------------------------

battery_index <- phones %>%
  filter(Released.Year == 2024) %>%
  left_join(
    battery_sc_2024 %>%
      select(Brand, Model, SC_index, HE_SC),
    by = c("Brand", "Model")
  ) %>%
  left_join(
    wireless_power_index %>%
      select(Brand, Model, wireless_power_index),
    by = c("Brand", "Model")
  ) %>%
  mutate(
    WP_index = wireless_power_index,

    Battery_Innovation_Index =
      1 +
      0.80 * (SC_index - 1) +
      0.20 * (WP_index - 1)
  )

# Controllo: le tre variabili escluse non entrano nel calcolo del BI.
# charging_power_w, wireless_qi e wireless_reverse sono utilizzate
# esclusivamente nelle statistiche descrittive precedenti e non nella formula.
excluded_variables <- c(
  "charging_power_w",
  "wireless_qi",
  "wireless_reverse"
)

if (!all(excluded_variables %in% names(battery_index))) {
  stop("Una o più variabili escluse non sono presenti nel dataset: verificare la struttura originale.")
}

# Controlli matematici sul BI.
if (any(
  battery_index$Battery_Innovation_Index <
    1 - 1e-12,
  na.rm = TRUE
)) {
  stop("Controllo BI violato: il Battery Innovation Index non può essere inferiore a 1.")
}

# ------------------------------------------------------------
# 5. Sensitivity analysis: 70/30, 75/25, 80/20, 85/15
# ------------------------------------------------------------

battery_sensitivity <- battery_index %>%
  mutate(
    BI_70_30 = 1 +
      0.70 * (SC_index - 1) +
      0.30 * (WP_index - 1),

    BI_75_25 = 1 +
      0.75 * (SC_index - 1) +
      0.25 * (WP_index - 1),

    BI_80_20 = 1 +
      0.80 * (SC_index - 1) +
      0.20 * (WP_index - 1),

    BI_85_15 = 1 +
      0.85 * (SC_index - 1) +
      0.15 * (WP_index - 1)
  )

# BI_80_20 deve coincidere con la baseline.
if (any(
  abs(
    battery_sensitivity$BI_80_20 -
      battery_sensitivity$Battery_Innovation_Index
  ) > 1e-12,
  na.rm = TRUE
)) {
  stop("Controllo sensitivity violato: BI_80_20 non coincide con la baseline.")
}

sensitivity_summary <- battery_sensitivity %>%
  summarise(
    across(
      c(BI_70_30, BI_75_25, BI_80_20, BI_85_15),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE),
        min = ~ min(.x, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  )

# Ranking e stabilità.
ranked_sensitivity <- battery_sensitivity %>%
  mutate(
    rank_70_30 = min_rank(desc(BI_70_30)),
    rank_75_25 = min_rank(desc(BI_75_25)),
    rank_80_20 = min_rank(desc(BI_80_20)),
    rank_85_15 = min_rank(desc(BI_85_15))
  )

rank_comparison <- function(data, alternative_rank, baseline_rank, comparison_name) {
  d <- data[[alternative_rank]] - data[[baseline_rank]]

  rho <- suppressWarnings(
    cor.test(
      data[[alternative_rank]],
      data[[baseline_rank]],
      method = "spearman",
      exact = FALSE
    )$estimate
  )

  tibble(
    Comparison = comparison_name,
    `Spearman rho` = as.numeric(rho),
    `Mean absolute rank change` = mean(abs(d), na.rm = TRUE),
    `Maximum absolute rank change` = max(abs(d), na.rm = TRUE)
  )
}

rank_stability <- bind_rows(
  rank_comparison(
    ranked_sensitivity,
    "rank_70_30",
    "rank_80_20",
    "70/30 vs 80/20"
  ),
  rank_comparison(
    ranked_sensitivity,
    "rank_75_25",
    "rank_80_20",
    "75/25 vs 80/20"
  ),
  rank_comparison(
    ranked_sensitivity,
    "rank_85_15",
    "rank_80_20",
    "85/15 vs 80/20"
  )
)

# ------------------------------------------------------------
# 6. Normalizzazione definitiva: ln(1+x) + min-max 0-100
# ------------------------------------------------------------

bi_raw_values <- battery_index$Battery_Innovation_Index

if (any(bi_raw_values < 0, na.rm = TRUE)) {
  stop("Impossibile applicare ln(1+x): sono presenti BI raw negativi.")
}

bi_log_min <- min(log1p(bi_raw_values), na.rm = TRUE)
bi_log_max <- max(log1p(bi_raw_values), na.rm = TRUE)

if (abs(bi_log_max - bi_log_min) < .Machine$double.eps) {
  stop("Normalizzazione impossibile: BI_log ha valore costante.")
}

battery_index <- battery_index %>%
  mutate(
    BI_log = log1p(Battery_Innovation_Index),

    Battery_Innovation_log_norm =
      100 *
      (BI_log - bi_log_min) /
      (bi_log_max - bi_log_min)
  )

# Controllo della normalizzazione.
if (abs(min(battery_index$Battery_Innovation_log_norm, na.rm = TRUE)) > 1e-10) {
  stop("Controllo normalizzazione violato: il minimo non è 0.")
}

if (abs(max(battery_index$Battery_Innovation_log_norm, na.rm = TRUE) - 100) > 1e-10) {
  stop("Controllo normalizzazione violato: il massimo non è 100.")
}

# ------------------------------------------------------------
# 7. Controllo specifico Xiaomi Mi Mix Flip
# ------------------------------------------------------------

mi_mix_flip_check <- battery_index %>%
  filter(str_detect(str_to_lower(Model), "mi mix flip")) %>%
  select(
    Brand,
    Model,
    battery_type,
    battery_capacity_mah,
    SC_index
  )

if (nrow(mi_mix_flip_check) > 0) {
  mi_mix_flip_4780 <- mi_mix_flip_check %>%
    filter(abs(battery_capacity_mah - 4780) <= 1e-12)

  if (nrow(mi_mix_flip_4780) > 0 &&
      any(abs(mi_mix_flip_4780$SC_index - 1) > 1e-12)) {
    stop("Controllo Xiaomi Mi Mix Flip violato: 4.780 mAh Silicon-Carbon deve avere SC = 1.")
  }
}

# ------------------------------------------------------------
# 8. Dataset finale da salvare
# ------------------------------------------------------------

battery_innovation_df <- battery_index %>%
  select(
    Brand,
    Model,
    model_std,
    Released.Year,
    Battery_Innovation_Index,
    Battery_Innovation_log_norm
  )

saveRDS(battery_innovation_df, "Battery Innovation Index.rds")

# ============================================================
# OUTPUT DA RIPORTARE NELLA NOTA METODOLOGICA
# ============================================================

cat("\n\n")
cat("============================================================\n")
cat("OUTPUT DA RIPORTARE NELLA NOTA METODOLOGICA\n")
cat("============================================================\n")

# ------------------------------------------------------------
# A. CAMPIONE
# ------------------------------------------------------------

n_2023 <- sum(phones$Released.Year == 2023, na.rm = TRUE)
n_2024 <- sum(phones$Released.Year == 2024, na.rm = TRUE)

n_sc_2023 <- nrow(sc_2023)
n_sc_2024 <- nrow(sc_2024)

cat("\n-----------------------------\n")
cat("A. CAMPIONE\n")
cat("-----------------------------\n")
cat("Numero totale smartphone 2023:", n_2023, "\n")
cat("Numero totale smartphone 2024:", n_2024, "\n")
cat("Numero smartphone Silicon-Carbon 2023:", n_sc_2023, "\n")
cat("Numero smartphone Silicon-Carbon 2024:", n_sc_2024, "\n")
cat("Capacità Silicon-Carbon 2023 (mAh):",
    paste(sc_2023_capacities, collapse = ", "), "\n")
cat("Minimo SC 2023 (mAh):", min(sc_2023_capacities), "\n")
cat("Massimo SC 2023 (mAh):", max(sc_2023_capacities), "\n")
cat("Mediana SC 2023 (mAh):", median(sc_2023_capacities), "\n")
cat("Frontiera Silicon-Carbon 2023 (mAh):", SC_FRONTIER_2023, "\n")

# ------------------------------------------------------------
# B. ESTENSIONE SILICON-CARBON
# ------------------------------------------------------------

sc_2024_above_n <- nrow(sc_2024_above_frontier)
sc_2024_above_share <- ifelse(
  n_2024 > 0,
  sc_2024_above_n / n_2024 * 100,
  NA_real_
)

sc_values_2024 <- sc_2024$SC_index

cat("\n-----------------------------\n")
cat("B. ESTENSIONE SILICON-CARBON\n")
cat("-----------------------------\n")
cat("Frontiera SC 2023:", SC_FRONTIER_2023, "mAh\n")
cat("Numero SC 2024 > 5.450 mAh:", sc_2024_above_n, "\n")
cat("Percentuale sul totale 2024:",
    round(sc_2024_above_share, 2), "%\n")

cat("\nSmartphone SC 2024 che superano la frontiera:\n")
if (sc_2024_above_n > 0) {
  print(
    sc_2024_above_frontier %>%
      transmute(
        Model,
        Capacity_mAh = battery_capacity_mah,
        SC_raw = SC_index,
        HE_SC
      ),
    row.names = FALSE
  )
} else {
  cat("Nessuno.\n")
}

cat("\nSmartphone SC 2024 con capacità <= 5.450 mAh o missing:\n")
if (nrow(sc_2024_at_or_below_frontier) > 0) {
  print(
    sc_2024_at_or_below_frontier %>%
      transmute(
        Model,
        Capacity_mAh = battery_capacity_mah,
        SC_raw = SC_index
      ),
    row.names = FALSE
  )
} else {
  cat("Nessuno.\n")
}

cat("\nMassimo HE_SC:", max(battery_sc_2024$HE_SC, na.rm = TRUE), "\n")
cat("Massimo SC:", max(sc_values_2024, na.rm = TRUE), "\n")
cat("Media SC:", mean(sc_values_2024, na.rm = TRUE), "\n")
cat("Mediana SC:", median(sc_values_2024, na.rm = TRUE), "\n")
cat("SD SC:", sd(sc_values_2024, na.rm = TRUE), "\n")
cat("P25 SC:", quantile(sc_values_2024, 0.25, na.rm = TRUE), "\n")
cat("P75 SC:", quantile(sc_values_2024, 0.75, na.rm = TRUE), "\n")
cat("P90 SC:", quantile(sc_values_2024, 0.90, na.rm = TRUE), "\n")
cat("P95 SC:", quantile(sc_values_2024, 0.95, na.rm = TRUE), "\n")
cat("Numero smartphone con SC = 1:",
    sum(abs(sc_values_2024 - 1) <= 1e-12, na.rm = TRUE), "\n")
cat("Numero smartphone con SC > 1:",
    sum(sc_values_2024 > 1 + 1e-12, na.rm = TRUE), "\n")

# ------------------------------------------------------------
# C. CONTROLLO XIAOMI MI MIX FLIP
# ------------------------------------------------------------

cat("\n-----------------------------\n")
cat("C. CONTROLLO CASO XIAOMI MI MIX FLIP\n")
cat("-----------------------------\n")

if (nrow(mi_mix_flip_check) > 0) {
  print(mi_mix_flip_check, row.names = FALSE)
} else {
  cat("Nessun modello contenente 'Mi Mix Flip' trovato nel dataset.\n")
}

# ------------------------------------------------------------
# D. WIRELESS POWER
# ------------------------------------------------------------

cat("\n-----------------------------\n")
cat("D. WIRELESS POWER\n")
cat("-----------------------------\n")
cat("Frontiera wireless 2023:", WIRELESS_FRONTIER_2023, "W\n")
cat("Frontiera wireless 2024:", wireless_frontier_2024, "W\n")
cat("HE wireless:", wireless_he, "\n")
cat("Numero smartphone 2024 con WP > 1:",
    wireless_power_summary$n_wp_gt_1, "\n")

cat("\nSmartphone WP > 1:\n")
wp_gt_1 <- wireless_power_index %>%
  filter(wireless_power_index > 1 + 1e-12) %>%
  arrange(desc(wireless_power_index)) %>%
  transmute(
    Model,
    WirelessPower_W = wireless_charging_power_w,
    WP = wireless_power_index
  )

if (nrow(wp_gt_1) > 0) {
  print(wp_gt_1, row.names = FALSE)
} else {
  cat("Nessuno.\n")
}

cat("\nMassimo WP:", wireless_power_summary$max, "\n")
cat("Media WP:", wireless_power_summary$mean, "\n")
cat("Mediana WP:", wireless_power_summary$median, "\n")
cat("Numero missing wireless power:",
    wireless_power_summary$n_missing_power, "\n")
cat("Numero WP = 1:", wireless_power_summary$n_wp_eq_1, "\n")

# ------------------------------------------------------------
# E. BATTERY INNOVATION INDEX RAW 80/20
# ------------------------------------------------------------

bi_raw <- battery_index$Battery_Innovation_Index

cat("\n-----------------------------\n")
cat("E. BATTERY INNOVATION INDEX RAW - BASELINE 80/20\n")
cat("-----------------------------\n")
cat("N:", sum(!is.na(bi_raw)), "\n")
cat("Media:", mean(bi_raw, na.rm = TRUE), "\n")
cat("Mediana:", median(bi_raw, na.rm = TRUE), "\n")
cat("SD:", sd(bi_raw, na.rm = TRUE), "\n")
cat("Minimo:", min(bi_raw, na.rm = TRUE), "\n")
cat("P25:", quantile(bi_raw, 0.25, na.rm = TRUE), "\n")
cat("P75:", quantile(bi_raw, 0.75, na.rm = TRUE), "\n")
cat("P90:", quantile(bi_raw, 0.90, na.rm = TRUE), "\n")
cat("P95:", quantile(bi_raw, 0.95, na.rm = TRUE), "\n")
cat("Massimo:", max(bi_raw, na.rm = TRUE), "\n")
cat("Numero BI = 1:",
    sum(abs(bi_raw - 1) <= 1e-12, na.rm = TRUE), "\n")
cat("Numero BI > 1:",
    sum(bi_raw > 1 + 1e-12, na.rm = TRUE), "\n")

cat("\nTOP 15 smartphone per BI raw:\n")
top15_bi <- battery_index %>%
  arrange(desc(Battery_Innovation_Index), Model) %>%
  mutate(rank = row_number()) %>%
  slice_head(n = 15) %>%
  transmute(
    rank,
    Model,
    battery_type,
    capacity = battery_capacity_mah,
    SC = SC_index,
    `wireless power` = wireless_charging_power_w,
    WP = WP_index,
    `BI raw` = Battery_Innovation_Index
  )

print(top15_bi, row.names = FALSE)

# ------------------------------------------------------------
# F. NORMALIZZAZIONE
# ------------------------------------------------------------

cat("\n-----------------------------\n")
cat("F. NORMALIZZAZIONE\n")
cat("-----------------------------\n")
cat("Minimo BI raw:", min(bi_raw, na.rm = TRUE), "\n")
cat("Massimo BI raw:", max(bi_raw, na.rm = TRUE), "\n")
cat("Minimo BI_log:", min(battery_index$BI_log, na.rm = TRUE), "\n")
cat("Massimo BI_log:", max(battery_index$BI_log, na.rm = TRUE), "\n")
cat("Minimo BI normalized:", min(battery_index$Battery_Innovation_log_norm, na.rm = TRUE), "\n")
cat("Massimo BI normalized:", max(battery_index$Battery_Innovation_log_norm, na.rm = TRUE), "\n")
cat("Media BI normalized:",
    mean(battery_index$Battery_Innovation_log_norm, na.rm = TRUE), "\n")
cat("Mediana BI normalized:",
    median(battery_index$Battery_Innovation_log_norm, na.rm = TRUE), "\n")

cat("\nTOP 15 dopo normalizzazione:\n")
top15_normalized <- battery_index %>%
  arrange(
    desc(Battery_Innovation_log_norm),
    Model
  ) %>%
  mutate(rank = row_number()) %>%
  slice_head(n = 15) %>%
  transmute(
    rank,
    Model,
    `BI raw` = Battery_Innovation_Index,
    `BI normalized` = Battery_Innovation_log_norm
  )

print(top15_normalized, row.names = FALSE)

# ------------------------------------------------------------
# G. SENSITIVITY ANALYSIS
# ------------------------------------------------------------

cat("\n-----------------------------\n")
cat("G. SENSITIVITY ANALYSIS\n")
cat("-----------------------------\n")

sensitivity_output <- tibble(
  Specification = c("70/30", "75/25", "80/20", "85/15"),
  Mean = c(
    sensitivity_summary$BI_70_30_mean,
    sensitivity_summary$BI_75_25_mean,
    sensitivity_summary$BI_80_20_mean,
    sensitivity_summary$BI_85_15_mean
  ),
  Median = c(
    sensitivity_summary$BI_70_30_median,
    sensitivity_summary$BI_75_25_median,
    sensitivity_summary$BI_80_20_median,
    sensitivity_summary$BI_85_15_median
  ),
  SD = c(
    sensitivity_summary$BI_70_30_sd,
    sensitivity_summary$BI_75_25_sd,
    sensitivity_summary$BI_80_20_sd,
    sensitivity_summary$BI_85_15_sd
  ),
  Minimum = c(
    sensitivity_summary$BI_70_30_min,
    sensitivity_summary$BI_75_25_min,
    sensitivity_summary$BI_80_20_min,
    sensitivity_summary$BI_85_15_min
  ),
  Maximum = c(
    sensitivity_summary$BI_70_30_max,
    sensitivity_summary$BI_75_25_max,
    sensitivity_summary$BI_80_20_max,
    sensitivity_summary$BI_85_15_max
  )
)

print(sensitivity_output, row.names = FALSE)

cat("\nStabilità dei ranking rispetto alla baseline 80/20:\n")
print(rank_stability, row.names = FALSE)

# ------------------------------------------------------------
# H. TOP RANKING SENSITIVITY
# ------------------------------------------------------------

cat("\n-----------------------------\n")
cat("H. TOP RANKING SENSITIVITY\n")
cat("-----------------------------\n")

top10_sensitivity <- function(data, value_col) {
  data %>%
    arrange(desc(.data[[value_col]]), Model) %>%
    mutate(rank = row_number()) %>%
    slice_head(n = 10) %>%
    transmute(
      rank,
      Model,
      `BI` = .data[[value_col]]
    )
}

cat("\nTOP 10 - 70/30:\n")
print(top10_sensitivity(battery_sensitivity, "BI_70_30"), row.names = FALSE)

cat("\nTOP 10 - 75/25:\n")
print(top10_sensitivity(battery_sensitivity, "BI_75_25"), row.names = FALSE)

cat("\nTOP 10 - 80/20:\n")
print(top10_sensitivity(battery_sensitivity, "BI_80_20"), row.names = FALSE)

cat("\nTOP 10 - 85/15:\n")
print(top10_sensitivity(battery_sensitivity, "BI_85_15"), row.names = FALSE)

cat("\n============================================================\n")
cat("FINE OUTPUT DA RIPORTARE NELLA NOTA METODOLOGICA\n")
cat("============================================================\n")

# ==============================================================================
# SOURCES AND AI STATEMENT
# ==============================================================================
#
# Sources:
# The code was developed by the author and based on the methodological
# framework of the thesis, R package documentation, and publicly available
# resources where applicable.
#
# AI statement:
# Generative AI tools were used to support code development, debugging,
# and refinement. The author reviewed, adapted, and validated the code
# and is responsible for the final implementation and results.
#
# ==============================================================================