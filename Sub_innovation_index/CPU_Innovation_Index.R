library(tidyverse)


# ============================================================
# 1. CARICAMENTO E PREPARAZIONE DEI DATI CPU
# ============================================================

phones <- readRDS("dataset definitivo.rds")


# Conversione dei clock in formato numerico.
# I valori mancanti vengono trattati come 0 per le variabili
# relative al clock dei core.

phones <- phones |>
  dplyr::mutate(
    prime_core_clock = na_if(prime_core_clock, "NA_real_") |>
      as.numeric() |>
      coalesce(0),
    
    efficiency_core_clock = na_if(efficiency_core_clock, "NA_real_") |>
      as.numeric() |>
      coalesce(0),
    
    prime_core_clock = as.numeric(prime_core_clock),
    efficiency_core_clock = as.numeric(efficiency_core_clock)
  )


# ============================================================
# 2. CONTROLLI PRELIMINARI DELLE VARIABILI CPU
# ============================================================

cpu_vars <- names(phones)[grepl(
  "soc|cpu|core|cache|fabric|foundry",
  names(phones),
  ignore.case = TRUE
)]

data.frame(
  variable = cpu_vars,
  class = sapply(phones[cpu_vars], class),
  missing = sapply(
    phones[cpu_vars],
    function(x) sum(is.na(x))
  ),
  unique = sapply(
    phones[cpu_vars],
    function(x) length(unique(x[!is.na(x)]))
  )
)


# Variabilità delle variabili CPU nei due anni

phones |>
  dplyr::group_by(Released.Year) |>
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(cpu_vars),
      ~ sum(!is.na(.x))
    )
  ) |>
  clipr::write_clip()


# ============================================================
# 3. DATASET DI CONTROLLO DELLE VARIABILI CPU
# ============================================================

cpu_check <- phones |>
  dplyr::select(
    Released.Year,
    soc_name,
    soc_year,
    fabrication_nm,
    cpu_cores_count,
    cpu_configuration,
    prime_core_model,
    prime_core_number,
    performance_core_model,
    performance_core_number,
    efficiency_core_model,
    efficiency_core_number,
    cache_l2_kb,
    cache_l3_kb,
    cpu_architecture_generation_version
  )


cpu_check |>
  dplyr::group_by(Released.Year) |>
  dplyr::summarise(
    dplyr::across(
      c(
        fabrication_nm,
        cpu_cores_count,
        prime_core_number,
        performance_core_number,
        efficiency_core_number,
        cache_l2_kb,
        cache_l3_kb,
        cpu_architecture_generation_version
      ),
      ~ paste(sort(unique(.x)), collapse = ", ")
    )
  ) |>
  clipr::write_clip()


# ============================================================
# 4. STATISTICHE DELLE VARIABILI NUMERICHE CPU
# ============================================================

cpu_numeric <- phones |>
  dplyr::select(
    Released.Year,
    fabrication_nm,
    cpu_cores_count,
    prime_core_number,
    prime_core_clock,
    performance_core_number,
    performance_core_clock,
    efficiency_core_number,
    efficiency_core_clock,
    cache_l2_kb,
    cache_l3_kb,
    cpu_architecture_generation_version
  )


cpu_numeric |>
  dplyr::group_by(Released.Year) |>
  dplyr::summarise(
    dplyr::across(
      everything(),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE),
        min = ~ min(.x, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE)
      )
    )
  ) |>
  clipr::write_clip()


# Correlazione tra variabili numeriche CPU

cor(
  cpu_numeric |>
    dplyr::select(-Released.Year),
  use = "pairwise.complete.obs"
)


# ============================================================
# 5. DISTRIBUZIONE DEI CORE PER ANNO
# ============================================================

core_by_year <- phones |>
  dplyr::select(
    Released.Year,
    prime_core_model,
    performance_core_model,
    efficiency_core_model
  ) |>
  tidyr::pivot_longer(
    cols = -Released.Year,
    names_to = "core_type",
    values_to = "core_model"
  ) |>
  dplyr::filter(
    !is.na(core_model),
    core_model != "",
    core_model != "NA_character"
  ) |>
  dplyr::count(
    core_type,
    core_model,
    Released.Year
  ) |>
  tidyr::pivot_wider(
    names_from = Released.Year,
    values_from = n,
    values_fill = 0
  ) |>
  dplyr::arrange(
    core_type,
    desc(`2024`),
    desc(`2023`)
  )


# ============================================================
# 6. GENERATIONAL INNOVATION SCORE DELLA CPU
# ============================================================

GI_reference <- tibble::tribble(
  ~comparison, ~type, ~new_performance, ~new_efficiency,
  
  "A510_A520", "efficiency", 8, 22,
  "A78_A710", "performance", NA, 30,
  "A710_A715", "performance", 5, 20,
  "A715_A720", "performance", 4.5, 20,
  "X2_X3", "prime", 25, NA,
  "X3_X4", "prime", 15, 40,
  "X4_X925", "prime", 15, NA,
  "Oryon_G1_G2", "oryon", 45, 44
)


GI_reference_benchmark <- tibble::tribble(
  ~comparison, ~type, ~old_single, ~new_single, ~old_multi, ~new_multi,
  
  "TaiShan_V120_TaiShan_V121", "TaiShan", 1324, 1421, 4116, 4323,
  "A17Pro_A18Pro", "prime", 2952, 3409, 7235, 8492,
  "A16_A18", "performance", 2641, 3409, 6989, 8492
)


# Calcolo GI documentale

GI_reference <- GI_reference |>
  dplyr::mutate(
    GI = dplyr::case_when(
      !is.na(new_performance) & !is.na(new_efficiency) ~
        (new_performance + new_efficiency) / 2,
      
      !is.na(new_performance) & is.na(new_efficiency) ~
        new_performance,
      
      is.na(new_performance) & !is.na(new_efficiency) ~
        new_efficiency,
      
      TRUE ~ NA_real_
    )
  )


# Calcolo GI benchmark

GI_reference_benchmark <- GI_reference_benchmark |>
  dplyr::mutate(
    single_change =
      (new_single - old_single) / old_single * 100,
    
    multi_change =
      (new_multi - old_multi) / old_multi * 100,
    
    GI = dplyr::case_when(
      !is.na(single_change) & !is.na(multi_change) ~
        (single_change + multi_change) / 2,
      
      !is.na(single_change) ~
        single_change,
      
      !is.na(multi_change) ~
        multi_change,
      
      TRUE ~ NA_real_
    )
  )


GI_all <- dplyr::bind_rows(
  GI_reference |>
    dplyr::select(comparison, GI),
  
  GI_reference_benchmark |>
    dplyr::select(comparison, GI)
)


# ============================================================
# 7. MAPPING DEL GI AI MODELLI DI CORE
# ============================================================

core_GI_mapping <- tibble::tribble(
  ~core_model, ~core_type, ~comparison,
  
  "Cortex-A510", "efficiency", NA_character_,
  "Cortex-A520", "efficiency", "A510_A520",
  
  "Cortex-A73", "performance", NA_character_,
  "Cortex-A75", "performance", NA_character_,
  "Cortex-A76", "performance", NA_character_,
  "Cortex-A78", "performance", NA_character_,
  "Cortex-A710", "performance", "A78_A710",
  "Cortex-A715", "performance", "A710_A715",
  "Cortex-A720", "performance", "A715_A720",
  
  "Oryon Phoenix M", "performance", "Oryon_G1_G2",
  
  "TaiShan V120", "performance", NA_character_,
  "TaiShan v120_L", "performance", NA_character_,
  "TaiShan V121", "performance", "TaiShan_V120_TaiShan_V121",
  
  "Cortex-X1", "prime", NA_character_,
  "Cortex-X2", "prime", NA_character_,
  "Cortex-X3", "prime", "X2_X3",
  "Cortex-X4", "prime", "X3_X4",
  "Cortex-X925", "prime", "X4_X925",
  
  "Oryon Phoenix L", "prime", "Oryon_G1_G2",
  
  "TaiShan v120_H", "prime", NA_character_
) |>
  dplyr::left_join(
    GI_all,
    by = "comparison"
  )


# ============================================================
# 8. MAPPING DEL GI APPLE
# ============================================================

apple_GI_mapping <- tibble::tribble(
  ~soc_name, ~core_type, ~comparison,
  
  "Apple A16 Bionic T8120", "performance", "A16_A18",
  "Apple A17 Pro T8130", "performance", "A17Pro_A18Pro",
  "Apple A18", "performance", "A16_A18",
  "Apple A18 Pro", "performance", "A17Pro_A18Pro"
) |>
  dplyr::left_join(
    GI_reference_benchmark |>
      dplyr::select(comparison, GI),
    by = "comparison"
  ) |>
  dplyr::mutate(
    GI = dplyr::if_else(
      soc_name %in% c(
        "Apple A16 Bionic T8120",
        "Apple A17 Pro T8130"
      ),
      0,
      GI
    )
  )


# ============================================================
# 9. GI DEFINITIVO DEI CORE
# ============================================================

core_GI <- tibble::tribble(
  ~core_model, ~GI,
  
  "Cortex-A510", 0,
  "Cortex-A520", 15,
  
  "Cortex-A710", 30,
  "Cortex-A715", 12.5,
  "Cortex-A720", 12.25,
  
  "Cortex-X2", 0,
  "Cortex-X3", 25,
  "Cortex-X4", 27.5,
  "Cortex-X925", 15,
  
  "Oryon Phoenix L", 44.5,
  "Oryon Phoenix M", 44.5,
  
  "TaiShan V120", 0,
  "TaiShan v120_L", 0,
  "TaiShan v120_H", 0,
  "TaiShan V121", 6.17771925343292,
  
  "Cortex-A78", 0,
  "Cortex-A76", 0,
  "Cortex-A75", 0,
  "Cortex-A73", 0,
  "Cortex-A53", 0,
  "Cortex-A55", 0
)


# ============================================================
# 10. APPLICAZIONE DEL GI AI TELEFONI
# ============================================================

phones_GI <- phones |>
  dplyr::left_join(
    core_GI |>
      dplyr::rename(
        prime_core_GI = GI
      ),
    by = c("prime_core_model" = "core_model")
  ) |>
  dplyr::left_join(
    core_GI |>
      dplyr::rename(
        performance_core_GI = GI
      ),
    by = c("performance_core_model" = "core_model")
  ) |>
  dplyr::left_join(
    core_GI |>
      dplyr::rename(
        efficiency_core_GI = GI
      ),
    by = c("efficiency_core_model" = "core_model")
  )


# Applicazione del GI benchmark Apple a livello CPU

phones_GI <- phones_GI |>
  dplyr::left_join(
    apple_GI_mapping |>
      dplyr::select(
        soc_name,
        apple_GI = GI
      ),
    by = "soc_name"
  ) |>
  dplyr::mutate(
    performance_core_GI = dplyr::if_else(
      !is.na(apple_GI),
      apple_GI,
      performance_core_GI
    )
  ) |>
  dplyr::select(-apple_GI)


# Caso ibrido Cortex-A715/Cortex-A710

phones_GI <- phones_GI |>
  dplyr::mutate(
    performance_core_GI = dplyr::if_else(
      performance_core_model == "Cortex-A715/Cortex-A710",
      (2 * 12.5 + 2 * 30) / 4,
      performance_core_GI
    )
  )


# ============================================================
# 11. CONFIGURAZIONE DEI CORE
# ============================================================

phones_GI <- phones_GI |>
  dplyr::mutate(
    prime_share =
      prime_core_number / cpu_cores_count,
    
    performance_share =
      performance_core_number / cpu_cores_count,
    
    efficiency_share =
      efficiency_core_number / cpu_cores_count
  )


configuration_reference <- phones_GI |>
  dplyr::filter(Released.Year == 2023) |>
  dplyr::count(
    prime_core_number,
    performance_core_number,
    efficiency_core_number,
    cpu_cores_count,
    name = "n_2023"
  ) |>
  dplyr::mutate(
    market_share_2023 = n_2023 / sum(n_2023)
  )


configuration_2024 <- phones_GI |>
  dplyr::filter(Released.Year == 2024) |>
  dplyr::count(
    prime_core_number,
    performance_core_number,
    efficiency_core_number,
    cpu_cores_count,
    name = "n_2024"
  ) |>
  dplyr::left_join(
    configuration_reference |>
      dplyr::select(
        prime_core_number,
        performance_core_number,
        efficiency_core_number,
        cpu_cores_count,
        n_2023,
        market_share_2023
      ),
    by = c(
      "prime_core_number",
      "performance_core_number",
      "efficiency_core_number",
      "cpu_cores_count"
    )
  ) |>
  dplyr::mutate(
    configuration_novelty = dplyr::if_else(
      is.na(n_2023),
      1,
      0
    )
  ) |>
  dplyr::arrange(dplyr::desc(n_2024))


# ============================================================
# 12. GI DEFINITIVO DELLA CPU
# ============================================================

phones_GI <- phones_GI |>
  dplyr::mutate(
    prime_contribution = dplyr::if_else(
      prime_core_number == 0,
      0,
      prime_core_number * prime_core_GI
    ),
    
    performance_contribution = dplyr::if_else(
      performance_core_number == 0,
      0,
      performance_core_number * performance_core_GI
    ),
    
    efficiency_contribution = dplyr::if_else(
      efficiency_core_number == 0,
      0,
      efficiency_core_number * efficiency_core_GI
    )
  )


phones_GI <- phones_GI |>
  dplyr::mutate(
    CPU_GI = dplyr::case_when(
      
      soc_name == "Apple A16 Bionic T8120" ~ 0,
      soc_name == "Apple A17 Pro T8130" ~ 0,
      soc_name == "Apple A18" ~ 25.2925582360207,
      soc_name == "Apple A18 Pro" ~ 16.4274533985837,
      
      TRUE ~ (
        prime_contribution +
          performance_contribution +
          efficiency_contribution
      ) / cpu_cores_count
    )
  )

# ============================================================
# 13. CLOCK INNOVATION – PHONE-LEVEL FRONTIER
# ============================================================
# 2023 frontier = maximum observed clock for each exact core model.
# For genuinely new 2024 cores, the comparison baseline is the
# 2023 maximum clock within the same core type.
#
# IMPORTANT:
# The clock score is calculated at PHONE-CORE level. A phone using
# a given core model does NOT automatically receive the maximum
# score observed for that core model in 2024.

core_frontier_2023 <- bind_rows(

  phones %>%
    filter(
      Released.Year == 2023,
      !is.na(prime_core_model),
      prime_core_number > 0,
      prime_core_clock > 0
    ) %>%
    group_by(core_model = prime_core_model) %>%
    summarise(
      core_type = "Prime",
      max_clock_2023 = max(prime_core_clock, na.rm = TRUE),
      n_phones_2023 = n(),
      .groups = "drop"
    ),

  phones %>%
    filter(
      Released.Year == 2023,
      !is.na(performance_core_model),
      performance_core_number > 0,
      performance_core_clock > 0
    ) %>%
    group_by(core_model = performance_core_model) %>%
    summarise(
      core_type = "Performance",
      max_clock_2023 = max(performance_core_clock, na.rm = TRUE),
      n_phones_2023 = n(),
      .groups = "drop"
    ),

  phones %>%
    filter(
      Released.Year == 2023,
      !is.na(efficiency_core_model),
      efficiency_core_number > 0,
      efficiency_core_clock > 0
    ) %>%
    group_by(core_model = efficiency_core_model) %>%
    summarise(
      core_type = "Efficiency",
      max_clock_2023 = max(efficiency_core_clock, na.rm = TRUE),
      n_phones_2023 = n(),
      .groups = "drop"
    )
) %>%
  arrange(core_type, core_model)


# 2023 type-level frontiers used only when a 2024 core model
# has no exact 2023 counterpart.

type_frontier_2023 <- tibble::tribble(
  ~core_type,    ~frontier_clock_2023,
  "Prime",       3.40,
  "Performance", 3.78,
  "Efficiency",  2.30
)

new_prime_cores <- c(
  "Cortex-X925",
  "TaiShan V121",
  "Oryon Phoenix L"
)

new_performance_cores <- c(
  "TaiShan V121",
  "Oryon Phoenix M"
)


# Phone-core level data

clock_phone_core <- phones %>%
  filter(Released.Year == 2024) %>%
  select(
    Brand,
    Model,
    model_std,
    cpu_cores_count,
    prime_core_model,
    prime_core_number,
    prime_core_clock,
    performance_core_model,
    performance_core_number,
    performance_core_clock,
    efficiency_core_model,
    efficiency_core_number,
    efficiency_core_clock
  ) %>%
  pivot_longer(
    cols = c(
      prime_core_model,
      performance_core_model,
      efficiency_core_model
    ),
    names_to = "core_type_raw",
    values_to = "core_model"
  ) %>%
  mutate(
    core_type = case_when(
      core_type_raw == "prime_core_model" ~ "Prime",
      core_type_raw == "performance_core_model" ~ "Performance",
      core_type_raw == "efficiency_core_model" ~ "Efficiency",
      TRUE ~ NA_character_
    ),

    core_number = case_when(
      core_type == "Prime" ~ prime_core_number,
      core_type == "Performance" ~ performance_core_number,
      core_type == "Efficiency" ~ efficiency_core_number,
      TRUE ~ NA_real_
    ),

    clock_2024 = case_when(
      core_type == "Prime" ~ prime_core_clock,
      core_type == "Performance" ~ performance_core_clock,
      core_type == "Efficiency" ~ efficiency_core_clock,
      TRUE ~ NA_real_
    )
  ) %>%
  select(
    Brand,
    Model,
    model_std,
    cpu_cores_count,
    core_type,
    core_model,
    core_number,
    clock_2024
  ) %>%
  filter(
    !is.na(core_model),
    core_model != "",
    !is.na(core_number),
    core_number > 0,
    !is.na(clock_2024),
    clock_2024 > 0
  ) %>%
  left_join(
    core_frontier_2023 %>%
      select(
        core_type,
        core_model,
        frontier_exact_2023 = max_clock_2023
      ),
    by = c("core_type", "core_model")
  ) %>%
  left_join(
    type_frontier_2023,
    by = "core_type"
  ) %>%
  mutate(

    # Exact 2023 frontier when the core already existed.
    # Type-level frontier only for the verified new 2024 cores.
    comparison_frontier = case_when(
      !is.na(frontier_exact_2023) ~ frontier_exact_2023,

      core_type == "Prime" &
        core_model %in% new_prime_cores ~
        frontier_clock_2023,

      core_type == "Performance" &
        core_model %in% new_performance_cores ~
        frontier_clock_2023,

      TRUE ~ NA_real_
    ),

    raw_clock_extension = case_when(
      is.na(comparison_frontier) ~ NA_real_,

      clock_2024 > comparison_frontier ~
        (clock_2024 - comparison_frontier) /
        comparison_frontier,

      TRUE ~ 0
    )
  )


# Control: any 2024 core without a valid comparison frontier

clock_unresolved_cores <- clock_phone_core %>%
  filter(is.na(comparison_frontier)) %>%
  distinct(core_type, core_model) %>%
  arrange(core_type, core_model)

clock_unresolved_cores


# Maximum positive raw extension by core type.
# Normalisation is therefore relative to the largest observed
# 2024 extension within each core type.

clock_max_by_type <- clock_phone_core %>%
  filter(!is.na(raw_clock_extension)) %>%
  group_by(core_type) %>%
  summarise(
    max_raw_extension = max(raw_clock_extension, na.rm = TRUE),
    .groups = "drop"
  )


clock_phone_core <- clock_phone_core %>%
  left_join(
    clock_max_by_type,
    by = "core_type"
  ) %>%
  mutate(
    clock_score = case_when(

      is.na(raw_clock_extension) ~ NA_real_,

      raw_clock_extension > 0 &
        max_raw_extension > 0 ~
        100 * raw_clock_extension /
        max_raw_extension,

      TRUE ~ 0
    ),

    weighted_clock_score =
      core_number * clock_score,

    weighted_raw_extension =
      core_number * raw_clock_extension
  )


# Final phone-level Clock Innovation.
# Core counts weight the contribution of each core type.
# If a core has no valid comparison, it is not used as a penalty.

phones_clock_frontier <- clock_phone_core %>%
  group_by(
    Brand,
    Model,
    model_std,
    cpu_cores_count
  ) %>%
  summarise(

    available_cores =
      sum(
        core_number[!is.na(raw_clock_extension)],
        na.rm = TRUE
      ),

    raw_clock_innovation =
      if_else(
        available_cores > 0,
        sum(
          weighted_raw_extension,
          na.rm = TRUE
        ) / available_cores,
        NA_real_
      ),

    clock_innovation =
      if_else(
        available_cores > 0,
        sum(
          weighted_clock_score,
          na.rm = TRUE
        ) / available_cores,
        NA_real_
      ),

    .groups = "drop"
  )


# ============================================================
# 14. GI – FRONTIER 2023 -> 2024
# ============================================================

core_GI_frontier <- tibble::tribble(
  ~core_model, ~GI,

  "Cortex-A510", 0,
  "Cortex-A520", 0,

  "Cortex-A710", 0,
  "Cortex-A715", 0,
  "Cortex-A720", 0,

  "Cortex-X2", 0,
  "Cortex-X3", 0,
  "Cortex-X4", 0,
  "Cortex-X925", 15,

  "Oryon Phoenix L", 44.5,
  "Oryon Phoenix M", 44.5,

  "TaiShan V120", 0,
  "TaiShan v120_L", 0,
  "TaiShan v120_H", 0,
  "TaiShan V121", 6.17771925343292,

  "Cortex-A78", 0,
  "Cortex-A76", 0,
  "Cortex-A75", 0,
  "Cortex-A73", 0,
  "Cortex-A53", 0,
  "Cortex-A55", 0
)


phones_GI_frontier <- phones |>
  left_join(
    core_GI_frontier |>
      rename(prime_core_GI = GI),
    by = c("prime_core_model" = "core_model")
  ) |>
  left_join(
    core_GI_frontier |>
      rename(performance_core_GI = GI),
    by = c("performance_core_model" = "core_model")
  ) |>
  left_join(
    core_GI_frontier |>
      rename(efficiency_core_GI = GI),
    by = c("efficiency_core_model" = "core_model")
  )


# Apple benchmark-based GI

phones_GI_frontier <- phones_GI_frontier |>
  mutate(
    performance_core_GI = case_when(

      soc_name == "Apple A16 Bionic T8120" ~ 0,
      soc_name == "Apple A17 Pro T8130" ~ 0,
      soc_name == "Apple A18" ~ 25.2925582360207,
      soc_name == "Apple A18 Pro" ~ 16.4274533985837,

      TRUE ~ performance_core_GI
    ),

    performance_core_GI = if_else(
      performance_core_model == "Cortex-A715/Cortex-A710",
      0,
      performance_core_GI
    )
  )


phones_GI_frontier <- phones_GI_frontier |>
  mutate(

    prime_contribution = if_else(
      prime_core_number == 0,
      0,
      prime_core_number * prime_core_GI
    ),

    performance_contribution = if_else(
      performance_core_number == 0,
      0,
      performance_core_number * performance_core_GI
    ),

    efficiency_contribution = if_else(
      efficiency_core_number == 0,
      0,
      efficiency_core_number * efficiency_core_GI
    ),

    CPU_GI_frontier =
      (
        prime_contribution +
        performance_contribution +
        efficiency_contribution
      ) / cpu_cores_count
  )


# Apple CPU-level GI overrides

phones_GI_frontier <- phones_GI_frontier |>
  mutate(
    CPU_GI_frontier = case_when(

      soc_name == "Apple A16 Bionic T8120" ~ 0,
      soc_name == "Apple A17 Pro T8130" ~ 0,
      soc_name == "Apple A18" ~ 25.2925582360207,
      soc_name == "Apple A18 Pro" ~ 16.4274533985837,

      TRUE ~ CPU_GI_frontier
    )
  )


# ============================================================
# 15. CACHE INNOVATION – STESSA LOGICA GIÀ UTILIZZATA
# ============================================================

cache_2023 <- phones |>
  filter(Released.Year == 2023) |>
  select(
    soc_name,
    cache_l2_kb,
    cache_l3_kb
  ) |>
  distinct() |>
  filter(
    !is.na(cache_l2_kb),
    !is.na(cache_l3_kb)
  )


cache_2024 <- phones |>
  filter(Released.Year == 2024) |>
  select(
    soc_name,
    cache_l2_kb,
    cache_l3_kb
  ) |>
  distinct() |>
  filter(
    !is.na(cache_l2_kb),
    !is.na(cache_l3_kb)
  )


cache_all <- bind_rows(
  cache_2023,
  cache_2024
)


is_pareto <- function(df) {

  sapply(seq_len(nrow(df)), function(i) {

    !any(
      df$cache_l2_kb >= df$cache_l2_kb[i] &
      df$cache_l3_kb >= df$cache_l3_kb[i] &
      (
        df$cache_l2_kb > df$cache_l2_kb[i] |
        df$cache_l3_kb > df$cache_l3_kb[i]
      )
    )
  })
}


cache_pareto_2023 <- cache_2023[
  is_pareto(cache_2023),
]


cache_pareto_2024 <- cache_2024[
  is_pareto(cache_2024),
]


hv_2d <- function(df, ref_l2, ref_l3) {

  df <- df |>
    arrange(
      desc(cache_l2_kb),
      desc(cache_l3_kb)
    )

  area <- 0

  for (i in seq_len(nrow(df))) {

    x_right <- df$cache_l2_kb[i]

    if (i == nrow(df)) {
      x_left <- ref_l2
    } else {
      x_left <- df$cache_l2_kb[i + 1]
    }

    y <- df$cache_l3_kb[i]

    area <- area +
      (x_right - x_left) *
      (y - ref_l3)
  }

  area
}


ref_l2 <- min(
  cache_all$cache_l2_kb,
  na.rm = TRUE
)

ref_l3 <- min(
  cache_all$cache_l3_kb,
  na.rm = TRUE
)


HV_cache_2023 <- hv_2d(
  cache_pareto_2023,
  ref_l2,
  ref_l3
)


HV_cache_2024 <- hv_2d(
  cache_pareto_2024,
  ref_l2,
  ref_l3
)


HE_cache <-
  (HV_cache_2024 - HV_cache_2023) /
  HV_cache_2023


F_cache <- 1 + HE_cache


c(
  ref_l2 = ref_l2,
  ref_l3 = ref_l3,
  HV_2023 = HV_cache_2023,
  HV_2024 = HV_cache_2024,
  HE_cache = HE_cache,
  F_cache = F_cache
)


# Cache innovation remains the established binary assignment:
# the 2024 SoCs identified by the previous analysis receive the
# observed frontier-extension value.

cpu_frontier_index <- phones_GI_frontier |>
  filter(Released.Year == 2024) |>
  select(
    Brand,
    Model,
    model_std,
    Released.Year,
    CPU_GI_frontier
  ) |>
  left_join(
    phones_clock_frontier |>
      select(
        model_std,
        clock_innovation
      ),
    by = "model_std"
  ) |>
  mutate(

    cache_innovation = if_else(
      model_std %in% c(
        "13 5g",
        "15 pro 5g",
        "15 5g",
        "rog phone 9 pro edition 5g",
        "rog phone 9 pro 5g",
        "rog phone 9 5g",
        "redmi k80 pro 5g"
      ),
      6.837607,
      0
    )
  )


# ============================================================
# 16. NORMALIZZAZIONE DEI TRE COMPONENTI
# ============================================================
# All three components are put on the same 0–100 scale before
# applying the theoretical weights.

all_cpu_index <- cpu_frontier_index |>
  mutate(

    GI_norm =
      100 * CPU_GI_frontier /
      max(
        CPU_GI_frontier,
        na.rm = TRUE
      ),

    clock_norm =
      clock_innovation,

    cache_norm =
      100 * cache_innovation /
      max(
        cache_innovation,
        na.rm = TRUE
      ),

    CPU_Innovation_Index =
      0.55 * GI_norm +
      0.30 * clock_norm +
      0.15 * cache_norm,

    CPU_Innovation_Index =
      pmin(
        100,
        pmax(
          0,
          CPU_Innovation_Index
        )
      )
  )

# Trasformazione logaritmica
all_cpu_index <- all_cpu_index |>
  mutate(
    
    # Passo 1: trasformazione logaritmica
    # CPU_Innovation_Index è già uno score composito 0-100.
    log_temp = case_when(
      is.na(CPU_Innovation_Index) ~ NA_real_,
      CPU_Innovation_Index == 0 ~ 0,
      CPU_Innovation_Index > 0 ~
        log(CPU_Innovation_Index + 1)
    ),
    
    # Passo 2: Min-Max con minimo teorico = 0
    CPU_Innovation_log_norm = case_when(
      is.na(log_temp) ~ NA_real_,
      log_temp == 0 ~ 0,
      log_temp > 0 ~
        (log_temp - 0) /
        (max(log_temp[log_temp > 0], na.rm = TRUE) - 0) * 100
    )
  )

# ============================================================
# 17. CONTROLLI FINALI
# ============================================================

all_cpu_index |>
  summarise(

    N = n(),

    min_CPU =
      min(
        CPU_Innovation_Index,
        na.rm = TRUE
      ),

    max_CPU =
      max(
        CPU_Innovation_Index,
        na.rm = TRUE
      ),

    mean_CPU =
      mean(
        CPU_Innovation_Index,
        na.rm = TRUE
      ),

    median_CPU =
      median(
        CPU_Innovation_Index,
        na.rm = TRUE
      ),

    zeros =
      sum(
        CPU_Innovation_Index == 0,
        na.rm = TRUE
      )
  )


all_cpu_index |>
  arrange(
    desc(CPU_Innovation_Index)
  ) |>
  select(
    Brand,
    Model,
    CPU_GI_frontier,
    GI_norm,
    clock_innovation,
    cache_innovation,
    cache_norm,
    CPU_Innovation_Index
  ) |>
  print(n = 30)


# ============================================================
# 18. ESPORTAZIONE FINALE
# ============================================================

CPU_Innovation_Index_final <- all_cpu_index |>
  select(
    Brand, Model, model_std, Released.Year, CPU_Innovation_log_norm)

saveRDS(
  CPU_Innovation_Index_final,
  "CPU Innovation Index.rds"
)


# ============================================================
# CONTROLLO FINALE – CLASSIFICHE CPU
# ============================================================

# 1. TOP GI
all_cpu_index |>
  filter(GI_norm > 0) |>
  arrange(desc(GI_norm)) |>
  select(Brand, Model, CPU_GI_frontier, GI_norm) |>
  print(n = 30) 

# 2. TOP CLOCK
all_cpu_index |>
  filter(clock_innovation > 0) |>
  arrange(desc(clock_innovation)) |>
  select(Brand, Model, clock_innovation) |>
  print(n = 30)

# 3. TOP CACHE
all_cpu_index |>
  filter(cache_norm > 0) |>
  arrange(desc(cache_norm)) |>
  select(Brand, Model, cache_innovation, cache_norm) |>
  print(n = 30)

# 4. TOP CPU FINALE
all_cpu_index |>
  arrange(desc(CPU_Innovation_Index)) |>
  select(
    Brand, Model,
    GI_norm,
    clock_innovation,
    cache_norm,
    CPU_Innovation_Index
  ) |>
  print(n = 30)

# 5. DISTRIBUZIONE
all_cpu_index |>
  summarise(
    N = n(),
    GI_positive = sum(GI_norm > 0, na.rm = TRUE),
    Clock_positive = sum(clock_innovation > 0, na.rm = TRUE),
    Cache_positive = sum(cache_norm > 0, na.rm = TRUE),
    CPU_positive = sum(CPU_Innovation_Index > 0, na.rm = TRUE),
    
    GI_mean = mean(GI_norm, na.rm = TRUE),
    GI_median = median(GI_norm, na.rm = TRUE),
    
    Clock_mean = mean(clock_innovation, na.rm = TRUE),
    Clock_median = median(clock_innovation, na.rm = TRUE),
    
    Cache_mean = mean(cache_norm, na.rm = TRUE),
    Cache_median = median(cache_norm, na.rm = TRUE),
    
    CPU_mean = mean(CPU_Innovation_Index, na.rm = TRUE),
    CPU_median = median(CPU_Innovation_Index, na.rm = TRUE)
  )

# ==============================================================================
# SOURCES AND AI STATEMENT
# ==============================================================================
#
# Sources:
# The code was developed by the author based on the methodological
# framework of the thesis and adapted from R documentation and
# publicly available resources where applicable.
#
# AI statement:
# Generative AI tools were used to support the development, debugging,
# and refinement of parts of the R code. The author reviewed, adapted,
# and validated the code and is responsible for the final implementation
# and results.
#
# ==============================================================================