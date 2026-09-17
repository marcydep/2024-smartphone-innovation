library(tidyverse)

phones <- readRDS("dataset definitivo.rds")


# ============================================================
# 1. ANAGRAFICA TECNOLOGICA GPU
# ============================================================

gpu_technology <- tibble::tribble(
  ~gpu_name, ~gpu_family, ~gpu_tier, ~gpu_architecture,
  ~gpu_generation, ~gpu_predecessor, ~gpu_ray_tracing,
  ~gpu_evolution_type,
  
  # ----------------------------------------------------------
  # Qualcomm Adreno
  # ----------------------------------------------------------
  
  "Adreno 610", "Adreno", "low", "Adreno 600",
  "Gen 1", "Adreno 506", FALSE, "Architecture_Change",
  
  "Adreno 611", "Adreno", "low", "Adreno 600",
  "Gen 2", "Adreno 610", FALSE, "Generation_Change",
  
  "Adreno 613", "Adreno", "low", "Adreno 600",
  "Gen 2", "Adreno 612", FALSE, "Generation_Change",
  
  "Adreno 619", "Adreno", "mid", "Adreno 600",
  "Gen 3", "Adreno 618", FALSE, "Generation_Change",
  
  "Adreno 642L", "Adreno", "sub-premium", "Adreno 600",
  "Gen 4", "Adreno 640", FALSE, "Generation_Change",
  
  "Adreno 644", "Adreno", "sub-premium", "Adreno 600",
  "Gen 4", "Adreno 642L", FALSE, "Generation_Change",
  
  
  "Adreno 710", "Adreno", "mid", "Adreno 700",
  "Gen 1", "Adreno 619", FALSE, "Architecture_Change",
  
  "Adreno 720", "Adreno", "mid", "Adreno 700",
  "Gen 2", "Adreno 710", FALSE, "Generation_Change",
  
  "Adreno 725", "Adreno", "high", "Adreno 700",
  "Gen 2", "Adreno 660", FALSE, "Generation_Change",
  
  "Adreno 730", "Adreno", "premium", "Adreno 700",
  "Gen 2", "Adreno 660", FALSE, "Generation_Change",
  
  "Adreno 732", "Adreno", "high", "Adreno 700",
  "Gen 3", "Adreno 725", TRUE, "Generation_Change",
  
  "Adreno 735", "Adreno", "high", "Adreno 700",
  "Gen 3", "Adreno 732", TRUE, "Configuration_Change",
  
  "Adreno 740", "Adreno", "premium", "Adreno 700",
  "Gen 3", "Adreno 730", TRUE, "Generation_Change",
  
  "Adreno 750", "Adreno", "premium", "Adreno 700",
  "Gen 4", "Adreno 740", TRUE, "Generation_Change",
  
  
  "Adreno 810", "Adreno", "sub-premium", "Adreno 800",
  "Gen 1", "Adreno 720", TRUE, "Architecture_Change",
  
  "Adreno 830", "Adreno", "premium", "Adreno 800",
  "Gen 1", "Adreno 750", TRUE, "Architecture_Change",
  
  
  # ----------------------------------------------------------
  # ARM Mali
  # ----------------------------------------------------------
  
  "Mali-T820", "Mali", "low", "Midgard",
  "4th Gen Midgard", "Mali-T720", FALSE, "Generation_Change",
  
  "Mali-G51MP4", "Mali", "low", "Bifrost",
  "Gen 1 Bifrost", "Mali-T830", FALSE, "Architecture_Change",
  
  "Mali-G52", "Mali", "mainstream", "Bifrost",
  "Gen 2 Bifrost", "Mali-G51", FALSE, "Generation_Change",
  
  "Mali-G52MP2", "Mali", "mainstream", "Bifrost",
  "Gen 2 Bifrost", "Mali-G51", FALSE, "Configuration",
  
  "Mali-G57MP2", "Mali", "mainstream", "Valhall",
  "Gen 1 Valhall", "Mali-G52", FALSE, "Architecture_Change",
  
  "Mali-G57MP4", "Mali", "mainstream", "Valhall",
  "Gen 1 Valhall", "Mali-G52", FALSE, "Configuration",
  
  "Mali-G68MP2", "Mali", "sub-premium", "Valhall",
  "Gen 2 Valhall", "Mali-G57", FALSE, "Generation_Change",
  
  "Mali-G68MP4", "Mali", "sub-premium", "Valhall",
  "Gen 2 Valhall", "Mali-G57", FALSE, "Configuration",
  
  "Mali-G68MP5", "Mali", "sub-premium", "Valhall",
  "Gen 2 Valhall", "Mali-G57", FALSE, "Configuration",
  
  "Mali-G77MP9", "Mali", "premium", "Valhall",
  "Gen 1 Valhall", "Mali-G76", FALSE, "Architecture_Change",
  
  "Mali-G610MP3", "Mali", "sub-premium", "Valhall",
  "Gen 3 Valhall", "Mali-G68", FALSE, "Generation_Change",
  
  "Mali-G610MP4", "Mali", "sub-premium", "Valhall",
  "Gen 3 Valhall", "Mali-G68", FALSE, "Configuration",
  
  "Mali-G610MP6", "Mali", "sub-premium", "Valhall",
  "Gen 3 Valhall", "Mali-G68", FALSE, "Configuration",
  
  "Mali-G615MP2", "Mali", "sub-premium", "Valhall",
  "Gen 4 Valhall", "Mali-G610", FALSE, "Generation_Change",
  
  "Mali-G615MP6", "Mali", "sub-premium", "Valhall",
  "Gen 4 Valhall", "Mali-G610", FALSE, "Configuration",
  
  "Mali-G710MP7", "Mali", "premium", "Valhall",
  "Gen 3 Valhall", "Mali-G78", FALSE, "Generation_Change",
  
  "Mali-G710MP10", "Mali", "premium", "Valhall",
  "Gen 3 Valhall", "Mali-G78", FALSE, "Configuration",
  
  "Mali-G715MP7", "Mali", "premium", "Valhall",
  "Gen 4 Valhall", "Mali-G710", FALSE, "Generation_Change",
  
  
  # ----------------------------------------------------------
  # ARM Immortalis
  # ----------------------------------------------------------
  
  "Immortalis-G715MP11", "Immortalis", "premium", "Valhall",
  "Gen 4 Valhall", "Mali-G710", TRUE, "Generation_Change",
  
  "Immortalis-G720MP12", "Immortalis", "premium", "5th Gen",
  "Gen 1 5th Gen", "Immortalis-G715", TRUE, "Architecture_Change",
  
  "Immortalis-G925MP12", "Immortalis", "premium", "6th Gen/CSS",
  "Gen 1 6th Gen", "Immortalis-G720", TRUE, "Architecture_Change",
  
  
  # ----------------------------------------------------------
  # Apple
  # ----------------------------------------------------------
  
  "G15P MP5", "Apple Custom GPU", "premium", "Apple Custom GPU",
  "A16", NA, FALSE, "No_Predecessor",
  
  "G16P MP6", "Apple Custom GPU", "premium", "Apple Custom GPU + RT",
  "A17 Pro", "G15P MP5", TRUE, "Architecture_Change",
  
  "G17P MP5", "Apple Custom GPU", "premium", "Apple Custom GPU + RT",
  "A18", "G16P MP6", TRUE, "Generation_Change",
  
  "G17P MP6", "Apple Custom GPU", "premium", "Apple Custom GPU + RT",
  "A18 Pro", "G16P MP6", TRUE, "Generation_Change",
  
  
  # ----------------------------------------------------------
  # Samsung Xclipse
  # ----------------------------------------------------------
  
  "Xclipse 530", "Xclipse", "high", "RDNA 3",
  "Gen 1", NA, TRUE, "No_Predecessor",
  
  "Xclipse 940", "Xclipse", "premium", "RDNA 3",
  "Gen 2", "Xclipse 920", TRUE, "Generation_Change",
  
  
  # ----------------------------------------------------------
  # PowerVR / IMG
  # ----------------------------------------------------------
  
  "GE8300", "PowerVR", "low", "Rogue",
  "Series8XE", "GE7800", FALSE, "Generation_Change",
  
  "GE8322", "PowerVR", "low", "Rogue",
  "Series8XE", "GE8300", FALSE, "Configuration",
  
  "PowerVR GE8320", "PowerVR", "low", "Rogue",
  "Series8XE", "GE8300", FALSE, "Configuration",
  
  "BXM-8-256", "IMG B-Series", "mainstream", "B-Series/XM",
  "B-Series", NA, FALSE, "No_Predecessor",
  
  
  # ----------------------------------------------------------
  # HiSilicon
  # ----------------------------------------------------------
  
  "Maleoon 910", "Maleoon", "high", "Maleoon Custom",
  "Gen 1", NA, FALSE, "No_Predecessor"
)

# ============================================================
# 2. COSTRUZIONE GPU DATASET DI BASE
# ============================================================

gpu_df <- phones |>
  dplyr::select(
    Brand,
    Model,
    Released.Year,
    model_std,
    architecture_generation,
    gpu_manufacturer,
    gpu_name,
    gpu_clock,
    gpu_ray_tracing
  ) |>
  dplyr::left_join(
    gpu_technology |>
      dplyr::select(
        gpu_name,
        gpu_family,
        gpu_tier,
        gpu_architecture,
        gpu_generation,
        gpu_predecessor,
        gpu_evolution_type
      ),
    by = "gpu_name"
  ) |>
  dplyr::select(-gpu_ray_tracing)

# INTEGRAZIONE DEI CLOCK
gpu_clock_add <- c(
  "magic 5 lite 5g" = 950,
  "aquos sense 7 plus 5g" = 950,
  "aquos sense 8 5g" = 800,
  "blade l220" = 600,
  "civi 3 5g" = 950,
  "defy 2 5g" = 950,
  "enjoy 70" = 650,
  "find n2 flip 5g" = 955,
  "galaxy a15 5g" = 950,
  "galaxy a24 4g" = 950,
  "70 lite 5g" = 825,
  "x8a 5g" = 825,
  "x9a 5g magic 5 lite" = 950,
  "iphone 15 5g" = 1398,
  "iphone 15 plus 5g" = 1398,
  "iphone 15 pro 5g" = 1395,
  "iphone 15 pro max 5g" = 1395,
  "mate x 3 4g" = 900,
  "mi 13 5g" = 680,
  "mi 13 pro 5g" = 680,
  "mi 13t 5g" = 950,
  "mi 13t pro 5g" = 955,
  "moto edge 40 neo 5g" = 950,
  "moto g power 5g" = 950,
  "moto g stylus 5g" = 950,
  "moto g52j 5g" = 950,
  "moto g52j ii 5g" = 950,
  "moto g54 5g" = 950,
  "moto g54 power 5g" = 950,
  "moto g73 5g" = 950,
  "narzo 60x 5g" = 950,
  "nord 3 5g" = 955,
  "nord ce 3 5g" = 844,
  "nova y71" = 650,
  "nubia z60 ultra 5g" = 903,
  "orbic fun+ 4g" = 650,
  "p60 4g" = 900,
  "p60 art 4g" = 900,
  "p60 pro 4g" = 900,
  "pixel 7a 5g" = 848,
  "poco c40" = 600,
  "poco m6 5g" = 950,
  "poco x5 5g" = 950,
  "razr 40 5g" = 800,
  "realme 10t 5g" = 950,
  "realme 11 5g" = 950,
  "realme 11x 5g" = 950,
  "realme c67 5g" = 950,
  "realme narzo 60 5g" = 950,
  "realme v50 5g" = 950,
  "redmi 13c 5g" = 950,
  "redmi k60 pro 5g" = 680,
  "redmi k60 ultra 5g" = 955,
  "redmi k60e 5g" = 950,
  "redmi note 12 5g" = 950,
  "redmi note 12r pro 5g" = 950,
  "redmi note 12t pro 5g" = 950,
  "vivo s17 5g" = 844,
  "vivo s17e 5g" = 950,
  "vivo v27 5g" = 950,
  "vivo v27 pro 5g" = 950,
  "vivo v27e" = 950,
  "vivo x90 pro 5g" = 981,
  "vivo x90s 5g" = 955,
  "vivo y100i 5g" = 950,
  "vivo y35+ 5g" = 950,
  "vivo y36 5g" = 950,
  "vivo y36i 5g" = 950,
  "vivo y77t 5g" = 950,
  "vivo y78 5g" = 950,
  "vivo y78t 5g" = 950,
  "ace 3v 5g" = 950,
  "aquos r9 pro 5g" = 1000,
  "armor 25t" = 950,
  "armor 25t pro 5g" = 950,
  "armor 27 pro 5g" = 950,
  "armor 27t pro 5g" = 950,
  "arrows we 2" = 950,
  "basio active 2 5g" = 950,
  "civi 4 pro 5g" = 1100,
  "cmf phone 1 5g" = 950,
  "easy smartphone a" = 950,
  "enjoy 70z" = 650,
  "era 30" = 650,
  "galaxy a55 5g" = 1300,
  "galaxy f15 5g" = 950,
  "galaxy m15 5g" = 950,
  "galaxy s24 fe 5g" = 1009,
  "galaxy s24 ultra 5g" = 1000,
  "galaxy xcover7 5g" = 950,
  "galaxy z flip6 5g" = 1000,
  "galaxy z fold6 5g" = 1000,
  "mi 14t pro 5g" = 1612,
  "moto edge 50 5g" = 1000,
  "moto edge 50 neo 5g" = 950,
  "moto edge 50 ultra 5g" = 1000,
  "moto g55 5g" = 950,
  "moto g64 5g" = 950,
  "moto g64y 5g" = 950,
  "phone (2a) 5g" = 950,
  "poco c75 5g" = 800,
  "poco f6 5g" = 1100,
  "poco m6 pro 4g" = 950,
  "poco m7 pro 5g" = 950,
  "razr 50 5g" = 950,
  "razr 50 ultra 5g" = 1000,
  "razr 50d 5g" = 950,
  "razr 50s 5g" = 950,
  "razr 5g" = 950,
  "razr+ 5g" = 1000,
  "redmi 14c 4g" = 1000,
  "redmi a3 pro 4g" = 1000,
  "redmi a3x 4g" = 650,
  "redmi a4 5g" = 800,
  "redmi k70 ultra 5g" = 1612,
  "redmi note 14 5g" = 950,
  "redmi note 14 pro 5g" = 1100,
  "reno12 5g" = 950,
  "reno12 pro 5g" = 950,
  "thinkphone 25 5g" = 950,
  "vivo y28 5g" = 950,
  "w25 5g" = 1000,
  "xperia 10 vi 5g" = 800
)

gpu_clock_add_df <- tibble::tibble(
  model_std = names(gpu_clock_add),
  gpu_clock_new = as.numeric(gpu_clock_add)
)

gpu_df <- phones |>
  dplyr::select(
    Brand,
    Model,
    Released.Year,
    model_std,
    gpu_name,
    gpu_clock
  ) |>
  dplyr::left_join(
    gpu_clock_add_df,
    by = "model_std"
  ) |>
  dplyr::mutate(
    gpu_clock = dplyr::coalesce(gpu_clock, gpu_clock_new)
  ) |>
  dplyr::select(-gpu_clock_new)

# ============================================================
# 3. GPU TRANSITIONS
# ============================================================

gpu_transitions <- gpu_technology |>
  dplyr::select(
    gpu_name,
    gpu_family,
    gpu_tier,
    gpu_architecture,
    gpu_generation,
    gpu_predecessor,
    gpu_evolution_type
  ) |>
  dplyr::distinct() |>
  dplyr::mutate(
    performance_gain = NA_real_,
    evidence_level = NA_character_,
    source = NA_character_
  )


# ============================================================
# 4. ADRENO — PERFORMANCE GAIN
# ============================================================

gpu_transitions <- gpu_transitions |>
  dplyr::mutate(
    
    performance_gain = dplyr::case_when(
      
      # Adreno 600
      gpu_name == "Adreno 610" ~ 0.300,
      gpu_name == "Adreno 611" ~ 0.000,
      gpu_name == "Adreno 613" ~ 0.000,
      gpu_name == "Adreno 619" ~ 0.150,
      gpu_name == "Adreno 642L" ~ -0.333,
      gpu_name == "Adreno 644" ~ 0.150,
      
      # Adreno 700
      gpu_name == "Adreno 710" ~ 0.250,
      gpu_name == "Adreno 720" ~ 1.000,
      gpu_name == "Adreno 725" ~ 0.884,
      gpu_name == "Adreno 730" ~ 0.500,
      gpu_name == "Adreno 732" ~ 0.450,
      gpu_name == "Adreno 735" ~ 0.160,
      gpu_name == "Adreno 740" ~ 0.504,
      gpu_name == "Adreno 750" ~ 0.050,
      
      # Adreno 800
      gpu_name == "Adreno 810" ~ 0.250,
      gpu_name == "Adreno 830" ~ 0.209,
      
      TRUE ~ performance_gain
    ),
    
    evidence_level = dplyr::case_when(
      
      gpu_name %in% c(
        "Adreno 619",
        "Adreno 644",
        "Adreno 710",
        "Adreno 725",
        "Adreno 730",
        "Adreno 735",
        "Adreno 740",
        "Adreno 750",
        "Adreno 810",
        "Adreno 830"
      ) ~ "Official/technical",
      
      gpu_name %in% c(
        "Adreno 610",
        "Adreno 611",
        "Adreno 613",
        "Adreno 642L",
        "Adreno 720",
        "Adreno 732"
      ) ~ "Technical_proxy",
      
      TRUE ~ evidence_level
    )
  )


# ============================================================
# 5. MALI + IMMORTALIS — PERFORMANCE GAIN
# ============================================================

gpu_transitions <- gpu_transitions |>
  dplyr::mutate(
    
    performance_gain = dplyr::case_when(
      
      # --------------------------------------------------------
      # Architecture Change
      # --------------------------------------------------------
      
      gpu_name == "Mali-G51MP4" ~ 0.400,
      gpu_name == "Mali-G57MP2" ~ 0.300,
      gpu_name == "Mali-G77MP9" ~ 0.300,
      gpu_name == "Immortalis-G720MP12" ~ 0.150,
      gpu_name == "Immortalis-G925MP12" ~ 0.370,
      
      # --------------------------------------------------------
      # Generation Change
      # --------------------------------------------------------
      
      gpu_name == "Mali-T820" ~ 0.400,
      gpu_name == "Mali-G52" ~ 0.300,
      gpu_name == "Mali-G68MP2" ~ 0.200,
      gpu_name == "Mali-G610MP3" ~ 0.200,
      gpu_name == "Mali-G710MP7" ~ 0.200,
      gpu_name == "Mali-G715MP7" ~ 0.150,
      gpu_name == "Immortalis-G715MP11" ~ 0.150,
      gpu_name == "Mali-G615MP2" ~ 0.150,
      
      # --------------------------------------------------------
      # Configuration
      # --------------------------------------------------------
      
      gpu_name == "Mali-G52MP2" ~ -0.333,
      gpu_name == "Mali-G57MP4" ~ 0.600,
      gpu_name == "Mali-G68MP4" ~ 1.000,
      gpu_name == "Mali-G68MP5" ~ 1.500,
      gpu_name == "Mali-G610MP4" ~ 0.330,
      gpu_name == "Mali-G610MP6" ~ 1.000,
      gpu_name == "Mali-G615MP6" ~ 2.000,
      gpu_name == "Mali-G710MP10" ~ 0.430,
      
      TRUE ~ performance_gain
    ),
    
    evidence_level = dplyr::case_when(
      
      # Official
      gpu_name %in% c(
        "Mali-G52",
        "Mali-G57MP2",
        "Mali-G610MP3",
        "Mali-G710MP7",
        "Mali-T820"
      ) ~ "Official",
      
      # Official / technical
      gpu_name %in% c(
        "Mali-G51MP4",
        "Mali-G77MP9",
        "Mali-G68MP2",
        "Mali-G715MP7",
        "Immortalis-G715MP11",
        "Immortalis-G720MP12",
        "Immortalis-G925MP12",
        "Mali-G615MP2",
        "Mali-G615MP6"
      ) ~ "Official/technical",
      
      # Technical proxy
      gpu_name %in% c(
        "Mali-G52MP2",
        "Mali-G57MP4",
        "Mali-G68MP4",
        "Mali-G68MP5",
        "Mali-G610MP4",
        "Mali-G610MP6",
        "Mali-G710MP10"
      ) ~ "Technical_proxy",
      
      TRUE ~ evidence_level
    )
  )


# ============================================================
# 6. APPLE + POWERVR + XCLIPSE
# ============================================================

gpu_transitions <- gpu_transitions |>
  dplyr::mutate(
    
    performance_gain = dplyr::case_when(
      
      # --------------------------------------------------------
      # Apple
      # --------------------------------------------------------
      
      gpu_name == "G15P MP5" ~ 0.000,
      gpu_name == "G16P MP6" ~ 0.200,
      gpu_name == "G17P MP5" ~ 0.170,
      gpu_name == "G17P MP6" ~ 0.200,
      
      # --------------------------------------------------------
      # PowerVR
      # --------------------------------------------------------
      
      gpu_name == "GE8300" ~ 0.000,
      gpu_name == "PowerVR GE8320" ~ 1.000,
      gpu_name == "GE8322" ~ 1.000,
      
      # --------------------------------------------------------
      # Samsung Xclipse
      # --------------------------------------------------------
      
      gpu_name == "Xclipse 940" ~ 0.200,
      
      TRUE ~ performance_gain
    ),
    
    evidence_level = dplyr::case_when(
      
      gpu_name == "G15P MP5" ~ "Official/technical",
      gpu_name == "G16P MP6" ~ "Official",
      gpu_name == "G17P MP5" ~ "Technical_proxy",
      gpu_name == "G17P MP6" ~ "Official",
      
      gpu_name == "GE8300" ~ "Technical_proxy",
      gpu_name == "PowerVR GE8320" ~ "Technical_proxy",
      gpu_name == "GE8322" ~ "Technical_proxy",
      
      gpu_name == "Xclipse 940" ~ "Technical_proxy",
      
      TRUE ~ evidence_level
    )
  )


# ============================================================
# 7. CORREZIONI FINALI CLASSIFICAZIONE
# ============================================================

gpu_transitions <- gpu_transitions |>
  dplyr::mutate(
    gpu_evolution_type = dplyr::case_when(
      gpu_name == "G15P MP5" ~ "No_Predecessor",
      gpu_name == "G17P MP6" ~ "Generation_Change",
      TRUE ~ gpu_evolution_type
    )
  )


# ============================================================
# 8. STRUCTURAL FACTOR
# ============================================================

gpu_transitions <- gpu_transitions |>
  dplyr::mutate(
    structural_factor = dplyr::case_when(
      
      gpu_evolution_type == "No_Predecessor" ~ 1,
      
      !is.na(performance_gain) ~
        pmax(1, 1 + performance_gain),
      
      TRUE ~ NA_real_
    )
  )


# ============================================================
# 9. CONTROLLO TRANSITIONS
# ============================================================

gpu_transitions |>
  dplyr::summarise(
    n = dplyr::n(),
    performance_missing = sum(is.na(performance_gain)),
    structural_missing = sum(is.na(structural_factor)),
    evidence_missing = sum(is.na(evidence_level))
  )


# Controllo duplicati
gpu_transitions |>
  dplyr::count(gpu_name) |>
  dplyr::filter(n > 1)


# Controllo classificazione
gpu_transitions |>
  dplyr::count(gpu_evolution_type)


# ============================================================
# 10. TRASFERIMENTO STRUCTURAL FACTOR A GPU_DF
# ============================================================

gpu_df <- gpu_df |>
  dplyr::left_join(
    gpu_transitions |>
      dplyr::select(
        gpu_name,
        gpu_tier,
        gpu_evolution_type,
        performance_gain,
        evidence_level,
        structural_factor
      ),
    by = "gpu_name",
    relationship = "many-to-one"
  )


# ============================================================
# 11. CLOCK FACTOR
# ============================================================

gpu_df <- gpu_df |>
  dplyr::group_by(gpu_tier) |>
  dplyr::mutate(
    
    clock_percentile = dplyr::percent_rank(gpu_clock),
    
    # Scenario principale: ±10%
    Clock_Factor_10 =
      0.90 + 0.20 * clock_percentile,
    
    # Robustness check: ±15%
    Clock_Factor_15 =
      0.85 + 0.30 * clock_percentile,
    
    # Robustness check: ±20%
    Clock_Factor_20 =
      0.80 + 0.40 * clock_percentile
    
  ) |>
  dplyr::ungroup()


# ============================================================
# 12. GPU INNOVATION INDEX
# ============================================================

gpu_df <- gpu_df |>
  dplyr::mutate(
    
    GPU_Innovation_10 =
      structural_factor * Clock_Factor_10,
    
    GPU_Innovation_15 =
      structural_factor * Clock_Factor_15,
    
    GPU_Innovation_20 =
      structural_factor * Clock_Factor_20
    
  )


# ============================================================
# 13. CONTROLLO FINALE
# ============================================================

gpu_df |>
  dplyr::summarise(
    n = dplyr::n(),
    structural_factor_missing =
      sum(is.na(structural_factor)),
    performance_gain_missing =
      sum(is.na(performance_gain)),
    clock_missing =
      sum(is.na(gpu_clock)),
    innovation_10_missing =
      sum(is.na(GPU_Innovation_10))
  )


# Distribuzione indice principale
gpu_df |>
  dplyr::summarise(
    n = dplyr::n(),
    min = min(GPU_Innovation_10, na.rm = TRUE),
    mean = mean(GPU_Innovation_10, na.rm = TRUE),
    median = median(GPU_Innovation_10, na.rm = TRUE),
    max = max(GPU_Innovation_10, na.rm = TRUE)
  )


# ============================================================
# 14. ROBUSTNESS CHECK
# ============================================================

gpu_df |>
  dplyr::select(
    GPU_Innovation_10,
    GPU_Innovation_15,
    GPU_Innovation_20
  ) |>
  cor(
    use = "complete.obs"
  )


top_gpu_models <- gpu_df |>
  dplyr::distinct(model_std, GPU_Innovation_15, .keep_all = TRUE) |>
  dplyr::arrange(dplyr::desc(GPU_Innovation_15)) |>
  dplyr::select(
    model_std,
    Brand,
    gpu_name,
    gpu_tier,
    gpu_evolution_type,
    gpu_clock,
    GPU_Innovation_15
  )

top_gpu_models |>
  dplyr::slice_head(n = 20)


gpu_brand_innovation <- gpu_df |>
  dplyr::group_by(Brand) |>
  dplyr::summarise(
    n_models = dplyr::n_distinct(model_std),
    mean_GPU_Innovation = mean(GPU_Innovation_15, na.rm = TRUE),
    median_GPU_Innovation = median(GPU_Innovation_15, na.rm = TRUE),
    max_GPU_Innovation = max(GPU_Innovation_15, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(mean_GPU_Innovation))

# Isolo i telefoni del 2024
gpu_df <- gpu_df |>
  dplyr::filter(
    Released.Year == "2024"
  ) |>
  dplyr::select(
    Brand, Model, model_std, GPU_Innovation_15
  )

# Lavoro di modifica del gpu index
gpu_2023 <- phones |>
  filter(Released.Year == 2023) |>
  distinct(gpu_name)

gpu_2024 <- phones |>
  filter(Released.Year == 2024) |>
  distinct(gpu_name)

gpu_2024 |>
  anti_join(gpu_2023, by = "gpu_name") |>
  arrange(gpu_name)

# 1. Correzione Xclipse 530
gpu_transitions <- gpu_transitions %>%
  mutate(
    gpu_evolution_type = if_else(
      gpu_name == "Xclipse 530",
      "Architecture_Change",
      gpu_evolution_type
    ),
    performance_gain = if_else(
      gpu_name == "Xclipse 530",
      0.53,
      performance_gain
    )
  )

# 2. GPU nuove osservate nel mercato 2024
new_gpus_2024 <- c(
  "Adreno 732",
  "Adreno 735",
  "Adreno 810",
  "Adreno 830",
  "Immortalis-G925MP12",
  "Xclipse 530",
  "Xclipse 940",
  "G17P MP5",
  "G17P MP6"
)

# 3. Flag di innovazione strutturale 2024
gpu_transitions <- gpu_transitions %>%
  mutate(
    gpu_new_2024 = gpu_name %in% new_gpus_2024
  )

# 4. Pesi dell'evoluzione
gpu_transitions <- gpu_transitions %>%
  mutate(
    evolution_weight = case_when(
      gpu_evolution_type == "Architecture_Change" ~ 2.5,
      gpu_evolution_type == "Generation_Change" ~ 1.5,
      gpu_evolution_type == "Configuration" ~ 0.5,
      TRUE ~ NA_real_
    )
  )

# 5. Structural Factor
#
# Direttiva specifica Adreno 700:
# - Adreno 732: evoluzione di configurazione rispetto ad Adreno 725.
#   Miglioramento della potenza grafica = +45%.
#   Il +5% di efficienza è mantenuto come evidenza descrittiva, ma non
#   entra nel Structural Factor perché la metodologia usa performance_gain.
#
# - Adreno 735: doppio elemento:
#   (a) evoluzione generazionale rispetto ad Adreno 725, con +45%;
#   (b) evoluzione di configurazione rispetto ad Adreno 732, con +16%.
#
# Tutti gli altri GPU mantengono ESATTAMENTE la metodologia precedente.
gpu_transitions <- gpu_transitions %>%
  mutate(
    structural_factor = case_when(

      !gpu_new_2024 ~ 1,

      # --------------------------------------------------------
      # CORREZIONE ADRENO 732
      # 725 -> 732: Configuration, +45%
      # --------------------------------------------------------
      gpu_name == "Adreno 732" ~
        1 + 0.450 * 1.5,

      # --------------------------------------------------------
      # CORREZIONE ADRENO 735
      # 725 -> 735: Generation, +45%
      # 732 -> 735: Configuration, +16%
      # --------------------------------------------------------
      gpu_name == "Adreno 735" ~
        1 +
        (0.450 * 1.5) +
        (0.160 * 0.5),

      # --------------------------------------------------------
      # TUTTI GLI ALTRI GPU: METODOLOGIA ORIGINALE
      # --------------------------------------------------------
      is.na(evolution_weight) ~ 1,
      is.na(performance_gain) ~ 1,
      TRUE ~ 1 + performance_gain * evolution_weight
    )
  )

# Controllo mirato delle transizioni Adreno 725 -> 732 -> 735
gpu_transitions %>%
  filter(gpu_name %in% c("Adreno 725", "Adreno 732", "Adreno 735")) %>%
  select(
    gpu_name,
    gpu_predecessor,
    gpu_evolution_type,
    performance_gain,
    evolution_weight,
    structural_factor
  ) %>%
  arrange(gpu_name)

gpu_2024_check <- phones %>%
  filter(Released.Year == 2024) %>%
  select(Brand, Model, gpu_name) %>%
  left_join(
    gpu_transitions %>%
      select(
        gpu_name,
        gpu_new_2024,
        structural_factor
      ),
    by = "gpu_name"
  )

gpu_2024_check %>%
  summarise(
    n_phones = n(),
    n_gpu_missing = sum(is.na(structural_factor)),
    n_new_gpu = sum(gpu_new_2024, na.rm = TRUE),
    n_structural_positive = sum(structural_factor > 1, na.rm = TRUE)
  )

gpu_clock_check <- phones %>%
  filter(Released.Year %in% c(2023,2024)) %>%
  select(
    Released.Year,
    Brand,
    Model,
    model_std,
    gpu_name,
    gpu_clock
  ) %>%
  left_join(
    gpu_clock_add_df,
    by = "model_std"
  ) %>%
  mutate(
    gpu_clock = dplyr::coalesce(gpu_clock, gpu_clock_new)
  ) %>%
  select(-gpu_clock_new) %>%
  left_join(
    gpu_transitions %>%
      select(gpu_name, gpu_tier) %>%
      distinct(),
    by = "gpu_name"
  ) %>%
  arrange(gpu_tier, gpu_name, Released.Year, gpu_clock)

gpu_clock_check

gpu_clock_frontier_2023 <- gpu_clock_check %>%
  filter(Released.Year == 2023, !is.na(gpu_clock)) %>%
  group_by(gpu_tier) %>%
  summarise(
    clock_frontier_2023 = max(gpu_clock),
    .groups = "drop"
  )

gpu_clock_frontier_2023

gpu_clock_2024_frontier <- gpu_clock_check %>%
  filter(Released.Year == 2024, !is.na(gpu_clock)) %>%
  left_join(
    gpu_clock_frontier_2023,
    by = "gpu_tier"
  ) %>%
  mutate(
    HE_clock = pmax(
      0,
      (gpu_clock - clock_frontier_2023) /
        clock_frontier_2023
    )
  ) %>%
  filter(HE_clock > 0) %>%
  arrange(desc(HE_clock)) %>%
  select(
    Brand,
    Model,
    gpu_name,
    gpu_tier,
    gpu_clock,
    clock_frontier_2023,
    HE_clock
  )

gpu_clock_2024_frontier

gpu_clock_frontier_summary <- gpu_clock_2024_frontier %>%
  group_by(gpu_tier, gpu_name, gpu_clock, clock_frontier_2023, HE_clock) %>%
  summarise(
    n_phones = n(),
    .groups = "drop"
  ) %>%
  arrange(gpu_tier, desc(HE_clock), gpu_name)

gpu_clock_frontier_summary

gpu_clock_frontier_summary %>%
  summarise(
    numerator = sum(HE_clock * n_phones),
    denominator = sum(n_phones),
    mean_HE_clock = numerator / denominator
  )

gpu_clock_innovation <- gpu_clock_frontier_summary %>%
  select(
    gpu_tier,
    gpu_name,
    gpu_clock,
    clock_frontier_2023,
    HE_clock
  ) %>%
  mutate(
    Clock_Innovation = HE_clock * 100
  )

# Rifacciamo il clock
gpu_clock_frontier_2023 <- gpu_clock_check %>%
  filter(
    Released.Year == 2023,
    !is.na(gpu_clock)
  ) %>%
  group_by(gpu_tier) %>%
  summarise(
    clock_frontier_2023 = max(gpu_clock),
    .groups = "drop"
  ) %>%
  arrange(gpu_tier)

gpu_clock_frontier_2023


gpu_clock_frontier_2024 <- gpu_clock_check %>%
  filter(
    Released.Year == 2024,
    !is.na(gpu_clock)
  ) %>%
  left_join(
    gpu_clock_frontier_2023,
    by = "gpu_tier"
  ) %>%
  filter(gpu_clock > clock_frontier_2023) %>%
  group_by(
    gpu_tier,
    gpu_name,
    gpu_clock,
    clock_frontier_2023
  ) %>%
  summarise(
    n_phones = n(),
    .groups = "drop"
  ) %>%
  mutate(
    HE_clock =
      (gpu_clock - clock_frontier_2023) /
      clock_frontier_2023,
    F_clock = 1 + HE_clock
  ) %>%
  arrange(gpu_tier, desc(gpu_clock))

gpu_clock_frontier_2024


gpu_clock_2024_check <- gpu_clock_check %>%
  filter(
    Released.Year == 2024,
    !is.na(gpu_clock)
  ) %>%
  left_join(
    gpu_clock_frontier_2023,
    by = "gpu_tier"
  ) %>%
  mutate(
    HE_clock = pmax(
      0,
      (gpu_clock - clock_frontier_2023) /
        clock_frontier_2023
    ),
    F_clock = 1 + HE_clock
  ) %>%
  select(
    Brand,
    Model,
    gpu_name,
    gpu_tier,
    gpu_clock,
    clock_frontier_2023,
    HE_clock,
    F_clock
  ) %>%
  arrange(desc(HE_clock), Brand, Model)


gpu_2024_check <- gpu_2024_check %>%
  left_join(
    gpu_clock_2024_check %>%
      select(
        Brand,
        Model,
        gpu_name,
        gpu_clock,
        gpu_tier,
        clock_frontier_2023,
        HE_clock,
        F_clock
      ),
    by = c("Brand", "Model", "gpu_name")
  )


# Indice finale
gpu_2024_check <- gpu_2024_check %>%
  mutate(
    structural_norm =
      100 * (structural_factor - 1) /
      max(structural_factor - 1),
    
    clock_norm =
      100 * HE_clock /
      max(HE_clock, na.rm = TRUE)
  )

gpu_2024_check %>%
  select(
    Brand,
    Model,
    gpu_name,
    structural_factor,
    structural_norm,
    gpu_clock,
    HE_clock,
    clock_norm
  ) %>%
  arrange(
    desc(structural_norm),
    desc(clock_norm)
  ) %>%
  head(20)


gpu_2024_check <- gpu_2024_check %>%
  mutate(
    GPU_Innovation_Index =
      0.85 * structural_norm +
      0.15 * clock_norm
  )

gpu_2024_check %>%
  select(
    Brand,
    Model,
    gpu_name,
    structural_norm,
    clock_norm,
    GPU_Innovation_Index
  ) %>%
  arrange(desc(GPU_Innovation_Index)) %>%
  head(20)


gpu_2024_check %>%
  summarise(
    n_phones = n(),
    min_GPU_Index = min(GPU_Innovation_Index, na.rm = TRUE),
    max_GPU_Index = max(GPU_Innovation_Index, na.rm = TRUE),
    n_below_0 = sum(GPU_Innovation_Index < 0, na.rm = TRUE),
    n_above_100 = sum(GPU_Innovation_Index > 100, na.rm = TRUE),
    n_zero = sum(GPU_Innovation_Index == 0, na.rm = TRUE)
  )


gpu_2024_check <- gpu_2024_check %>%
  mutate(
    GPU_Innovation_Index = pmin(
      100,
      pmax(
        0,
        0.85 * structural_norm +
          0.15 * clock_norm
      )
    )
  )


gpu_2024_check %>%
  summarise(
    n_phones = n(),
    min_GPU_Index = min(GPU_Innovation_Index, na.rm = TRUE),
    max_GPU_Index = max(GPU_Innovation_Index, na.rm = TRUE),
    n_below_0 = sum(GPU_Innovation_Index < 0, na.rm = TRUE),
    n_above_100 = sum(GPU_Innovation_Index > 100, na.rm = TRUE),
    n_zero = sum(GPU_Innovation_Index == 0, na.rm = TRUE)
  )


gpu_2024_check %>%
  summarise(
    n_structural_gt0 =
      sum(structural_norm > 0, na.rm = TRUE),
    mean_structural_norm =
      mean(structural_norm, na.rm = TRUE),
    median_structural_norm =
      median(structural_norm, na.rm = TRUE),
    max_structural_norm =
      max(structural_norm, na.rm = TRUE)
  )


gpu_2024_index <- gpu_2024_check |>
  select(
    Brand,
    Model,
    GPU_Innovation_Index
  )

gpu_df <- gpu_df |>
  mutate(
    
    # Passo 1: trasformazione logaritmica
    # CPU_Innovation_Index è già uno score composito 0-100.
    log_temp = case_when(
      is.na(GPU_Innovation_Index) ~ NA_real_,
      GPU_Innovation_Index == 0 ~ 0,
      GPU_Innovation_Index > 0 ~
        log(GPU_Innovation_Index + 1)
    ),
    
    # Passo 2: Min-Max con minimo teorico = 0
    GPU_Innovation_log_norm = case_when(
      is.na(log_temp) ~ NA_real_,
      log_temp == 0 ~ 0,
      log_temp > 0 ~
        (log_temp - 0) /
        (max(log_temp[log_temp > 0], na.rm = TRUE) - 0) * 100
    )
  )

gpu_innovation_df <- gpu_df |>
  select(-GPU_Innovation_Index, -log_temp)

saveRDS(gpu_innovation_df, "GPU Innovation Index.rds")

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