library(tidyverse)

phones <- readRDS("dataset definitivo.rds")

# Sistemiamo i valori mancanti
ai_missing_models <- c(
  "Honor Magic V3",
  "V40",
  "V40 Pro",
  "SM-W9025 W25 5G Dual SIM TD-LTE CN 1TB",
  "vivo X100 Ultra 5G Premium Edition Dual SIM TD-LTE CN 1TB V2366HA",
  "Reno12 Pro 5G 2024 Premium Edition Global Dual SIM TD-LTE V1 512GB CPH2629",
  "ROG Phone 8 Pro 5G Global Dual SIM TD-LTE 512GB AI2401",
  "ROG Phone 8 Pro Edition 5G Global Dual SIM TD-LTE 1TB AI2401",
  "ROG Phone 9 Pro 5G Global Dual SIM TD-LTE 512GB AI2501",
  "ROG Phone 9 Pro Edition 5G Global Dual SIM TD-LTE 1TB AI2501",
  "ThinkPhone 25 5G Standard Edition Global Dual SIM TD-LTE 256GB XT2409-6",
  "Redmi Note 14 Pro 5G Premium Edition Dual SIM TD-LTE CN 512GB 24090RA29C",
  "Redmi Note 14 Pro+ 5G Top Edition Dual SIM TD-LTE CN 512GB 24115RA8EC",
  "Redmi K80 5G Standard Edition Dual SIM TD-LTE CN 512GB 24117RK2CC",
  "Redmi K80 Pro 5G Lamborghini Squadra Corse Edition Dual SIM TD-LTE CN 1TB 24127RK2CC",
  "Pura 70 Pro+ Dual SIM TD-LTE CN 1TB HBN-AL10 / HBN-AL80",
  "RAZR 50 5G 2024 Premium Edition Global Dual SIM TD-LTE 512GB XT2453-1",
  "RAZR 50d 5G 2024 Standard Edition Dual SIM TD-LTE JP 256GB M-51E XT2453-8",
  "RAZR 50s 5G 2024 Standard Edition Dual SIM TD-LTE JP 256GB A403MO XT2453-9",
  "RAZR+ 5G 5th gen 2024 Premium Edition TD-LTE US 256GB XT2451-2",
  "Pixel 9 5G Global TD-LTE 129GB GUR25",
  "Pocket 2 4G Standard Edition Dual SIM TD-LTE CN 1TB LEM-AL00",
  "Nova Flip 4G Dual SIM TD-LTE CN 256GB PSD-AL00",
  "Nova 12 Pro 4G Dual SIM TD-LTE CN 256GB ADA-AL00",
  "Moto Edge 50 5G 2024 Premium Edition Global Dual SIM TD-LTE 25GB XT2407-1",
  "Moto Edge 50 Ultra 5G 2024 Premium Edition Global Dual SIM TD-LTE 1TB XT2401-1",
  "Moto Edge 50s Pro 5G 2024 Standard Edition Dual SIM TD-LTE JP A402MO XT2403-5",
  "Mate XT 4G Ultimate Design Dual SIM TD-LTE CN 1TB GRL-AL10",
  "Mi 14 Ultra 5G Premium Edition Global Dual SIM TD-LTE 512GB 24030PN60G",
  "Mi 14T 5G Global Dual SIM TD-LTE 512GB 2406APNFAG",
  "Mi Mix Flip 5G Standard Edition Global Dual SIM TD-LTE 512GB 2405CPX3DG",
  "Mi Mix Fold 4 2024 5G Premium Edition Dual SIM TD-LTE CN 1TB 24072PX77C",
  "iPhone 16 5G A3287 Global Dual SIM TD-LTE 512GB",
  "iPhone 16 Plus 5G A3290 Global Dual SIM TD-LTE 512GB",
  "iPhone 16 Pro Max 5G A3296 Global Dual SIM TD-LTE 512GB",
  "IronFlip 5G Global Dual SIM TD-LTE 512GB VTL-202302",
  "SM-S926B Galaxy S24+ 5G Global TD-LTE 512GB",
  "SM-A166P/DS Galaxy A16 5G 2024 Standard Edition Global Dual SIM TD-LTE 128GB",
  "Find X7 Ultra 5G Standard Edition Dual SIM TD-LTE CN 256GB PHY110",
  "Civi 4 Pro 5G Premium Edition Dual SIM TD-LTE CN 512GB 24053PY09C",
  "Aquos R9 Pro 5G TD-LTE JP SH-M30",
  "Ace 3 Pro 5G Porcelain Collector Edition Dual SIM TD-LTE CN 1TB PJX110",
  "Zenfone 10 5G Standard Edition Global Dual SIM TD-LTE 256GB AI2302",
  "21 Pro 5G Premium Edition Dual SIM TD-LTE CN 1TB M481Q",
  "Vivo X Flip 5G Dual SIM TD-LTE CN 256GB V2256A",
  "Vivo X Fold2 5G 2023 Dual SIM TD-LTE CN 256GB V2266A",
  "Vivo X90 5G Standard Edition Global Dual SIM TD-LTE 256GB V2218",
  "Vivo X90 Pro 5G Premium Edition Global Dual SIM TD-LTE 256GB V2219",
  "Vivo X90s 5G Standard Edition Dual SIM TD-LTE CN 256GB V2241HA",
  "vivo iQOO 11S 5G Premium Edition Dual SIM TD-LTE CN 1TB V2304A",
  "vivo iQOO 12 Pro 5G Premium Edition Dual SIM TD-LTE CN 1TB V2329A",
  "ROG Phone 7 5G Premium Edition Global Dual SIM TD-LTE Version A 256GB AI2205",
  "ROG Phone 7 Pro 5G Dual SIM TD-LTE CN Version B 512GB AI2205",
  "ROG Phone 7 Ultimate 5G Global Dual SIM TD-LTE Version A 512GB AI2205",
  "ThinkPhone 5G 2023 Global Dual SIM TD-LTE 256GB XT2309-2",
  "Reno11 Pro 5G Dual SIM TD-LTE CN 256GB PJJ110",
  "Reno10 Pro+ 5G Standard Edition Global Dual SIM TD-LTE TW V4 256GB CPH2521",
  "Redmi K60 5G Standard Edition Dual SIM TD-LTE CN 128GB 23013RK75C",
  "Redmi K60 Pro 5G Top Edition Dual SIM TD-LTE CN 512GB 22127RK46C",
  "Redmi K70 5G Premium Edition Dual SIM TD-LTE CN 1TB 23113RKC6C",
  "Redmi K70 Pro 5G Top Edition Dual SIM TD-LTE CN 1TB 23117RK66C",
  "P60 4G Global Dual SIM TD-LTE 256GB LNA-LX9 / LNA-L29",
  "P60 Art 4G Global Dual SIM TD-LTE 1TB MNA-LX9 / MNA-L29",
  "P60 Pro 4G Standard Edition Global TD-LTE 256GB MNA-LX9 / MNA-L09",
  "Phone (2) 5G Standard Edition Global Dual SIM TD-LTE 128GB A065",
  "Phone 5G Premium Edition Dual SIM TD-LTE CN 1TB N2301",
  "Pixel Fold 5G UW Global TD-LTE 512GB G9FPL",
  "Nubia Red Magic 8 Pro+ 5G Transformers Edition Dual SIM TD-LTE CN 512GB NX729J",
  "Nubia Red Magic 8S Pro 5G Standard Edition Dual SIM TD-LTE CN 256GB NX729S",
  "Nubia Red Magic 8S Pro+ 5G Top Edition Dual SIM TD-LTE CN 256GB NX729S",
  "Nubia Z50 5G Urban Outdoor Edition Global Dual SIM TD-LTE 256GB NX711J",
  "Nubia Z50 Ultra 5G Standard Edition Dual SIM TD-LTE CN 256GB NX712J",
  "Nubia Z50S Pro 5G Standard Edition Global Dual SIM TD-LTE 1TB NX713J",
  "Nubia Z60 Ultra 5G Premium Edition Global Dual SIM TD-LTE 512GB NX721J",
  "Open 5G 2023 Premium Edition Global TD-LTE 512GB CPH2551",
  "Nova 12 Ultra 4G Dual SIM TD-LTE CN 1TB ADA-AL00U",
  "Moto Edge+ 5G 2023 3rd gen Standard Edition TD-LTE US 512GB XT2301-1",
  "Mi 14 5G Base Edition Dual SIM TD-LTE CN 256GB 23127PN0CC",
  "Mi Mix Fold 3 2023 5G Premium Edition Dual SIM TD-LTE CN 1TB 2308CPXD0C",
  "Mate 60 Pro+ Dual SIM TD-LTE CN 512GB ALN-AL10",
  "Mate 60 RS Ultimate Design Dual SIM TD-LTE CN 1TB ALN-AL10",
  "Mate X 3 4G Standard Edition Global Dual SIM TD-LTE 512GB ALT-LX9 / ALT-L29",
  "Mate X5 4G Collector Edition Dual SIM TD-LTE CN 1TB ALT-AL10",
  "Metavertu 2 5G Global Dual SIM TD-LTE 512GB VTL-202301",
  "Mi 13 5G Standard Edition Global Dual SIM TD-LTE 256GB 2211133G",
  "Mi 13 Pro 5G Premium Edition Global Dual SIM TD-LTE 512GB 2210132G",
  "Mi 13 Ultra 5G Standard Edition Global Dual SIM TD-LTE 512GB 2304FPN6DG",
  "iPhone 15 Pro Max 5G A3106 Global Dual SIM TD-LTE 1TB",
  "Honor 80 GT 5G Standard Edition Dual SIM TD-LTE CN 512GB AGT-AN00",
  "Honor 80 Pro Flat Screen Edition 5G Dual SIM TD-LTE CN 256GB ANB-AN00",
  "Honor Magic 5 5G Global Dual SIM TD-LTE 256GB PGT-N09",
  "Honor Magic 5 Ultimate 5G Dual SIM TD-LTE CN 512GB PGT-AN20",
  "Honor Magic Vs 5G Premium Edition Global Dual SIM TD-LTE 512GB FRI-NX9",
  "SM-S916B Galaxy S23+ 5G Global TD-LTE 256GB",
  "SM-F731B Galaxy Z Flip 5 5G Global TD-LTE 512GB",
  "SM-F946B/DS Galaxy Z Fold5 5G Global Dual SIM TD-LTE 1TB",
  "Find X6 5G Premium Edition Dual SIM TD-LTE CN 512GB PGFM10",
  "Find X6 Pro 5G Standard Edition Dual SIM TD-LTE CN 256GB PGEM10",
  "Find N3 5G 2023 Premium Edition Global TD-LTE 512GB CPH2499",
  "Axon 50 Ultra 5G Dual SIM TD-LTE CN 512GB",
  "Aquos R8s 5G Global TD-LTE SH-R80",
  "Aquos R8s Pro 5G Global TD-LTE SH-R80P",
  "Ace 2 Pro 5G Genshin Impact Limited Edition Dual SIM TD-LTE CN 512GB PJA110",
  "Ace 2V 5G Premium Edition Dual SIM TD-LTE CN 256GB PHP110",
  "11R 5G Standard Edition Dual SIM TD-LTE IN 128GB CPH2487",
  "20 Infinity 5G Standard Edition Dual SIM TD-LTE CN 512GB M392Q",
  "20 Pro 5G Premium Edition Dual SIM TD-LTE CN 128GB M391Q",
  "21 5G Standard Edition Dual SIM TD-LTE CN 256GB M461Q"
)

ai_missing_models[
  ai_missing_models == "Moto Edge 50s Pro 5G 2024 Standard Edition Dual SIM TD-LTE JP A402MO XT2403-5"
] <- "Moto Edge 50s Pro 5G 2024 Standard Edition Dual SIM TD-LTE JP 256GB A402MO XT2403-5"

ai_missing <- phones |>
  dplyr::filter(Model %in% ai_missing_models)

ai_reference <- phones |>
  dplyr::filter(
    soc_name %in% unique(ai_missing$soc_name),
    !is.na(INT8.CNNs)
  ) |>
  dplyr::select(
    Model,
    model_std,
    soc_name,
    INT8.CNNs,
    INT8.Transformer,
    FP16.CNNs,
    FP16.Transformer,
    INT8.Parallel,
    FP16.Parallel
  ) |>
  dplyr::arrange(soc_name, Model)

ai_soc_medians <- ai_reference |>
  dplyr::group_by(soc_name) |>
  dplyr::summarise(
    INT8.CNNs = median(INT8.CNNs),
    INT8.Transformer = median(INT8.Transformer),
    FP16.CNNs = median(FP16.CNNs),
    FP16.Transformer = median(FP16.Transformer),
    INT8.Parallel = median(INT8.Parallel),
    FP16.Parallel = median(FP16.Parallel),
    .groups = "drop"
  )


ai_missing_joined <- ai_missing |>
  dplyr::left_join(
    ai_soc_medians,
    by = "soc_name",
    suffix = c("_phone", "_median")
  )

ai_external_reference <- tibble::tribble(
  ~soc_name, ~INT8.CNNs_ext, ~INT8.Transformer_ext, ~FP16.CNNs_ext, ~FP16.Transformer_ext, ~INT8.Parallel_ext, ~FP16.Parallel_ext, ~imputation_method,
  
  "HiSilicon KIRIN8000",
  6.2, 54, 9, 26, 0.4, 0.6,
  "AI-Benchmark smartphone-level: Nova 13 Pro",
  
  "Qualcomm Snapdragon 7s Gen 3",
  157, 776, 29, 6.9, 10, 1.8,
  "AI-Benchmark smartphone-level: median of 7 smartphones",
  
  "MediaTek Dimensity 9200",
  242, 164, 307, 95, 27, 26,
  "AI-Benchmark processor-level"
)

ai_missing_complete <- ai_missing_joined |>
  dplyr::left_join(
    ai_external_reference,
    by = "soc_name"
  )

ai_missing_complete <- ai_missing_complete |>
  dplyr::mutate(
    INT8.CNNs_final = dplyr::coalesce(INT8.CNNs_median, INT8.CNNs_ext),
    INT8.Transformer_final = dplyr::coalesce(INT8.Transformer_median, INT8.Transformer_ext),
    FP16.CNNs_final = dplyr::coalesce(FP16.CNNs_median, FP16.CNNs_ext),
    FP16.Transformer_final = dplyr::coalesce(FP16.Transformer_median, FP16.Transformer_ext),
    INT8.Parallel_final = dplyr::coalesce(INT8.Parallel_median, INT8.Parallel_ext),
    FP16.Parallel_final = dplyr::coalesce(FP16.Parallel_median, FP16.Parallel_ext)
  )

ai_missing_complete <- ai_missing_complete |>
  dplyr::mutate(
    imputation_source = dplyr::case_when(
      !is.na(INT8.CNNs_median) ~ "Internal dataset - SoC median",
      soc_name == "HiSilicon KIRIN8000" ~ "AI-Benchmark - Nova 13 Pro",
      soc_name == "Qualcomm Snapdragon 7s Gen 3" ~ "AI-Benchmark - external smartphone median",
      soc_name == "MediaTek Dimensity 9200" ~ "AI-Benchmark - processor-level",
      TRUE ~ "Not imputed"
    )
  )

ai_imputed <- ai_missing_complete |>
  dplyr::select(
    Model,
    soc_name,
    INT8.CNNs_final,
    INT8.Transformer_final,
    FP16.CNNs_final,
    FP16.Transformer_final,
    INT8.Parallel_final,
    FP16.Parallel_final,
    imputation_source
  )

# INTEGRAZIONE IN PHONES e RIORDINO
phones <- phones |>
  dplyr::left_join(
    ai_imputed,
    by = "Model"
  ) |>
  dplyr::mutate(
    INT8.CNNs = dplyr::coalesce(INT8.CNNs, INT8.CNNs_final),
    INT8.Transformer = dplyr::coalesce(INT8.Transformer, INT8.Transformer_final),
    FP16.CNNs = dplyr::coalesce(FP16.CNNs, FP16.CNNs_final),
    FP16.Transformer = dplyr::coalesce(FP16.Transformer, FP16.Transformer_final),
    INT8.Parallel = dplyr::coalesce(INT8.Parallel, INT8.Parallel_final),
    FP16.Parallel = dplyr::coalesce(FP16.Parallel, FP16.Parallel_final),
    AI_imputation_source = imputation_source
  ) |>
  dplyr::select(
    -INT8.CNNs_final,
    -INT8.Transformer_final,
    -FP16.CNNs_final,
    -FP16.Transformer_final,
    -INT8.Parallel_final,
    -FP16.Parallel_final,
    -imputation_source
  )

ai_df <- phones |>
  dplyr::select(
    Brand,
    Model,
    model_std,
    soc_name.x,
    
    # CPU / AI Benchmark
    CPU.F.Score,
    INT8.CNNs,
    INT8.Transformer,
    FP16.CNNs,
    FP16.Transformer,
    INT8.Parallel,
    FP16.Parallel,
    
    # Provenienza imputazione
    AI_imputation_source
  )

# Ai gen capability
partial_hardware_models <- c(
  "ace 2 pro 5g",
  "ace 2v 5g",
  "11r 5g",
  "20 infinity 5g",
  "20 pro 5g",
  "redmi k60 5g",
  "redmi k60 pro 5g",
  "redmi k70 5g",
  "reno10 pro+ 5g",
  "reno11 pro 5g",
  "rog phone 7 5g",
  "rog phone 7 pro 5g",
  "rog phone 7 ultimate 5g",
  "thinkphone 5g 2023",
  "iqoo 11s 5g",
  "vivo x flip 5g",
  "vivo x fold2 5g",
  "vivo x90 5g",
  "vivo x90 pro 5g",
  "vivo x90s 5g",
  "zenfone 10 5g",
  "nova 12 pro 4g",
  "nova flip 4g",
  "pocket 2 4g",
  "red magic 8 pro+ 5g",
  "red magic 8s pro 5g",
  "red magic 8s pro+ 5g",
  "z50 5g",
  "z50 ultra 5g",
  "z50s pro 5g",
  "open 5g 2023",
  "nova 12 ultra 4g",
  "moto edge+ 5g 2023",
  "mi mix fold 3 2023 5g",
  "mate 60 pro+",
  "mate 60 rs",
  "mate x 3 4g",
  "mate x5 4g",
  "mi 13 5g",
  "mi 13 pro 5g",
  "mi 13 ultra 5g",
  "80 gt 5g",
  "80 pro 5g",
  "magic 5 5g",
  "magic 5 ultimate 5g",
  "magic vs 5g",
  "galaxy s23+ 5g",
  "galaxy z flip 5 5g",
  "galaxy z fold5 5g",
  "find x6 5g",
  "find x6 pro 5g",
  "find n3 5g 2023",
  "axon 50 ultra 5g",
  "aquos r8s 5g",
  "aquos r8s pro 5g",
  "oneplus 11 5g",
  "galaxy s23 5g",
  "galaxy s23 ultra 5g",
  "galaxy s23 fe 5g",
  "magic 5 pro 5g",
  "mate 60",
  "mate 60 pro",
  "mi 13t pro 5g",
  "pixel 7a 5g",
  "poco f4 gt 5g",
  "poco f5 5g",
  "poco f5 pro 5g",
  "razr 40 ultra 5g",
  "gt neo5 se 5g",
  "gt3 5g",
  "redmi k60 extreme edition top 5g",
  "nord 3 5g",
  "oneplus 12r 5g"
)

partial_hardware_models <- dplyr::recode(
  partial_hardware_models,
  "20 infinity 5g" = "20 5g",
  "thinkphone 5g 2023" = "thinkphone 5g",
  "iqoo 11s 5g" = "vivo iqoo 11s 5g",
  "red magic 8 pro+ 5g" = "nubia red magic 8 pro+ 5g",
  "red magic 8s pro 5g" = "nubia red magic 8s pro 5g",
  "red magic 8s pro+ 5g" = "nubia red magic 8s pro+ 5g",
  "z50 5g" = "nubia z50 5g",
  "z50 ultra 5g" = "nubia z50 ultra 5g",
  "z50s pro 5g" = "nubia z50s pro 5g",
  "open 5g 2023" = "open 5g",
  "moto edge+ 5g 2023" = "moto edge+ 5g",
  "mi mix fold 3 2023 5g" = "mi mix fold 3 5g",
  "galaxy z flip 5 5g" = "galaxy z flip5 5g",
  "find n3 5g 2023" = "find n3 5g",
  "oneplus 11 5g" = "11 5g",
  "gt neo5 se 5g" = "realme gt neo5 se 5g",
  "gt3 5g" = "realme gt3 5g",
  "redmi k60 extreme edition top 5g" = "redmi k60 ultra 5g",
  "oneplus 12r 5g" = "12r 5g"
)

ai_df <- ai_df |>
  dplyr::mutate(
    AI_Gen_Capability = dplyr::if_else(
      model_std %in% partial_hardware_models,
      "Partial_Hardware",
      NA_character_
    )
  )

partial_software_models <- c(
  "p60 4g",
  "p60 art 4g",
  "p60 pro 4g",
  "phone (2) 5g",
  "phone 5g",
  "pixel fold 5g",
  "ironflip 5g",
  "metavertu 2 5g",
  "moto edge 40 pro 5g",
  "xperia 1 v 5g",
  "xperia 5 v 5g",
  "moto edge 50 pro 5g",
  "moto edge 50s pro 5g"
)

ai_df <- ai_df |>
  dplyr::mutate(
    AI_Gen_Capability = dplyr::if_else(
      model_std %in% partial_software_models,
      "Partial_Software",
      AI_Gen_Capability
    )
  )

cloud_models <- c(
  "galaxy a16 5g",
  "moto edge 50 5g",
  "pura 70 pro+",
  "razr 50 5g",
  "razr 50d 5g",
  "razr 50s 5g",
  "redmi note 14 pro 5g",
  "redmi note 14 pro+ 5g",
  "reno12 pro 5g",
  "thinkphone 25 5g",
  "v40 5g",
  "v40 pro 5g",
  "galaxy a55 5g",
  "moto edge 50 neo 5g",
  "phone (2a) 5g",
  "poco f6 5g",
  "pura 70",
  "pura 70 pro",
  "pura 70 ultra",
  "razr 5g",
  "redmi k70 ultra 5g",
  "reno12 5g",
  "reno12 f 5g",
  "gt 6 5g",
  "gt 6t 5g",
  "200 5g",
  "200 pro 5g",
  "nord 4 5g",
  "galaxy a54 5g",
  "cmf phone 1 5g",
  "galaxy a35 5g",
  "mate xt 4g",
  "mi 14 ultra 5g",
  "mi 14t 5g",
  "mi mix flip 5g",
  "mi mix fold 4 5g",
  "moto edge 50 ultra 5g"
)


generative_npu_models_std <- c(
  "12 5g",
  "iphone 15 pro 5g",
  "mi 14 pro 5g",
  "pixel 8 5g",
  "pixel 8 pro 5g",
  "redmi k70e 5g",
  "vivo iqoo 12 5g",
  "vivo x100 5g",
  "vivo x100 pro 5g",
  "ace 3v 5g",
  "find x7 5g",
  "find x8 5g",
  "find x8 pro 5g",
  "galaxy s24 5g",
  "galaxy s24 fe 5g",
  "galaxy s24 ultra 5g",
  "galaxy z flip6 5g",
  "galaxy z fold6 5g",
  "iphone 16 pro 5g",
  "mi 14t pro 5g",
  "pixel 8a 5g",
  "pixel 9 pro 5g",
  "pixel 9 pro fold 5g",
  "pixel 9 pro xl 5g",
  "razr 50 ultra 5g",
  "rog phone 8 5g",
  "rog phone 9 5g",
  "vivo x200 5g",
  "vivo x200 pro 5g",
  "vivo x200 pro mini 5g",
  "zenfone 11 ultra 5g",
  "magic 6 pro 5g",
  "13 5g",
  "15 5g",
  "15 pro 5g",
  "21 5g",
  "iphone 15 pro max 5g",
  "mi 14 5g",
  "nubia z60 ultra 5g",
  "redmi k70 pro 5g",
  "vivo iqoo 12 pro 5g",
  "21 pro 5g",
  "ace 3 pro 5g",
  "aquos r9 pro 5g",
  "civi 4 pro 5g",
  "find x7 ultra 5g",
  "galaxy s24+ 5g",
  "iphone 16 pro max 5g",
  "pixel 9 5g",
  "razr+ 5g",
  "redmi k80 5g",
  "redmi k80 pro 5g",
  "rog phone 8 pro 5g",
  "rog phone 8 pro edition 5g",
  "rog phone 9 pro 5g",
  "rog phone 9 pro edition 5g",
  "vivo x100 ultra 5g",
  "w25 5g",
  "magic v3 5g"
)

ai_df <- phones |>
  dplyr::select(Model, model_std) |>
  dplyr::mutate(
    AI_Gen_Capability = dplyr::case_when(
      model_std %in% generative_npu_models_std ~ "Generative_NPU",
      model_std %in% cloud_models ~ "Cloud",
      model_std %in% partial_hardware_models ~ "Partial_Hardware",
      model_std %in% partial_software_models ~ "Partial_Software",
      TRUE ~ NA_character_
    )
  )

# Assegnamo i pesi
ai_df <- ai_df |>
  dplyr::mutate(
    AI_Capability_Weight = dplyr::case_when(
      AI_Gen_Capability == "Generative_NPU"    ~ 1.00,
      AI_Gen_Capability == "Partial_Hardware"  ~ 0.75,
      AI_Gen_Capability == "Partial_Software"  ~ 0.50,
      AI_Gen_Capability == "Cloud"             ~ 0.25,
      TRUE                                     ~ NA_real_
    )
  )

# Riordino
ai_df <- ai_df |>
  dplyr::left_join(
    phones |>
      dplyr::select(model_std, Brand, Released.Year) |>
      dplyr::distinct(),
    by = "model_std"
  )

ai_df <- ai_df |>
  relocate(Brand) |>
  relocate(Released.Year, .after = 3)

# Distribuzione temporale delle categorie
ai_df |>
  dplyr::filter(!is.na(AI_Gen_Capability)) |>
  dplyr::count(
    Released.Year,
    AI_Gen_Capability
  ) |>
  dplyr::arrange(
    Released.Year,
    AI_Gen_Capability
  )

ai_df |>
  dplyr::filter(
    AI_Gen_Capability == "Generative_NPU"
  ) |>
  dplyr::count(Released.Year)

# Includiamo i valori dei benchmark
ai_df <- ai_df |>
  dplyr::left_join(
    phones |>
      dplyr::select(
        model_std,
        INT8.CNNs,
        INT8.Transformer,
        FP16.CNNs,
        FP16.Transformer,
        INT8.Parallel,
        FP16.Parallel
      ) |>
      dplyr::distinct(),
    by = "model_std"
  )


ai_df <- ai_df |>
  mutate(across(c(INT8.CNNs, INT8.Transformer, FP16.CNNs, FP16.Transformer,
                  INT8.Parallel, FP16.Parallel), 
                ~ if_else(is.na(AI_Capability_Weight), NA, .)))

# Iniziamo la costruzione della parte performance dell'indice
ai_frontier_2023 <- ai_df |>
  dplyr::filter(
    Released.Year == 2023,
    AI_Gen_Capability == "Generative_NPU"
  ) |>
  dplyr::summarise(
    INT8.CNNs = max(INT8.CNNs, na.rm = TRUE),
    INT8.Transformer = max(INT8.Transformer, na.rm = TRUE),
    FP16.CNNs = max(FP16.CNNs, na.rm = TRUE),
    FP16.Transformer = max(FP16.Transformer, na.rm = TRUE),
    INT8.Parallel = max(INT8.Parallel, na.rm = TRUE),
    FP16.Parallel = max(FP16.Parallel, na.rm = TRUE)
  )

ai_df <- ai_df |>
  dplyr::mutate(
    AI_Perf_INT8_CNNs =
      INT8.CNNs / ai_frontier_2023$INT8.CNNs,
    
    AI_Perf_INT8_Transformer =
      INT8.Transformer / ai_frontier_2023$INT8.Transformer,
    
    AI_Perf_FP16_CNNs =
      FP16.CNNs / ai_frontier_2023$FP16.CNNs,
    
    AI_Perf_FP16_Transformer =
      FP16.Transformer / ai_frontier_2023$FP16.Transformer,
    
    AI_Perf_INT8_Parallel =
      INT8.Parallel / ai_frontier_2023$INT8.Parallel,
    
    AI_Perf_FP16_Parallel =
      FP16.Parallel / ai_frontier_2023$FP16.Parallel
  )

# Calcolo dell'ai performance
ai_df <- ai_df |>
  dplyr::mutate(
    AI_Performance = rowMeans(
      dplyr::across(
        dplyr::starts_with("AI_Perf_")
      ),
      na.rm = FALSE
    )
  )

# Costruzione dell'AI Innovation Index
#
# L'obiettivo dell'indice è misurare esclusivamente l'innovazione
# tecnologica come estensione della frontiera prestazionale AI 2023.
#
# Pertanto:
# - AI_Performance <= 1  -> nessuna estensione della frontiera -> score = 0
# - AI_Performance > 1   -> estensione della frontiera -> 
#                            capability * performance relativa
#
# IMPORTANTE:
# la soglia viene applicata a AI_Performance, NON a AI_Innovation.
# In questo modo, ad esempio, una configurazione con capability = 0.75
# e performance = 1.20 produce 0.90 e viene correttamente considerata
# innovativa, perché supera la frontiera 2023.

ai_df <- ai_df |>
  dplyr::mutate(
    AI_Innovation = dplyr::case_when(
      is.na(AI_Capability_Weight) ~ NA_real_,
      is.na(AI_Performance) ~ NA_real_,
      AI_Performance > 1 ~ AI_Capability_Weight * AI_Performance,
      TRUE ~ 0
    )
  )

# Manteniamo esclusivamente gli smartphone 2024 per la costruzione
# dell'indice finale e della sua normalizzazione.
ai_df <- ai_df |>
  dplyr::filter(Released.Year == 2024)

# AI_Innovation è lo score PRE-NORMALIZZAZIONE.
# La normalizzazione Min-Max viene effettuata sul campione 2024:
#
# AI_Norm_i = 100 * (AI_i - min(AI)) / (max(AI) - min(AI))
#
# Poiché i casi senza frontier extension sono posti a 0, il minimo
# dovrebbe essere 0 tra le osservazioni con informazione AI disponibile.

ai_min <- min(ai_df$AI_Innovation, na.rm = TRUE)
ai_max <- max(ai_df$AI_Innovation, na.rm = TRUE)

ai_df <- ai_df |>
  dplyr::mutate(
    AI_Innovation_Norm = dplyr::case_when(
      is.na(AI_Innovation) ~ NA_real_,
      AI_Innovation == ai_min ~ 0,
      TRUE ~ 100 * (AI_Innovation - ai_min) / (ai_max - ai_min)
    )
  )

# Normalizzazione con il Min-Max logaritmico
ai_df <- ai_df |>
  mutate(
    # Passo 1: Calcoliamo il logaritmo solo dove AI_Innovation > 0
    log_temp = case_when(
      is.na(AI_Innovation) ~ NA_real_,
      AI_Innovation == 0   ~ 0,
      AI_Innovation > 0    ~ log(((AI_Innovation - 1) * 100) + 1)
    ),
    
    # Passo 2: Applichiamo il Min-Max escludendo gli 0 e i NA dal calcolo di min/max
    AI_Innovation_log_norm = case_when(
      is.na(log_temp) ~ NA_real_,
      log_temp == 0   ~ 0,
      log_temp > 0    ~ (log_temp - 0) / 
        (max(log_temp[log_temp > 0], na.rm = TRUE) - 0) * 100
    )
  )


# Controllo empirico PRE-NORMALIZZAZIONE:
# mostra i telefoni ordinati per AI Innovation grezza.
ai_pre_normalizzazione <- ai_df |>
  dplyr::select(
    Brand,
    Model,
    model_std,
    AI_Gen_Capability,
    AI_Capability_Weight,
    AI_Performance,
    AI_Innovation
  ) |>
  dplyr::arrange(dplyr::desc(AI_Innovation))

print(ai_pre_normalizzazione, n = Inf)

# Controllo empirico POST-NORMALIZZAZIONE:
# mostra i telefoni ordinati per AI Innovation normalizzata.
ai_post_normalizzazione <- ai_df |>
  dplyr::select(
    Brand,
    Model,
    model_std,
    AI_Gen_Capability,
    AI_Capability_Weight,
    AI_Performance,
    AI_Innovation,
    AI_Innovation_Norm
  ) |>
  dplyr::arrange(dplyr::desc(AI_Innovation_Norm))

print(ai_post_normalizzazione, n = Inf)

# Statistiche sintetiche dell'indice AI 2024.
ai_summary <- ai_df |>
  dplyr::summarise(
    N = dplyr::n(),
    N_AI_available = sum(!is.na(AI_Innovation)),
    N_AI_positive = sum(AI_Innovation > 0, na.rm = TRUE),
    N_AI_zero = sum(AI_Innovation == 0, na.rm = TRUE),
    AI_min = min(AI_Innovation, na.rm = TRUE),
    AI_mean = mean(AI_Innovation, na.rm = TRUE),
    AI_median = median(AI_Innovation, na.rm = TRUE),
    AI_max = max(AI_Innovation, na.rm = TRUE),
    AI_Norm_min = min(AI_Innovation_Norm, na.rm = TRUE),
    AI_Norm_mean = mean(AI_Innovation_Norm, na.rm = TRUE),
    AI_Norm_median = median(AI_Innovation_Norm, na.rm = TRUE),
    AI_Norm_max = max(AI_Innovation_Norm, na.rm = TRUE)
  )

print(ai_summary)

definitive_ai_df <- ai_df |>
  select(Brand, Model, model_std, Released.Year, AI_Innovation_log_norm)

saveRDS(definitive_ai_df, "AI Innovation Index.rds")

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