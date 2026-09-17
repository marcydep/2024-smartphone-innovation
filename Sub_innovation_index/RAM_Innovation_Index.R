library(tidyverse)

phones <- readRDS("dataset definitivo.rds")

phones$F_RAM <- 1

phones$F_RAM[phones$model_std %in% c(
  "redmi k80 pro 5g",
  "rog phone 9 5g",
  "rog phone 9 pro 5g",
  "rog phone 9 pro edition 5g",
  "vivo x200 5g",
  "vivo x200 pro 5g",
  "vivo x200 pro mini 5g",
  "15 5g",
  "15 pro 5g",
  "13 5g"
)] <- 1.1111

phones$Ram_innovation_index <- phones$F_RAM

ram_df <- phones |>
  filter(Released.Year == "2024") |>
  select(
    Brand,
    Model,
    model_std,
    Released.Year,
    Ram_innovation_index
  )

# Normalizzazione
ram_df <- ram_df |>
  mutate(
    # Passo 1: Calcoliamo il logaritmo solo dove Ram_innovation_index > 0
    log_temp = case_when(
      is.na(Ram_innovation_index) ~ NA_real_,
      Ram_innovation_index == 0   ~ 0,
      Ram_innovation_index > 0    ~ log(((Ram_innovation_index - 1) * 100) + 1)
    ),
    
    # Passo 2: Applichiamo il Min-Max escludendo gli 0 e i NA dal calcolo di min/max
    Ram_innovation_index_log_norm = case_when(
      is.na(log_temp) ~ NA_real_,
      log_temp == 0   ~ 0,
      log_temp > 0    ~ (log_temp - 0) / 
        (max(log_temp[log_temp > 0], na.rm = TRUE) - 0) * 100
    )
  )

ram_innovation_df <- ram_df |>
  select(-log_temp, -Ram_innovation_index)

saveRDS(ram_innovation_df, "Ram Innovation Index.rds")

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