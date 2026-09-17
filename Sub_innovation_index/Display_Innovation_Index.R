library(tidyverse)

phones <- readRDS("dataset definitivo.rds")

phones$F_PWM <- 1

phones$F_PWM[phones$Model == "Honor Magic 6 Pro"] <- 1.125
phones$F_PWM[phones$Model == "Honor Magic V3"] <- 1.125

phones$Display_Innovation_Index <- phones$F_PWM

display_df <- phones |>
  filter(Released.Year == "2024") |>
  select(
    Brand,
    Model,
    model_std,
    Released.Year,
    Display_Innovation_Index
  )

# Normalizzazione
display_df <- display_df |>
  mutate(
    # Passo 1: Calcoliamo il logaritmo solo dove Display_Innovation_Index > 0
    log_temp = case_when(
      is.na(Display_Innovation_Index) ~ NA_real_,
      Display_Innovation_Index == 0   ~ 0,
      Display_Innovation_Index > 0    ~ log(((Display_Innovation_Index - 1) * 100) + 1)
    ),
    
    # Passo 2: Applichiamo il Min-Max escludendo gli 0 e i NA dal calcolo di min/max
    Display_Innovation_Index_log_norm = case_when(
      is.na(log_temp) ~ NA_real_,
      log_temp == 0   ~ 0,
      log_temp > 0    ~ (log_temp - 0) / 
        (max(log_temp[log_temp > 0], na.rm = TRUE) - 0) * 100
    )
  )

display_innovation_df <- display_df |>
  select(-log_temp, -Display_Innovation_Index)

saveRDS(display_innovation_df, "Display Innovation Index.rds")

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