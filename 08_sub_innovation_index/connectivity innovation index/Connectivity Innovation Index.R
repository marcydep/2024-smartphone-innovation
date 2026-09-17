library(tidyverse)
library(DescTools)

phones <- readRDS("dataset definitivo.rds")

connectivity_overview <- phones %>%
  filter(Released.Year %in% c(2023, 2024)) %>%
  select(
    Released.Year,
    bluetooth_version,
    wifi_standard,
    nfc_support,
    felica_support,
    usb_type,
    usb_pd_version
  )

connectivity_overview %>%
  summarise(
    bluetooth_missing = sum(is.na(bluetooth_version)),
    wifi_missing = sum(is.na(wifi_standard)),
    nfc_missing = sum(is.na(nfc_support)),
    felica_missing = sum(is.na(felica_support)),
    usb_type_missing = sum(is.na(usb_type)),
    usb_pd_missing = sum(is.na(usb_pd_version))
  )

connectivity_overview %>%
  count(Released.Year, bluetooth_version) %>%
  arrange(Released.Year, bluetooth_version)

connectivity_overview %>%
  count(Released.Year, wifi_standard) %>%
  arrange(Released.Year, wifi_standard)

connectivity_overview %>%
  count(Released.Year, nfc_support) %>%
  arrange(Released.Year, nfc_support)

connectivity_overview %>%
  count(Released.Year, felica_support) %>%
  arrange(Released.Year, felica_support)

connectivity_overview %>%
  count(Released.Year, usb_type) %>%
  arrange(Released.Year, usb_type)

connectivity_overview %>%
  count(Released.Year, usb_pd_version) %>%
  arrange(Released.Year, usb_pd_version)

# Test statistici
connectivity_tests <- list(
  
  bluetooth = table(
    phones$Released.Year,
    phones$bluetooth_version
  ),
  
  wifi = table(
    phones$Released.Year,
    phones$wifi_standard
  ),
  
  nfc = table(
    phones$Released.Year,
    phones$nfc_support
  ),
  
  felica = table(
    phones$Released.Year,
    phones$felica_support
  ),
  
  usb = table(
    phones$Released.Year,
    phones$usb_type
  ),
  
  usb_pd = table(
    phones$Released.Year,
    phones$usb_pd_version
  )
)

lapply(
  connectivity_tests,
  chisq.test
)

phones %>%
  filter(Released.Year %in% c(2023, 2024)) %>%
  count(Released.Year, bluetooth_version) %>%
  group_by(Released.Year) %>%
  mutate(
    percentage = n / sum(n) * 100
  ) %>%
  ungroup() %>%
  arrange(bluetooth_version, Released.Year)

# Indice
phones <- phones %>%
  mutate(
    Bluetooth_Innovation_Factor = case_when(
      Released.Year == 2024 & bluetooth_version == 6 ~ 1.7078,
      Released.Year == 2024 ~ 1,
      TRUE ~ NA_real_
    )
  )

connectivity_index <- phones |>
  filter(Released.Year == "2024") |>
  select(Brand,
         Model,
         model_std,
         Released.Year,
         Bluetooth_Innovation_Factor)

# Normalizzazione
connectivity_index <- connectivity_index |>
  mutate(
    # Passo 1: Calcoliamo il logaritmo solo dove Bluetooth_Innovation_Factor > 0
    log_temp = case_when(
      is.na(Bluetooth_Innovation_Factor) ~ NA_real_,
      Bluetooth_Innovation_Factor == 0   ~ 0,
      Bluetooth_Innovation_Factor > 0    ~ log(((Bluetooth_Innovation_Factor - 1) * 100) + 1)
    ),
    
    # Passo 2: Applichiamo il Min-Max escludendo gli 0 e i NA dal calcolo di min/max
    Bluetooth_Innovation_Factor_log_norm = case_when(
      is.na(log_temp) ~ NA_real_,
      log_temp == 0   ~ 0,
      log_temp > 0    ~ (log_temp - 0) / 
        (max(log_temp[log_temp > 0], na.rm = TRUE) - 0) * 100
    )
  )

connectivity_innovation_df <- connectivity_index |>
  select(-log_temp, -Bluetooth_Innovation_Factor)

saveRDS(connectivity_innovation_df, "Connectivity Innovation Index.rds")

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