library(tidyverse)
library(ggplot2)

phones <- readRDS("dataset definitivo.rds")
ai_df <- readRDS("AI Innovation Index.rds")
battery_df <- readRDS("Battery Innovation Index.rds")
camera_df <- readRDS("Camera Innovation Index.rds")
connectivity_df <- readRDS("Connectivity Innovation Index.rds")
cpu_df <- readRDS("CPU Innovation Index.rds")
display_df <- readRDS("Display Innovation Index.rds")
gpu_df <- readRDS("GPU Innovation Index.rds")
ram_df <- readRDS("Ram Innovation Index.rds")

innovation_df <- phones |>
  select(Brand, Model, model_std, Released.Year)

lista_df <- list(
  ai_df %>% select(model_std, AI_Innovation_log_norm),
  cpu_df %>% select(model_std, CPU_Innovation_log_norm),
  ram_df %>% select(model_std, Ram_innovation_index_log_norm),
  camera_df %>% select(model_std, Camera_Innovation_log_norm),
  battery_df %>% select(model_std, Battery_Innovation_log_norm),
  display_df %>% select(model_std, Display_Innovation_Index_log_norm),
  connectivity_df %>% select(model_std, Bluetooth_Innovation_Factor_log_norm)
)

innovation_df <- lista_df %>% 
  reduce(left_join, by = "model_std") %>% 
  left_join(phones, ., by = "model_std")

gpu_clean <- gpu_df %>%
  select(Model, GPU_Innovation_log_norm)

innovation_df <- innovation_df %>% 
  left_join(gpu_clean, by = "Model")

innovation_df <- innovation_df |>
  filter(Released.Year == "2024") |>
  select(Brand, Model, model_std, Released.Year, AI_Innovation_log_norm,
         CPU_Innovation_log_norm, Ram_innovation_index_log_norm, 
         GPU_Innovation_log_norm, Camera_Innovation_log_norm,
         Battery_Innovation_log_norm, Display_Innovation_Index_log_norm,
         Bluetooth_Innovation_Factor_log_norm)

saveRDS(innovation_df, "Dataset completo con Innovation Score.rds")

# Numero di smartphone con estensione positiva per ciascun indice
c(
  AI = sum(innovation_df$AI_Innovation_log_norm > 1, na.rm = TRUE),
  CPU = sum(innovation_df$CPU_Innovation_log_norm > 0, na.rm = TRUE),
  RAM = sum(innovation_df$Ram_innovation_index_log_norm > 1, na.rm = TRUE),
  GPU = sum(innovation_df$GPU_Innovation_log_norm > 1, na.rm = TRUE),
  Camera = sum(innovation_df$Camera_Innovation_log_norm > 1, na.rm = TRUE),
  Battery = sum(innovation_df$Battery_Innovation_log_norm > 1, na.rm = TRUE),
  Display = sum(innovation_df$Display_Innovation_Index_log_norm > 1, na.rm = TRUE),
  Bluetooth = sum(innovation_df$Bluetooth_Innovation_Factor_log_norm > 1, na.rm = TRUE)
)

# Valori di estensione positiva per ciascuna componente

list(
  AI = sort(innovation_df$AI_Innovation_log_norm[
    innovation_df$AI_Innovation_log_norm > 1
  ]),
  
  CPU = sort(innovation_df$CPU_Innovation_log_norm[
    innovation_df$CPU_Innovation_log_norm > 0
  ]),
  
  RAM = sort(innovation_df$Ram_innovation_index_log_norm[
    innovation_df$Ram_innovation_index_log_norm > 1
  ]),
  
  GPU = sort(innovation_df$GPU_Innovation_log_norm[
    innovation_df$GPU_Innovation_log_norm > 1
  ]),
  
  Camera = sort(innovation_df$Camera_Innovation_log_norm[
    innovation_df$Camera_Innovation_log_norm > 1
  ]),
  
  Battery = sort(innovation_df$Battery_Innovation_log_norm[
    innovation_df$Battery_Innovation_log_norm > 1
  ]),
  
  Display = sort(innovation_df$Display_Innovation_Index_log_norm[
    innovation_df$Display_Innovation_Index_log_norm > 1
  ]),
  
  Bluetooth = sort(innovation_df$Bluetooth_Innovation_Factor_log_norm[
    innovation_df$Bluetooth_Innovation_Factor_log_norm > 1
  ])
)

# Valutazione dei NA
subindices <- c(
  "AI_Innovation_log_norm",
  "CPU_Innovation_log_norm",
  "Ram_innovation_index_log_norm",
  "GPU_Innovation_log_norm",
  "Camera_Innovation_log_norm",
  "Battery_Innovation_log_norm",
  "Display_Innovation_Index_log_norm",
  "Bluetooth_Innovation_Factor_log_norm"
)

sapply(innovation_df[subindices], function(x) {
  c(
    N = length(x),
    Valid = sum(!is.na(x)),
    Missing = sum(is.na(x)),
    Zero = sum(x == 0, na.rm = TRUE),
    Positive = sum(x > 0, na.rm = TRUE)
  )
})

# Completiamo i missing
innovation_df$AI_Innovation_log_norm[
  is.na(innovation_df$AI_Innovation_log_norm)
] <- 0

# Controllo finale
check_indices <- sapply(innovation_df[subindices], function(x) {
  c(
    N = length(x),
    Missing = sum(is.na(x)),
    Zero = sum(x == 0, na.rm = TRUE),
    Positive = sum(x > 0, na.rm = TRUE),
    Min = min(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE),
    Mean = mean(x, na.rm = TRUE),
    Median = median(x, na.rm = TRUE)
  )
})
check_indices

# CALCOLO INDICE LATO AZIENDA
innovation_df$Innovation_Score <- rowMeans(
  innovation_df[subindices]
)

# CONTROLLO SULL'INDICE LATO AZIENDA
summary(innovation_df$Innovation_Score)

c(
  N = sum(!is.na(innovation_df$Innovation_Score)),
  Missing = sum(is.na(innovation_df$Innovation_Score)),
  Min = min(innovation_df$Innovation_Score, na.rm = TRUE),
  Max = max(innovation_df$Innovation_Score, na.rm = TRUE),
  Mean = mean(innovation_df$Innovation_Score, na.rm = TRUE),
  Median = median(innovation_df$Innovation_Score, na.rm = TRUE),
  SD = sd(innovation_df$Innovation_Score, na.rm = TRUE)
)

quantile(
  innovation_df$Innovation_Score,
  probs = c(0, 0.10, 0.25, 0.50, 0.75, 0.90, 1),
  na.rm = TRUE
)

top20 <- order(
  innovation_df$Innovation_Score,
  decreasing = TRUE
)[1:20]

innovation_df[top20, c(
  "Brand",
  "Model",
  "Innovation_Score"
)]

#######################
## GRAFICI ############
#######################

# Istogramma di prova
ggplot(innovation_df, aes(x = Innovation_Score)) +
  geom_histogram(
    bins = 25,
    boundary = 0,
    color = "white"
  ) +
  labs(
    title = "Distribuzione dell'Innovation Score",
    x = "Innovation Score",
    y = "Numero di smartphone"
  ) +
  theme_minimal()

# Strip plot
ggplot(
  innovation_df,
  aes(
    x = Innovation_Score,
    y = 0,
    color = Innovation_Score > 0
  )
) +
  geom_jitter(
    height = 0.16,
    width = 0,
    size = 2.2,
    alpha = 0.65
  ) +
  scale_color_manual(
    values = c(
      "FALSE" = "grey50",
      "TRUE" = "#D95F02"
    ),
    labels = c(
      "FALSE" = "Non innovatore (Score = 0)",
      "TRUE" = "Innovatore (Score > 0)"
    ),
    name = NULL
  ) +
  scale_y_continuous(
    limits = c(-0.3, 0.3),
    breaks = NULL
  ) +
  scale_x_continuous(
    limits = c(0, 72),
    breaks = seq(0, 70, 10)
  ) +
  labs(
    title = "Distribuzione dell'Innovation Score",
    subtitle = "Ogni punto rappresenta uno smartphone del campione (n = 172)",
    x = "Innovation Score",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      size = 17,
      face = "bold"
    ),
    plot.subtitle = element_text(
      size = 11,
      color = "grey40"
    ),
    axis.title.x = element_text(
      size = 12,
      margin = margin(t = 10)
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.text = element_text(size = 10),
    plot.margin = margin(10, 15, 10, 10)
  )

# Aggiustamenti delle incongruenze dei brand
innovation_df$Brand[
  innovation_df$Brand == "Oneplus"
] <- "OnePlus"

innovation_df$Brand[
  innovation_df$Brand == "BBK"
] <- "Vivo"

brand_summary <- innovation_df |>
  dplyr::group_by(Brand) |>
  dplyr::summarise(
    N = dplyr::n(),
    Mean_Innovation = mean(Innovation_Score),
    Median_Innovation = median(Innovation_Score),
    .groups = "drop"
  ) |>
  dplyr::arrange(desc(Mean_Innovation))

brand_summary

# Score medio per brand
brand_plot_positive <- brand_summary |>
  dplyr::filter(Mean_Innovation > 0) |>
  dplyr::mutate(
    Brand_label = paste0(Brand, " (N=", N, ")"),
    Brand_label = factor(
      Brand_label,
      levels = rev(Brand_label)
    )
  )

ggplot(
  brand_plot_positive,
  aes(
    x = Mean_Innovation,
    y = Brand_label
  )
) +
  geom_col(
    width = 0.65,
    fill = "#D95F02"
  ) +
  geom_text(
    aes(
      label = sprintf("%.1f", Mean_Innovation)
    ),
    hjust = -0.15,
    size = 3.5
  ) +
  scale_x_continuous(
    limits = c(0, 21),
    breaks = seq(0, 20, 5),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "Innovation Score medio per brand",
    subtitle = "Brand con almeno uno smartphone innovatore",
    x = "Innovation Score medio",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(
      size = 16,
      face = "bold",
      hjust = 0.5
    ),
    
    plot.subtitle = element_text(
      size = 10.5,
      color = "grey40",
      hjust = 0.5
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 9),
    axis.title.x = element_text(
    hjust = 0.5,
    margin = margin(t = 10))
  )

# Quota di smartphone innovatori per brand
brand_diffusion <- innovation_df |>
  dplyr::group_by(Brand) |>
  dplyr::summarise(
    N = dplyr::n(),
    Innovators = sum(Innovation_Score > 0),
    Innovation_Rate = 100 * Innovators / N,
    .groups = "drop"
  ) |>
  dplyr::arrange(desc(Innovation_Rate))

brand_diffusion

brand_diffusion_positive <- brand_diffusion |>
  dplyr::filter(Innovators > 0) |>
  dplyr::mutate(
    Brand_label = paste0(Brand, " (N=", N, ")"),
    Brand_label = factor(
      Brand_label,
      levels = rev(Brand_label)
    )
  )

ggplot(
  brand_diffusion_positive,
  aes(
    x = Innovation_Rate,
    y = Brand_label
  )
) +
  geom_segment(
    aes(
      x = 0,
      xend = Innovation_Rate,
      y = Brand_label,
      yend = Brand_label
    ),
    linewidth = 0.9,
    color = "grey75"
  ) +
  geom_point(
    size = 4,
    color = "#D95F02"
  ) +
  geom_text(
    aes(
      label = sprintf("%.1f%%", Innovation_Rate)
    ),
    hjust = -0.2,
    size = 3.4
  ) +
  scale_x_continuous(
    limits = c(0, 108),
    breaks = seq(0, 100, 20),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Diffusione dell'innovazione per brand",
    subtitle = "Quota di smartphone innovatori tra i brand con almeno un innovatore",
    x = "Smartphone innovatori (%)",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title.position = "plot",
    
    plot.title = element_text(
      size = 16,
      face = "bold",
      hjust = 0.5
    ),
    
    plot.subtitle = element_text(
      size = 10.5,
      color = "grey40",
      hjust = 0.5
    ),
    
    axis.title.x = element_text(
      size = 12,
      hjust = 0.5
    ),
    
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 9),
    
    plot.margin = margin(10, 25, 10, 10)
  )

# Heatmap sui sottoindici
heatmap_data_positive <- heatmap_data |>
  dplyr::filter(Brand %in% brand_order[brand_order %in% 
                                         unique(brand_summary$Brand[
                                           brand_summary$Mean_Innovation > 0
                                         ])])

heatmap_data_positive <- heatmap_data_positive |>
  dplyr::mutate(
    Technology = factor(
      Technology,
      levels = c(
        "AI",
        "CPU",
        "RAM",
        "GPU",
        "Camera",
        "Battery",
        "Display",
        "Bluetooth"
      ),
      labels = c(
        "AI",
        "CPU",
        "RAM",
        "GPU",
        "Camera",
        "Batteria",
        "Display",
        "Bluetooth"
      )
    )
  )

ggplot(
  heatmap_data_positive,
  aes(
    x = Technology,
    y = Brand,
    fill = Innovation
  )
) +
  geom_tile(
    color = "white",
    linewidth = 0.5
  ) +
  scale_fill_gradient(
    low = "white",
    high = "#D95F02",
    limits = c(0, 100),
    breaks = c(0, 25, 50, 75, 100)
  ) +
  labs(
    title = "Profilo di innovazione tecnologica per brand",
    subtitle = "Innovation Score medio per ciascuna dimensione tecnologica",
    x = "Dimensione tecnologica",
    y = NULL,
    fill = "Innovation Score"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title.position = "plot",
    
    plot.title = element_text(
      size = 16,
      face = "bold",
      hjust = 0.5
    ),
    
    plot.subtitle = element_text(
      size = 10.5,
      color = "grey40",
      hjust = 0.5
    ),
    
    axis.text.x = element_text(
      size = 10,
      angle = 35,
      hjust = 1
    ),
    
    axis.text.y = element_text(
      size = 10
    ),
    
    axis.title.x = element_text(
      size = 12,
      hjust = 0.5,
      margin = margin(t = 12)
    ),
    
    panel.grid = element_blank(),
    
    legend.position = "right",
    
    plot.margin = margin(10, 15, 10, 15)
  )

# Dimensioni che contribuiscono ad un Innovation Score più alto
subindex_means <- innovation_df |>
  dplyr::summarise(
    AI = mean(AI_Innovation_log_norm),
    CPU = mean(CPU_Innovation_log_norm),
    RAM = mean(Ram_innovation_index_log_norm),
    GPU = mean(GPU_Innovation_log_norm),
    Camera = mean(Camera_Innovation_log_norm),
    Battery = mean(Battery_Innovation_log_norm),
    Display = mean(Display_Innovation_Index_log_norm),
    Bluetooth = mean(Bluetooth_Innovation_Factor_log_norm)
  ) |>
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "Technology",
    values_to = "Mean_Innovation"
  ) |>
  dplyr::arrange(desc(Mean_Innovation))

subindex_means

ggplot(
  subindex_means,
  aes(
    x = Mean_Innovation,
    y = reorder(Technology, Mean_Innovation)
  )
) +
  geom_col(
    width = 0.65,
    fill = "#D95F02"
  ) +
  geom_text(
    aes(
      label = sprintf("%.2f", Mean_Innovation)
    ),
    hjust = -0.15,
    size = 3.5
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Innovazione media per dimensione tecnologica",
    subtitle = "Media dei sottoindici sui 172 smartphone del campione",
    x = "Innovation Score medio",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title.position = "plot",
    
    plot.title = element_text(
      size = 16,
      face = "bold",
      hjust = 0.5
    ),
    
    plot.subtitle = element_text(
      size = 10.5,
      color = "grey40",
      hjust = 0.5
    ),
    
    axis.title.x = element_text(
      size = 12,
      hjust = 0.5
    ),
    
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 9),
    
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    
    plot.margin = margin(10, 20, 10, 10)
  )

# In quanti smartphone vi sono innovazioni tecnologiche per dimensione
subindex_diffusion <- innovation_df |>
  dplyr::summarise(
    AI = sum(AI_Innovation_log_norm > 0),
    CPU = sum(CPU_Innovation_log_norm > 0),
    RAM = sum(Ram_innovation_index_log_norm > 0),
    GPU = sum(GPU_Innovation_log_norm > 0),
    Camera = sum(Camera_Innovation_log_norm > 0),
    Battery = sum(Battery_Innovation_log_norm > 0),
    Display = sum(Display_Innovation_Index_log_norm > 0),
    Bluetooth = sum(Bluetooth_Innovation_Factor_log_norm > 0)
  ) |>
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "Technology",
    values_to = "Innovators"
  ) |>
  dplyr::mutate(
    Percentage = 100 * Innovators / nrow(innovation_df)
  ) |>
  dplyr::arrange(desc(Innovators))

subindex_diffusion

ggplot(
  subindex_diffusion,
  aes(
    x = Innovators,
    y = reorder(Technology, Innovators)
  )
) +
  geom_col(
    width = 0.65,
    fill = "#D95F02"
  ) +
  geom_text(
    aes(
      label = paste0(
        Innovators,
        " (",
        sprintf("%.1f%%", Percentage),
        ")"
      )
    ),
    hjust = -0.15,
    size = 3.5
  ) +
  scale_x_continuous(
    breaks = seq(0, 40, 10),
    limits = c(0, 44),
    expand = expansion(mult = c(0, 0.01))
  ) +
  labs(
    title = "Diffusione dell'innovazione per dimensione tecnologica",
    subtitle = "Numero e quota di smartphone con estensione positiva della frontiera",
    x = "Smartphone innovatori",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title.position = "plot",
    
    plot.title = element_text(
      size = 16,
      face = "bold",
      hjust = 0.5
    ),
    
    plot.subtitle = element_text(
      size = 10.5,
      color = "grey40",
      hjust = 0.5
    ),
    
    axis.title.x = element_text(
      size = 12,
      hjust = 0.5
    ),
    
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 9),
    
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    
    plot.margin = margin(
      t = 10,
      r = 35,
      b = 10,
      l = 10
    )
  )

# Grafico per top 21

top21_plot <- innovation_df |>
  dplyr::arrange(dplyr::desc(Innovation_Score)) |>
  dplyr::slice_head(n = 25) |>
  dplyr::mutate(
    Model_clean = Model |>
      stringr::str_remove("\\s+5G.*$") |>
      stringr::str_remove("\\s+Premium Edition.*$") |>
      stringr::str_remove("\\s+Standard Edition.*$") |>
      stringr::str_remove("\\s+Top Edition.*$") |>
      stringr::str_remove("\\s+Edition.*$") |>
      stringr::str_trim()
  )

top21_plot <- top21_plot |>
  dplyr::mutate(
    Model_clean = dplyr::case_when(
      Brand == "Huawei" & stringr::str_detect(Model_clean, "Mate XT") ~ "Mate XT Ultimate",
      Brand == "Huawei" & stringr::str_detect(Model_clean, "Pura 70 Ultra") ~ "Pura 70 Ultra",
      Brand == "Huawei" & stringr::str_detect(Model_clean, "Pura 70 Pro") ~ "Pura 70 Pro",
      Brand == "Huawei" & stringr::str_detect(Model_clean, "Pura 70 Pro+") ~ "Pura 70 Pro+",
      .default = Model_clean
    )
  )

top21_plot <- top21_plot |>
  dplyr::mutate(
    Model_clean = stringr::str_remove(
      Model_clean,
      regex(paste0("^", Brand, "\\s+"), ignore_case = TRUE)
    ),
    Label = paste(Brand, Model_clean, sep = " — ")
  )

top21_plot <- top21_plot |>
  dplyr::mutate(
    Label = dplyr::case_when(
      Brand == "Asus" & stringr::str_detect(Model_clean, "ROG Phone 9 Pro") ~ "Asus — ROG Phone 9 Pro (Edition)",
      Brand == "Apple" & Model_clean %in% c("iPhone 16", "iPhone 16 Plus") ~ "Apple — iPhone 16 (Plus)",
      Brand == "Apple" & Model_clean %in% c("iPhone 16 Pro", "iPhone 16 Pro Max") ~ "Apple — iPhone 16 Pro (Max)",
      Brand == "Huawei" & Model_clean %in% c("Pura 70 Pro", "Pura 70 Pro+") ~ "Huawei — Pura 70 Pro (+)",
      .default = Label
    )
  ) |>
  dplyr::group_by(Label) |>
  dplyr::summarise(
    Innovation_Score = dplyr::first(Innovation_Score),
    Brand = dplyr::first(Brand), 
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(Innovation_Score)) |>
  dplyr::slice_head(n = 21)

# Ordine dal più innovativo al meno innovativo
top21_plot <- top21_plot |>
  arrange(Innovation_Score) |>
  mutate(
    Label = factor(Label, levels = Label)
  )

# Grafico Top 21
ggplot(top21_plot, aes(x = Innovation_Score, y = Label)) +
  geom_col(fill = "orange", width = 0.7) +
  geom_text(
    aes(label = sprintf("%.1f", Innovation_Score)),
    hjust = -0.15,
    size = 3.5
  ) +
  scale_x_continuous(
    limits = c(0, max(top21_plot$Innovation_Score) * 1.10),
    expand = c(0, 0)
  ) +
  labs(
    title = "Top 21 smartphone per Innovation Score",
    subtitle = "I migliori 21 smartphone ordinati per Innovation Score",
    x = "Innovation Score",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 15
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      color = "grey40",
      size = 11
    ),
    axis.title.x = element_text(
      hjust = 0.5
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 10)
  )

# Boxplot per brand
brand_boxplot <- innovation_df |>
  group_by(Brand) |>
  mutate(
    N = n(),
    Brand_label = paste0(Brand, " (N=", N, ")")
  ) |>
  ungroup() |>
  mutate(
    Brand_label = reorder(Brand_label, Innovation_Score, FUN = median)
  )

ggplot(
  brand_boxplot,
  aes(x = Innovation_Score, y = Brand_label)
) +
  geom_boxplot(
    fill = "orange",
    alpha = 0.7,
    width = 0.65,
    outlier.shape = 16
  ) +
  geom_jitter(
    height = 0.10,
    width = 0,
    alpha = 0.35,
    size = 1.5
  ) +
  labs(
    title = "Distribuzione dell'Innovation Score per brand",
    subtitle = "Variabilità dell'innovazione tecnologica tra i brand",
    x = "Innovation Score",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 15
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      size = 11
    ),
    axis.title.x = element_text(
      hjust = 0.5
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

###################
# Kruskal wallis #
##################
kruskal.test(Innovation_Score ~ Brand, data = innovation_df)


# Test di robustezza dell'indice

robustness_results <- lapply(subindices, function(dim) {
  
  remaining <- setdiff(subindices, dim)
  
  score_loo <- rowMeans(
    innovation_df[, remaining],
    na.rm = FALSE
  )
  
  cor_test <- cor.test(
    innovation_df$Innovation_Score,
    score_loo,
    method = "spearman",
    exact = FALSE
  )
  
  data.frame(
    Excluded_Dimension = dim,
    Spearman_Rho = unname(cor_test$estimate),
    P_Value = cor_test$p.value
  )
}) |>
  bind_rows()

robustness_results

saveRDS(innovation_df, "Indice di Innovazione complessivo per i 172 dispositivi.rds")

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
