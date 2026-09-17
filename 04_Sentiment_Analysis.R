library(tidyverse)
library(ggplot2)
library(scales)

sentiment <- read.csv("amazon_reviews_sentiment.csv", stringsAsFactors = FALSE,
                       na.strings = c(""))

innovation_df <- readRDS("Indice di Innovazione complessivo per i 172 dispositivi.rds")

sentiment <- sentiment %>%
  mutate(
    across(
      c(performance, battery, camera, display),
      ~ na_if(.x, "NA")
    )
  )

sentiment <- sentiment %>%
  mutate(
    across(
      c(performance, battery, camera, display, overall),
      as.numeric
    )
  )

# Analisi descrittiva
aspect_summary <- sentiment %>%
  summarise(
    Performance = sum(!is.na(performance)),
    Battery = sum(!is.na(battery)),
    Camera = sum(!is.na(camera)),
    Display = sum(!is.na(display))
  ) %>%
  pivot_longer(
    everything(),
    names_to = "Aspect",
    values_to = "N"
  ) %>%
  mutate(
    Mention_Rate = N / nrow(sentiment)
  )

aspect_summary

# Media
sentiment_means <- sentiment %>%
  summarise(
    Performance = mean(performance, na.rm = TRUE),
    Battery = mean(battery, na.rm = TRUE),
    Camera = mean(camera, na.rm = TRUE),
    Display = mean(display, na.rm = TRUE),
    Overall = mean(overall, na.rm = TRUE)
  ) %>%
  pivot_longer(
    everything(),
    names_to = "Aspect",
    values_to = "Mean_Sentiment"
  )

sentiment_means

# Tabella descrittiva per smartphone
smartphone_descriptive <- sentiment %>%
  group_by(Smartphone) %>%
  summarise(
    Reviews = n(),
    
    Performance_N = sum(!is.na(performance)),
    Performance_Mean = mean(performance, na.rm = TRUE),
    
    Battery_N = sum(!is.na(battery)),
    Battery_Mean = mean(battery, na.rm = TRUE),
    
    Camera_N = sum(!is.na(camera)),
    Camera_Mean = mean(camera, na.rm = TRUE),
    
    Display_N = sum(!is.na(display)),
    Display_Mean = mean(display, na.rm = TRUE),
    
    Overall_Mean = mean(overall, na.rm = TRUE),
    
    .groups = "drop"
  )

smartphone_descriptive

# Distribuzione
sentiment_distribution <- sentiment %>%
  select(performance, battery, camera, display, overall) %>%
  pivot_longer(
    everything(),
    names_to = "Aspect",
    values_to = "Sentiment"
  ) %>%
  mutate(
    Sentiment = ifelse(is.na(Sentiment), NA, Sentiment)
  ) %>%
  filter(!is.na(Sentiment)) %>%
  count(Aspect, Sentiment) %>%
  group_by(Aspect) %>%
  mutate(
    Percentage = n / sum(n)
  ) %>%
  ungroup()

sentiment_distribution

# Quanto varia il sentiment tra gli smartphone?
smartphone_variability <- smartphone_descriptive %>%
  summarise(
    across(
      c(Performance_Mean, Battery_Mean, Camera_Mean,
        Display_Mean, Overall_Mean),
      list(
        Mean = ~ mean(.x, na.rm = TRUE),
        SD = ~ sd(.x, na.rm = TRUE),
        Min = ~ min(.x, na.rm = TRUE),
        Max = ~ max(.x, na.rm = TRUE)
      )
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to = c("Aspect", "Statistic"),
    names_sep = "_(?=[^_]+$)",
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from = Statistic,
    values_from = Value
  )

smartphone_variability

# Figura per il mention rate
fig1 <- aspect_summary %>%
  mutate(
    Aspect = factor(
      Aspect,
      levels = c("Performance", "Camera", "Battery", "Display")
    )
  ) %>%
  ggplot(aes(x = Aspect, y = Mention_Rate)) +
  geom_col(width = 0.65, fill="orange") +
  geom_text(
    aes(label = percent(Mention_Rate, accuracy = 0.1)),
    vjust = -0.4,
    size = 4
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 0.50),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = "La rilevanza degli attributi degli smartphone nelle recensioni dei consumatori",
    subtitle = "Quota di recensioni che menzionano ciascuna dimensione del prodotto",
    x = NULL,
    y = "Quota di recensioni"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    ),
    plot.subtitle = element_text(
      hjust = 0.5
    ),
    plot.title.position = "plot",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

# Score percepiti per componenti per ogni smartphone
perceived_scores <- smartphone_descriptive %>%
  transmute(
    Smartphone,
    Performance_Perceived = (Performance_Mean + 2) / 4 * 100,
    Battery_Perceived     = (Battery_Mean + 2) / 4 * 100,
    Camera_Perceived      = (Camera_Mean + 2) / 4 * 100,
    Display_Perceived     = (Display_Mean + 2) / 4 * 100,
    CPI = (Overall_Mean + 2) / 4 * 100
  )

perceived_scores

# Distribuzione e variabilità dei nuovi indicatori
perceived_summary <- perceived_scores %>%
  summarise(
    Performance_Mean = mean(Performance_Perceived, na.rm = TRUE),
    Performance_SD   = sd(Performance_Perceived, na.rm = TRUE),
    Performance_Min  = min(Performance_Perceived, na.rm = TRUE),
    Performance_Max  = max(Performance_Perceived, na.rm = TRUE),
    
    Battery_Mean = mean(Battery_Perceived, na.rm = TRUE),
    Battery_SD   = sd(Battery_Perceived, na.rm = TRUE),
    Battery_Min  = min(Battery_Perceived, na.rm = TRUE),
    Battery_Max  = max(Battery_Perceived, na.rm = TRUE),
    
    Camera_Mean = mean(Camera_Perceived, na.rm = TRUE),
    Camera_SD   = sd(Camera_Perceived, na.rm = TRUE),
    Camera_Min  = min(Camera_Perceived, na.rm = TRUE),
    Camera_Max  = max(Camera_Perceived, na.rm = TRUE),
    
    Display_Mean = mean(Display_Perceived, na.rm = TRUE),
    Display_SD   = sd(Display_Perceived, na.rm = TRUE),
    Display_Min  = min(Display_Perceived, na.rm = TRUE),
    Display_Max  = max(Display_Perceived, na.rm = TRUE),
    
    CPI_Mean = mean(CPI, na.rm = TRUE),
    CPI_SD   = sd(CPI, na.rm = TRUE),
    CPI_Min  = min(CPI, na.rm = TRUE),
    CPI_Max  = max(CPI, na.rm = TRUE)
  )

perceived_summary

summary(perceived_scores)


# Dispersione PIS
perceived_scores %>%
  summarise(
    Performance_SD = sd(Performance_Perceived),
    Battery_SD = sd(Battery_Perceived),
    Camera_SD = sd(Camera_Perceived),
    Display_SD = sd(Display_Perceived),
    CPI_SD = sd(CPI)
  )

# Technical Category Score
technical_scores <- innovation_df %>%
  transmute(
    model_std,
    
    Performance_Technical = rowMeans(
      cbind(
        AI_Innovation_log_norm,
        CPU_Innovation_log_norm,
        Ram_innovation_index_log_norm,
        GPU_Innovation_log_norm
      ),
      na.rm = TRUE
    ),
    
    Camera_Technical = Camera_Innovation_log_norm,
    Battery_Technical = Battery_Innovation_log_norm,
    Display_Technical = Display_Innovation_Index_log_norm,
    
    Innovation_Score
    )

technical_scores

# Model mapping
model_mapping <- c(
  "Galaxy A35" = "galaxy a35 5g",
  "Galaxy M55" = "galaxy m55 5g",
  "Galaxy S24" = "galaxy s24 5g",
  "Galaxy S24 ultra" = "galaxy s24 ultra 5g",
  "Google Pixel 8a" = "pixel 8a 5g",
  "Google Pixel 9" = "pixel 9 5g",
  "Google Pixel 9 Pro" = "pixel 9 pro 5g",
  "Honor 200" = "200 5g",
  "Honor 200 Pro" = "200 pro 5g",
  "Motorola Edge 50 Fusion" = "moto edge 50 fusion 5g",
  "Motorola Edge 50 Neo" = "moto edge 50 neo 5g",
  "Motorole Edge 50 Ultra" = "moto edge 50 ultra 5g",
  "Nothing Phone (2a)" = "phone (2a) 5g",
  "Oneplus 12r" = "12r 5g",
  "Oneplus 13" = "13 5g",
  "Poco F6" = "poco f6 5g",
  "ROG Phone 9 Pro" = "rog phone 9 pro 5g",
  "Realme 12 Pro+" = "12 pro plus 5g",
  "Realme GT6T" = "gt 6t 5g",
  "Samsung A55" = "galaxy a55 5g",
  "Sony Xperia 10 VI" = "xperia 10 vi 5g",
  "Xiaomi 14T" = "mi 14t 5g",
  "Xiaomi 14T Pro" = "mi 14t pro 5g",
  "Xiaomi 15" = "15 5g",
  "Xiaomi Redmi note 14" = "redmi note 14 5g",
  "Xiaomi Redmi note 14 Pro+" = "redmi note 14 pro+ 5g",
  "iPhone 16" = "iphone 16 5g",
  "iPhone 16 Pro" = "iphone 16 pro 5g"
)

technical_scores_28 <- technical_scores %>%
  filter(model_std %in% unname(model_mapping))

# Perceived score associato effettivamente ai 28 smartphone
perceived_scores_43 <- perceived_scores %>%
  mutate(
    model_std = unname(model_mapping[Smartphone])
  ) %>%
  left_join(
    technical_scores_28,
    by = "model_std"
  )

# Correlazioni tecnico-percezione
correlations <- tibble(
  Category = c("Performance", "Camera", "Battery", "Display", "CPI"),
  Pearson = c(
    cor(perceived_scores_43$Performance_Technical,
        perceived_scores_43$Performance_Perceived,
        use = "complete.obs"),
    cor(perceived_scores_43$Camera_Technical,
        perceived_scores_43$Camera_Perceived,
        use = "complete.obs"),
    cor(perceived_scores_43$Battery_Technical,
        perceived_scores_43$Battery_Perceived,
        use = "complete.obs"),
    NA,
    cor(perceived_scores_43$Innovation_Score,
        perceived_scores_43$CPI,
        use = "complete.obs")
  ),
  Spearman = c(
    cor(perceived_scores_43$Performance_Technical,
        perceived_scores_43$Performance_Perceived,
        method = "spearman",
        use = "complete.obs"),
    cor(perceived_scores_43$Camera_Technical,
        perceived_scores_43$Camera_Perceived,
        method = "spearman",
        use = "complete.obs"),
    cor(perceived_scores_43$Battery_Technical,
        perceived_scores_43$Battery_Perceived,
        method = "spearman",
        use = "complete.obs"),
    NA,
    cor(perceived_scores_43$Innovation_Score,
        perceived_scores_43$CPI,
        method = "spearman",
        use = "complete.obs")
  )
)

correlations

# Tabella con media tecnica e media percepita per categoria
comparison_table <- tibble(
  Category = c("Performance", "Camera", "Battery", "Display", "CPI"),
  Technical_Mean = c(
    mean(perceived_scores_43$Performance_Technical),
    mean(perceived_scores_43$Camera_Technical),
    mean(perceived_scores_43$Battery_Technical),
    mean(perceived_scores_43$Display_Technical),
    mean(perceived_scores_43$Innovation_Score)
  ),
  Perceived_Mean = c(
    mean(perceived_scores_43$Performance_Perceived),
    mean(perceived_scores_43$Camera_Perceived),
    mean(perceived_scores_43$Battery_Perceived),
    mean(perceived_scores_43$Display_Perceived),
    mean(perceived_scores_43$CPI)
  )
)

comparison_table

# Correlazione overall + ranking dei 28
overall_comparison <- perceived_scores_43 %>%
  select(
    Smartphone,
    Innovation_Score,
    CPI
  ) %>%
  mutate(
    Innovation_Rank = rank(-Innovation_Score, ties.method = "min"),
    CPI_Rank = rank(-CPI, ties.method = "min"),
    Rank_Difference = Innovation_Rank - CPI_Rank
  ) %>%
  arrange(desc(Innovation_Score))

overall_comparison

cor.test(
  perceived_scores_43$Innovation_Score,
  perceived_scores_43$CPI,
  method = "spearman",
  exact = FALSE
)

# =========================
# Figura 2
# Innovation Score vs CPI
# =========================

fig2 <- ggplot(
  perceived_scores_43,
  aes(x = Innovation_Score, y = CPI)
) +
  geom_point(
    color = "#E67E22",
    size = 3
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "#00FF00",
    linewidth = 0.8
  ) +
  labs(
    title = "Technical Innovation e Percezione dei Consumatori",
    subtitle = "Innovation Score vs Consumer Perception Index",
    x = "Innovation Score",
    y = "Consumer Perception Index (CPI)"
  ) +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

fig2


# =========================
# Figura 2.1
# Performance
# =========================

fig2_1 <- ggplot(
  perceived_scores_43,
  aes(x = Performance_Technical,
      y = Performance_Perceived)
) +
  geom_point(
    color = "#E67E22",
    size = 3
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "#00FF00",
    linewidth = 0.8
  ) +
  labs(
    title = "Performance Tecnica e Percepita",
    subtitle = "Technical Performance vs Consumer Perception",
    x = "Technical Performance",
    y = "Perceived Performance"
  ) +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

fig2_1


# =========================
# Figura 2.2
# Camera
# =========================

fig2_2 <- ggplot(
  perceived_scores_43,
  aes(x = Camera_Technical,
      y = Camera_Perceived)
) +
  geom_point(
    color = "#E67E22",
    size = 3
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "#00FF00",
    linewidth = 0.8
  ) +
  labs(
    title = "Performance Tecnica e Percepita della Camera",
    subtitle = "Technical Camera Innovation vs Consumer Perception",
    x = "Technical Camera Innovation",
    y = "Perceived Camera"
  ) +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

fig2_2


# =========================
# Figura 2.3
# Battery
# =========================

fig2_3 <- ggplot(
  perceived_scores_43,
  aes(x = Battery_Technical,
      y = Battery_Perceived)
) +
  geom_point(
    color = "#E67E22",
    size = 3
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "#00FF00",
    linewidth = 0.8
  ) +
  labs(
    title = "Performance Tecnica e Percepita della Batteria",
    subtitle = "Technical Battery Innovation vs Consumer Perception",
    x = "Technical Battery Innovation",
    y = "Perceived Battery"
  ) +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

fig2_3

# 4.4
innovation_distribution <- perceived_scores_43 %>%
  select(Smartphone, Innovation_Score) %>%
  arrange(Innovation_Score)

innovation_distribution

quantile(
  perceived_scores_43$Innovation_Score,
  probs = c(0, .25, .33, .50, .67, .75, 1),
  na.rm = TRUE
)

summary(perceived_scores_43$Innovation_Score)

# Definizione degli Innovation Groups
innovation_groups <- perceived_scores_43 %>%
  mutate(
    Innovation_Group = case_when(
      Innovation_Score == 0 ~ "Low Innovation",
      Innovation_Score <= 11.174146 ~ "Medium Innovation",
      Innovation_Score > 11.174146 ~ "High Innovation"
    )
  )

innovation_groups %>%
  group_by(Innovation_Group) %>%
  summarise(
    N = n(),
    Mean_CPI = mean(CPI),
    SD_CPI = sd(CPI),
    Median_CPI = median(CPI),
    Min_CPI = min(CPI),
    Max_CPI = max(CPI)
  )

innovation_groups %>%
  arrange(Innovation_Group, Innovation_Score) %>%
  select(
    Smartphone,
    Innovation_Score,
    Innovation_Group,
    CPI
  )

# Distribuzione della CPI nei tre gruppi
ggplot(
  innovation_groups,
  aes(x = Innovation_Group, y = CPI)
) +
  geom_boxplot(
    fill = "#E67E22",
    alpha = 0.7
  ) +
  geom_jitter(
    width = 0.08,
    size = 2.5,
    color = "black"
  ) +
  labs(
    title = "Consumer Perception by Innovation Group",
    subtitle = "CPI distribution across Low, Medium and High Innovation smartphones",
    x = "Innovation Group",
    y = "Consumer Perception Index (CPI)"
  ) +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

anova_model <- aov(CPI ~ Innovation_Group, data = innovation_groups)

summary(anova_model)

shapiro.test(residuals(anova_model))

bartlett.test(CPI ~ Innovation_Group, data = innovation_groups)

# ============================================================
# DATA-DRIVEN CLUSTERING DELL'INNOVATION SCORE
# ============================================================

library(cluster)
library(classInt)

# Dataset: solo Innovation Score dei 28 smartphone
innovation_cluster <- perceived_scores_43 %>%
  select(Smartphone, Innovation_Score) %>%
  arrange(Innovation_Score)

x <- innovation_cluster$Innovation_Score


# ============================================================
# 1. ELBOW METHOD
# ============================================================

set.seed(123)

k_values <- 2:5

elbow_results <- data.frame(
  k = k_values,
  WSS = sapply(k_values, function(k) {
    kmeans(matrix(x, ncol = 1), centers = k, nstart = 100)$tot.withinss
  })
)

elbow_results


ggplot(elbow_results, aes(x = k, y = WSS)) +
  geom_line() +
  geom_point(size = 3) +
  scale_x_continuous(breaks = k_values) +
  labs(
    title = "Elbow Method for Innovation Score",
    subtitle = "Within-cluster variation across different numbers of clusters",
    x = "Number of clusters (k)",
    y = "Total within-cluster sum of squares"
  ) +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )


# ============================================================
# 2. SILHOUETTE METHOD
# ============================================================

silhouette_results <- data.frame(
  k = k_values,
  Silhouette = sapply(k_values, function(k) {
    
    km <- kmeans(
      matrix(x, ncol = 1),
      centers = k,
      nstart = 100
    )
    
    sil <- silhouette(
      km$cluster,
      dist(matrix(x, ncol = 1))
    )
    
    mean(sil[, "sil_width"])
  })
)

silhouette_results


ggplot(silhouette_results, aes(x = k, y = Silhouette)) +
  geom_line() +
  geom_point(size = 3) +
  scale_x_continuous(breaks = k_values) +
  labs(
    title = "Silhouette Analysis for Innovation Score",
    subtitle = "Average silhouette width across different numbers of clusters",
    x = "Number of clusters (k)",
    y = "Average silhouette width"
  ) +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )


# ============================================================
# 3. K-MEANS: k = 2, 3, 4
# ============================================================

set.seed(123)

kmeans_results <- list()

for (k in 2:4) {
  
  km <- kmeans(
    matrix(x, ncol = 1),
    centers = k,
    nstart = 100
  )
  
  kmeans_results[[paste0("k", k)]] <- innovation_cluster %>%
    mutate(
      Cluster = km$cluster
    ) %>%
    arrange(Cluster, Innovation_Score)
  
}


# Visualizzazione delle soluzioni
kmeans_results$k2 |> print(n=100)
kmeans_results$k3 |> print(n=100)
kmeans_results$k4 |> print(n=100)


# ============================================================
# 4. FUNZIONE PER ESTRARRE I RANGHI DEI CLUSTER
# ============================================================

cluster_summary <- function(df) {
  
  df %>%
    group_by(Cluster) %>%
    summarise(
      N = n(),
      Min = min(Innovation_Score),
      Max = max(Innovation_Score),
      Mean = mean(Innovation_Score),
      .groups = "drop"
    ) %>%
    arrange(Mean)
}


cluster_summary(kmeans_results$k2)
cluster_summary(kmeans_results$k3)
cluster_summary(kmeans_results$k4)


# ============================================================
# 5. JENKS NATURAL BREAKS
# ============================================================

jenks_results <- list()

for (k in 2:4) {
  
  jenks <- classInt::classIntervals(
    x,
    n = k,
    style = "jenks"
  )
  
  jenks_results[[paste0("k", k)]] <- jenks
}


# Intervalli individuati da Jenks
jenks_results$k2$brks
jenks_results$k3$brks
jenks_results$k4$brks


# ============================================================
# 6. ASSEGNAZIONE DEI TELEFONI AI CLUSTER JENKS
# ============================================================

jenks_assignment <- function(k) {
  
  jenks <- jenks_results[[paste0("k", k)]]
  
  innovation_cluster %>%
    mutate(
      Cluster = cut(
        Innovation_Score,
        breaks = jenks$brks,
        include.lowest = TRUE,
        labels = FALSE
      )
    ) %>%
    arrange(Cluster, Innovation_Score)
}


jenks_k2 <- jenks_assignment(2)
jenks_k3 <- jenks_assignment(3)
jenks_k4 <- jenks_assignment(4)


jenks_k2 |> print(n=100)
jenks_k3 |> print(n=100)
jenks_k4 |> print(n=100)


# ============================================================
# 7. RIASSUNTO JENKS
# ============================================================

jenks_summary <- function(df) {
  
  df %>%
    group_by(Cluster) %>%
    summarise(
      N = n(),
      Min = min(Innovation_Score),
      Max = max(Innovation_Score),
      Mean = mean(Innovation_Score),
      .groups = "drop"
    ) %>%
    arrange(Cluster)
}


jenks_summary(jenks_k2)
jenks_summary(jenks_k3)
jenks_summary(jenks_k4)

# Innovation Score dell'intero campione tecnico
innovation_full <- technical_scores %>%
  select(model_std, Innovation_Score) %>%
  filter(!is.na(Innovation_Score))

x_full <- innovation_full$Innovation_Score


# ============================================================
# GAP STATISTIC
# ============================================================

set.seed(123)

gap_result <- clusGap(
  x = matrix(x_full, ncol = 1),
  FUNcluster = function(x, k) {
    kmeans(x, centers = k, nstart = 100)
  },
  K.max = 5,
  B = 500
)

gap_result


# Grafico Gap Statistic
plot(gap_result)

gap_table <- data.frame(
  k = 1:5,
  Gap = gap_result$Tab[, "gap"],
  SE = gap_result$Tab[, "SE.sim"]
)

gap_table

gap_result$Tab

max_gap_k <- which.max(gap_result$Tab[, "gap"])

max_gap_k

gap_table %>%
  filter(k %in% c(2, 3, 4))

# ============================================================
# STRUTTURA DELL'INNOVATION SCORE - CAMPIONE COMPLETO
# ============================================================

innovation_full <- technical_scores %>%
  select(model_std, Innovation_Score) %>%
  filter(!is.na(Innovation_Score))

x_full <- innovation_full$Innovation_Score


# K-means con 3 gruppi
set.seed(123)

km_full_3 <- kmeans(
  matrix(x_full, ncol = 1),
  centers = 3,
  nstart = 100
)

innovation_full_km3 <- innovation_full %>%
  mutate(Cluster = km_full_3$cluster)


innovation_full_km3 %>%
  group_by(Cluster) %>%
  summarise(
    N = n(),
    Min = min(Innovation_Score),
    Max = max(Innovation_Score),
    Mean = mean(Innovation_Score),
    .groups = "drop"
  ) %>%
  arrange(Mean)


# Jenks con 3 gruppi
jenks_full_3 <- classInt::classIntervals(
  x_full,
  n = 3,
  style = "jenks"
)

jenks_full_3$brks

innovation_full %>%
  mutate(
    Jenks_Group = cut(
      Innovation_Score,
      breaks = jenks_full_3$brks,
      include.lowest = TRUE,
      labels = FALSE
    )
  ) %>%
  group_by(Jenks_Group) %>%
  summarise(
    N = n(),
    Min = min(Innovation_Score),
    Max = max(Innovation_Score),
    Mean = mean(Innovation_Score),
    .groups = "drop"
  )

# Verifichiamo che la clusterizzazione sia applicabile
innovation_groups_jenks <- perceived_scores_43 %>%
  mutate(
    Innovation_Group = case_when(
      Innovation_Score <= 6.588571 ~ "Low Innovation",
      Innovation_Score <= 32.054882 ~ "Medium Innovation",
      Innovation_Score > 32.054882 ~ "High Innovation"
    )
  )

innovation_groups_jenks %>%
  count(Innovation_Group)

innovation_groups_jenks %>%
  group_by(Innovation_Group) %>%
  summarise(
    N = n(),
    Mean_Innovation = mean(Innovation_Score),
    Mean_CPI = mean(CPI),
    SD_CPI = sd(CPI),
    Median_CPI = median(CPI),
    .groups = "drop"
  )

# Regressione principale: Innovation Score -> CPI

model_cpi <- lm(
  CPI ~ Innovation_Score,
  data = perceived_scores_43
)

summary(model_cpi)

par(mfrow = c(2, 2))
plot(model_cpi)
par(mfrow = c(1, 1))

# Check di robustezza con m55 escluso
model_cpi_no_m55 <- lm(
  CPI ~ Innovation_Score,
  data = subset(perceived_scores_43, Smartphone != "Galaxy M55")
)

summary(model_cpi_no_m55)

# L'M55 è realmente influente?
# Cook's distance
cooks_d <- cooks.distance(model_cpi)

# Soglia indicativa
threshold <- 4 / nrow(perceived_scores_43)

# Smartphone ordinati per influenza
cook_table <- data.frame(
  Smartphone = perceived_scores_43$Smartphone,
  Cooks_Distance = cooks_d
) |>
  arrange(desc(Cooks_Distance))

cook_table

# Osservazioni sopra la soglia
cook_table |>
  filter(Cooks_Distance > threshold)

threshold

# 1. Normalità dei residui
shapiro.test(residuals(model_cpi))
shapiro.test(residuals(model_cpi_no_m55))

# 2. Omoschedasticità
library(lmtest)
bptest(model_cpi)
bptest(model_cpi_no_m55)

# Prima figura regressione
ggplot(perceived_scores_43,
       aes(x = Innovation_Score, y = CPI)) +
  
  geom_point(size = 3, alpha = 0.8) +
  
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "darkgreen",
    linewidth = 1
  ) +
  
  geom_text(
    data = subset(perceived_scores_43, Smartphone == "Galaxy M55"),
    aes(label = Smartphone),
    vjust = 1.5,
    hjust = 0.5,
    size = 3.5
  ) +
  
  labs(
    title = "Technical Innovation and Percezione dei Consumatori",
    subtitle = "Regressione Lineare tra Innovation Score e Consumer Perception Index",
    x = "Innovation Score",
    y = "Consumer Perception Index (CPI)"
  ) +
  
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# Regressione senza M55:
perceived_scores_43 %>% 
  filter(Smartphone != "Galaxy M55") %>%
ggplot(aes(x = Innovation_Score, y = CPI)) +
  
  geom_point(size = 3, alpha = 0.8) +
  
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "darkgreen",
    linewidth = 1
  ) +
  
  labs(
    title = "Technical Innovation and Percezione dei Consumatori",
    subtitle = "Regressione Lineare tra Innovation Score e Consumer Perception Index",
    x = "Innovation Score",
    y = "Consumer Perception Index (CPI)"
  ) +
  
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

par(mfrow = c(2, 2))
plot(model_cpi_no_m55)
par(mfrow = c(1, 1))

# Sottoregressioni
model_performance <- lm(
  Performance_Perceived ~ Performance_Technical,
  data = perceived_scores_43
)

summary(model_performance)

  model_camera <- lm(
    Camera_Perceived ~ Camera_Technical,
    data = perceived_scores_43
  )
  
  summary(model_camera)

model_battery <- lm(
  Battery_Perceived ~ Battery_Technical,
  data = perceived_scores_43
)

summary(model_battery)

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