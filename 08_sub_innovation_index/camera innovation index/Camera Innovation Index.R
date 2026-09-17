library(tidyverse)

# ------------------------------------------------------------
# 0. CARICAMENTO DATASET
# ------------------------------------------------------------

phones <- readRDS("dataset definitivo.rds")

# ------------------------------------------------------------
# 1. CORREZIONI DATI CAMERA GIA' VALIDATE
# ------------------------------------------------------------

# False AUX2 present
phones <- phones %>%
  mutate(
    aux2_present = case_when(
      Brand == "Sharp" & grepl("Aquos Wish4", Model, ignore.case = TRUE) ~ "No",
      Brand == "Realme" & grepl("C65", Model, ignore.case = TRUE) ~ "No",
      TRUE ~ aux2_present
    )
  )

# Missing / recovered optical zoom values
phones <- phones %>%
  mutate(
    main_optical_zoom = case_when(
      grepl("Redmi Note 13 5G", Model, ignore.case = TRUE) &
        Released.Year == 2024 ~ 1,
      grepl("Enjoy 60X", Model, ignore.case = TRUE) &
        Released.Year == 2023 ~ 1,
      grepl("Enjoy 70", Model, ignore.case = TRUE) &
        !grepl("70z", Model, ignore.case = TRUE) &
        Released.Year == 2023 ~ 1,
      grepl("Narzo 60x", Model, ignore.case = TRUE) &
        Released.Year == 2023 ~ 1,
      grepl("nova 11i", Model, ignore.case = TRUE) &
        Released.Year == 2023 ~ 1,
      grepl("Redmi Note 12R Pro", Model, ignore.case = TRUE) &
        Released.Year == 2023 ~ 1,
      grepl("A60", Model, ignore.case = TRUE) &
        Released.Year == 2024 ~ 1,
      grepl("Enjoy 70z", Model, ignore.case = TRUE) &
        Released.Year == 2024 ~ 1,
      grepl("RAZR 5G 2024", Model, ignore.case = TRUE) &
        Released.Year == 2024 ~ 1,
      grepl("GT 6T", Model, ignore.case = TRUE) &
        Released.Year == 2024 ~ 1,
      grepl("Nord 4", Model, ignore.case = TRUE) &
        Released.Year == 2024 ~ 1,
      grepl("V40", Model, ignore.case = TRUE) &
        Released.Year == 2024 ~ 1,
      TRUE ~ main_optical_zoom
    ),

    aux2_megapixels = case_when(
      grepl("Redmi Note 13 5G", Model, ignore.case = TRUE) ~ 2,
      grepl("Enjoy 60X", Model, ignore.case = TRUE) ~ 2,
      grepl("Enjoy 70", Model, ignore.case = TRUE) &
        !grepl("70z", Model, ignore.case = TRUE) ~ 2,
      grepl("Narzo 60x", Model, ignore.case = TRUE) ~ 2,
      grepl("nova 11i", Model, ignore.case = TRUE) ~ 2,
      grepl("Redmi Note 12R Pro", Model, ignore.case = TRUE) ~ 2,
      grepl("A60", Model, ignore.case = TRUE) ~ 2,
      grepl("Enjoy 70z", Model, ignore.case = TRUE) ~ 2,
      grepl("RAZR 5G 2024", Model, ignore.case = TRUE) ~ 13,
      grepl("GT 6T", Model, ignore.case = TRUE) ~ 8,
      grepl("Nord 4", Model, ignore.case = TRUE) ~ 8,
      grepl("V40", Model, ignore.case = TRUE) ~ 50,
      TRUE ~ aux2_megapixels
    ),

    aux2_sensor_format = case_when(
      grepl("RAZR 5G 2024", Model, ignore.case = TRUE) ~ "1/3.0",
      grepl("GT 6T", Model, ignore.case = TRUE) ~ "1/4.0",
      grepl("Nord 4", Model, ignore.case = TRUE) ~ "1/4.0",
      grepl("V40", Model, ignore.case = TRUE) ~ "1/2.76",
      TRUE ~ aux2_sensor_format
    ),

    aux2_aperture = case_when(
      grepl("Redmi Note 13 5G", Model, ignore.case = TRUE) ~ 2.4,
      grepl("Enjoy 60X", Model, ignore.case = TRUE) ~ 2.4,
      grepl("Enjoy 70", Model, ignore.case = TRUE) &
        !grepl("70z", Model, ignore.case = TRUE) ~ 2.4,
      grepl("Narzo 60x", Model, ignore.case = TRUE) ~ 2.4,
      grepl("nova 11i", Model, ignore.case = TRUE) ~ 2.4,
      grepl("Redmi Note 12R Pro", Model, ignore.case = TRUE) ~ 2.4,
      grepl("A60", Model, ignore.case = TRUE) ~ 2.4,
      grepl("Enjoy 70z", Model, ignore.case = TRUE) ~ 2.4,
      grepl("RAZR 5G 2024", Model, ignore.case = TRUE) ~ 2.2,
      grepl("GT 6T", Model, ignore.case = TRUE) ~ 2.2,
      grepl("Nord 4", Model, ignore.case = TRUE) ~ 2.2,
      grepl("V40", Model, ignore.case = TRUE) ~ 2.0,
      TRUE ~ aux2_aperture
    ),

    aux2_focal_length_mm = case_when(
      grepl("RAZR 5G 2024", Model, ignore.case = TRUE) ~ 15.5,
      grepl("GT 6T", Model, ignore.case = TRUE) ~ 15.91,
      grepl("Nord 4", Model, ignore.case = TRUE) ~ 16,
      grepl("V40", Model, ignore.case = TRUE) ~ 15,
      TRUE ~ aux2_focal_length_mm
    )
  )

# ------------------------------------------------------------
# 2. CAMERA DATASET DI LAVORO
# ------------------------------------------------------------

camera_vars <- c(
  "main_sensor_format",
  "main_pixel_size_micrometer",
  "main_camera_resolution_pixel",
  "main_megapixel",
  "main_aperture",
  "main_optical_zoom",
  "main_focus_cdaf",
  "main_focus_pdaf",
  "main_focus_laser",
  "main_focus_manual",
  "main_video_resolution",
  "main_flash_led_number",
  "main_camera_ois",
  "main_camera_eis",
  "main_camera_hdr_video",
  "main_camera_pixel_binning",
  "main_camera_gimbal",
  "main_camera_macro",
  "main_camera_slow_motion",
  "selfie_resolution_pixel",
  "selfie_aperture",
  "selfie_pixel_size_micrometer",
  "selfie_sensor_format",
  "selfie_eis",
  "selfie_hdr_photo",
  "selfie_hdr_video",
  "selfie_pixel_binning",
  "selfie_macro",
  "selfie_slow_motion",
  "aux_aperture",
  "aux_sensor_format",
  "aux_pixel_size_micrometer",
  "aux_camera_ois",
  "aux_camera_eis",
  "aux_camera_hdr_photo",
  "aux_camera_hdr_video",
  "aux_camera_pixel_binning",
  "aux_camera_macro",
  "aux_camera_slow_motion",
  "aux2_present",
  "aux2_megapixels",
  "aux2_pixel_size_micrometer",
  "aux2_sensor_format",
  "aux2_aperture",
  "aux2_focal_length_mm"
)

camera_audit <- data.frame(
  variable = camera_vars,
  class = sapply(phones[camera_vars], class),
  n_unique = sapply(phones[camera_vars], function(x) n_distinct(x, na.rm = TRUE)),
  n_missing = sapply(phones[camera_vars], function(x) sum(is.na(x))),
  missing_pct = sapply(phones[camera_vars], function(x) mean(is.na(x)) * 100)
)

print(camera_audit)

# ------------------------------------------------------------
# 3. HELPER: PARETO FRONTIER
# ------------------------------------------------------------

# direction:
# "max" = higher is better
# "min" = lower is better
#
# A point is Pareto-efficient if no other point is at least
# as good in every dimension and strictly better in at least one.

pareto_frontier <- function(df, directions) {

  vars <- names(directions)
  n <- nrow(df)

  efficient <- rep(TRUE, n)

  for (i in seq_len(n)) {

    for (j in seq_len(n)) {

      if (i == j) next

      at_least_as_good <- TRUE
      strictly_better <- FALSE

      for (v in vars) {

        if (directions[[v]] == "max") {

          if (df[[v]][j] < df[[v]][i]) {
            at_least_as_good <- FALSE
            break
          }

          if (df[[v]][j] > df[[v]][i]) {
            strictly_better <- TRUE
          }

        } else if (directions[[v]] == "min") {

          if (df[[v]][j] > df[[v]][i]) {
            at_least_as_good <- FALSE
            break
          }

          if (df[[v]][j] < df[[v]][i]) {
            strictly_better <- TRUE
          }
        }
      }

      if (at_least_as_good && strictly_better) {
        efficient[i] <- FALSE
        break
      }
    }
  }

  df[efficient, ]
}

# ------------------------------------------------------------
# 4. MAIN CAMERA
# ------------------------------------------------------------

main_pareto <- phones %>%
  select(
    Brand,
    Model,
    Released.Year,
    main_megapixel,
    main_pixel_size_micrometer,
    main_aperture
  ) %>%
  filter(
    !is.na(main_megapixel),
    !is.na(main_pixel_size_micrometer),
    !is.na(main_aperture)
  )

main_2023 <- main_pareto %>%
  filter(Released.Year == 2023)

main_frontier_2023 <- pareto_frontier(
  main_2023,
  c(
    main_megapixel = "max",
    main_pixel_size_micrometer = "max",
    main_aperture = "min"
  )
) %>%
  arrange(desc(main_megapixel))

print(main_frontier_2023)

main_2024 <- main_pareto %>%
  filter(Released.Year == 2024)

main_2024$extends_frontier_2023 <- sapply(seq_len(nrow(main_2024)), function(i) {
  
  !any(
    main_frontier_2023$main_megapixel >= main_2024$main_megapixel[i] &
      main_frontier_2023$main_pixel_size_micrometer >=
      main_2024$main_pixel_size_micrometer[i] &
      main_frontier_2023$main_aperture <= main_2024$main_aperture[i]
  )
})

# Joint frontier: only genuine 2024 extensions survive as new
# technological frontier points.
main_combined <- bind_rows(
  main_2023 %>% mutate(Year_Group = "2023"),
  main_2024 %>% mutate(Year_Group = "2024")
)

main_combined_frontier <- pareto_frontier(
  main_combined,
  c(
    main_megapixel = "max",
    main_pixel_size_micrometer = "max",
    main_aperture = "min"
  )
)

main_new_frontier <- main_combined_frontier %>%
  filter(
    Released.Year == 2024,
    extends_frontier_2023 == TRUE
  ) %>%
  distinct(
    main_megapixel,
    main_pixel_size_micrometer,
    main_aperture,
    .keep_all = TRUE
  )

print(main_new_frontier)

# Exact configurations validated during the previous analysis.
main_2024_configs <- tibble(
  config = c(
    "Huawei Pura 70 / Pro / Pro+",
    "Huawei Pura 70 Ultra",
    "Xiaomi 15 Pro"
  ),
  main_megapixel = c(50.3, 50.3, 50.0),
  main_pixel_size_micrometer = c(1.2, 1.6, 1.6),
  main_aperture = c(1.4, 1.6, 1.44)
)

# Exact hypervolume function used for the final validated Main result.
hv3 <- function(points) {

  points <- unique(as.data.frame(points))

  if (nrow(points) == 0) return(0)

  points <- points[
    points$x > 0 &
      points$y > 0 &
      points$z > 0,
  ]

  if (nrow(points) == 0) return(0)

  x_values <- sort(unique(c(0, points$x)))

  total_volume <- 0

  for (j in seq_len(length(x_values) - 1)) {

    x_left <- x_values[j]
    x_right <- x_values[j + 1]

    if (x_right <= x_left) next

    active <- points %>%
      filter(x >= x_right)

    if (nrow(active) == 0) next

    active <- active %>%
      arrange(desc(y))

    max_z <- 0
    area <- 0

    for (k in seq_len(nrow(active))) {

      current_y <- active$y[k]
      current_z <- active$z[k]

      if (current_z > max_z) {
        area <- area + current_y * (current_z - max_z)
        max_z <- current_z
      }
    }

    total_volume <- total_volume +
      (x_right - x_left) * area
  }

  total_volume
}

main_frontier <- main_frontier_2023 %>%
  select(
    main_megapixel,
    main_pixel_size_micrometer,
    main_aperture
  ) %>%
  distinct()

mp_min <- min(main_frontier$main_megapixel)
mp_max <- max(main_frontier$main_megapixel)
pixel_min <- min(main_frontier$main_pixel_size_micrometer)
pixel_max <- max(main_frontier$main_pixel_size_micrometer)
ap_min <- min(main_frontier$main_aperture)
ap_max <- max(main_frontier$main_aperture)

main_frontier_norm <- main_frontier %>%
  mutate(
    x = (main_megapixel - mp_min) / (mp_max - mp_min),
    y = (main_pixel_size_micrometer - pixel_min) /
      (pixel_max - pixel_min),
    z = (ap_max - main_aperture) / (ap_max - ap_min)
  ) %>%
  select(x, y, z)

main_2024_norm <- main_2024_configs %>%
  mutate(
    x = (main_megapixel - mp_min) / (mp_max - mp_min),
    y = (main_pixel_size_micrometer - pixel_min) /
      (pixel_max - pixel_min),
    z = (ap_max - main_aperture) / (ap_max - ap_min)
  )

hv_main_2023 <- hv3(main_frontier_norm)

main_hv_results <- main_2024_norm %>%
  mutate(
    delta_hv = purrr::map_dbl(
      seq_len(n()),
      ~ hv3(
        bind_rows(
          main_frontier_norm,
          main_2024_norm[.x, c("x", "y", "z")]
        )
      ) - hv_main_2023
    ),
    extension_share = delta_hv / hv_main_2023
  ) %>%
  select(config, delta_hv, extension_share)

print(main_hv_results)

# Validated factors from the previous analysis.
# First normalization:
# Pura 70 family = 1.00155
# Pura 70 Ultra = 1.05822
# Xiaomi 15 Pro = 1.17988

# ------------------------------------------------------------
# 5. SELFIE CAMERA
# ------------------------------------------------------------

selfie_pareto <- phones %>%
  select(
    Brand,
    Model,
    Released.Year,
    selfie_resolution_pixel,
    selfie_pixel_size_micrometer,
    selfie_aperture
  ) %>%
  mutate(
    resolution_width = as.numeric(sub("x.*", "", selfie_resolution_pixel)),
    resolution_height = as.numeric(sub(".*x", "", selfie_resolution_pixel)),
    selfie_megapixel =
      (resolution_width * resolution_height) / 1000000
  ) %>%
  filter(
    !is.na(selfie_megapixel),
    !is.na(selfie_pixel_size_micrometer),
    !is.na(selfie_aperture)
  )

selfie_2023 <- selfie_pareto %>%
  filter(Released.Year == 2023)

selfie_frontier_2023 <- pareto_frontier(
  selfie_2023,
  c(
    selfie_megapixel = "max",
    selfie_pixel_size_micrometer = "max",
    selfie_aperture = "min"
  )
) %>%
  arrange(desc(selfie_megapixel))

selfie_2024 <- selfie_pareto %>%
  filter(Released.Year == 2024)

selfie_2024$extends_frontier_2023 <- sapply(
  seq_len(nrow(selfie_2024)),
  function(i) {

    !any(
      selfie_frontier_2023$selfie_megapixel >= selfie_2024$selfie_megapixel[i] &
        selfie_frontier_2023$selfie_pixel_size_micrometer >=
          selfie_2024$selfie_pixel_size_micrometer[i] &
        selfie_frontier_2023$selfie_aperture <=
          selfie_2024$selfie_aperture[i]
    )
  }
)

selfie_combined <- bind_rows(
  selfie_2023 %>% mutate(Year_Group = "2023"),
  selfie_2024 %>% mutate(Year_Group = "2024")
)

selfie_combined_frontier <- pareto_frontier(
  selfie_combined,
  c(
    selfie_megapixel = "max",
    selfie_pixel_size_micrometer = "max",
    selfie_aperture = "min"
  )
)

selfie_new_frontier <- selfie_combined_frontier %>%
  filter(
    Released.Year == 2024,
    extends_frontier_2023 == TRUE
  ) %>%
  distinct(
    selfie_megapixel,
    selfie_pixel_size_micrometer,
    selfie_aperture,
    .keep_all = TRUE
  )

print(selfie_new_frontier)

selfie_frontier <- selfie_frontier_2023 %>%
  select(
    selfie_megapixel,
    selfie_pixel_size_micrometer,
    selfie_aperture
  ) %>%
  distinct()

selfie_mp_min <- min(selfie_frontier$selfie_megapixel)
selfie_mp_max <- max(selfie_frontier$selfie_megapixel)
selfie_pixel_min <- min(selfie_frontier$selfie_pixel_size_micrometer)
selfie_pixel_max <- max(selfie_frontier$selfie_pixel_size_micrometer)
selfie_ap_min <- min(selfie_frontier$selfie_aperture)
selfie_ap_max <- max(selfie_frontier$selfie_aperture)

selfie_frontier_norm <- selfie_frontier %>%
  mutate(
    x = (selfie_megapixel - selfie_mp_min) /
      (selfie_mp_max - selfie_mp_min),
    y = (selfie_pixel_size_micrometer - selfie_pixel_min) /
      (selfie_pixel_max - selfie_pixel_min),
    z = (selfie_ap_max - selfie_aperture) /
      (selfie_ap_max - selfie_ap_min)
  ) %>%
  select(x, y, z)

selfie_2024_configs <- tibble(
  config = c(
    "OPPO Reno12 Pro",
    "Motorola Edge 50 Pro / Ultra / 50s Pro",
    "Xiaomi Civi 4 Pro",
    "Google Pixel 8a"
  ),
  selfie_megapixel = c(
    50.331648,
    50.13504,
    31.961088,
    12.9792
  ),
  selfie_pixel_size_micrometer = c(
    0.64,
    0.64,
    1.12,
    1.22
  ),
  selfie_aperture = c(
    2.0,
    1.9,
    2.0,
    2.2
  )
) %>%
  mutate(
    x = (selfie_megapixel - selfie_mp_min) /
      (selfie_mp_max - selfie_mp_min),
    y = (selfie_pixel_size_micrometer - selfie_pixel_min) /
      (selfie_pixel_max - selfie_pixel_min),
    z = (selfie_ap_max - selfie_aperture) /
      (selfie_ap_max - selfie_ap_min)
  )

hv_selfie_2023 <- hv3(selfie_frontier_norm)

selfie_hv_results <- selfie_2024_configs %>%
  mutate(
    delta_hv = purrr::map_dbl(
      seq_len(n()),
      ~ hv3(
        bind_rows(
          selfie_frontier_norm,
          selfie_2024_configs[.x, c("x", "y", "z")]
        )
      ) - hv_selfie_2023
    ),
    extension_share = delta_hv / hv_selfie_2023
  ) %>%
  select(config, delta_hv, extension_share)

print(selfie_hv_results)

# Validated factors:
# Reno12 Pro = 1.07286
# Motorola Edge 50 family = 1.12714
# Civi 4 Pro = 1.13571
# Pixel 8a = 1.00203

# ------------------------------------------------------------
# 6. AUX CAMERA
# ------------------------------------------------------------

aux_pareto <- phones %>%
  select(
    Brand,
    Model,
    Released.Year,
    aux_sensor_format,
    aux_pixel_size_micrometer,
    aux_aperture
  ) %>%
  mutate(
    aux_sensor_denominator = as.numeric(sub("1/", "", aux_sensor_format)),
    aux_sensor_size = 1 / aux_sensor_denominator
  ) %>%
  filter(
    !is.na(aux_sensor_size),
    !is.na(aux_pixel_size_micrometer),
    !is.na(aux_aperture)
  )

aux_2023 <- aux_pareto %>%
  filter(Released.Year == 2023)

aux_frontier_2023 <- pareto_frontier(
  aux_2023,
  c(
    aux_sensor_size = "max",
    aux_pixel_size_micrometer = "max",
    aux_aperture = "min"
  )
) %>%
  distinct(
    aux_sensor_size,
    aux_pixel_size_micrometer,
    aux_aperture,
    .keep_all = TRUE
)

aux_combined <- aux_pareto

aux_combined_frontier <- pareto_frontier(
  aux_combined,
  c(
    aux_sensor_size = "max",
    aux_pixel_size_micrometer = "max",
    aux_aperture = "min"
  )
)

aux_new_frontier_configs <- aux_combined_frontier %>%
  filter(Released.Year == 2024) %>%
  distinct(
    aux_sensor_size,
    aux_pixel_size_micrometer,
    aux_aperture
  ) %>%
  anti_join(
    aux_frontier_2023 %>%
      distinct(
        aux_sensor_size,
        aux_pixel_size_micrometer,
        aux_aperture
      ),
    by = c(
      "aux_sensor_size",
      "aux_pixel_size_micrometer",
      "aux_aperture"
    )
  )

print(aux_new_frontier_configs)

# Exact AUX hypervolume implementation.
hv_aux_exact <- function(frontier) {

  frontier <- frontier %>%
    select(x, y, z) %>%
    distinct() %>%
    filter(
      x > 0,
      y > 0,
      z > 0
    )

  if (nrow(frontier) == 0) return(0)

  x_breaks <- sort(unique(c(0, frontier$x)))
  y_breaks <- sort(unique(c(0, frontier$y)))
  z_breaks <- sort(unique(c(0, frontier$z)))

  hv <- 0

  for (i in seq_len(length(x_breaks) - 1)) {

    x_left <- x_breaks[i]
    x_right <- x_breaks[i + 1]
    dx <- x_right - x_left

    for (j in seq_len(length(y_breaks) - 1)) {

      y_left <- y_breaks[j]
      y_right <- y_breaks[j + 1]
      dy <- y_right - y_left

      for (k in seq_len(length(z_breaks) - 1)) {

        z_left <- z_breaks[k]
        z_right <- z_breaks[k + 1]
        dz <- z_right - z_left

        dominated <- any(
          frontier$x >= x_right &
            frontier$y >= y_right &
            frontier$z >= z_right
        )

        if (dominated) {
          hv <- hv + dx * dy * dz
        }
      }
    }
  }

  hv
}

# The validated AUX result:
# HV2023 = 0.7519987
# HV2023 + Pixel 9 = 0.7632098
# HE = 0.01490837
# Factor = 1.01490837

# ------------------------------------------------------------
# 7. AUX2 CAMERA
# ------------------------------------------------------------
#
# IMPORTANT METHODOLOGICAL DECISION:
# Pixel size is NOT included in the quantitative Pareto frontier.
#
# Reason:
# pixel size has an ambivalent technological interpretation:
# larger pixels can improve light collection, while smaller pixels
# permit higher pixel density. Therefore there is no universal
# monotonic "higher/lower is better" direction.
#
# Quantitative AUX2 frontier therefore uses:
#   megapixels          ↑
#   sensor physical size ↑
#
# Pixel size remains diagnostic information only.
# ------------------------------------------------------------

aux2_pareto <- phones %>%
  select(
    Brand,
    Model,
    Released.Year,
    aux2_megapixels,
    aux2_pixel_size_micrometer,
    aux2_sensor_format
  ) %>%
  mutate(
    aux2_sensor_denominator = as.numeric(
      sub("1/", "", aux2_sensor_format)
    ),
    aux2_sensor_size = 1 / aux2_sensor_denominator
  ) %>%
  filter(
    !is.na(aux2_megapixels),
    !is.na(aux2_sensor_size)
  )

aux2_2023 <- aux2_pareto %>%
  filter(Released.Year == 2023)

aux2_frontier_2023 <- pareto_frontier(
  aux2_2023,
  c(
    aux2_megapixels = "max",
    aux2_sensor_size = "max"
  )
)

aux2_frontier_2023 <- aux2_frontier_2023 %>%
  arrange(desc(aux2_megapixels))

print(
  aux2_frontier_2023 %>%
    select(
      Brand,
      Model,
      aux2_megapixels,
      aux2_sensor_size,
      aux2_sensor_format,
      aux2_pixel_size_micrometer
    )
)

# 2024 points that are not dominated by any 2023 frontier point.
aux2_2024 <- aux2_pareto %>%
  filter(Released.Year == 2024)

aux2_2024$extends_2023_frontier <- sapply(
  seq_len(nrow(aux2_2024)),
  function(i) {

    !any(
      aux2_frontier_2023$aux2_megapixels >= aux2_2024$aux2_megapixels[i] &
        aux2_frontier_2023$aux2_sensor_size >=
          aux2_2024$aux2_sensor_size[i]
    )
  }
)

aux2_2024_extensions <- aux2_2024 %>%
  filter(extends_2023_frontier)

print(
  aux2_2024_extensions %>%
    arrange(desc(aux2_megapixels)) %>%
    select(
      Brand,
      Model,
      aux2_megapixels,
      aux2_sensor_size,
      aux2_sensor_format,
      aux2_pixel_size_micrometer
    )
)

# Expected validated result:
# vivo X100 Ultra -> 200 MP, 1/1.40"
# vivo X200 Pro   -> 200 MP, 1/1.40"
# OPPO Find X7    -> 64.2 MP, 1/2.00"
#
# X100 Ultra and X200 Pro are the same technological configuration
# and therefore must count once for hypervolume purposes.

aux2_configs_2024 <- aux2_2024_extensions %>%
  distinct(
    aux2_megapixels,
    aux2_sensor_size
  )

# Normalize on the 2023 frontier range.
aux2_mp_min <- min(aux2_frontier_2023$aux2_megapixels)
aux2_mp_max <- max(aux2_frontier_2023$aux2_megapixels)

aux2_sensor_min <- min(aux2_frontier_2023$aux2_sensor_size)
aux2_sensor_max <- max(aux2_frontier_2023$aux2_sensor_size)

aux2_frontier_norm <- aux2_frontier_2023 %>%
  mutate(
    x = (aux2_megapixels - aux2_mp_min) /
      (aux2_mp_max - aux2_mp_min),
    y = (aux2_sensor_size - aux2_sensor_min) /
      (aux2_sensor_max - aux2_sensor_min)
  ) %>%
  select(x, y)

aux2_configs_norm <- aux2_configs_2024 %>%
  mutate(
    x = (aux2_megapixels - aux2_mp_min) /
      (aux2_mp_max - aux2_mp_min),
    y = (aux2_sensor_size - aux2_sensor_min) /
      (aux2_sensor_max - aux2_sensor_min)
  )

# Exact 2D hypervolume: union of rectangles from the origin.
hypervolume_2d <- function(df) {

  df <- df %>%
    select(x, y) %>%
    distinct() %>%
    filter(
      is.finite(x),
      is.finite(y),
      x > 0,
      y > 0
    )

  if (nrow(df) == 0) return(0)

  x_breaks <- sort(unique(c(0, df$x)))
  hv <- 0

  for (i in seq_len(length(x_breaks) - 1)) {

    x_left <- x_breaks[i]
    x_right <- x_breaks[i + 1]

    if (x_right <= x_left) next

    # At a given x interval, the union reaches the maximum y
    # among points with x >= the right edge.
    active <- df %>%
      filter(x >= x_right)

    if (nrow(active) == 0) next

    y_max <- max(active$y)

    hv <- hv + (x_right - x_left) * y_max
  }

  hv
}

hv_aux2_2023 <- hypervolume_2d(aux2_frontier_norm)

print(hv_aux2_2023)

# Increment of each distinct 2024 technological configuration.
aux2_hv_results <- aux2_configs_norm %>%
  rowwise() %>%
  mutate(
    hv_total = hypervolume_2d(
      bind_rows(
        aux2_frontier_norm,
        tibble(x = x, y = y)
      )
    ),
    delta_hv = hv_total - hv_aux2_2023,
    extension_share = delta_hv / hv_aux2_2023
  ) %>%
  ungroup()

print(aux2_hv_results)

# ------------------------------------------------------------
# 8. STEPLESS VARIABLE APERTURE
# ------------------------------------------------------------
#
# Quantitative factor validated previously:
#
# Xiaomi 13 Ultra: f/1.9 - f/4.0
# Xiaomi 14 Ultra: f/1.63 - f/4.0
#
# Stop-range:
# S = 2 * log2(f_max / f_min)
#
# Relative extension:
# (S_2024 - S_2023) / S_2023 = approximately 0.201
#
# Factor = 1.201
#
# The continuous 0.01-stop control is qualitative evidence of
# the structural stepless innovation, while 1.201 quantifies
# the expansion of the optical aperture range.
# ------------------------------------------------------------

S_2023 <- 2 * log2(4 / 1.9)
S_2024 <- 2 * log2(4 / 1.63)

stepless_extension <- (S_2024 - S_2023) / S_2023
stepless_factor <- 1 + stepless_extension

print(
  tibble(
    S_2023 = S_2023,
    S_2024 = S_2024,
    extension = stepless_extension,
    factor = stepless_factor
  )
)

# ------------------------------------------------------------
# 9. INTEGRAZIONE DEI SEI FATTORI
# ------------------------------------------------------------

# IMPORTANT:
# AUX2 factor is intentionally left as the current validated
# MP-ratio until the new 2D hypervolume result is reviewed.
#
# Current validated factor:
#   200.5 / 63.7 = 3.147566719
#
# After reviewing aux2_hv_results, replace ONLY this value
# if the final methodological decision is to use the new
# multidimensional factor.

phones <- phones %>%
  mutate(

    Camera_Main_Frontier_Factor = case_when(

      Released.Year == 2024 &
        grepl("^Pura 70 ", Model, ignore.case = TRUE) &
        !grepl("Ultra", Model, ignore.case = TRUE) ~ 1.00155,

      Released.Year == 2024 &
        grepl("Pura 70 Ultra", Model, ignore.case = TRUE) ~ 1.05822,

      Released.Year == 2024 &
        Model == "15 Pro" ~ 1.17988,

      Released.Year == 2024 ~ 1,

      TRUE ~ NA_real_
    ),

    Camera_Selfie_Frontier_Factor = case_when(

      Released.Year == 2024 &
        grepl("Reno12 Pro", Model, ignore.case = TRUE) ~ 1.07286,

      Released.Year == 2024 &
        grepl("Edge 50 Pro", Model, ignore.case = TRUE) &
        !grepl("Fusion|Neo", Model, ignore.case = TRUE) ~ 1.12714,

      Released.Year == 2024 &
        grepl("Edge 50 Ultra", Model, ignore.case = TRUE) ~ 1.12714,

      Released.Year == 2024 &
        grepl("Edge 50s Pro", Model, ignore.case = TRUE) ~ 1.12714,

      Released.Year == 2024 &
        grepl("Civi 4 Pro", Model, ignore.case = TRUE) ~ 1.13571,

      Released.Year == 2024 &
        grepl("Pixel 8a", Model, ignore.case = TRUE) ~ 1.00203,

      Released.Year == 2024 ~ 1,

      TRUE ~ NA_real_
    ),

    Camera_AUX_Aperture_Factor = case_when(

      Released.Year == 2024 &
        grepl("Pixel 9", Model, ignore.case = TRUE) &
        !grepl("Fold", Model, ignore.case = TRUE) ~ 1.01490837,

      Released.Year == 2024 ~ 1,

      TRUE ~ NA_real_
    ),

    Camera_AUX2_Frontier_Factor = case_when(

      Released.Year == 2024 &
        aux2_megapixels == 200.5 &
        aux2_pixel_size_micrometer == 0.56 ~
        200.5 / 63.7,

      Released.Year == 2024 ~ 1,

      TRUE ~ NA_real_
    ),

    Camera_Dual_Periscopic_Factor = case_when(

      Released.Year == 2024 &
        grepl(
          "Find X7 Ultra|Find X8 Pro",
          Model,
          ignore.case = TRUE
        ) ~ 1.053571,

      Released.Year == 2024 ~ 1,

      TRUE ~ NA_real_
    ),

    Camera_Stepless_Innovation = case_when(

      Released.Year == 2024 &
        model_std == "mi 14 ultra 5g" ~ 1.201,

      Released.Year == 2024 ~ 1,

      TRUE ~ NA_real_
    )
  )

# ------------------------------------------------------------
# 10. CONTROLLO DEI SEI FATTORI
# ------------------------------------------------------------

camera_factors <- phones %>%
  filter(Released.Year == 2024) %>%
  select(
    Brand,
    Model,
    Camera_Main_Frontier_Factor,
    Camera_Selfie_Frontier_Factor,
    Camera_AUX_Aperture_Factor,
    Camera_AUX2_Frontier_Factor,
    Camera_Dual_Periscopic_Factor,
    Camera_Stepless_Innovation
  )

camera_control <- camera_factors %>%
  summarise(
    N = n(),

    Main_innovative =
      sum(Camera_Main_Frontier_Factor > 1, na.rm = TRUE),

    Selfie_innovative =
      sum(Camera_Selfie_Frontier_Factor > 1, na.rm = TRUE),

    AUX_innovative =
      sum(Camera_AUX_Aperture_Factor > 1, na.rm = TRUE),

    AUX2_innovative =
      sum(Camera_AUX2_Frontier_Factor > 1, na.rm = TRUE),

    Dual_Periscopic_innovative =
      sum(Camera_Dual_Periscopic_Factor > 1, na.rm = TRUE),

    Stepless_innovative =
      sum(Camera_Stepless_Innovation > 1, na.rm = TRUE),

    Main_NA =
      sum(is.na(Camera_Main_Frontier_Factor)),

    Selfie_NA =
      sum(is.na(Camera_Selfie_Frontier_Factor)),

    AUX_NA =
      sum(is.na(Camera_AUX_Aperture_Factor)),

    AUX2_NA =
      sum(is.na(Camera_AUX2_Frontier_Factor)),

    Dual_Periscopic_NA =
      sum(is.na(Camera_Dual_Periscopic_Factor)),

    Stepless_NA =
      sum(is.na(Camera_Stepless_Innovation))
  )

print(camera_control)

# Expected validated control:
# N = 172
# Main = 5
# Selfie = 6
# AUX = 3
# AUX2 = 2
# Dual Periscopic = 2
# Stepless = 1
# all NA = 0

# ------------------------------------------------------------
# 11. CURRENT CAMERA INNOVATION INDEX
# ------------------------------------------------------------

phones <- phones %>%
  mutate(
    Camera_Innovation_Index = case_when(

      Released.Year == 2024 ~

        Camera_Main_Frontier_Factor *
        Camera_Selfie_Frontier_Factor *
        Camera_AUX_Aperture_Factor *
        Camera_AUX2_Frontier_Factor *
        Camera_Dual_Periscopic_Factor *
        Camera_Stepless_Innovation,

      TRUE ~ NA_real_
    )
  )

camera_index_summary <- phones %>%
  filter(Released.Year == 2024) %>%
  summarise(
    N = n(),
    Mean = mean(Camera_Innovation_Index),
    Median = median(Camera_Innovation_Index),
    SD = sd(Camera_Innovation_Index),
    Min = min(Camera_Innovation_Index),
    P25 = quantile(Camera_Innovation_Index, 0.25),
    P75 = quantile(Camera_Innovation_Index, 0.75),
    P90 = quantile(Camera_Innovation_Index, 0.90),
    P95 = quantile(Camera_Innovation_Index, 0.95),
    Max = max(Camera_Innovation_Index)
  )

print(camera_index_summary)

# ------------------------------------------------------------
# 12. TOP 20 - DIAGNOSTICA
# ------------------------------------------------------------

camera_top20 <- phones %>%
  filter(Released.Year == 2024) %>%
  arrange(desc(Camera_Innovation_Index)) %>%
  select(
    Brand,
    Model,
    Camera_Innovation_Index,
    Camera_Main_Frontier_Factor,
    Camera_Selfie_Frontier_Factor,
    Camera_AUX_Aperture_Factor,
    Camera_AUX2_Frontier_Factor,
    Camera_Dual_Periscopic_Factor,
    Camera_Stepless_Innovation
  ) %>%
  slice_head(n = 20)

print(camera_top20)

# ------------------------------------------------------------
# 13. SENSITIVITY TEST: LOG TRANSFORMATION OF AUX2
# ------------------------------------------------------------

aux2_raw <- 200.5 / 63.7
aux2_log <- 1 + log(aux2_raw)

phones_ranking_test <- phones %>%
  filter(Released.Year == 2024) %>%
  mutate(

    Camera_Index_Raw = Camera_Innovation_Index,

    Camera_Index_LogAUX2 =
      Camera_Main_Frontier_Factor *
      Camera_Selfie_Frontier_Factor *
      Camera_AUX_Aperture_Factor *
      if_else(
        Camera_AUX2_Frontier_Factor > 1,
        1 + log(Camera_AUX2_Frontier_Factor),
        1
      ) *
      Camera_Dual_Periscopic_Factor *
      Camera_Stepless_Innovation
  )

print(
  phones_ranking_test %>%
    arrange(desc(Camera_Index_LogAUX2)) %>%
    select(
      Brand,
      Model,
      Camera_Index_Raw,
      Camera_Index_LogAUX2
    ) %>%
    slice_head(n = 20)
)

print(
  phones_ranking_test %>%
    summarise(
      Raw_Mean = mean(Camera_Index_Raw),
      Raw_Median = median(Camera_Index_Raw),
      Raw_SD = sd(Camera_Index_Raw),
      Raw_Max = max(Camera_Index_Raw),

      LogAUX2_Mean = mean(Camera_Index_LogAUX2),
      LogAUX2_Median = median(Camera_Index_LogAUX2),
      LogAUX2_SD = sd(Camera_Index_LogAUX2),
      LogAUX2_Max = max(Camera_Index_LogAUX2)
    )
)

# ------------------------------------------------------------
# 14. AUX2 PARETO
# ------------------------------------------------------------
pareto_2d <- function(df) {
  
  n <- nrow(df)
  efficient <- rep(TRUE, n)
  
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      
      if (i != j) {
        
        dominates <-
          df$aux2_megapixels[j] >= df$aux2_megapixels[i] &&
          df$aux2_sensor_size[j] >= df$aux2_sensor_size[i] &&
          (
            df$aux2_megapixels[j] > df$aux2_megapixels[i] ||
              df$aux2_sensor_size[j] > df$aux2_sensor_size[i]
          )
        
        if (dominates) {
          efficient[i] <- FALSE
          break
        }
      }
    }
  }
  
  df[efficient, ]
}

aux2_2023_pareto <- aux2_pareto %>%
  filter(Released.Year == 2023) %>%
  pareto_2d()

# 2024
aux2_2024_extensions <- aux2_pareto %>%
  filter(Released.Year == 2024) %>%
  rowwise() %>%
  mutate(
    extends_2023_frontier = !any(
      aux2_2023_pareto$aux2_megapixels >= aux2_megapixels &
        aux2_2023_pareto$aux2_sensor_size >= aux2_sensor_size
    )
  ) %>%
  ungroup() %>%
  filter(extends_2023_frontier)

# Hypervolume
aux2_2023 <- aux2_pareto %>%
  filter(Released.Year == 2023)

mp_min <- min(aux2_2023$aux2_megapixels)
mp_max <- max(aux2_2023$aux2_megapixels)

sensor_min <- min(aux2_2023$aux2_sensor_size)
sensor_max <- max(aux2_2023$aux2_sensor_size)

aux2_norm <- aux2_pareto %>%
  mutate(
    MP_norm =
      (aux2_megapixels - mp_min) /
      (mp_max - mp_min),
    
    Sensor_norm =
      (aux2_sensor_size - sensor_min) /
      (sensor_max - sensor_min)
  )


hypervolume_2d <- function(df) {
  
  df <- df %>%
    distinct(MP_norm, Sensor_norm) %>%
    arrange(MP_norm)
  
  x <- c(0, df$MP_norm)
  y <- c(0, df$Sensor_norm)
  
  hv <- 0
  
  for (i in 2:length(x)) {
    
    x_width <- x[i] - x[i - 1]
    
    y_height <- max(
      df$Sensor_norm[df$MP_norm >= x[i - 1] &
                       df$MP_norm <= x[i]],
      na.rm = TRUE
    )
    
    hv <- hv + x_width * y_height
  }
  
  hv
}

hv_2023 <- hypervolume_2d(
  aux2_norm %>%
    filter(Released.Year == 2023)
)

hv_2023

# Incrementi singoli
aux2_configs_2024 <- aux2_2024_extensions %>%
  distinct(
    aux2_megapixels,
    aux2_sensor_size
  ) %>%
  mutate(
    MP_norm =
      (aux2_megapixels - mp_min) /
      (mp_max - mp_min),
    
    Sensor_norm =
      (aux2_sensor_size - sensor_min) /
      (sensor_max - sensor_min)
  )

aux2_configs_2024

hv_with_config <- aux2_configs_2024 %>%
  rowwise() %>%
  mutate(
    HV_total = hypervolume_2d(
      bind_rows(
        aux2_norm %>%
          filter(Released.Year == 2023) %>%
          select(MP_norm, Sensor_norm),
        
        tibble(
          MP_norm = MP_norm,
          Sensor_norm = Sensor_norm
        )
      )
    ),
    
    HV_increment = HV_total - hv_2023,
    
    HE = HV_increment / hv_2023
  ) %>%
  ungroup()

hv_with_config

# ------------------------------------------------------------
# AUX2 — ROBUST NORMALIZATION 2023 + 2024
# ------------------------------------------------------------

aux2_all <- aux2_pareto %>%
  filter(Released.Year %in% c(2023, 2024))

mp_min_all <- min(aux2_all$aux2_megapixels, na.rm = TRUE)
mp_max_all <- max(aux2_all$aux2_megapixels, na.rm = TRUE)

sensor_min_all <- min(aux2_all$aux2_sensor_size, na.rm = TRUE)
sensor_max_all <- max(aux2_all$aux2_sensor_size, na.rm = TRUE)

aux2_norm_all <- aux2_all %>%
  mutate(
    MP_norm =
      (aux2_megapixels - mp_min_all) /
      (mp_max_all - mp_min_all),
    
    Sensor_norm =
      (aux2_sensor_size - sensor_min_all) /
      (sensor_max_all - sensor_min_all)
  )

hv_2023_all <- hypervolume_2d(
  aux2_norm_all %>%
    filter(Released.Year == 2023) %>%
    select(MP_norm, Sensor_norm)
)

hv_with_config_all <- aux2_configs_2024 %>%
  rowwise() %>%
  mutate(
    MP_norm =
      (aux2_megapixels - mp_min_all) /
      (mp_max_all - mp_min_all),
    
    Sensor_norm =
      (aux2_sensor_size - sensor_min_all) /
      (sensor_max_all - sensor_min_all),
    
    HV_total =
      hypervolume_2d(
        bind_rows(
          aux2_norm_all %>%
            filter(Released.Year == 2023) %>%
            select(MP_norm, Sensor_norm),
          
          tibble(
            MP_norm = MP_norm,
            Sensor_norm = Sensor_norm
          )
        )
      ),
    
    HV_increment = HV_total - hv_2023_all,
    
    HE = HV_increment / hv_2023_all
  ) %>%
  ungroup()

hv_2023_all
hv_with_config_all

# ------------------------------------------------------------
# AUX2 — SENSITIVITY OF FRONTIER EXTENSION
# ------------------------------------------------------------

aux2_test_points <- tibble(
  aux2_megapixels = c(64.2, 100, 120, 150, 175, 200),
  aux2_sensor_size = c(
    0.5,
    0.7142857,
    0.7142857,
    0.7142857,
    0.7142857,
    0.7142857
  )
)

aux2_test_points <- aux2_test_points %>%
  mutate(
    MP_norm =
      (aux2_megapixels - mp_min_all) /
      (mp_max_all - mp_min_all),
    
    Sensor_norm =
      (aux2_sensor_size - sensor_min_all) /
      (sensor_max_all - sensor_min_all)
  )

aux2_test_hv <- aux2_test_points %>%
  rowwise() %>%
  mutate(
    HV_total =
      hypervolume_2d(
        bind_rows(
          aux2_norm_all %>%
            filter(Released.Year == 2023) %>%
            select(MP_norm, Sensor_norm),
          
          tibble(
            MP_norm = MP_norm,
            Sensor_norm = Sensor_norm
          )
        )
      ),
    
    HV_increment = HV_total - hv_2023_all,
    
    HE = HV_increment / hv_2023_all,
    
    Factor = 1 + HE
  ) %>%
  ungroup()

aux2_test_hv


# ------------------------------------------------------------
# AUX2 — SENSITIVITY ANALYSIS OF FRONTIER EXTENSION
# ------------------------------------------------------------

aux2_sensitivity <- hv_with_config_all %>%
  mutate(
    
    # Raw linear transformation
    Factor_Linear =
      1 + HE,
    
    # Logarithmic transformation
    Factor_Log =
      1 + log1p(HE),
    
    # Square-root transformation
    Factor_Sqrt =
      1 + sqrt(HE),
    
    # Cube-root transformation
    Factor_CubeRoot =
      1 + HE^(1/3)
    
  ) %>%
  select(
    aux2_megapixels,
    aux2_sensor_size,
    HE,
    Factor_Linear,
    Factor_Log,
    Factor_Sqrt,
    Factor_CubeRoot
  )

aux2_sensitivity

# ------------------------------------------------------------
# CAMERA INDEX — SENSITIVITY TO AUX2 TRANSFORMATION
# ------------------------------------------------------------

aux2_sensitivity <- hv_with_config_all %>%
  transmute(
    aux2_megapixels,
    aux2_sensor_size,
    Factor_Linear = 1 + HE,
    Factor_Log = 1 + log1p(HE),
    Factor_Sqrt = 1 + sqrt(HE),
    Factor_CubeRoot = 1 + HE^(1/3)
  )

# ------------------------------------------------------------
# CORREZIONE LOOKUP AUX2
# ------------------------------------------------------------

aux2_64_factor <- aux2_sensitivity %>%
  filter(aux2_megapixels == 64.2) %>%
  pull(Factor_Linear)

aux2_64_log <- aux2_sensitivity %>%
  filter(aux2_megapixels == 64.2) %>%
  pull(Factor_Log)

aux2_64_sqrt <- aux2_sensitivity %>%
  filter(aux2_megapixels == 64.2) %>%
  pull(Factor_Sqrt)

aux2_64_cube <- aux2_sensitivity %>%
  filter(aux2_megapixels == 64.2) %>%
  pull(Factor_CubeRoot)

aux2_200_factor <- aux2_sensitivity %>%
  filter(aux2_megapixels == 200.5) %>%
  pull(Factor_Linear)

aux2_200_log <- aux2_sensitivity %>%
  filter(aux2_megapixels == 200.5) %>%
  pull(Factor_Log)

aux2_200_sqrt <- aux2_sensitivity %>%
  filter(aux2_megapixels == 200.5) %>%
  pull(Factor_Sqrt)

aux2_200_cube <- aux2_sensitivity %>%
  filter(aux2_megapixels == 200.5) %>%
  pull(Factor_CubeRoot)

# Controllo
aux2_64_factor
aux2_64_log
aux2_64_sqrt
aux2_64_cube

aux2_200_factor
aux2_200_log
aux2_200_sqrt
aux2_200_cube

# ------------------------------------------------------------
# ASSIGN AUX2 SENSITIVITY FACTORS
# ------------------------------------------------------------

phones_sensitivity <- phones %>%
  mutate(
    
    AUX2_Linear = case_when(
      Released.Year == 2024 &
        grepl("X100 Ultra|X200 Pro", Model, ignore.case = TRUE) ~
        aux2_200_factor,
      
      Released.Year == 2024 &
        grepl("Find X7", Model, ignore.case = TRUE) ~
        aux2_64_factor,
      
      Released.Year == 2024 ~ 1,
      TRUE ~ NA_real_
    ),
    
    AUX2_Log = case_when(
      Released.Year == 2024 &
        grepl("X100 Ultra|X200 Pro", Model, ignore.case = TRUE) ~
        aux2_200_log,
      
      Released.Year == 2024 &
        grepl("Find X7", Model, ignore.case = TRUE) ~
        aux2_64_log,
      
      Released.Year == 2024 ~ 1,
      TRUE ~ NA_real_
    ),
    
    AUX2_Sqrt = case_when(
      Released.Year == 2024 &
        grepl("X100 Ultra|X200 Pro", Model, ignore.case = TRUE) ~
        aux2_200_sqrt,
      
      Released.Year == 2024 &
        grepl("Find X7", Model, ignore.case = TRUE) ~
        aux2_64_sqrt,
      
      Released.Year == 2024 ~ 1,
      TRUE ~ NA_real_
    ),
    
    AUX2_CubeRoot = case_when(
      Released.Year == 2024 &
        grepl("X100 Ultra|X200 Pro", Model, ignore.case = TRUE) ~
        aux2_200_cube,
      
      Released.Year == 2024 &
        grepl("Find X7", Model, ignore.case = TRUE) ~
        aux2_64_cube,
      
      Released.Year == 2024 ~ 1,
      TRUE ~ NA_real_
    )
  )

# CONTROL
phones_sensitivity %>%
  filter(Released.Year == 2024) %>%
  summarise(
    N = n(),
    Linear_NA = sum(is.na(AUX2_Linear)),
    Log_NA = sum(is.na(AUX2_Log)),
    Sqrt_NA = sum(is.na(AUX2_Sqrt)),
    CubeRoot_NA = sum(is.na(AUX2_CubeRoot)),
    Linear_max = max(AUX2_Linear, na.rm = TRUE),
    Log_max = max(AUX2_Log, na.rm = TRUE),
    Sqrt_max = max(AUX2_Sqrt, na.rm = TRUE),
    CubeRoot_max = max(AUX2_CubeRoot, na.rm = TRUE)
  )




# ------------------------------------------------------------
# CAMERA INDEX — FULL SENSITIVITY ANALYSIS
# ------------------------------------------------------------

phones_sensitivity <- phones_sensitivity %>%
  mutate(
    
    Camera_Index_Linear =
      Camera_Main_Frontier_Factor *
      Camera_Selfie_Frontier_Factor *
      Camera_AUX_Aperture_Factor *
      AUX2_Linear *
      Camera_Dual_Periscopic_Factor *
      Camera_Stepless_Innovation,
    
    Camera_Index_Log =
      Camera_Main_Frontier_Factor *
      Camera_Selfie_Frontier_Factor *
      Camera_AUX_Aperture_Factor *
      AUX2_Log *
      Camera_Dual_Periscopic_Factor *
      Camera_Stepless_Innovation,
    
    Camera_Index_Sqrt =
      Camera_Main_Frontier_Factor *
      Camera_Selfie_Frontier_Factor *
      Camera_AUX_Aperture_Factor *
      AUX2_Sqrt *
      Camera_Dual_Periscopic_Factor *
      Camera_Stepless_Innovation,
    
    Camera_Index_CubeRoot =
      Camera_Main_Frontier_Factor *
      Camera_Selfie_Frontier_Factor *
      Camera_AUX_Aperture_Factor *
      AUX2_CubeRoot *
      Camera_Dual_Periscopic_Factor *
      Camera_Stepless_Innovation
  )

# ------------------------------------------------------------
# DISTRIBUTION
# ------------------------------------------------------------

camera_sensitivity_distribution <- phones_sensitivity %>%
  filter(Released.Year == 2024) %>%
  summarise(
    
    N = n(),
    
    Mean_Linear = mean(Camera_Index_Linear),
    Median_Linear = median(Camera_Index_Linear),
    SD_Linear = sd(Camera_Index_Linear),
    Max_Linear = max(Camera_Index_Linear),
    
    Mean_Log = mean(Camera_Index_Log),
    Median_Log = median(Camera_Index_Log),
    SD_Log = sd(Camera_Index_Log),
    Max_Log = max(Camera_Index_Log),
    
    Mean_Sqrt = mean(Camera_Index_Sqrt),
    Median_Sqrt = median(Camera_Index_Sqrt),
    SD_Sqrt = sd(Camera_Index_Sqrt),
    Max_Sqrt = max(Camera_Index_Sqrt),
    
    Mean_CubeRoot = mean(Camera_Index_CubeRoot),
    Median_CubeRoot = median(Camera_Index_CubeRoot),
    SD_CubeRoot = sd(Camera_Index_CubeRoot),
    Max_CubeRoot = max(Camera_Index_CubeRoot)
  )

camera_sensitivity_distribution

# ------------------------------------------------------------
# CAMERA INDEX — RANKING SENSITIVITY
# ------------------------------------------------------------

ranking_sensitivity <- phones_sensitivity %>%
  filter(Released.Year == 2024) %>%
  select(
    Brand,
    Model,
    Camera_Index_Linear,
    Camera_Index_Log,
    Camera_Index_Sqrt,
    Camera_Index_CubeRoot
  ) %>%
  mutate(
    Rank_Linear = rank(
      -Camera_Index_Linear,
      ties.method = "min"
    ),
    
    Rank_Log = rank(
      -Camera_Index_Log,
      ties.method = "min"
    ),
    
    Rank_Sqrt = rank(
      -Camera_Index_Sqrt,
      ties.method = "min"
    ),
    
    Rank_CubeRoot = rank(
      -Camera_Index_CubeRoot,
      ties.method = "min"
    )
  )

# Spearman correlations between rankings
ranking_correlations <- cor(
  ranking_sensitivity %>%
    select(
      Camera_Index_Linear,
      Camera_Index_Log,
      Camera_Index_Sqrt,
      Camera_Index_CubeRoot
    ),
  method = "spearman"
)

ranking_correlations



# ------------------------------------------------------------
# CAMERA CORE INNOVATION INDEX
# ------------------------------------------------------------

phones_CCI <- phones_sensitivity %>%
  mutate(
    
    # Core Camera Innovation Index
    Camera_Core_Innovation_Index =
      (
        Camera_Main_Frontier_Factor *
          Camera_Selfie_Frontier_Factor *
          Camera_AUX_Aperture_Factor *
          AUX2_Log
      )^(1/4)
  )

# ------------------------------------------------------------
# DISTRIBUTION — 2024
# ------------------------------------------------------------

CCI_distribution <- phones_CCI %>%
  filter(Released.Year == 2024) %>%
  summarise(
    N = n(),
    Mean = mean(Camera_Core_Innovation_Index),
    Median = median(Camera_Core_Innovation_Index),
    SD = sd(Camera_Core_Innovation_Index),
    Min = min(Camera_Core_Innovation_Index),
    P25 = quantile(Camera_Core_Innovation_Index, 0.25),
    P75 = quantile(Camera_Core_Innovation_Index, 0.75),
    P90 = quantile(Camera_Core_Innovation_Index, 0.90),
    P95 = quantile(Camera_Core_Innovation_Index, 0.95),
    Max = max(Camera_Core_Innovation_Index)
  )

CCI_distribution

# ------------------------------------------------------------
# FINAL CAMERA INNOVATION INDEX
# ------------------------------------------------------------

phones_final_camera <- phones_sensitivity %>%
  mutate(
    
    # Definitive AUX2 factor: logarithmic transformation
    Camera_AUX2_Frontier_Factor =
      AUX2_Log,
    
    # Core Camera Innovation Index
    Camera_Core_Innovation_Index =
      (
        Camera_Main_Frontier_Factor *
          Camera_Selfie_Frontier_Factor *
          Camera_AUX_Aperture_Factor *
          Camera_AUX2_Frontier_Factor
      )^(1/4),
    
    # Final Camera Innovation Index
    Camera_Innovation_Index =
      Camera_Core_Innovation_Index *
      Camera_Stepless_Innovation *
      Camera_Dual_Periscopic_Factor
  )

# ------------------------------------------------------------
# CONTROL — 2024
# ------------------------------------------------------------

camera_final_control <- phones_final_camera %>%
  filter(Released.Year == 2024) %>%
  summarise(
    N = n(),
    NA_Camera_Index = sum(is.na(Camera_Innovation_Index)),
    Mean = mean(Camera_Innovation_Index),
    Median = median(Camera_Innovation_Index),
    SD = sd(Camera_Innovation_Index),
    Min = min(Camera_Innovation_Index),
    P25 = quantile(Camera_Innovation_Index, 0.25),
    P75 = quantile(Camera_Innovation_Index, 0.75),
    P90 = quantile(Camera_Innovation_Index, 0.90),
    P95 = quantile(Camera_Innovation_Index, 0.95),
    Max = max(Camera_Innovation_Index)
  )

camera_final_control

# ------------------------------------------------------------
# TOP 20 — FINAL CAMERA INNOVATION INDEX
# ------------------------------------------------------------

camera_final_top20 <- phones_final_camera %>%
  filter(Released.Year == 2024) %>%
  arrange(desc(Camera_Innovation_Index)) %>%
  select(
    Brand,
    Model,
    Camera_Core_Innovation_Index,
    Camera_Stepless_Innovation,
    Camera_Dual_Periscopic_Factor,
    Camera_Innovation_Index
  ) %>%
  slice_head(n = 20)

camera_final_top20

camera_df <- phones_final_camera |>
  filter(Released.Year == "2024") |>
  select(
    Brand,
    Model,
    model_std,
    Released.Year,
    Camera_Core_Innovation_Index
  )

camera_df <- camera_df |>
  mutate(
    # Passo 1: Calcoliamo il logaritmo solo dove Camera_Core_Innovation_Index > 0
    log_temp = case_when(
      is.na(Camera_Core_Innovation_Index) ~ NA_real_,
      Camera_Core_Innovation_Index == 0   ~ 0,
      Camera_Core_Innovation_Index > 0    ~ log(((Camera_Core_Innovation_Index - 1) * 100) + 1)
    ),
    
    # Passo 2: Applichiamo il Min-Max escludendo gli 0 e i NA dal calcolo di min/max
    Camera_Innovation_log_norm = case_when(
      is.na(log_temp) ~ NA_real_,
      log_temp == 0   ~ 0,
      log_temp > 0    ~ (log_temp - 0) / 
        (max(log_temp[log_temp > 0], na.rm = TRUE) - 0) * 100
    )
  )

camera_innovation_df <- camera_df |>
  select(-Camera_Core_Innovation_Index, -log_temp)

saveRDS(camera_innovation_df, "Camera Innovation Index.rds")

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