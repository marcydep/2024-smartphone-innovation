library(tidyverse)

cartella_csv <- "C:/Users/marcy/Desktop/Scraping dei telefoni"

file_list <- list.files(path = cartella_csv, pattern = "\\.csv$", full.names = TRUE)

leggi_e_rinomina <- function(file_path) {
  df <- read_csv(file_path, show_col_types = FALSE)
  nome_file <- tools::file_path_sans_ext(basename(file_path))
  nome_smartphone <- str_remove(nome_file, "_\\d+$")

  df_modificato <- df %>%
    rename(
      review_url = Link_Titolo,
      reviewer = Nome,
      rating = aiconalt,
      review_date = Visualizzazione,
      review_text = Visualizzazione1
    ) %>%
    mutate(Smartphone = nome_smartphone) %>%
    relocate(Smartphone) 
  
  return(df_modificato)
}

dataset_finale <- map(file_list, leggi_e_rinomina) %>%
  bind_rows() %>%
  group_by(Smartphone) %>%
  slice_head(n = 100) %>%
  ungroup()

write_csv(dataset_finale, file.path(cartella_csv, "amazon_reviews_master.csv"))

cat("Fatto! File unico creato con successo.\n")

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