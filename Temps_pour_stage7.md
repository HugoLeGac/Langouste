# === Rstudio ===

# Packages
```
library(dplyr)
library(stringr)
library(ggplot2)
```

# Pass way directory
```
folder_path <- "C:/Users/UTILISATEUR/Documents/cours/M2/S10/Stage/Stage_M2/Mars/Dispersion/Dispersion_active/2022/Juin"  
file_list <- list.files(path = folder_path, pattern = "^Position.*\\.csv$", full.names = TRUE)
```

# Fonction pour voir si le fichier n'est pas vide 
```
fichier_valide <- function(f) {
  lignes <- tryCatch({
    nrow(read.csv(f, header = FALSE, skip = 1, fill = TRUE, sep = ","))
  }, error = function(e) {
    return(0)
  })
  return(lignes > 0)
}
```

# Ne garder que les fichiers valides
```
fichiers_valides <- file_list[sapply(file_list, fichier_valide)]
```

# Fonction de traitement
```
process_file <- function(file_path) {
  file_name <- basename(file_path)
  location <- str_extract(file_name, "(?<=stage7_)[A-Z]+(?=_202)")
  
  df <- read.csv(file_path)
  df$location <- location
  return(df)
}

all_data <- lapply(fichiers_valides, process_file) %>% bind_rows()
```

# Moyenne de la colonne "time_index" par localisation
```
result <- all_data %>%
  group_by(location) %>%
  summarise(
    mean_time_index = mean(time_index, na.rm = TRUE),
    n_particles = sum(!is.na(time_index))
  ) %>%
  bind_rows(
    all_data %>%
      summarise(
        location = "TOTAL",
        mean_time_index = mean(time_index, na.rm = TRUE),
        n_particles = sum(!is.na(time_index))
      )
  )


print(result)
```
