# === Rstudio ===

# Package
```
library(geosphere)
```

# Pass way directory
```
dossier <- "C:/Users/UTILISATEUR/Documents/cours/M2/S10/Stage/Stage_M2/Mars/Dispersion/Dispersion_active/2022/Avril"
fichiers <- list.files(path = dossier, pattern = "^Position.*\\.csv$", full.names = TRUE)
```

# Cadre de l'étude
```
lon_min <- -5.6
lon_max <- -1.3
lat_min <- 47.04
lat_max <- 49.93
```

# Coordonnées de chacun des points d'émission
```
coord_ref <- data.frame(
  Nom = c("BI", "LG", "SE", "GB", "OU", "FN", "IV", "BA", "PT", "SI"),
  Longitude = c(-3.06, -4, -5.22, -4.59, -5.18, -4.78, -4.65, -4.02, -3.77, -3.44),
  Latitude = c(47.18, 47.67, 48.03, 48.32, 48.45, 48.8, 48.69, 48.78, 48.92, 48.92)
)
```

# Définir le rayon de connectivité autour des points d'émission (en mètre)
```
rayon <- 10000 
```

# Fonction pour faire un tri des fichiers vides
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

# Savoir combien de fichiers a des informations
```
fichiers_valides <- fichiers[sapply(fichiers, fichier_valide)]
cat(length(fichiers_valides), "fichiers valides trouvés sur", length(fichiers), "\n")
```

# Fonction pour traiter un fichier donné
```
analyse_fichier <- function(nom_fichier) {
  data <- read.csv(nom_fichier, header = FALSE, skip = 1, fill = TRUE, sep = ",")
  data <- data[, c(4,5)]
  colnames(data) <- c("Latitude", "Longitude")
  
  hors_cadre <- data[data$Longitude < lon_min | data$Longitude > lon_max |
                       data$Latitude < lat_min | data$Latitude > lat_max |
                       data$Longitude == lon_min | data$Longitude == lon_max |
                       data$Latitude == lat_min | data$Latitude == lat_max, ]
  
  pourcentage_hors_cadre <- (nrow(hors_cadre) / nrow(data)) * 100
  
  resultats <- data.frame(
    Fichier = basename(nom_fichier),
    Particules_Hors_Cadre = nrow(hors_cadre),
    Pourcentage_Hors_Cadre = round(pourcentage_hors_cadre, 2)
  )
  
  detecte_proximite <- function(df, ref_coords, rayon) {
    connexions <- data.frame()
    zones_count <- list()
    
    for (i in 1:nrow(ref_coords)) {
      distances <- distHaversine(df[, c("Longitude", "Latitude")], ref_coords[i, c("Longitude", "Latitude")])
      proches <- df[distances <= rayon, ]
      if (nrow(proches) > 0) {
        proches$Ref_Nom <- ref_coords$Nom[i]
        proches$Rayon <- rayon
        connexions <- rbind(connexions, proches)
        zone_nom <- ref_coords$Nom[i]
        zones_count[[zone_nom]] <- nrow(proches)
      }
    }
    
    zones_connectees_unique <- character()
    for (zone in names(zones_count)) {
      particules_zone <- zones_count[[zone]]
      zones_connectees_unique <- c(zones_connectees_unique, paste0(particules_zone, zone))
    }
    
    zones_connectees_str <- paste(zones_connectees_unique, collapse = ", ")
    
    return(list(connexions = connexions, zones = zones_connectees_str))
  }
  
  # Analyse pour le rayon de 10 000 m
  resultat_proximite <- detecte_proximite(data, coord_ref, rayon)
  particules_connectees <- resultat_proximite$connexions
  zones_connectees <- resultat_proximite$zones
  
  dans_proximite <- nrow(particules_connectees)
  pourcentage_proximite <- (dans_proximite / nrow(data)) * 100
  
  # Ajouter les résultats au tableau
  resultats[paste0("Particules_", rayon, "m")] <- dans_proximite
  resultats[paste0("Pourcentage_", rayon, "m")] <- round(pourcentage_proximite, 2)
  resultats[paste0("Zones_", rayon, "m")] <- zones_connectees
  
  return(resultats)
}
```

# Traiter tous les fichiers valides
```
tableau_final <- do.call(rbind, lapply(fichiers_valides, analyse_fichier))
```

# Sauvegarder les résultats en csv
```
write.table(tableau_final,  file.path(dossier,"resultats_connectivite_stade7.csv"), sep = ";", row.names = FALSE)
```

# Afficher un aperçu du tableau final pour avoir une idée du rendu
```
print(head(tableau_final))
```
