# === Rstudio === 
# Packages
``` 
library(geosphere)
library(dplyr)
library(viridis)
library(reshape2)
library(ggplot2)
library(lubridate)
```

# Ficher à combiner obtenu à l'aide du code "connectivité stage 7"
fichiers_entree <- list.files(path = "C:/Users/UTILISATEUR/Documents/cours/M2/S10/Stage/Stage_M2/Mars/Dispersion/Dispersion_active/2022/Matrice_2022", pattern = "^resultats.*\\.csv$", full.names = TRUE)
repertoire_sauvegarde <- "C:/Users/UTILISATEUR/Documents/cours/M2/S10/Stage/Stage_M2/Mars/Dispersion/Dispersion_active/2022/Matrice_2022/"

# Charger et combiner tous les fichiers
tableaux_list <- lapply(fichiers_entree, function(fichier) {
  read.table(fichier, sep = ";", header = TRUE)
})

tableau_final <- bind_rows(tableaux_list)

# Définir les zones de référence
zones_ref <- c("BI", "LG", "SE", "GB", "OU", "FN", "IV", "BA", "PT", "SI")
ordre_fichiers <- zones_ref

# Ajouter la localisation
tableau_final$Localisation <- sapply(strsplit(tableau_final$Fichier, "_"), function(x) x[3])

# Rayon d’analyse
rayons <- c(10000)

# === Fonctions inchangées mais adaptées === #

generer_matrices_connectivite <- function(rayons, zones_ref, tableau_final) {
  list_matrices_12648 <- list()
  list_matrices_total <- list()
  
  for (rayon in rayons) {
    matrice_12648 <- matrix(0, nrow = nrow(tableau_final), ncol = length(zones_ref),
                            dimnames = list(tableau_final$Fichier, zones_ref))
    
    matrice_total <- matrix(0, nrow = nrow(tableau_final), ncol = length(zones_ref),
                            dimnames = list(tableau_final$Fichier, zones_ref))
    
    for (i in 1:nrow(tableau_final)) {
      zones_text <- as.character(tableau_final[i, paste0("Zones_", rayon, "m")])
      pourcentage_total <- tableau_final[i, paste0("Pourcentage_", rayon, "m")]
      
      if (!is.na(zones_text) && zones_text != "") {
        zones_connexes <- unlist(strsplit(zones_text, ","))
        
        details_zones <- lapply(zones_connexes, function(zone) {
          matches <- regmatches(zone, regexec("([0-9]+)([A-Z]+)", zone))
          if (length(matches[[1]]) == 3) {
            return(list(count = as.numeric(matches[[1]][2]), zone = matches[[1]][3]))
          }
          return(NULL)
        })
        
        details_zones <- details_zones[!sapply(details_zones, is.null)]
        
        if (length(details_zones) > 0) {
          total_particles_detected <- sum(sapply(details_zones, function(x) x$count))
          
          for (detail in details_zones) {
            if (detail$zone %in% zones_ref) {
              colonne <- which(zones_ref == detail$zone)
              
              matrice_12648[i, colonne] <- (detail$count / 12648) * pourcentage_total
              matrice_total[i, colonne] <- (detail$count / total_particles_detected) * 100
            }
          }
        }
      }
    }
    
    list_matrices_12648[[paste0(rayon, "m")]] <- matrice_12648
    list_matrices_total[[paste0(rayon, "m")]] <- matrice_total
  }
  
  return(list(avec_12648 = list_matrices_12648,
              avec_total = list_matrices_total))
}

# Fusion uniquement par Localisation (plus de Mois)
fusionner_matrices_par_localisation <- function(matrices_connectivite, tableau_final, zones_ref) {
  list_matrices_fusionnees <- list()
  
  # Retirer GB comme localisation d’émission
  localisations_emission <- setdiff(ordre_fichiers, "GB")
  
  for (rayon in names(matrices_connectivite)) {
    matrice <- matrices_connectivite[[rayon]]
    tableau_final$Fichier <- rownames(matrice)
    
    tableau_complet <- cbind(tableau_final, matrice)
    
    grouped_existants <- tableau_complet %>%
      group_by(Localisation) %>%
      summarise(across(all_of(zones_ref), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
    
    # Ne garder que les localisations autorisées
    grouped <- grouped_existants %>%
      filter(Localisation %in% localisations_emission) %>%
      mutate(Localisation = factor(Localisation, levels = localisations_emission)) %>%
      arrange(Localisation)
    
    # Garder les colonnes correspondant aux zones d’arrivée
    colonnes_ordonnee <- intersect(zones_ref, colnames(grouped))
    matrice_fusionnee <- as.matrix(grouped[, colonnes_ordonnee])
    
    rownames(matrice_fusionnee) <- grouped$Localisation
    
    list_matrices_fusionnees[[rayon]] <- matrice_fusionnee
  }
  
  return(list_matrices_fusionnees)
}

# Générer les matrices
matrices_connectivite <- generer_matrices_connectivite(rayons, zones_ref, tableau_final)
matrices_connectivite_fichiers_12648 <- matrices_connectivite$avec_12648
matrices_connectivite_fichiers_total <- matrices_connectivite$avec_total

# Fusionner par Localisation (pas de Mois)
matrices_connectivite_fusionnees_12648 <- fusionner_matrices_par_localisation(matrices_connectivite_fichiers_12648, tableau_final, zones_ref)
matrices_connectivite_fusionnees_total <- fusionner_matrices_par_localisation(matrices_connectivite_fichiers_total, tableau_final, zones_ref)

# Fonction pour créer la heatmap
create_heatmap_fusionnee <- function(matrice, rayon, suffixe) {
  matrice_long <- melt(matrice)
  colnames(matrice_long) <- c("Localisation", "Zone", "Pourcentage")
  
  # CORRECTION : conserver les noms de zones et localisations
  matrice_long$Zone <- factor(matrice_long$Zone, levels = colnames(matrice))
  matrice_long$Localisation <- factor(matrice_long$Localisation, levels = rownames(matrice))
  
  heatmap_plot <- ggplot(matrice_long, aes(x = Zone, y = Localisation, fill = Pourcentage)) +
    geom_tile() +
    scale_fill_gradientn(colors = c("white", "#fee833", "#6cdb92", "#26828e","#482777", "black"),
                       values = c(0, 0.02, 0.05, 0.10, 0.50, 1),
                       limits = c(0, 100)) +
    theme_minimal() +
    labs(title = paste("Connectivité par localisation (", rayon, "m - ", suffixe, ")", sep = ""),
         x = "Zone d'arrivée", y = "Zone d'émission", fill = "Pourcentage") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  file_name <- paste0("heatmap_connectivite_", suffixe, "_loc_", rayon, "m.png")
  
  print(paste("Heatmap enregistrée sous", file_name))
  return(heatmap_plot)
}

# Afficher les heatmaps
for (rayon in rayons) {
  print(create_heatmap_fusionnee(matrices_connectivite_fusionnees_12648[[paste0(rayon, "m")]], rayon, "12648"))
  print(create_heatmap_fusionnee(matrices_connectivite_fusionnees_total[[paste0(rayon, "m")]], rayon, "total"))
}

# === Normalisation par colonne (colonnes = 100%) ===
normaliser_par_colonne <- function(matrice) {
  col_sums <- colSums(matrice, na.rm = TRUE)
  sweep(matrice, 2, col_sums, FUN = function(x, s) ifelse(s == 0, 0, x / s * 100))
}


matrice_total <- matrices_connectivite_fusionnees_total[[paste0(10000, "m")]]
matrice_colnorm <- normaliser_par_colonne(matrice_total)

rownames(matrice_colnorm) <- rownames(matrice_total)
colnames(matrice_colnorm) <- zones_ref

write.table(matrice_colnorm,
            file.path(repertoire_sauvegarde, "matrice_connectivite_fusionnee_total_colnorm_loc_10000.csv"),
            sep = ";", row.names = TRUE, col.names = NA, quote = FALSE)

# Heatmap normalisée
matrice_long_colnorm <- melt(matrice_colnorm)
colnames(matrice_long_colnorm) <- c("Localisation", "Zone", "Pourcentage")

heatmap_colnorm <- ggplot(matrice_long_colnorm, aes(x = Zone, y = Localisation, fill = Pourcentage)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("white", "#fee833", "#6cdb92", "#26828e","#482777", "black"),
                       values = c(0, 0.01, 0.1, 0.25, 0.50, 1),
                       limits = c(0, 100)) +
  theme_minimal() +
  labs(x = "Sites d'arrivée", y = "Sites d'émission", fill = "Connectivité (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Enregistrer la heatmap normalisée
ggsave(file.path(repertoire_sauvegarde, "heatmap_connectivite_colnorm_total_loc_10000_2023.png"), plot = heatmap_colnorm, width = 8, height = 6, dpi = 300)

print(heatmap_colnorm)
