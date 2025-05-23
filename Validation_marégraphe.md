# === Rstudio ===

# Packages
```
library(ggplot2)
library(dplyr)
```

Télécharger au préalable les données de marée sur le SHOM 
```
maree_ref <- read.csv("Port_Tudy_09-2021.csv", sep = ";", skip = 2, header = FALSE)
```

# Renommer les colonnes pour plus de clarté
```
colnames(maree_ref) <- c("Date", "Heure", "Hauteur", "Source")
```

# Combiner 'Date' et 'Heure' en une seule colonne datetime
```
maree_ref$Datetime <- as.POSIXct(paste(maree_ref$Date, maree_ref$Heure), format = "%d/%m/%Y %H:%M:%S")
```

# Conversion de la hauteur en numérique
```
maree_ref$Hauteur <- as.numeric(maree_ref$Hauteur)
```

# Création du graphique
```
ggplot(maree_ref, aes(x = Datetime, y = Hauteur)) +
  geom_line(color = "blue") +
  labs(title = "Marégraphe - Hauteur de marée au cours du temps (Brest)",
       x = "Temps",
       y = "Hauteur de marée (m)") +
  theme_minimal()
```
#######################################
Ajouter le fichier de marée modélisé 
```
maree_mod <- read.csv("Port_Tudy_09-2021_MOD.csv", sep = ";", skip = 2, header = FALSE)
```
# Renommer les colonnes pour plus de clarté
```
colnames(maree_mod) <- c("Date", "Heure", "Hauteur", "Source")
```

# Combiner 'Date' et 'Heure' en une seule colonne datetime
```
maree_mod$Datetime <- as.POSIXct(paste(maree_mod$Date, maree_mod$Heure), format = "%d/%m/%Y %H:%M:%S")
```

# Conversion de la hauteur en numérique
```
maree_mod$Hauteur <- as.numeric(maree_mod$Hauteur)
maree_mod <- na.omit(maree_mod)
```

# Création du graphique
```
ggplot(maree_mod, aes(x = Datetime, y = Hauteur)) +
  geom_line(color = "blue") +
  labs(title = "Marégraphe - Hauteur de marée modélisé au cours du temps (Brest)",
       x = "Temps",
       y = "Hauteur de marée (m)") +
  theme_minimal()
```

####################
Fusionner les deux tableaux de données par heur

# Fusionner les deux datasets en fonction de Nbr_heure
```
maree_combine <- merge(maree_ref, maree_mod, by = "Nbr_heure", suffixes = c("_ref", "_mod"))
```

# Calcul de la différence entre les hauteurs
```
maree_combine$Diff_Hauteur <- abs(maree_combine$Hauteur_mod - maree_combine$Hauteur_centree)
```

# Moyenne des différences de hauteur
```
moyenne_diff <- mean(maree_combine$Diff_Hauteur, na.rm = TRUE)
print(paste("Moyenne des différences de hauteur:", round(moyenne_diff, 2), "m"))
```

# Calcul de l'amplitude des marées
```
amplitude_maree <- max(maree_combine$Hauteur_centree, na.rm = TRUE) - min(maree_combine$Hauteur_centree, na.rm = TRUE)
```

# Calcul du rapport entre la moyenne des différences et l'amplitude
```
rapport_diff_amplitude <- (moyenne_diff / amplitude_maree) * 100
```

# Affichage du résultat
```
print(paste("La différence moyenne représente", round(rapport_diff_amplitude, 2), "% de l'amplitude des marées."))
```

# Faire un zoom sur 48h
```
min_time <- min(maree_combine$Nbr_heure, na.rm = TRUE)  # Prend le premier Nbr_heure
max_time <- min_time + 48
```

# Filtrer les données sur cette période
```
maree_zoom <- maree_combine %>% filter(Nbr_heure >= min_time & Nbr_heure <= max_time)
```

# Tracé des deux courbes sur le même graphe
```
ggplot() +
  geom_line(data = maree_combine, aes(x = Nbr_heure, y = Hauteur_centree, color = "Référence"), size = 1) +
  geom_line(data = maree_combine, aes(x = Nbr_heure, y = Hauteur_mod, color = "Modèle"), size = 1) +
  labs(title = "Marégraphe de Roscoff septembre",
    x = "Temps (heures depuis le début de l'année)",
       y = "Hauteur de marée (m)",
       color = "Légende") +
  scale_color_manual(values = c("Référence" = "blue", "Modèle" = "red")) +
  theme_minimal()


ggplot(maree_combine, aes(x = Nbr_heure, y = Diff_Hauteur)) +
  geom_bar(stat = "identity", fill = "purple", alpha = 0.7) +
  geom_hline(yintercept = moyenne_diff, linetype = "dashed", color = "red", size = 1) +
  labs(title = "Différences absolues entre modèle et référence",
       x = "Temps (heures depuis le début de l'année)",
       y = "Différence absolue (m)") +
  annotate("text", x = max(maree_combine$Nbr_heure), y = moyenne_diff + 0.05, 
           label = paste("Moyenne =", round(moyenne_diff, 2), "m"), color = "red", hjust = 1) +
  theme_minimal()

ggplot() +
  geom_line(data = maree_zoom, aes(x = Nbr_heure, y = Hauteur_centree, color = "Référence"), size = 1) +
  geom_line(data = maree_zoom, aes(x = Nbr_heure, y = Hauteur_mod, color = "Modèle"), size = 1) +
  labs(x = "Temps (heures depuis le début de l'année)",
       y = "Hauteur de marée (m)",
       color = "Légende") +
  scale_color_manual(values = c("Référence" = "blue", "Modèle" = "red")) +
  theme_minimal()
```
