# === Rstudio ===

# Packages
```
library(tidyverse)
library(readr)
library(stringr)
library(ggsignif)
library(FSA)
library(multcompView)
```

# Pass way directory
```
dossier <- "C:/Users/UTILISATEUR/Documents/cours/M2/S10/Stage/Stage_M2/Mars/Dispersion/Dispersion_active/2022/Zone_2022"
fichiers <- list.files(path = dossier, pattern = "^Repartition.*\\.csv$", full.names = TRUE)
```

# Lire et concaténer tous les fichiers
```
resultats <- list()

for (fichier in fichiers) {
  nom_fichier <- basename(fichier)
  
  # Extraire le site entre "stage7_" et "_DATE"
  site <- str_extract(nom_fichier, "(?<=stage7_)[A-Z]{2}")
  
  # Lecture avec séparateur ";"
  df <- read_delim(fichier, delim = ";", show_col_types = FALSE)
  
  # Vérifie s'il y a des lignes de données
  if (nrow(df) == 0) {
    message(paste("⚠️ Fichier vide (aucune ligne de données) :", nom_fichier))
    next
  }
  
  # Vérifie si les colonnes nécessaires sont là
  if (!all(c("zone", "inf_40m", "sup_40m") %in% names(df))) {
    message(paste("⚠️ Colonnes manquantes dans :", nom_fichier))
    next
  }
  
  # Sélectionner et renommer les colonnes
  df_simplifie <- df %>%
    select(zone, `inf_40m`, `sup_40m`) %>%
    rename(
      Zone = zone,
      Particules_inf_40m = `inf_40m`,
      Particules_sup_40m = `sup_40m`
    ) %>%
    mutate(Site = site)
  
  resultats[[length(resultats) + 1]] <- df_simplifie
}
```

# Fusionner les fichiers
```
df_final <- bind_rows(resultats)
```

# Calculer la quantité totale de particules par zone et station
```
df_site_zone <- df_final %>%
  group_by(Zone, Site) %>%
  summarise(
    Particules_inf_40m = sum(Particules_inf_40m, na.rm = TRUE),
    Particules_sup_40m = sum(Particules_sup_40m, na.rm = TRUE)
  ) %>%
  mutate(Total_particules = Particules_inf_40m + Particules_sup_40m) %>%
  ungroup()
```

# Totaux par zone pour les proportions internes à chaque zone
```
df_zone_totals <- df_site_zone %>%
  group_by(Zone) %>%
  summarise(Total_zone = sum(Total_particules))
```

# Fusionner les totaux par zone avec les données par station
```
df_site_zone <- df_site_zone %>%
  left_join(df_zone_totals, by = "Zone") %>%
  mutate(Proportion = Total_particules / Total_zone)
```

# Test statistique : Kruskal-Wallis pour entre chaque site
```
p_values <- df_site_zone %>%
  group_by(Zone) %>%
  filter(n_distinct(Site) > 1) %>%  # Zones avec plus d'une station
  summarise(
    p_value = kruskal.test(Total_particules ~ Site)$p.value
  )

df_site_zone$Zone <- factor(df_site_zone$Zone,
                                  levels = c("Sud_Bretagne", "Iroise", "Manche_ouest", "Nord_Bretagne", "Golfe_Normano-Breton"))

df_site_zone$Site <- factor(df_site_zone$Site,
                                  levels = c("SI", "PT", "BA", "IV", "FN", "OU", "SE", "LG", "BI"))

palette_sites <- c(
  "SI" = "#fee833",
  "PT" = "#b7e26b",
  "BA" = "#6cdb92",
  "IV" = "#1f9e89",
  "FN" = "#26828e",
  "OU" = "#31688e",
  "SE" = "#3e4a89",
  "LG" = "#482777",
  "BI" = "#440154"
)

ggplot(df_site_zone, aes(x = Zone, y = Total_particules, fill = Site)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
  labs(title = "Répartition des particules par zone et station",
       x = "Zone", y = "Nombre total de particules") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = palette_sites)
```

# Calculer la proportion globale de chaque
```
total_global <- sum(df_site_zone$Total_particules, na.rm = TRUE)

df_global_pourcent <- df_site_zone %>%
  mutate(Proportion_globale = 100 * Total_particules / total_global)

df_global_pourcent$Zone <- factor(df_global_pourcent$Zone,
                                  levels = c("Sud_Bretagne", "Iroise", "Manche_ouest", "Nord_Bretagne", "Golfe_Normano-Breton"))

df_global_pourcent$Site <- factor(df_global_pourcent$Site,
                                  levels = c("SI", "PT", "BA", "IV", "FN", "OU", "SE", "LG", "BI"))

palette_sites <- c(
  "SI" = "#fee833",
  "PT" = "#b7e26b",
  "BA" = "#6cdb92",
  "IV" = "#1f9e89",
  "FN" = "#26828e",
  "OU" = "#31688e",
  "SE" = "#3e4a89",
  "LG" = "#482777",
  "BI" = "#440154"
)

ggplot(df_global_pourcent, aes(x = Zone, y = Proportion_globale, fill = Site)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
  labs(x = "Zone", y = "Nombre de larves (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = palette_sites)
```

# Différence de répartition entre les zones 
```
test_zone <- kruskal.test(Total_particules ~ Zone, data = df_site_zone)
print(paste("p-value Kruskal-Wallis entre zones :", test_zone$p.value))
```

# Test de Dunn post-hoc avec correction de Benjamini-Hochberg 
```
dunn_resultats <- dunnTest(Total_particules ~ Zone, data = df_site_zone, method = "bh")
print(dunn_resultats)

p_values <- dunn_resultats$res$P.adj
comparaisons <- dunn_resultats$res$Comparison
groupes <- unique(unlist(strsplit(comparaisons, " - ")))
p_matrix <- matrix(1, nrow = length(groupes), ncol = length(groupes),
                   dimnames = list(groupes, groupes))

for (i in 1:length(p_values)) {
  groupes_pair <- unlist(strsplit(comparaisons[i], " - "))
  p_matrix[groupes_pair[1], groupes_pair[2]] <- p_values[i]
  p_matrix[groupes_pair[2], groupes_pair[1]] <- p_values[i]
}
```

# Génération des lettres homogènes 
```
lettres <- multcompLetters(p_matrix < 0.05)$Letters

lettres_df <- data.frame(Zone = names(lettres), Label = lettres)
print(lettres_df)
```

# Boxplot des totaux de particules par zone avec lettres
```
ggplot(df_site_zone, aes(x = Zone, y = Total_particules, fill = Zone)) +
  geom_boxplot(alpha = 0.6) +
  theme_minimal() +
  labs(x = "Zone", y = "Total de particules par site") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +  # 🔹 Suppression de la légende
  geom_jitter(width = 0.2, size = 2, alpha = 0.7) +
  geom_text(data = lettres_df, aes(x = Zone, y = max(df_site_zone$Total_particules) * 1.05, label = Label),
            size = 5, color = "black")

```

# Récupération du tableau de résultats
```
dunn_table <- dunn_resultats$res

comparaisons_significatives <- dunn_table %>% filter(P.adj < 0.05)

print("Comparaisons significatives entre zones (p.adj < 0.05) :")
print(comparaisons_significatives)
```

