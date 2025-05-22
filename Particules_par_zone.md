```
Python
``` 
# PACKAGES
```
import pandas as pd
import geopandas as gpd
from shapely.geometry import Point, Polygon
import matplotlib.pyplot as plt
import numpy as np
import xarray as xr
from scipy.interpolate import RegularGridInterpolator
import os
from pandas.errors import EmptyDataError
```

# PASS WAY DIRECTORY
```
base_path = "C:/Users/UTILISATEUR/Documents/cours/M2/S10/Stage/Stage_M2/Mars/Dispersion/Dispersion_active/2013/Fevrier"
```

# FICHIER BATHYMÉTRIE
A télécharger au préalable sur CMEMS
```
fichier_bathymetrie = "C:/Users/UTILISATEUR/Documents/cours/M2/S10/Stage/Stage_M2/Mars/Dispersion/Dispersion_active/gebco_2024_n50.0_s47.0_w-5.6_e-1.3.nc"
```

# CHARGEMENT DE LA BATHYMÉTRIE
```
ds = xr.open_dataset(fichier_bathymetrie)
lon_bathy = ds['lon'].values
lat_bathy = ds['lat'].values
depth_bathy = ds['elevation'].values 

interp = RegularGridInterpolator((lat_bathy[::-1], lon_bathy), depth_bathy[::-1, :], bounds_error=False, fill_value=np.nan)
```

# DÉFINITION DES ZONES 
```
zones = {
    "Manche_ouest": Polygon([(-5.55, 48.6), (-4.5, 48.6), (-4.5, 49.8), (-5.55, 49.8)]),
    "Nord_Bretagne": Polygon([(-4.5, 48.6), (-3.5, 48.6), (-3, 48.83), (-3, 49.23), (-4.5, 49)]),
    "Golfe_Normano-Breton": Polygon([(-3, 48.83), (-2.68, 48.53), (-1.52, 48.66), (-1.66, 49.23), (-1.99, 49.72),  (-3, 49.69)]),
    "Iroise": Polygon([(-5.55, 47.8), (-4.25, 47.8), (-4.25, 48.6), (-5.55, 48.6)]),
    "Sud_Bretagne": Polygon([(-4.9, 47.8), (-4.25, 47.8),(-4.25, 47.888), (-3.97, 47.888),(-2.75, 47.7), (-2.5, 47.465), (-2.9, 47.14), (-3.8, 47.5), (-5.15, 47.54)])
}
zones_gdf = gpd.GeoDataFrame({'zone': list(zones.keys()), 'geometry': list(zones.values())}, crs='EPSG:4326')
```

# FOND DE CARTE 
```
world = gpd.read_file("https://raw.githubusercontent.com/nvkelso/natural-earth-vector/master/geojson/ne_10m_admin_0_countries.geojson")
france = world[world['ADMIN'] == 'France']
```

# COULEURS DES ZONES
```
zone_colors = {
    'Manche_ouest': 'red',
    'Iroise': 'black',
    'Iles_Anglo-Normandes': 'blue',
    'Nord_Bretagne': 'lightgreen',
    'Sud_Bretagne': 'yellow',
}
```

# TRAITEMENT DE TOUS LES FICHIERS CSV
```
for file in os.listdir(base_path):
    if file.endswith("00.csv") and file.startswith("Position_"):
        fichier_particles = os.path.join(base_path, file)
        print(f"Traitement du fichier : {file}")

        # Sécurisation de la lecture du CSV
        try:
            df = pd.read_csv(fichier_particles, sep=',')
        except EmptyDataError:
            print(f"⚠️  Fichier vide ou mal formé, ignoré : {file}")
            continue

        if df.empty or 'latitude' not in df.columns or 'longitude' not in df.columns:
            print(f"⚠️  Fichier vide ou colonnes manquantes, ignoré : {file}")
            continue

        # Nom de base sans extension
        nom_base = os.path.splitext(file)[0]
        fichier_sortie_csv = os.path.join(base_path, f"Repartition_{nom_base}.csv")
        image_path = os.path.join(base_path, f"Carte_{nom_base}.png")

        # Création du GeoDataFrame
        geometry = [Point(xy) for xy in zip(df['longitude'], df['latitude'])]
        gdf = gpd.GeoDataFrame(df, geometry=geometry, crs='EPSG:4326')

        # Interpolation des profondeurs
        coords = np.array(list(zip(gdf['latitude'], gdf['longitude'])))
        gdf['profondeur'] = -interp(coords)  # Profondeur positive

        # Classification zone et profondeur
        gdf['zone'] = None
        gdf['profondeur_categorie'] = gdf['profondeur'].apply(lambda x: '>40m' if x > 40 else '=<40m')
        for zone_name, polygon in zones.items():
            mask = gdf.within(polygon)
            gdf.loc[mask, 'zone'] = zone_name

        # Analyse de répartition 
        repartition = gdf.groupby(['zone', 'profondeur_categorie']).size().unstack(fill_value=0)
        repartition['Stage7'] = repartition.sum(axis=1)
        if '≤40m' not in repartition.columns:
            repartition['=<40m'] = 0
        if '>40m' not in repartition.columns:
            repartition['>40m'] = 0
        repartition['% =<40m'] = (repartition['=<40m'] / repartition['Stage7'] * 100).round(2)
        repartition['% >40m'] = (repartition['>40m'] / repartition['Stage7'] * 100).round(2)

        # Pourcentage global
        total_global = 3162
        repartition['% total =<40m'] = (repartition['=<40m'] / total_global * 100).round(2)
        repartition['% total >40m'] = (repartition['>40m'] / total_global * 100).round(2)
        
        # Sauvegarde CSV
        repartition.to_csv(fichier_sortie_csv, sep=';', index=True)
        print(f"✅ Fichier CSV enregistré : {fichier_sortie_csv}")

        # Carte
        fig, ax = plt.subplots(figsize=(10, 10))
        france.plot(ax=ax, color='lightgrey', edgecolor='black')

        # Ligne des 40m
        lon_grid, lat_grid = np.meshgrid(lon_bathy, lat_bathy)
        depth_interp = -depth_bathy
        contour = ax.contour(lon_grid, lat_grid, depth_interp, levels=[40], colors='grey', linewidths=1)
        ax.clabel(contour, inline=True, fontsize=8, fmt='40m')

        # Zones
        for idx, zone in zones_gdf.iterrows():
            zone_name = zone['zone']
            zone_geom = zone['geometry']
            if zone_geom.boundary.is_valid:
                gpd.GeoSeries([zone_geom.boundary]).plot(ax=ax, color=zone_colors.get(zone_name, 'grey'), linewidth=2)
            gpd.GeoSeries([zone_geom]).plot(ax=ax, color=zone_colors.get(zone_name, 'grey'), alpha=0.3)

        # Particules
        gdf.plot(ax=ax, color='purple', markersize=5, alpha=0.7, label='Particules')

        # Ajustements
        ax.set_xlim([-5.6, -1.3])
        ax.set_ylim([47, 50])
        ax.set_title(f"Répartition des particules : {nom_base}", fontsize=14)
        ax.set_xlabel("Longitude")
        ax.set_ylabel("Latitude")
        ax.legend(loc='lower right')
        ax.grid(True)

        # Sauvegarde de la carte
        plt.savefig(image_path, dpi=300)
        print(f"🗺️ Carte enregistrée : {image_path}")
        plt.close()
```
