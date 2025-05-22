# Packages
```
import xarray as xr
import numpy as np
import pandas as pd
import os
from glob import glob
```

# Pass way directory
```input_folder = "E:/Hugo/Stage/Stage_M2/Mars/Dispersion/Dispersion_active/2022/Juin"
output_folder = input_folder 

nc_files = glob(os.path.join(input_folder, "*.nc"))
```
# Boucle 
'''
for input_path in nc_files:
    filename = os.path.splitext(os.path.basename(input_path))[0]

    try:
        ds = xr.open_dataset(input_path)

        # Variables demandé
        stage = ds['STAGE']
        lat = ds['latitude']
        lon = ds['longitude']

        results = []

        for i in range(stage.sizes['traj']):
            stage_i = stage.isel(traj=i).values

            idx = np.argmax(stage_i == 7) if np.any(stage_i == 7) else -1

            if idx >= 0:
                lat_val = lat.isel(traj=i, time=idx).item()
                lon_val = lon.isel(traj=i, time=idx).item()
                time_val = ds['time'].isel(time=idx).item()
                stage_val = stage_i[idx]

                results.append({
                    'traj': ds.traj[i].item(),
                    'time_index': idx,
                    'stage': stage_val,
                    'latitude': lat_val,
                    'longitude': lon_val
                })

        df_stage7_first_hits = pd.DataFrame(results)

        output_path = os.path.join(output_folder, f"Position_stage7_{filename}.csv")
        df_stage7_first_hits.to_csv(output_path, index=False)
        print(f"✅ Fichier exporté : {output_path}")

    except Exception as e:
        print(f"❌ Erreur avec le fichier {filename}: {e}")
'''
