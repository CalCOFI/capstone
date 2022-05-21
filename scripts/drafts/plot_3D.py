# -*- coding: utf-8 -*-
"""
Created on Thu Jan 20 10:41:54 2022

@author: prera
"""
#%%
import plotly.graph_objects as go
import pandas as pd

import plotly.io as pio
pio.renderers.default='browser'

#%%
data = pd.read_csv('data/bottle_and_cast.csv')
lat = data.Lat_Dec.unique() #Read in unique lat and lon values for plotting
lon = data.Lon_Dec.unique()
depth = data.Depthm.values
ox = data.O2ml_L.values
#%%
"""
The goal is to be able to input a 3D coord (lat,lon,depth) and get the value of a parameter (oxygen, temp etc.) at that coordinate
or 4D and include time
"""

#%%
#We find the depths corresponding to each latitude and longitude
def param_at_coords(data, param,lat='Lat_Dec',lon='Lon_Dec'):
    param_dict = data.to_dict(orient='list')
    lat = list(set(data[str(lat)])) #Read in unique lat and lon values for plotting
    lon = list(set(data[str(lon)]))
    param = str(param)
    #see if param is in keys
    if param in param_dict.keys():
        result = []
        for i,j in zip(lat,lon):
            code = "result.append(data[(data[['Lat_Dec','Lon_Dec']].values == [i,j]).all(axis=1)].{}.values)".format(param)
            exec(code)
    return result
    
#%%
depth = data.Depthm.values
depths = []
for i,j in zip(lat,lon):
    depths.append(data[(data[['Lat_Dec','Lon_Dec']].values == [i,j]).all(axis=1)].Depthm.values)
#%%
#Do the same with oxygen, we expect there to be one oxygen value for every depth value
params = []
for idx in range(0,len(depths)):
    for i,j,k in zip(lat,lon,depths[idx]):
        index = data.index[(data[['Lat_Dec','Lon_Dec','Depthm']].values == [i,j,k]).all(axis=1)]
        params.append([data.iloc[i] for i in index])
#%%
#Plot the lat and lon with plotly

fig = go.Figure(data=[go.Surface(z=depths, x=lat, y=lon)])

#fig.add_trace(go.Surface(x=lat, y=lon, z=depths, surfacecolor=ox))

fig.update_layout(title='Ocean Depth', autosize=False,
                  width=500, height=500,
                  margin=dict(l=65, r=50, b=65, t=90))
fig.show()
