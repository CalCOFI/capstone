# -*- coding: utf-8 -*-
"""
Created on Thu Jan 20 10:41:54 2022

@author: prera
"""

import plotly.graph_objects as go
import pandas as pd

import plotly.io as pio
pio.renderers.default='browser'


data = pd.read_csv('bottle_and_cast.csv')
lat = data.Lat_Dec.unique() #Read in unique lat and lon values for plotting
lon = data.Lon_Dec.unique()
depth = data.Depthm.values
ox = data.O2ml_L.values

#We find the depths corresponding to each latitude and longitude
depths = []
for i,j in zip(lat,lon):
    depths.append(data[(data[['Lat_Dec','Lon_Dec']].values == [i,j]).all(axis=1)].Depthm.values)

#Do the same with oxygen
oxy = []
for idx in range(0,len(depths)-1):
    for i,j,k in zip(lat,lon,depths[idx]):
        oxy.append(data[(data[['Lat_Dec','Lon_Dec','Depthm']].values == [i,j,k]).all(axis=1)].O2ml_L.values)

#Plot the lat and lon with plotly

fig = go.Figure(data=[go.Surface(z=depths, x=lat, y=lon)])

#fig.add_trace(go.Surface(x=lat, y=lon, z=depths, surfacecolor=ox))

fig.update_layout(title='Ocean Depth', autosize=False,
                  width=500, height=500,
                  margin=dict(l=65, r=50, b=65, t=90))
fig.show()
