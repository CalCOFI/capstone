# -*- coding: utf-8 -*-
"""
Created on Tue Jan 25 10:11:55 2022

@author: prera
"""

import pandas as pd
import numpy as np
import geopandas as gpd
from shapely.geometry import Point
from geopandas import GeoDataFrame
import matplotlib.pyplot as plt
import plotly.express as px

#%%
#setup

data = pd.read_csv('data/bottle_and_cast.csv')
geometry = [Point(xy) for xy in zip(data['Lat_Dec'].values,data['Lon_Dec'].values)]
gdf = GeoDataFrame(data, geometry = geometry)

#%%
#geopandas plot
world = gpd.read_file(gpd.datasets.get_path('naturalearth_lowres'))
gdf.plot(ax=world.plot(figsize=(15, 15)), marker='o', color='red', markersize=15)
#%%
#plotly plot
#first make a new dataframe
d = {'lat':data['Lat_Dec'].unique,'lon':data['Lon_Dec'].unique}
df = pd.DataFrame(data = d)
fig = px.scatter_geo(data, lat = data['Lat_Dec'].unique,lon = data['Lon_Dec'].unique, hover_name = "Depthm")#hover_name can be list
fig.update_layout(title = 'Title', title_x=0.5)
fig.show()
