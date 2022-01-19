# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""
import pandas as pd

data = pd.read_csv('C:/Users/prera/Documents/GitHub/CalCOFI22/dataset_to_use/CalCOFI_Database_194903-202001_csv_22Sep2021/194903-202001_Bottle.csv')
df = pd.DataFrame(data)
#[889500 rows x 62 columns]


#df.dropna results in an empty table
#First, only keeping the columns we want
table = df[['Btl_Cnt', 'Cst_Cnt','Depth_ID','Depthm','T_degC','O2ml_L','R_Depth']]
#[889500 rows x 6 columns]
#R_Temp not found in index

#Now cutting down by time
#No time in this table, so two ways to do it. Either merge the processed cast table and this table on Cst_Cnt, or extract date from Depthm that is
#formatted as:
#[Century]-[Year][Month][ShipCode]-[CastType][Julian Day]-[CastTime]-[Line][Sta]) but adds three additional variables: [Depth][Bottle]-[Rec_Ind]
#The second option is not as good because the best I can do is get the year and the month:
#str(table['Depth_ID'][889499][:2])+str(table['Depth_ID'][889499][3:7])

#Doing it the first way
cast_data = pd.DataFrame(pd.read_csv('C:/Users/prera/Documents/GitHub/CalCOFI22/cast_table.csv'))
combined = pd.merge(table, cast_data, how="left", on="Cst_Cnt")
cut_by_date = combined.dropna()
#Keys are now: ['Btl_Cnt', 'Cst_Cnt', 'Depth_ID', 'Depthm', 'T_degC', 'O2ml_L','R_Depth', 'Unnamed: 0', 'Cast_ID', 'Quarter', 'Date', 'Year', 'Month', 'Lat_Dec', 'Lon_Dec']

#finally cut down by depth. We only want surface measurements for now
final = cut_by_date.loc[cut_by_date['Depthm']==0]
final.to_csv()