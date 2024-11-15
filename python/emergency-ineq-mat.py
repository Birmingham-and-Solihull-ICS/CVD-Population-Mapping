# -*- coding: utf-8 -*-
"""
Inequality matricies for the following:
    - Percentage of emergency CVD admissions
    
Raw data columns:
    IMD_decile, ethnicity_broad, total_admissions, emergency_admissions, 
    emergency_percentage, p_hat, Z, LowerCI95, UpperCI95	

"""

import seaborn as sns
import pandas as pd
import EquiPy.Matrix as Mat

import matplotlib.pyplot as plt
import numpy as np

custom_params = {"axes.spines.right": False, "axes.spines.top": False}
sns.set_theme(style="ticks", rc=custom_params)

def conf_int(p, n):
    
    Z = 1.64485
    lower = p-(p + Z**2/(2*n) - Z * np.sqrt((p*(1-p)/n) + Z**2/(4*n**2))) / (1 + Z**2/n)
    upper = (p + Z**2/(2*n) + Z * np.sqrt((p*(1-p)/n) + Z**2/(4*n**2))) / (1 + Z**2/n)-p
    
    return (upper + lower)/2


#%% Load data
data = pd.read_excel("../data/PrimaryCause_PercentageEmergency_IMDEthnicity.xlsx")

data["Ethnicity"] = data["ethnicity_broad"]
data["Ethnicity"] = data["Ethnicity"].replace("Asian or Asian British ", 'Asian')
data["Ethnicity"] = data["Ethnicity"].replace("Black or Black British ", 'Black')
data["Ethnicity"] = data["Ethnicity"].replace("Other Ethnic Groups ", 'Other')

data["IMD Quintile"] = np.floor(data["IMD_decile"]+1)/2
data["IMD Quintile"] = data["IMD Quintile"].astype(int)

data = data.groupby(["Ethnicity", "IMD Quintile"])[["total_admissions",
       "emergency_admissions"]].sum().reset_index()


#%% Create pivot tables

N_table = data[["Ethnicity", "IMD Quintile", "total_admissions"]].pivot_table(
    values = "total_admissions", 
    index = "IMD Quintile",
    columns = "Ethnicity"
    )

n_table = data[["Ethnicity", "IMD Quintile", "emergency_admissions"]].pivot_table(
    values = "emergency_admissions", 
    index = "IMD Quintile",
    columns = "Ethnicity"
    )

perc_table = 100 * n_table / N_table

#%% Plot inequality matrix

eth_n = np.sum(N_table, axis = 0)
eth_p = np.sum(perc_table/100*N_table, axis = 0)/np.sum(N_table, axis = 0)
eth_av = pd.DataFrame({
    "%" : np.round(100*eth_p,1),
    "+/-" : np.round(100*conf_int(eth_p, eth_n), 1)
              }).sort_values("%", ascending=False)


imd_n = np.sum(N_table, axis = 1)
imd_p = np.sum(perc_table/100*N_table, axis = 1)/np.sum(N_table, axis = 1)
imd_av = pd.DataFrame({
    "%" : np.round(100*imd_p,1),
    "+/-" : np.round(100*conf_int(imd_p, imd_n), 1)
              }).sort_values("%", ascending=False)    

print(eth_av)
print()
print(imd_av) 

fig = Mat.inequality_map(N_table, 
                    perc_pivot = perc_table,
                    title = "CVD Admissions\nas Emergency %",
                    #ttest = True,
                    letter="",
                    IMD_ticks = ["(Most Deprived) - 1", "2", "3", "4", "(Least Deprived) - 5"],
                    CI_method = "Wilson",
                    palette = "Blues"
                    )

fig.savefig("../outcome/emergency-cvd-admissions.png", bbox_inches = "tight",
            dpi = 300)