# Import the necessary libraries

# import sql libraries
import pyodbc

# import data manipulation libraries
import pandas as pd
import os

# import data visualization libraries
import plotly_express as px

#import parallel processing libraries
from multiprocessing import cpu_count
from multiprocessing.pool import ThreadPool
import multiprocessing as mp

# import machine learning libraries
from sklearn.cluster import KMeans

# Fix the number of CPU used :
nCPU = os.cpu_count()
pool = ThreadPool(processes=nCPU)

##########################################################################################################################################################





#################
# 
# # import the data into a pandas dataframe :
# Import DataSet with parallelization on the different Cpus
path = 'C:/Users/33646/Desktop/M2/EEE/DataBase/Project/activity-rating.csv'
def import_df(file_name):
    df = pd.read_csv(file_name, sep=';')
    return df
df_rating = pool.apply_async(import_df, (path, )).get() 

#select the 3 first columns of the dataframe :
df_rating = df_rating.iloc[:,0:3]
df_rating



# df-rating to dict without NaN values
