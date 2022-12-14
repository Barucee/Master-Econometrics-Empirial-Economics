# Import the necessary libraries

# import sql libraries
import pyodbc

# import data manipulation libraries
import pandas as pd
import os

# import data visualization libraries
import plotly_express as px
import plotly.graph_objs as go
import plotly as py

# import datetime libraries
import datetime as dt

#import streamlit app
import streamlit as st

##########################################################################################################################################################


# Connect to the data base
PathActivityRating = os.path.abspath(os.path.join(os.path.dirname(__file__),"../Data/activity-rating.csv"))

dfActivityRating =  pd.read_csv(PathActivityRating, sep=';')

###############################################################################################################
# Bon j'arrive pas a faire l'abstract path il faudra travailler dessus                                       ##
###############################################################################################################
PathAccessData = os.path.abspath(os.path.join(os.path.dirname(__file__),"../Data/DatabaseProjet.accdb"))
                                              
conn = pyodbc.connect(r'Driver={Microsoft Access Driver (*.mdb, *.accdb)};' + fr'DBQ={PathAccessData};')










# Creation of a streamlit App :

## We could do this code in different file of python for each page of the app but we thought that it 
# would longer to do it.

pages = st.sidebar.selectbox('Select the page', ['Introduction πΊοΈ','SQL Queries π', 'Discovering behavior and detecting worrying changes π΄π©Ί'])

st.title("Data Base Project ππ΄π©Ί")

if pages == 'Introduction πΊοΈ' :
    
    st.header("Introduction πΊοΈ")
    st.write("This visualization streamlit is the project of DataBase Management of Benjamin CLOUET & \
            Bruce RAVEY  \n \
            We'll study...!!! Copy the introduction of our paper !!! explain the different pages")
    
elif pages == 'SQL Queries π' :
    
    packages = st.sidebar.selectbox('Select the Package', 
                                    ['Package 1 : Management of activities π',
                                     'Package 2 : Monitoring elderly peopleβs health state π΄ π©Ί',
                                     'Package 3: Situation awareness π‘π¨'])
    
    if packages == 'Package 1 : Management of activities π' :
        
        P1Queries = st.sidebar.selectbox('Select the Query',
                                         ['F1 : Activity session and its animator π¨π»βπ«ποΈββοΈ',
                                          'R1 : Activities planned between two dates ποΈ',
                                          'R2 : Free activities having a difficulty less than a given value',
                                          'R3 : List of activities mixing seniors and young peopleπ΄π¦',
                                          'R4 : Number of sessions per activity with their average rate and maximum duration.Limited to activities having at least two sessions π',
                                          'R5 : Percentage of activities animated by elderly people π΄π¨π»βπ«'])
        
        st.header("Package 1 : Management of activities π")
        
        if P1Queries == 'F1 : Activity session and its animator π¨π»βπ«ποΈββοΈ' :

            st.subheader("F1 : Activity session and its animator π¨π»βπ«ποΈββοΈ")
        
            QueryP1F1 = "SELECT SA.ActivityName AS Name_of_Activity, S.SessionId AS ID_Session, I.FirstName + " " +  I.LastName AS Animator FROM Inhabitant AS I INNER JOIN (SkillActivity AS SA INNER JOIN [Session] AS S ON SA.ActivityName = S.ActivityName) ON I.InHabitantId = S.InHabitantId;"
            
            dfP1F1 = pd.read_sql(QueryP1F1, conn)

            st.dataframe(dfP1F1)
        
        if P1Queries == 'R1 : Activities planned between two dates ποΈ' :
            
            st.subheader("R1 : Activities planned between two dates ποΈ")

            QueryP1R1 = 'SELECT S.ActivityName AS Name_of_Activity, S.DateS AS Date_of_Activity \
                        FROM [Session] AS S \
                        WHERE S.DateS BETWEEN #11/1/2022# AND #11/4/2022#'
            
            dfP1R1 = pd.read_sql(QueryP1R1, conn)
            
            st.dataframe(dfP1R1)
            
        if P1Queries == 'R2 : Free activities having a difficulty less than a given value' :
            
            st.subheader("R2 : Free activities having a difficulty less than a given value")
            
            difficulty = st.selectbox('Select the difficulty', ['0','1','2','3','4','5'])
            
            QueryP1R2 = f'SELECT SA.ActivityName AS ActivityName \
                        FROM SkillActivity AS SA \
                        WHERE SA.Difficulty > {difficulty} AND SA.Price =0;'
            
            dfP1R2 = pd.read_sql(QueryP1R2, conn)
            
            st.dataframe(dfP1R2)
    



