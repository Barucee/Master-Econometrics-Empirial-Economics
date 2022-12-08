#################################### Part 4 - Q4 - Simulation #######################################
## In this file, we try to do extra-plot on our data input and output                              ##                                                                                    ##
#####################################################################################################
# Import the libraries :
import os
import pandas as pd
import datetime as dt
import matplotlib.pyplot as plt
import matplotlib
from P3SimulationClass import Simulation

########### Upload of the df input ###########

# create the path for the data :
pathdf = os.path.abspath("../Data/Coffeebar_2016-2020.csv")

# open the data
df_input = pd.read_csv(pathdf, sep=';')

df_input['TIME'] = pd.to_datetime(df_input['TIME'])

print(df_input)
########### Upload of the df probqbilities ###########

# create the path for the data :
pathprobabilities = os.path.abspath("../Data/probabilities.csv")

# open the data
df_probabilities = pd.read_csv(pathprobabilities, sep=',', index_col=0)

print(df_probabilities)

########### Plot FOOD per Week Day ###########

#https://stackoverflow.com/questions/47864691/pandas-group-by-weekday-m-t-w-t-f-s-s
days = ['Monday','Tuesday','Wednesday','Thursday','Friday','Saturday', 'Sunday']
DataFoodPerDayOfWeek = df_input.groupby(df_input["TIME"].dt.day_name())["FOOD"].count().reindex(days)

figfoodsperweek, axfoodperweek = plt.subplots()
fig = axfoodperweek.bar(DataFoodPerDayOfWeek.index, DataFoodPerDayOfWeek.values, color='brown')
axfoodperweek.set_xlabel('Weekday')
axfoodperweek.set_ylabel('Sold Food')
axfoodperweek.set_title('Total amount of foods sold over the days of the week')
axfoodperweek.set_xticks(DataFoodPerDayOfWeek.index)
axfoodperweek.set_xticklabels(DataFoodPerDayOfWeek.index, rotation=30)
axfoodperweek.set_ylim(ymin=20000)
for bar in fig:
    yval = bar.get_height()
    plt.text(bar.get_x() + 0.10, yval + .03, yval)
plt.show()
figfoodsperweek.savefig(os.path.abspath("../Results/FoodPerWeekDay.png"))


########### Plot DRINKS per Week Day ###########

DataDrinksPerDayOfWeek = df_input.groupby(df_input["TIME"].dt.day_name())["DRINKS"].count().reindex(days)

figdrinksperweek, axdrinksperweek = plt.subplots()
fig = axdrinksperweek.bar(DataDrinksPerDayOfWeek.index, DataDrinksPerDayOfWeek.values, color='blue')
axdrinksperweek.set_xlabel('Weekday')
axdrinksperweek.set_ylabel('Sold Drinks')
axdrinksperweek.set_title('Total amount of drinks sold over the days of the week')
axdrinksperweek.set_xticks(DataDrinksPerDayOfWeek.index)
axdrinksperweek.set_xticklabels(DataDrinksPerDayOfWeek.index, rotation=30)
axdrinksperweek.set_ylim(ymin=42000)
for bar in fig:
    yval = bar.get_height()
    plt.text(bar.get_x() + 0.10, yval + .03, yval)
plt.show()
figdrinksperweek.savefig(os.path.abspath("../Results/DrinksPerWeekDay.png"))


########### Plot FOOD per Months ###########
Months = ['January','February','March','April','May','June', 'July', 'August', 'September', 'October', 'November', 'December']

DataFoodPerMonth = df_input.groupby(df_input["TIME"].dt.strftime('%B'))["FOOD"].count().reindex(Months)

figfoodspermonth, axfoodpermonth = plt.subplots()
fig = axfoodpermonth.bar(DataFoodPerMonth.index, DataFoodPerMonth.values, color='brown')
axfoodpermonth.set_xlabel('Weekday')
axfoodpermonth.set_ylabel('Sold Food')
axfoodpermonth.set_title('Total amount of foods sold over the months of the year')
axfoodpermonth.set_xticks(DataFoodPerMonth.index)
axfoodpermonth.set_xticklabels(DataFoodPerMonth.index, rotation=30)
axfoodpermonth.set_ylim(ymin=12000)
for bar in fig:
    yval = bar.get_height()
    plt.text(bar.get_x(), yval + .03, yval)
plt.show()
figfoodspermonth.savefig(os.path.abspath("../Results/FoodPerMonth.png"))


########### Plot DRINKS per Months ###########
DataDrinksPerMonth = df_input.groupby(df_input["TIME"].dt.strftime('%B'))["DRINKS"].count().reindex(Months)

figdrinkspermonth, axdrinkspermonth = plt.subplots()
fig = axdrinkspermonth.bar(DataDrinksPerMonth.index, DataDrinksPerMonth.values, color='blue')
axdrinkspermonth.set_xlabel('Weekday')
axdrinkspermonth.set_ylabel('Sold Food')
axdrinkspermonth.set_title('Total amount of drinks sold over the months of the year')
axdrinkspermonth.set_xticks(DataDrinksPerMonth.index)
axdrinkspermonth.set_xticklabels(DataDrinksPerMonth.index, rotation=30)
axdrinkspermonth.set_ylim(ymin=22000)
for bar in fig:
    yval = bar.get_height()
    plt.text(bar.get_x(), yval + .03, yval)
plt.show()
figdrinkspermonth.savefig(os.path.abspath("../Results/DrinksPerMonth.png"))

########### Plot count of DRINKS ###########
countdrinks = df_input.groupby(df_input["DRINKS"])["DRINKS"].count()

figcountdrinks, axcountdrinks = plt.subplots()
fig = axcountdrinks.bar(countdrinks.index, countdrinks.values, color='blue')
axcountdrinks.set_xlabel('Kind of Drink')
axcountdrinks.set_ylabel('Number of Drinks')
axcountdrinks.set_title('Total amount of each kind of drink sold')
axcountdrinks.set_xticks(countdrinks.index)
axcountdrinks.set_xticklabels(countdrinks.index, rotation=15)
axcountdrinks.set_ylim(ymin=30000)
for bar in fig:
    yval = bar.get_height()
    plt.text(bar.get_x(), yval + .03, yval)
plt.show()
figcountdrinks.savefig(os.path.abspath("../Results/CountDrinks.png"))

########### Plot count of FOODS ###########
countfoods = df_input.groupby(df_input["FOOD"])["FOOD"].count()

figcountfood, axcountfood = plt.subplots()
fig = axcountfood.bar(countfoods.index, countfoods.values, color='brown')
axcountfood.set_xlabel('Kind of Food')
axcountfood.set_ylabel('Number of Foods')
axcountfood.set_title('Total amount of each kind of food sold')
axcountfood.set_xticks(countfoods.index)
axcountfood.set_xticklabels(countfoods.index, rotation=15)
axcountfood.set_ylim(ymin=20000)
for bar in fig:
    yval = bar.get_height()
    plt.text(bar.get_x(), yval + .03, yval)
plt.show()
figcountfood.savefig(os.path.abspath("../Results/CountFoods.png"))
########### Plot probability DRINKS per timeslot ###########

figProbDrinks, axProbDrinks = plt.subplots()

for ColumnsDrinks in df_probabilities.columns[1:7]:
    axProbDrinks.plot(df_probabilities["TIME"], df_probabilities[ColumnsDrinks])

axProbDrinks.legend(df_probabilities.columns[1:7])

axProbDrinks.set_xlabel('TIME OF DAY')
axProbDrinks.set_ylabel('PROBABILITY PER DRINK')
axProbDrinks.set_title('PROBABILITY CONDITIONAL ON THE TIME OF DAY')
plt.xticks(df_probabilities["TIME"][::12],  rotation=30)
plt.show()
figProbDrinks.savefig(os.path.abspath("../Results/ProbDrinksPerHour.png"))

########### Plot probability Food per timeslot ###########

figProbFoods, axProbFood = plt.subplots()

for ColumnsFoods in df_probabilities.columns[7:]:
    axProbFood.plot(df_probabilities["TIME"], df_probabilities[ColumnsFoods])

axProbFood.legend(df_probabilities.columns[7:])

axProbFood.set_xlabel('TIME OF DAY')
axProbFood.set_ylabel('PROBABILITY PER DRINK')
axProbFood.set_title('PROBABILITY CONDITIONAL ON THE TIME OF DAY')
plt.xticks(df_probabilities["TIME"][::12],  rotation=30)
plt.show()
figProbFoods.savefig(os.path.abspath("../Results/ProbFoodsPerHour.png"))