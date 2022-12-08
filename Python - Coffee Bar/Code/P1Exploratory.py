################################# Exploratory code ##############################
## In this Code We'll explore the data furnished for the exam by doing the     ##
## questions of the first part of the exam. We will also determine the         ##
## probabilities.                                                              ##
#################################################################################

# Import the libraries :
import os
import pandas as pd
import datetime as dt
import matplotlib.pyplot as plt

# create the path for the data :
pathdf = os.path.abspath("../Data/Coffeebar_2016-2020.csv")

# open the data
df = pd.read_csv(pathdf, sep=';')

print("--------------------------------------")
# Check the data types :
print(df.dtypes)
# We can notice that the date is not in Date format, so we'll change it

print("--------------------------------------")

# transform the variable "TIME" in datetime type :
df['TIME'] = pd.to_datetime(df['TIME'])

# check the data types now
print(df.dtypes)

print("--------------------------------------")
# look if there is missing values
print(df.isnull().sum())

print("--------------------------------------")

########### Q1.A- What food and drinks are sold by the coffee bar? ###########

print("Question 1 :")

# Get the unique value in the columns "Drinks" and "Foods :
listDrinks = df['DRINKS'].unique()
listFood = df['FOOD'].unique()

# We print the drinks inside a sentence :
print("About drinks, the coffebar has sold : {} and {}.".format(", ".join(listDrinks[:-1]), listDrinks[-1]))

# We can see that there is missing values for the list food, it can not be printed like that as a sentence so :
listFood = ["nothing" if pd.isnull(x) else x for x in listFood]
# We print the drinks inside a sentence :
print("About food, the Coffebar has sold  : {} and {}.".format(", ".join(listFood[0:-1]), listFood[-1]))

########### Q1.B - How many unique customers did the bar have? ###########

# We count the number of unique customers by counting the unique IDs :
nbunique = df['CUSTOMER'].nunique()
print(f"The coffebar has {nbunique} unique customers.")

########### Q2.A- A bar plot of the total amount of sold foods over the five years ###########
print("--------------------------------------")

print("Question 2 :")

# We group by the number of foods sold per year :
DataFoodPerYear = df.groupby([df["TIME"].dt.year])["FOOD"].count()

# We plot with Matplotlib DataFoodPerYear :
fig1, ax1 = plt.subplots()
ax1.bar(DataFoodPerYear.index, DataFoodPerYear.values, color='brown')
ax1.set_xlabel('Year')
ax1.set_ylabel('Sold Food')
ax1.set_title('Total amount of foods sold over the five years')
ax1.set_xticks(DataFoodPerYear.index)
ax1.set_xticklabels(DataFoodPerYear.index)
ax1.set_ylim(ymin=30000)
for i, v in enumerate(DataFoodPerYear.values):
    plt.text(DataFoodPerYear.index[i] - 0.25, v + 0.01, str(v))
plt.show()

# We save the fig in our Results repository :
fig1.savefig(os.path.abspath("../Results/FoodPerYear.png"))

########### Q2.B- A bar plot of the total amount of sold Drinks over the five years ###########

# We group by the number of Drinks sold per year :
DataDrinksPerYear = df.groupby([df.TIME.dt.year])["DRINKS"].count()

# We plot with Matplotlib DataDrinksPerYear :
fig2, ax2 = plt.subplots()
ax2.bar(DataDrinksPerYear.index, DataDrinksPerYear.values, color='blue')
ax2.set_xlabel('Year')
ax2.set_ylabel('Sold Drinks')
ax2.set_title('Total amount of drinks sold over the five years')
ax2.set_xticks(DataDrinksPerYear.index)
ax2.set_xticklabels(DataDrinksPerYear.index)
ax2.set_ylim(ymin=60000)
for i, v in enumerate(DataDrinksPerYear.values):
    plt.text(DataDrinksPerYear.index[i] - 0.25, v + 0.01, str(v))
plt.show()

# We save the fig in our Results repository :
fig2.savefig(os.path.abspath("../Results/DrinkPerYear.png"))

print("--------------------------------------")

########### Q3- Determine the average that a customer buys a certain food or drink at any given time ###########

print("Question 3 :")

# We fill in the missing value by a string with a space in order to have a column of probability :
df.fillna(" ", inplace=True)

# We group by the probabilities by Time :
DRINKProbabilities = df.groupby([df.TIME.dt.time])["DRINKS"].apply(lambda x: x.value_counts() / len(x))
FOODProbabilities = df.groupby([df.TIME.dt.time])["FOOD"].apply(lambda x: x.value_counts() / len(x))

# We unstack our probability in order to have the type of product as column :
DRINKProbabilities = DRINKProbabilities.unstack()
FOODProbabilities = FOODProbabilities.unstack()

# We concat the probabilities of foods and drink inside one dataframe :
probabilities = pd.concat([DRINKProbabilities, FOODProbabilities], axis=1)

# We reset the index because the "TIME" were in index :
probabilities.reset_index(inplace=True)

# We replace the NA of probabilities by 0 :
probabilities.fillna(0, inplace=True)
print(probabilities)

# Now we export the dataset into a csv file :
path_probabilities = os.path.abspath("../Data/probabilities.csv")
probabilities.to_csv(path_probabilities)
