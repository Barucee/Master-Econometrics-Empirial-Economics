import os
import pandas as pd
import datetime as dt
import matplotlib.pyplot as plt


pathdf = os.path.abspath("../Data/Coffeebar_2016-2020.csv")
df = pd.read_csv(pathdf, sep=';')
df['TIME'] = pd.to_datetime(df['TIME'])

RCHistory = df[df.duplicated(['CUSTOMER'])] # dataframe only with the returning customers
print(RCHistory)
NumberRecurrent = RCHistory['CUSTOMER'].nunique()
print(f'There are {NumberRecurrent} recurrent customers.')
print('-------------')


RCHPerHour = RCHistory.groupby([RCHistory['TIME'].dt.hour])["TIME"].count()


# We plot with Matplotlib :
fig421, ax421 = plt.subplots()
ax421.bar(RCHPerHour.index, RCHPerHour.values, color='green')
ax421.set_xlabel('Hour of the Day')
ax421.set_ylabel('Number of Returning Customers')
ax421.set_title('Total amount of Returning Customers over the last 5 years, per hour')
ax421.set_xticks(RCHPerHour.index)
ax421.set_xticklabels(RCHPerHour.index)
for i, v in enumerate(RCHPerHour.values):
    plt.text(RCHPerHour.index[i] - 0.35, v + 0.1, str(v))
plt.show()

fig421.savefig(os.path.abspath("../Results/NbReturningPerHour.png"))

print('-------------')

df["TYPE"] = df['CUSTOMER'].isin(RCHistory['CUSTOMER'])


df["TYPE"] = df['TYPE'].replace([True, False], ["Returning", "OneTime"])

ProbabilityType = df.groupby([df.TIME.dt.time])["TYPE"].apply(lambda x: x.value_counts() / len(x))
ProbabilityType = ProbabilityType.unstack()
ProbPerType = pd.concat([ProbabilityType], axis=1)
print(ProbPerType)

# plotting the probabilities
ax = ProbPerType.plot()
fig = ax.get_figure()
fig.savefig(os.path.abspath("../Results/ProbabilityPerCustomer.png"))
plt.show()



#calculate the correlation between type of customer and what they drinks
corrpDRINKS= pd.crosstab(df["DRINKS"], df["TYPE"], normalize='columns')

print(corrpDRINKS)

# importing the required function
from scipy.stats import chi2_contingency

# Performing Chi-sq test
ChiSqResultDrinks = chi2_contingency(corrpDRINKS)

# P-Value is the Probability of H0 being True
# If P-Value&gt;0.05 then only we Accept the assumption(H0)

print('The P-Value of the ChiSq Test for drinks are:', ChiSqResultDrinks[1])


#calculate the correlation between type of customer and what they eat
corrpFOODS= pd.crosstab(df["FOOD"], df["TYPE"], normalize='columns')

print(corrpFOODS)

# Performing Chi-sq test
ChiSqResultFoods = chi2_contingency(corrpFOODS)

# P-Value is the Probability of H0 being True
# If P-Value&gt;0.05 then only we reject the assumption(H0)

print('The P-Value of the ChiSq Test for foods is:', ChiSqResultFoods[1])