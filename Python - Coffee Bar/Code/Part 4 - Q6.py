#################################### Part 4 - Q4 - Simulation #######################################
## In this file, we simulate the data set of the Part 4 Q4, so we choose that the ubereat ariable  ##
## is true, the fees of the service for the customer and the amount of this fee which comes back   ##
## to the restaurant. In the previous simulation, they were as None. Then, we implement the        ##
## simulation.                                                                                     ##
#####################################################################################################

from P3SimulationClass import Simulation
import matplotlib.pyplot as plt
import os

Part4_Q6 = Simulation(Ubereat = True,
                      probabilityUberEat = 0.02,
                      UberEatFees = 5,
                      PercentOfUberEatForTheCoffee = 0.2,
                      Path_output_DataFrame="../Results/Simulation_Part4_Q6.csv"
                      )
Part4_Q6.fill_in_Dictionary()
print(Part4_Q6.transform_in_Data_Frame())

output = Part4_Q6.data_frame_with_extra_column()

# Plot the revenue of the simulation :
RevenueSimulationQ6 = output.groupby(output["TIME"].dt.date)["Revenue"].sum()

fig, ax = plt.subplots()
RevenueSimulationQ6.plot()
ax.set_xlabel('Day')
ax.set_ylabel('Revenue')
ax.set_title('Revenue Per Day')
plt.savefig(os.path.abspath("../Results/RevenueSimulationPart4Q6.png"))
plt.show()

# Plot the number of returning customers of the simulation :
NbReturningSimulation6 = output.groupby(output["TIME"].dt.date)["Returning Pool size"].mean()

figRecurrent, axReccurent = plt.subplots()
NbReturningSimulation6.plot()
axReccurent.set_xlabel('Day')
axReccurent.set_ylabel('Size of the returning pool')
axReccurent.set_title('Size of the returning pool per day')
plt.savefig(os.path.abspath("../Results/NbReccurentSimulationPart4Q6.png"))
plt.show()
