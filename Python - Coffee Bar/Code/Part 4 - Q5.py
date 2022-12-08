#################################### Part 4 - Q5 - Simulation #######################################
## In this file, we simulate the data set of the Part 4 Q1, so we change the "normal" value of     ##
## hipster budget to 40                                                                            ##
#####################################################################################################

from P3SimulationClass import Simulation
import matplotlib.pyplot as plt
import os

Part4_Q5 = Simulation(budgetHipster=40,
                     Path_output_DataFrame="../Results/Simulation_Part4_Q5.csv"
                     )
Part4_Q5.fill_in_Dictionary()
print(Part4_Q5.transform_in_Data_Frame())

output = Part4_Q5.data_frame_with_extra_column()

# Plot the revenue of the simulation :
RevenueSimulationQ5 = output.groupby(output["TIME"].dt.date)["Revenue"].sum()

fig, ax = plt.subplots()
RevenueSimulationQ5.plot()
ax.set_xlabel('Day')
ax.set_ylabel('Revenue')
ax.set_title('Revenue Per Day')
plt.savefig(os.path.abspath("../Results/RevenueSimulationPart4Q5.png"))
plt.show()

# Plot the number of returning customers of the simulation :
NbReturningSimulation5 = output.groupby(output["TIME"].dt.date)["Returning Pool size"].mean()

figRecurrent, axReccurent = plt.subplots()
NbReturningSimulation5.plot()
axReccurent.set_xlabel('Day')
axReccurent.set_ylabel('Size of the returning pool')
axReccurent.set_title('Size of the returning pool per day')
plt.savefig(os.path.abspath("../Results/NbReccurentSimulationPart4Q5.png"))
plt.show()