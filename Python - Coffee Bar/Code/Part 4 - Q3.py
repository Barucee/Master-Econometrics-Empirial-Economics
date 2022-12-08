#################################### Part 4 - Q3 - Simulation #######################################
## In this file, we simulate the data set of the Part 4 Q3, so we change the "normal" value of     ##
## returning customer to 50 and we will look at the difference of revenue with the classic         ##
## simulation                                                                                      ##
#####################################################################################################

from P3SimulationClass import Simulation
import matplotlib.pyplot as plt
import os

Part4_Q3 = Simulation(nbReturning=50,
                      Path_output_DataFrame="../Results/Simulation_Part4_Q3.csv"
                      )
Part4_Q3.fill_in_Dictionary()
print(Part4_Q3.transform_in_Data_Frame())

output = Part4_Q3.data_frame_with_extra_column()

# Plot the revenue of the simulation :
RevenueSimulationQ3 = output.groupby(output["TIME"].dt.date)["Revenue"].sum()

fig, ax = plt.subplots()
RevenueSimulationQ3.plot()
ax.set_xlabel('Day')
ax.set_ylabel('Revenue')
ax.set_title('Revenue Per Day')
plt.savefig(os.path.abspath("../Results/RevenueSimulationPart4Q3.png"))
plt.show()

# Plot the number of returning customers of the simulation :
NbReturningSimulation3 = output.groupby(output["TIME"].dt.date)["Returning Pool size"].mean()

figRecurrent, axReccurent = plt.subplots()
NbReturningSimulation3.plot()
axReccurent.set_xlabel('Day')
axReccurent.set_ylabel('Size of the returning pool')
axReccurent.set_title('Size of the returning pool per day')
plt.savefig(os.path.abspath("../Results/NbReccurentSimulationPart4Q3.png"))
plt.show()