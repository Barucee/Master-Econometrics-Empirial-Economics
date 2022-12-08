####################################### Part 3 - Simulation #########################################
## In this file, we simulate the classic dataset for the part 3.                                   ##
#####################################################################################################

# Import the libraries and the classes of other files :
from P3SimulationClass import Simulation
import matplotlib.pyplot as plt
import os

Part3simulation = Simulation(Path_output_DataFrame="../Results/Simulation_Part3.csv")

Part3simulation.fill_in_Dictionary()
print(Part3simulation.transform_in_Data_Frame())

output = Part3simulation.data_frame_with_extra_column()


# Plot the revenue of the simulation :
RevenueSimulation1 = output.groupby(output["TIME"].dt.date)["Revenue"].sum()

figRevenue, axRevenue = plt.subplots()
RevenueSimulation1.plot()
axRevenue.set_xlabel('Day')
axRevenue.set_ylabel('Revenue')
axRevenue.set_title('Revenue Per Day')
plt.savefig(os.path.abspath("../Results/RevenueSimulationPart3.png"))
plt.show()

# Plot the number of returning customers of the simulation :
NbReturningSimulation1 = output.groupby(output["TIME"].dt.date)["Returning Pool size"].mean()

figRecurrent, axReccurent = plt.subplots()
NbReturningSimulation1.plot()
axReccurent.set_xlabel('Day')
axReccurent.set_ylabel('Size of the returning pool')
axReccurent.set_title('Size of the returning pool per day')
plt.savefig(os.path.abspath("../Results/NbReccurentSimulationPart3.png"))
plt.show()

