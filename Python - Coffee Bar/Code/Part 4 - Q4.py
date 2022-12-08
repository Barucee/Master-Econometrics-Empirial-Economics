#################################### Part 4 - Q4 - Simulation #######################################
## In this file, we simulate the data set of the Part 4 Q4, so we implement a change of price value##
## and a date of change, which was in the previous simulation as "none". Then, we implement the    ##
## simulation                                                                                      ##
#####################################################################################################

from P3SimulationClass import Simulation
import datetime as dt
import matplotlib.pyplot as plt
import os

Part4_Q4 = Simulation(ChangeOfPrice = 1.2,
                      TimeChangeOfPrice = dt.date(2018, 1, 1),
                      Path_output_DataFrame="../Results/Simulation_Part4_Q4.csv"
                      )
Part4_Q4.fill_in_Dictionary()
print(Part4_Q4.transform_in_Data_Frame())

output = Part4_Q4.data_frame_with_extra_column()

# Plot the revenue of the simulation :
RevenueSimulationQ4 = output.groupby(output["TIME"].dt.date)["Revenue"].sum()

fig, ax = plt.subplots()
RevenueSimulationQ4.plot()
ax.set_xlabel('Day')
ax.set_ylabel('Revenue')
ax.set_title('Revenue Per Day')
plt.savefig(os.path.abspath("../Results/RevenueSimulationPart4Q4.png"))
plt.show()

# Plot the number of returning customers of the simulation :
NbReturningSimulation4 = output.groupby(output["TIME"].dt.date)["Returning Pool size"].mean()

figRecurrent, axReccurent = plt.subplots()
NbReturningSimulation4.plot()
axReccurent.set_xlabel('Day')
axReccurent.set_ylabel('Size of the returning pool')
axReccurent.set_title('Size of the returning pool per day')
plt.savefig(os.path.abspath("../Results/NbReccurentSimulationPart4Q4.png"))
plt.show()

