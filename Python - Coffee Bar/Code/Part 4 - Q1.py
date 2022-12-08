#################################### Part 4 - Q1 - Simulation #######################################
## In this file, we simulate the data set of the Part 4 Q1, so we us the functionpurchase_history_ ##
## of_one_returning_customer() in order to have a purchasing history of 2 recurrent customersn     ##
#####################################################################################################

from P3SimulationClass import Simulation

Part4_Q1 = Simulation(Path_output_DataFrame = "../Results/Simulation_Part4_Q1.csv")

Part4_Q1.fill_in_Dictionary()
print(Part4_Q1.transform_in_Data_Frame())
print(Part4_Q1.purchase_history_of_one_returning_customer())
print(Part4_Q1.purchase_history_of_one_returning_customer())