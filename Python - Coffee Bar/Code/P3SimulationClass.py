##################################### Part 3 - Simulation Class #####################################
## In this file we create the class object which will allow us to simulate the coffe bar for       ##
## part 3 & 4 and the functions :                                                                  ##
##    - generate_random_customer_id() : generate a random customer ID                              ##
##    - generate_unique_customer_id() : generate a customer ID which has not been already          ##
##    attributed                                                                                   ##
##    - Generate_OnlyOnce_customer() : generate customer who come only once                        ##
##    - create_pool_of_returning_customers() : creation of a pool of returning customers           ##
##    - generate_customer() : generate randomly a customer                                         ##
##    - extract_probabilities_as_dictionary() : Extract the probability data frame as dictionary   ##
##    - fill_in_Dictionary() : fill in the output dictionary                                       ##
##    - transform_in_Data_Frame() : transform the output dictionary as a dataframe                 ##
##    - purchase_history_of_one_returning_customer() : give randomly the pucharsing history of one ##
##    returning customer. Part 4 Q1
#####################################################################################################

# Import the libraries :
from P2Customer import Customer
from P2Recurrent import Recurrent, HipsterRecurrent, RegularRecurrent
from P2OnlyOnce import OnlyOnce, TripAdvisor
import pandas as pd
import datetime as dt
import random
import os

# We create the class simulation and all its attributes :
class Simulation(object):
    def __init__(self,
                 Path_output_DataFrame,
                 probabilityTripAdvisor=0.1,
                 nbReturning=1000,
                 probabilityHipster=1 / 3,
                 probabilityRecurrent=0.2,
                 budgetHipster=500,
                 pricesdrink={"milkshake": 5, "frappucino": 4, "water": 2, "coffee": 3, "soda": 3, "tea": 3},
                 pricesfood={"sandwich": 2, "cookie": 2, "pie": 3, "muffin": 3, " ": 0},
                 start_date=dt.date(2016, 1, 1),
                 end_date=dt.date(2020, 12, 31),
                 delta=dt.timedelta(days=1),
                 ChangeOfPrice=None,
                 TimeChangeOfPrice=None,
                 Ubereat=None,
                 probabilityUberEat=0,
                 UberEatFees=None,
                 PercentOfUberEatForTheCoffee=None
                 ):
        self.df_input = pd.read_csv(os.path.abspath("../Data/probabilities.csv"), sep=",", index_col=0)
        self.dictionary_input = {}
        self.Dictionary_output = {'TIME': [], 'ID': [], 'Drinks': [], 'Food': [], 'Revenue': [],'Returning Pool size' : []}
        self.DataFrame_output = pd.DataFrame
        self.start_date = start_date
        self.end_date = end_date
        self.delta = delta
        self.customers = []
        self.customer_ids = set()
        self.RecurrentCustomersList = []
        self.RecurrentCustomersListOutput = []
        self.probabilityTripAdvisor = probabilityTripAdvisor
        self.nbReturning = nbReturning
        self.probabilityHipster = probabilityHipster
        self.probabilityRecurrent = probabilityRecurrent
        self.pricesdrink = pricesdrink
        self.pricesfood = pricesfood
        self.budgetHipster = budgetHipster
        self.dictionary_probabilities_drinks = {}
        self.dictionary_probabilities_foods = {}
        self.list_key = []
        self.Path_output_DataFrame = Path_output_DataFrame
        self.ChangeOfPrice = ChangeOfPrice
        self.TimeChangeOfPrice = TimeChangeOfPrice
        self.UberEat = Ubereat
        self.probabilityUberEat = probabilityUberEat
        self.UberEatFees = UberEatFees
        self.PercentOfUberEatForTheCoffee = PercentOfUberEatForTheCoffee

    # generate a random customer ID :
    def generate_random_customer_id(self):
        return "CID{}".format("".join(str(random.randint(0, 9)) for i in range(8)))

    # generate a customer ID which has not been already attributed :
    def generate_unique_customer_id(self):
        customer_id = self.generate_random_customer_id()
        while customer_id in self.customer_ids:
            customer_id = self.generate_random_customer_id()
        self.customer_ids.add(customer_id)
        return customer_id

    # We generate customer who come only once :
    def Generate_OnlyOnce_customer(self):
        if random.uniform(0, 1) <= self.probabilityTripAdvisor:
            generate_TripAdvisor = TripAdvisor(self.generate_unique_customer_id())
            while not generate_TripAdvisor.is_in_budget(self.pricesdrink,self.pricesfood):
                generate_TripAdvisor = TripAdvisor(self.generate_unique_customer_id())
            return generate_TripAdvisor
        else:
            generate_OnlyOnce = OnlyOnce(self.generate_unique_customer_id())
            while not generate_OnlyOnce.is_in_budget(self.pricesdrink,self.pricesfood) :
                generate_OnlyOnce = OnlyOnce(self.generate_unique_customer_id())
            return generate_OnlyOnce

    # Creation of a pool of returning customers :
    def create_pool_of_returning_customers(self):
        for i in range(self.nbReturning):
            if random.uniform(0, 1) <= self.probabilityHipster:
                Hipster = HipsterRecurrent(self.generate_unique_customer_id())
                Hipster.budget.append(self.budgetHipster)
                self.RecurrentCustomersList.append(Hipster)
            else:
                Regular = RegularRecurrent(self.generate_unique_customer_id())
                self.RecurrentCustomersList.append(Regular)

    # Generate customer :
    def generate_customer(self):
        if random.uniform(0, 1) <= self.probabilityRecurrent and len(self.RecurrentCustomersList) > 0:
            recurrentcustomer = random.choice(self.RecurrentCustomersList)
            while recurrentcustomer.is_in_budget(self.pricesdrink, self.pricesfood) == False:
                # delete the customer from the list
                self.RecurrentCustomersListOutput.append(recurrentcustomer)
                for i in range(len(self.RecurrentCustomersList)):
                    if self.RecurrentCustomersList[i].id == recurrentcustomer.id:
                        self.RecurrentCustomersList.remove(self.RecurrentCustomersList[i])
                        break
                if len(self.RecurrentCustomersList) == 0:
                    OnlyOnceCustomer = self.Generate_OnlyOnce_customer()
                    return OnlyOnceCustomer
                else :
                    recurrentcustomer = random.choice(self.RecurrentCustomersList)
            return recurrentcustomer
        else:
            OnlyOnceCustomer = self.Generate_OnlyOnce_customer()
            return OnlyOnceCustomer

    # Extract the probability data frame as dictionary :
    def extract_probabilities_as_dictionary(self):
        self.dictionary_input = self.df_input.to_dict()
        self.list_key = list(self.dictionary_input)
        for i in range(len(self.dictionary_input["TIME"])):
            list_prob_drinks = []
            list_prob_foods = []
            for key in self.list_key[1:7]:
                list_prob_drinks.append(self.dictionary_input[key][i])
            self.dictionary_probabilities_drinks[i] = list_prob_drinks
            for key in self.list_key[7:]:
                list_prob_foods.append(self.dictionary_input[key][i])
            self.dictionary_probabilities_foods[i] = list_prob_foods

    # Fill in the output dictionary
    def fill_in_Dictionary(self):
        Simulation.create_pool_of_returning_customers(self)
        Simulation.extract_probabilities_as_dictionary(self)
        while self.start_date <= self.end_date:
            if self.start_date == self.TimeChangeOfPrice:
                self.pricesfood.update((key, value * self.ChangeOfPrice) for key, value in self.pricesfood.items())
                self.pricesdrink.update((key, value * self.ChangeOfPrice) for key, value in self.pricesdrink.items())
            for i in range(len(self.dictionary_probabilities_drinks)):
                customer = Simulation.generate_customer(self)
                customer.buy(i,
                             self.start_date,
                             self.pricesdrink,
                             self.pricesfood,
                             self.list_key,
                             self.dictionary_input,
                             self.dictionary_probabilities_drinks,
                             self.dictionary_probabilities_foods)
                self.Dictionary_output["TIME"].append(customer.time[-1])
                self.Dictionary_output["ID"].append(customer.id)
                self.Dictionary_output["Drinks"].append(customer.drinks[-1])
                self.Dictionary_output["Food"].append(customer.foods[-1])
                self.Dictionary_output["Revenue"].append(customer.budget[-2] - customer.budget[-1])
                self.Dictionary_output["Returning Pool size"].append(len(self.RecurrentCustomersList))
                

                # Q6- UberEat for hipster
                if random.uniform(0, 1) < self.probabilityUberEat and any(isinstance(x, HipsterRecurrent) and x.is_in_budget(self.pricesdrink, self.pricesfood, self.UberEat,self.UberEatFees) == True for x in self.RecurrentCustomersList) and self.UberEat == True:
                    UberEat_Customer = random.choice(self.RecurrentCustomersList)
                    while not isinstance(UberEat_Customer, HipsterRecurrent) or not UberEat_Customer.is_in_budget(self.pricesdrink, self.pricesfood, self.UberEat, self.UberEatFees):
                        if isinstance(UberEat_Customer, HipsterRecurrent) == True :
                            if UberEat_Customer.is_in_budget(self.pricesdrink, self.pricesfood, self.UberEat,self.UberEatFees) == False :
                                self.RecurrentCustomersListOutput.append(UberEat_Customer)
                                for j in range(len(self.RecurrentCustomersList)):
                                    if self.RecurrentCustomersList[j].id == UberEat_Customer.id:
                                        self.RecurrentCustomersList.remove(self.RecurrentCustomersList[j])
                                        break
                        UberEat_Customer = random.choice(self.RecurrentCustomersList)
                    UberEat_Customer.buy(i,
                                         self.start_date,
                                         self.pricesdrink,
                                         self.pricesfood,
                                         self.list_key,
                                         self.dictionary_input,
                                         self.dictionary_probabilities_drinks,
                                         self.dictionary_probabilities_foods,
                                         self.UberEat,
                                         self.UberEatFees)
                    self.Dictionary_output["TIME"].append(UberEat_Customer.time[-1])
                    self.Dictionary_output["ID"].append(UberEat_Customer.id)
                    self.Dictionary_output["Drinks"].append(UberEat_Customer.drinks[-1])
                    self.Dictionary_output["Food"].append(UberEat_Customer.foods[-1])
                    self.Dictionary_output["Revenue"].append(UberEat_Customer.budget[-2]
                                                             - UberEat_Customer.budget[-1]
                                                             - self.UberEatFees * (1 - self.PercentOfUberEatForTheCoffee))
                    self.Dictionary_output["Returning Pool size"].append(len(self.RecurrentCustomersList))
            self.start_date += self.delta
        self.RecurrentCustomersListOutput.extend(self.RecurrentCustomersList)

    # Transform the output dictionary as a dataframe
    def transform_in_Data_Frame(self):
        self.DataFrame_output = self.DataFrame_output.from_dict(self.Dictionary_output)
        df_output = self.DataFrame_output.iloc[:, 0:4]
        df_output.to_csv(os.path.abspath(self.Path_output_DataFrame))
        return print(df_output)

    def purchase_history_of_one_returning_customer(self):
        returning = random.choice(self.RecurrentCustomersListOutput)
        return returning.purchase_history()

    def data_frame_with_extra_column(self):
        self.DataFrame_output = self.DataFrame_output.from_dict(self.Dictionary_output)
        return self.DataFrame_output