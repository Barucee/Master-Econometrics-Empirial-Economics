################################### Part 2 - SubClasses Recurrent ###################################
## In this file we create the subclasses of the customers "Recurrent" and the functions :          ##
##    - purchase history() : to allow the customer to "say" what he did                            ##
#####################################################################################################

#Import the libraries
from P2Customer import Customer
import random
import datetime as dt

# Creation of the subclass "Recurrent" with its attributes :
class Recurrent(Customer):
    def __init__(self, id):
        super().__init__(id)
        self.budget = []
        self.time = []
        self.foods = []
        self.drinks = []

    # function which allow the customer to "say" what he did :
    def purchase_history(self):
        print(f"The customer {self.id} :\n")
        for i in range(len(self.foods)):
            if self.foods[i] == " ":
                print(f"On {self.time[i].date()} at {self.time[i].time()}, He has bought {self.drinks[i]}.\n"
                      f"He had a budget of {self.budget[i]} and now a budget of {self.budget[i + 1]}. He has spent {self.budget[i] - self.budget[i+1]}.\n")
            else:
                print(
                    f"On {self.time[i].date()} at {self.time[i].time()}, He has bought {self.foods[i]} and {self.drinks[i]}.\n"
                    f"He had a budget of {self.budget[i]} and now a budget of {self.budget[i + 1]}. He has spent {self.budget[i] - self.budget[i+1]}.\n")

# Creation of the subclass "Regular Recurrent" with its attributes :
class RegularRecurrent(Recurrent):
    def __init__(self, id):
        super().__init__(id)
        self.budget = [250]
        self.time = []
        self.foods = []
        self.drinks = []

# Creation of the subclass "Hipster Recurrent" with its attributes :
class HipsterRecurrent(Recurrent):
    def __init__(self, id):
        super().__init__(id)
        self.budget = []
        self.time = []
        self.foods = []
        self.drinks = []

    def buy(self, time, date, pricesdrink, pricesfood, list_key, dictionary_input, dictionary_probabilities_drinks, dictionary_probabilities_foods, UberEat = None,UberEatFees = None):
        drink = random.choices(list_key[1:7], weights=dictionary_probabilities_drinks[time])[0]
        food = random.choices(list_key[7:], weights=dictionary_probabilities_foods[time])[0]
        self.time.append(
            dt.datetime.combine(date, dt.datetime.strptime(dictionary_input["TIME"][time], '%H:%M:%S').time()))
        self.foods.append(food)
        self.drinks.append(drink)
        if UberEat == True :
            self.budget.append(self.budget[-1] - pricesfood[food] - pricesdrink[drink] - UberEatFees)
        else :
            self.budget.append(self.budget[-1] - pricesfood[food] - pricesdrink[drink])

    # Function which allow to know if the Hipster with uber is able to buy or not :
    def is_in_budget(self, pricesdrink, pricesfood, ubereat = None, UberEatFees = None):
        if ubereat == True :
            if self.budget[-1] < max(pricesdrink.values()) + max(pricesfood.values()) + UberEatFees:
                return False
            else:
                return True
        else :
            if self.budget[-1] < max(pricesdrink.values()) + max(pricesfood.values()):
                return False
            else:
                return True