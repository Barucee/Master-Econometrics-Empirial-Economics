################################### Part 2 - SubClasses OnlyOnce ####################################
## In this file we create the subclasses of the customers "OnlyOnce" and the functions :           ##
##    - is_in_budget() : To know if the customer is able to buy or not                             ##
##    - buy() : To allow the customer to buy drinks and food + append the choice to its attributes ##
##    - purchase history() : to allow the customer to "say" what he did                            ##
#####################################################################################################

#Import the libraries
from P2Customer import Customer
import random
import datetime as dt

# Creation of the subclass "OnlyOnce" with its attributes :
class OnlyOnce(Customer):
    def __init__(self, id):
        super().__init__(id)
        self.budget = [100]
        self.time = []
        self.foods = []
        self.drinks = []

# Creation of the subclass "OnlyOnce" with its attributes :
class TripAdvisor(OnlyOnce):
    def __init__(self, id):
        super().__init__(id)
        self.budget = [100]
        self.tip = round(random.uniform(1, 10), 2)
        self.time = []
        self.foods = []
        self.drinks = []

    # Function which allow to know if the TripAdvisor customer is able to buy or not :
    def is_in_budget(self, pricesdrink, pricesfood):
        if self.budget[-1] < max(pricesdrink.values()) + max(pricesfood.values()) + self.tip:
            return False
        else:
            return True

    # Function which allow the customer to buy drinks and food + append the choice to its attributes :
    def buy(self, time, date, pricesdrink, pricesfood, list_key, dictionary_input, dictionary_probabilities_drinks, dictionary_probabilities_foods):
        drink = random.choices(list_key[1:7], weights=dictionary_probabilities_drinks[time])[0]
        food = random.choices(list_key[7:], weights=dictionary_probabilities_foods[time])[0]
        self.time.append(
            dt.datetime.combine(date, dt.datetime.strptime(dictionary_input["TIME"][time], '%H:%M:%S').time()))
        self.foods.append(food)
        self.drinks.append(drink)
        self.budget.append(self.budget[-1] - pricesfood[food] - pricesdrink[drink] - self.tip)

    # function which allow the customer to "say" what he did :
    def purchase_history(self):
        if self.foods[-1] == " ":
            return print(
                f"On {self.time[-1].date()} at {self.time[-1].time()}, the Customer {self.id} has bought {self.drinks[-1]}. He gave a tip of {self.tip}\n"
                f"He had a budget of {self.budget[-2]} and now a budget of {self.budget[-1]}. He has spent {self.budget[-2] - self.budget[-1]}.")
        else:
            return print(
                f"On {self.time[-1].date()} at {self.time[-1].time()}, the Customer {self.id} has bought {self.foods[-1]} and {self.drinks[-1]}. He gave a tip of {self.tip}\n"
                f"He had a budget of {self.budget[-2]} and now a budget of {self.budget[-1]}. He has spent {self.budget[-2] - self.budget[-1]}.")
