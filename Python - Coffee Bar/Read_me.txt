COFFEE BAR SIMULATION :

The present project contains a variety of files with the following structure:
1) The Code Directory:
    - P1Exploratory: A dataset containing information of the drink and food bought in a Coffee bar over 5 a span
        of five years is explored. The probabilities of each food and drink being sold at each time are calculated.
    - P2Customer, P2OnlyOnce, P2Recurrent: file containing the class description of the type of customers.
    - P3SimulationClass, P3Simulation: First file containing the description of a simulation of a coffeeshop, using the
        probabilities calculated in P1Exploratory and the customers described in the P2 files, second to run and print
        the simulation results
    - Part 4 -(Q1-Q6): Further proof of the simulation working (Q1) and counterfactual scenarios (Q2-Q6)
    - Extra Plots: plots regarding the provided data frame and the one created
2) Data Directory: contains the provided data frame and three other files containing probabilities.
3) Documentation Directory: Exercise description.
4) Results Directory: Saved graphs and simulation results saved.

ASSUMPTIONS MADE:
- The Coffee Bar is open every day (no exception) during 5 years
- The Data frame replicates exclusively the act of selling fixed food and drinks (no special orders, no discounts,
    no cost consideration)
- There is always someone buying while the store is open

SET UP: The parameters set are: probabilityTripAdvisor, nbReturning, probabilityHipster,probabilityRecurrent,
    BudgetHipster, pricesdrink (dictionary), pricesfood(same), start date(dt.date() function),
    end date (dt.date() function), delta of days (dt.timedelta), ChangeOfPrice, TimeChangeOfPrice(dt.date()),
    UberEat(Boolean), ProbabilityUberEat (float 0,1), UberEatFees, PercentOfUberEatForTheCoffee

    The only attribute compulsory to launch the simulation is the path of the data frame output

DATABASE CONFIGURATION: All databases, the initial one and the ones produced in this simulation are csv files

VERSION: The version used is Python 3.9

DEPENDENCIES: The following libraries have been used: os, pandas, random, matplotlib.pyplot, datetime, and the function
chi2_contingency from the library scipy.stats


