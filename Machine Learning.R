#Creation of the Data Set
library("readxl")

outputLaevenAndValencia <- read_excel("./Laeven and Valencia, 2013 and 2018.xlsx", sheet = 2)

WorldBankData <- read_excel("./WB data.xlsx")
