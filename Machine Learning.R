# Import Libraries
library("readxl")
<<<<<<< HEAD
library("dplyr")
require(dplyr)
require(rlang)
=======
library(dplyr)
library(purrr)
>>>>>>> bbef5363fa656a2c6231539871f171f5458e345f


<<<<<<< HEAD
# Creation of the Data Set
outputLaevenAndValenciaRaw <- read_excel("./Laeven and Valencia, 2013 and 2018.xlsx", sheet = 2)
WorldBankDataRaw <- read_excel("./WB data.xlsx")

# Change some column name
WorldBankDataRaw <- WorldBankDataRaw %>% 
                                      rename(Country = `Country Name`)


#Filter Data set
AdvancedCountry <- c("Australia"
                     , "Austria"
                     , "Belgium"
                     , "Canada"
                     , "Czech Republic"
                     , "Czechia"
                     , "Denmark"
                     , "Estonia"
                     , "Finland"
                     , "France"
                     , "Germany"
                     , "Greece"
                     , "China, P.R.: Hong Kong"
                     , "Hong Kong SAR, China"
                     , "Iceland"
                     , "Ireland"
                     , "Israel"
                     , "Italy"
                     , "Japan"
                     , "Korea"
                     , "Korea, Rep."
                     , "Latvia"
                     , "Lithuania"
                     , "Luxembourg"
                     , "Netherlands"
                     , "New Zealand"
                     , "Norway"
                     , "Portugal"
                     , "Singapore"
                     , "Slovak Republic"
                     , "Slovenia"
                     , "Spain"
                     , "Sweden"
                     , "Switzerland"
                     , "United Kingdom"
                     , "United States")

#attention Hong Kong avec une rédaction special
outputLaevenAndValenciaAdvanced <- filter(outputLaevenAndValenciaRaw, Country %in% AdvancedCountry)
WorldBankDataAdvanced <- filter(WorldBankDataRaw, Country %in% AdvancedCountry)

=======
WorldBankData <- read_excel("./WB data.xlsx", na="..")
>>>>>>> bbef5363fa656a2c6231539871f171f5458e345f
