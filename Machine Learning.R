# Import Libraries
library("readxl")
library("dplyr")
require(dplyr)
require(rlang)
library(purrr)



# Creation of the Data Set
outputLaevenAndValenciaRaw <- read_excel("./Laeven and Valencia, 2013 and 2018.xlsx", sheet = 2)
WorldBankDataRaw <- read_excel("./WB data.xlsx")
WorldBankexternernaldebtRaw <- read_excel("./WB data external debt.xlsx")
OpennessIndexWB <- read.csv("./OpennessIndexWB.csv", sep =",")


# Change some column name
WorldBankDataRaw <- WorldBankDataRaw %>% 
                                      rename(Country = `Country Name`)
WorldBankexternernaldebtRaw <- WorldBankexternernaldebtRaw %>% 
                                      rename(Country = `Country Name`)


# change some country name in order that they are the same

WorldBankDataRaw$Country[WorldBankDataRaw$Country == "Hong Kong SAR, China"] <- "China, P.R.: Hong Kong"
WorldBankDataRaw$Country[WorldBankDataRaw$Country == "Czechia"] <- "Czech Republic"

#Creation of the subset advanced countries
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


# Filtering the countries
outputLaevenAndValenciaAdvanced <- filter(outputLaevenAndValenciaRaw, Country %in% AdvancedCountry)
WorldBankDataAdvanced <- filter(WorldBankDataRaw, Country %in% AdvancedCountry)

#PB for this one !!!
#!
#!
WorldBankexternernaldebtAdvanced <- filter(WorldBankexternernaldebtRaw, Country == "Australia")
#!
#!




