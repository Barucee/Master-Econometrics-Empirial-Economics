# Import Libraries
library("readxl")
library("dplyr")
require(dplyr)
library(tidyr)
require(rlang)
library(purrr)



# Creation of the Data Set
outputLaevenAndValenciaRaw <- read_excel("./Laeven and Valencia, 2013 and 2018.xlsx", sheet = 2)
WorldBankDataRaw <- read_excel("./WB data.xlsx", na = "..")
WorldBankexternernaldebtRaw <- read_excel("./WB data external debt.xlsx")
IMFPublicDebtToGDP <- read_excel("./IMF - Public Debt-to-GDP.xls", na="no data")

#PB OpennesIndexWB
## !
OpennessIndexWB <- read.csv("./OpennessIndexWB.csv", sep =",")
## !
#PB OpennesIndexWB

# Change some column name
WorldBankDataRaw <- WorldBankDataRaw %>% 
                                      rename(Country = `Country Name`)

WorldBankexternernaldebtRaw <- WorldBankexternernaldebtRaw %>% 
                                      rename(Country = `Country Name`)

IMFPublicDebtToGDP <- IMFPublicDebtToGDP %>% 
                                      rename(Country = `General Government Debt (Percent of GDP)`)




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
                     , "Korea, Republic of"
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
IMFPublicDebtToGDPAdvanced <- filter(IMFPublicDebtToGDP, Country %in% AdvancedCountry) #Pas Hong Kong

#PB for this one !!!
#!
#!
WorldBankexternernaldebtAdvanced <- filter(WorldBankexternernaldebtRaw, Country == "Australia")
#!
#!


# Transform all from wide to long
IMFPublicDebtToGDPAdvanced <- gather(IMFPublicDebtToGDPAdvanced, year, "Public Debt To GDP", "1950":"2020", factor_key=TRUE)


#coding crises
outputLaevenAndValenciaAdvanced <- outputLaevenAndValenciaAdvanced[,c(1,2)]
outputLaevenAndValenciaAdvanced <- outputLaevenAndValenciaAdvanced %>% 
  separate_rows("Systemic Banking Crisis (starting date)", sep=",") %>% 
  separate("Systemic Banking Crisis (starting date)", into=c("Crisis"),sep = "_", convert = TRUE)









