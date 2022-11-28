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
IMFPublicDebtToGDP <- read_excel("./IMF - Public Debt-to-GDP.xls")
WorldGDP <- read_excel("./WB data GDP.xls")
IMFPublicDebtToGDP <- read_excel("./IMF - Public Debt-to-GDP.xls", na="no data")


#PB OpennesIndexWB
## !
OpennessIndexWB <- read.csv("./OpennessIndexWB.csv", sep =",")
## !
#PB OpennesIndexWB


# Change some column name
WorldBankDataRaw <- WorldBankDataRaw %>% 
  rename(Country = `Country Name`)

IMFPublicDebtToGDP <- IMFPublicDebtToGDP %>% 
  rename(Country = `General Government Debt (Percent of GDP)`)
IMFPublicDebtToGDP <- IMFPublicDebtToGDP %>% 
  rename(Year = `year`)
WorldGDP <- WorldGDP %>% 
  rename(Country = `Country Name`)


# change some country name in order that they are the same

WorldBankDataRaw$Country[WorldBankDataRaw$Country == "Hong Kong SAR, China"] <- "China, P.R.: Hong Kong"
WorldBankDataRaw$Country[WorldBankDataRaw$Country == "Czechia"] <- "Czech Republic"
IMFPublicDebtToGDP$Country[IMFPublicDebtToGDP$Country == "Korea, Republic of"] <- "Korea"

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
WorldGDP <- filter(WorldGDP,Country == "World")


#delete some columns for world

drop <- c("Country Code","Indicator Name","Indicator Code","Country")
WorldGDP = WorldGDP[,!(names(WorldGDP) %in% drop)]


# Transform all from wide to long
IMFPublicDebtToGDPAdvanced <- gather(IMFPublicDebtToGDPAdvanced, Year, "Public Debt To GDP", "1950":"2020", factor_key=TRUE)
WorldGDP <- gather(WorldGDP, Year, "GDP growth", "1960":"2021", factor_key=TRUE)


# Transform all from wide to long
IMFPublicDebtToGDPAdvanced <- gather(IMFPublicDebtToGDPAdvanced, year, "Public Debt To GDP", "1950":"2020", factor_key=FALSE)


#Merging 
IMFPublicDebtToGDPAdvanced <- IMFPublicDebtToGDPAdvanced %>% 
  rename ( Year = year) 
IMFPublicDebtToGDPAdvanced$Year<- as.numeric(IMFPublicDebtToGDPAdvanced$Year)
IMFPublicDebtToGDPAdvanced$Country<- as.array(IMFPublicDebtToGDPAdvanced$Country)

WorldBankDataAdvanced$Country <- as.array(WorldBankDataAdvanced$Country)
df<- merge(IMFPublicDebtToGDPAdvanced,WorldBankDataAdvanced)
df$openness_index <- df$Exports + df$Imports
df <- select(df, -c(`External debt`,`Exports`,`Imports`,`Public debt`,`Bank capital to assets ratio (%)`) )


#Merging 
IMFPublicDebtToGDPAdvanced <- IMFPublicDebtToGDPAdvanced %>% 
  rename ( Year = year) 
IMFPublicDebtToGDPAdvanced$Year<- as.numeric(IMFPublicDebtToGDPAdvanced$Year)
IMFPublicDebtToGDPAdvanced$Country<- as.array(IMFPublicDebtToGDPAdvanced$Country)

WorldBankDataAdvanced$Country <- as.array(WorldBankDataAdvanced$Country)
df<- merge(IMFPublicDebtToGDPAdvanced,WorldBankDataAdvanced)
df$openness_index <- df$Exports + df$Imports
df <- select(df, -c(`External debt`,`Exports`,`Imports`,`Public debt`,`Bank capital to assets ratio (%)`) )



#coding crises
outputLaevenAndValenciaAdvanced <- outputLaevenAndValenciaAdvanced[,c(1,2)]
outputLaevenAndValenciaAdvanced <- outputLaevenAndValenciaAdvanced %>% 
  separate_rows("Systemic Banking Crisis (starting date)", sep=",") %>% 
  separate("Systemic Banking Crisis (starting date)", into=c("Year"),sep = "_", convert = TRUE)
outputLaevenAndValenciaAdvanced <- outputLaevenAndValenciaAdvanced %>% drop_na(Year)
outputLaevenAndValenciaAdvanced$Crisis <- 1


df <- merge(x=WorldBankDataAdvanced,y=IMFPublicDebtToGDPAdvanced, 
            by=c("Country","Year"), all.x=TRUE)
df <- merge(x=df,y=WorldGDP, 
            by=c("Year"), all.x=TRUE)
df <- merge(x=df,y=outputLaevenAndValenciaAdvanced, 
            by=c("Country","Year"), all.x=TRUE)