
# Import Libraries
library("readxl")
library("dplyr")
require(dplyr)
library(tidyr)
require(rlang)
library(purrr)



# Creation of the Data Set
WorldBankDataRaw <- read_excel("./WB data.xlsx", na = "..")
IMFPublicDebtToGDP <- read_excel("./IMF - Public Debt-to-GDP.xls", na="no data")
crises <- read_excel("./Crises.xlsx" )
crises<-crises[,-4]



# Change some variables'names
WorldBankDataRaw <- WorldBankDataRaw %>% 
  rename(Country = `Country Name`)%>% 
  rename(acc_balance = `Account balance`)


IMFPublicDebtToGDP <- IMFPublicDebtToGDP %>% 
  rename(Country = `General Government Debt (Percent of GDP)`)


# change some country name in order that they are the same

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
crisesAdvanced <- filter(crises, Country %in% AdvancedCountry)
WorldBankDataAdvanced <- filter(WorldBankDataRaw, Country %in% AdvancedCountry)
IMFPublicDebtToGDPAdvanced <- filter(IMFPublicDebtToGDP, Country %in% AdvancedCountry) #Pas Hong Kong

rm(crises)
rm(IMFPublicDebtToGDP)
rm(WorldBankDataRaw)

# Transforming IMF data from wide to long
IMFPublicDebtToGDPAdvanced <- gather(
  IMFPublicDebtToGDPAdvanced, 
  Year, "Public Debt To GDP", 
  "1950":"2020", factor_key=FALSE)




IMFPublicDebtToGDPAdvanced$Year<- as.numeric(IMFPublicDebtToGDPAdvanced$Year)
IMFPublicDebtToGDPAdvanced$Country<- as.array(IMFPublicDebtToGDPAdvanced$Country)
WorldBankDataAdvanced$Country <- as.array(WorldBankDataAdvanced$Country)

#Merging IMF and WB data
df<- merge(IMFPublicDebtToGDPAdvanced,WorldBankDataAdvanced)

#Creating the "openness index"
df$openness_index <- df$Exports + df$Imports

#selecting variables of interest
df <- select(df, -c(`External debt`,`Exports`,`Imports`,`Public debt`,`Bank capital to assets ratio (%)`) )

#converting 'crises' data to yearly
crisesAdvanced <- crisesAdvanced %>%
  group_by(Country, Year) %>%
  summarize(credit_gdp = mean(credit_gdp_ratio, na.rm=TRUE),
            banking_crysis = max(`Banking crisis`)) 

#Merging with  crises data 
df<- merge(df,crisesAdvanced)
rm(WorldBankDataAdvanced)
rm(IMFPublicDebtToGDPAdvanced)
rm(crisesAdvanced)

#Creating the variable corresponding to "pre crysis year"as in the paper
#Pre-crisis: 1 if a crisis occurs in the next 3 years


df_noNA<- df %>%
  na.omit(df)

#a ne pas run sur df a cause du mutate_all
df_noNA<- df_noNA %>% 
  group_by(Country) %>%
  mutate(Pre1 = lead(banking_crysis), Pre2 = lead(banking_crysis, 2))%>%
  mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
  mutate(crysis = banking_crysis + Pre1 + Pre2 )









