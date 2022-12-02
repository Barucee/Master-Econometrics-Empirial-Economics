
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

#Variable preparation
library(datawizard)


#detrending and standardizing the variables

df<- df %>%
  group_by(Country) %>%
  standardize(, select=c('Public Debt To GDP', 'Credit to private sector',
                         'Inflation','openness_index','credit_gdp'))          

df_noNA<- df %>%
  na.omit(df)


#Creating the variable corresponding to "pre crysis year"as in the paper
#Pre-crisis: 1 if a crisis occurs in the next 3 years

df_noNA<- df_noNA %>% 
  group_by(Country) %>%
  mutate(Pre1 = lead(banking_crysis), Pre2 = lead(banking_crysis, 2))%>%

    mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
  mutate(crysis = banking_crysis + Pre1 + Pre2 )

df_noNA$crysis[df_noNA$crysis == 0] <- -1 

df_fitting<-df_noNA %>%
  select(-banking_crysis, -Pre1, -Pre2)

  
# Adaboost
library(JOUSBoost) 
library(foreach)
install.packages("doSNOW")
library(doSNOW)
set.seed(777)

#put processor cores
cores <- makeCluster(1)
registerDoSNOW(cores)


#Defining predictors and response variables
predictors <- as.matrix(df_fitting[,c(3:8)])
crises     <- as.matrix(df_fitting[,9])

#Creating a test and training sample
train.index<- sample(1:603,400)
ytrue<-crises[-train.index]

#Let's look at how the number of trees and nodes impacts Sensitivity, precision and accuracy



tree.nodes = 1:6
trees.num = c( (seq(20 , 100 , by  = 10)), 
             (seq(100, 250 , by  = 50))) 

adaboost.grid <-
  foreach(t = tree.nodes) %:%
    foreach(n = trees.num)  %dopar% {
      adabst = JOUSBoost::adaboost(predictors[train.index,], crises[train.index,], tree_depth = t, n_rounds =n)

      confusion.train   = adabst$confusion_matrix
      accuracy.train    = (confusion.train[2,2] + confusion.train[1,1] ) / sum(confusion.train)
      precision.train   = confusion.train[2,2]  / sum(confusion.train[,2])
      sensitivity.train = confusion.train[2,2]  / sum(confusion.train[2,])
  
      adabst.yhat = JOUSBoost::predict.adaboost(adabst,predictors[-train.index,], type="response")
      confusion.test = table(ytrue, adabst.yhat)
      accuracy.test    = (confusion.test[2,2] + confusion.test[1,1] ) / sum(confusion.test)
      precision.test   = confusion.test[2,2]  / sum(confusion.test[,2])
      sensitivity.test = confusion.test[2,2]  / sum(confusion.test[2,])
  
      results = data.frame(
        algorithm = "AdaBoost",
        tree.nodes = t,
        tree.num = n,
        accuracy = accuracy.test,
        precision = precision.test,
        sensitivity = sensitivity.test,
        training_accuracy = accuracy.train,
        training_precision = precision.train,
        training_sensitivity = sensitivity.train,
        stringsAsFactors = FALSE)
  
      results<- list(model = adabst, metrics = results)
  }



