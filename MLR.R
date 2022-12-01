
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

df<- detrend(df, select=c('Public Debt To GDP', 'Credit to private sector',
                          'Inflation','openness_index','credit_gdp','Inflation','acc_balance'), group='Country')

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

  
# 
library(JOUSBoost) #https://www.rdocumentation.org/packages/JOUSBoost/versions/2.1.0/topics/adaboost
set.seed(777)





#Defining predictors and response variables
predictors <- as.matrix(df_fitting[,c(3:8)])
crises     <- as.matrix(df_fitting[,9])


#Estimating the average Sensitivity, Accuracy and Precision over 100 iterarions

#A changer
for (i in (1:2) ) {
  train_index_`i`<- sample(1:603,400)
  replication_`i`<- adaboost(predictors[train_index_`i`,], crises[train_index_`i`], 1, 30, F)
  predictions_test<-predict(replication,predictors[-train_index_`i`,], 'response')
  x<-table(predictions_test_`i`,crises[-train_index_`i`])
  confusion_matrix <- 0 + x   
}


set.seed(777)

train_index<- sample(1:603,400)
replication<- adaboost(predictors[train_index,], crises[train_index], 1, 30, F)
predictions_test<-predict(replication,predictors[-train_index,], 'response')





#xg boost

install.packages("xgboost")
require(xgboost)
library(data.table)
library(mlr)

# https://www.hackerearth.com/practice/machine-learning/machine-learning-algorithms/beginners-tutorial-on-xgboost-parameter-tuning-r/tutorial/

df_fittingtable <- setDT(df_fitting) 

labels <- df_fitting$crysis 

new_tr <- model.matrix(~+0,data = df_fittingtable[,-c("crysis"),with=F]) 

dtrain <- xgb.DMatrix(data = new_tr,label = labels) 

params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)

xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 100, nfold = 5, showsd = T, stratified = T, print.every.n = 10, early.stop.round = 20, maximize = F)

