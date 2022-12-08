
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
library(doSNOW)
set.seed(777)

#put processor cores
cores <- makeCluster(4)
registerDoSNOW(cores)


#Defining predictors and response variables
predictors <- as.matrix(df_fitting[,c(3:8)])
crises     <- as.matrix(df_fitting[,9])

#Creating a test and training sample
train.index<- sample(1:603,400)
ytrue<-crises[-train.index]

#Let's look at how the number of trees and nodes impacts Sensitivity, precision and accuracy

x_dev = as.matrix(dev_data)

tree_depth = 1:3
n_trees = c(
  (seq(from = 10, to = 20, by = 10)),
  (seq(from = 250, to = 400, by = 50)))
# AdaBoost will run 555 times. This is going to take a long time!

start_time <- Sys.time()

# packages for parallel processing


numOfclusters <- 4 # yup, i've got 10+ clusters on my machine :)
cl <- makeCluster(numOfclusters)
registerDoSNOW(cl)

adaBoost_grid <-
  foreach(t = tree_depth) %:%
  foreach(n = n_trees) %dopar% {
    mod = JOUSBoost::adaboost(X = predictors, y = crises, tree_depth = t, n_rounds =n)
    # training results
    tr_conf = mod$confusion_matrix
    tr_precision = tr_conf[2,2]/sum(tr_conf[,2])
    tr_recall = tr_conf[2,2]/sum(tr_conf[2,])
    tr_accuracy = (tr_conf[1,1] + tr_conf[2,2]) / length(crises)
    tr_f1_score = 2 * tr_precision * tr_recall / (tr_precision + tr_recall)
    
    # dev_test results
    preds = JOUSBoost::predict.adaboost(mod, X=predictors, type="response")
    conf = table(crises, preds)
    precision = conf[2,2]/sum(conf[,2])
    recall = conf[2,2]/sum(conf[2,])
    accuracy = (conf[1,1] + conf[2,2]) / length(crises)
    f1_score = 2 * precision * recall / (precision + recall)
    
    # metrics to append
    model_results = data.frame(algo = "AdaBoost",
                               tree_depth = t,
                               n_trees = n,
                               accuracy = accuracy,
                               precision = precision,
                               recall = recall,
                               f1_score = f1_score,
                               training_accuracy = tr_accuracy,
                               training_precision = tr_precision,
                               training_recall = tr_recall,
                               training_f1_score = tr_f1_score,
                               stringsAsFactors = FALSE)
    
    # final list item to return
    list(model = mod, metrics = model_results)
  }
