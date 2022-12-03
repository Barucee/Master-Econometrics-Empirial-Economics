
# Import Libraries
library("readxl")
library("dplyr")
require(dplyr)
library(tidyr)
require(rlang)
library(purrr)
library(knitr)




# Dataset Creation --------------------------------------------------------

WorldBankDataRaw <- read_excel("./WB data.xlsx", na = "..")
IMFPublicDebtToGDP <- read_excel("./IMF - Public Debt-to-GDP.xls", na="no data")
crises <- read_excel("./Crises.xlsx" )
crises<-crises[,-4]



# Changing the following variable names: country, account balance 
WorldBankDataRaw <- WorldBankDataRaw %>% 
  rename(Country = `Country Name`)%>% 
  rename(acc_balance = `Account balance`)


IMFPublicDebtToGDP <- IMFPublicDebtToGDP %>% 
  rename(Country = `General Government Debt (Percent of GDP)`)


# Changing Czechia to Czech Republic

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


# Filtering the advanced countries
crisesAdvanced             <- filter(crises, Country %in% AdvancedCountry)
WorldBankDataAdvanced      <- filter(WorldBankDataRaw, Country %in% AdvancedCountry)
IMFPublicDebtToGDPAdvanced <- filter(IMFPublicDebtToGDP, Country %in% AdvancedCountry) #Pas Hong Kong

rm(crises)
rm(IMFPublicDebtToGDP)
rm(WorldBankDataRaw)
rm(AdvancedCountry)

# Transforming IMF data from wide to long
IMFPublicDebtToGDPAdvanced <- gather(
  IMFPublicDebtToGDPAdvanced, 
  Year, "Public Debt To GDP", 
  "1950":"2020", factor_key=FALSE)



#Changing variable type
IMFPublicDebtToGDPAdvanced$Year    <- as.numeric(IMFPublicDebtToGDPAdvanced$Year)
IMFPublicDebtToGDPAdvanced$Country <- as.array(IMFPublicDebtToGDPAdvanced$Country)
WorldBankDataAdvanced$Country      <- as.array(WorldBankDataAdvanced$Country)

#Merging IMF and WB data
df <- merge(IMFPublicDebtToGDPAdvanced,WorldBankDataAdvanced)

#Creating the "openness index"
df$openness_index <- df$Exports + df$Imports

#selecting variables of interest
df <- select(df, -c(`External debt`,`Exports`,`Imports`,`Public debt`,`Bank capital to assets ratio (%)`) )

#converting 'crises' data from quarterly to yearly
crisesAdvanced <- crisesAdvanced %>%
                group_by(Country, Year) %>%
  summarize(credit_gdp = mean(credit_gdp_ratio, na.rm=TRUE),
            banking_crysis = max(`Banking crisis`)) 

#Merging with crises data 
df <- merge(df,crisesAdvanced)
rm(WorldBankDataAdvanced)
rm(IMFPublicDebtToGDPAdvanced)
rm(crisesAdvanced)



# Variable Preparation ----------------------------------------------------
library(datawizard)



# standardizing the variables

df <- df %>%
  group_by(Country) %>%
  standardize(, select=c('Public Debt To GDP', 
                         'Credit to private sector',
                         'Inflation',
                         'openness_index',
                         'credit_gdp'))          

#removing NAs
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

df_fitting <- df_noNA %>%
  select(-banking_crysis, -Pre1, -Pre2)


# Adaboost ----------------------------------------------------------------

library(JOUSBoost) 
library(foreach)
library(doSNOW)
library(kableExtra)
set.seed(777)




#Defining predictors and response variables
predictors <- as.matrix(df_fitting[,c(3:8)])
crises     <- as.matrix(df_fitting[,9])

#Creating a test and training sample
train.index <- sample(1:603,400)
xtrain      <- as.matrix(predictors[train.index,])
xtest       <- as.matrix(predictors[-train.index,])
ytrain      <- as.matrix(crises[train.index])
ytest       <- as.matrix(crises[-train.index])


#GridSearch
#Let's look at how the number of trees and nodes impacts 
#Sensitivity, Precision and Accuracy




tree.nodes = 1:6 
trees.num = c(
  (seq(from = 25, to = 100, by  = 25)), 
  (seq(from = 150, to = 300 , by  = 50))) 

#this is for parallel processing, put processor cores in clusters

numOfclusters <- 4 
cl <- makeCluster(numOfclusters)
registerDoSNOW(cl)


adaboost.grid <-
  foreach(t = tree.nodes) %:%
  foreach(n = trees.num)  %dopar% {
    adabst = JOUSBoost::adaboost(X = xtrain, y = ytrain, tree_depth = t, n_rounds =n)
    
    confusion.train   = matrix(c(adabst$confusion_matrix,0,0),nrow=2)
    accuracy.train    = (confusion.train[2,2] + confusion.train[1,1] ) / sum(confusion.train)
    precision.train   = confusion.train[2,2]  / sum(confusion.train[,2])
    sensitivity.train = confusion.train[2,2]  / sum(confusion.train[2,])
    
    adabst.yhat = JOUSBoost::predict.adaboost(adabst, xtest, type="response")
    confusion.test   = matrix(c(table(ytest, adabst.yhat),0,0),nrow=2)
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
      #training_accuracy = accuracy.train,
      #training_precision = precision.train,
      #training_sensitivity = sensitivity.train,
      stringsAsFactors = FALSE)
    
      list(model = adabst, metrics = results)
  }


adaboost_metrics <- 
  lapply(adaboost.grid, function(outside_item) 
  {lapply(outside_item, function(inside_item)
  { inside_item[['metrics']]})})

adaboost_metrics <- as_tibble(bind_rows(lapply(adaboost_metrics, bind_rows)))
adaboost_metrics$accuracy  <-round(adaboost_metrics$accuracy, 3)
adaboost_metrics$precision <-round(adaboost_metrics$precision, 3)
adaboost_metrics$sensitivity <-round(adaboost_metrics$sensitivity, 3)
adaboost_metrics$score <- ( 0.5*adaboost_metrics$sensitivity + 
                         0.3*adaboost_metrics$precision +
                         0.2*adaboost_metrics$accuracy)
adaboost_metrics$score <-round(adaboost_metrics$score, 3)

adagridtab1<-adaboost_metrics[1:24,] %>% 
  select(tree.nodes, tree.num, accuracy, precision, sensitivity, score) %>% 
  arrange(tree.nodes, tree.num)

adagridtab2<-adaboost_metrics[25:48,] %>% 
  select(tree.nodes, tree.num, accuracy, precision, sensitivity, score) %>% 
  arrange(tree.nodes, tree.num)


adagridtab_1 <- kbl(adagridtab1, 'latex', caption = "Adaboost Gridsearch, 1 to 3 nodes", booktabs=T, 
             linesep=c("","", "", "","", "", "", "\\hline")) %>% #LINESEP A CHANGER SI ON CHANGE LE GRID
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  column_spec(3, color = spec_color(adagridtab1$accuracy, end = 0.8, direction = -1))%>%
  column_spec(4, color = spec_color(adagridtab1$precision, end = 0.8, direction = -1))%>%
  column_spec(5, color = spec_color(adagridtab1$sensitivity, end = 0.8, direction = -1))%>%
  column_spec(6, color = spec_color(adagridtab1$score, end = 0.8, direction = -1))


adagridtab_2 <- kbl(adagridtab2, 'latex', caption = "Adaboost Gridsearch, 4 to 6 nodes", booktabs=T, 
                    linesep=c("","", "", "","", "", "", "\\hline")) %>% #LINESEP A CHANGER SI ON CHANGE LE GRID
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  column_spec(3, color = spec_color(adagridtab2$accuracy, end = 0.8, direction = -1))%>%
  column_spec(4, color = spec_color(adagridtab2$precision, end = 0.8, direction = -1))%>%
  column_spec(5, color = spec_color(adagridtab2$sensitivity, end = 0.8, direction = -1))%>%
  column_spec(6, color = spec_color(adagridtab1$score, end = 0.8, direction = -1))
  
save_kable(adagridtab_1,'adagrid1.tex')
save_kable(adagridtab_2,'adagrid2.tex')


