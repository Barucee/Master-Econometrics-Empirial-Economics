
# Import Libraries
library("readxl")
library("dplyr")
require(dplyr)
library(tidyr)
require(rlang)
library(purrr)
library(knitr)




# Dataset Creation --------------------------------------------------------


WorldBankDataRaw   <- read_excel("./WB data.xlsx", na = "..")
IMFPublicDebtToGDP <- read_excel("./IMF - Public Debt-to-GDP.xls", na = "no data")
crises             <- read_excel("./Crises.xlsx")
crisesm            <- crises[, -4]



# Changing the following variable names: country, account balance 
WorldBankDataRaw    <- WorldBankDataRaw %>%
  rename(Country     = `Country Name`) %>%
  rename(acc_balance = `Account balance`)

IMFPublicDebtToGDP <- IMFPublicDebtToGDP %>%
  rename(Country   = `General Government Debt (Percent of GDP)`)


# Changing Czechia to Czech Republic

WorldBankDataRaw$Country[WorldBankDataRaw$Country == "Czechia"] <-
  "Czech Republic"

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
IMFPublicDebtToGDPAdvanced <- gather(IMFPublicDebtToGDPAdvanced,
                                     Year,
                                     "Public Debt To GDP",
                                     "1950":"2020",
                                     factor_key = FALSE)



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
  summarize(
    credit_gdp = mean(credit_gdp_ratio, na.rm = TRUE),
    banking_crysis = max(`Banking crisis`)
  ) 

#Merging with crises data 
df <- merge(df, crisesAdvanced)
rm(WorldBankDataAdvanced)
rm(IMFPublicDebtToGDPAdvanced)
rm(crisesAdvanced)


# Variable Preparation ----------------------------------------------------
library(datawizard)



# standardizing the variables

df <- df %>%
  group_by(Country) %>%
  standardize(
    ,
    select = c(
      'Public Debt To GDP',
      'Credit to private sector',
      'Inflation',
      'openness_index',
      'credit_gdp'
    )
  )

#removing NAs
df_noNA <- df %>%
  na.omit(df)


#Creating the variable corresponding to "pre crysis year"as in the paper
#Pre-crisis: 1 if a crisis occurs in the next 3 years

df_noNA <- df_noNA %>%
  group_by(Country) %>%
  mutate(Pre1 = lead(banking_crysis),
         Pre2 = lead(banking_crysis, 2)) %>%
  mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
  mutate(crysis = banking_crysis + Pre1 + Pre2)

df_noNA$crysis[df_noNA$crysis == 0] <- -1

df_fitting <- df_noNA %>%
  select(-banking_crysis,-Pre1,-Pre2)


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

numOfclusters <- 3
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
    
    adabst.yhat      = JOUSBoost::predict.adaboost(adabst, xtest, type = "response")
    confusion.test   = matrix(c(table(ytest, adabst.yhat),0,0),nrow=2)
    accuracy.test    = (confusion.test[2,2] + confusion.test[1,1] ) / sum(confusion.test)
    precision.test   = confusion.test[2,2]  / sum(confusion.test[,2])
    sensitivity.test = confusion.test[2,2]  / sum(confusion.test[2,])
    
    results = data.frame(
      algorithm   = "AdaBoost",
      tree.nodes  = t,
      tree.num    = n,
      accuracy    = accuracy.test,
      precision   = precision.test,
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

best.ada<-adaboost_metrics[which.max(adaboost_metrics$score),]

# Exporting tables --------------------------------------------------------


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





# Making graphs -----------------------------------------------------------
library(ggplot2)
library(gridExtra)
ada_accuracy<- ggplot(adaboost_metrics, aes(x=tree.nodes, y = accuracy)) + 
  geom_point(aes(colour = as.factor(tree.num)),size = 3) + 
  scale_color_discrete("# of tree") + 
  ggtitle("Accuracy in function of # of tree and node")

ada_precision<- ggplot(adaboost_metrics, aes(x=tree.nodes, y = precision)) + 
  geom_point(aes(colour = as.factor(tree.num)),size = 3) + 
  scale_color_discrete("# of tree") + 
  ggtitle("Precision in function of # of tree and node")

ada_sensitivity<- ggplot(adaboost_metrics, aes(x=tree.nodes, y = sensitivity)) + 
  geom_point(aes(colour = as.factor(tree.num)),size = 3) + 
  scale_color_discrete("# of tree") + 
  ggtitle("Sensitivity in function of # of tree and nodes")

ada_score <- ggplot(adaboost_metrics, aes(x=tree.nodes, y = score)) + 
  geom_point(aes(colour = as.factor(tree.num)),size = 3) + 
  scale_color_discrete("# of tree") + 
  ggtitle("Score in function of # of tree and node")

grid.arrange(ada_accuracy, ada_precision,ada_sensitivity,ada_score, nrow = 2)



# Threshold selection -----------------------------------------------------
alphas <- (0:1000)*0.0001

adaboost.threshold <-
  foreach(a = alphas)  %dopar% {
    adabst2 = JOUSBoost::adaboost(X = xtrain, y = ytrain, tree_depth = 1, n_rounds =90)
    adabst2.yhato = JOUSBoost::predict.adaboost(adabst2, xtest, type="prob")
    adabst2.yhat  <- rep(-1, length(adabst2.yhato))
    adabst2.yhat[adabst2.yhato > a] <- 1
    
    confusion.test   = matrix(c(table(ytest, adabst2.yhat),0,0),nrow=2)
    sensitivity.test = confusion.test[2,2]  / sum(confusion.test[2,])
    fpr.test         = confusion.test[1,2] / sum(confusion.test[1,])
    
    results2 = data.frame(
      algorithm = "AdaBoost",
      Threshold = a,
      sensitivity = sensitivity.test,
      FalsePositiveRate = fpr.test,
      stringsAsFactors = FALSE)
    list(model = adabst2, metrics = results2)
  }

adaboost_metrics2 <- 
  lapply(adaboost.threshold, function(inside_item)
  { inside_item[['metrics']]})

adaboost_metrics2 <- as_tibble(bind_rows(lapply(adaboost_metrics2, bind_rows)))
adaboost.AUROC<-sum(adaboost_metrics2$sensitivity*0.001)

ROC_curve <- ggplot(adaboost_metrics2, aes(x = FalsePositiveRate, y=sensitivity )) + 
  geom_line() +
  ggtitle("ROC Curve",subtitle = "AUROC = 0.82")


# Adaboost vs logit -------------------------------------------------------


adabst3 = JOUSBoost::adaboost(X = xtrain, y = ytrain, tree_depth = 5, n_rounds =75)
adabst3.yhat = JOUSBoost::predict.adaboost(adabst3, xtest, type="response")

confusion.best   = matrix(c(table(ytest, adabst3.yhat),0,0),nrow=2)
accuracy.best    = (confusion.best[2,2] + confusion.best[1,1] ) / sum(confusion.best)
precision.best   = confusion.best[2,2]  / sum(confusion.best[,2])
sensitivity.best = confusion.best[2,2]  / sum(confusion.best[2,])
score.best <- ( 0.5*sensitivity.best + 
           0.3*precision.best +
           0.2*accuracy.best)

ytrain2<-as.numeric(ytrain)
ytrain2[ytrain == -1] <- 0
df.logit<-data.frame(cbind(xtrain,ytrain2))
logit<- glm(ytrain2~., df.logit, family = "binomial" )
yhatlogito<- predict(logit, newdata= data.frame(xtest), "response")
yhatlogit <- rep(0, length(yhatlogito))
yhatlogit[yhatlogito>0.5]<-1

confusion.logit   = matrix(c(table(ytest, yhatlogit),0,0),nrow=2)
accuracy.logit    = (confusion.logit[2,2] + confusion.logit[1,1] ) / sum(confusion.logit)
precision.logit   = confusion.logit[2,2]  / sum(confusion.logit[,2])
sensitivity.logit = confusion.logit[2,2]  / sum(confusion.logit[2,])
score.logit <- (  0.5*sensitivity.logit + 
                  0.3*precision.logit   +
                  0.2*accuracy.logit)

compar=data.frame( 
  algorithm   = c("AdaBoost","Logit"),
  accuracy    = c(accuracy.best,accuracy.logit),
  precision   = c(precision.best,precision.logit),
  sensitivity = c(sensitivity.best,sensitivity.logit),
  score       = c(score.best,score.logit) )
  

comparkbl<-kbl(compar, 'latex', caption = "Adaboost vs Logit", booktabs=T)
save_kable(comparkbl,"compar.tex")

















# XGboost ----------------------------------------------------------------

library(xgboost)

# remove the -1 which was only for the adaBoost
ytrain = ifelse(ytrain == 1, 1, 0) 
ytest = ifelse(ytest == 1, 1, 0) 


#Creation of Dataframe --------------------------------------------------------------------
dtrain <- xgb.DMatrix(data = xtrain,
                      label=ytrain)
dvalidation <- xgb.DMatrix(data= xtest,
                           label = ytest)
watchlist <- list(train=dtrain, validation=dvalidation)


#Setting the parameters of the grid --------------------------------------------------------
max_depth <- 2:8 
shrinkage <- c(0.1, 0.01, 0.001, 0.0001) 
xgb_models <- list()
dlist = list()
slist = list()

#Loop on each parameters in order to train the XG-Boost Model ----------------------------------
for(d in max_depth) {
  for(s in shrinkage){
    param <- list(
      objective = "binary:logistic",
      eta = s,
      max_depth = d,
      colsamplebytree = 0.8,
      subsample = 0.8, 
      #min_child_weight = 3,
      base_score = 0.50,
      #lambda = 100,
      #lambda_bias = 5,
      #alpha = 0.1,
      verbose = FALSE
    )
    
    mod = xgboost::xgb.train(
      params = param,
      data = dtrain, 
      nrounds = 1000,
      watchlist = watchlist, 
      nthread = 10,
      maximize = FALSE,
      early_stopping_rounds = 100,
      verbose = FALSE)
    
    xgb_models = append(xgb_models, list(mod))
    dlist <- append(dlist,d)
    slist <- append(slist,s)
  }
}


# Calcul of the Metrics  --------------------------------------------------------

xgb_metrics <- data.frame(algo = character(),
                          tree_depth = integer(),
                          n_trees = integer(),
                          shrinkage = numeric(),
                          accuracy = numeric(),
                          precision = numeric(),
                          sensitivity = numeric(),
                          score = numeric(),
                          training_accuracy = numeric(),
                          training_precision = numeric(),
                          training_sensitivity = numeric(),
                          training_score = numeric(),
                          stringsAsFactors = FALSE)


ConfusionMatrix <- function(prediction, truth) {
  conf <- matrix(c(table(prediction, truth),0,0),nrow=2)
  df <- data.frame(precision = conf[2,2]/sum(conf[,2]),
                   sensitivity = conf[2,2]/sum(conf[2,]),
                   accuracy = (conf[1,1] + conf[2,2]) / length(prediction))
  df$score <- ( 0.5*df$sensitivity + 
                  0.3*df$precision +
                  0.2*df$accuracy)
  return(list(confusion_matrix=conf, metrics=df))
}

for(i in 1:28) {
  mod = xgb_models[[i]]
  preds = predict(mod, dvalidation, type="response")
  validation_metrics = ConfusionMatrix(preds > 0.5, ytest)$metrics
  
  training_preds = predict(mod, dtrain, type="response")
  training_metrics = ConfusionMatrix(training_preds > 0.5, ytrain)$metrics
  
  xgb_metrics <- xgb_metrics %>% 
    add_row(algo = "Gradient Boosting",
            tree_depth = dlist[[i]],
            n_trees = mod$niter,
            shrinkage = slist[[i]],
            accuracy = validation_metrics$accuracy,
            precision = validation_metrics$precision,
            sensitivity = validation_metrics$sensitivity,
            score = validation_metrics$score,
            training_accuracy = training_metrics$accuracy,
            training_precision = training_metrics$precision,
            training_sensitivity = training_metrics$sensitivity,
            training_score = training_metrics$score)
}

xgb_metrics$accuracy  <-round(xgb_metrics$accuracy, 3)
xgb_metrics$precision <-round(xgb_metrics$precision, 3)
xgb_metrics$sensitivity <-round(xgb_metrics$sensitivity, 3)
xgb_metrics$score <-round(xgb_metrics$score, 3)




# Exporting tables --------------------------------------------------------

xgbgridtab1<-xgb_metrics[1:28,] %>% 
  select(tree_depth, n_trees, shrinkage, accuracy, precision, sensitivity, score) %>% 
  arrange(tree_depth, n_trees)

xgbgridtab_1 <- kbl(xgbgridtab1, 'latex', caption = "XGB Gridsearch", booktabs=T, 
                    linesep=c("","", "", "","", "", "", "\\hline")) %>% #LINESEP A CHANGER SI ON CHANGE LE GRID
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  column_spec(3, color = spec_color(xgbgridtab1$accuracy, end = 0.8, direction = -1))%>%
  column_spec(4, color = spec_color(xgbgridtab1$precision, end = 0.8, direction = -1))%>%
  column_spec(5, color = spec_color(xgbgridtab1$sensitivity, end = 0.8, direction = -1))%>%
  column_spec(6, color = spec_color(xgbgridtab1$score, end = 0.8, direction = -1))

save_kable(xgbgridtab_1,'xgbgridtab_1.tex')


# Making graphs -----------------------------------------------------------
library(ggplot2)
library(gridExtra)

xgb_accuracy <- ggplot(xgb_metrics, aes(x=tree_depth, y = accuracy)) + 
  geom_point(aes(colour = as.factor(shrinkage)),size = 3) + 
  scale_color_discrete("# of shrinkage") + 
  ggtitle("Accuracy in function of # of shrinkage and max depth")

xgb_precision <- ggplot(xgb_metrics, aes(x=tree_depth, y = precision)) + 
  geom_point(aes(colour = as.factor(shrinkage)),size = 3) + 
  scale_color_discrete("# of shrinkage") + 
  ggtitle("Precision in function of # of shrinkage and max depth")

xgb_sensitivity <- ggplot(xgb_metrics, aes(x=tree_depth, y = sensitivity)) + 
  geom_point(aes(colour = as.factor(shrinkage)),size = 3) + 
  scale_color_discrete("# of shrinkage") + 
  ggtitle("Sensitivity in function of # of shrinkage and max depth")

xgb_score <- ggplot(xgb_metrics, aes(x=tree_depth, y = score)) + 
  geom_point(aes(colour = as.factor(shrinkage)),size = 3) + 
  scale_color_discrete("# of shrinkage") + 
  ggtitle("Score in function of # of shrinkage and max depth")

grid.arrange(xgb_accuracy, xgb_precision,xgb_sensitivity,xgb_score, nrow = 2)


# Threshold selection -----------------------------------------------------
alphas <- (0:100)*0.01

XGBoost.threshold <-
  for(a in alphas){
    param <- list(
      objective = "binary:logistic",
      eta = 1e-02,
      max_depth = 3,
      colsamplebytree = 0.8,
      subsample = 0.8, 
      #min_child_weight = 3,
      base_score = 0.50,
      #lambda = 100,
      #lambda_bias = 5,
      alpha = a,
      verbose = FALSE
    )
    mod = xgboost::xgb.train(
      params = param,
      data = dtrain, 
      nrounds = 1000,
      watchlist = watchlist, 
      nthread = 10,
      maximize = FALSE,
      early_stopping_rounds = 100,
      verbose = FALSE)
    
    
    
    xgboost2.yhato =  predict(mod, dvalidation, type="response")
    xgboost2.yhat  <- rep(-1, length(xgboost2.yhato))
    xgboost2.yhat[xgboost2.yhato > a] <- 1
    
    confusion.test   = matrix(c(table(ytest, xgboost2.yhat),0,0),nrow=2)
    sensitivity.test = confusion.test[2,2]  / sum(confusion.test[2,])
    fpr.test         = confusion.test[1,2] / sum(confusion.test[1,])
    
    results2 = data.frame(
      algorithm = "XG-Boost",
      Threshold = a,
      sensitivity = sensitivity.test,
      FalsePositiveRate = fpr.test,
      stringsAsFactors = FALSE)
    list(model = mod, metrics = results2)
  }

xgboost_metrics2 <- 
  lapply(XGBoost.threshold, function(inside_item)
  { inside_item[['metrics']]})

xgboost_metrics2 <- as_tibble(bind_rows(lapply(xgboost_metrics2, bind_rows)))
xgboost.AUROC<-sum(xgboost_metrics2$sensitivity*0.001)

ROC_curve <- ggplot(xgboost_metrics2, aes(x = FalsePositiveRate, y=sensitivity )) + 
  geom_line() +
  ggtitle("ROC Curve",subtitle = "AUROC = 0.82")


# logit VS AdaBoost Vs XgBoost -------------------------------------------------------

## AdaBoost
ytrain2<-as.numeric(ytrain)
ytrain2[ytrain == 0] <- -1
adabst4 = JOUSBoost::adaboost(X = xtrain, y = ytrain2, tree_depth = 5, n_rounds =75)
adabst4.yhat = JOUSBoost::predict.adaboost(adabst4, xtest, type="response")

confusion.best   = matrix(c(table(ytest, adabst4.yhat),0,0),nrow=2)
accuracy.best    = (confusion.best[2,2] + confusion.best[1,1] ) / sum(confusion.best)
precision.best   = confusion.best[2,2]  / sum(confusion.best[,2])
sensitivity.best = confusion.best[2,2]  / sum(confusion.best[2,])
score.best <- ( 0.5*sensitivity.best + 
                  0.3*precision.best +
                  0.2*accuracy.best)

## Logit

df.logit<-data.frame(cbind(xtrain,ytrain2))
logit<- glm(ytrain2~., df.logit, family = "binomial" )
yhatlogito<- predict(logit, newdata= data.frame(xtest), "response")
yhatlogit <- rep(0, length(yhatlogito))
yhatlogit[yhatlogito>0.5]<-1

confusion.logit   = matrix(c(table(ytest, yhatlogit),0,0),nrow=2)
accuracy.logit    = (confusion.logit[2,2] + confusion.logit[1,1] ) / sum(confusion.logit)
precision.logit   = confusion.logit[2,2]  / sum(confusion.logit[,2])
sensitivity.logit = confusion.logit[2,2]  / sum(confusion.logit[2,])
score.logit <- (  0.5*sensitivity.logit + 
                    0.3*precision.logit   +
                    0.2*accuracy.logit)





## XG-Boost

param <- list(
  objective = "binary:logistic",
  eta = 1e-02,
  max_depth = 3,
  colsamplebytree = 0.8,
  subsample = 0.8, 
  #min_child_weight = 3,
  base_score = 0.50,
  #lambda = 100,
  #lambda_bias = 5,
  alpha = 0.5, ####################### A changer ##############################
  verbose = FALSE
)

mod = xgboost::xgb.train(
  params = param,
  data = dtrain, 
  nrounds = 1000,
  watchlist = watchlist, 
  nthread = 10,
  maximize = FALSE,
  early_stopping_rounds = 100,
  verbose = FALSE)
preds = predict(mod, dvalidation, type="response")
validation_metrics = ConfusionMatrix(preds > 0.5, ytest)$metrics



compar=data.frame( 
  algorithm   = c("Logit","AdaBoost","XG-Boost"),
  accuracy    = c(accuracy.logit, accuracy.best, validation_metrics$accuracy),
  precision   = c(precision.logit, precision.best, validation_metrics$precision),
  sensitivity = c(sensitivity.logit, sensitivity.best, validation_metrics$sensitivity),
  score       = c(score.logit, score.best, validation_metrics$score) )

comparkbl<-kbl(compar, 'latex', caption = "Logit vs Adaboost vs XG-Boost", booktabs=T)
save_kable(comparkbl,"compar.tex")










