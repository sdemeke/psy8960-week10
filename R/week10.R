#Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(caret)
library(labelled)
library(tictoc)
#set.seed(24)

#require RANN, ranger, glmnet

#Data Import and Cleaning
gss_tbl <- read_sav("../data/GSS2016.sav") %>%  #N = 2867, 961 vars
  unlabelled() %>% 
  rename(workhours = MOSTHRS) %>% 
  drop_na(workhours) %>% #remove NAs in max hours worked variable, N = 570
  select(which(colMeans(is.na(.)) < 0.75)) #drop cols with >75% missingness. 538 vars left



#Visualization

gss_tbl %>% 
  ggplot(aes(x=workhours)) + geom_histogram()



#Analysis
ml_function <- function(dat = gss_tbl, ml_model = "lm",no_folds = 3) { #default values
  
  set.seed(24)
  cv_index <- createDataPartition(dat$workhours, p = 0.75, list = FALSE)
  train_dat <- dat[cv_index,] 
  test_dat <- dat[-cv_index,] 
  
  fold_indices <- createFolds(train_dat$workhours, k = no_folds)
  
  
  model <- caret::train(
    workhours~.,
    data = train_dat, 
    metric = "Rsquared",
    method = ml_model,
    preProcess = c("center","scale","nzv","medianImpute"), 
    na.action = na.pass,
    trControl = trainControl(
      method = "cv", 
      number = no_folds, 
      verboseIter = TRUE,
      indexOut = fold_indices 
    )
  )
  
  #save output in df
  predicted <- predict(model, test_dat, na.action = na.pass)
  
  
  results <- tibble(
    model_name = ml_model,
    cv_rsq = max( model[["results"]][["Rsquared"]]),
    ho_rsq = cor(predicted, test_dat$workhours)
  )
  
  return(results)
  
}


ml_methods <- c("lm","glmnet","ranger","xgbLinear") #
 
my_list <- vector(mode="list")


for(i in 1:length(ml_methods)) {
  
 tic()
  
 my_list[[i]] <- ml_function(ml_model = ml_methods[i])
 
 toc()
  
}


#Publication

table1_tbl <- do.call("rbind", my_list) %>% 
  mutate(algo = c("OLS Regression","Elastic Net","Random Forest", "eXtreme Gradient Boosting"),
         .before = cv_rsq)  %>% 
  select(-c(model_name)) %>% 
  mutate(across(ends_with("_rsq"), \(x)   gsub("0\\.",".",format(round(x, digits=2), nsmall = 2)) ) )




#running OLS reg aka lm, elastic net aka glmnet, random forest aka ranger or rf, extreme gradient boosting aka
#on caret website, there are 3 diff methods for eXtreme with diff numbers of hyperparameters
#xgbDART, xgbLinear, and xgbTree



#use 75/25 split


#for reproducibility and fair comparison across models, need same splits for training and holdout
#create own trainControl object
#first create train/test indexes


#for reproducibility and fair comparison across models, need same splits for training and holdout
#create own trainControl object
#first create train/test indexes


##LM
#lm on grid search, time elapsed = 46 sec
# Resampling results:
#   
#   RMSE      Rsquared   MAE     
# 126.0067  0.1416766  32.37984
# 
# Tuning parameter 'intercept' was held constant at a value of TRUE

##GLMNET
#on grid search with 3 alpha, 3 lambdas = 124 secs
# alpha  lambda     RMSE       Rsquared   MAE     
# 0.10   0.8330873   4.777404  0.9171673  3.031020   #best model
# 0.10   2.6344535   5.830942  0.8886126  4.033140
# 0.10   8.3308735   8.609608  0.7777801  6.194248
# 0.55   0.8330873   6.895614  0.8400237  4.731411
# 0.55   2.6344535   9.353078  0.7084596  6.584342
# 0.55   8.3308735  11.160454  0.6837524  8.254906
# 1.00   0.8330873   8.305106  0.7651537  5.724268
# 1.00   2.6344535   9.896860  0.6824147  7.018449
# 1.00   8.3308735  13.005730  0.6752084  9.801693
# 
# Rsquared was used to select the optimal model using the largest value.
# The final values used for the model were alpha = 0.1 and lambda = 0.8330873.

#RANDOM FOREST
#when ranger on grid with no customization, time is 511 seconds
# mtry  splitrule   RMSE       Rsquared   MAE     
# 2  variance    11.301201  0.8707799  8.337993
# 2  extratrees  11.607862  0.8691268  8.586230
# 51  variance     6.445191  0.9145326  4.324541   #best model
# 51  extratrees   6.927534  0.9085432  4.740719
# 1320  variance     5.863813  0.8457853  3.200832
# 1320  extratrees   5.155011  0.8994593  3.204340
# 
# Tuning parameter 'min.node.size' was held constant at a value of 5


#when ranger on random search with no customization, time is 581 sec:
# min.node.size  mtry  splitrule  RMSE      Rsquared   MAE     
# 3              617  variance   4.474389  0.9340484  2.966425
# 6              988  maxstat    6.578666  0.8638582  4.239857
# 18             1141  variance   5.501925  0.8946563  3.693464
