#Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(caret)
library(labelled)
library(tictoc)
#require RANN, ranger, glmnet

#Data Import and Cleaning
gss_tbl_raw <- read_sav("../data/GSS2016.sav") #N = 2867, 961 vars

gss_tbl <- gss_tbl_raw %>% 
  unlabelled() %>% 
  rename(workhours = MOSTHRS) %>% 
  drop_na(workhours) %>% #remove NAs in max hours worked variable, N = 570
  select(which(colMeans(is.na(.)) < 0.75)) #drop cols with >75% missingness. 538 vars left


#Visualization

gss_tbl %>% 
  ggplot(aes(x=workhours)) + geom_histogram()



#Analysis

#running OLS reg aka lm, elastic net aka glmnet, random forest aka ranger or rf, extreme gradient boosting aka
#on caret website, there are 3 diff methods for eXtreme with diff numbers of hyperparameters
#xgbDART, xgbLinear, and xgbTree

ml_methods <- c("lm","glmnet","ranger","xgbLinear")

set.seed(24)

#use 75/25 split
cv_index <- createDataPartition(gss_tbl$workhours, p = 0.75, list = FALSE)
train_gss <- gss_tbl[cv_index,] #didnt do random shuffling of rows
test_gss <- gss_tbl[-cv_index,] #holdout

#for reproducibility and fair comparison across models, need same splits for training and holdout
#create own trainControl object
#first create train/test indexes

fold_indices <- createFolds(train_gss$workhours, k = 10)

myControl <- trainControl(
  method = "cv", 
  number = 10, 
  verboseIter = TRUE,
  search = "random",
  indexOut = fold_indices #why not index or indexFinal?
 )



#for reproducibility and fair comparison across models, need same splits for training and holdout
#create own trainControl object
#first create train/test indexes


#Be sure to get estimates of both 10-fold CV and holdout CV.??
tic()
model <- caret::train(
  workhours~.,
  data = train_gss, 
  metric = "Rsquared",
  method = "ranger", #with glmnet, default tests 3 alpha, 3 lambdas
  preProcess = c("center","scale","nzv","medianImpute"), #does order matter here? center and scale? use nzv instead?
  na.action = na.pass,
  trControl = myControl
  # tuneLength = 3, #default is len 3
   #tuneGrid = expand.grid(.mtry = c(150,81,1841)) #takes foreever..
  )
#from ranger documentation
#min node size, default 1 for class, 5 for regression, 3 for survival, 10 for prob
toc()

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



predicted <- predict(model, test_gss, na.action = na.pass,  metric = "Rsquared")

cor(predicted, test_gss$workhours)

#error with ranger: Error: mtry can not be larger than number of variables in data. Ranger will EXIT now.
#no error when search=random but richard said grid search


#warnings-  prediction from a rank-deficient fit may be misleading


#after running all models, save them as list
model_list <- list(
  glmnet = model_glmnet,
  rf = model_ranger
)

#pass list to resamples() then summarize




#Publication