#Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
#library(foreign)
library(caret)
#install.packages("RANN")
#library(RANN)
#install.packages("labelled")
library(labelled)
#require RANN, ranger, glmnet

#Data Import and Cleaning
gss_tbl_raw <- read_sav("../data/GSS2016.sav") #N = 2867, 961 vars

gss_tbl <- gss_tbl_raw %>% 
  unlabelled() %>% 
  rename(workhours = MOSTHRS) %>% 
  drop_na(workhours) %>% #remove NAs in max hours worked variable, N = 570
  select(which(colMeans(is.na(.)) < 0.75)) %>% #drop cols with >75% missingness. 538 vars left


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
  search = "grid",
  indexOut = fold_indices, #why not indexOut or indexFinal?
  tuneLength = 3 #default is len 3
)



#for reproducibility and fair comparison across models, need same splits for training and holdout
#create own trainControl object
#first create train/test indexes


#Be sure to get estimates of both 10-fold CV and holdout CV.??

model <- caret::train(
  workhours~.,
  data = train_gss, 
  metric = "Rsquared",
  method = "glmnet", #with glmnet, default tests 3 alpha, 3 lambdas
  preProcess = c("center","scale","zv","medianImpute"), #does order matter here? center and scale?
  na.action = na.pass,
  trControl = myControl
)



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