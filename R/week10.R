#Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(caret)
library(labelled)


#Data Import and Cleaning

#The following series of pipes imports the SPSS data and removes the SPSS labels 
#such that all variables with label attributes for each value are converted to 
#factor while variables with labels for only missing values are coerced to numeric 
#(e.g., MOSTHRS). Then, the MOSTHRS variable is renamed according to project 
#instructions and all missing values for this variable are dropped from the entire
#dataset. Finally, colMeans() calculates the missingness proportion for each 
#variable and all columns with more than 75% missingness are deselected.

gss_tbl <- read_sav("../data/GSS2016.sav") %>%  #N = 2867, 961 vars
  unlabelled() %>% 
  rename(workhours = MOSTHRS) %>% 
  drop_na(workhours) %>% 
  select(which(colMeans(is.na(.)) < 0.75))  #drop cols with >75% missingness. 538 vars left


#Visualization

#The ggplot code below graphs a univariate distribution of the workhours variable as a frequency histogram.

gss_tbl %>% 
  ggplot(aes(x=workhours)) + geom_histogram()


#Analysis

#The custom ml_function() takes in a dataframe (default gss_tbl), an ML model 
#(default OLS), and a variable for the number of folds (default 10). The body of
#the function creates indices to enable splitting the given data into a 75/25 
#training and holdout split (random seed is fixed for reproducibility). Then, 
#createFolds() splits the training set into the specified number of folds. In the
#main training model function, the ML model method is specified from a function 
#parameter, the pre processing is set to standardize the data, remove nearly zero
#variance variables, and impute the median value for missing data. The training
#settings further specify the cross-validaion method, the number of folds, and
#the folds to use in resampling. The predict() generates predictions on the 
#holdout data. Finally, the results tibble saves the name of the ML method used,
#the cross-validated Rsquared, and the holdout Rsquared as the correlation between
#the generated predictions and the true DV values in the holdout data.

ml_function <- function(dat = gss_tbl, ml_model = "lm", no_folds = 10) { #default values
  
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


#The ml_methods vector saves the names of the models to be run for the project
#An empty list will save the results tibble created for each ML model run through
#the custom function and the for loop iterates through each method to run each
#respective model. The final list contains 4 data frame elements for the
#Rquared results for each model. I chose to execute the models this way because
#all of them can use the same caret::train() settings and for more fair comparison
#between the final results

ml_methods <- c("lm","glmnet","ranger","xgbTree") 
 
ml_results_list <- vector(mode="list")

for(i in 1:length(ml_methods)) {
  ml_results_list[[i]] <- ml_function(ml_model = ml_methods[i])
}


#Publication

#The final seriers of pipes first collapse the list of dataframes into one,
#re-creates a model name variable to fit with assignment instructions, and formats
#the Rsquared values as specified to remove any leading zeros and round all values
#to 2 decimal places. I used gsub() instead of str_remove() because sometimes a
#negative Rsquared is possible and it was easier for me to specify removing only
#a leading zero with this function.

table1_tbl <- do.call("rbind", ml_results_list) %>% 
  mutate(algo = c("OLS Regression","Elastic Net","Random Forest", "eXtreme Gradient Boosting"),
         .before = cv_rsq)  %>% 
  select(-c(model_name)) %>% 
  mutate(across(ends_with("_rsq"), \(x) gsub("0\\.",".",format(round(x, digits=2), nsmall = 2)) ) )

# > table1_tbl
# A tibble: 4 × 3
#   algo                      cv_rsq ho_rsq
#   <chr>                     <chr>  <chr> 
# 1 OLS Regression            .17    "-.02"
# 2 Elastic Net               .90    " .67"
# 3 Random Forest             .91    " .69"
# 4 eXtreme Gradient Boosting .92    " .64"


##Answers to Questions

##Question 1
#Both the k-fold CV and holdout R squared values improved dramatically
#from the OLS regression to the other tested models. The k-fold CV Rsquared
#was around .9 for all other three models and the holdout R squared was not much
#different between them either. The OLS model performed the worst with a nearly
#negative holdout R squared.

#How did your results change between models? Why do you think this happened, specifically?



##Question 2
#Consistently, the k-fold CV result was more accurate than the holdout CV across
#all tested models.This happened likely because the models overfitted on the
#set of training data and underperformed when model parameters were tried out
#on a new set of unseen and untrained data. In other words, the variance is higher
#and the models may have been biased towards the training data.

##Question 3
#In a real-life prediction model, I would choose to use the Elastic Net model.
#In terms of computational speed, this model was nearly 5x faster than the Random
#Forest and the Gradient Boosting models but resulted in nearly equivalent k-fold
#and holdout R squared values so the extra complexity in the Random Forest and
#Gradient Boosting did not necessarily yield greater accuracy. Further, the model
#has two generally interpretable hyperparameters that denote the level of penalty
#and the ratio between LASSO and Ridge regression. If two models results in 
#nearly equivalent predictions by accuracy but one is more interpretable, I would
#think that is an advantage. A potential tradeoff with the Elastic Net could be
#for nonlinear..??




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


# Warning messages:
#   1: In predict.lm(modelFit, newdata) :
#   prediction from a rank-deficient fit may be misleading
# 2: In predict.lm(modelFit, newdata) :
#   prediction from a rank-deficient fit may be misleading
# 3: In predict.lm(modelFit, newdata) :
#   prediction from a rank-deficient fit may be misleading
# 4: In predict.lm(modelFit, newdata) :
#   prediction from a rank-deficient fit may be misleading
# 5: model fit failed for Fold3: mtry=1320, min.node.size=5, splitrule=variance Error in ranger::ranger(dependent.variable.name = ".outcome", data = x,  : 
#                                                                                                         User interrupt or internal error.
#                                                                                                       
#                                                                                                       6: model fit failed for Fold3: mtry=1320, min.node.size=5, splitrule=extratrees Error in ranger::ranger(dependent.variable.name = ".outcome", data = x,  : 
#                                                                                                                                                                                                                 User interrupt or internal error.
#                                                                                                                                                                                                               
#                                                                                                                                                                                                               7: In nominalTrainWorkflow(x = x, y = y, wts = weights, info = trainInfo,  :
#                                                                                                                                                                                                                                            There were missing values in resampled performance measures.




