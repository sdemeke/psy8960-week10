#Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(caret)


#Data Import and Cleaning

#The following series of pipes imports the SPSS data and removes the SPSS labels 
#such that all variables with numeric values are coerced to numeric and label-defined
#NA values are left as missing. Then, the MOSTHRS variable is renamed according to project 
#instructions and all missing values for this variable are dropped from the entire
#dataset. Finally, colMeans() calculates the missingness proportion for each 
#variable and all columns with more than 75% missingness are deselected.

gss_tbl <- read_sav("../data/GSS2016.sav") %>%  
  labelled::remove_labels() %>%  
  rename(workhours = MOSTHRS) %>% 
  drop_na(workhours) %>% 
  select(which(colMeans(is.na(.)) < 0.75))  


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
#variance variables, and impute the median values for missing data. The training
#settings further specify the cross-validation method, the number of folds, and
#the folds to use in resampling. The predict() generates predictions on the 
#holdout data. Finally, the results tibble saves the name of the ML method used,
#the cross-validated Rsquared, and the holdout Rsquared as the correlation between
#the generated predictions and the true DV values in the holdout data.

ml_function <- function(dat = gss_tbl, ml_model = "lm", no_folds = 10) { 
  
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

#The final series of pipes first collapses the list of dataframes into one,
#re-creates a model name variable to fit with assignment instructions, and formats
#the Rsquared values as specified to remove any leading zeros and round all values
#to 2 decimal places. I used gsub() instead of str_remove() because sometimes a
#negative Rsquared is possible and it was easier for me to specify removing only
#a leading zero with this function.

table1_tbl <- do.call("rbind", ml_results_list) %>% 
  mutate(algo = c("OLS Regression","Elastic Net","Random Forest", 
                  "eXtreme Gradient Boosting"),
         .before = cv_rsq)  %>% 
  select(-c(model_name)) %>% 
  mutate(across(ends_with("_rsq"),
                \(x) gsub("0\\.",".",format(round(x, digits=2), nsmall = 2)) ) )


# A tibble: 4 × 3
#  algo                      cv_rsq ho_rsq
# 1 OLS Regression            .12    .18   
# 2 Elastic Net               .83    .66   
# 3 Random Forest             .93    .66   
# 4 eXtreme Gradient Boosting .93    .68  


##Answers to Questions

##Question 1
#Both the k-fold CV and holdout R squared values improved dramatically
#from the OLS regression to the other tested models. The k-fold CV Rsquared
#was around .8-.9 for all other models and the holdout R squared values were not so
#different between them either (range .66-.68). The OLS model performed the worst.
#Even though the holdout Rsquared was greater than the training cross-validated
#value, the Rsquared was below .2 in both cases (poor accuracy). The other three
#models use algorithms that can lower variance and diminish overfitting compared
#to the  OLS model.

##Question 2
#The k-fold CV result was usually more accurate than the holdout CV across
#the tested models. This happened likely because the models overfitted on the
#set of training data and underperformed when model parameters were tried out
#on a new set of unseen and untrained data. In other words, the variance is higher
#and the models may have been biased towards the training data so accuracy for the
#holdout predictions was lower.

##Question 3
#In a real-life prediction model, I would choose to use the Elastic Net model.
#In terms of computational speed, this model was faster than the Random
#Forest and the Gradient Boosting models but resulted in nearly equivalent k-fold
#and holdout R squared values so the extra complexity in the Random Forest and
#Gradient Boosting did not necessarily yield greater accuracy, especially for the
#holdout Rsquared values which were equivalent between Elastic Net and the more 
#complex tree models. In other words, Elastic Net performed equivalently on the
#untested data. A potential tradeoff with the Elastic Net could be lack of ability
#to model non-linear relationships.
