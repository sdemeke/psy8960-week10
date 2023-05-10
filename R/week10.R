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
#gss_tbl1 <- read.spss("../data/GSS2016.sav", to.data.frame = TRUE)
gss_tbl_raw <- read_sav("../data/GSS2016.sav") #N = 2867, 961 vars
#works better than foreign::read.spss but creates weird class structure

#checking all inappropriate responses labeled as NA
#haven_labelled gives details on the labels that should be NA
#ex look_for(gss_tbl_raw, "MOSTHRS")
# val_labels(gss_tbl_raw$MOSTHRS) - returns same #of missing so already taken care of
#To remove value labels, use remove_val_labels().
#for analyses, need to convert to categorical or continuous
#unlabelled() will convert to factor if all values have value label and continuance otherwise into numeric or character

# gss_tbl_raw %>% 
#   look_for("ABSINGLE") #ran table(absingle), all IAP, DK, NA already coverted to NA
#   
# unlabelled(gss_tbl_raw) %>% 
#   look_for("ABSINGLE") #coltype reverts to just dbl and no labels but same #missing
# #vars like ABSINGLE converted to factor. MOSTHRS converted to dbl
# #does this matter?

gss_tbl <- gss_tbl_raw %>% 
  unlabelled() %>% 
  rename(workhours = MOSTHRS) %>% 
  drop_na(workhours) %>% #remove NAs in max hours worked variable, N = 570
  select(which(colMeans(is.na(.)) < 0.75)) %>% #drop cols with >75% missingness. 538 vars left

#mutate(across(everything(), as.numeric))

#Visualization

gss_tbl %>% 
  ggplot(aes(x=workhours)) + geom_histogram()



#Analysis

#running OLS reg aka lm, elastic net aka glmnet, random forest aka ranger or rf, extreme gradient boosting aka
#on caret website, there are 3 diff methods for eXtreme with diff numbers of hyperparameters
#xgbDART, xgbLinear, and xgbTree

ml_methods <- c("lm","glmnet","ranger","xgbLinear")


#use 75/25 split
split <- round(nrow(gss_tbl) * 0.75)
train_gss <- gss_tbl[1:split,] #didnt do random shuffling of rows
test_gss <- gss_tbl[(split+1):nrow(gss_tbl),] #holdout

#for reproducibility and fair comparison across models, need same splits for training and holdout
#create own trainControl object
#first create train/test indexes
set.seed(24)
fold_indices <- createFolds(train_gss$workhours, k = 10)

myControl <- trainControl(
  method = "cv", 
  number = 10, 
  verboseIter = TRUE,
  search = "grid",
  indexOut = fold_indices  #why not indexOut or indexFinal?
)

tune_grid <- expand.grid(.mtry = )


#for reproducibility and fair comparison across models, need same splits for training and holdout
#create own trainControl object
#first create train/test indexes
set.seed(24)
myFolds <- createFolds(train_gss$workhours, k = 10)

myControl <- trainControl(
  method = "cv", 
  number = 10, 
  verboseIter = TRUE,
  search = "grid",
  index = myFolds  #why not indexOut or indexFinal?
)



#Be sure to get estimates of both 10-fold CV and holdout CV.??

model <- caret::train(
  workhours~.,
  data = train_gss, 
  method = "ranger", #with glmnet, default tests 3 alpha, 2 lambdas
  preProcess = c("zv","medianImpute"), #does order matter here? center and scale?
  na.action = na.pass,
  trControl = trainControl(
    method = "cv", 
    number = 10, 
    verboseIter = TRUE,
    search = "grid" #grid not appropriate for ranger
  )
)
predict(model, test_gss, na.action = na.pass)

#error with ranger: Error: mtry can not be larger than number of variables in data. Ranger will EXIT now.
#no error when search=random but richard said grid search

#need to automate searching through plausible range of hyperparameters
#adaptive sampling?


#warnings about zero variance variables
#go away if i add zv to preprocess
#warnings-  prediction from a rank-deficient fit may be misleading


#after running all models, save them as list
model_list <- list(
  glmnet = model_glmnet,
  rf = model_ranger
)

#pass list to resamples() then summarize




#Publication