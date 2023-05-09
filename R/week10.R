#Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
#library(foreign)
library(caret)
#install.packages("RANN")
library(RANN)

#Data Import and Cleaning
#gss_tbl1 <- read.spss("../data/GSS2016.sav", to.data.frame = TRUE)
gss_tbl_raw <- read_sav("../data/GSS2016.sav") #N = 2867, 961 vars
#works better than foreign::read.spss but creates weird class structure

gss_tbl <- gss_tbl_raw %>% 
  rename(workhours = MOSTHRS) %>% 
  drop_na(workhours) %>% #remove NAs in max hours worked variable, N = 570
  select(which(colMeans(is.na(.)) < 0.75)) %>% #drop cols with >75% missingness. 538 vars left
  mutate(across(everything(), as.numeric))
  
missing_cols <- summarise(gss_tbl,across(everything(),
                                         ~ (sum(is.na(.x))/nrow(gss_tbl))<=0.75 )) 
# > rowSums(missing_cols)
# [1] 538

#colMeans is simpler
#colMeans(is.na(gss_tbl))

#MOSTHRS = MOST HRS/WEEK WORKED IN PAST MONTH



#Visualization

gss_tbl %>% 
  ggplot(aes(x=workhours)) +
  geom_bar()



#Analysis

#running OLS reg aka lm, elastic net aka glmnet, random forest aka ranger or rf, extreme gradient boosting aka
#on caret website, there are 3 diff methods for eXtreme with diff numbers of hyperparameters
#xgbDART, xgbLinear, and xgbTree

ml_methods <- c("lm","glmnet","ranger","xgb")


#run into error with original str
#Error in na.fail.default(list(workhours = c(15, 30, 50, 46, 50, 50, 50,  : 
#missing values in object

#use 75/25 split
split <- round(nrow(gss_tbl) * 0.75)
train_gss <- gss_tbl[1:split,] #didnt do random shuffling of rows
test_gss <- gss_tbl[(split+1):nrow(gss_tbl),]

#Be sure to get estimates of both 10-fold CV and holdout CV.??

model <- caret::train(
  workhours~.,
  data = train_gss, 
  method = "lm",
  preProcess = c("zv","medianImpute"), #does order matter here?
  na.action = na.pass,
  trControl = trainControl(
    method = "cv", 
    number = 10, 
    verboseIter = TRUE,
    search = "grid"
  )
)

#need to automate searching through plausible range of hyperparameters
#adaptive sampling?


#warnings about zero variance variables
#go away if i add zv to preprocess
#more errors= package RANN is required
#more errors=  Cannot find more nearest neighbours than there are points
#solve above by switching to boosted imputation, bagImpute?
#richard explicitly said medianImpute
#warnings-  prediction from a rank-deficient fit may be misleading

#Publication