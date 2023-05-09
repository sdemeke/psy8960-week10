#Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
#library(foreign)

#Data Import and Cleaning
#gss_tbl1 <- read.spss("../data/GSS2016.sav", to.data.frame = TRUE)
gss_tbl_raw <- read_sav("../data/GSS2016.sav") #N = 2867, 961 vars
#works better than foreign::read.spss but creates weird class structure

gss_tbl <- gss_tbl_raw %>% 
  rename(workhours = MOSTHRS) %>% 
  drop_na(workhours) %>% #remove NAs in max hours worked variable, N = 570
  select(which(colMeans(is.na(.)) < 0.75)) #drop cols with >75% missingness. 538 vars left

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











#Publication