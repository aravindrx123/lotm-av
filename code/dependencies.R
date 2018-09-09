## Dependencies
packages<-c("data.table", "RJDBC", "ggplot2", "ebmc", "dplyr", "xgboost","glmnet", 
            "colorspace", "TraMineR","ade4","caret","xlsx","dplyr","stringr",
            "rpart","rattle","rBayesianOptimization")

# Check installed packages
ix<-packages%in%installed.packages()[,1]
if(sum(!ix)>0){
  sapply(packages[!ix], FUN=function(x) install.packages(x))  
}

# Load packages
sapply(packages, FUN=function(x) require(x, character.only = TRUE))

rm(ix,packages)