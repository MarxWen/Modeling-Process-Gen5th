# list.of.packages <- c( "readr", "dplyr", "vtreat", "caret", "Matrix", "rBayesianOptimization",
#                        "Ckmeans.1d.dp", "ROCR", "stringi", "xgboost")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)

library(readr)        # Fast read csv file.
library(dplyr)        # Wrangling: count()
library(vtreat)       # One-Hot Encoding
library(caret)        # Delete near zero variables with nearZeroVar(), highly correlated variables with findCorrelation () and train model with train().
library(Matrix)       # Construct a Matrix of a class that inherits from Matrix.
library(xgboost)      # Build extreme gradiant boosting with xgboost(). 
library(rBayesianOptimization) # R implementation of bayesian global optimization with gaussian processes
library(Ckmeans.1d.dp)# Required for xgb.plot.importance().
library(ROCR)         # Draw ROC Curve.
library(stringi)      # String concat operator %s+%.

wd<-getwd()
setwd(wd)
set.seed(123)
waterfall_stage = 'waterfall_1'

#' Load Data 
#' Bureau Data
Clean_Data <- get(load("../../../Data/For_Modeling/US1807_Clean_Raw_Date.rda"))

# Clean_Data[Clean_Data$c01_months_at_address=='Blank_NULL',"c01_months_at_address"] <- -0.01
# Clean_Data[Clean_Data$c01_months_at_address=='Blank_NA',"c01_months_at_address"] <- -0.02
# Clean_Data$c01_months_at_address <- as.numeric(Clean_Data$c01_months_at_address)
# 
# Clean_Data$c01_bank_routing_number <- as.character(Clean_Data$c01_bank_routing_number)
# Clean_Data$c01_bank_routing_number <- sapply(Clean_Data$c01_bank_routing_number, 
#                                        function(x) {
#                                          if (nchar(x)==7) paste("00", x, sep = "")
#                                          else if (nchar(x)==8) paste("0", x, sep = "")
#                                          else if (x=="-0.01") "Blank_NULL"
#                                          else if (x=="-0.02") "Blank_NA"
#                                          else x
#                                        })
# 
# Clean_Data[Clean_Data$d02_LastChargeOffDate=='0000-00-00', 'd02_LastChargeOffDate'] <- "Blank_NULL"

#' Census Data
# fname_census <- file.path(wd, "../../../Data/Processed/census_data_final_180322.csv")
# census <- read_csv(file=fname_census, na=c("",-666666666))
# census <- as.data.frame(census)
# census[is.na(census)] <- -0.01
# save(census, file='../../../Data/For_Modeling/census_data_final_180322.rda')
load("../../../Data/For_Modeling/census_data_final_180322.rda")

#' Targets
# fname_perf <- file.path(wd, "../../../Data/Processed/dependent_variables_C_171010_180430.csv")
# perf <- read_csv(file=fname_perf, na=c("NULL"))
# perf <- as.data.frame(perf)
# save(perf, file='../../../Data/For_Modeling/Data_Dependent_20171010_20180430_C.rda')
load("../../../Data/For_Modeling/Data_Dependent_20171010_20180430_C.rda")

#' Merge Data
data <- merge(census, Clean_Data, by=c("c01_zip_code"))

#' Select Target (Dependent Variable)
data <- merge(perf[!is.na(perf$FPD), c("FPD", "MRID", "Sys_reqAmt")], 
              data, by=c("MRID"))
remove(list=c("Clean_Data", "data_res", "census", "perf"))


#' Specify the Data Types
data_dict  <- read_csv(file="../../../Data/For_Modeling/Variable_Attributes_20180709_C.csv")  
var_special <- data_dict[(data_dict$Not_Useful_yc==0) 
                         & (data_dict$special_encoding==1)
                         & (data_dict[,waterfall_stage]==1),]$variable_names
vars <- data_dict[(data_dict$Not_Useful_yc==0) 
                  & (data_dict$special_encoding==0) 
                  & (data_dict[,waterfall_stage]==1)
                  & (!is.na(data_dict$dtype)),]
table(vars$dtype)
var_num <- vars[vars$dtype %in% c('continuous', 'ordinal_categorical'),]$variable_names   
var_datetime <- vars[vars$dtype == 'datetime',]$variable_names    
var_cate <- vars[vars$dtype == 'nominal_categorical',]$variable_names    
var_chr <- vars[vars$dtype == 'character',]$variable_names    # categorical varibales with too many levels, sensitive personal data
#dim(vars)[1] - length(var_num) - length(var_datetime) - length(var_cate) - length(var_chr)

var_special <- union(c("Sys_reqAmt"), var_special)

remove(list=c("data_dict", "vars"))

#' Select the sample window
# data <- data[(data$sys_ApplicationDate>='2017-10-01') & (data$sys_ApplicationDate<'2018-01-26'),]

#' Random Train Test Split
data <- data[order(runif(nrow(data))), ]
train_ix <- sample(nrow(data), 0.70 * nrow(data))   # row index of training data
#train_ix <- sample(nrow(data), 0.9999 * nrow(data))   # row index of training data
test_ix <- setdiff(seq_len(nrow(data)), train_ix)      # row index of test data
train <- data[train_ix,]
test <- data[test_ix,]

cat("train",
    "\n # loans:", dim(train)[1],
    "\n % FPD:", dim(train[train$FPD==1,])[1] / dim(train)[1],
    "\ntest",
    "\n # loans:", dim(test)[1],
    "\n % FPD:", dim(test[test$FPD==1,])[1] / dim(test)[1]
)

remove(list=c("train_ix","test_ix","data"))

#' Feature Engineering 
#' Special Variables
kit_special_var <- list(
  sys_ApplicationDate_hour_der = list(raw_var_name = 'sys_ApplicationDate', obj = NULL),
  sys_ApplicationDate_day_der = list(raw_var_name = 'sys_ApplicationDate', obj = NULL),
  c01_fed_routing_symbol_primary_der = list(raw_var_name = 'c01_bank_routing_number', obj = NULL),
  c01_fed_routing_symbol_thrift_der = list(raw_var_name = 'c01_bank_routing_number', obj = NULL),
  c01_fed_routing_symbol_electronic_der = list(raw_var_name = 'c01_bank_routing_number', obj = NULL),
  c01_ABA_identifier_freq_der = list(raw_var_name = 'c01_bank_routing_number', obj = NULL),
  c01_loan_to_income_der = list(raw_var_name = c('Sys_reqAmt', 'c01_net_monthly_income'), obj = NULL)
  )

#' sys_ApplicationDate: extract time of the day and day of the month
sys_ApplicationDate_hour <- function(data, obj) {
  data$sys_ApplicationDate <- as.POSIXct(data$sys_ApplicationDate) 
  data$sys_ApplicationDate_hour_der <- as.numeric(format(data$sys_ApplicationDate, "%H"))
  return(data)
}
sys_ApplicationDate_day <- function(data, obj) {
  data$sys_ApplicationDate <- as.Date(data$sys_ApplicationDate)
  data$sys_ApplicationDate_day_der <- as.numeric(format(data$sys_ApplicationDate, "%d"))
  return(data)
}
kit_special_var[['sys_ApplicationDate_hour_der']]$fun <- sys_ApplicationDate_hour
kit_special_var[['sys_ApplicationDate_day_der']]$fun <- sys_ApplicationDate_day
remove(list = c('sys_ApplicationDate_hour', 'sys_ApplicationDate_day'))

#' c01_bank_routing_number: extract Federal Reserve Routing Symbol
fed_routing_symbol_primary <- function(data, obj){
  bank_routing_number_XX <- as.numeric(substr(data$c01_bank_routing_number,1,2))
  data$c01_fed_routing_symbol_primary_der <- sapply(bank_routing_number_XX, function(x) ifelse(!is.na(x), ifelse(x>=1 & x<=12, 1, 0), 0))
  return(data)
}
fed_routing_symbol_thrift <- function(data, obj){
  bank_routing_number_XX <- as.numeric(substr(data$c01_bank_routing_number,1,2))
  data$c01_fed_routing_symbol_thrift_der <- sapply(bank_routing_number_XX, function(x) ifelse(!is.na(x), ifelse(x>=21 & x<=32, 1, 0), 0))
  return(data)
}
fed_routing_symbol_electronic <- function(data, obj){
  bank_routing_number_XX <- as.numeric(substr(data$c01_bank_routing_number,1,2))
  data$c01_fed_routing_symbol_electronic_der <- sapply(bank_routing_number_XX, function(x) ifelse(!is.na(x), ifelse(x>=61 & x<=72, 1, 0), 0))
  return(data)
}
kit_special_var[['c01_fed_routing_symbol_primary_der']]$fun <- fed_routing_symbol_primary
kit_special_var[['c01_fed_routing_symbol_thrift_der']]$fun <- fed_routing_symbol_thrift
kit_special_var[['c01_fed_routing_symbol_electronic_der']]$fun <- fed_routing_symbol_electronic
remove(list = c('fed_routing_symbol_primary', 'fed_routing_symbol_thrift', 'fed_routing_symbol_electronic'))

#' c01_bank_routing_number: extract ABA Institution Identifier and calulated its frequency
train$c01_ABA_identifier <- substr(train$c01_bank_routing_number,5,8)
ABA_identifier_freq_training <- plyr::count(train,'c01_ABA_identifier')
colnames(ABA_identifier_freq_training) <- c("c01_ABA_identifier", "c01_ABA_identifier_freq_der")
kit_special_var[['c01_ABA_identifier_freq_der']]$obj <- ABA_identifier_freq_training
train$c01_ABA_identifier <- NULL

ABA_identifier_freq <- function(data, ABA_identifier_freq_training){
  data$c01_ABA_identifier <- substr(data$c01_bank_routing_number,5,8)
  data <- merge(data, ABA_identifier_freq_training, by='c01_ABA_identifier', all.x=TRUE)
  data[is.na(data$c01_ABA_identifier_freq_der), "c01_ABA_identifier_freq_der"] <- 0
  data$c01_ABA_identifier <- NULL
  return(data)
}
kit_special_var[['c01_ABA_identifier_freq_der']]$fun <- ABA_identifier_freq
remove(list = c('ABA_identifier_freq_training', 'ABA_identifier_freq'))

#' Sys_reqAmt / c01_net_monthly_income = c01_loan_to_income_der
loan_to_income <- function(data, obj){
  data$c01_loan_to_income_der <- data$Sys_reqAmt / data$c01_net_monthly_income
  data[is.na(data$c01_loan_to_income_der), 'c01_loan_to_income_der'] <- -0.01
  return(data)
}
kit_special_var[['c01_loan_to_income_der']]$fun <- loan_to_income
remove(loan_to_income)

#' feature engineering steps exclusively for waterfall 3
if (waterfall_stage == 'waterfall_3') {
  #' d02_TimeSinceLastInquiry_days & d02_TimeSinceLastInquiry_years
  kit_special_var[['d02_time_since_last_inquiry']] <- list(raw_var_name=c('d02_TimeSinceLastInquiry_days', 'd02_TimeSinceLastInquiry_years'), obj=NULL) 
  time_since_last_inquiry <- function(data, obj){
    convert_to_days <- function(years, days) return(365*years+days)
    data$d02_time_since_last_inquiry <- mapply(convert_to_days, 
                                               as.numeric(train$d02_TimeSinceLastInquiry_years), 
                                               as.numeric(train$d02_TimeSinceLastInquiry_days))
    return(data)
  }
  kit_special_var[['d02_time_since_last_inquiry']]$fun <- time_since_last_inquiry
  remove(time_since_last_inquiry)
  
  #' c06_clear_bureau_lite_reason_codes
  reason_code_raw <- 'c06_clear_bureau_lite_reason_codes'
  
  temp <- sapply(train[,reason_code_raw], function(x) unlist(strsplit(x,","))[1])
  level_names <- setdiff(unique(temp), "NULL")    # list of unique reason codes in training set
  
  dummy_reason_code <- function(data, obj){
    # reason_code_der, reason_code_raw, code = obj 
    data[,obj[1]] <- sapply(data[,obj[2]], function(x) ifelse(grepl(obj[3], x), 1, 0))
    return(data)
  }
  
  for (code in level_names){
    reason_code_der <- paste(reason_code_raw, code, sep="_")
    kit_special_var[[reason_code_der]] <- list(raw_var_name=reason_code_raw, obj=c(reason_code_der, reason_code_raw, code))
    kit_special_var[[reason_code_der]]$fun <- dummy_reason_code
  }
  
  #' c06_score_x_reason_codes
  reason_code_raw <- 'c06_score_x_reason_codes'
  most_common_codes <- function(data, reason_code_raw, thres){
    combined_col <- vector()
    var_names <- vector()
    for (i in seq(4)){
      var_names[i] <- paste(reason_code_raw, i, sep="_")
      train[,var_names[i]] <- sapply(data[, reason_code_raw], function(x) unlist(strsplit(x,","))[i])
      combined_col <- c(combined_col, train[, var_names[i]])
    }
    combined_col <- combined_col[!is.na(combined_col) & (combined_col!="NULL")]
    
    value_counts <- plyr::count(combined_col)
    value_counts <- value_counts[order(-value_counts$freq),]
    num_of_codes <- sum(cumsum(value_counts$freq)/length(combined_col) < thres) + 1
    level_names <- as.character(value_counts[1:num_of_codes, "x"])
    #cat("select top", num_of_codes, "most common codes")
    return(level_names)
  }
  
  level_names <- most_common_codes(train, reason_code_raw, thres=0.8)
  
  for (code in level_names){
    reason_code_der <- paste(reason_code_raw, code, sep="_")
    kit_special_var[[reason_code_der]] <- list(raw_var_name=reason_code_raw, obj=c(reason_code_der, reason_code_raw, code))
    kit_special_var[[reason_code_der]]$fun <- dummy_reason_code
  }
  
  #' c06_vantage_reason_codes
  reason_code_raw <- 'c06_vantage_reason_codes'
  
  level_names <- most_common_codes(train, reason_code_raw, thres=0.8)
  
  for (code in level_names){
    reason_code_der <- paste(reason_code_raw, code, sep="_")
    kit_special_var[[reason_code_der]] <- list(raw_var_name=reason_code_raw, obj=c(reason_code_der, reason_code_raw, code))
    kit_special_var[[reason_code_der]]$fun <- dummy_reason_code
  }
  
  remove(list = c('temp', 'level_names', 'code', 'reason_code_der', 'reason_code_raw', 'most_common_codes', 'dummy_reason_code'))
  
}

#' Categorical Variables with Too Many Levels
#' Create indicators of missing values 
kit_chr_var <- list(raw_var_name = var_chr, obj = NULL)
binarize_chr_var <- function(data, var_chr, obj){
  for (raw_name in var_chr) {
    new_name <- paste(raw_name, "der", sep = "_")
    data[,new_name] <- sapply(data[,raw_name], function(x) {if (x %in% c("NONE", -0.01)) 1 else 0}) 
    cat("\n\t", raw_name)
  }
  return(data)
}
kit_chr_var$fun <- binarize_chr_var
remove(binarize_chr_var)

#' Datetime Variables
#' Replace the timestamps by the time intervals between the timestamps and sys_ApplicationDate
kit_datetime_var <- list(raw_var_name = var_datetime, obj = NULL)
transform_datetime_var <- function(data, var_datetime, obj) {
  calculate_time_diff <- function (date_str, sys_ApplicationDate) {
    if (date_str == 'NONE') date_diff <- -0.01
    else {
      date <- as.Date(date_str, format = "%Y-%m-%d")
      date_diff <- as.numeric(difftime(sys_ApplicationDate, date, units="day"), units='days')
    }
    return(date_diff)
  }
  calculate_time_diff_next_payday <- function (date_str, sys_ApplicationDate) {
    if (date_str == 'NONE') date_diff <- -0.01
    else {
      date <- as.Date(date_str, format = "%Y-%m-%d")
      date_diff <- as.numeric(difftime(date, sys_ApplicationDate, units="day"), units='days')
    }
    return(date_diff)
  }
  data$sys_ApplicationDate <- as.Date(data$sys_ApplicationDate)
  for (raw_name in var_datetime) {
    new_name <- paste(raw_name, "der", sep="_")
    if (raw_name == 'c01_date_of_next_payday') data[,new_name] <- mapply(calculate_time_diff_next_payday, data[,raw_name], data$sys_ApplicationDate) 
    else data[,new_name] <- mapply(calculate_time_diff, data[,raw_name], data$sys_ApplicationDate) 
    cat("\n\t", raw_name)
  }
  return(data)
}
kit_datetime_var$fun <- transform_datetime_var
remove(transform_datetime_var)

#' Categorical Variables
#' encode categorical variables with at least 2 levels (remove the variables with only one value)
bool_ix_2 <- sapply(train[var_cate], function(x) length(levels(as.factor(x))))>=2
var_cate <- var_cate[bool_ix_2]
kit_cate_var <- list(raw_var_name = var_cate,
                     obj = vtreat::designTreatmentsZ(train, var_cate, minFraction=0, verbose=FALSE))
encode_cate_var <- function(data, var_cate, obj) {
  tplan <- obj
  sf <- tplan$scoreFrame
  newvars <- sf$varName[sf$code %in% c("lev")]         #, "clean", "isBAD")] 
  dataVtreat <- vtreat::prepare(tplan, data, varRestriction = newvars)
  data <- cbind(data[,!(names(data) %in% var_cate)], dataVtreat)
  return(data)
}
kit_cate_var$fun <- encode_cate_var
remove(list = c('bool_ix_2', 'encode_cate_var'))

#' Wrap all the feature engineering steps in kit_xgb
kit_xgb <- list(kit_special_var = kit_special_var, 
                kit_chr_var = kit_chr_var, 
                kit_datetime_var = kit_datetime_var, 
                kit_cate_var = kit_cate_var) 
remove(list = c("kit_special_var", "kit_chr_var", "kit_datetime_var", "kit_cate_var"))

#' Execute the feature engineering on training set
raw_var_selected <- c("FPD", var_num, var_chr, var_datetime, var_cate, var_special)
raw_var_selected <- raw_var_selected[!is.na(raw_var_selected)]
train <- train[,raw_var_selected]
train_ <- train  # training dataset with raw variables

#' Debug kit_xgb
# for (j in 1:2) {
#    cat("\n\t",j," ",names(kit_xgb[[1]])[[j]])
#    train <- kit_xgb[[1]][[j]]$fun(train, kit_xgb[[1]][[j]]$obj)
#  }
# train <- kit_xgb[[2]]$fun(train, intersect(kit_xgb[[2]]$raw_var_name, raw_var_selected), kit_xgb[[2]]$obj)
# train <- kit_xgb[[3]]$fun(train, intersect(kit_xgb[[3]]$raw_var_name, raw_var_selected), kit_xgb[[3]]$obj)
# train <- kit_xgb[[4]]$fun(train, intersect(kit_xgb[[4]]$raw_var_name, raw_var_selected), kit_xgb[[4]]$obj)

exe_feature_engineering <- function(data, kit_xgb, raw_var_selected) {
  # variables need special encoding
  for (j in 1:length(kit_xgb[[1]])) {
    if (all(kit_xgb[[1]][[j]]$raw_var_name %in% raw_var_selected)) {
      cat("\n\t",j," ",names(kit_xgb[[1]])[[j]])
      data <- kit_xgb[[1]][[j]]$fun(data, kit_xgb[[1]][[j]]$obj)
    }
  }
  
  # other variables
  for (i in 2:length(kit_xgb)) {
    cat("\n", i, " ", names(kit_xgb)[i])
    data <- kit_xgb[[i]]$fun(data, intersect(kit_xgb[[i]]$raw_var_name, raw_var_selected), kit_xgb[[i]]$obj)
  }
  
  return(data)
}

save(exe_feature_engineering, file = "../../../Data/For_Excecution/exe_feature_engineering.rda")
train <- exe_feature_engineering(train, kit_xgb, raw_var_selected)
train <- train[,!(names(train) %in% c(var_chr, var_datetime, var_cate, var_special))]
remove(list = c('var_num', 'var_chr', 'var_datetime', 'var_cate', 'var_special', 'raw_var_selected'))

#' Feature Selection: further remove duplicated, near zero variance and highly correlated variables
dup_vars <- colnames(train)[endsWith(colnames(train), "lev_x.true") | endsWith(colnames(train), "lev_x.N")]
train[,dup_vars] <- NULL
ZeroVar_vars <- nearZeroVar(train[,names(train)!='FPD'], freqCut=(dim(train)[1]-50)/50, names = TRUE)
train[,ZeroVar_vars] <- NULL
corr_vars <- findCorrelation(cor(train[,names(train)!='FPD']), cutoff = 0.95, names = TRUE)
train[,corr_vars] <- NULL
remove(list=c("dup_vars", "ZeroVar_vars","corr_vars"))

#' Feature Selection with XGBoost
#' Initialization
der_var_selected <- names(train)[names(train) != 'FPD']
num_of_vars_old <- length(der_var_selected)
num_of_vars_new <- 0
i <- 1

while((num_of_vars_old > 50) & (num_of_vars_new < num_of_vars_old)) {
  if (i >= 2) {
  der_var_selected <- imp[imp$Gain>0]$Feature
  num_of_vars_old <- length(der_var_selected)
  }
  cat("\n Round = ", i)
  cat("\t Number of Features = ", num_of_vars_old)
  
  #' convert all input features to numeric format and stored in a sparse matrix
  dtrain<-list()
  mat_train <- as.matrix(sapply(train[, der_var_selected], as.numeric))
  dtrain$data <- Matrix(mat_train, sparse=TRUE)
  dtrain$label <- as.numeric(train$FPD)
  dm_train <- xgb.DMatrix(dtrain$data, label = dtrain$label)
  
  #' Parameter Tuning
  cv_folds <- KFold(dtrain$label, nfolds = 5, stratified = TRUE, seed = 1)
  xgb_cv_bayes <- function(max.depth, min_child_weight, subsample, colsample_bytree, gamma) {
    cv <- xgb.cv(params = list(booster = "gbtree", 
                               eta = 0.01,
                               max_depth = max.depth,
                               min_child_weight = min_child_weight,
                               subsample = subsample, 
                               colsample_bytree = colsample_bytree,
                               gamma=gamma, 
                               lambda = 1, 
                               alpha = 0,
                               scale_pos_weight = (length(dtrain$label) / sum(dtrain$label) - 1),
                               objective = "binary:logistic",
                               eval_metric = "auc"),
                 data = dm_train, nround = 1000,
                 folds = cv_folds, prediction = TRUE, showsd = TRUE,
                 early_stopping_rounds = 5, maximize = TRUE, verbose = 0)
    
    list(Score = max(cv$evaluation_log[,test_auc_mean]),
         Pred = cv$pred)
  }
  
  OPT_Res <- BayesianOptimization(xgb_cv_bayes,
                                  bounds = list(max.depth = c(2L, 6L),
                                                min_child_weight = c(5L, 10L),
                                                subsample = c(0.6, 0.9),
                                                colsample_bytree = c(0.6, 1.0),
                                                gamma = c(0, 1)),
                                  init_grid_dt = NULL, init_points = 10, n_iter = 20,
                                  acq = "ucb", kappa = 2.576, eps = 0.0,
                                  verbose = FALSE)
  
  opt_para <- as.numeric(OPT_Res$Best_Par)
  opt_max_depth <- opt_para[1]
  opt_min_child_weight <- opt_para[2]
  opt_subsample <- opt_para[3]
  opt_colsample_bytree <- opt_para[4]
  opt_gamma <- opt_para[5]
  
  #' fit on entire training set
  model_xgb <- xgboost(data=dtrain$data, 
                       label=dtrain$label,
                       booster="gbtree",
                       max_depth=opt_max_depth, 
                       eta = 0.01, 
                       nround = 1000, 
                       colsample_bytree = opt_colsample_bytree,      
                       min_child_weight = opt_min_child_weight,
                       subsample = opt_subsample,
                       gamma = opt_gamma,
                       lambda = 1, 
                       alpha = 0,
                       scale_pos_weight = (length(dtrain$label) / sum(dtrain$label) - 1),
                       verbose = 0,
                       eval_metrics=list("error"),
                       objective="binary:logistic")
  
  imp <- xgb.importance(feature_names=dtrain$data@Dimnames[[2]], model=model_xgb)
  num_of_vars_new <- length(imp[imp$Gain>0]$Feature)
  i <- i+1
}

xgb.plot.importance(imp, top_n = 20)
remove(list = c("i", "cv_folds", "dtrain", "dm_train", "imp", "mat_train", 
                "OPT_Res", "opt_para", "opt_colsample_bytree", "opt_gamma", "opt_max_depth", 
                "opt_min_child_weight", "opt_subsample", "xgb_cv_bayes", "num_of_vars_new", "num_of_vars_old"))

#' Save the post-transformation features that have been selected
save(der_var_selected, file = sprintf("../../../Data/For_Excecution/der_var_selected_%s.rda", waterfall_stage) )

#' Find the corresponding raw features
#' Special Variables
var_special_selected_der <- intersect(der_var_selected, names(kit_xgb$kit_special_var))
var_special_selected_raw <- vector()
for (i in var_special_selected_der) {
  var_special_selected_raw <- union(var_special_selected_raw, kit_xgb$kit_special_var[[i]]$raw_var_name)
}
raw_var_selected <- setdiff(der_var_selected, var_special_selected_der)
raw_var_selected <- union(raw_var_selected, var_special_selected_raw)

#' Restore derived variable names back to raw variable names
get_raw_var_name <- function(der_var_name){
  raw_var_name <- sub("_der$", "", der_var_name, perl=TRUE)
  raw_var_name <- sub("_lev_[[:graph:]]+$", "", raw_var_name, perl=TRUE)
  raw_var_name <- sub("_clean$", "", raw_var_name, perl=TRUE)
  raw_var_name <- sub("_isBAD$", "", raw_var_name, perl=TRUE)
  return(unique(raw_var_name))
}

raw_var_selected <- get_raw_var_name(raw_var_selected)
print(length(raw_var_selected))
remove(get_raw_var_name)
save(raw_var_selected, file = sprintf("../../../Data/For_Excecution/raw_var_selected_%s.rda", waterfall_stage))

#' Re-create the kit_cate_var only for categorical variables that have been selected
var_cate <- intersect(kit_xgb$kit_cate_var$raw_var_name, raw_var_selected)
kit_xgb$kit_cate_var$raw_var_name <- var_cate
kit_xgb$kit_cate_var$obj <- vtreat::designTreatmentsZ(train_[,var_cate], var_cate, minFraction=0, verbose=FALSE)
save(kit_xgb, file = sprintf("../../../Data/For_Excecution/kit_xgb_%s.rda", waterfall_stage))

#' Save the final model object
save(model_xgb, file = sprintf("../../../Data/For_Excecution/model_xgb_%s.rda", waterfall_stage))
remove(list = c("i", "var_special_selected_der", "var_special_selected_raw", "var_cate", "train_"))

#' Prepare Test Data
load(sprintf("../../../Data/For_Excecution/der_var_selected_%s.rda", waterfall_stage))
load(sprintf("../../../Data/For_Excecution/raw_var_selected_%s.rda", waterfall_stage))
load(sprintf("../../../Data/For_Excecution/kit_xgb_%s.rda", waterfall_stage))
load("../../../Data/For_Excecution/exe_feature_engineering.rda")
load(sprintf("../../../Data/For_Excecution/model_xgb_%s.rda", waterfall_stage))

test <- exe_feature_engineering(test, kit_xgb, raw_var_selected)

#' Test model
dtest <- list()
mat_test <- as.matrix(sapply(test[, der_var_selected], as.numeric))
print(dim(mat_test))
dtest$data <- Matrix(mat_test, sparse=TRUE)
dtest$label <- as.numeric(test$FPD)

#' Create a prediction object
probs <- predict(model_xgb, dtest$data)

#' Model Evaluation
pred <- ROCR::prediction(predictions=probs, labels=test$FPD)

#' Draw KS chart 
score1 <- pred@predictions[[1]][pred@labels[[1]]==0]
score2 <- pred@predictions[[1]][pred@labels[[1]]==1]
group <- c(rep("score1", length(score1)), rep("score2", length(score2)))
dat <- data.frame(KSD=c(score1,score2), group=group)
#' Create ECDF of data
cdf1 <- ecdf(score1) 
cdf2 <- ecdf(score2) 
#' Find min and max statistics to draw line between points of greatest distance
minMax <- seq(min(score1, score2), max(score1, score2), length.out=length(score1))
#' Compute KS
ks <- max(abs(cdf1(minMax) - cdf2(minMax))) %>% print()
#' Find the predicted probability where the cumulative distributions have the biggest difference. 
x0 <- minMax[which(abs(cdf1(minMax) - cdf2(minMax)) == ks)] 
y0 <- cdf1(x0)
y1 <- cdf2(x0)

dat %>%
  ggplot(aes(x=KSD, group=group, color=group)) +
  stat_ecdf(size=1) +
  geom_segment(aes(x=x0[1], y=y0[1], xend=x0[1], yend=y1[1]),
               linetype="dashed", color="red") +
  geom_point(aes(x=x0[1], y=y0[1]), color="red", size=5) +
  geom_point(aes(x=x0[1], y=y1[1]), color="red", size=5) +
  annotate("text", x=0.3, y=0.00, hjust=0.1, vjust=0, size=5,
           label=paste("KS =", round(ks, 3))) +
  labs(x="Score",
       y="ECDF",
       title="KS Plot for Extreme Gradiant Boost Model",
       caption="Source: " %s+% "xgboost") +
  theme(axis.text.x=element_text(hjust=1))

#' Draw ROC curve
perf <- ROCR::performance(pred, "tpr", "fpr")
auc <- ROCR::performance(pred, "auc")@y.values[[1]] %>% print()
pd <- data.frame(fpr=unlist(perf@x.values), tpr=unlist(perf@y.values))

pd %>%
  ggplot(aes(x=fpr, y=tpr)) +
  geom_line(colour="red") +
  geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey") +
  annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
           label=paste("AUC =", round(auc, 3))) +
  labs(x="False Positive Rate",
       y="True Positive Rate",
       title="ROC Curve for Extreme Gradiant Boosting Model",
       caption="Source: " %s+% "xgboost") +
  theme(axis.text.x=element_text(hjust=1))