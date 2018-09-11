
library(smbinning)
library(vtreat)
# This code is to deal with One hot encoding

data <- get(load("Data_Independent_20180628.Rda"))
df = data
summary(as.numeric(df$c01_ssn_first_name_count))
table(df$d02_MaximumTradelinePrincipal)




data_wd <- "W:/Analytics_Group_Files/US_files/2018_ProjectFiles/US1807_Unpack_Date_Set"


# --------------------------------------------------------- #
# Product Universal Cleaning Code
# --------------------------------------------------------- #
# Args: Raw Product Data Frame
#       Add the prefix and switch the . to _
#       FUN which test the out of range by the product dictionary
#       FUN which orgainze the MRID and Time of Application
setwd(data_wd)
att <- read.csv(file="Variable_Attributes_20180831.csv",header=T)

FUNC_Convention_Treatment <- function(data,att,name_exc,name_prefix){
  # Args: This function will take in a data frame, data attributes, name for exception
  #       a. Paste the prefix string to the variable names ; Gsub the . to _
  #       b. Filter the data by the attribute table for usable variables
  #       c. Check numeric ranges and character levels for outliers
  #Character: NONE    #Numeric: -0.01
  #       d. Treat the blank, null, none value to NA
  
  data_trans <- data[,colnames(data)%in%name_exc==F]
  data_keeps <- data[,name_exc]
  
  #----------------------------------------------------------------------
  # 
  cat("a. Paste the prefix string to the variable names ; Gsub the . to _")
  cat("\n")
  colnames(data_trans) <- gsub(".","_",colnames(data_trans),fixed = T)
  colnames(data_trans) <- paste(name_prefix,colnames(data_trans),sep="_")
  
  #----------------------------------------------------------------------
  # 
  cat("b. Filter the data by the attribute table") 
  cat("\n")
  name_useful <- colnames(data_trans) %in% att[att$Not_Useful==0,]$variable_names
  data_useful <- data_trans[name_useful]
  name_treatment <- colnames(data_useful)
  
  #----------------------------------------------------------------------
  # 
  cat("c. Check levels and outliers")
  cat("\n")
  att_num <- intersect(as.character(att[att$type_integer==1,]$variable_names),name_treatment)
  att_chr <- intersect(as.character(att[att$type_character==1,]$variable_names),name_treatment)
  
  #========================= Check the numeric variable out of range =============================#
  data_num <- data_trans[,att_num]
  data_num <- lapply(data_num, function(x) as.numeric(as.character(x)))
  data_num <- as.data.frame(data_num,stringsAsFactors=FALSE)
  
  range_mx <- att[att$variable_names %in% att_num,c("variable_names","range_min","range_max")]
  data_num_check_range <- data_num   # Get the range table
  
  for(i in 1:nrow(range_mx)){
    data_num_check_range[,i] <- findInterval(data_num[,i],c(range_mx[i,]$range_min-1,range_mx[i,]$range_max)+1)==1
  } # Check whether every col is within range
  
  data_num[data_num_check_range==F] <- -0.01  # Out of range to -0.01
  cat("numeric finished")
  cat("\n")
  # data_num[is.na(data_num_check_range)] <- -0.01   # Na to -0.01  But this time, we will leave them there
  
  #========================== Check the character variable levels =============================#
  data_chr <- data_trans[,att_chr]
  data_chr <- apply(data_chr, 2, as.character)
  data_chr <- as.data.frame(data_chr,stringsAsFactors=FALSE)
  
  range_mx <- att[att$variable_names %in% att_chr & att$range_levels%in%c("date_level","multi_level")==F,c("variable_names","range_levels")]
  range_mx$range_levels <- as.character(range_mx$range_levels)
  range_mx_list <- lapply(range_mx$range_levels,function(x) strsplit(x,";"))
  
  # Character has three main situations, date variable, high level variable and normal variable
  # att_chr_date <- unlist(intersect(att[att$range_levels=="date_level",]$variable_names,att_chr))
  att_chr_level <- unlist(intersect(att[att$range_levels%in%c("multi_level","date_level")==F,]$variable_names,att_chr))
  att_chr_multi <- unlist(intersect(att[att$range_levels%in%c("multi_level","date_level"),]$variable_names,att_chr))
  
  data_chr_level <- as.data.frame(data_chr[,unlist(att_chr_level)],stringsAsFactors=FALSE)
  data_chr_multi <- as.data.frame(data_chr[,unlist(att_chr_multi)],stringsAsFactors=FALSE)
  
  data_chr_check_range <- data_chr_level
  dim(data_chr_level)
  # Date Variables
  for(i in 1:nrow(range_mx)){
    data_chr_check_range[,i] <- data_chr_level[,i] %in% range_mx_list[[i]][[1]]
  }
  
  data_chr_level[data_chr_check_range==F] <- "NONE" # Out of range to NONE
  # data_chr_level[is.na(data_chr_check_range)] <- "NONE"  # Na to NONE BUT this time we leave it there
  data_chr_level[data_chr_level=="None"] <- NA
  data_chr_level[data_chr_level=="NULL"] <- NA
  data_chr_level[data_chr_level==""] <- NA  # We give this name just avoid confusion
  
  data_chr_multi[data_chr_multi=="None"] <- NA
  data_chr_multi[data_chr_multi=="NULL"] <- NA
  # data_chr_multi[is.na(data_chr_multi)] <- "NONE"  # We will leave the vtreat to handle the NA
  data_chr_multi[data_chr_multi==""] <- NA
  
  cat("character finished")
  cat("\n")
  data_res = cbind(data_num,data_chr_level,data_chr_multi)
  
  print(colnames(data_trans)[!name_useful])
  data_return <- cbind(data_keeps,data_res)
  return(data_return)
}


C01 <- read.csv(file="new_c01_complete.csv",header = T)
name_exc <- c("PortfolioID","Application_ID","ScoredDate")
name_prefix <- "c01"
df_c01 <- FUNC_Convention_Treatment(C01,att,name_exc,name_prefix)
save(df_c01,file="df_c01.rda")

C06

D02

D04

NS01


head(C01)
dim(C01)
colnames(C01)

# --------------------------------------------------------- #
# Product Specific Cleaning Code
# --------------------------------------------------------- #
# Args: Raw Product Data Frame
#       FUN which do transformation into statistical raw data
#       FUN which change the data into readable fashion
#       FUN which tag the variable name by product


# C01 Cleaning
setwd(data_wd)
df_c01 = get(load(file="df_c01.rda"))
setwd(data_wd)


# C01 Data Cleaning Series
FUNC_C01_Date_To_Days <- function(data){
  # Args: This function transfer the date to days. It is special designed for the Clarity Inquiry Product
  # Warning: Date variable which indicate the application date should present in the data frame with name "sys_ApplicationDate"
  
  if("sys_ApplicationDate"%in% colnames(data)==F){
    print("Error: Date varible named 'sys_ApplicationDate' not found")
    stop()
  }
  
  format1 <- "%m/%d/%Y"
  format2 <- "%Y-%m-%d"
  app <- as.Date(substr(data$sys_ApplicationDate,1,10),format="%Y-%m-%d")
  data$c01_date_of_birth <- floor(difftime(app,as.Date(substr(data$c01_date_of_birth,1,10),format=format1),units = "days")/365)
  data$c01_date_of_next_payday = -difftime(app,as.Date(substr(data$c01_date_of_next_payday,1,10),format=format1),units = "days")
  data$c01_date_of_last_activity <- difftime(app,as.Date(substr(data$c01_date_of_last_activity,1,10),format="%Y-%m-%d"),units = "days")
  data$c01_last_seen_by_group <- difftime(app,as.Date(substr(data$c01_last_seen_by_group,1,10),format="%Y-%m-%d"),units = "days")
  data$c01_last_seen_by_location <- difftime(app,as.Date(substr(data$c01_last_seen_by_location,1,10),format="%Y-%m-%d"),units = "days")
  data$c01_last_seen_by_account <- difftime(app,as.Date(substr(data$c01_last_seen_by_account,1,10),format="%Y-%m-%d"),units = "days")
  data$c01_last_purchased <- difftime(app,as.Date(substr(data$c01_last_purchased,1,10),format="%Y-%m-%d"),units = "days")
  
  
  return(data)
}
FUNC_C01_Routing_Number <- function(data){
  # Args: This function will change the routing number to it's full length
  
  data$c01_bank_routing_number <- as.character(data$c01_bank_routing_number)
  data$c01.bank_routing_number <- sapply(data$c01_bank_routing_number,
                                         function(x) {
                                           if (nchar(x)==7) paste("00", x, sep = "")
                                           else if (nchar(x)==8) paste("0", x, sep = "")
                                           else if (x=="-0.01") "Blank_NULL"
                                           else if (x=="-0.02") "Blank_NA"
                                           else x
                                         })
  
  return(data)
}

colnames(census) <- paste("c01",colnames(census),sep="_")
colnames(census)[1] <- "c01_zip_code"
FUNC_C01_Census_Date <- function(data,census){
  # Args: This function will merge the census data by the zip code
  # Warning: Need to load the census data and provide it with the zip code called "c01_zip_code"
  
  data = merge(data, census, all.x = TRUE, by="c01_zip_code")
  
  return(data)
}
head(census)

df_c01 <- FUNC_C01_Date_To_Days(df_c01)
df_c01 <- FUNC_C01_Routing_Number(df_c01)
df_c01 <- FUNC_C01_Census_Date(df_c01,census)
colnames(df_c01)

# C06 Data Cleaning Series
FUNC_C06_Reason_Code_To_Level 
FUNC_C06_99999_To_NA

# D04 Data Cleaning Series
FUNC_D04_Fua_To_Level
FUNC_D04_HRI_To_Level

# D02 Data Cleaning Series
FUNC_D02_Date_To_Days

# After all these changes, there should be no multi_level problems


# --------------------------------------------------------- #
# Dependent Measure Cleaning Code
# --------------------------------------------------------- #
# Args: We define five different dependent measure this time
#       FPD - First Payment Default
#       SPD - Second Payment Default Given didn't FPD
#       TPO - Total Pay off indicated by the loan status equals to pay off
#       SWD - System With Draw which we didn't talk to the customer
#       CWD - Customer With Draw populated by most frequent controllable reason codes
setwd(data_wd)
perf <- read.csv(file="perf_full_processed_170101_180628_plus_withdraw.csv",header=T)
head(perf)

colnames(perf)[1] <- "PortfolioID"
dim(perf)

# FPD - First Payment Default
y_FPD <- perf[perf$FirstPaymentDefault!="NULL",c("PortfolioID","Application_ID","FirstPaymentDefault")]
colnames(y_FPD)[3] <- "y"
# y_FPD <- as.character(y_FPD)

# SPD - Second Payment Default
y_SPD <- perf[perf$SecondPaymentDefault!="NULL",c("PortfolioID","Application_ID","SecondPaymentDefault")]

# TPO - Total Paid Off
perf$LoanStatusToDate <- as.character(perf$LoanStatusToDate)
y_TPO <- perf[perf$LoanStatusToDate %in% c("N","K","W")==F,c("PortfolioID","Application_ID","LoanStatusToDate")]
y_TPO[y_TPO$LoanStatusToDate %in% c("D","A"),"LoanStatusToDate"] <- 1
y_TPO[y_TPO$LoanStatusToDate %in% c("D","A")==F,"LoanStatusToDate"] <- 0

# SWD - System With Draw
y_SWD <- perf[,c("PortfolioID","Application_ID","SystemWithdrawn")]


# CWD - With Draw Reason in detail
# Customer Cancelled - Fee too High
#                    - Needs funds sooner then we can provide
#                    - Amount too low
#                    - No Longer interested
y_CWD <- perf[perf$SystemWithdrawn==0,c("PortfolioID","Application_ID","WithdrawnReason")]
y_CWD


# ---------------------------------------------------------- #
# Creating Vtreat objects and encoding selection
# ---------------------------------------------------------- #

# c01 vtreat and objects
df_c01_y_FPD <- merge(df_c01,y_FPD,by=c("PortfolioID","Application_ID"))

df_c01_y_FPD$ScoredDate <- NULL
df_c01_y_FPD$Month <- format(df_c01_y_FPD$sys_ApplicationDate,"%m-%y")

FUNC_Encode_Preparation <- function(data,att){
  # data = df_c01_y_FPD
  
  encode1_name <- c(colnames(data)[colnames(data) %in% att[att$Encoding==1,]$variable_names],"y")
  encode2_name <- colnames(data)[colnames(data) %in% att[att$Encoding==2,]$variable_names]
  
  df1 <- apply(data[,encode1_name],2,as.character)
  # dim(df1)
  df2 <- apply(data[,encode2_name],2,as.numeric)

  df_out <- cbind(df1,df2)
  return(df_out)
}

dim(df_x_c01_y_FPD)
df_x_c01_y_FPD <- as.data.frame(df_x_c01_y_FPD)
df_x_c01_y_FPD <- FUNC_Encode_Preparation(df_c01_y_FPD,att)
colnames(df_x_c01_y_FPD)

df_x_c01_y_FPD$y
cfe <- mkCrossFrameCExperiment(df_x_c01_y_FPD, colnames(df_x_c01_y_FPD)[1:length(colnames(df_x_c01_y_FPD))-1],'y')
df_x_c01_y_FPD$y = as.numeric(as.character(df_x_c01_y_FPD$y))


treatments <- designTreatmentsC(df_x_c01_y_FPD, colnames(df_x_c01_y_FPD),'y',1)
cfe <- mkCrossFrameNExperiment(df_x_c01_y_FPD, c('c01_B03001_002E', 'c01_date_of_birth'),'y')

df_x_c01_y_FPD




ID_name_df_c01_y_FPD <- c("PortfolioID","Application_ID","Month","sys_ApplicationDate")
encode1_name_df_c01_y_FPD <- colnames(df_c01_y_FPD)[colnames(df_c01_y_FPD) %in% att[att$Encoding==1,]$variable_names]
encode2_name_df_c01_y_FPD <- colnames(df_c01_y_FPD)[colnames(df_c01_y_FPD) %in% att[att$Encoding==2,]$variable_names]

df_x_c01_y_FPD <- df_c01_y_FPD[encode1_name_df_c01_y_FPD]


x_name_df_c01_y_FPD <- colnames(df_c01_y_FPD)[colnames(df_c01_y_FPD) %in% ID_name_df_c01_y_FPD==F]
ID_df_c01_y_FPD <- df_c01_y_FPD[,c("PortfolioID","Application_ID","Month","sys_ApplicationDate")]
table(df_c01_y_FPD$Month)






# ---------------------------------------------------------- #
# Modeling and Feature Engineering
# ---------------------------------------------------------- #
C01 <- read.csv(file="new_c01_complete.csv",header = T)
head(C01)
dim(C01)
colnames(C01)

C01_df <- merge()


head(df_c01)

table(C01$state)
# FUN: Date to Days

df_c01$sys_ApplicationDate <- as.Date(df_c01$ScoredDate)
 
perf$SystemWithdrawn <- as.character(perf$LoanStatusToDate)



table(perf$WithdrawnReason)
table(perf$IsOrig)
table(perf$SystemWithdrawn)
perf$IsOrig

dim(y_FPD)
dim(y_SPD)
dim(y_TTD)


head(perf)

table(perf[perf$LoanStatusToDate%in%c("W"),]$WithdrawnReason )
dim(perf[perf$LoanStatusToDate%in%c("W"),])



#--------------------> Create Training Testing Validation <----------------------#
colnames(y_FPD)
colnames(df_c01)

df_c01_y_FPD <- merge(df_c01,y_FPD,by=c("PortfolioID","Application_ID"))
df_c01_y_FPD$ScoredDate <- NULL
df_c01_y_FPD$Month <- format(df_c01_y_FPD$sys_ApplicationDate,"%m-%y")

ID_name_df_c01_y_FPD <- c("PortfolioID","Application_ID","Month","sys_ApplicationDate")
x_name_df_c01_y_FPD <- colnames(df_c01_y_FPD)[colnames(df_c01_y_FPD) %in% ID_name_df_c01_y_FPD==F]
ID_df_c01_y_FPD <- df_c01_y_FPD[,c("PortfolioID","Application_ID","Month","sys_ApplicationDate")]
table(df_c01_y_FPD$Month)


# ****** Time Range Cut ******* #
FUNC_Time_Range_Cut_Ind_List <- function(data){
  # Args: This function will give a list of index with indicating the business operation time range
  # Warning: Varible "sys_ApplicationDate" needs to be avaliable
  
  btime_ind <- seq(1:nrow(data))[data$sys_ApplicationDate < "2017-08-01"]
  gtime_ind <- seq(1:nrow(data))[data$sys_ApplicationDate >= "2017-08-01" & data$sys_ApplicationDate < "2018-03-01"]
  rtime_ind <- seq(1:nrow(data))[data$sys_ApplicationDate >= "2018-03-01"]
  
  ind_list <- list(btime_ind=btime_ind,
                   gtime_ind=gtime_ind,
                   rtime_ind=rtime_ind)
  return(ind_list)
}

ind_list_BGR_df_c01_y_FPD <- FUNC_Time_Range_Cut_Ind_List(df_c01_y_FPD)


# ****** Cross Validation Cut ******* #
FUNC_Train_Valid_Test_Ind_List <- function(data,seed,per){
  
  sp_size = per * nrow(data)
  test_valid_ind = sample(1:nrow(data),size=sp_size)
  
  t_size = .5 * length(test_valid_ind)
  test_ind = sample(test_valid_ind,t_size)
  valid_ind = test_valid_ind[-test_ind]
  train_ind = seq(1:nrow(data))[-test_valid_ind]
    
  ind_list <- list(train_ind=train_ind,
                   valid_ind=valid_ind,
                   test_ind=test_ind)
  return(ind_list)
}

ind_list_B_TVT_df_c01_y_FPD <- FUNC_Train_Valid_Test_Ind_List(df_c01_y_FPD[ind_list_BGR_df_c01_y_FPD$btime_ind,],seed=2018,0.3)
ind_list_G_TVT_df_c01_y_FPD <- FUNC_Train_Valid_Test_Ind_List(df_c01_y_FPD[ind_list_BGR_df_c01_y_FPD$gtime_ind,],seed=2018,0.5)
ind_list_R_TVT_df_c01_y_FPD <- FUNC_Train_Valid_Test_Ind_List(df_c01_y_FPD[ind_list_BGR_df_c01_y_FPD$rtime_ind,],seed=2018,0.5)


#--------------------> Create V Treatment <----------------------#

ind_list_B_TVT_df_c01_y_FPD

colnames(df_c01_y_FPD)
cat_var_names <- as.character(att[att$Not_Useful==0 & att$variable_prefix=="c01" & 
                       att$cat_categorical==1 & att$type_character==1 &
                       att$range_levels%in%c("multi_level","date_level")==F,]$variable_names)

df_x_c01_y_FPD <- df_c01_y_FPD[,x_name_df_c01_y_FPD]


#--------------------> Cross Valid For Encoding <----------------------#

colnames(df_x_c01_y_FPD)
cfe <- mkCrossFrameCExperiment(df_x_c01_y_FPD, c('c01_B03001_002E', 'c01_date_of_birth'), 'y')
df_x_c01_y_FPD$y = as.numeric(as.character(df_x_c01_y_FPD$y))


treatments <- designTreatmentsC(df_x_c01_y_FPD, colnames(df_x_c01_y_FPD),'y',1)
cfe <- mkCrossFrameNExperiment(df_x_c01_y_FPD, c('c01_B03001_002E', 'c01_date_of_birth'),'y')

att$cat_categorical

mkCrossFrameNExperiment

cfe$treatments$scoreFrame
treatments$scoreFrame
treatments$treatments






df_c01_y_FPD_treat <- designTreatmentsZ(df_x_c01_y_FPD,cat_var_names,rareCount=0)
c01_treatments$scoreFrame
df_x_c01_y_FPD$y <- as.numeric(as.character(df_x_c01_y_FPD$y))


c01_train_treat <- prepare(c01_treatments,c01_train,pruneSig=0.99)
c01_test_treat <- prepare(c01_treatments,c01_test,pruneSig=0.99)
dim(c01_test_treat)








# FUN: Additional Data

#_______________________________________________________________
# Clairy Bureau Lite

# FUN: reason code split

# FUN: Date tp days



#_______________________________________________________________
# DataX IDV

# FUN: Risk code hri and fua to dummies



#_______________________________________________________________
# DataX RPT
# FUN: Date to Days




#_______________________________________________________________
# Neustar 
# FUN: cell phone










colnames(C01_final)



setwd("W:/Analytics_Group_Files/US_files/2018_ProjectFiles/US1807_Unpack_Date_Set")
att = read.csv(file="Variable_Attributes_20180628.csv")



c01 = get(load("C01_final_20180628.Rda"))
dim(c01)
colnames(c01)
head(c01)
dim(data)
colnames(data)


c01=c01[,c(1:110,156)]
dim(c01)
head(c01)


length(y)

data_c01 = data[]

designTreatmentsC

varlist
outcomename

dim(c01)

c01


dTrainC <- data.frame(x=c('a','a','a','b','b','b'),
                      z=c(1,2,3,4,5,6),
                      y=c(FALSE,FALSE,TRUE,FALSE,TRUE,TRUE))
dTestC <- data.frame(x=c('a','b','c',NA),
                     z=c(10,20,30,NA))

dim(c01)
colnames(c01)


c01_y = merge(c01,Data_Dependent,by="MRID")
c01 <- c01_y[,1:111]
dim(c01_y)
c01_y$FPD
length(c01_y$FPD)
c01_perf <- cbind(c01,c01_y$FPD)
colnames(c01_perf)[112] <- "FPD"
c01_perf <- c01_perf[is.na(c01_perf$FPD)==F,]
dim(c01_perf)

c01_train <- c01_perf[1:20000,]
c01_test <- c01_perf[20000:39086,]

c01_treatments <- designTreatmentsC(c01_train,colnames(c01_train),'FPD',TRUE)
c01_treatments$scoreFrame



c01_train_treat <- prepare(c01_treatments,c01_train,pruneSig=0.99)
c01_test_treat <- prepare(c01_treatments,c01_test,pruneSig=0.99)
dim(c01_test_treat)

colnames(c01_test_treat)
head(c01_test_treat)
c01_test_treat$

## load German credit data
data("GermanCredit")
## training/validation split
train <- sample(nrow(GermanCredit), round(0.6*nrow(GermanCredit)))
woemodel <- woe(credit_risk~., data = GermanCredit[train,], zeroadj=0.5, applyontrain = TRUE)
woemodel
## plot variable information values and woes
plot(woemodel)
plot(woemodel, type = "woes")
## apply woes
traindata <- predict(woemodel, GermanCredit[train,], replace = TRUE)
str(traindata)
## fit logistic regression model
glmodel <- glm(credit_risk~., traindata, family=binomial)

200
200




vtreat





