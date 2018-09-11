
# Cleaning code for the 2016 and 2017 data

Integrity_wd <- "//Consrvdc/company_data/Analytics_Group_Files/US_files/2017_ProjectFiles/US1705_Model_Data_Integrity_S"
data_wd <- "W:/Analytics_Group_Files/US_files/Modeling_Process/Modeling_Process_Gamma/Data_Csv_Update"
project_wd <- "W:/Analytics_Group_Files/US_files/2018_ProjectFiles/US1804_Data_Cleaning_S"
raw_wd <- "W:/Analytics_Group_Files/US_files/2018_ProjectFiles/US1807_Unpack_Date_Set"


setwd(raw_wd)
C01 <- read.csv(file="c01_complete.csv",header = T)
C06 <- read.csv(file="c06_complete.csv",header = T)
D02_ats <- read.csv(file="d02_complete_ats.csv",header = T)
D02_uid <- read.csv(file="d02_complete_uid.csv",header = T)
D04 <- read.csv(file="d04_complete.csv",header = T)
NS01_c <- read.csv(file="ns01_complete.csv",header = T)
Perf <- read.csv(file="perf_full_processed_170101_180628.csv",header = T)
D04_a <- D04


sum(temp5$c01_zip_code %in%census$c01_zip_code)

setwd(project_wd)
# ---------------------------------------------------------------------------->
# Read and load all the datasets
# ---------------------------------------------------------------------------->
# C01 <- get(load("C01.Rda"))
# C06 <- get(load("C06.Rda"))
# D02_ats <- get(load("D02ATS.Rda"))
# D02_uid <- get(load("D02UID.Rda"))
# D04_a <- get(load("D04.Rda"))
D04_b <- read.csv(file="Speedy_IDV_Output_Anonymized_28JUN17.csv",header = T)
NS01_a <- read.csv(file="US2016_Retrofit_NS.csv",header = T)
NS01_b <- read.csv(file="SpeedyServicing_MPIC_allout.csv",header = T)
# NS01_c <- get(load("NS01.Rda"))
match_ID <- get(load("match_ID.r"))

# ---------------------------------------------------------------------------->
# Generate the MRID and Product indicators !! Use _ instead of . for the latest version
# ---------------------------------------------------------------------------->
Perf$MRID <- paste(Perf$PortFolioID,Perf$Application_ID,sep="_")
# Perf$MRID <- paste(Perf$ScoredPortfolioID,Perf$ScoredAppID,sep="_")

length(Perf$MRID) # 1676187
head(Perf)

dim(C01)
colnames(C01) <- gsub(".","_",colnames(C01),fixed = T)
colnames(C01) <- paste("c01",colnames(C01),sep="_")
C01$MRID <- paste(C01$c01_PortfolioID,C01$c01_Application_ID,sep="_")
C01$c01_PortfolioID <- NULL
C01$c01_Application_ID <- NULL
sum(C01$MRID %in% Perf$MRID)  # 172704



dim(C06) # 271766  106
colnames(C06) <- gsub(".","_",colnames(C06),fixed = T)
colnames(C06) <- paste("c06",colnames(C06),sep="_")
C06$MRID <- paste(C06$c06_ï__PortfolioID,C06$c06_Application_ID,sep="_")
C06$c06_ï__PortfolioID <- NULL
C06$c06_Application_ID <- NULL
sum(C06$MRID %in% Perf$MRID) # 172710

dim(D02_ats) # 276772    306
dim(D02_uid) # 276676    287
colnames(D02_ats)
D02_uid$MRID <- paste(D02_uid$ï..PortfolioID,D02_uid$Application_ID,sep="_")
D02_uid$ï..PortfolioID <- NULL
D02_uid$Application_ID <- NULL
D02_ats$MRID <- paste(D02_ats$ï..PortfolioID,D02_ats$Application_ID,sep="_")
D02_ats$ï..PortfolioID <- NULL
D02_ats$Application_ID <- NULL
D02 <- merge(D02_ats,D02_uid,by="MRID")
dim(D02) # 275696   592
colnames(D02) <- gsub(".","_",colnames(D02),fixed = T)
colnames(D02) <- paste("d02",colnames(D02),sep="_")
colnames(D02)[1] <- "MRID"
sum(D02$MRID %in% Perf$MRID)  # 172912

 

dim(D04_a) # 253102    18
dim(D04_b) # 56712   30
colnames(D04_a) <- gsub(".","_",colnames(D04_a),fixed = T)
colnames(D04_a) <- c("PortFolioID","Application_ID","ScoredDate","verify_dob","valid_ssn",
                     "nap_summary","nap_status","nap_type","fua_1","fua_2",
                     "fua_3","fua_4","hri_1","hri_2","hri_3","hri_4","hri_5","hri_6")
D04_a$ScoredDate <- NULL
D04_b <- D04_b[colnames(D04_a)]
D04_a$MRID <- paste(D04_a$PortFolioID,D04_a$Application_ID,sep="_")
D04_a$PortFolioID <- NULL
D04_a$Application_ID <- NULL
D04_b$MRID <- paste(D04_b$PortFolioID,D04_b$Application_ID,sep="_")
D04_b$PortFolioID <- NULL
D04_b$Application_ID <- NULL
common_id <- intersect(D04_b$MRID,D04_a$MRID)
D04_a_id <- D04_a$MRID[D04_a$MRID%in%common_id==F]
D04_b_id <- D04_b$MRID[D04_b$MRID%in%common_id==F]
D04 <- rbind(D04_a[D04_a$MRID%in%D04_a_id,],D04_a[D04_a$MRID%in%common_id,],D04_b[D04_b$MRID%in%D04_b_id,])
dim(D04) # 271290   16
colnames(D04) <- paste("d04",colnames(D04),sep="_")
colnames(D04)[16] <- "MRID"
sum(D04$MRID %in% Perf$MRID) # 173227




dim(NS01_a) # 94951   67
dim(NS01_b) # 448346   62
dim(NS01_c) # 93673   55
colnames(NS01_a) <- gsub(".","_",colnames(NS01_a),fixed = T)
colnames(NS01_b) <- gsub(".","_",colnames(NS01_b),fixed = T)
colnames(NS01_c)
common_var <- intersect(colnames(NS01_a),colnames(NS01_b))
colnames(NS01_b)[colnames(NS01_b) %in% common_var ==F]
NS01_a <- NS01_a[,c("Port_and_App","EID_1320_ResultCode_Home",
                 "Prepaid_Phone_Attribute_Home","Business_Phone_Indicator_Home",
                 "Phone_In_Service_Indicator_Home","Phone_Type_Indicator_Home",
                 "Service_Discontinued_Indicator_Home","EID_1320_ResultCode_Mobile",
                 "Prepaid_Phone_Attribute_Mobile","Business_Phone_Indicator_Mobile",
                 "Phone_In_Service_Indicator_Mobile","Phone_Type_Indicator_Mobile",
                 "Service_Discontinued_Indicator_Mobile",common_var)]
colnames(NS01_a) <- c("MRID","Phone_1___EID_1320_ResultCode",
                      "Phone_1___Prepaid_Phone_Attribute","Phone_1___Business_Phone_Indicator",
                      "Phone_1___Phone_In_Service_Indicator","Phone_1___Phone_Type_Indicator",
                      "Phone_1___Service_Discontinued_Indicator","Phone_2___EID_1320_ResultCode",
                      "Phone_2___Prepaid_Phone_Attribute","Phone_2___Business_Phone_Indicator",
                      "Phone_2___Phone_In_Service_Indicator","Phone_2___Phone_Type_Indicator",
                      "Phone_2___Service_Discontinued_Indicator",common_var)
NS01_b$MRID <- paste(NS01_b$PortFolioID,NS01_b$Application_ID,sep="_")
NS01_b <- NS01_b[,c("MRID","Phone_1___EID_1320_ResultCode",
                "Phone_1___Prepaid_Phone_Attribute","Phone_1___Business_Phone_Indicator",
                "Phone_1___Phone_In_Service_Indicator","Phone_1___Phone_Type_Indicator",
                "Phone_1___Service_Discontinued_Indicator","Phone_2___EID_1320_ResultCode",
                "Phone_2___Prepaid_Phone_Attribute","Phone_2___Business_Phone_Indicator",
                "Phone_2___Phone_In_Service_Indicator","Phone_2___Phone_Type_Indicator",
                "Phone_2___Service_Discontinued_Indicator",common_var)]
dim(NS01_a)   # 94951   51
dim(NS01_b)   # 448346   51

250/700

NS01_b <- merge(NS01_b, match_ID, by="MRID")
dim(NS01_b)
head(NS01_b)
NS01_b$MRID <- NULL
colnames(NS01_b)[51] <- "MRID"

common_id_ns <- intersect(NS01_a$MRID,NS01_b$MRID)
length(common_id_ns)  # 1982
NS01 <- rbind(NS01_a[NS01_a$MRID %in% common_id_ns == F,],NS01_b)
dim(NS01) # 148176   51
colnames(NS01) <- paste("ns01",colnames(NS01),sep="_")
colnames(NS01)[1] <- "MRID"
sum(NS01$MRID %in% Perf$MRID) # 141794   55008

colnames(NS01_c)
name_NS01_c <- c("PortfolioID","Application_ID","ScoredDate",
                 
                 "ns01_Phone_1___Business_Phone_Indicator",
                 "ns01_Phone_1___Phone_In_Service_Indicator",
                 "ns01_Phone_1___Phone_Type_Indicator",
                 "ns01_Phone_1___Prepaid_Phone_Attribute",
                 "ns01_Phone_1___Service_Discontinued_Indicator",
                 "ns01_Phone_2___Business_Phone_Indicator",
                 "ns01_Phone_2___Phone_In_Service_Indicator",
                 "ns01_Phone_2___Phone_Type_Indicator",
                 "ns01_Phone_2___Prepaid_Phone_Attribute",
                 "ns01_Phone_2___Service_Discontinued_Indicator",
                 
                 
                 "ns01_X1st_Address_CMRA",
                 "ns01_X1st_Address_DPV_Confirm",
                 "ns01_X1st_Address_Prison",
                 "ns01_X1st_Address_RBDI",
                 "ns01_X1st_Address_Score",
                 "ns01_X1st_Address_USPS_Type",
                 "ns01_X1st_Address_Vacancy",
                 "ns01_X1st_Address_Validation",
                 "ns01_X1st_Email_Reason",
                 "ns01_X1st_Email_Repository",
                 "ns01_X1st_Email_Score",
                 "ns01_X1st_Email_Validation",
                 "ns01_X1st_IPAddress_Country",
                 "ns01_X1st_IPAddress_State",
                 "ns01_X1st_IPAddress_Validation",
                 "ns01_X1st_Phone_Active",
                 "ns01_X1st_Phone_BPI",
                 "ns01_X1st_Phone_Connected_in_DA",
                 "ns01_X1st_Phone_Daylight_Savings_Time_Observed",
                 "ns01_X1st_Phone_Mobile",
                 "ns01_X1st_Phone_Postal_State_Code",
                 "ns01_X1st_Phone_Score",
                 "ns01_X1st_Phone_Time_Zone",
                 "ns01_X1st_Phone_Validation",
                 
                 
                 "ns01_X2nd_Phone_Active",
                 "ns01_X2nd_Phone_BPI",
                 "ns01_X2nd_Phone_Connected_in_DA",
                 "ns01_X2nd_Phone_Daylight_Savings_Time_Observed",
                 "ns01_X2nd_Phone_Mobile",
                 "ns01_X2nd_Phone_Postal_State_Code",
                 "ns01_X2nd_Phone_Score",
                 "ns01_X2nd_Phone_Time_Zone",
                 "ns01_X2nd_Phone_Validation",
                 
                 
                 "ns01_X3rd_Phone_Active",
                 "ns01_X3rd_Phone_BPI",
                 "ns01_X3rd_Phone_Connected_in_DA",
                 "ns01_X3rd_Phone_Daylight_Savings_Time_Observed",
                 "ns01_X3rd_Phone_Mobile",
                 "ns01_X3rd_Phone_Postal_State_Code",
                 "ns01_X3rd_Phone_Score",
                 "ns01_X3rd_Phone_Time_Zone",
                 "ns01_X3rd_Phone_Validation")

colnames(NS01_c) <- name_NS01_c

NS01_c$MRID <- paste(NS01_c$PortfolioID,NS01_c$Application_ID,sep="_")
sum(NS01_c$MRID %in% Perf$MRID) # 91575


# ---------------------------------------------------------------------------->
# Merge all the dataset using the MRID
# ---------------------------------------------------------------------------->
unique_MRID <- intersect(intersect(intersect(intersect(C01$MRID,C06$MRID),D02$MRID),D04$MRID),NS01$MRID)
unique_MRID <- intersect(intersect(intersect(C01$MRID,C06$MRID),D02$MRID),D04$MRID)
Perf$ScoredApplicationDate
date <- format(as.Date(Perf[Perf$MRID %in% unique_MRID,"ScoredApplicationDate"]), "%Y-%m")
t(table(date))
length(unique_MRID) # 264659

C01_final <- C01[C01$MRID %in% unique_MRID,]
C06_final <- C06[C06$MRID %in% unique_MRID,]
D02_final <- D02[D02$MRID %in% unique_MRID,]
D04_final <- D04[D04$MRID %in% unique_MRID,]
NS01_final <- NS01[NS01$MRID %in% unique_MRID,]
NS01_c_final <- NS01_c[NS01_c$MRID %in% unique_MRID,]
Perf_final <- Perf[Perf$MRID %in% unique_MRID,]

dim(C01_final) 
dim(C06_final)
dim(D02_final)
dim(D04_final)
dim(NS01_final)  # 136110
dim(NS01_c_final)  # 92564
dim(Perf_final)   # 171823

name_NS_common <- intersect(colnames(NS01_final),colnames(NS01_c_final))
NS_common_ab <- NS01_final[name_NS_common]
NS_common_c <- NS01_c_final[name_NS_common]
NS_uncommon_c <- NS01_c_final[colnames(NS01_c_final)%in%name_NS_common==F]
dim(NS_uncommon_c)
colnames(NS_uncommon_c)
NS_uncommon_c$MRID <- paste(NS_uncommon_c$PortfolioID,NS_uncommon_c$Application_ID,sep="_")
NS_uncommon_c$PortfolioID <- NULL
NS_uncommon_c$Application_ID <- NULL
NS_common <- rbind(NS_common_ab,NS_common_c)
dim(NS_common)

# setwd("W:/Analytics_Group_Files/US_files/Modeling_Process/Modeling_Process_Gamma/Data_Rda_Update")
setwd("W:/Analytics_Group_Files/US_files/2018_ProjectFiles/US1803_Second_Gen_Watefall_Design")
setwd(raw_wd)

# setwd(project_wd)
save(C01_final, file = "C01_final_20180628.rda")
save(C06_final, file = "C06_final_20180628.rda")
save(D02_final, file = "D02_final_20180628.rda")
save(D04_final, file = "D04_final_20180628.rda")
save(NS01_final, file = "NS01_final_201800628.rda")
save(NS_common, file = "NS01_common_final_20180628.rda")
save(NS_uncommon_c, file = "NS01_uncommon_final_20180628.rda")
save(Perf_final, file = "Perf_final_20180628.rda")


######################################################################################################################
# Up till this step, the individual data set has been merge. But they are not cleaned for the out of range and NA
# Beginning here we treat the out of range and extreme values

C01_final <- get(load("C01_final_20180628.rda"))
C06_final <- get(load("C06_final_20180125.rda"))
D02_final <- get(load("D02_final_20180125.rda"))
D04_final <- get(load("D04_final_20180125.rda"))
NS_common <- get(load("NS01_common_final_20180125.rda"))
NS_uncommon_c <- get(load("NS01_uncommon_final_20180125.rda"))
Perf_final <- get(load("Perf_final_20180125.rda"))

setwd(project_wd)
temp1 <- merge(C01_final,C06_final,by="MRID")
temp2 <- merge(temp1,D02_final,by="MRID")
dim(temp2) #
temp3 <- merge(temp2,D04_final,all.x= TRUE,by="MRID")
dim(temp3) # 
temp4 <- merge(temp3,NS_common,all.x = TRUE,by="MRID")
dim(temp4) # 
temp5 <- merge(temp4,NS_uncommon_c,all.x = TRUE,by="MRID")
dim(temp5) # 



dim(d04)

dim(Perf_final)
save(Perf_final, file="Perf_final.rda")



getwd()

save(temp5,file="Data_Independent_20180628.rda")
colnames(temp5)
getwd()


data <- get(load("Data_Independent_20180628.rda"))
perf <- get(load("Perf_final.rda"))

head(perf)
app_date <- perf[c("MRID","AcceptedAppDate_day")]
colnames(app_date)[2] <- "sys_ApplicationDate"

head(data)
table(data$c01.housing_status)

# "A" "B" "C" "D" "G" "K" "L" "N"        "P" "R" "W"
# "A" "B" "C" "D" "G"     "L" "N" "NULL" "P" "R" "W"
#  1   1   1   0   n   n   1   0          0   1   n

setwd("W:/Analytics_Group_Files/US_files/2018_ProjectFiles/US1803_Second_Gen_Watefall_Design")

# setwd("W:/Analytics_Group_Files/US_files/2017_ProjectFiles/US1708_Renjin_Envrionment_Design")
att <- read.csv(file="Variable_Attributes_20180709_V2.csv",header=T)
Data_Attributes <- att
select_col <- as.character(Data_Attributes[Data_Attributes$variable_product%in%c("system","neustar","clarity_inq") & Data_Attributes$Not_Useful==0 ,]$variable_names)

data <- merge(data,app_date,by="MRID")

select_col[select_col%in%colnames(data)==F]
data_wf1 <- data[,select_col]
dim(data_wf1)

dim(data)
FUNC_Data_Clean_WF1 <- function(data,att){

  # data = data_wf1  
  var_names <- colnames(data)
  att_num <- intersect(as.character(att[att$type_integer==1,]$variable_names),var_names)
  att_chr <- intersect(as.character(att[att$type_character==1,]$variable_names),var_names)
  
  ### Clean the numeric variables according to the range list from att
  data_num <- data[,att_num]
  data_num <- lapply(data_num, function(x) as.numeric(as.character(x)))
  data_num <- as.data.frame(data_num,stringsAsFactors=FALSE)
  
  range_mx <- att[att$variable_names %in% att_num,c("variable_names","range_min","range_max")]
  data_num_check_range <- data_num   # Get the range table
  
  for(i in 1:nrow(range_mx)){
    data_num_check_range[,i] <- findInterval(data_num[,i],c(range_mx[i,]$range_min-1,range_mx[i,]$range_max)+1)==1
  } # Check whether every col is within range

  data_num[data_num_check_range==F] <- -0.01  # Out of range to -0.01
  data_num[is.na(data_num_check_range)] <- -0.01   # Na to -0.01
  head(data_num)
  #---------------------------------------------------------------------------------------------------#
  
  ### Clean the character variables according to the range list from att
  data_chr <- data[,att_chr]
  data_chr <- apply(data_chr, 2, as.character)
  data_chr <- as.data.frame(data_chr,stringsAsFactors=FALSE)
  
  range_mx <- att[att$variable_names %in% att_chr & att$range_levels%in%c("date_level","multi_level")==F,c("variable_names","range_levels")]
  range_mx$range_levels <- as.character(range_mx$range_levels)
  range_mx_list <- lapply(range_mx$range_levels,function(x) strsplit(x,";"))
  range_mx_list[[2]][[1]]
  
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
   # Change all the variabels which has <NA> to "NONE"
  
  data_chr_level[data_chr_check_range==F] <- "NONE" # Out of range to NONE
  data_chr_level[is.na(data_chr_check_range)] <- "NONE"  # Na to NONE
  data_chr_level[data_chr_level=="None"] <- "NONE"
  data_chr_level[data_chr_level=="NULL"] <- "NONE"
  data_chr_level[data_chr_level==""] <- "NONE"
  
  data_chr_multi[data_chr_multi=="None"] <- "NONE"
  data_chr_multi[data_chr_multi=="NULL"] <- "NONE"
  data_chr_multi[data_chr_multi==""] <- "NONE"
  
  data_chr_multi[is.na(data_chr_multi)] <- "NONE"
  
  data_res = cbind(data_num,data_chr_level,data_chr_multi)

  head(data_res)
  return(data_res)
}

data_res <- FUNC_Data_Clean_WF1(data,att)
apply(data_res$c01_bank_account_number,2,function(x) nchar(x))





data_res$c01_bank_routing_number
data_res$c01_bank_routing_number <- sapply(data_res$c01_bank_routing_number, 
                                             function(x) {
                                               if (nchar(x)==7) paste("00", x, sep = "")
                                               else if (nchar(x)==8) paste("0", x, sep = "")
                                               else x
                                             })
getwd()


setwd(raw_wd)
save(data_res,file="US20180628_Clean_Raw_Data_WF_One.rda")
save(Perf_final,file="US20180628_Perf_Raw_Data.rda")

ddata = data_res[data_res$c01_zip_code %in% census$c01_zip_code == F,]
ddata[,c("MRID","c01_zip_code")]
ddata[3,]$c01_zip_code


data_res <- merge(data_res,census,by="c01_zip_code")
dim(data_res)
head(data_res)


sum(data_res$c01_zip_code%in%census$c01_zip_code == T )
sum(data_num$c01_zip_code%in%census$c01_zip_code == T )


length(data_res$c01_zip_code)
sum(data_res$c01_zip_code<0)


length(unique(census$c01_zip_code))
head(census)


dim(data_res)
head(data_res)
49339/(49339+122484)


#########################################################################################################























data$c01_bank_account_number


  data_num_check_range <- data_num   # Get the range table
  
  sign2 <- ";"
  by_var <- strsplit(range_mx$range_levels[4],sign2)
  
  FUNCTION_split_vector_str <- function(str){
    sign1 <- "~"
    sign2 <- ";"
    by_var <- strsplit(str,sign2)
    len_by_var <- lengths(by_var)
    var_names <- vector(length = len_by_var)
    var_value <- vector(length = len_by_var)
    for(i in 1:len_by_var){
      by_var_split <- as.character(strsplit(by_var[[1]][i],sign2))
      var_names[i] <- as.character(strsplit(by_var_split,sign1)[[1]][1])
      var_value[i] <- as.character(strsplit(by_var_split,sign1)[[1]][2])
    }
    var_value <- as.data.frame(t(var_value),stringsAsFactors = F)
    names(var_value) <- var_names
    return(var_value)
  }
  
  data_num_check_range <- lapply(data_num, function(x) range(x))
  
  dim(data_num_check_OFR)
  data_num_check_OFR <- apply(data_num,2, function(x) nchar(colnames(x)))
  
  cat("Number of data points being censored ",sum(is.na(data_num)))
  
 
  
  findInterval(data$c01_ofac_score, c(0,100) ) == 1
  
  
}



NC_Data.Assign_data_type <- function(data,att){
 data <-  Data_Independent
 att <-  Data_Attributes
  
  num1 <- colnames(Data_Independent)
  num2 <- Data_Attributes$variable_names
  if(length(num1)!= sum(num1%in%num2)){
    cat("Warning: illigal variable names observed in the data n/")
    out <- num1[num1%in%num2==F]
    # plot(out)
  }
  
  var_names <- colnames(data)
  att_num <- intersect(as.character(Data_Attributes[Data_Attributes$type_int==1,]$variable_names),var_names)
  att_chr <- intersect(as.character(Data_Attributes[Data_Attributes$type_char==1,]$variable_names),var_names)
  
  data_num <- data[,att_num]
  data_num <- lapply(data_num, function(x) as.numeric(x))
  data_num <- as.data.frame(data_num,stringsAsFactors=FALSE)
  cat("Number of data points being censored ",sum(is.na(data_num)))
  
  data_chr <- data[,att_chr]
  data_chr <- apply(data_chr, 2, as.character)
  data_chr <- as.data.frame(data_chr,stringsAsFactors=FALSE)
  
  data <- cbind(data_num,data_chr)
  return(data)
}

Data_Independent_Clean <- data
dim(data)
NC_Data.Assign_data_type(Data_Independent,Data_Attributes)
getwd()
setwd("W:/Analytics_Group_Files/US_files/Modeling_Process/Modeling_Process_Gamma/Data_Rda_Update")

save(Data_Independent_Clean,file="Data_Independent_Clean_20171010.rda")

head(Perf_final)
Perf_final$TTD <- Perf_final$LoanStatusToDate
# "A" "B" "C" "D" "G" "K" "L" "N" "NULL" "P" "R" "W"

Perf_final$FPD <- Perf_final$FirstPaymentDefault

levels(Perf_final$TTD) <- c("1","1","1","0","NA","NA","1","0","NA","0","1","NA")
Perf_final$TTD <- as.character(Perf_final$TTD)
Perf_final$PayInRatio <- round(as.numeric(as.character(Perf_final$TotInPayToDate)) / as.numeric(as.character(Perf_final$OriginatedAmount)),3)

save(Perf_final,file="Data_Dependent_Clean_20171010.rda")
table(NS01_c$Work_Phone_Time_Zone )

dim(c01)

str(c01)




install.packages("RODBC")
library(RODBC)

myconn <-odbcConnect("CONSRVSQL",uid="sven",pwd="Pa55word1!")


cn <- odbcDriverConnect(connection="Driver={SQL Server};server=CONSRVSQL;database=LMSMaster;trusted_connection=yes;")

dataSQLQueryEUR <- sqlQuery(cn, "select * from vResellerSalesAmountEUR")



