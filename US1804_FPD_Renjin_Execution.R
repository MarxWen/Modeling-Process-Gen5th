setwd("W:/Analytics_Group_Files/US_files/UnderWriting_Records/US1801_Default_Series_Model_Documentation/US1801_FPD_V2/Renjin Files")
setwd("W:/Analytics_Group_Files/US_files/UnderWriting_Records/US1801_Default_Series_Model_Documentation/US1801_FPD_V2/Renjin Files Alt_B")

# save(df_xxx,file = "df_xxx.Rda")
library(partykit)
library(klaR)
library(glmnet)
library(Formula)

source("US1804_FPD_Renjin_Source.R")

att <- get(load("US1804_FPD_att.Rda"))
bin_num <- get(load("US1804_FPD_bin_num.Rda"))
bin_chr <- get(load("US1804_FPD_bin_chr.Rda"))
df_woe_bin_train <- get(load("US1804_FPD_df_woe_bin_train.Rda"))
final_var_names <- get(load("US1804_FPD_final_var_names.Rda"))
final_num_names <- get(load("US1804_FPD_final_num_names.Rda"))
final_chr_names <- get(load("US1804_FPD_final_chr_names.Rda"))
mod <- get(load("US1804_FPD_mod.Rda"))
final_raw_names <- get(load("US1804_FPD_final_raw_names.Rda"))
function_names <- get(load("US1804_FPD_function_names.Rda"))
Master_Function_List <- get(load("US1804_FPD_Master_Function_List.Rda"))
Master_Tree_List <- get(load("US1804_FPD_Master_Tree_List.Rda"))

US1804_FPD_Envir <- as.environment(as.list(globalenv(),all.names=TRUE)[US1804_FPD_Obj_List])
rm(list=(US1804_FPD_Obj_List))

# 
# setwd("W:/Analytics_Group_Files/US_files/2017_ProjectFiles/US1708_Renjin_Envrionment_Design/US1708_FPD_Model_Edition2/Renjin")
# 
# final_var_names2 <- get(load("US1711_FPD_final_var_names.Rda"))
# final_num_names2 <- get(load("US1711_FPD_final_num_names.Rda"))
# final_chr_names2 <- get(load("US1711_FPD_final_chr_names.Rda"))
# mod2 <- get(load("US1711_FPD_mod.Rda"))
# final_raw_names2 <- get(load("US1711_FPD_final_raw_names.Rda"))
# 
# final_var_names2[final_var_names2 %in% final_var_names == F]
# final_var_names2[final_var_names2 %in% final_var_names == T]
# final_var_names[final_var_names %in% final_var_names2 == F]
# 
# coef(mod)
# coef(mod2)

#############################################################################################

sample_str_long <- "sys.ApplicationDate~2017-11-06T10:24:11Z;c01.bank_account_zeros~0;c01.date_of_birth~1991-04-13;c01.date_of_last_activity~;c01.date_of_next_payday~2017-11-17;c01.last_purchased~;c01.last_seen_by_account~;c01.last_seen_by_group~;c01.last_seen_by_location~;c01.net_monthly_income~4875;c06.call7370x~44;c06.cbcc6160x~400;c06.cbcc7110x~98;c06.ciln8220x~98;c06.ciqt9413x~0;c06.ciqt9416x~2;c06.ciqt9417x~4;c06.clear_bureau_lite_score~843;c06.vantage_reason_codes~TS,BP,RT,T1;c06.vantage_score~501;d02.AchReturns_days14~NULL;d02.AchReturns_days180~NULL;d02.AchReturns_days90~NULL;d02.AchReturns_years1~NULL;d02.AchReturns_years3~NULL;d02.BankAccountABA_days60~1;d02.BankAccountABA_years1~1;d02.BankAccountABA_years7~1;d02.BankAccountByEmail_days60~1;d02.BankAccountByEmail_years1~2;d02.BankAccountByEmail_years7~2;d02.HomeAddressByBankAccount_days180~1;d02.HomeAddressByBankAccount_years3~1;d02.HomeAddressByBankAccount_years7~1;d02.LastChargeOffDate~;d02.LastInquiryDate~;d02.LastPaymentAmount~;d02.LastPaymentDate~;d02.LastPaymentDisposition~;d02.LastReturnDate~;d02.LastTradelineDate~;d02.MaximumTotalPrincipal~0;d02.MaximumTotalPrincipalPaidOff~0;d02.SecondLastPaymentAmount~;d02.SecondLastPaymentDate~;d02.SecondLastPaymentDisposition~NULL;d02.ThirdLastPaymentAmount~;d02.ThirdLastPaymentDate~;d02.ThirdLastPaymentDisposition~;d02.TotalPaidOffs~0;d04.nap_summary~12;ns01.Phone_1___Phone_In_Service_Indicator~A7;ns01.Phone_1___Prepaid_Phone_Attribute~Y;ns01.Phone_1___Service_Discontinued_Indicator~7;ns01.Phone_2___Phone_In_Service_Indicator~NULL;ns01.X1st_Phone_Score~0;c01.drivers_license_invalid~false;c01.housing_status~2;c06.call1360x~4;c06.cbcx5320x~4250;c06.clear_bureau_lite_reason_codes~BL09,BL07,BL05,BL03;c06.score_x_reason_codes~63,61,81,40;d02.ACHDebitAttempts_years7~0;d02.ApplicationInquiries_days180~0;d02.ApplicationInquiries_days90~0;d02.ApplicationInquiries_years2~0;d02.LastPaymentType~;d02.PaymentsCompleted_days30~0;d02.PaymentsCompleted_years3~0;d02.SecondLastPaymentType~;d02.SsnByBankAccount_days30~1;d02.ThirdLastPaymentType~;d02.VendorInquiries_days180~0;d02.VendorInquiries_days30~0;d02.VendorInquiries_days90~0;d02.VendorInquiries_years2~0;d02.WorkPhoneByBankAccount_days30~1;d02.WorkPhoneByBankAccount_years1~1;d02.WorkPhoneByBankAccount_years7~1;d04.nap_status~;d04.nap_type~P;ns01.Phone_2___Phone_Type_Indicator~NULL;ns01.Phone_2___Service_Discontinued_Indicator~NULL;ns01.X1st_Email_Score~85;ns01.X1st_Phone_Active~0;ns01.X1st_Phone_BPI~U;ns01.X1st_Phone_Connected_in_DA~U;ns01.X1st_Phone_Mobile~;ns01.X2nd_Phone_Active~1;ns01.X2nd_Phone_BPI~U;ns01.X2nd_Phone_Connected_in_DA~U;ns01.X2nd_Phone_Mobile~Y;d02.HomeAddressByBankAccount_days30~1;d02.HomeAddressByBankAccount_years1~1;d02.HomeAddressByBankAccount_years7~1;c06.call7360x~52;c06.cbcc7120x~99;c06.cbcx5320x~4250;c06.cbrc7140x~98;c06.crev5020x~4456;ns01.Phone_2___Phone_In_Service_Indicator~NULL;ns01.X1st_Phone_Score~0;ns01.X2nd_Phone_Time_Zone~6;c06.ciln8220x~98;d02.ApplicationInquiries_days180~0;d02.ApplicationInquiries_days30~0;d02.ApplicationInquiries_days90~0;d02.ApplicationInquiries_years2~0;d02.HomeAddressByBankAccount_days30~1;d02.HomeAddressByBankAccount_years1~1;d02.VendorInquiries_days180~0;d02.VendorInquiries_days30~0;d02.VendorInquiries_days90~0;d02.VendorInquiries_years2~0;d04.nap_type~P;ns01.Phone_1___Phone_In_Service_Indicator~A7;ns01.Phone_1___Service_Discontinued_Indicator~7;ns01.X1st_Email_Score~85"
sample_str_long <- "sys.ApplicationDate~2017-11-13T15:55:51Z;c01.bank_account_zeros~0;c01.date_of_birth~1972-04-28;c01.date_of_last_activity~2017-10-16T03:31:12Z;c01.date_of_next_payday~2017-12-04;c01.last_purchased~;c01.last_seen_by_account~;c01.last_seen_by_group~;c01.last_seen_by_location~;c01.net_monthly_income~6000;c06.call7370x~19;c06.cbcc6160x~60;c06.cbcc7110x~118;c06.ciln8220x~137;c06.ciqt9413x~1;c06.ciqt9416x~2;c06.ciqt9417x~3;c06.clear_bureau_lite_score~873;c06.vantage_reason_codes~RT,BP,TF,T1;c06.vantage_score~515;d02.AchReturns_days14~NULL;d02.AchReturns_days180~NULL;d02.AchReturns_days90~NULL;d02.AchReturns_years1~NULL;d02.AchReturns_years3~NULL;d02.BankAccountABA_days60~1;d02.BankAccountABA_years1~1;d02.BankAccountABA_years7~2;d02.BankAccountByEmail_days60~1;d02.BankAccountByEmail_years1~1;d02.BankAccountByEmail_years7~2;d02.HomeAddressByBankAccount_days180~1;d02.HomeAddressByBankAccount_years3~1;d02.HomeAddressByBankAccount_years7~1;d02.LastChargeOffDate~;d02.LastInquiryDate~2017-11-03;d02.LastPaymentAmount~428.75;d02.LastPaymentDate~2011-12-27;d02.LastPaymentDisposition~COMPLETE;d02.LastReturnDate~;d02.LastTradelineDate~2011-11-28;d02.MaximumTotalPrincipal~350;d02.MaximumTotalPrincipalPaidOff~350;d02.SecondLastPaymentAmount~;d02.SecondLastPaymentDate~;d02.SecondLastPaymentDisposition~NULL;d02.ThirdLastPaymentAmount~;d02.ThirdLastPaymentDate~;d02.ThirdLastPaymentDisposition~;d02.TotalPaidOffs~1;d04.nap_summary~12;ns01.Phone_1___Phone_In_Service_Indicator~A7;ns01.Phone_1___Prepaid_Phone_Attribute~N;ns01.Phone_1___Service_Discontinued_Indicator~0;ns01.Phone_2___Phone_In_Service_Indicator~NULL;ns01.X1st_Phone_Score~0;c01.drivers_license_invalid~false;c01.housing_status~;c06.call1360x~13;c06.cbcx5320x~3100;c06.clear_bureau_lite_reason_codes~BL09,BL10,BL04,BL05;c06.score_x_reason_codes~40,63,36,11;d02.ACHDebitAttempts_years7~1;d02.ApplicationInquiries_days180~1;d02.ApplicationInquiries_days90~1;d02.ApplicationInquiries_years2~1;d02.LastPaymentType~ACH;d02.PaymentsCompleted_days30~0;d02.PaymentsCompleted_years3~0;d02.SecondLastPaymentType~;d02.SsnByBankAccount_days30~1;d02.ThirdLastPaymentType~;d02.VendorInquiries_days180~1;d02.VendorInquiries_days30~1;d02.VendorInquiries_days90~1;d02.VendorInquiries_years2~1;d02.WorkPhoneByBankAccount_days30~1;d02.WorkPhoneByBankAccount_years1~1;d02.WorkPhoneByBankAccount_years7~1;d04.nap_status~;d04.nap_type~U;ns01.Phone_2___Phone_Type_Indicator~NULL;ns01.Phone_2___Service_Discontinued_Indicator~NULL;ns01.X1st_Email_Score~85;ns01.X1st_Phone_Active~0;ns01.X1st_Phone_BPI~U;ns01.X1st_Phone_Connected_in_DA~U;ns01.X1st_Phone_Mobile~;ns01.X2nd_Phone_Active~1;ns01.X2nd_Phone_BPI~C;ns01.X2nd_Phone_Connected_in_DA~U;ns01.X2nd_Phone_Mobile~Y;d02.HomeAddressByBankAccount_days30~1;d02.HomeAddressByBankAccount_years1~1;d02.HomeAddressByBankAccount_years7~1;c06.call7360x~19;c06.cbcc7120x~125;c06.cbcx5320x~3100;c06.cbrc7140x~136;c06.crev5020x~8966;ns01.Phone_2___Phone_In_Service_Indicator~NULL;ns01.X1st_Phone_Score~0;ns01.X2nd_Phone_Time_Zone~7;c06.ciln8220x~137;d02.ApplicationInquiries_days180~1;d02.ApplicationInquiries_days30~1;d02.ApplicationInquiries_days90~1;d02.ApplicationInquiries_years2~1;d02.HomeAddressByBankAccount_days30~1;d02.HomeAddressByBankAccount_years1~1;d02.VendorInquiries_days180~1;d02.VendorInquiries_days30~1;d02.VendorInquiries_days90~1;d02.VendorInquiries_years2~1;d04.nap_type~U;ns01.Phone_1___Phone_In_Service_Indicator~A7;ns01.Phone_1___Service_Discontinued_Indicator~0;ns01.X1st_Email_Score~85"


data$New_Score <- 0
for( j in 1995:dim(data)[1]){
# j=2
  # j=1851
vec <- as.character(df_mx[j])
sample_str_long <- substr(vec,21,nchar(vec)-2)
data_mx_raw <- US1804_FPD_Envir$FUNCTION_split_vector_str(sample_str_long)
data_mx_clean <- US1804_FPD_Envir$FUNCTION_assign_data_type(data_mx_raw,US1804_FPD_Envir$att)
data_mx_derive <- data_mx_clean

i=11
for(i in 1:length(US1804_FPD_Envir$function_names)){
 
  derive <- US1804_FPD_Envir$Master_Function_List[US1804_FPD_Envir$function_names[i]][[1]]
  data_mx_derive <- derive(data_mx_derive,US1804_FPD_Envir$Master_Tree_List)
 
  print(i)
}
data_woe <- US1804_FPD_Envir$FUNCTION_create_woe_value(data_mx_derive,US1804_FPD_Envir$bin_num,US1804_FPD_Envir$bin_chr,
                                  US1804_FPD_Envir$df_woe_bin_train,US1804_FPD_Envir$final_var_names,
                                  US1804_FPD_Envir$final_num_names,US1804_FPD_Envir$final_chr_names)
sum(is.na(data_woe))
data_woe[is.na(data_woe)] <- 0
data_prob <- US1804_FPD_Envir$FUNCTION_predict_probability(US1804_FPD_Envir$mod,data_woe)
US1804_FPD_score <- 0
US1804_FPD_score  <- US1804_FPD_Envir$FUNCTION_calculate_score(data_prob,0)
# rm(list=(US1804_FPD_Envir$US1804_FPD_pro_List))

data$New_Score[j] <- US1804_FPD_score
print(j)
}

US1804_FPD_Envir$FUNCTION_create_woe_value
##

(data$New_Score,as.numeric(as.character(data$Sresult)))








ns01.Phone_1___Prepaid_Phone_Attribute
length(data_woe)
final_var_names[final_var_names %in% colnames(data_woe)==F]
length(df_woe_mx_test[j,final_var_names])
final_var_names%in%colnames(df_woe_mx_test[j,final_var_names])

 if(sum(data_woe[final_var_names] == df_woe_mx_test[j,final_var_names])!= 49){
     print(sum(data_mx_derive[final_var_names] == df_temp2_test[j,final_var_names]))
   }

}
df_woe_mx_test


US1804_FPD_Envir$df_woe_bin_train$ns01.X2nd_Phone_Validation
df_woe_bin_train$ns01.X2nd_Phone_Validation




hist(df_xxx$c06.vantage_score)


dim(df_xxx)


function(df_temp2_test,bin_num,bin_chr,df_woe_bin_train,
         final_names,num_names,chr_names){
  
  
  data_mx_derive
  # data_mx_derive,US1804_FPD_Envir$bin_num,US1804_FPD_Envir$bin_chr,
  # US1804_FPD_Envir$df_woe_bin_train,US1804_FPD_Envir$final_var_names,
  # US1804_FPD_Envir$final_num_names,US1804_FPD_Envir$final_chr_names
  num_names <- final_num_names
  chr_names <- final_chr_names
  final_names <- final_var_names
  data_mx_derive
  num_mx <- data_mx_derive[intersect(num_names,colnames(data_mx_derive))]
  df_bin_num_test <- Data.SMBin_numeric_apply(num_mx,bin_num)
  
  chr_mx <- data_mx_derive[intersect(chr_names,colnames(data_mx_derive))]
  df_bin_chr_test <- Data.SMBin_character_apply(chr_mx,bin_chr)
  
  df_data_test <- cbind(df_bin_num_test,df_bin_chr_test)
  final_names[final_names %in% names(df_data_test)==F]
  df_woe_test <- df_data_test[,final_var_names,drop=FALSE]
  
  df_woe_mx_test <- Data.Get_woe_apply(df_woe_test,df_woe_bin_train)
  
  return(df_woe_mx_test)
}
df_woe_test_local <- df_woe_test
df_woe_mx_test_local <- df_woe_mx_test

length(df_woe_test_local[26,])

length(df_woe_test)


length(df_woe_mx_test_local[26,])

length(df_woe_mx_test)

rbind(data_woe,df_woe_mx_test_local[27,])




