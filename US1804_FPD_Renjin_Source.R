
# This is the Source Code of the US1710_TTD_Model

US1804_FPD_Obj_List <- c("att","bin_num",
  "bin_chr","df_woe_bin_train",
  "final_var_names",
  "final_num_names",
  "final_chr_names",
  "mod","final_raw_names",
  "function_names",
  "Master_Function_List","Master_Tree_List",
  "FUNCTION_split_vector_str",
  "FUNCTION_assign_data_type",
  "FUNCTION_create_woe_value",
  "FUNCTION_predict_probability",
  "FUNCTION_calculate_score",
  "US1804_FPD_Obj_List",
  "US1804_FPD_pro_List")

US1804_FPD_pro_List <- c("data_mx_raw","data_mx_clean","data_mx_derive",
                             "i","data_woe","data_prob","derive")

#----
# High Cor Variables

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

FUNCTION_assign_data_type <- function(data,att){
  
  var_names <- colnames(data)
  att_num <- intersect(as.character(att[att$type_int==1,]$variable_names),var_names)
  att_chr <- intersect(as.character(att[att$type_char==1,]$variable_names),var_names)
  
  data_num <- data[,att_num]
  data_num <- apply(data_num, 2, as.numeric)
  data_num[is.na(data_num)] <- 0.01
  data_num <- as.data.frame(t(data_num),stringsAsFactors=FALSE)
  rownames(data_num) <- NULL
  data_chr <- data[,att_chr]
  data_chr[data_chr=="" | data_chr=="NULL"] <- "BLANK"
  data_chr[is.na(data_chr)] <- "BLANK"
  
  data <- cbind(data_num,data_chr)
  return(data)
}


# df_temp2_test=data_mx_derive
# bin_num=US1711_TTD_Envir$bin_num
# bin_chr=US1711_TTD_Envir$bin_chr
# df_woe_bin_train=US1711_TTD_Envir$df_woe_bin_train
# final_names=US1711_TTD_Envir$final_var_names
# final_num_names=US1711_TTD_Envir$final_num_names
# final_chr_names=US1711_TTD_Envir$final_chr_names


FUNCTION_create_woe_value <- function(df_temp2_test,bin_num,bin_chr,df_woe_bin_train,
                                  final_names,num_names,chr_names){
  apply_match <- function(woe.obj, x.vec){
    xwoe <- sapply(x.vec, function(z) return(woe.obj[which(names(woe.obj) == z)]))  
    return(as.numeric(xwoe))
  }
  Data.SMBin_numeric_apply <-  function(num_mx,bin_num){
    data_set = num_mx
    object_list = bin_num
    
    data_out = data.frame(matrix(0, dim(data_set)[1], dim(data_set)[2]))
    names(data_out) = names(data_set)
    colnames = colnames(data_set)
    
    for(i in 1:length(colnames)){
      x = colnames[i]
      variable <- as.numeric(unlist(data_set[x]))
      data_out[x] = cut( variable, breaks = object_list[[x]])
    }
    return(data_out)
  }
  Data.SMBin_character_apply <- function(chr_mx,bin_chr){
    data_set=chr_mx
    object_list=bin_chr
    
    data_set <- data_set[colnames(data_set)%in%names(object_list)]
    
    data_out = data.frame(matrix(0, dim(data_set)[1], dim(data_set)[2]))
    names(data_out) = names(data_set)
    colnames = colnames(data_set)
    
    for(i in 1:length(colnames)){
      x = colnames[i]
      if(is.null(object_list[[x]])==FALSE){
        data_out[x] = apply_match(object_list[[x]], data_set[x][,1]) 
      }else{
        data_out[x] = "NoBin"
      }
    }
    for(i in 1:dim(data_out)[2]){
      data_out[,i] = as.factor(data_out[,i])
    }
    return(data_out)
  }
  Data.Get_woe_apply <- function(df_woe_test,df_woe_bin_train){
    data_set=df_woe_test
    object_list=df_woe_bin_train
    
    data_set <- data_set[colnames(data_set)%in%names(object_list)]
    
    data_out = data.frame(matrix(0, dim(data_set)[1], dim(data_set)[2]))
    names(data_out) = names(data_set)
    colnames = colnames(data_set)
    
    # k=floor(seq(1,length(colnames),by=length(colnames)/7))
    
    for(i in 1:length(colnames)){
      # if(i%in%k==TRUE){
      #   print(paste(i,"of",length(colnames),"-",round(i/length(colnames)*100,2),"%"))
      # }
      x = colnames[i]
      data_out[x] = apply_match(object_list[[x]], data_set[x][,1])
    }
    return(data_out)
  }
  
  num_mx <- df_temp2_test[intersect(num_names,colnames(df_temp2_test))]
  df_bin_num_test <- Data.SMBin_numeric_apply(num_mx,bin_num)
  
  chr_mx <- df_temp2_test[intersect(chr_names,colnames(df_temp2_test))]
  df_bin_chr_test <- Data.SMBin_character_apply(chr_mx,bin_chr)
  
  df_data_test <- cbind(df_bin_num_test,df_bin_chr_test)
  final_names[final_names %in% names(df_data_test)==F]
  df_woe_test <- df_data_test[,final_names,drop=FALSE]
  
  df_woe_mx_test <- Data.Get_woe_apply(df_woe_test,df_woe_bin_train)
  
  return(df_woe_mx_test)
}

FUNCTION_predict_probability <- function(mod,data_woe){
  data_woe[is.na(data_woe)] <- 0
  data_prob = predict(mod, s="lambda.min", newx = as.matrix(data_woe), type = "response")
  return(data_prob)
}

FUNCTION_calculate_score <-function (prob_list,direction){
  if(direction == 1){
    prob_list <- prob_list
  }else{
    prob_list <- 1- prob_list
  }
  n <- length(prob_list)
  Factor <- 20/log(2)
  Offset <- 600-Factor*log(50)
  score <- NA
  prob_list <- as.data.frame(prob_list)
  score <- apply(prob_list,1, function(x) round(Offset+Factor*log(x/(1-x)),2))
  return (score)
}


