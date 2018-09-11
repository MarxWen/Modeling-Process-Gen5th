# This is a demo of the modeling process using kit OO System
library(MASS)
library(partykit)
library(klaR)
library(glmnet)
library(Formula)
library(data.tree)
library(smbinning)



deve_wd <- "W:/Analytics_Group_Files/US_files/2018_ProjectFiles/US1805_Modeling_Process_Development/Development Code"
setwd(deve_wd)
source("US1807_kit_source.r")


raw_wd <- "\\\\Consrvdc/company_data/Analytics_Group_Files/US_files/2018_ProjectFiles/US1807_Unpack_Date_Set"
setwd(raw_wd)
Data_Independent <- get(load("US20180628_Clean_Raw_Data_WF_One.rda"))
Data_Dependent <- get(load("Perf_final_20180628.rda"))

dependent <- "FPD"
data_mx <- merge(Data_Independent,Data_Dependent,by="MRID")
data_mx = data_mx[data_mx$FPD %in% c("1","0"),]
data_mx$sys_ApplicationDate <- as.character(data_mx$ScoredApplicationDate)

y <- data_mx[,dependent]
data <- data_mx

dim(data)
length(y)
head(data)
ls()
700 + 680 + 480 + 800 + 720+ 730 - 3850
260 + 300


process_kit <- function(kit_group,
                        kit_name,
                        process){

}

kit = kit_c01_Der_days_since_last_seen_by_group

execute_kit <- function(data,
                        kit){
  
  # Execute a single kits with a input data set
  # The kits executed using all its arguments and objects according to the inhertage tree
  
  # Args:
  #   data: The data set with all variables the kit needed
  #   kit: The name of the kits to be executed and loaded in the environment
  
  # Returns:
  #   res: The original data set with additional result column 
  #---------------------------------------------------------------------------
  
  #------ Read Kit ------#
  # Read contents into operation environment
  slot_var <- kit$slot_var
  slot_der <- kit$slot_der
  slot_fun <- kit$slot_fun
  slot_obj <- kit$slot_obj
  slot_seq <- kit$slot_seq
  
  # Predefine variables to prepare for execution
  raw_var_name <- slot_var$raw_var_name
  der_var_name <- slot_der$der_var_name
  exe_func_tree <- slot_fun
  exe_objs_tree <- slot_obj
  exe_seqs_tree <- slot_seq
  
  # Translate the inheritage tree to name vector
  tn <- as.Node(exe_seqs_tree)
  exe_seqs_name <- names(tn$Get('level', traversal = "post-order")) 
  exe_seqs_name %in% names(exe_func_tree)
  
  # Check the data has all the fields needed
  if(is.element("FALSE",raw_var_name%in%names(data))){
    stop("Input data doesn't contain all necessary variables")
  }
  
  ### To be developed
  
  #------ Execute Kit ------#
  data_exe <- data[,raw_var_name]
  len_exe = length(exe_seqs_name)
  
  if(len_exe==1 & is.null(exe_func_tree[exe_seqs_name[1]][[1]])){
    res <- data
  }else{
  for(i in 1:len_exe){
    exe_func_name <- exe_seqs_name[i] # The name of the function being execute
    exe_func_body <- exe_func_tree[exe_func_name][[1]] # Assign the function body
    exe_func_objs <- exe_func_objs[exe_func_name][[1]] # Assign the objects needed
    
    exe_have_objs <- is.null(exe_func_objs) # Does the function need obj
    
    data_exe = exe_func_body(data_exe,exe_func_objs)
  }
    res <- data_exe[der_var_name]
  }
  
  return(res)
}

process_kit_innocent <- function(data,
                                 var_list){
  
  # This function will auto translate every single variable into a kit
  # These kit would have empty func and objs with one level of inhertage
  
  len_var_list = length(var_list)
  
  for(i in 1:len_var_list){
    
    var_name = var_list[i]
    slot_var <- list(raw_var_name=var_name)
    slot_der <- list(der_var_name=var_name)
    slot_fun <- list(NULL)
    slot_obj <- list(NULL)
    slot_seq <- list(var_name)
    slot_seq$name <- var_name
    kit_innocent <- list(slot_var=slot_var,slot_der=slot_der,slot_fun=slot_fun,slot_obj=slot_obj,slot_seq=slot_seq)
    assign(paste("kit",var_name,sep = "_"),kit_innocent)
    
  }
  
  kit_group_names <- paste("kit",var_list,sep = "_")
  
  return(kit_group_names)
}

parallel_assemble_kit <- function(kit_group,
                       kit_name){
  
  # Combine a serises of kits into one
  # This functino will merge all the inputs, inheritage and eliminate the duplicates
  
  # Args:
  #  kit_group: a list of kit objects with our standard naming convension
  
  # Returns:
  # kit_new: newly combined kit with all the objects combined

  len_group = length(kit_group)
  
  raw_var_name <- NULL
  der_var_name <- NULL
  exe_func_tree <- list()
  exe_objs_tree <- list()
  exe_seqs_tree <- list()
  
  for(i in 1:len_group){
    kit=kit_group[[i]]
    
    #------ Read Kit ------#
    # Read contents into operation environment
    slot_var <- kit$slot_var
    slot_der <- kit$slot_der
    slot_fun <- kit$slot_fun
    slot_obj <- kit$slot_obj
    slot_seq <- kit$slot_seq
    
    # Predefine variables to prepare for execution
    # Add on new variables to assemble
    raw_var_name <- c(raw_var_name,slot_var$raw_var_name)
    der_var_name <- c(der_var_name,slot_der$der_var_name)
    exe_func_tree[[i]] <- slot_fun
    exe_objs_tree[[i]] <- slot_obj
    exe_seqs_tree[[i]] <- slot_seq
  }
  
  #------ Reduce Duplicated ------#
  func_tree_names = names(unlist(exe_func_tree,recursive = FALSE))
  func_tree_names_nonempty = func_tree_names[!func_tree_names%in%c("")]
  func_tree_names_nonempty_unique = unique(func_tree_names_nonempty)
  objs_tree_names_nonempty_unique = func_tree_names_nonempty_unique
  # Find the unique and nonempty function names and later filter
  
  slot_var <- list(raw_var_name=unique(raw_var_name))
  slot_der <- list(der_var_name=unique(der_var_name))
  slot_fun <- unlist(exe_func_tree,recursive = FALSE)[func_tree_names_nonempty_unique]
  slot_obj <- unlist(exe_objs_tree,recursive = FALSE)[objs_tree_names_nonempty_unique]
  slot_seq <- exe_seqs_tree
  slot_seq$name <- kit_name
  
  new_kit <- list(slot_var=slot_var,
                  slot_der=slot_der,
                  slot_fun=slot_fun,
                  slot_obj=slot_obj,
                  slot_seq=slot_seq)
  return(new_kit)
}

vertical_assemble_kit <- function(kit,
                                kit_name,
                                kit_fit){
  
  # This function will merge TWO kit in a inheritage fashion
  # The input kit should first run through the parallel assemble
  # Iuput should be restricted to only one
  
  raw_var_name <- NULL
  der_var_name <- NULL
  exe_func_tree <- list()
  exe_objs_tree <- list()
  exe_seqs_tree <- list()
  
  #------ Read Kit ------#
  # Read contents into operation environment
  slot_var_low <- kit$slot_var
  slot_der_low <- kit$slot_der
  slot_fun_low <- kit$slot_fun
  slot_obj_low <- kit$slot_obj
  slot_seq_low <- kit$slot_seq
  
  slot_var_up <- kit_fit$slot_var
  slot_der_up <- kit_fit$slot_der
  slot_fun_up <- kit_fit$slot_fun
  slot_obj_up <- kit_fit$slot_obj
  slot_seq_up <- kit_fit$slot_seq
  
  # Predefine variables to prepare for execution
  raw_var_name <- slot_var_low$raw_var_name
  der_var_name <- slot_der_up$der_var_name
  exe_func_tree[[1]] <- slot_fun_low
  exe_func_tree[[2]] <- slot_fun_up
  exe_objs_tree[[1]] <- slot_obj_low
  exe_objs_tree[[2]] <- slot_obj_up
  exe_seqs_tree <- slot_seq_low
  exe_seqs_tree$name <- kit_name
  
  # Check the data has all the fields needed
  if(is.element("FALSE",slot_var_up$raw_var_name%in%slot_der_low$der_var_name)){
    stop("Input kit doesn't contain all necessary variables for fit assemble")
  }
  
  #------- Assemble the new kit -------#
  slot_var <- list(raw_var_name=unique(raw_var_name))
  slot_der <- list(der_var_name=unique(der_var_name))
  slot_fun <- unlist(exe_func_tree,recursive = FALSE)[func_tree_names_nonempty_unique]
  slot_obj <- unlist(exe_objs_tree,recursive = FALSE)[objs_tree_names_nonempty_unique]
  slot_seq <- exe_seqs_tree
  
  new_kit <- list(slot_var=slot_var,
                  slot_der=slot_der,
                  slot_fun=slot_fun,
                  slot_obj=slot_obj,
                  slot_seq=slot_seq)
  return(new_kit)
}

get_woe_obj <- function(data,y,x_var_name){
  
  # Type = "chr" means it's a character variable
  # Type = "num" means it's a numeric variable
  # If non of these two, meaning the variable violate certain assumptions
  # colnames(data)
  # x_var_name <- "ns01_X1st_Phone_Validation"
  
  #----- Determind what type ------#
  data_woe <- cbind(y,data[x_var_name])
  
  if(sapply(data_woe, is.numeric)[2]){
    type = "num"
  }else if(sapply(data_woe, is.character)[2]|sapply(data_woe, is.factor)[2]){
    type = "chr"
  }else{
    type = "non"
  }
  
  #------ rules set by the smbinning package -----#
  if(type=="num" & length(unique(data_woe[,2]))<5){
    type = "num_less_five"
  }
  if(type=="chr" & length(unique(data_woe[,2]))<2){
    type = "chr_one_level"
  }
  
  #------ Use smbinning to get the woe ------#
  if(type=="chr"){
    data_woe[,x_var_name] <- as.factor(data_woe[,x_var_name])
    woe_res = smbinning.factor(data_woe,x=x_var_name,y="y",maxcat = 15)
  }else if(type=="num"){
    woe_res <- smbinning(data_woe,x=x_var_name,y="y")
  }else{
    woe_res <- NULL
  }
  
  if(length(names(woe_res))<5){
    type <- woe_res[1]
    woe_res <- NULL
  }
  
  woe_obj = list(woe_res=woe_res,type=type,x_var_name=x_var_name)
  return(woe_obj)
}

prd_woe_obj <- function(data,woe_obj){
  
  # This function will use the calculated woe object to predict woe
  # Main usage is to be inbedded into the vertical assemble process
  
  #----- Read the objects in the output list -----#
  woe_res <- woe_obj$woe_res
  type <- woe_obj$type
  x_var_name <- woe_obj$x_var_name
  x_der_name <- paste("woe",x_var_name,sep="_")
  data_woe2 <- data[,x_var_name]
  
  if(is.null(woe_res)==F){
    ivout=woe_res
    ivout$col_id <- which(colnames(data)==x_var_name)[1]
  }else if(type == "chr"){
    data[,x_var_name] <- as.factor(data[,x_var_name])
    data_bin <- data
    res <- smbinning.factor.gen(data_bin,ivout,x_der_name)
    
    ???????????
    ???????????
    ???????????
    
    function (df=data_bin, ivout, chrname = x_der_name) 
    {
      df = cbind(df, tmpname = NA)
      ncol = ncol(df)
      col_id = ivout$col_id
      df[, ncol][is.na(df[, col_id])] = 0
      if (is.null(ivout$groups)) {
        b = ivout$cuts
        for (i in 1:length(b)) {
          df[, ncol][df[, col_id] == b[i]] = i
        }
      }
      else {
        for (i in 1:length(ivout$groups)) {
          gelements = as.list(strsplit(as.character(gsub("'","", ivout$groups[i])), ",")[[1]])
          df[, ncol][df[, col_id] %in% gelements] = i
        }
      }
      df[, ncol] = as.factor(df[, ncol])
      if (is.null(ivout$groups)) {
        blab = c(paste("01 =  '", b[1], "'"))
        for (i in 2:length(b)) {
          blab = c(blab, paste(sprintf("%02d", i), "=  '", 
                               b[i], "'"))
        }
      }
      else {
        blab = c(paste("01 in (", ivout$groups[1], ")"))
        for (i in 2:length(ivout$groups)) {
          blab = c(blab, paste(sprintf("%02d", i), "in (", 
                               ivout$groups[i], ")"))
        }
      }
      if (any(is.na(df[, col_id]))) {
        blab = c("00 Miss", blab)
      }
      blab = gsub(" '", "'", blab)
      blab = gsub("' ", "'", blab)
      df[, ncol] = factor(df[, ncol], labels = blab)
      names(df)[names(df) == "tmpname"] = chrname
      return(df)
    }
    
    
  }else if(type == "num"){
    res <- smbinning.gen(data,ivout,x_var_name)
  }else{
    res <- data
  }
  
  return(res)
}


kit = kit_B19051_002E

woe_assemble_kit <- function(data,y,x_var_name,
                             kit,kit_name){
  
  # This kit is a combo of a series of kits
  # The goal of it is to calculate the woe objects and assemble that vertically to a existing kit
  
  # 0. use the execute_kit to create the variable needed
  # 1. use the get_woe_obj to train the object
  # 2. use the prd_woe_obj to generate function which run the object
  # 3. use the vertical_assmeble_kit to add woe into one kit
  
  data_local <- execute_kit(data,kit)
  x_var_name = kit$slot_der$der_var_name
  woe_obj <- get_woe_obj(data,y,x_var_name)
  
  
  temp_woe_kit
  
  prd_woe_obj
  
  var_name = var_list[i]
  slot_var <- list(raw_var_name=x_var_name)
  slot_der <- list(der_var_name=paste("woe_",x_var_name,sep="_"))
  slot_fun <- list(prd_woe_obj=prd_woe_obj)
  names(slot_fun) <- paste("woe",x_var_name,sep="_")
  slot_obj <- list(woe_obj=woe_obj)
  names(slot_obj) <- paste("woe",x_var_name,sep="_")
  slot_seq <- list(var_name)
  slot_seq$name <- var_name
  kit_innocent <- list(slot_var=slot_var,slot_der=slot_der,slot_fun=slot_fun,slot_obj=slot_obj,slot_seq=slot_seq)
  assign(paste("kit",var_name,sep = "_"),kit_innocent)
  
  
  
  
}





kit_c01_Der_info_valid
as.Node(slot_seq)
as.Node(kit_c01_bank_routing_valid$slot_seq)
as.Node(kit_training_20180808$slot_seq)
as.Node(kit_c01_Der_info_valid$slot_seq)
### First process_kit_innocent ###
var_list = c("B19051_002E","c01_drivers_license_invalid")
colnames(data)
created_kit_name_list <- process_kit_innocent(data,var_list)
kit_group <- list(kit_B19051_002E,kit_c01_drivers_license_invalid)

kit_name <- "training_20180808"
kit_training_20180808 <- parallel_assemble_kit(kit_group,kit_name)

kit_training_20180808$slot_der
as.Node(kit_training_20180808$slot_seq)
kit_name <- "kit_c01_Der_info_valid"
kit_fit <- kit_c01_Der_info_valid
kit_c01_Der_info_valid <- vertical_assemble_kit(kit=kit_training_20180808,
                                                kit_name,
                                                kit_fit)




process_kit_woe <- function(data,
                            var_list){
  
  
  
}






var_list <- c("c01_total_historical_inquiries","c01_ssn_first_last_name_count",
              "c01_bank_account_zeros")





kit_c01_bank_account_zeros <- process_kit_woe(data,y,
                                              kit_c01_bank_account_zeros,
                                              "kit_c01_bank_account_zeros")


kit_c01_Der_info_valid <- process_kit_woe(data,y,
                                          kit_c01_Der_info_valid,
                                          "kit_c01_Der_info_valid")

as.Node(kit_c01_Der_info_valid$k5)

kit_group <- list(kit_c01_ssn_first_last_name_count,
                  kit_c01_total_historical_inquiries,
                  kit_c01_ofac_score,
                  kit_c01_bank_account_zeros,
                  
                  kit_c01_Der_info_valid,
                  kit_c01_Der_c_number_count,
                  kit_c01_Der_age_in_years,
                  kit_c01_Der_days_to_next_payday,
                  kit_c01_Der_days_to_last_activity,
                  kit_c01_Der_days_since_last_seen_by_group,
                  kit_c01_Der_days_since_last_seen_by_location,
                  kit_c01_Der_days_since_last_seen_by_account,
                  kit_c01_Der_days_since_last_purchased)

for(i in 1:length(kit_group)){
  kit_group[i] <- a
    
}

kit_group

kit_name <- "New_kit_demo"


create_kit <- function(kit_group,
                       kit_name){
  
  # Combine a serises of kits into one
  # This functino will merge all the inputs, inheritage and eliminate the duplicates
  
  # Args:
  #  kit_group: a list of kit objects with our standard naming convension
  
  # Returns:
  # kit_new: newly combined kit with all the objects combined
  
  #----------- Assign empty slots for the new kit object -------------#
  len_group = length(kit_group)
  raw_var_name <- NULL
  der_var_name <-NULL
  func_tree <- list()
  objs_tree <- list()
  exe_seq <- list()
  
  # i=4
  #---------- Combine all function needs to be executed in all kits into one ----------------#
  for(i in 1:len_group){
    raw_var_name <- c(raw_var_name,kit_group[[i]]$k1$raw_var_name)
    der_var_name <- c(der_var_name,kit_group[[i]]$k2$der_var_name)
    
    func_tree[[i]] <- kit_group[[i]]$k3
    objs_tree[[i]] <- kit_group[[i]]$k4
    
    exe_seq[[i]] <- kit_group[[i]]$k5
    print(i)
  }  
  
  k1 <- list(raw_var_name=unique(raw_var_name))
  k2 <- list(der_var_name=unique(der_var_name))
  
  func_name_list = names(unlist(func_tree,recursive = FALSE))
  func_name_list = func_name_list[func_name_list%in%c("")==F & is.na(func_name_list)==F]
  unique_function_names = unique(func_name_list)
  k3 <- unlist(func_tree,recursive = FALSE)[unique_function_names]
  
  obj_name_list = names(unlist(objs_tree,recursive = FALSE))
  obj_name_list = obj_name_list[obj_name_list%in%c("")==F & is.na(obj_name_list)==F]
  unique_object_names = unique(obj_name_list)
  k4 <- unlist(objs_tree,recursive = FALSE)[unique_object_names]
  
  k5 <- exe_seq
  k5$name <- kit_name
  
  new_kit <- list(k1=k1,k2=k2,k3=k3,k4=k4,k5=k5)
  
  return(new_kit)
}


new_kit = create_kit(kit_group,kit_name)
as.Node(new_kit$slot_seq)

execute_kit <- function(data,y,kit){
  
  # Execute a single kits with a input data set
  # The kits executed using all its arguments and objects according to the inhertage tree
  
  # Args:
  #   data: The data set with all variables the kit needed
  #   kit: The name of the kits to be executed and loaded in the environment
  
  # Returns:
  #   res: The original data set with additional result column 
  
  data = data
  kit=new_kit
  raw_var_name <- kit$k1$raw_var_name
  der_var_name <-kit$k2$der_var_name
  func_tree <- kit$k3
  objs_tree <- kit$k4
  exe_seq <- kit$k5
  
  tn <- as.Node(exe_seq)
  
  exe_seq_res <- names(tn$Get('level', traversal = "post-order")) # The sequen of execution
  data_raw <- data[,unlist(raw_var_name)] # The data set with all needed data
  len_exe = length(exe_seq_res)-1 # How many functions would be executed
  data_process <- data_raw
  
  # i=5
  
  for(i in 1:len_exe){
    exe_func_name <- exe_seq_res[i]
    temp_exe <- func_tree[exe_func_name][[1]]
    simple_raw <- is.null(unlist(temp_exe))
    if(simple_raw==F){
      temp_exe_obj <- objs_tree[[exe_func_name]]
      data_process <- temp_exe(data_process,temp_exe_obj,y)
    }else{
      data_process <- data_process
    }
    print(i)
  }
  
  res <- data_process[der_var_name]
  return(res)
}


new_kit_res = execute_kit(data,y,new_kit)
as.Node(new_kit$k5)







#####################











