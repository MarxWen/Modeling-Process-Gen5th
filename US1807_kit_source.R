
process_kit_woe <- function(data,
                            y,
                            kit,
                            kit_name){
  # Process a single kits with certain utility functions
  # This process is designed to quickly uniformly add on functions
  # It can be used to add certain transformation process, prediction process or aggregation process
  
  # Args:
  #   data: The data set with all variables the kit needed
  #   kit: The name of the kits to be executed and loaded in the environment
  
  # Returns:
  #   res: The original data set with additional result column 
  # data = data_mx
  # kit = kit_c01_total_historical_inquiries
  # kit_name = "kit_c01_total_historical_inquiries"
  
  raw_var_name <- kit$slot_var$raw_var_name
  der_var_name <- kit$slot_der$der_var_name
  func_tree <- kit$slot_fun
  objs_tree <- kit$slot_obj
  exe_seq <- kit$slot_seq
  simple_raw <- is.null(unlist(func_tree))
  
  exe_seq_res <- names(as.Node(exe_seq)$Get('level', traversal = "post-order")) # The sequen of execution
  # 
  # raw_var_name <- "c01_total_historical_inquiries"
  data_raw <- data[,raw_var_name] # The data set with all needed data
  
  len_exe = length(exe_seq_res) # How many functions would be executed
  data_process <- data_raw
  
  # i=1
  #--------- exe the queue ---------#
  for(i in 1:len_exe){
    exe_func_name <- exe_seq_res[i]
    temp_exe <- func_tree[exe_func_name][[1]]
    if(simple_raw==F){
      temp_exe_obj <- objs_tree[exe_func_name]
      data_process <- temp_exe(data_process,temp_exe_obj,y)
    }else{
      data_process <- data_process
    }
  }
  
  #-------- type & df ----------#
  if(simple_raw==F){
    res <- data_process[der_var_name]
    data_woe <- cbind(res,y)
    type_chr = is.character(data_process[,der_var_name])
  }else{
    res <- data_process
    data_woe <- as.data.frame(cbind(res,y))
    colnames(data_woe)[1] <- der_var_name
    type_chr = is.character(data_process)
  }
  
  #------- smbinning ---------#
  if(type_chr==T){
    data_woe[,der_var_name] <- as.factor(data_woe[,der_var_name])
    woe_res = smbinning.factor(data_woe,x=der_var_name,y="y",maxcat = 15)
  }else{
    data_woe[,der_var_name] <- as.numeric(data_woe[,der_var_name])
    woe_res <- smbinning(data_woe,x=raw_var_name,y="y")
  }
  
  
  #-------- add the additional woe function to the kit --------------#
  new_var_name <- paste("woe",der_var_name,sep="_")
  obj_woe <- list(type_chr=type_chr,new_var_name=new_var_name,ivout=woe_res)
  obj <- temp_exe_obj
  
  get_woe <- function(data,obj,y){
    
    type_chr <- obj$type_chr
    new_var_name <- obj$new_var_name
    ivout <- obj$ivout
    
    if(type_chr==T & length(ivout)>6){
      res <- smbinning.factor.gen(data,ivout,new_var_name)
    }else if(length(ivout)>6){
      res <- smbinning.gen(data,ivout,new_var_name)
    }else{
      res <- data
    }
    
    return(res)
  }
  
  #---------------------------- Change the slot_fun,slot_obj,slot_seq 
  new_var_name_function <- paste(new_var_name,"DER",sep="_")
  
  func_tree$get_woe <- get_woe
  names(func_tree)[length(names(func_tree))] <- new_var_name_function
  
  objs_tree$obj_woe <- obj_woe
  names(objs_tree)[length(names(objs_tree))] <- new_var_name_function
  
  exe_seq <- list(new_var_name_function,exe_seq)
  exe_seq$name <- new_var_name_function
  #------------------------------------------------------
  
  slot_var <- list(raw_var_name=unique(raw_var_name))
  slot_der <- list(der_var_name=new_var_name)
  
  names(unlist(func_tree))
  unique_function_names = unique(names(unlist(func_tree)))
  slot_fun <- unlist(func_tree)[unique_function_names]
  
  unique_object_names = unique(names(objs_tree))
  slot_obj <- objs_tree[unique_object_names]
  
  slot_seq <- exe_seq
  slot_seq$name <- new_var_name_function
  
  new_kit <- list(slot_var=slot_var,slot_der=slot_der,slot_fun=slot_fun,slot_obj=slot_obj,slot_seq=slot_seq)
  
  return(new_kit)
}


create_kit <- function(kit_group,
                       kit_name){
  
  # Combine a serises of kits into one
  # This function will merge all the inputs, inheritage and eliminate the duplicates
  
  # Args:
  #  kit_group: a list of kit objects with our standard naming convension
  
  # Returns:
  # kit_new: newly combined kit with all the objects combined
  
  kit_group
  len_group = length(kit_group)
  
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
  slot_seq$name <- kit_name
  
  new_kit <- list(slot_var=slot_var,
                  slot_der=slot_der,
                  slot_fun=slot_fun,
                  slot_obj=slot_obj,
                  slot_seq=slot_seq)
  return(new_kit)
}


execute_kit <- function(data,y,kit){
  
  # Execute a single kits with a input data set
  # The kits executed using all its arguments and objects according to the inhertage tree
  
  # Args:
  #   data: The data set with all variables the kit needed
  #   kit: The name of the kits to be executed and loaded in the environment
  
  # Returns:
  #   res: The original data set with additional result column 
  
  data = data
  
  raw_var_name <- kit$slot_var$raw_var_name
  der_var_name <-kit$slot_der$der_var_name
  func_tree <- kit$slot_fun
  objs_tree <- kit$slot_obj
  exe_seq <- kit$slot_seq
  
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
    raw_var_name <- c(raw_var_name,kit_group[[i]]$slot_var$raw_var_name)
    der_var_name <- c(der_var_name,kit_group[[i]]$slot_der$der_var_name)
    
    func_tree[[i]] <- kit_group[[i]]$slot_fun
    objs_tree[[i]] <- kit_group[[i]]$slot_obj
    
    exe_seq[[i]] <- kit_group[[i]]$slot_seq
    print(i)
  }  
  
  slot_var <- list(raw_var_name=unique(raw_var_name))
  slot_der <- list(der_var_name=unique(der_var_name))
  
  func_name_list = names(unlist(func_tree,recursive = FALSE))
  func_name_list = func_name_list[func_name_list%in%c("")==F & is.na(func_name_list)==F]
  unique_function_names = unique(func_name_list)
  slot_fun <- unlist(func_tree,recursive = FALSE)[unique_function_names]
  
  
  obj_name_list = names(unlist(objs_tree,recursive = FALSE))
  obj_name_list = obj_name_list[obj_name_list%in%c("")==F & is.na(obj_name_list)==F]
  unique_object_names = unique(obj_name_list)
  slot_obj <- unlist(objs_tree,recursive = FALSE)[unique_object_names]
  
  slot_seq <- exe_seq
  slot_seq$name <- kit_name
  
  new_kit <- list(slot_var=slot_var,slot_der=slot_der,slot_fun=slot_fun,slot_obj=slot_obj,slot_seq=slot_seq)
  
  return(new_kit)
}



# kit_c01_bank_account_zeros
slot_var <- list(raw_var_name=c("c01_bank_account_zeros"))
slot_der <- list(der_var_name=c("c01_bank_account_zeros"))
slot_fun <- list(NULL)
slot_obj <- list(NULL)
slot_seq <- list(c("c01_bank_account_zeros"))
slot_seq$name <- "c01_bank_account_zeros"
kit_c01_bank_account_zeros <- list(slot_var=slot_var,slot_der=slot_der,slot_fun=slot_fun,slot_obj=slot_obj,slot_seq=slot_seq)

# kit_c01_ofac_score
slot_var <- list(raw_var_name=c("c"))
slot_der <- list(der_var_name=c("c01_ofac_score"))
slot_fun <- list(NULL)
slot_obj <- list(NULL)
slot_seq <- list(c("c01_ofac_score"))
slot_seq$name <- "c01_ofac_score"
kit_c01_ofac_score <- list(slot_var=slot_var,slot_der=slot_der,slot_fun=slot_fun,slot_obj=slot_obj,slot_seq=slot_seq)

# kit_c01_total_historical_inquiries
slot_var <- list(raw_var_name=c(""))
slot_der <- list(der_var_name=c("c01_total_historical_inquiries"))
slot_fun <- list(NULL)
slot_obj <- list(NULL)
slot_seq <- list(c("c01_total_historical_inquiries"))
slot_seq$name <- "c01_total_historical_inquiries"
kit_c01_total_historical_inquiries <- list(slot_var=slot_var,slot_der=slot_der,slot_fun=slot_fun,slot_obj=slot_obj,slot_seq=slot_seq)

# kit_c01_ssn_first_last_name_count
slot_var <- list(raw_var_name=c("c01_ssn_first_last_name_count"))
slot_der <- list(der_var_name=c("c01_ssn_first_last_name_count"))
slot_fun <- list(NULL)
slot_obj <- list(NULL)
slot_seq <- list(c("c01_ssn_first_last_name_count"))
slot_seq$name <- "c01_ssn_first_last_name_count"
kit_c01_ssn_first_last_name_count <- list(slot_var=slot_var,slot_der=slot_der,slot_fun=slot_fun,slot_obj=slot_obj,slot_seq=slot_seq)


#######################################################
# > kit_c01_Der_info_valid  
slot_var <- list(raw_var_name=c("c01_bank_routing_valid","c01_drivers_license_invalid"))
slot_der <- list(der_var_name=c("c01_Der_info_valid"))
# "c01.bank_routing_valid","c01.drivers_license_invalid","c01.home_or_cell_phone_state_invalid"
c01_Der_info_valid_DER <- function(data,obj,y){
  
  # Derive variable which are all valid in 
  # "c01_bank_routing_valid","c01_drivers_license_invalid"
  logic1 <- data$c01_bank_routing_valid %in% c("true","TRUE",TRUE)
  logic2 <- data$c01_drivers_license_invalid %in% c("false","FALSE",FALSE)
  
  data$c01_Der_info_valid <- as.character(logic1 & logic2)
  
  return(data)
}
slot_fun <- list(c01_Der_info_valid_DER=c01_Der_info_valid_DER)
slot_obj <- list(NULL)

# slot_seq <- expand.grid(slot_der$der_var_name,slot_var$raw_var_name)
slot_seq <- list(c("c01_bank_routing_valid","c01_drivers_license_invalid"))
slot_seq$name <- "c01_Der_info_valid_DER"
kit_c01_Der_info_valid <- list(slot_var=slot_var,slot_der=slot_der,slot_fun=slot_fun,slot_obj=slot_obj,slot_seq=slot_seq)


#######################################################
# > kit_c01_Der_c_number_count
slot_var <- list(raw_var_name=c("c01_number_of_ssns_with_bank_account","c01_ssn_distinct_first_last_name_count","c01_ssn_first_appearance"))
slot_der <- list(der_var_name=c("c01_Der_c_number_count"))
# "c01.bank_routing_valid","c01.drivers_license_invalid","c01.home_or_cell_phone_state_invalid"
c01_Der_c_number_count_DER <- function(data,obj,y){
  
  # Range of count bigger than 2 
  # "c01.number_of_ssns_with_bank_account",
  # "c01.ssn_distinct_first_last_name_count",
  # For the "c01.ssn_first_appearance" is None
  
  logic1 <- as.numeric(data$c01_number_of_ssns_with_bank_account) > 2
  logic2 <- as.numeric(data$c01_ssn_distinct_first_last_name_count) > 3
  logic3 <- data$c01_ssn_first_appearance %in% c("true","TRUE",TRUE)
  
  data$c01_Der_c_number_count <- data$c01_ssn_first_appearance
  
  data$c01_Der_c_number_count[logic3] <- "fs"
  data$c01_Der_c_number_count[!logic3] <- "nfs"
  data$c01_Der_c_number_count[!logic3 & (logic1 | logic2)] <- "cb"
  
  return(data)
}

slot_fun <- list(c01_Der_c_number_count_DER=c01_Der_c_number_count_DER)
slot_obj <- list(NULL)

slot_seq <- list(c("c01_number_of_ssns_with_bank_account","c01_ssn_distinct_first_last_name_count","c01_ssn_first_appearance"))
slot_seq$name <- "c01_Der_c_number_count_DER"
kit_c01_Der_c_number_count <- list(slot_var=slot_var,slot_der=slot_der,slot_fun=slot_fun,slot_obj=slot_obj,slot_seq=slot_seq)


#######################################################
# > kit_c01_Der_age_in_years
slot_var <- list(raw_var_name=c("sys_ApplicationDate","c01_date_of_birth"))
slot_der <- list(der_var_name=c("c01_Der_age_in_years"))

c01_Der_age_in_years_DER <- function(data,obj,y){
  
  format1 <- "%m/%d/%Y"
  format2 <- "%Y-%m-%d"
  app <- as.Date(substr(data$sys_ApplicationDate,1,10),format="%Y-%m-%d")
  data$c01_Der_age_in_years <- floor(difftime(app,as.Date(substr(data$c01_date_of_birth,1,10),format=format2),units = "days")/365)
  data$c01_Der_age_in_years[is.na(data$c01_Der_age_in_years)] <- -0.01
  
  return(data)
}

slot_fun <- list(c01_Der_age_in_years_DER=c01_Der_age_in_years_DER)
slot_obj <- list(NULL)

slot_seq <- list(c("sys_ApplicationDate","c01_date_of_birth"))
slot_seq$name <- "c01_Der_age_in_years_DER"
kit_c01_Der_age_in_years <- list(slot_var=slot_var,slot_der=slot_der,slot_fun=slot_fun,slot_obj=slot_obj,slot_seq=slot_seq)


#######################################################
# > kit_c01_Der_days_to_next_payday
slot_var <- list(raw_var_name=c("sys_ApplicationDate","c01_date_of_next_payday"))
slot_der <- list(der_var_name=c("c01_Der_days_to_next_payday"))

c01_Der_days_to_next_payday_DER <- function(data,obj,y){
  
  format1 <- "%m/%d/%Y"
  format2 <- "%Y-%m-%d"
  app <- as.Date(substr(data$sys_ApplicationDate,1,10),format="%Y-%m-%d")
  data$c01_Der_days_to_next_payday = -difftime(app,as.Date(substr(data$c01_date_of_next_payday,1,10),format=format1),units = "days")
  data$c01_Der_days_to_next_payday[is.na(data$c01_Der_days_to_next_payday)] <- -0.01
  
  return(data)
}

slot_fun <- list(c01_Der_days_to_next_payday_DER=c01_Der_days_to_next_payday_DER)
slot_obj <- list(NULL)

slot_seq <- list(c("sys_ApplicationDate","c01_date_of_next_payday"))
slot_seq$name <- "c01_Der_days_to_next_payday_DER"
kit_c01_Der_days_to_next_payday <- list(slot_var=slot_var,slot_der=slot_der,slot_fun=slot_fun,slot_obj=slot_obj,slot_seq=slot_seq)


#######################################################
# > kit_c01_Der_days_to_last_activity
slot_var <- list(raw_var_name=c("sys_ApplicationDate","c01_date_of_last_activity"))
slot_der <- list(der_var_name=c("c01_Der_days_to_last_activity"))

c01_Der_days_to_last_activity_DER <- function(data,obj,y){
  
  format1 <- "%m/%d/%Y"
  format2 <- "%Y-%m-%d"
  app <- as.Date(substr(data$sys_ApplicationDate,1,10),format="%Y-%m-%d")
  data$c01_Der_days_to_last_activity <- difftime(app,as.Date(substr(data$c01_date_of_last_activity,1,10),format="%Y-%m-%d"),units = "days")
  data$c01_Der_days_to_last_activity[is.na(data$c01_Der_days_to_last_activity)] <- -0.01
  
  return(data)
}

slot_fun <- list(c01_Der_days_to_last_activity_DER=c01_Der_days_to_last_activity_DER)
slot_obj <- list(NULL)

slot_seq <- list(c("sys_ApplicationDate","c01_date_of_last_activity"))
slot_seq$name <- "c01_Der_days_to_last_activity_DER"
kit_c01_Der_days_to_last_activity <- list(slot_var=slot_var,slot_der=slot_der,slot_fun=slot_fun,slot_obj=slot_obj,slot_seq=slot_seq)


#######################################################
# > kit_c01_Der_days_since_last_seen_by_group
slot_var <- list(raw_var_name=c("sys_ApplicationDate","c01_last_seen_by_group"))
slot_der <- list(der_var_name=c("c01_Der_days_since_last_seen_by_group"))

c01_Der_days_since_last_seen_by_group_DER <- function(data,obj,y){
  
  format1 <- "%m/%d/%Y"
  format2 <- "%Y-%m-%d"
  app <- as.Date(substr(data$sys_ApplicationDate,1,10),format="%Y-%m-%d")
  data$c01_Der_days_since_last_seen_by_group <- difftime(app,as.Date(substr(data$c01_last_seen_by_group,1,10),format="%Y-%m-%d"),units = "days")
  data$c01_Der_days_since_last_seen_by_group[is.na(data$c01_Der_days_since_last_seen_by_group)] <- -0.01
  
  return(data)
}

slot_fun <- list(c01_Der_days_since_last_seen_by_group_DER=c01_Der_days_since_last_seen_by_group_DER)
slot_obj <- list(NULL)

slot_seq <- list(c("sys_ApplicationDate","c01_last_seen_by_group"))
slot_seq$name <- "c01_Der_days_since_last_seen_by_group_DER"
kit_c01_Der_days_since_last_seen_by_group <- list(slot_var=slot_var,slot_der=slot_der,slot_fun=slot_fun,slot_obj=slot_obj,slot_seq=slot_seq)


#######################################################
# > kit_c01_Der_days_since_last_seen_by_location
slot_var <- list(raw_var_name=c("sys_ApplicationDate","c01_last_seen_by_location"))
slot_der <- list(der_var_name=c("c01_Der_days_since_last_seen_by_location"))

c01_Der_days_since_last_seen_by_location_DER <- function(data,obj,y){
  
  format1 <- "%m/%d/%Y"
  format2 <- "%Y-%m-%d"
  app <- as.Date(substr(data$sys_ApplicationDate,1,10),format="%Y-%m-%d")
  data$c01_Der_days_since_last_seen_by_location <- difftime(app,as.Date(substr(data$c01_last_seen_by_location,1,10),format="%Y-%m-%d"),units = "days")
  data$c01_Der_days_since_last_seen_by_location[is.na(data$c01_Der_days_since_last_seen_by_location)] <- -0.01
  
  return(data)
}

slot_fun <- list(c01_Der_days_since_last_seen_by_location_DER=c01_Der_days_since_last_seen_by_location_DER)
slot_obj <- list(NULL)

slot_seq <- list(c("sys_ApplicationDate","c01_last_seen_by_location"))
slot_seq$name <- "c01_Der_days_since_last_seen_by_location_DER"
kit_c01_Der_days_since_last_seen_by_location <- list(slot_var=slot_var,slot_der=slot_der,slot_fun=slot_fun,slot_obj=slot_obj,slot_seq=slot_seq)


#######################################################
# > kit_c01_Der_days_since_last_seen_by_account
slot_var <- list(raw_var_name=c("sys_ApplicationDate","c01_last_seen_by_account"))
slot_der <- list(der_var_name=c("c01_Der_days_since_last_seen_by_account"))

c01_Der_days_since_last_seen_by_account_DER <- function(data,obj,y){
  
  format1 <- "%m/%d/%Y"
  format2 <- "%Y-%m-%d"
  app <- as.Date(substr(data$sys_ApplicationDate,1,10),format="%Y-%m-%d")
  data$c01_Der_days_since_last_seen_by_account <- difftime(app,as.Date(substr(data$c01_last_seen_by_account,1,10),format="%Y-%m-%d"),units = "days")
  data$c01_Der_days_since_last_seen_by_account[is.na(data$c01_Der_days_since_last_seen_by_account)] <- -0.01
  
  return(data)
}

slot_fun <- list(c01_Der_days_since_last_seen_by_account_DER=c01_Der_days_since_last_seen_by_account_DER)
slot_obj <- list(NULL)

slot_seq <- list(c("sys_ApplicationDate","c01_last_seen_by_account"))
slot_seq$name <- "c01_Der_days_since_last_seen_by_account_DER"
kit_c01_Der_days_since_last_seen_by_account <- list(slot_var=slot_var,slot_der=slot_der,slot_fun=slot_fun,slot_obj=slot_obj,slot_seq=slot_seq)


#######################################################
# > kit_c01_Der_days_since_last_purchased
slot_var <- list(raw_var_name=c("sys_ApplicationDate","c01_last_purchased"))
slot_der <- list(der_var_name=c("c01_Der_days_since_last_purchased"))

c01_Der_days_since_last_purchased_DER <- function(data,obj,y){
  
  format1 <- "%m/%d/%Y"
  format2 <- "%Y-%m-%d"
  app <- as.Date(substr(data$sys_ApplicationDate,1,10),format="%Y-%m-%d")
  data$c01_Der_days_since_last_purchased <- difftime(app,as.Date(substr(data$c01_last_purchased,1,10),format="%Y-%m-%d"),units = "days")
  data$c01_Der_days_since_last_purchased[is.na(data$c01_Der_days_since_last_purchased)] <- -0.01
  
  return(data)
}

slot_fun <- list(c01_Der_days_since_last_purchased_DER=c01_Der_days_since_last_purchased_DER)
slot_obj <- list(NULL)

slot_seq <- list(c("sys_ApplicationDate","c01_last_purchased"))
slot_seq$name <- "c01_Der_days_since_last_purchased_DER"
kit_c01_Der_days_since_last_purchased <- list(slot_var=slot_var,slot_der=slot_der,slot_fun=slot_fun,slot_obj=slot_obj,slot_seq=slot_seq)


