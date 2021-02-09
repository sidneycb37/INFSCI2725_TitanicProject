for(i in 1:length(lastnameframe$Lname)){
  if(str_detect(lastnameframe$Lname[i], "-")){
    lastnameframe$Lname[i] <- str_split(lastnameframe$Lname[i], "-")[[1]][2]
  }
}  



#Functions
names_Vector_finder <- function(data) {
  names_vector <- vector(mode = "character", length = length(data[,1]))
  for(i in 1:length(names_vector)){
    names_vector[i] <- as.character(data$Name[data$PassengerId == i])
  }
  return(names_vector)
}

Single_LastName_Family <- function(input_list, family_counter, data) {
  
  #Step 1 get indices for each passenger by passenger id
  index_vector <- vector("numeric", length= length(input_list))
  for(i in 1:length(input_list)){
    index_vector[i] <- which(data$PassengerId ==i)
  }
  
  #Step 2 get the number of people with same last names for every passenger
  lname_sum_vector <- vector("numeric", length= length(input_list))
  for(i in 1:length(input_list)){
    lname_sum_vector[i] <- sum(data$Lname == data$Lname[index_vector[i]])
  }
  
  #Step 3 get a boolean for each passenger of whether the number of last names is equivalent to family size
  lastname_size_equality <- vector("logical", length= length(input_list))
  for(i in 1:length(input_list)){
    lastname_size_equality[i] <- lname_sum_vector[i] <= data$Family_size[index_vector[i]]
  }
  
  #Step 4 construct set of last names without duplicates
  subset_last_names <- vector("character")
  j <- 1
  for(i in 1:length(input_list)){
    if(lastname_size_equality[i]){
      subset_last_names[j] <- data$Lname[index_vector[i]]
      j <- j+1
    }
  }
  subset_last_names <- sort(unique(subset_last_names))
  for(i in subset_last_names){
    passengers_with_name <- data$PassengerId[data$Lname == i]
    for(j in passengers_with_name){
      input_list[j] <- family_counter
    }
    family_counter <- family_counter+1
  }
  return(list(input_list, family_counter))
}

Remaining_Last_Names <- function(input_list, data) {
  names_list<- vector("character")
  j <- 1
  index_vector <- vector("numeric", length= length(input_list))
  for(i in 1:length(input_list)){
    index_vector[i] <- which(data$PassengerId ==i)
  }
  for(i in 1:length(input_list)){
    if(is.null(input_list[[i]])){
      names_list[j] <- data$Lname[index_vector[i]]
      j <- j +1
    }
  }
  names_list <- sort(unique(names_list))
  return(names_list)
}

Splitting_Shared_names <- function(names_list, family_list, data, Family_counter){
  for(i in names_list){
    #subset the dataframe for just those names
    subset_data <- data %>%
      filter(Lname == i)
    last_name_sizes <- nrow(subset_data)
    family_sizes <- subset_data$Family_size
    unique_family_sizes <- unique(family_sizes)
    k <- 2
    right_number_to_parse <- vector('logical')
    right_number_to_parse[1] <- TRUE
    for(j in unique_family_sizes) {
      if(j==1){
        num_ones <- sum(family_sizes ==1)
      }
      else if(j >1){
        right_number_to_parse[k] <- sum(family_sizes==j) ==j
        k <- k+1
      }
    }
    parse_all_separately <- FALSE
    no_one_unique_family_sizes <- unique_family_sizes[unique_family_sizes!= 1]
    total_family_size_sum <- sum(no_one_unique_family_sizes) +num_ones
    print(all(right_number_to_parse))
    if(total_family_size_sum ==last_name_sizes &  all(right_number_to_parse)){
      parse_all_separately <- TRUE
    }
    if(parse_all_separately){
      for(j in unique_family_sizes){
        if(j ==1){
          one_member_families <- subset_data %>%
            filter(subset_data$Family_size ==1)
          for(k in 1:length(one_member_families$Lname)){
            passenger_ID <- one_member_families$PassengerId[k]
            family_list[[passenger_ID]] <- Family_counter
            Family_counter <- Family_counter+1
          }
        }
        else if (j > 1){
          multi_member_families <- subset_data %>%
            filter(subset_data$Family_size ==j)
          for(k in 1:length(multi_member_families$Lname)){
            passenger_ID <- multi_member_families$PassengerId[k]
            family_list[[passenger_ID]] <- Family_counter
          }
        }
      }
    }
  }
  return(list(family_list, Family_counter))
}


#Main
Family_list <- vector(mode="list", length= length(lastnameframe$PassengerId))
names(Family_list) <- names_Vector_finder(data=lastnameframe)
Family_counter <- 1
result <- Single_LastName_Family(input_list = Family_list, family_counter = Family_counter, data= lastnameframe)
Family_counter <- result[[2]]
Family_list <- result[[1]]
testframe$familyid <- NULL

Names_to_process <- vector("character")
Names_to_process <- Remaining_Last_Names(input_list = Family_list, data=lastnameframe)

result <- Splitting_Shared_names(names_list = Names_to_process, family_list = Family_list,data = lastnameframe, Family_counter = Family_counter)


#for(i in 1:length(Family_list)){
#  if(!is.null(Family_list[[i]])){
#    testframe$familyid[testframe$PassengerId==i] <- Family_list[[i]]
#    }
#}


