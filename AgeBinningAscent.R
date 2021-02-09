# kind of silly gradient ascent/descent stepwise function. 
# It doesn't *really* work but it kind of guides the age binning should we choose to do it. 
# Current suggested binning for age:  c(8,18,21,30,40)

# Let's calculate a thing to optimize - maybe an anova between groups?

library(Information)

group_difference = function(comparison_var, splitting_var, divisions = c(2,10,30,50)){
  # The goal here is to produce a single metric which can be used to maximize "meaningful difference" between categories
  
  
  # Binning function - could move out of the function if it seems useful to do so
  # Assumes that the binned inputs are part of a continuous or ordered discrete variable
  binner = function(input, div = divisions){
    
    for(i in 1:(length(div)+1)){
      
      if(is.na(input)){
        return(NA)
        
      }else if(i == (length(div) +1)){
        
        return(i)
        
      }else if(input <= div[i]){
        
        return(i)
        
      }

    }
    
  }
  
  
  splitting_var_binned = sapply(splitting_var, binner)
  
  
  #Assess differences across the bins
  #vtab = table(comparison_var, splitting_var_binned)
  
  # This creates a vector of proportions
  #diffvector = vtab[1,] / (vtab[1,] + vtab[2,])
  
  #difference_result = 0
  # 
  # for(i in 1:(length(diffvector)-1)){
  #   for(j in (i+1):length(diffvector)){
  #     
  #     difference_result = difference_result + sqrt(abs(diffvector[i] - diffvector[j])) 
  #     # the sqrt term is meant here to penalize groups in which there is a lot of difference between one group and the rest
  #     # and little difference between the others. 
  #     
  #   }
  # 
  #   return(difference_result)
  #   
  # }
  
  
  IV = create_infotables(data.frame("Split" = comparison_var, "Orig" = splitting_var, "Bin" = splitting_var_binned), y = "Split", parallel = FALSE)
  
  score = IV$Summary[1,2] - IV$Summary[2,2]
  
  return(score)
  
}


# Now for the actual gradient ascent:

age_descent = function(comparison_var, splitting_var, starting_divisions = c(5,15,40,60), step = 3, repetitions = 500){
  
  
  current_div = starting_divisions
  continue = T
  current_score = 1
  
  output_div = starting_divisions
  
  steps = data.frame(c(-step:step))
  
  
  #generate the list of possible steps
  #This creates a matrix where each row is one of the possible combos of -1 to 1 that any of the divisions might move in
  for(i in 1:length(starting_divisions)){
    steps[,i] = -step:step
  }
  
  move_list = expand.grid(steps)
  
  
  
  
  for(k in 1:repetitions){
    print(k)
    
    for(i in sample(1:nrow(move_list))){
      test_div = output_div + move_list[i,]
      
      score = group_difference(comparison_var, splitting_var, divisions = test_div)
      
      #print(current_score)
      #print(score)
      
      if(score < current_score){
        current_score = score
        output_div = test_div
        
        print(output_div)
        break
        
      }
      
      
    }
    
  }
  
  return(output_div)
}


age_descent(dataNewTrain$Survived, dataNewTrain$Imputed_Age)
