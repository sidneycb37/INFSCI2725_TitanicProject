library(Information)


IV <- create_infotables(data=dataNewTrain, y="Survived", parallel=FALSE)


print(IV$Summary, row.names=FALSE)
print(IV$Tables$N_OPEN_REV_ACTS, row.names=FALSE)

binning = function(input_var, divisions){
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
  
  
  var_binned = sapply(input_var, binner)
  return(var_binned)
}

table(dataNewTrain$Survived, binning(dataNewTrain$Imputed_Age, c(8,18,21,30,40)))
