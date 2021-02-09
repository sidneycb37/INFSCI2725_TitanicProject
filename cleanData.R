## Sidney for titanic project



#install.packages("glmnet")
#install.packages("caret")
#install.packages("mice")
library(glmnet)
library(caret)
#rm(list=ls()) # Uncomment if this is not being loaed into a file with other scripts

setwd("D:/Lexar Backup/Grad School/Semester 2/Big Data/Project")
#setwd("D:/R Working Directory")

trainFileNameLoc = 'train.csv'
testFileNameLoc = 'test.csv'

dataTrain = read.csv(file=trainFileNameLoc,header=TRUE,sep=',')
dataTest = read.csv(file=testFileNameLoc,header=TRUE,sep=',')


######## Tom's Contribution fxns: AGE Predictive Mean Matching ########
library(mice)


##### Creating a Title Variable for use in Imputation #####

find_title = function(data){
  
  nameChanOld = match('name',tolower(names(data))) #name index
  title = 1:nrow(data) # create a blank vector to start placing your titles in
  
  for(dataInd in 1:nrow(data)){  
    if (grepl('Mrs', data[dataInd,nameChanOld]) | grepl('Mme', data[dataInd,nameChanOld]) ){
      title[dataInd] = "Mrs"
      
    } else if(grepl('Mr', data[dataInd,nameChanOld])) {
      title[dataInd] = "Mr"
      
    } else if(grepl('Miss', data[dataInd,nameChanOld]) | grepl('Mlle', data[dataInd,nameChanOld]) | grepl('Ms', data[dataInd,nameChanOld])) {
      title[dataInd] = "Miss"
      
    } else if(grepl('Master', data[dataInd,nameChanOld])){
      title[dataInd] = "Master"
      
    } else if(grepl('Dr', data[dataInd,nameChanOld])){
      title[dataInd] = "Dr" 
      
    } else if(grepl('Rev', data[dataInd,nameChanOld])){
      title[dataInd] = "Rev"
      
    } else{ title[dataInd] = "None"}
  }
  
  return(as.factor(title))
  
}

find_title(dataTrain)

### Remember to Attach Title as a variable at the end of the tables



##### Predictive mean matching (multiple imputation)#####


# The predictor matrix decides which variables are to be used in imputation
# Each row is a variable to be imputed and each column the variable to be used in imputation
# A 1 indicates that the column variable of that number will be used in the imputation of the variable on that row
# For example, a 1 at [6,3] for dataTrain indicates that Pclass would be used to impute Age

impute_age = function(data, imputedvars = c("Age"), excludedvars = c("PassengerId", "Survived", "Name", "Ticket", "Cabin")) {
  predictor_matrix = matrix(data = 0, nrow = ncol(data), ncol = ncol(data)) #initialize such that there are no predictions made
  
  
  imputed = match(imputedvars, colnames(data))
  # Select rows to do imputation on
  predictor_matrix[imputed,] = 1 # Currently, only Age is being imputed
  
  #Select Columns NOT to use during imputation
  
  excluded_columns = na.omit(match(excludedvars, names(data))) # na.omit necessary to remove NAs (non-matches) from the list of excluded columns
  
  predictor_matrix[,excluded_columns] = 0 #Tell Mice not to predict using the highly missing or irrelevant columns
  
  colnames(predictor_matrix) <- colnames(data)
  rownames(predictor_matrix) <- colnames(data)
  
  # Make a 0 diagonal to prevent a thing being imputed based on itself
  for(i in 1:nrow(predictor_matrix)){
    predictor_matrix[i,i] = 0
  }
  
  imp_multi <- mice(data, m = 5, predictorMatrix = predictor_matrix, method = "pmm")  # Impute missing values multiple times
  data_imp_multi_all <- complete(imp_multi,       # Store multiply imputed data
                                       "repeated",
                                       include = TRUE)
  
  age_imputed = apply(data_imp_multi_all[,c("Age.1", "Age.2", "Age.3", "Age.4", "Age.5")], 1, median )
  
  return(age_imputed)
}

#For insanity checks:

# impute_age(dataTest)

# View(data.frame(dataTrain[,c('Name')], impute_age(dataTrain), is.na(dataTrain$Age)))



# Cleaning data function
cleanData = function(data, bTrain) {
  data[data==""] <- NA # make sure all empty cells are "na"
  
  # find relevant header names
  passIdOld = match('passengerid',tolower(names(data)))
  outcomeChanOld = match('survived',tolower(names(data)))
  classChanOld = match('pclass',tolower(names(data)))
  nameChanOld = match('name',tolower(names(data)))
  sexChanOld = match('sex',tolower(names(data)))
  ageChanOld = match('age',tolower(names(data)))
  sibSpChanOld = match('sibsp',tolower(names(data)))
  parchChanOld = match('parch',tolower(names(data)))
  ticketChanOld = match('ticket',tolower(names(data)))
  fareChanOld = match('fare',tolower(names(data)))
  cabinChanOld = match('cabin',tolower(names(data)))
  embarkChanOld = match('embarked',tolower(names(data)))
  
  # create new dataframe
  # include the outcome channel only if this is a training set (doesn't exist for the testing set)
  if (bTrain == 1){
    dataNew = data.frame("Survived"=data[,outcomeChanOld],"Name"=data[,nameChanOld],"PassengerID"=data[,passIdOld],"PClass"=data[,classChanOld],"Female"=double(nrow(data)),"Male"=double(nrow(data)),"AgeBucket"=double(nrow(data)),"lowerDeck"=double(nrow(data)),"middleDeck"=double(nrow(data)),"upperDeck"=double(nrow(data)),"Embarked_S"=double(nrow(data)),"Embarked_C"=double(nrow(data)),"Embarked_Q"=double(nrow(data)))
  }
  else {
    dataNew = data.frame("Name"=data[,nameChanOld],"PassengerID"=data[,passIdOld],"PClass"=data[,classChanOld],"Female"=double(nrow(data)),"Male"=double(nrow(data)),"AgeBucket"=double(nrow(data)),"lowerDeck"=double(nrow(data)),"middleDeck"=double(nrow(data)),"upperDeck"=double(nrow(data)),"Embarked_S"=double(nrow(data)),"Embarked_C"=double(nrow(data)),"Embarked_Q"=double(nrow(data)))
  }
  # find relevant header names for new data set
  passId = match('passengerid',tolower(names(dataNew)))
  outcomeChan = match('survived',tolower(names(dataNew)))
  classChan = match('pclass',tolower(names(dataNew)))
  nameChan = match('name',tolower(names(dataNew)))
  femaleChan = match('female',tolower(names(dataNew)))
  maleChan = match('male',tolower(names(dataNew)))
  ageBucketChan = match('agebucket',tolower(names(dataNew)))
  sibSpChan = match('sibsp',tolower(names(dataNew)))
  
  embarkSChan = match('embarked_s',tolower(names(dataNew)))
  embarkCChan = match('embarked_c',tolower(names(dataNew)))
  embarkQChan = match('embarked_q',tolower(names(dataNew)))
  
  lowDeckChan = match('lowerdeck',tolower(names(dataNew)))
  middleDeckChan = match('middledeck',tolower(names(dataNew)))
  upDeckChan = match('upperdeck',tolower(names(dataNew)))
  
  # iterate through each sample
  for (dataInd in 1:nrow(data)) 
    {
    
    ########### AGE: first check if age exists ###########
    isNA = is.na(data[dataInd,ageChanOld])
    # estimate age if it is missing
    
    # 
    
    if (isTRUE(isNA)) {
      if (grepl('Mrs', data[dataInd,nameChanOld])){
        dataNew[dataInd,ageBucketChan] = 2; # Assume they are middle aged if "Mrs." title
        data[dataInd,ageChanOld] = 40;
      } else if(grepl('Mr', data[dataInd,nameChanOld])) {
        dataNew[dataInd,ageBucketChan] = 2; # Assume they are middle aged if "Mr." title
        data[dataInd,ageChanOld] = 40;
      } else if(grepl('Miss', data[dataInd,nameChanOld])) {
        dataNew[dataInd,ageBucketChan] = 0; # Assume they are young if "Miss" title
        data[dataInd,ageChanOld] = 10;
      } else if(grepl('Master', data[dataInd,nameChanOld])){
        dataNew[dataInd,ageBucketChan] = 0; # Assume they are young if "Master" title
        data[dataInd,ageChanOld] = 10;
      }
    }
    else {
      
      if(data[dataInd,ageChanOld]< 7) {
        dataNew[dataInd,ageBucketChan] = 0;
      }
      else if(data[dataInd,ageChanOld]< 15) {
        dataNew[dataInd,ageBucketChan] = 1;
      }
      if(data[dataInd,ageChanOld]< 50) {
        dataNew[dataInd,ageBucketChan] = 2;
      }
      else {
        dataNew[dataInd,ageBucketChan] = 3;
      }
    }
      # CONSIDER CHANGING OR EXPANDING THESE BUCKETS (ex. 15 yr old capabilities very different from 2 year old capabilities, yet in same bucket rn)
      
      
      ########### DECK: check if cabin exists ###########
      isNA = is.na(data[dataInd,cabinChanOld])
      # estimate deck if cabin assignment is missing
      if (isTRUE(isNA)) {
        if (grepl('1',data[dataInd, classChanOld])){
          dataNew[dataInd,upDeckChan] = 1; # assume that a first class passenger is on a higher deck
        }
        else if (grepl('2',data[dataInd, classChanOld])) {
          dataNew[dataInd,middleDeckChan] = 1; # assume that a second class passenger is on a middle deck
        }
        else if (grepl('3',data[dataInd, classChanOld])) {
          dataNew[dataInd,lowDeckChan] = 1;  # assume that a third class passenger is on a lower deck
        }
      }
      else { # Assign number value to each deck
        # assign decks A through B as upper decks, C through D as middle and all others lower
        if ( (grepl('A',data[dataInd, cabinChanOld])) || grepl('B',data[dataInd, cabinChanOld]) )
          {
          dataNew[dataInd,upDeckChan] = 1; 
        }
        else if ( (grepl('C',data[dataInd, cabinChanOld])) || (grepl('D',data[dataInd, cabinChanOld])) )
          {
            dataNew[dataInd,middleDeckChan] = 1; 
        }
        else
        {
          dataNew[dataInd,lowDeckChan] = 1;
        }
      }
      
      
      ########### EMBARKED: assign embarked status to number ###########
      isNA = is.na(data[dataInd,embarkChanOld])
      if (!(isNA)) {
        if (grepl('S',data[dataInd, embarkChanOld])){
          dataNew[dataInd,embarkSChan] = 1;
        }
        else if (grepl('C',data[dataInd, embarkChanOld])) {
          dataNew[dataInd,embarkCChan]=1; 
        }
        else if (grepl('Q',data[dataInd, embarkChanOld])) {
          dataNew[dataInd,embarkQChan]=1;
        }
      }
      else {
        dataNew[dataInd,embarkSChan] = 1; # If missing, Assign S since the majority of passengers embarked here
      } # intuition tells me this likely doesn't affect survival chance, but look at result and consider omitting
      
      ########### SEX: assign embarked sex to number ###########
      if (grepl('female',data[dataInd, sexChanOld])){
        dataNew[dataInd,femaleChan] = 1;
      }
      else if (grepl('male',data[dataInd, sexChanOld])) {
        dataNew[dataInd,maleChan] = 1; 
      }
    }
    
  
  # Tom's add-on section
  
  dataNew$Title = find_title(data)
  
  dataNew$Imputed_Age = impute_age(data = data.frame(data, "Title" = find_title(data)))
  
  ia = dataNew$Imputed_Age
  
  dataNew$Imputed_Binned_Age = as.factor(ifelse(ia < 8, "Child", ifelse(ia < 18, "Adolescent", ifelse(ia < 21, "Young Adult", ifelse(ia > 50, "Old", "Adult" )))))
  
  return(dataNew)
}



######## END OF DATA CLEANING ########


######### MODEL PREP #########
# clean data
dataNewTrain = cleanData(dataTrain,bTrain=1)
dataNewTest = cleanData(dataTest,bTrain=0)

# decide start of x data
startIdChanTrain = match('pclass',tolower(names(dataNewTrain)))
startIdChanTest = match('pclass',tolower(names(dataNewTest)))
outcomeChan = match('survived',tolower(names(dataNewTrain)))

# assign x and y data
xDataTrain = as.matrix((dataNewTrain[,startIdChanTrain:length(dataNewTrain)]))
yDataTrain = (dataNewTrain[,outcomeChan])
xDataTest = as.matrix((dataNewTest[,startIdChanTest:length(dataNewTest)]))
#yDataTest = (dataNewTest[,outcomeChan])

# standardize x features
xDataTrainScale = scale(xDataTrain)
xDataTestScale = scale(xDataTest)

##### LASSO REGRESSION #####
fit <-glmnet(x = xDataTrainScale, y = yDataTrain, alpha = 1) 
# different values of alpha return different estimators, alpha = 1 is the lasso.
plot(fit, xvar = "lambda")
lasso.pred <- predict(fit, newx = xDataTestScale)
lassoPred = lasso.pred[,ncol(lasso.pred)]
plot(round(lassoPred))
coefficientsLasso = coef(fit)



#### BINARY CLASSIFIER ####
objModel = train(xDataTrainScale, as.factor(yDataTrain),method='gbm');
summary(objModel)
binaryPrediction = predict(object=objModel, xDataTestScale, type='raw')


dataNewTest$lassoEstimatedSurvival = round(lassoPred)
dataNewTest$binaryEstimatedSurvival = binaryPrediction
#dataNewTest$lassoEstimatedSurvival = round(lassoPred)

write.csv(dataNewTest, file = "testTom.csv")
write.csv(dataNewTrain, file = "trainTom.csv")
