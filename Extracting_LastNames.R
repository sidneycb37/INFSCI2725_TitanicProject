library(dplyr)
library(stringr)

#get lastname column and subset to common names
dataFull <- rbind(dataTest, within(dataTrain, rm(Survived)))

lastnameframe <- dataFull %>% 
  mutate(Lname= str_extract(Name, '^([^,]+)'), Family_size = SibSp + Parch +1)
  







