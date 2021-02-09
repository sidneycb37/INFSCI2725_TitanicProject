library(dplyr)
library(stringr)

#get lastname column and subset to common names
dataFull <- rbind(dataTest, within(dataTrain, rm(Survived)))
  
commonNames <- dataFull %>% 
  mutate(Lname= str_extract(Name, '^([^,]+)'), Family_size = SibSp + Parch +1) %>% 
  group_by(Lname) %>% 
  filter(n() != Family_size) %>% 
  select(one_of(c("Lname", "Embarked", "Ticket", "Family_size", "Sex", "Age", "Pclass")))

commonNames_sameTicket <- commonNames %>% 
  group_by(Ticket) %>% 
  filter(n() >1) %>% 
  filter(n() > Family_size)

#Briefly explored these manually as it was less work than making algorithmic searches


#I quickly found 6 sets of passengers who had suspicious similarities (e.g. consecutive or identical ticket numbers)
#Questionable_Families is a dataset with just these 6 last names
Questionable_Families <- dataFull %>% 
  mutate(Lname= str_extract(Name, '^([^,]+)'), Family_size = SibSp + Parch +1) %>% 
  filter(Lname == "Watt" | Lname == "Cor" | Lname == "Ware" | Lname ==  "Carrau" | Lname ==  "Lam" | Lname == "Risien")

#What follows is an online investigation into the veracity of the 0's in the family-related columns 

##Watt: appears to be a mislabel (mother and daughter)
#https://www.encyclopedia-titanica.org/titanic-survivor/bessie-watt.html

##Cor: Three people from the same small city in Croatia (200-300 people in ~1910-1920) all purchased from same swiss office
#e.g. https://www.encyclopedia-titanica.org/titanic-victim/bartol-cor.html
#https://bs.wikipedia.org/wiki/Kri%C4%8Dina

##Ware: Definite mistake, asymmetric relation on the Spouse/Siblings column and titanica confirms the marriage
#https://www.encyclopedia-titanica.org/titanic-victim/john-james-ware.html
#https://www.encyclopedia-titanica.org/titanic-survivor/florence-louise-ware.html

##Carrau: Related but potentially only nephew-uncle or cousin-cousin
#https://www.encyclopedia-titanica.org/titanic-victim/francisco-carrau.html

##Lam: Seems reasonable, two Lams same ticket but titanica explains 8 sailor colleagues shared a ticket here 
##(also Lam is a super common chinese last name and both were from Hong Kong)
#https://www.encyclopedia-titanica.org/titanic-survivor/ali-lam.html

##Risien: Confirmed mislabel, Beard and Emma were married, with that old tradition of marrying a late wife's sister...
#https://www.encyclopedia-titanica.org/titanic-victim/emma-risien.html
#https://www.encyclopedia-titanica.org/titanic-victim/samuel-beard-risien.html
