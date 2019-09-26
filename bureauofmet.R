library(tidyverse)
bomdata <- read_csv("Data/BOM_data.csv")
bomdata
bomstation <- read_csv("Data/BOM_stations.csv")
bomstation
# Question 1
# seperate the minimum and the maximum
# filter to select station and redataset to dervie a shortcut for the data set i.e. bomdata as well lated variables
# how many for min and max?  is it count()?
# from character to numeric
TMTM <- separate(bomdata,col=Temp_min_max, into = c("Temp_min","Temp_max"), sep = "/")
TMTM
#so far we have created variants out of the original dataset i.e.
#bomdata being the short cut as well as TMTM being the data set where
#we derived the two columns, out of the one to seperate the minimum
#and maximum temperatures
#STEP2 filter the data to keep only the rows that have minimum, maximum
#and rainfall
TMTM%>%
  filter(Temp_min!='-',Temp_max!='-',Rainfall!=0)
#above filter is filtering out - therefore we assume it has picked 
#up all values against all three.
TMTM%>%
  filter(Temp_min!='-',Temp_max!='-',Rainfall!=0)%>%
  group_by(Station_number)
#we can see there are 20 groups of station numbers
TMTM%>%
  filter(Temp_min!='-',Temp_max!='-',Rainfall!=0)%>%
  group_by(Station_number)%>%
  summarise(n = n())
#last line sums lists the 20 stations
# In summary the full answer for question one is below
TMTM%>%
  filter(Temp_min!='-',Temp_max!='-',Rainfall!=0)%>%
  group_by(Station_number)%>%
  summarise(n = n())

#QUESTION2 as.numeric() is the recommended in hint

TempMinMaxValues <- filter(TMTM,Temp_min!='-',Temp_max!='-',Rainfall!=0)
TempMinMaxValues
mutate(TempMinMaxValues,TDiff = as.numeric(Temp_max)-as.numeric(Temp_min))
#above created a new column TDiff to get the difference between Max and Min
#TempMinMaxValuesTempMinMaxValuesQuestion which month saw the lowest temperature difference
#use mean function to get average
#sort assending
TempMinMaxValues%>%
  mutate(TDiff = as.numeric(Temp_max)-as.numeric(Temp_min))%>%
  mutate(ave = mean(TDiff))%>%
  arrange(TDiff)%>%
  select(Station_number,Year,Month,TDiff,ave)
 
# I think above seems right however checking with others there are averages
# lower than 6.51 in the data set.  6.51 was in December 1998

CheckVariant <- TempMinMaxValues%>%
  mutate(TDiff = as.numeric(Temp_max)-as.numeric(Temp_min))%>%
  mutate(ave = mean(TDiff))%>%
  arrange(TDiff)%>% 
  select(Station_number,Year,Month,TDiff,ave)
CheckVariant
tail(CheckVariant)
head(CheckVariant)

#CheckVariant confirms something is not right as the tail of the dta set is
#also returning 6.51 changing to TDiff also 
TempMinMaxValues%>%
  mutate(TDiff = as.numeric(Temp_max)-as.numeric(Temp_min))%>%
 group_by(Month)%>%
  summarise(average = mean(TDiff)#Confused but now thinking question may be asking what is the average of TDiff
summarise(CheckVariant,AvTD=mean(TDiff))
#above returns an average of 7.36 still doesn't answer question though.
#Do we need to arrange by month and find the average of DIF per month.

#Dong Chen solution below which states, create a new column to derive the difference
# then collate by month
TempMinMaxValues%>%
  mutate(TDiff = as.numeric(Temp_max)-as.numeric(Temp_min))%>%
  group_by(Month)%>%
  summarise(Difference = mean(TDiff))%>%
  filter(Difference == min(Difference))

#QUESTION3
#Which states show the lowest average daily temperature difference



  


