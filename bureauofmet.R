library(tidyverse)
bomdata <- read_csv("Data/BOM_data.csv")
bomdata
bomstation <- read_csv("Data/BOM_stations.csv")
bomstation
# Question 1
# seperate the minimum and the maximum
# filter to select station and related variables
# how many for min and max?  is it count()?
# from character to numeric

TMTM <- separate(bomdata,col=Temp_min_max, into = c("Temp_min","Temp_max"), sep = "/")
# above suggests the column needs tidying
# column is in character and needs to be numeric
filter(bomdata,Tem_Min,Tem_Max,Rainfall)
TMTM
filter(bomdata, Year == "1998")
filter(bomdata, Temp_min_max == "20/29.7")
bomdata %>% 
  separate(Temp_min_max,c("Tem_Min","Tem_Max"))%>%
  filter(Year == "1998")
TMTM
ncol(TMTM)
nrow(TMTM)
separate(bomdata,Temp_min_max,c("Tem_Min","Tem_Max"))
ncol(bomdata)
nrow(bomdata)
TMTM$Temp_min
filter(TMTM == "")
bomdata$Temp_min_max
filter(bomdata,Temp_min_max,"-/1-")
group_by(bomdata,Station_number)
summarise(bomdata,Station_number)

#if data was tidy you would
#bomdata%>%
#  separate(Temp_min_max,c("Tem_Min","Tem_Max"))%>%
#  filter(Tem_Min,Tem_Max,Rainfall)
# group_by(bomdata,Station_number)
# summarise(bomdata,Station_number =)
# question 2 mutate TempDif=Tem_Max-Tem_Min)
bomdata%>%
  separate(Temp_min_max,c("Tem_Min","Tem_Max"))%>%
  filter(Tem_Min!='-')%>%
  filter(Tem_Max!='-')%>%
  filter(Rainfall!='0')%>%
  group_by(Station_number)%>%
  summarise (n-n())

write_csv(bomdata)
#using Felix's solution
#Question 1: For each station, how many days have a minimum temperature, a maximum temperature and a rainfall measurement recorded?
 BOM_data <- read_csv("data/BOM_data.csv")
 
 bomdata

  BOM_data <- read_csv("data/BOM_data.csv") #Load data
separated_min_max <- separate(BOM_data, col = Temp_min_max, into = c('min', 'max'), sep = "/") #Separate temperatures into min and max
only_data <- separated_min_max %>% 
  filter(min != '-') %>% #Filter out min rows without value
  filter(max != '-') %>% #Filter out max rows without value
  filter(Rainfall != '0') %>% #Filter out rainfall
  group_by(Station_number) %>% #Group by Station numbers
  summarise(n = n()) #Count number of rows
write_csv(only_data, "results/question1.csv")

BOM_data <- read_csv("data/BOM_data.csv")
separated_min_max <- separate(BOM_data,Temp_min_max,c("min","max"))
only_data <- separated_min_max%>%
  filter(min != '-')
bomdata
TMTM%>%
  filter(Temp_min!='-',Temp_max!='-',Rainfall!=0)%>%
  group_by(Station_number)%>%
  summarise(n))




