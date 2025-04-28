#  Note learn dput
### DW marat# installing/loading the latest installr package:
## install.packages("installr") # install installr
# library(installr) #load

# install.packages("installr"); library(installr) # install+load installr

# updateR() # updating R.hon results app

#  GUI for Shiny ggplot - need totry this

#install.packages("devtools")
#devtools::install_github("gertstulp/ggplotgui")
#library("ggplotgui")



#library(shiny) see already loaded below
#library(esquisse)
#install.packages("esquisse")
#-----------LIBRARY LOADING---------------Install
library(magrittr)
library(ggformula)
library(plyr)
library(ggplot2)
library(reshape2)
library(rsconnect)
library(stringr)
library(ggpubr)
library(shiny)
library(shinythemes)
library(tidyverse) # group of packages for data wrangling and visualisation (ggplot2)
library(lubridate) # convert date to R date format
library(DT) # Data Tables package for interactive html tables
library(RColorBrewer)
#library(esquisse)

WinTime_data<- read.csv("Winning Time Profiles.csv") %>%
  select(Year, Distance, Y_2000, Y_1975, Y_1979, Y_1980, Y_1994, Y_2023, Y_1989, Y_1978, Y_1977, Y_1981, Y_1984, Y_2011F, Y_2011)

#--------------Retirements data-------------
Retire_data <- read.csv("Retirements0817.csv") %>%
  select(Year, a_Pewsey, b_Hungerford, c_Newbury, d_Aldermaston, e_Reading, f_Marsh, g_Marlow, h_Bray, i_OldWindsor, j_Shepperton, k_Teddington, l_Tideway) %>%
  pivot_longer(-Year, names_to = "Result")

Retire_datatable <- read.csv("Retirements0817.csv") %>%
  select(Year, a_Pewsey, b_Hungerford, c_Newbury, d_Aldermaston, e_Reading, f_Marsh, g_Marlow, h_Bray, i_OldWindsor, j_Shepperton, k_Teddington, l_Tideway)


#--------------Race Numbers data-------------
RND_data <- read.csv("DWNumbers.csv") %>%
  select(Year, Doubles, Doubles_Finish, Doubles_Retired, Junior_Doubles, Junior_Doubles_Finish, Junior_Doubles_Retired, Singles, Singles_Finish, Singles_Retired, VetJunior, VetJunior_Finish, VetJunior_Retired, Endeavour, Endeavour_Finish, Endeavour_Retired) %>%
  pivot_longer(-Year, names_to = "Result")
#  arrange(Year)

RND_datatable <- read.csv("DWNumbers.csv") %>%
  select(Year, Doubles, Doubles_Finish, Doubles_Retired, Junior_Doubles, Junior_Doubles_Finish, Junior_Doubles_Retired, Singles, Singles_Finish, Singles_Retired, VetJunior, VetJunior_Finish, VetJunior_Retired, Endeavour, Endeavour_Finish, Endeavour_Retired)





#--------------Read SplitTime data-------------

SplitTimeData <- read.csv("FirstPlaceTimesk.csv") %>%
#SplitTimeData <- read.csv("FirstPlaceTimesj.csv") %>%
  select(Distance, Crew, Time, Flow, Year, Clock, CheckPoint, Speed, SplitTimePoint, SplitSpeed, SpeedIncrease, Section)

Sptest2 <- read.csv("TestPlot2.csv") %>%
  select(Distance, Crew, Time, Flow, Year, Clock, CheckPoint, Speed, SplitTimePoint, SplitSpeed, SpeedIncrease, Section)

Sptest3 <- read.csv("TestPlot3.csv") %>%
  select(Distance, Crew, Time, Flow, Year, Clock, CheckPoint, Speed, SplitTimePoint, SplitSpeed, SpeedIncrease, Section)

# 2014testdataDEC20.csv

DW2017S <- read.csv("DWSplitFullMod.csv") %>%
  select(Year, Place, Crew, a_Pewsey, b_Hungerford, c_Newbury, d_Aldermaston, e_Reading, f_Marsh, g_Marlow, h_Bray, i_OldWindsor, j_Shepperton, k_Teddington, l_Westminster, s_Canal, s_River, s_Thames, s_Tideway, RaceTime, TidewayTime, PewseyTime) %>%
  pivot_longer(-c(Place, Year, Crew, RaceTime, TidewayTime, PewseyTime), names_to = "TimePoint")

DW2017STime <- read.csv("DWSplitFullMod.csv") %>%
  #mutate(A_Devizes = 0) %>%
  mutate(B_Pewsey = B_Pewsey-A_Devizes) %>%
  mutate(C_Hungerford = C_Hungerford-A_Devizes) %>%
  mutate(D_Newbury= D_Newbury-A_Devizes) %>%
  mutate(E_Aldermaston = E_Aldermaston-A_Devizes) %>%
  mutate(F_Reading= F_Reading-A_Devizes) %>%
  mutate(G_Marsh= G_Marsh-A_Devizes) %>%
  mutate(H_Marlow= H_Marlow-A_Devizes) %>%
  mutate(I_Bray= 	I_Bray-A_Devizes) %>%
  mutate(J_OldWindsor = J_OldWindsor-A_Devizes) %>%
  mutate(	K_Shepperton= K_Shepperton-A_Devizes) %>%
  mutate(L_Teddington= L_Teddington-A_Devizes) %>%
  mutate(M_Westminster = M_Westminster-A_Devizes) %>%
  select(Year, Place, Crew, 	B_Pewsey,	C_Hungerford,	D_Newbury,	E_Aldermaston,	F_Reading,	G_Marsh,	H_Marlow,	I_Bray,	J_OldWindsor,	K_Shepperton,	L_Teddington,	M_Westminster, TidewayTime, PewseyTime, RaceTime) %>%
  pivot_longer(-c(Place, Year, Crew, RaceTime, TidewayTime, PewseyTime), names_to = "TimePoint")


  
 # A_Devizes,	B_Pewsey,	C_Hungerford,	D_Newbury,	E_Aldermaston,	F_Reading,	G_Marsh,	H_Marlow,	I_Bray,	J_OldWindsor,	K_Shepperton,	L_Teddington,	M_Westminster

  
DW2017SN <- read.csv("DWSplitFullMod.csv") %>% select(Year, RaceTime, TidewayTime, PewseyTime,NewburyTime,ReadingTime,MarlowTime,OldWindsorTime,SheppertonTime,TeddingtonTime)


DW2017ST <- read.csv("DWSplitFullMod.csv") %>%
  select(Year, Boat, a_Pewsey, b_Hungerford, c_Newbury, d_Aldermaston, e_Reading, f_Marsh, g_Marlow, h_Bray, i_OldWindsor, j_Shepperton, k_Teddington, l_Westminster, Crew, Place)
colnames(DW2017ST) <- c("Year", "Boat", "Pewsey", "Hungerford", "Newbury", "Aldermaston", "Reading", "Marsh", "Marlow", "Bray", "OldWindsor", "Shepperton", "Teddington", "Westminster", "Crew", "Place")

DW2017STT <- read.csv("DWSplitFullMod.csv") %>%
  select(Year, Boat, Devizes, Pewsey, Hungerford, Newbury, Aldermaston, Reading, Marsh, Marlow, Bray, OldWindsor, Shepperton, Teddington, Westminster, Crew, Place)


DW2017STDev <- read.csv("DWSplitFullMod.csv") %>%
  select(Year, A_Devizes, B_Pewsey, C_Hungerford, D_Newbury, E_Aldermaston, F_Reading, G_Marsh, H_Marlow, I_Bray, J_OldWindsor, K_Shepperton, L_Teddington, M_Westminster, Crew, Place) %>%
  pivot_longer(-c(Place, Year, Crew), names_to = "TimePoint")

DW2017STDevAll <- read.csv("DWSplitFullMod.csv") %>%
  select(Year, A_Devizes, B_Pewsey, D_Newbury,  F_Reading, H_Marlow, J_OldWindsor, K_Shepperton, L_Teddington, M_Westminster, Crew, Place) %>%
  pivot_longer(-c(Place, Year, Crew), names_to = "TimePoint")

DWHighTimeA <- read.csv("DWSplitFullMod.csv") %>% 
  select(Year,L_Teddington) %>%
  mutate (L_Teddington = L_Teddington-24)%>%
# mutate(WSName = paste(SWSName)) %>%
  arrange(desc(Year),desc(L_Teddington))


DWHighTideB <- read.csv("ExportRWeather2019.csv") %>%
  select(HighTide,HighTide2,RaceOn, Year) %>%
  arrange(desc(Year))%>%
mutate(HighTideDecimal = {
  xlist <- strsplit(HighTide, split = ":")
  h <- as.numeric(sapply(xlist, "[", 1))
  m <- as.numeric(sapply(xlist, "[", 2))
#  s <- as.numeric(sapply(xlist, "[", 3))
  xdec <- h + (m / 60) 
  
})%>%
  mutate(HighTide2Decimal = {
    xlist <- strsplit(HighTide2, split = ":")
    h <- as.numeric(sapply(xlist, "[", 1))
    m <- as.numeric(sapply(xlist, "[", 2))
    #  s <- as.numeric(sapply(xlist, "[", 3))
    xdec <- h + (m / 60) 

})%>%
  mutate(Stop1 = HighTideDecimal +3.5)%>%
  mutate(Stop2 = HighTide2Decimal + 1.5)


#  unique()

plotCutOff <- merge(DWHighTideB,DWHighTimeA, by="Year")%>% filter(Year > 2006)


#-------------WaterSide DATASET LOADING--------------------

wsmain_data <- read.csv("ExportRWS2019.csv") %>%
  mutate(WSTime = format(WSTime, format = "%H:%M:%S")) %>%
  select(Year, WSRace, WSClass, WSName, SWSName, WSPosition, WSTime, WSClub, WSNotes) %>%
  arrange(desc(Year), WSRace, WSClass, WSTime, WSPosition, WSName, WSTime) %>%
  mutate(WSName2 = paste(SWSName)) %>%
  mutate(WSName = paste(SWSName)) %>%
  mutate(WSDecimalTime = {
    xlist <- strsplit(WSTime, split = ":")
    h <- as.numeric(sapply(xlist, "[", 1))
    m <- as.numeric(sapply(xlist, "[", 2))
    s <- as.numeric(sapply(xlist, "[", 3))
    xdec <- h + (m / 60) + (s / 60 / 60)
  })



wsmain_data$WSPosition <- as.numeric(as.character(wsmain_data$WSPosition))
wsmain_data$Year <- as.numeric(as.character(wsmain_data$Year))
wsmain_data$WSRace %<>% as.character
# wsmain_data$WSBoatType %<>% as.character
wsmain_data$WSClass %<>% as.character
wsmain_data$WSRace %<>% as.character
wsmain_data$WSName %<>% as.character
wsmain_data$WSClub %<>% as.character
wsmain_data$WSNotes %<>% as.character

#datatable(df) %>% formatStyle(
 # 'V6',
#  backgroundColor = styleEqual(c(0, 1), c('gray', 'yellow'))

wstable_data <- wsmain_data %>%
  # select(-WSName2, -WSBoatType)
  select(-WSName2,-SWSName)

wsmenus <- wsmain_data %>%
  select(Year, WSRace, WSClass) %>%
  arrange(desc(Year), WSRace, WSClass) %>%
  unique()



wsdivs <- wsmain_data %>%
  select(WSClass) %>%
  arrange(WSClass) %>%
  unique()

wsdivsposition <- wsmain_data %>%
  select(WSPosition) %>%
  arrange(WSPosition) %>%
  unique()

wsdivsclass <- wsmain_data %>%
  select(WSClass) %>%
  arrange(WSClass) %>%
  unique()

wspaddlers <- wsmain_data %>%
  select(WSName2) %>%
  arrange(WSName2) %>%
  unique()



WSATime <- wsmain_data %>%
  select(WSTime) %>%
  arrange(WSTime) %>%
  unique()

wsAddYears <- wsmain_data %>%
  select(Year) %>%
  arrange(desc(Year)) %>%
  unique()

# wstop3s <- wsmain_data %>%
# select(WSName2, WSPosition) %>%
#  filter(WSPosition %in%(c(1,2,3))) %>%
# group_by(WSName2) %>%
#  add_tally() %>%
# select(-WSPosition) %>%
#  unique()

# wscompleteCount <- wsmain_data %>%
# select(WSName2, WSPosition) %>%
#  filter(!is.na(WSPosition)) %>%
# select(-WSPosition) %>%
#  group_by(WSName2) %>%
# add_tally() %>%
#  unique()

# wsfaveLadiess <- wsmain_data %>%
# select(WSName2, WSRace) %>%
#  group_by(WSName2, WSRace) %>%
# add_tally() %>%
# filter(n>1) %>%
#  arrange(WSName2, -n) %>%
# unique()

# Weather Table

WMain_Data <- read.csv("ExportRWeather2019.csv") %>%
  select(EasterSunday, HighTide, RaceOn, Year, SeniorFlow, FirstSenior, MaxWind, MinTemperature, CompletionRate, EasterOffset, Regression_D, Regression_C, Regression_B, Regression_A) %>%
  arrange(desc(Year), desc(SeniorFlow), desc(FirstSenior), desc(MaxWind), desc(MinTemperature), desc(CompletionRate)) %>%
  unique()


Flow <- WMain_Data %>%
  select(SeniorFlow) %>%
  arrange(SeniorFlow) %>%
  unique()

FirstPlace <- WMain_Data %>%
  select(FirstSenior) %>%
  arrange(FirstSenior) %>%
  unique()

Wind <- WMain_Data %>%
  select(MaxWind) %>%
  arrange(MaxWind) %>%
  unique()

Temp <- WMain_Data %>%
  select(MinTemperature) %>%
  arrange(MinTemperature) %>%
  unique()

Completion <- WMain_Data %>%
  select(CompletionRate) %>%
  arrange(CompletionRate) %>%
  unique()

WMMain_Data <- read.csv("ExportRWeather2019.csv") %>%
  select(Year, SeniorFlow, MaxWind, MinTemperature, CompletionRate) %>%
  arrange(desc(Year), desc(SeniorFlow), desc(MaxWind), desc(MinTemperature), desc(CompletionRate)) %>%
  unique()

# DW Data

ClassAge <- read.csv("AgeSex.csv") %>%
  select(Year, Class, Age1, Age2, AgeB, Sex, BoatType, Place, Time)%>%
  arrange(desc(Year), Class) 
  

main_datatwo <- read.csv("ExportRDW2019.csv") %>%
  select(Year, Class, Position, Name, Club, Time, Flow, Veteran, Ladies, BoatType, Military, SubClass, Trophies, Record, Notes, Club1, Country, C1, BoatDesign, Finish) %>%
  arrange(desc(Year), Position, Time, Ladies, Military, Trophies, Veteran, desc(Flow)) %>%
  mutate(Name2 = paste(Name)) %>%
  mutate(DecimalTime = {
    xlist <- strsplit(Time, split = ":")
    h <- as.numeric(sapply(xlist, "[", 1))
    m <- as.numeric(sapply(xlist, "[", 2))
    s <- as.numeric(sapply(xlist, "[", 3))
    xdec <- h + (m / 60) + (s / 60 / 60)
  }) %>%
  mutate(Name2nd = {
    xlist <- strsplit(Name, split = " & ")
    h <- sapply(xlist, "[", 1)
  }) %>%
  mutate(Name1 = {
    xlist <- strsplit(Name, split = " & ")
    h <- sapply(xlist, "[", 2)
  }) %>%
  mutate(Club2nd = {
    xlist <- strsplit(Club, split = " & ")
    h <- sapply(xlist, "[", 1)
  }) %>%
  mutate(Club1 = {
    xlist <- strsplit(Club, split = " & ")
    h <- sapply(xlist, "[", 2)
  })

main_data <- read.csv("ExportRDW2019.csv") %>%
  select(Year, Class, Position, Name, Club, Time, Flow, Veteran, Ladies, BoatType, Military, SubClass, Trophies, Record, Notes, Club1, Country, C1, BoatDesign, Finish)%>%
  arrange(desc(Year), Position, Time, Ladies, Military, Trophies, Veteran, desc(Flow)) %>%
  mutate(Name2 = paste(Name)) %>%
  mutate(DecimalTime = {
    xlist <- strsplit(Time, split = ":")
    h <- as.numeric(sapply(xlist, "[", 1))
    m <- as.numeric(sapply(xlist, "[", 2))
    s <- as.numeric(sapply(xlist, "[", 3))
    xdec <- h + (m / 60) + (s / 60 / 60)
  }) %>%
  mutate(Name1 = {
    xlist <- strsplit(Name, split = " & ")
    h <- sapply(xlist, "[", 1)
  }) %>%
  mutate(Name2nd = {
    xlist <- strsplit(Name, split = " & ")
    h <- sapply(xlist, "[", 2)
  }) %>%
  mutate(Club1 = {
    xlist <- strsplit(Club, split = " & ")
    h <- sapply(xlist, "[", 1)
  }) %>%
  mutate(Club2nd = {
    xlist <- strsplit(Club, split = " & ")
    h <- sapply(xlist, "[", 2)
  }) 

ab <- rbind(main_datatwo, main_data)
abLadies <- rbind(main_datatwo, main_data)                                                                                            






main_data$Year <- as.numeric(as.character(main_data$Year))
main_data$SubClass %<>% as.character
main_data$BoatType %<>% as.character
main_data$Class %<>% as.character
main_data$Ladies %<>% as.character
main_data$Military %<>% as.character
main_data$Club %<>% as.character
main_data$Notes %<>% as.character
main_data$Name %<>% as.character
main_data$Trophies %<>% as.character
main_data$Position %<>% as.numeric(as.character(main_data$Position))
main_data$Flow %<>% as.numeric(as.character(main_data$Flow))

table_data <- main_data

table_datatwo <- main_datatwo 

menus <- main_data %>%
  select(Year, Name, Ladies, Military, Class, SubClass, Trophies, BoatType, Position) %>%
  arrange(desc(Year), Ladies, Military, Class, SubClass, Trophies, BoatType, Position, Name) %>%
  unique()

table_dataCrewstwo <- ab %>%
  select(Name1, Year, Class, Name, Time, Club, Position, Trophies, BoatType, DecimalTime, Ladies, Military, Finish) %>%
  filter(Name1 != "") %>%
  arrange(Name1, Year)

table_dataCrewstwoLadies <- abLadies %>%
  select(Name1, Year, Class, Name, Time, Club, Position, Trophies, BoatType, DecimalTime, Ladies, Military, Finish) %>%
  filter( Ladies == "Mixed" | Ladies == "Ladies") %>%
  arrange(Name1, Year)

menus1 <- ab %>%
  select(Name1, Club1) %>%
  arrange(Name1, Club1) %>%
  unique()

table_dataCrewstwoA <- main_datatwo %>%
  select(Year, Class, Name, Time, Club, Position, Trophies, BoatType, DecimalTime, Ladies, Military) %>%
  arrange(Year, Position)

table_dataCrews <- main_data %>%
  select(Year, Class, Name, Position, Time, Flow, Club, Veteran, Ladies, Military, SubClass, Trophies, BoatType, DecimalTime, BoatDesign, Finish) %>%
  arrange(desc(Year), Position)
# Note to me.  need to add the different table selections


table_dataCrewsABCD <- main_data %>%
  select(Year, Name, Class, DecimalTime) %>%
  arrange(desc(Year))

#Expand main_data to columns
datapipeABCD <- main_data %>%
  mutate(DWSenior = case_when(Class == "Senior" ~ DecimalTime)) %>%
  mutate(DWJunior = case_when(Class == "Junior" ~ DecimalTime)) %>%
  mutate(DWSingles = case_when(Class == "Singles" ~ DecimalTime)) %>%
  mutate(DWVetJunior = case_when(Class == "Vet/Junior" ~ DecimalTime)) %>%
  mutate(DWEndeavour = case_when(Class == "Endeavour" ~ DecimalTime))%>%
#  mutate(NewName = paste (Year, Name , sep=" "))%>%
select(Name,Year,DWSenior,DWJunior,DWSingles,DWVetJunior,DWEndeavour) 
data


blackyears <- main_data %>%select(Year, Flow)



table_dataCrewsABCDWS <- wsmain_data %>%
  select(Year, WSName, WSRace, WSDecimalTime) %>%
  arrange(desc(Year))

datapipeABCDWS <- wsmain_data %>%
  mutate(WatersideRaceA = case_when(WSRace == "A" ~ WSDecimalTime)) %>%
  mutate(WatersideRaceB = case_when(WSRace == "B" ~ WSDecimalTime)) %>%
  mutate(WatersideRaceC = case_when(WSRace == "C" ~ WSDecimalTime)) %>%
  mutate(WatersideRaceD = case_when(WSRace == "D" ~ WSDecimalTime)) %>%
  mutate(Name = paste (WSName))%>%
  select(Name,Year, WatersideRaceA,WatersideRaceB,WatersideRaceC,WatersideRaceD)%>%group_by(Year,Name)%>%
summarise(
  WS.Races = n(),WatersideA = sum(WatersideRaceA, na.rm = TRUE),WatersideB = sum(WatersideRaceB, na.rm = TRUE),WatersideC = sum(WatersideRaceC, na.rm = TRUE),WatersideD = sum(WatersideRaceD, na.rm = TRUE)
)%>% complete(Name) %>%arrange(desc(Year))%>% filter(Year >= 1990)
data


#data 



datapipeABCDcom <- merge(datapipeABCDWS,datapipeABCD)
as.data.frame(datapipeABCDcom)
datapipeABCDcom[datapipeABCDcom == 0] <- NA


table_dataCrewsCrews <- main_data %>%
  select(Name, Year, Class, Time, Club, Position, Trophies, BoatType) %>%
  arrange(Name, desc(Year), Class, Time, Club, Position, Trophies, BoatType)

table_dataCrewsClub <- main_data %>%
  select(Club, Year, Class, Time, Name, Position, Trophies, BoatType) %>%
  filter(Club != "") %>%
  arrange(Club, desc(Year), Time, Name, Position, Trophies, BoatType)

table_dataCrewsPlace <- main_data %>%
  select(Position, Year, Class, Name, BoatType, Club, Time, Flow, Veteran, Ladies, Military, SubClass, Trophies) %>%
  filter(Position != "0") %>%
  arrange(Position, desc(Year), Name, BoatType, Club, Time, SubClass)

table_dataCrewsTime <- main_data %>%
  select(Time, Year, Name, Position, Finish, Class, BoatType, Flow, Club, Veteran, Ladies, Military, SubClass, Trophies) %>%
  filter(Time != "") %>%
  arrange(Time, desc(Year), Position, BoatType, Name, Club, SubClass)

table_dataCrewsYear <- main_data %>%
  select(Year, Position, Time, Name, Class, Flow, Club, Veteran, Ladies, Military, SubClass, Trophies, BoatType) %>%
  arrange(desc(Year), Position, Time, Name, Club, SubClass, Trophies, BoatType)

table_dataCrewsCanadians <- main_data %>%
  select(Year, Class, Time, Name, Position, Club, Trophies, BoatType) %>%
  filter(BoatType == "Canadian") %>%
  arrange(desc(Year), Time, Name, Club, Position, Trophies)

table_dataCrewsFoldingBoat <- main_data %>%
  select(Year, Class, Time, Name, Position, Club, Trophies, BoatType) %>%
  filter(BoatType == "Folding Boat") %>%
  arrange(desc(Year), Time, Name, Club, Position, Trophies)

table_dataCrewsLadies <- main_data %>%
  select(Year, Class, Time, Name, Position, Club, Trophies, Ladies) %>%
  filter(Ladies != "Male") %>%
  arrange(desc(Year), Time, Name, Club, Position, Trophies)

table_dataRecords <- main_data %>%
  select(Record, Year, Class, Name, Position, Time, Club) %>%
  # select(Record !="")%>%
  filter(Record != "") %>%
  arrange(Class, Record)

table_dataTrophies <- main_data %>%
  select(Year, Class, Name, Position, Time, Club, Trophies, Record) %>%
  # select(Record !="")%>%
  filter(Trophies != "") %>%
  arrange(desc(Year), Class, Trophies)# new code april 24  ie Year added


table_dataExceptions <- main_data %>%
  select(Year, Class, Name, Time, Club, Notes, Position,Record) %>%
  # select(Record !="")%>%
  filter(Notes != "") %>%
  arrange(Year, Class)

table_dataVisitors <- main_data %>%
  select(Country, Year, Class, Name, Time, Club, Trophies, Record) %>%
  filter(Country != "") %>%
  arrange(Country, Year, Class)




wstable_dataCrews <- wsmain_data %>%
  select(Year, WSRace, WSClass, WSPosition, WSName, WSClub, WSTime) %>%
  arrange(desc(Year))


wstable_dataCrewsCrew <- wsmain_data %>%
  select(WSName, Year, WSRace, WSClass, WSTime, WSClub, WSPosition, WSNotes) %>%
  filter(WSName != "") %>%
  arrange(desc(Year), WSName, WSRace, WSTime, WSClass, WSClub)


wstable_dataCrewsClub <- wsmain_data %>%
  select(WSClub, Year, WSRace, WSClass, WSTime, WSName, WSPosition, WSNotes) %>%
  filter(WSClub != "") %>%
  arrange(desc(Year), WSClub, WSName, WSRace, WSTime, WSClass)

wstable_dataCrewsTimeY <- wsmain_data %>%
  select(Year, WSRace, WSTime, WSClass, WSPosition, WSClass, WSName, WSClub, WSNotes) %>%
  arrange(desc(Year), WSTime, WSClass, WSName, WSRace, WSClub)

wstable_dataCrewsTime <- wsmain_data %>%
  select(WSRace, WSTime, WSClass, Year, WSPosition, WSClass, WSName, WSClub, WSNotes) %>%
  filter(!is.null(WSTime)) %>%
  arrange(WSTime, desc(Year), WSName, WSRace, WSTime, WSClass, WSClub)


wstable_dataCrewsRace <- wsmain_data %>%
  select(WSRace, WSTime, Year, WSPosition, WSClass, Year, WSName, WSClub) %>%
  arrange(desc(Year), WSRace, WSClass, WSPosition, WSName, WSTime, WSClub)

wstable_dataCrewsYear <- wsmain_data %>%
  select(Year, WSRace, WSClass, WSName, WSTime, WSPosition, WSClub, WSNotes) %>%
  arrange(desc(Year), WSRace, WSClass, WSPosition, WSName, WSTime, WSClub)


wstable_dataCrewsSubClass <- wsmain_data %>%
  select(WSClass, Year, WSRace, WSTime, WSPosition, WSName, WSClub, WSNotes) %>%
  arrange(WSRace, desc(Year), WSName, WSRace, WSTime, WSClass, WSClub)



# lists in order

ResultType <- RND_data %>%
  select(Result) %>%
  arrange(Result) %>%
  unique()


Places <- DW2017S %>%
  select(Place) %>%
  arrange(Place) %>%
  unique()

SplitYearNew <- DW2017S %>%
  select(Year) %>%
  arrange(desc(Year)) %>%
  unique()

#arrange(desc(Year)
RetireYearNew <- Retire_data %>%
select(Year) %>%
  arrange(Year) %>%
  unique()

AgeYear <- ClassAge%>%
  select(Year) %>%
  arrange(Year) %>%
  unique()

paddlers <- main_data %>%
  select(Name2) %>%
  arrange(Name2) %>%
  unique()



boattypes <- main_data %>%
  select(BoatType) %>%
  arrange(BoatType) %>%
  unique()

divs <- main_data %>%
  select(Military) %>%
  arrange(Military) %>%
  unique()

divssc <- main_data %>%
  select(SubClass) %>%
  arrange(SubClass) %>%
  unique()



divsM <- main_data %>%
  select(Military) %>%
  arrange(Military) %>%
  unique()

divsposition <- main_data %>%
  select(Position) %>%
  arrange(Position) %>%
  unique()

divsclass <- main_data %>%
  select(Class) %>%
  arrange(Class) %>%
  unique()

divssubclass <- main_data %>%
  select(SubClass) %>%
  arrange(SubClass) %>%
  unique()

divsLadies <- main_data %>%
  select(Ladies) %>%
  arrange(Ladies) %>%
  unique()

divsclub <- main_data %>%
  select(Club) %>%
  arrange(Club) %>%
  unique()

divstrophies <- main_data %>%
  select(Trophies) %>%
  arrange(Trophies) %>%
  unique()

# new
completeCount <- main_data %>%
  select(Name, Position) %>%
  filter(!is.na(Position)) %>%
  select(-Position) %>%
  group_by(Name) %>%
  add_tally() %>%
  unique()


# top3s <- main_data %>%
# select(Name2, Position) %>%
# arrange(Class,Position, Year)%>%
#  group_by(Name2) %>%
# add_tally()  %>%
#  select(-Position) %>%
# unique()

# top3clubs <- main_data %>%
# select(Club, Position) %>%
#  group_by(Club) %>%
# add_tally() %>%
#  select(-Position) %>%
# unique()

data_Class <- c("Senior", "Junior", "Singles", "VetJunior","Endeavour")
data_setsAGE <- c("Senior", "Veteran", "Century", "Over 50")
data_setsdw <- c("DW", "WS", "Comparison", "Weather")  # Not Used
data_sets <- c("Army", "Canadian", "Century", "Civilian", "European", "Ladies", "Ladies C2", "Mixed", "Navy", "Over 50", "Overseas", "Police", "RAF", "Reserve", "Scouts", "Services", "Tyne", "U17 School", "University", "Vet Ladies", "Veteran")
data_setsM <- c("Army", "Navy", "RAF", "Army Reserve", "Navy Reserve", "RAF Reserve", "Civilian Reserve")
WatersideATimeList <- c("1.5", "1.6", "1.7", "1.8", "1.9", "2.0", "2.1", "2.2", "2.3", "2.4", "2.5", "2.6", "2.7", "2.8", "2.9", "3.0", "3.1", "3.2", "3.3", "3.4", "3.5", "3.6", "3.7", "3.8", "3.9", "4.0")
WatersideRaceL <- c("All", "A", "B", "C", "D", "S")
WatersideDTimeList <- c("4.0", "4.25", "4.5", "4.75", "5.0", "5.25", "5.5", "5.75", "6.0", "6.25", "6.5", "6.75", "7.0", "7.25", "7.5", "7.75", "8.0")
TrophyList <- c("Army", "Canadian", "Century", "Civilian", "European", "Home Built", "Ladies", "Lee", "Mixed", "Navy", "Over 50", "Overseas", "Police", "RAF", "Reserve", "Scouts", "Services", "Trophies", "Tyne", "U17 School", "University", "Vet Ladies", "Veteran")
SplitName <- c("MouleSharpe142","GreenhamPerrett205", "CornishGreenham187", "CornishGreenham158", "PhillipsLewis155", "WellsWells99.1", "CornishGreenham90.5", "GreenawayFowler80", "GreenhamBelcher70.2", "CornishViljoen28.1", "LawlerBrown23", "KingHendron23", "KingButler20", "BakerCapps229")
SplitFlow <- c("20", "40", "60", "80", "100", "120", "140", "160", "180", "200")
SplitBoat <- c("1", "2", "3")
PositionCode <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
TimeP <- c("A_Devizes", "B_Pewsey", "C_Hungerford", "D_Newbury", "E_Aldermaston", "F_Reading", "G_Marsh", "H_Marlow", "I_Bray", "J_OldWindsor", "K_Shepperton", "L_Teddington", "M_Westminster")
Distance <- c("0", "11.75", "25.5", "34.5", "43", "54", "61", "70.5", "78.5", "87", "97", "108.5", "125")
NewburyCutoff <- c("5.5","6","6.5","7","7.5","8","8.5","9","9.5","9.9")
ReadingCutoff <- c("9","9.5","10","10.5","11","12","13","14","15","15.5","16","16.5","17")
MarlowCutoff <- c("18","19","20","20.5","21","21.5","22")
OldWindsorCutoff <- c("23","24","24.5","25","25.5")
SheppertonCutoff <- c("26","26.5","27","27.5","28","29.5","29")
BlackyearsList <-c("2000","2001","2018","2020","2021")
Pre1971Years <-c("1948","1949","1950","1951","1952","1953","1954","1955","1956","1957","1958","1959","1960","1961","1962","1963","1964","1965","1966","1967","1968","1969","1970")

#-------------UI SECTION ----
ui <- fluidPage(
  # shinythemes::themeSelector(),
  theme = shinytheme("flatly"),
  list(tags$head(HTML('<link rel="icon", href="DW Logo.jpg",
                      type="image/png" />'))),
  div(
    style = "padding: 1px 0px; width: '100%'",
    titlePanel(
      title = "", windowTitle = "DW and WS Results Database"
    )
  ),
  navlistPanel(
    widths = c(2, 10),
    tabPanel(
      HTML("<span style='font-size: 150%; font-weight: bold;'>Introduction</span>"),
#      "Introduction",
      fluidRow(
        column(10, tags$h2("Welcome to the DW and Waterside Results Database with R programming")),
        column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
      ),
      # column(12, tags$h2("Welcome to the DW and WS Results Database in the R Programming Language "  )),
      fluidRow(sidebarLayout(
        sidebarPanel(
          column(12, tags$img(src = "P1030108C.jpg", width = "100%")),
          #       sidebarPanel(column(12, tags$img(src="1970 Croften and The long Run Modified.jpg", width = "100%")),
          (column(12, tags$h4("Congratulations to all those Completing this gruelling event:"))),
          (column(12, tags$img(src = "1000 Mile Club.jpg", width = "100%"))),
          (column(12, tags$h4("The 1000 Mile Club 2011")))
        ),
        mainPanel

        (
          fluidRow(column(12, tags$h4("What Why How"))),
          fluidRow(column(12, tags$h4("What - This application is written in R code following a British Canoeing MRC app (now replaced) for Race Results."))),
          fluidRow(column(12, tags$h4("Why - It is written in R code to enable dynamic graphics and statistics to be generated by the user.  The provision of an additional code base could offer an alternative future for the DW results database subject to a willing database administrator."))),
          fluidRow(column(12, tags$h4("How - The IBM Notes database (Web enabled in 2005) generates structured CSV files which are used by the application."))),
          fluidRow(column(12, tags$h4("Code - The code is available to interested parties on GitHub."))),
          fluidRow(column(12, tags$h4("Please note that the database only shows completions. It follows the conventions of David Keane; a past DW Organizer who collated the results and includes individuals who completed the course but for whom the existing rules did not allow formal participation.  "))),
          fluidRow(column(12, tags$h4("Web design and Maintenance by Arthur.J@InceOnline.co.uk"))),
          fluidRow(column(12, tags$h5("Acknowledgments-James Smythe (British Canoeing) and Callum Staff"))),
          fluidRow(column(12, tags$h5("Version 6.1 Apr 2024"))),
          fluidRow(column(12, tags$h4("Corrections & Comments", tags$a(href = "http://www.canoeraceresults.co.uk/extranet/devizes.nsf/4180a9361f29b17f80257a9a00509ff3?OpenForm", "Click here"))))
          
        )
      ))
    ),
    navbarMenu(
      HTML("<span style='color: red; font-weight: bold;'>DW Section</span>"), #Change to red and bold
 #     "DW Section",
      tabPanel(
        "Thousand Mile Club",
        tabsetPanel(
          tabPanel(
            "Thousand Mile Paddlers",
            fluidRow(column(12, tags$h4("The list includes paddlers who have completed the course 8 or more times irrespective of their class or the year.
                                         It therefore includes paddlers who are not recognised by DW. You may sort individual columns in this list"))),
            fluidRow(column(12, tags$h4("Paddlers with 8 or more Senior completions are shown in red.  A full list of completions is available under DW Results
                                        - DW Crew Completions"))),
            
            fluidRow(column(12, DT::dataTableOutput("tmctable"))),
            
            fluidRow(column(12, tags$h4("Len Blackmore and J H Burr completed their 8 together with Len adding to his total to reach 10.  Barry Mckenna and Gary Wakley now have 10 completions together.
            Richard Spinks remains the only competitor to have entered the 1000 mile club with all 8 in a Klepper. Michael Underhill-Race is the only competitor to have completed 8 Senior in a Canadian with Robert Campbell
            completing 10 in three classes.
            Note James King completed his first DW course after the race was cancelled in 2000 (7th May) - His inclusion in this list is a courtesy")))
          ),
         
          tabPanel(
            "Thousand Mile Ladies",
            fluidRow(column(12, tags$h4("The list includes Ladies who have completed the course 8 or more times irrespective of their class or the year.
                                         It therefore includes paddlers who are not recognised by DW. You may sort individual columns in this list"))),
            fluidRow(column(12, tags$h4("Paddlers with 8 or more Senior completions are shown in red.  A full list of completions is available under DW Results
                                        - DW Crew Completions"))),
            
            fluidRow(column(12, DT::dataTableOutput("tmctableLadies"))),
            
            ),
          
          
          
         
          tabPanel(
            "Thousand Mile Paddlers with 8 Senior",
            fluidRow(column(12, tags$h4("The list includes paddlers who have completed the Senior course 8 or more times irrespective of the year
                                         It may therefore includes paddlers who are not recognised by DW. You may sort individual columns in this list"))),
            fluidRow(column(12, tags$h4("A full list of completions is available under DW Results
                                        - DW Crew Completions"))),
            
            fluidRow(column(12, DT::dataTableOutput("tmctableSen")))
          ),
         
          tabPanel(
            "Thousand Mile Chart By Class",
            fluidRow(
              column(10, tags$h2("DW and Waterside Results Database - Thousand Mile Club")),
              column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
            ),
            fluidRow(column(12, tags$h4("These lists includes paddlers who have completed the course 8 or more times irrespective of their class or the year. It therefore includes paddlers who are not recognised by DW"))),
            fluidRow(column(12, plotOutput("tmcchart"))),
            fluidRow(column(2,selectInput("ClassTC","Pick one or more classes",data_Class,selected = "Senior",multiple = TRUE))),
            fluidRow(column(12, plotOutput("tmcchart1")))
          ),
          tabPanel(
            "Thousand Mile Chart By Boat Type",
            fluidRow(
              column(10, tags$h2("DW and Waterside Results Database - Thousand Mile Club")),
              column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
            ),
            fluidRow(column(12, tags$h4("The list includes paddlers who have completed the course 8 or more times irrespective of their class or the year. It therefore includes paddlers who are not recognised by DW"))),
            fluidRow(column(12, plotOutput("tmcchartBT1")))
          ),
          tabPanel(
            "List of Paddlers with 7 Completions",
            fluidRow(column(12, tags$h4("The list includes paddlers who have completed the course 7 times irrespective of their class or the year.
                                         It may therefore includes paddlers who are not recognised by DW.  You may sort individual columns in this list"))),
            
            # Create a new row for the table.
            fluidRow(column(12, DT::dataTableOutput("tmctable7")))
          )
        )
      ),
 
 
      tabPanel(
        "DW Results",
        tabsetPanel(
          tabPanel(
            "DW Data Set",
            fluidRow(column(12, tags$h4("DW Crews & Clubs - Please click choices and press Delete to change. - Note: A Position of 0 indicates a completion outside the normal race rules."))),
            fluidRow(
              column(2, selectInput("YearCC", "Pick Year", c("All", unique(menus$Year)), selected = "All", multiple = FALSE)),
              column(2, selectInput("ClassCC", "Pick Class", c("All", unique(menus$Class)), selected = "All", multiple = FALSE)),
              column(2, selectInput("LadiesCC", "Ladies or Mixed", c("All", unique(menus$Ladies)), selected = "All", multiple = FALSE)),
              column(2, selectInput("divCC", "Civilian or Military", c("All", unique(menus$Military)), selected = "All", multiple = FALSE)),
              column(2, selectInput("BTCC", "Choose Boat Type:", c("All", unique(menus$BoatType)), selected = "All", multiple = FALSE)),
              column(2, selectInput("PLCC", "Choose Position:", c("All", unique(divsposition$Position)), selected = "All", multiple = FALSE))
            ),
            fluidRow(column(12, DT::dataTableOutput("tableCrews"))),
            fluidRow(column(12, tags$h5("The value shown for Flow is more representative of that for the Senior Class.  The Flow can change significantly over the course of the race. The 2022 event stopped at Teddington")))
          ),
          tabPanel(
            "DW Crews",
            fluidRow(column(12, tags$h4("DW Crews - Name1 lists individual competitors while Name lists crews for doubles."))),
            fluidRow(column(12, DT::dataTableOutput("tableCrewstwo"))),
          ),


  
          tabPanel(
            "DW Crew Completions",
            fluidRow(column(12, tags$h4("DW Crews -  Total Number of Completions by Competitor."))),
            #     fluidRow(
            #      column(2,selectInput("NamesumC", "Crew",c("All",unique(menus1$Name1)),selected = "All",multiple = FALSE))),
            fluidRow(column(12, tags$h5("The chart below can be used to find members of the 1000 mile club from the Completions Column.  It also provides dates for first and last completions including the span for all Classes"))),
            fluidRow(column(12, DT::dataTableOutput("tableCrewstwoDev1"))),
          ),
          tabPanel(
            "DW Averages",
            fluidRow(
              column(4, tags$h4("DW Group, Club or Competitor Averages")),
              column(8, tags$h4("Paddlers shown with place 0 are listed as exceptions and may not have a valid time with averages being missleading", style = "color:red"))
            ),
            fluidRow(
              column(1, selectInput("YearCCtwosum", "From Year", c(unique(menus$Year)), selected = "1948", multiple = FALSE)),
              column(1, selectInput("YearCCtwosumL", "To Year", c(unique(menus$Year)), selected = "2023", multiple = FALSE)),
              column(2, selectInput("ClassCCtwosum", "Pick Class", c("All",unique(menus$Class)), selected = "Senior", multiple = FALSE)),
              column(2, selectInput("LadiesCCtwosum", "Ladies or Mixed", c("All", unique(menus$Ladies)), selected = "All", multiple = FALSE)),
              column(2, selectInput("divCCtwosum", "Civilian or Military", c("All", unique(menus$Military)), selected = "All", multiple = FALSE)),
              column(1, selectInput("PLCCtwosum", "From Position:", c(unique(divsposition$Position)), selected = "1", multiple = FALSE)),
              column(1, selectInput("PLCCtwosumL", "To Position:", c(unique(divsposition$Position)), selected = "174", multiple = FALSE)),
              column(2, selectInput("att2boatsum", "Boat Type", c("All", unique(menus$BoatType)), selected = "All", multiple = FALSE))
            ),
            fluidRow(
              column(2, selectInput("Namesum", "Crew", c("All", unique(menus1$Name1)), selected = "All", multiple = FALSE)),
              column(2, selectInput("Clubsum", "Club", c("All", unique(menus1$Club1)), selected = "All", multiple = FALSE))
            ),
            fluidRow(column(12, DT::dataTableOutput("tableCrewstwoSeniorA"))),
            fluidRow(column(12, tags$h4("The summary data will change with Class selection", style = "color:red"))),
            fluidRow(column(12, tags$h4("The 2022 event finished at Teddington", style = "color:black"))),
            fluidRow(column(12, DT::dataTableOutput("tableCrewstwoSeniorB"))),
            #    fluidRow(column(12, tags$h4("Please select only one class for averages.",style="color:red"))),
            #    fluidRow(column(12, tags$h4("The above display gives averages for groups, clubs or competitors who have completed the event several times. You may select the groups above.
            #                              Paddlers shown with place 0 may not include a time and therefore may not provide averages"))),
          ),
          tabPanel(
            "DW Clubs",
            fluidRow(column(12, tags$h4("DW Clubs - Please select the required Class and use the Search box which is case sensitive.... Tip - if you have entered the whole word try adding a space"))),
            fluidRow(
              column(4, selectInput("YearCCNCL", "Pick Year", c("All", unique(menus$Year)), selected = "All", multiple = FALSE)),
              column(8, radioButtons("ClassCCNCL", "Pick Class", c("All", unique(menus$Class)), selected = "Senior", inline = TRUE))
            ),
            #                  column(2,selectInput("LadiesCC","Ladies or Mixed",c("All",unique(menus$Ladies)),selected = "All",multiple = TRUE)),
            #                 column(2,selectInput("divCC","Civilian or Military",c("All",unique(menus$Military)),selected = "All",multiple = TRUE)),
            #                column(2,selectInput("BTCC", "Choose Boat Type:", c("All",unique(menus$BoatType)), selected = "All", multiple = TRUE)),
            #               column(2,selectInput("PLCC", "Choose Position:", c("All",unique(divsposition$Position)), selected = "All", multiple = FALSE))),
            fluidRow(column(12, DT::dataTableOutput("tableClub"))),
            fluidRow(column(12, tags$h5("The value shown for Flow is more representative of that for the Senior Class.  The Flow can change significantly over the course of the race")))
          ),
          tabPanel(
            "DW Place",
            fluidRow(column(12, tags$h4("DW Place - Please click choices and press Delete to change."))),
            fluidRow(
              column(3, selectInput("YearCCNP", "Pick Year", c("All", unique(menus$Year)), selected = "All", multiple = FALSE)),
              column(3, selectInput("PLCCNP", "Choose Position:", c("All", unique(divsposition$Position)), selected = "All", multiple = FALSE)),
              column(6, radioButtons("ClassCCNP", "Pick Class", c("All", unique(menus$Class)), selected = "Senior", inline = TRUE))
            ),
            #                  column(2,selectInput("LadiesCC","Ladies or Mixed",c("All",unique(menus$Ladies)),selected = "All",multiple = TRUE)),
            #                 column(2,selectInput("divCC","Civilian or Military",c("All",unique(menus$Military)),selected = "All",multiple = TRUE)),
            #                column(2,selectInput("BTCC", "Choose Boat Type:", c("All",unique(menus$BoatType)), selected = "All", multiple = TRUE)),

            fluidRow(column(12, DT::dataTableOutput("tablePlace"))),
            fluidRow(column(12, tags$h5("The value shown for Flow is more representative of that for the Senior Class.  The Flow can change significantly over the course of the race")))
          ),
          tabPanel(
            "DW Time",
            fluidRow(column(12, tags$h4("DW Time  - Note: The 4 day events stopped at Teddington in 2016. All classes finished at Teddington in 2022. A Position of 0 indicates a completion outside the normal race rules."))),
            #  fluidRow(column(4,selectInput("YearCCNT", "Pick Year",c("All",unique(menus$Year)),selected = "All",multiple = FALSE)),
            fluidRow(column(8, radioButtons("ClassCCNT", "Pick Class", c("All", unique(menus$Class)), selected = "Senior", inline = TRUE))),
            #                  column(2,selectInput("LadiesCC","Ladies or Mixed",c("All",unique(menus$Ladies)),selected = "All",multiple = TRUE)),
            #                 column(2,selectInput("divCC","Civilian or Military",c("All",unique(menus$Military)),selected = "All",multiple = TRUE)),
            #                column(2,selectInput("BTCC", "Choose Boat Type:", c("All",unique(menus$BoatType)), selected = "All", multiple = TRUE)),
            #               column(2,selectInput("PLCC", "Choose Position:", c("All",unique(divsposition$Position)), selected = "All", multiple = FALSE))),
            fluidRow(column(12, DT::dataTableOutput("tableTime"))),
            fluidRow(column(12, tags$h5("The value shown for Flow is more representative of that for the Senior Class.  The Flow can change significantly over the course of the race")))
          ),
          tabPanel(
            "DW Year",
            fluidRow(column(12, tags$h4("DW Year - Please click choices and press Delete to change. The event finished at Teddington in 2022. Note: A Position of 0 indicates a completion outside the normal race rules."))),
            fluidRow(
              column(4, selectInput("YearCCNY", "Pick Year", c("All", unique(menus$Year)), selected = "All", multiple = FALSE)),
              column(8, radioButtons("ClassCCNY", "Pick Class", c("All", unique(menus$Class)), selected = "Senior", inline = TRUE))
            ),
            #                  column(2,selectInput("LadiesCC","Ladies or Mixed",c("All",unique(menus$Ladies)),selected = "All",multiple = TRUE)),
            #                 column(2,selectInput("divCC","Civilian or Military",c("All",unique(menus$Military)),selected = "All",multiple = TRUE)),
            #                column(2,selectInput("BTCC", "Choose Boat Type:", c("All",unique(menus$BoatType)), selected = "All", multiple = TRUE)),
            #               column(2,selectInput("PLCC", "Choose Position:", c("All",unique(divsposition$Position)), selected = "All", multiple = FALSE))),
            fluidRow(column(12, DT::dataTableOutput("tableYear"))),
            fluidRow(column(12, tags$h5("The value shown for Flow is more representative of that for the Senior Class.  The Flow can change significantly over the course of the race")))
          ),
          tabPanel(
            "DW Canadians",
            fluidRow(column(12, tags$h4("DW Canadians. The event finished at Teddington in 2022. Please click choices and press Delete to change."))),
            fluidRow(
              column(4, selectInput("YearCCNCA", "Pick Year", c("All", unique(menus$Year)), selected = "All", multiple = FALSE)),
              column(8, radioButtons("ClassCCNCA", "Pick Class", c("All", unique(menus$Class)), selected = "Senior", inline = TRUE))
            ),
            #                    column(2,selectInput("LadiesCC","Ladies or Mixed",c("All",unique(menus$Ladies)),selected = "All",multiple = TRUE)),
            #                   column(2,selectInput("divCC","Civilian or Military",c("All",unique(menus$Military)),selected = "All",multiple = TRUE)),
            #                  column(2,selectInput("BTCC", "Choose Boat Type:", c("All",unique(menus$BoatType)), selected = "All", multiple = TRUE)),
            #                 column(2,selectInput("PLCC", "Choose Position:", c("All",unique(divsposition$Position)), selected = "All", multiple = FALSE))),
            fluidRow(column(12, DT::dataTableOutput("tableCanadians"))),
            fluidRow(column(12, tags$h5("The value shown for Flow is more representative of that for the Senior Class.  The Flow can change significantly over the course of the race")))
          ),
          tabPanel(
            "DW Folding Boat",
            fluidRow(column(12, tags$h4("DW Folding Boats. The event finished at Teddington in 2022. Please click choices and press Delete to change. "))),
            fluidRow(
              column(4, selectInput("YearCCNFB", "Pick Year", c("All", unique(menus$Year)), selected = "All", multiple = FALSE)),
              column(8, radioButtons("ClassCCNFB", "Pick Class", c("All", unique(menus$Class)), selected = "Senior", inline = TRUE))
            ),
            #                  column(2,selectInput("LadiesCC","Ladies or Mixed",c("All",unique(menus$Ladies)),selected = "All",multiple = TRUE)),
            #                 column(2,selectInput("divCC","Civilian or Military",c("All",unique(menus$Military)),selected = "All",multiple = TRUE)),
            #                column(2,selectInput("BTCC", "Choose Boat Type:", c("All",unique(menus$BoatType)), selected = "All", multiple = TRUE)),
            #               column(2,selectInput("PLCC", "Choose Position:", c("All",unique(divsposition$Position)), selected = "All", multiple = FALSE))),
            fluidRow(column(12, DT::dataTableOutput("tableFoldingBoat"))),
            fluidRow(column(12, tags$h5("The value shown for Flow is more representative of that for the Senior Class.  The Flow can change significantly over the course of the race")))
          ),
          tabPanel(
            "DW Ladies",
            fluidRow(column(12, tags$h4("DW Ladies. The event finished at Teddington in 2022. Please click choices and press Delete to change."))),
            fluidRow(
              column(4, selectInput("YearCCNL", "Pick Year", c("All", unique(menus$Year)), selected = "All", multiple = FALSE)),
              column(8, radioButtons("ClassCCNL", "Pick Class", c("All", unique(menus$Class)), selected = "Senior", inline = TRUE))
            ),
            #                  column(2,selectInput("LadiesCC","Ladies or Mixed",c("All",unique(menus$Ladies)),selected = "All",multiple = TRUE)),
            #                 column(2,selectInput("divCC","Civilian or Military",c("All",unique(menus$Military)),selected = "All",multiple = TRUE)),
            #                column(2,selectInput("BTCC", "Choose Boat Type:", c("All",unique(menus$BoatType)), selected = "All", multiple = TRUE)),
            #               column(2,selectInput("PLCC", "Choose Position:", c("All",unique(divsposition$Position)), selected = "All", multiple = FALSE))),
            fluidRow(column(12, DT::dataTableOutput("tableLadies"))),
            fluidRow(column(12, tags$h5("The value shown for Flow is more representative of that for the Senior Class.  The Flow can change significantly over the course of the race")))
          ),
          tabPanel(
            "DW Exceptions Table",
            fluidRow(column(12, tags$h4("DW Exceptions Table - Please note this table includes individuals who completed the course but for whom the existing rules may not have allowed formal participation."))),
            fluidRow(column(12, DT::dataTableOutput("tableExceptions")))
          )
        )
      ),
      tabPanel(
        "DW Records",
        tabsetPanel(
          tabPanel(
            "DW Records Table",
            fluidRow(
              column(10, tags$h2("DW and Waterside Results Database - Records Table")),
              column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
            ),
            #       fluidRow(column(12, tags$h4("DW Records Table"))),
            column(3, selectInput("attRecordclassR", "Choose Class", c(unique(divsclass$Class)), selected = "Senior", multiple = FALSE)),
            fluidRow(column(12, DT::dataTableOutput("tableCrewsR")))
          ),
          tabPanel(
            "DW Records Chart",
            fluidRow(
              column(10, tags$h2("DW and Waterside Results Database - Records Chart")),
              column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
            ),
            # fluidRow(column(12, tags$h4("DW Records Chart"))),
            column(3, selectInput("attRecordclass", "Choose Class", c(unique(divsclass$Class)), selected = "Senior", multiple = TRUE)),
            fluidRow(column(12, plotOutput("Record")))
          )
        )
      ),
      tabPanel(
        "DW Paddler History",
        fluidRow(
          column(10, tags$h2("DW and Waterside Results Database - Paddler History")),
          column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
        ),
        fluidRow(
          column(12, tags$h3(textOutput("DW Paddlername"), tags$h4("Please check the DW Database for the correct Name. - Note: A Position of 0 indicates a completion outside the normal race rules.  The 2022 event stopped at Teddington"))),
          column(6, textInput("paddler", "Enter Paddler - The Name is Case Sensitive (Last Name  First Name/Initial)", value = "")),
        ),
        column(12, tags$h3(textOutput("DW Paddlername 2"), tags$h5("You may check for crew doubles if you use the exact format shown in the DW Crews and Clubs table eg Hendron Richard & King James
                                                                         , Cornish Tim & Greenham Brian, Lewis Richard & Phillips Mark C, Morrissey Jim, White Ian.  These all demonstrate a good relationship between Time and Flow.
                                                                         They also illustrate that fast paddlers are more likely to benefit form Flow."))),
        fluidRow(column(12, plotOutput("positions"))),
        fluidRow(column(12, tags$h5("The value shown for Flow is more representative of that for the Senior Class.  The Flow can change significantly over the course of the race.  Flow has been added as Text to the Left and Centre plots with Year added to the Right Plot"))),
        fluidRow(column(12, plotOutput("TimePlacePositions"))),
        fluidRow(column(12, tags$h4("The box Plot shows the results for the nominated Crew.  The box includes 50% on the results. The top and bottom part of the box include 25% each.  The remaining 25%'s are shown by the vertical lines, top and bottom.  Outliers are shown by dots.  The average is shown by the black circle. "))),
        fluidRow(column(12, plotOutput("TimePlacePositions1"))),
        fluidRow(column(12, tags$h4("The above plots are by Position while the previous plots are by time"))),
        # column(3, tags$h4("Endeavour")),
        column(3, tags$h4("Junior")),
        column(3, tags$h4("Senior")),
        column(3, tags$h4("Singles")),
        column(3, tags$h4("Vet/Junior")),
        # column(3, verbatimTextOutput("SummaryEndCrew")),
        column(3, verbatimTextOutput("SummaryJunCrew")),
        column(3, verbatimTextOutput("SummarySenCrew")),
        column(3, verbatimTextOutput("SummarySinCrew")),
        column(3, verbatimTextOutput("SummaryVJCrew")),
      ),

      tabPanel(
        "DW Clubs",
        tabsetPanel(
      tabPanel(
        "DW Clubs Chart",
        fluidRow(
          column(10, tags$h2("DW and Waterside Results Database - DW Clubs")),
          column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
        ),
        #     fluidRow(column(12, tags$h4("DW Clubs"))),
        fluidRow(
          #  column(12, tags$h3(textOutput("DW Clubs"))),
          column(3, selectInput("attclubyear", "Pick Year(s)", c(unique(menus$Year)), selected = "2023", multiple = TRUE)),
          column(3, selectInput("attclubclass", "Choose Class(es)", c(unique(divsclass$Class)), selected = "Senior", multiple = TRUE))
        ),
        fluidRow(column(12, plotOutput("clubplot"))),
        column(12, tags$h4("The largest group (Independents) have not indicated membership of a club and are excluded. The chart only includes clubs with a total of more than 24 completions based on the 1st named club"))
      ),
      
      tabPanel(
        "DW Clubs Chart - Pre 1971 Years",
        fluidRow(
          column(10, tags$h2("DW and Waterside Results Database - DW Clubs")),
          column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
        ),
        #     fluidRow(column(12, tags$h4("DW Clubs"))),
        fluidRow(
          #  column(12, tags$h3(textOutput("DW Clubs"))),
      #    column(3, selectInput("attclubyearPre", "Pick Year(s)", c(unique(menus$Year)), selected = "1948", multiple = TRUE)),
       #   column(3, selectInput("attclubclassPre", "Choose Class(es)", c(unique(divsclass$Class)), selected = "Senior", multiple = TRUE))
        ),
        fluidRow(column(12, plotOutput("clubplotPre"))),
        column(12, tags$h4("The largest group (Independents) have not indicated membership of a club and are excluded. The chart only includes clubs with a total of more than 10
                           completions based on the 1st named club over all years to current date however the chart only displays data for the years from 1948 to 1970. Prior to 1971 bank support was not premitted"))
      ),
      tabPanel(
        "DW Clubs Table",
        fluidRow(
          column(10, tags$h2("DW and Waterside Results Database - DW Clubs")),
          column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
        ),
           fluidRow(column(12, DT::dataTableOutput("ClubNumbertable"))),
        column(12, tags$h4("The largest group (Independents) have not indicated membership of a club and are excluded. The chart only includes clubs with a total of more than 24 completions based on the 1st named club"))
      ),
      tabPanel(
        "DW Club History",
        fluidRow(
          column(10, tags$h2("DW and Waterside Results Database - Club History")),
          column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
        ),
        fluidRow(
          column(12, tags$h3(textOutput("DW Clubname"), tags$h4("Please check the DW Database for the correct Name. - Note: A Position of 0 indicates a completion outside the normal race rules"))),
          column(12, textInput("regclub", "Enter Club - The Name is Case Sensitive", value = ""))
        ),
        fluidRow(column(12, plotOutput("clubpositions"))),
        fluidRow(column(12, tags$h5("The value shown for Flow is more representative of that for the Senior Class.  The Flow can change significantly over the course of the race"))),
        fluidRow(column(12, plotOutput("TimePlacePositionsClub"))),
        fluidRow(column(12, tags$h4("The box Plot shows the results for the nominated Club.  The box includes 50% on the results. The top and bottom part of the box include 25% each.  The remaining 25%'s are shown by the vertical lines, top and bottom.  Outliers are shown by dots.  The average is shown by the black circle. "))),
        fluidRow(column(12, plotOutput("TimePlacePositions1Club"))),
        fluidRow(column(12, tags$h4("The above plots are by Position while the previous plots are by time"))),
        # column(3, tags$h4("Endeavour")),
        column(3, tags$h4("Junior")),
        column(3, tags$h4("Senior")),
        column(3, tags$h4("Singles")),
        column(3, tags$h4("Vet/Junior")),
        # column(3, verbatimTextOutput("SummaryEndClub")),
        column(3, verbatimTextOutput("SummaryJunClub")),
        column(3, verbatimTextOutput("SummarySenClub")),
        column(3, verbatimTextOutput("SummarySinClub")),
        column(3, verbatimTextOutput("SummaryVJClub")),
      )
        )),
      
      
      tabPanel(
        "DW Class & Subclass",
        tabsetPanel(
          tabPanel(
            "DW Class by Year",
            fluidRow(
              column(10, tags$h2("DW and Waterside Results Database - Class by Year")),
              column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
            ),
            fluidRow(
              column(12, tags$h4("Please click choices and press Delete to change")),
              column(3, selectInput("att2class", "Choose Class", c(unique(menus$Class)), selected = "Senior", multiple = TRUE)),
              column(3, selectInput("attyearTY", "Select Years Since", c(unique(menus$Year)), selected = "1948", multiple = FALSE))
            ),
            fluidRow(column(12, plotOutput("Mattendance1"))),
            fluidRow(column(12, tags$h4("Shows the number of boats completing the course each year"))),
            fluidRow(column(12, plotOutput("Mattendance1N"))),
            fluidRow(column(12, tags$h4("Shows the total number of boats completing the course"))),
            fluidRow(column(12, plotOutput("MattendanceMPY"))),
            fluidRow(column(12, tags$h4("Shows the Times for boats completing the course.  Note that Endeavour times can be similar to Vet/Junior and Junior.  
                Almost by definition Endeavour crews might be expected to be slower than the non stop Senior crews; or they might enter the Senior Class unless prevented by age rules.
                eg Junior Senior combinations are not permitted to enter the Vet Junior Event.  In some years Endeavour crews have been as fast or even faster than the other four day doubles classes.")))
          ),
          tabPanel(
            "DW Military by Year",
            fluidRow(
              column(10, tags$h2("DW and Waterside Results Database - Military by Year")),
              column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
            ),
            fluidRow(
              column(12, tags$h4("Please click choices and press Delete to change")),
              column(3, selectInput("attMArm", "Choose Arm", data_setsM, selected = "Army", multiple = TRUE)),
              column(3, selectInput("attMclass", "Choose Class", c(unique(menus$Class)), selected = "Senior", multiple = TRUE))
            ),
            fluidRow(column(12, plotOutput("MattendanceM"))),
            fluidRow(column(12, tags$h4("Shows the number of boats completing the course"))),
            fluidRow(column(12, plotOutput("MattendanceMP"))),
            fluidRow(column(12, tags$h4("Shows the Positions of boats completing the course")))
          ),
          tabPanel(
            "DW Sex by Year",
            fluidRow(
              column(10, tags$h2("DW and Waterside Results Database - Sex by Year")),
              column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
            ),
            fluidRow(
              column(12, tags$h4("Please click choices and press Delete to change")),
              column(3, selectInput("attLclass", "Choose Sex", c(unique(menus$Ladies)), selected = "Male", multiple = TRUE)),
              column(3, selectInput("attL1class", "Choose Class", c(unique(menus$Class)), selected = "Senior", multiple = TRUE))
            ),
            fluidRow(column(12, plotOutput("MattendanceL"))),
            fluidRow(column(12, tags$h4("Shows the number of boats completing the course"))),
            fluidRow(column(12, plotOutput("MattendanceLS"))),
            fluidRow(column(12, tags$h4("Shows the positions of boats completing the course by sex")))
          ),
          tabPanel(
            "DW Minor Subclass by Year",
            fluidRow(
              column(10, tags$h2("DW and Waterside Results Database - Minor SubClass")),
              column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
            ),
            #    fluidRow(column(12, tags$h4("DW Minor SubClass"))),
            fluidRow(
              column(12, tags$h4("Please click choices and press Delete to change")),
              column(3, selectInput("attSubclass", "Choose Subclass", c(unique(divssubclass$SubClass)), selected = "Overseas", multiple = TRUE)),
              column(3, selectInput("attL2class", "Choose Class", c(unique(menus$Class)), selected = "Senior", multiple = TRUE))
            ),
            fluidRow(column(12, plotOutput("MattendanceSL"))),
            fluidRow(column(12, tags$h4("Shows the number of boats completing the course. The Subclass Options have been reduced for this Chart.  Age, Services and Sex subclasses are shown separately in the application"))),
            fluidRow(column(12, plotOutput("MattendanceSLT"))),
            fluidRow(column(12, tags$h4("Shows the total number of boats completing the course. ")))
          ),
          tabPanel(
            "DW Age group by Year",
            fluidRow(
              column(10, tags$h2("DW and Waterside Results Database - Age group by Year")),
              column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
            ),
            #     fluidRow(column(12, tags$h4("DW Age group by Year"))),
            fluidRow(
              column(12, tags$h4("Please click choices and press Delete to change")),
              column(4, selectInput("attL2classCL", "Choose Class", c(unique(menus$Class)), selected = "Senior", multiple = TRUE)),
              column(4, selectInput("attSubclassSEX", "Choose Male, Female or Mixed", c(unique(divsLadies$Ladies)), selected = "Male", multiple = TRUE)),
              column(4, selectInput("attSubclassBOAT", "Choose Boat Type", c(unique(boattypes$BoatType)), selected = "Kayak", multiple = TRUE))
            ),
            fluidRow(column(12, plotOutput("MattendanceSLO"))),
            fluidRow(column(12, tags$h4("Shows the number of boats completing the course."))),
            fluidRow(column(12, plotOutput("MattendanceSLOP"))),
            fluidRow(column(12, tags$h4("Shows the Positions of boats completing the course by Age group.")))
          ),
          tabPanel(
            "DW Boat Type by Year",
            fluidRow(
              column(10, tags$h2("DW and Waterside Results Database - Boat Types")),
              column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
            ),
            # fluidRow(column(12, tags$h4("Boat types include Kayak, Canadian and Folding Boats"))),
            fluidRow(
              column(12, tags$h4("Please click choices and press Delete to change")),
              column(3, selectInput("att2boat", "Choose Boat Type", c(unique(boattypes$BoatType)), selected = "Kayak", multiple = TRUE))
            ),
            fluidRow(column(12, plotOutput("Mattendance1BT"))),
            fluidRow(column(12, tags$h4("Shows the number of boats completing the course")))
          ),
          tabPanel(
            "DW Attendance by Specified Year(s)",
            fluidRow(
              column(10, tags$h2("DW and Waterside Results Database - Attendance")),
              column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
            ),
            fluidRow(
              column(12, tags$h4("Civilian and Military Attendance - Click choices and press Delete to change")),
              column(3, selectInput("attdiv", "Choose Civilian and/or Arms", c(unique(menus$Military)), selected = "Civilian", multiple = TRUE)),
              column(3, selectInput("attclass", "Choose Class", c(unique(divsclass$Class)), selected = "Senior", multiple = TRUE)),
              column(3, selectInput("region", "Choose Boat Type:", c(unique(boattypes$BoatType)), selected = "Kayak", multiple = TRUE)),
              column(3, selectInput("attyear", "Pick one or more Years", c(unique(menus$Year)), selected = "2023", multiple = TRUE))
            ),
            fluidRow(column(12, plotOutput("Mattendance")))
          ),
          tabPanel(
            "DW Subclass Totals",
            fluidRow(
              column(10, tags$h2("DW and Waterside Results Database - Subclass Totals")),
              column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
            ),
            fluidRow(
              column(12, tags$h4("Please click choices and press Delete to change")),
              #  column(3,selectInput("attMArmT", "Choose Arm", data_setsM, selected = "Army", multiple = TRUE)),
              column(3, selectInput("attMclassT", "Choose Class", c(unique(menus$Class)), selected = "Senior", multiple = TRUE)),
              column(3, selectInput("attyearT", "Select Years Since", c(unique(menus$Year)), selected = "1948", multiple = FALSE))
            ),
            fluidRow(column(12, plotOutput("MattendanceMT"))),
            fluidRow(column(12, tags$h4("Shows the number of boats completing the course By Military Arm. Please note that Reserves are also included in their main arm"))),
            fluidRow(column(12, plotOutput("MattendanceLT"))),
            fluidRow(column(12, tags$h4("Shows the number of boats completing the course By Sex, Boat type and Age Group"))),
            #     fluidRow(column(12, plotOutput("MattendanceSLOPT"))),
            #     fluidRow(column(12, tags$h4("Shows the number of boats completing the course By Boat Type"))),
            #    fluidRow(column(12, plotOutput("MattendanceSLOT"))),
            #   fluidRow(column(12, tags$h4("Shows the number of boats completing the course By Age Group"))),
            fluidRow(column(12, plotOutput("MattendanceSLToT"))),
            fluidRow(column(12, tags$h4("Shows the number of boats completing the course for other Subclasses"))),

            #         fluidRow(column(12, plotOutput("MattendanceMP"))),
            #        fluidRow(column(12, tags$h4("Shows the Positions of boats completing the course")))
          )
        )
      ),
      tabPanel(
        "DW Winning Times",
        fluidRow(
          column(10, tags$h2("DW and Waterside Results Database - Winning Times")),
          column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
        ),
        fluidRow(
          # column(12, tags$h4("DW Winning Times")),
          column(3, selectInput("atttimeclass", "Choose Class", c(unique(divsclass$Class)), selected = "Senior", multiple = TRUE)),
          column(3, selectInput("Position", "Choose Position:", c(unique(divsposition$Position)), selected = "1", multiple = FALSE)),
          column(3, selectInput("attnyear", "Plot after Year", c(unique(menus$Year)), selected = "1948", multiple = FALSE))
        ),
        fluidRow(column(12, plotOutput("Time"))),
        fluidRow(column(12, tags$h4("Shows Times for Place over selected years.  NB Before 1971 No bank feeding was permitted.  The Endeavour Crews have been included to illustrate the fastest time for the Class.  The 2022 event stopped at Teddington"))),
        fluidRow(column(12, plotOutput("TimePlace"))),
        fluidRow(column(12, tags$h4("The box Plot shows the results for the nominated place (eg 1st), for the selected Class and Years, divided into 4 groups.
                                         The box includes 50% on the results. The top and bottom part of the box include 25% each.  The remaining 25%'s are shown by the vertical lines, top and bottom.
                                         Outliers are shown by dots.  The average (mean) is shown by the black circle.  Figures for the horizontal lines of the box are shown below for each class. "))),
        column(2, tags$h4("Endeavour")),
        column(2, tags$h4("Junior")),
        column(2, tags$h4("Senior")),
        column(2, tags$h4("Singles")),
        column(4, tags$h4("Vet/Junior")),
        column(2, verbatimTextOutput("SummaryEnd")),
        column(2, verbatimTextOutput("SummaryJun")),
        column(2, verbatimTextOutput("SummarySen")),
        column(2, verbatimTextOutput("SummarySin")),
        column(2, verbatimTextOutput("SummaryVJun")),
      ),
      tabPanel(
        "DW Trophies",
        tabsetPanel(
          tabPanel(
            "DW Trophy Table",
            fluidRow(
              column(10, tags$h2("DW and Waterside Results Database- Trophy Times")),
              column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
            ),
            fluidRow(
              #   column(12, tags$h4("Trophy Times")),
              column(3, selectInput("attTrophytimeclassT", "Choose Class", c(unique(divsclass$Class)), selected = "Senior", multiple = FALSE)),
              column(3, selectInput("attTrophiesT", "Choose Trophy:", TrophyList, selected = "Reserve", multiple = FALSE))
            ),
            fluidRow(column(12, DT::dataTableOutput("tableCrewsRT"))),
            fluidRow(column(12, tags$h4("Senior Doubles Veteran is for the Oldest competitor to complete the course,
                                     Lee is over 35,  Centuary is both paddlers over 50. The Services Trophy may be won by a regular in either Army, Navy or RAF.")))
          ),
          tabPanel(
            "DW Trophy Chart",
            fluidRow(
              column(10, tags$h2("DW and Waterside Results Database - Trophy Chart")),
              column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
            ),
            fluidRow(
              column(3, selectInput("attTrophytimeclass", "Choose Class", c(unique(divsclass$Class)), selected = "Senior", multiple = FALSE)),
              column(3, selectInput("attTrophies", "Choose Trophy:", TrophyList, selected = "Reserve", multiple = FALSE))
            ),
            fluidRow(column(12, plotOutput("TrophyTime"))),
            fluidRow(column(12, tags$h4("Where a Trophy is selected all trophies won by the crew are shown. Senior Doubles Veteran is for the Oldest competitor to complete the course,  Lee is over 35,  Centuary is both paddlers over 50. The Services Trophy may be won by a regular in either Army, Navy or RAF.")))
          )
        )
      ),
      tabPanel(
        "DW Visitors",
        tabsetPanel(
  
          tabPanel(
            "DW Visitors Table",
            fluidRow(
              column(10, tags$h2("DW and Waterside Results Database - Visitors Table")),
              column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
            ),
            # fluidRow(column(12, tags$h4("DW Visitors Table."))),
            fluidRow(column(12, DT::dataTableOutput("tableVisitors")))
          ),
          tabPanel(
            "DW Visitors Chart",
            fluidRow(
              column(10, tags$h2("DW and Waterside Results Database - Visitors Chart")),
              column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
            ),
            #    fluidRow(
            #      column(12, tags$h4("DW Visitor Countries"))),
            fluidRow(column(12, plotOutput("Visitor")))
          )
        )
      ),
      tabPanel(
        "DW Distribution",
        tabsetPanel(
          tabPanel(
            "DW Class Distribution",
            fluidRow(
              column(10, tags$h2("DW and Waterside Results Database - Class Distribution")),
              column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
            ),
            fluidRow(
              column(12, tags$h4("Distribution shows the  range of times for the Year and Class.  The 2022 event stopped at Teddington")),
              column(3, selectInput("attnyear2", "Choose one or more Years", c(unique(menus$Year)), selected = "2023", multiple = TRUE)),
              column(3, selectInput("atttimeclass1", "Choose one or more Classes", c(unique(divsclass$Class)), selected = "Senior", multiple = TRUE)),
              fluidRow(column(12, plotOutput("Distribution"))),
              column(3, tags$h4("Senior")),
              column(3, tags$h4("Junior")),
              column(3, tags$h4("Singles")),
              column(3, tags$h4("Vet/Junior")),
              column(3, verbatimTextOutput("SummarySenCrewD")),
              column(3, verbatimTextOutput("SummaryJunCrewD")),
              column(3, verbatimTextOutput("SummaryVJCrewD")),
              column(3, verbatimTextOutput("SummarySinCrewD"))
     #         column(3, verbatimTextOutput("SummaryEndCrewD"))
            ),
            fluidRow(column(12, tags$h4("The box plot(3) shows statistical data in which a rectangle is drawn to represent the second and third quartiles with a horizontal line inside to indicate the median value. The lower and upper quartiles are shown as vertical lines top and bottom of the rectangle.  Points above or below the vertical lines are outliers; perhaps because they missed the tide")))
          ),
          tabPanel(
            "DW SubClass Distribution",
            fluidRow(
              column(10, tags$h2("DW and Waterside Results Database - SubClass Distribution")),
              column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
            ),
            fluidRow(
              column(12, tags$h4("Distribution shows the  range of times for the Year and Class")),
              column(3, selectInput("attnyear2SC", "Choose one or more Years", c(unique(menus$Year)), selected = "2023", multiple = TRUE)),
              column(3, selectInput("atttimeclass1SC", "Choose one or more Classes", c(unique(divsclass$Class)), selected = "Senior", multiple = TRUE)),
              fluidRow(column(12, plotOutput("SubClassDistribution"))),
              fluidRow(column(12, tags$h4("The box plots show statistical data in which a rectangle is drawn to represent 50% of results and with a horizontal line to indicate the median value. 
                                          The lower and upper 25% are shown as vertical lines top and bottom of the rectangle.  Points above or below the vertical lines are outliers; perhaps because they missed the tide"))),
              
              fluidRow(column(6, tags$h3("Military")),
              column(6, tags$h3("BoatType"))),
              fluidRow(column(2, tags$h4("Army")),
              column(2, tags$h4("Navy")),
              column(2, tags$h4("RAF")),
              column(2, tags$h4("Canadian")),
              column(2, tags$h4("Folding Boat")),
              column(2, tags$h4("Kayak"))),
              fluidRow(column(2, verbatimTextOutput("SummaryMilitaryA")),
              column(2, verbatimTextOutput("SummaryMilitaryN")),
              column(2, verbatimTextOutput("SummaryMilitaryR")),
              column(2, verbatimTextOutput("SummaryBoatTypeC")),
              column(2, verbatimTextOutput("SummaryBoatTypeF")),
              column(2, verbatimTextOutput("SummaryBoatTypeK"))),
              fluidRow(column(6, tags$h3("Ladies")),
                       column(6, tags$h3("Veteran"))),
              fluidRow(column(2, tags$h4("Ladies")),
                       column(2, tags$h4("Male")),
                       column(2, tags$h4("Mixed")),
                       column(2, tags$h4("Century - 50 & over")),
                       column(2, tags$h4("Senior")),
                       column(2, tags$h4("Veteran - 35 & over"))),
              fluidRow(column(2, verbatimTextOutput("SummaryLadiesL")),
                       column(2, verbatimTextOutput("SummaryLadiesM")),
                       column(2, verbatimTextOutput("SummaryLadiesMix")),
                       column(2, verbatimTextOutput("SummaryVeteranC")),
                       column(2, verbatimTextOutput("SummaryVeteranS")),
                       column(2, verbatimTextOutput("SummaryVeteranV")))
            ),
            )
        )
      ),
      tabPanel(
        "DW Completions Data",
        tabsetPanel(
          tabPanel(
            "DW Senior Completion Rate",
            fluidRow(
              column(10, tags$h2("DW and Waterside Results Database - Senior Completion Rates")),
              column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
            ),
            fluidRow(column(12, tags$h4("The 2022 Event stopped at Teddington"))),
            fluidRow(column(12, plotOutput("CompletionR"))),
            fluidRow(column(12, plotOutput("CompletionR1"))),
            column(12, tags$h4("These plots suggest the largest factor in low completion rates is a strong head wind. Temperatures below zero also affect completion rates.  Flow rates either high or
                                low do not seem to be critical provided the race is considered safe to complete"))
          ),
          tabPanel(
            "DW Race Numbers",
            fluidRow(
              column(10, tags$h2("DW and Waterside Results Database - Race Numbers")),
              column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
            ),
            column(3, selectInput("ResultsTypeC", "Choose one or more results", c(unique(ResultType$Result)), selected = "Doubles", multiple = TRUE)),
            fluidRow(column(12, plotOutput("DWRN1"))),
            column(12, tags$h4("The graph can show the number entered, the number completed and the number retired for each class.  You may select completed and retired together to get the number
         entered or use the entered value.  You may also select the numbers for all classes to get totals."))
          ),
          tabPanel(
            "DW Race Numbers Table",
            fluidRow(column(12, DT::dataTableOutput("RNDtable")))
          ),
          tabPanel(
            "DW Retirement Locations",
            fluidRow(
              column(10, tags$h2("DW and Waterside Results Database - Retirement Locations")),
              column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
            ),
            column(3, selectInput("RetireYearS", "Choose one or more Years", c(unique(RetireYearNew$Year)), selected = c(unique(RetireYearNew$Year)), multiple = TRUE)),
            fluidRow(column(12, plotOutput("RetirePlot"))),
            column(12, tags$h4("The plot shows the number of retirements by location.  The location reported may be recorded by the crew or their support team.  Normally the last check point passed will be the recorded location and thus retirements recorded on the tideway are unusual.
                              "))
          ),
          tabPanel(
            "DW Retirements Table",
            fluidRow(column(12, DT::dataTableOutput("RetireTable")))
          ),
          tabPanel(
            "DW About Sex and Age",
            fluidRow(column(6, selectInput("SAclass", "Choose Class", c(unique(menus$Class)), selected = "Senior", multiple = TRUE)),
            column(6, selectInput("AgeYearS", "Choose one or more Years", c(unique(AgeYear$Year)), selected = c(unique(AgeYear$Year)), multiple = TRUE))),
          
            fluidRow(column(12, plotOutput("SexAge1"))),
            column(12, tags$h4("The plots show the range of positions and times by the age of competitors.")),
            fluidRow(column(12, plotOutput("SexAge2"))),
          
          )
        )
      
    ),
#---------------

tabPanel(
  HTML('<span style="color: red;font-weight: bold;"> Advice</span>'),
  tabsetPanel(
    tabPanel(
      "A Paddlers Advice",
 #    fluidRow(column(12, tags$h5("This is a paddlers view"))),
      tags$iframe(style = "height:1000px; width:100%", src = "A Paddlers Advice for DW crews.pdf")),
    
    tabPanel(
      "DW Predicted Times Explained",
 #    fluidRow(column(12, tags$h4("The graph can show the number entered."))),
      tags$iframe(style = "height:1000px; width:100%", src = "DW Predicted Times Data Explained.pdf")),
 
   tabPanel(
      "Standard  Speed Table",
      #    fluidRow(column(12, tags$h4("The graph can show the number entered."))),
      tags$iframe(style = "height:1000px; width:100%", src = "Predicted Times Table.pdf")),
 
    tabPanel(
      "Start and Finish Time Profile",
      #    fluidRow(column(12, tags$h4("The graph can show the number entered."))),
      tags$iframe(style = "height:1000px; width:100%", src = "Start and Finish Times Senior Example.pdf"))
      
))),
 
#-------------------

    navbarMenu(
      HTML('<span style="color: blue;font-weight: bold;"> Waterside Section</span>'),
 #     HTML("<span style='color: red; font-weight: bold;'>DW Section</span>"), #Change to red and bold
  #      "Waterside Section",
      
      tabPanel(
        "Waterside Results",
        tabsetPanel(
          tabPanel(
            "WaterSide Data Set",
            fluidRow(column(12, tags$h4("WaterSide Data Set - Please click choices and press Delete to change. "))),
            fluidRow(column(12, tags$h4("Please click choices and press Delete to change"))),
            fluidRow(
              column(3, selectInput("WSYear", "Pick one or more Years", c("All", unique(wsmenus$Year)), selected = "All", multiple = FALSE)),
              column(3, selectInput("WSCCRace", "Choose one or more Races", WatersideRaceL, selected = "All", multiple = FALSE)),
              column(3, selectInput("WScl", "Choose one or more Classes", c("All", unique(wsmenus$WSClass)), selected = "All", multiple = FALSE)),
              column(3, selectInput("WPLCC", "Choose Position:", c("All", unique(wsdivsposition$WSPosition)), selected = "All", multiple = FALSE))
            ),
            fluidRow(column(12, DT::dataTableOutput("WStable")))
          ),
          tabPanel(
            "WaterSide Crews",
            fluidRow(column(12, tags$h4("WaterSide Crews - Please click choices and press Delete to change. "))),
                    fluidRow(
              column(6, selectInput("WSCCRaceNC", "Choose one or more Races", WatersideRaceL, selected = "All", multiple = FALSE)),
              column(6, selectInput("WSclNC", "Choose one or more Classes", c("All", unique(wsmenus$WSClass)), selected = "All", multiple = FALSE))
            ),
            fluidRow(column(12, DT::dataTableOutput("WStableCrews")))
          ),
          tabPanel(
            "WaterSide Clubs",
            fluidRow(column(12, tags$h4("WaterSide Clubs - Please click choices and press Delete to change. "))),
            fluidRow(
              column(3, selectInput("WSYearNCl", "Pick one or more Years", c("All", unique(wsmenus$Year)), selected = "All", multiple = FALSE)),
              column(3, selectInput("WSCCRaceNCl", "Choose one or more Races", WatersideRaceL, selected = "All", multiple = FALSE)),
              column(3, selectInput("WSclNCl", "Choose one or more Classes", c("All", unique(wsmenus$WSClass)), selected = "All", multiple = FALSE))
            ),
             fluidRow(column(12, DT::dataTableOutput("WStableClubs")))
          ),
          tabPanel(
            "WaterSide Times by Year",
            fluidRow(column(12, tags$h4("WaterSide Times by Year - Please click choices and press Delete to change. "))),
            fluidRow(
              column(3, selectInput("WSYearNT", "Pick one or more Years", c("All", unique(wsmenus$Year)), selected = "All", multiple = FALSE)),
              column(3, selectInput("WSCCRaceNT", "Choose one or more Races", WatersideRaceL, selected = "All", multiple = FALSE)),
              column(3, selectInput("WSclNT", "Choose one or more Classes", c("All", unique(wsmenus$WSClass)), selected = "All", multiple = FALSE))
            ),
           fluidRow(column(12, DT::dataTableOutput("WStableTimeYear")))
          ),
          tabPanel(
            "WaterSide Time",
            fluidRow(column(12, tags$h4("WaterSide Time - Please click choices and press Delete to change. "))),
            #  fluidRow(column(3,selectInput("WSYearT","Pick one or more Years",c("All",unique(wsmenus$Year)),selected = "All",multiple = TRUE)),
            fluidRow(
              column(6, selectInput("WSCCRaceT", "Choose one or more Races", WatersideRaceL, selected = "All", multiple = FALSE)),
              column(6, selectInput("WSclT", "Choose one or more Classes", c("All", unique(wsmenus$WSClass)), selected = "All", multiple = FALSE))
            ),
            fluidRow(column(12, DT::dataTableOutput("WStableTime")))
          ),
          tabPanel(
            "WaterSide Year",
            fluidRow(column(12, tags$h4("WaterSide Year - Please click choices and press Delete to change. "))),
            fluidRow(
              column(3, selectInput("WSYearY", "Pick one or more Years", c("All", unique(wsmenus$Year)), selected = "All", multiple = FALSE)),
              column(3, selectInput("WSCCRaceY", "Choose one or more Races", WatersideRaceL, selected = "All", multiple = FALSE)),
              column(3, selectInput("WSclY", "Choose one or more Classes", c("All", unique(wsmenus$WSClass)), selected = "All", multiple = FALSE))
            ),
           fluidRow(column(12, DT::dataTableOutput("WStableYear")))
          )
        )
      ),
      tabPanel(
        "WS Paddler History",
        fluidRow(
          column(10, tags$h2("DW and Waterside Results Database - WS Paddler History")),
          column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
        ),
        fluidRow(
          column(6, textInput("WSpaddler", "Enter Paddler - Last Name  First Name/Initial", value = "")),
          column(6, selectInput("WSPRace", "Choose one or more Races", WatersideRaceL, selected = "A", multiple = FALSE))
        ),
        fluidRow(column(3, tags$h4("WS Positions"), tags$h4(textOutput("WS top3s")), tags$h4(textOutput("WS medals")))),
        fluidRow(column(12, plotOutput("WSPositions"))),
        fluidRow(column(12, tags$h4("If no paddler is selected all paddlers will be shown."))),
        fluidRow(column(12, plotOutput("WSTimePlacePositions1"))),
        fluidRow(column(12, tags$h4("The box Plot shows the results for the nominated Crew(s). All Races - A B C D are shown in this section.  Individual classes are shown. "))),
        column(3, tags$h4("Race A")),
        column(3, tags$h4("Race B")),
        column(3, tags$h4("Race C")),
        column(3, tags$h4("Race D")),
        column(3, verbatimTextOutput("SummaryWSACrew")),
        column(3, verbatimTextOutput("SummaryWSBCrew")),
        column(3, verbatimTextOutput("SummaryWSCCrew")),
        column(3, verbatimTextOutput("SummaryWSDCrew")),
        fluidRow(column(12, tags$h4("The tables show results for the combined classes.  While the average (mean) and median can be the same or nearly the same, they are different if more of the data values are clustered toward one end of their range and/or if there are a few extreme values. ... Under these circumstances, median gives a better representation of central tendency than average."))),
      ),
      tabPanel(
        "WS Club History",
        fluidRow(
          column(10, tags$h2("DW and Waterside Results Database - WS Club History")),
          column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
        ),
        # fluidRow(column(12, tags$h4("WS Club History")),
        fluidRow(
          column(12, tags$h3(textOutput("WS Clubname"), tags$h4("Note: A Position of 0 indicates a completion outside the normal race rules"))),
          column(6, textInput("WSregclub", "Enter Club - The Name is Case Sensitive", value = "")),
          column(6, selectInput("WSPCRace", "Choose one or more Races", WatersideRaceL, selected = "A", multiple = FALSE))
        ),
        fluidRow(column(12, plotOutput("WSClubPositions"))),
        fluidRow(column(12, plotOutput("WSclubpositionsN1"))),
        fluidRow(column(12, tags$h4("The box Plot shows the results for the nominated Club(s). All Races - A B C D are shown in this section.  Individual classes are shown. "))),
        column(3, tags$h4("Race A")),
        column(3, tags$h4("Race B")),
        column(3, tags$h4("Race C")),
        column(3, tags$h4("Race D")),
        column(3, verbatimTextOutput("SummaryWSAClub")),
        column(3, verbatimTextOutput("SummaryWSBClub")),
        column(3, verbatimTextOutput("SummaryWSCClub")),
        column(3, verbatimTextOutput("SummaryWSDClub")),
        fluidRow(column(12, tags$h4("The tables show results for the combined classes.  While the average (mean) and median can be the same or nearly the same, they are different if more of the data values are clustered toward one end of their range and/or if there are a few extreme values. ... Under these circumstances, median gives a better representation of central tendency than average."))),
      ),
      tabPanel(
        "WS Clubs",
        fluidRow(
          column(10, tags$h2("DW and Waterside Results Database - Waterside Clubs")),
          column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
        ),
        #      fluidRow(column(12, tags$h4("WaterSide Clubs")),
        fluidRow(
          column(12, tags$h3(textOutput("WS Clubs"))),
          column(3, selectInput("attWSclubyear", "Pick Year(s)", c(unique(wsmenus$Year)), selected = "2023", multiple = TRUE)),
          column(6, selectInput("WSCRace", "Choose one or more Races", WatersideRaceL, selected = "A", multiple = FALSE))
        ),
        fluidRow(column(12, plotOutput("WSclubplot")))
      ),
      tabPanel(
        "WS Attendance",
        fluidRow(
          column(10, tags$h2("DW and Waterside Results Database - WS Attendance by Race and Class")),
          column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
        ),
        fluidRow(
          column(12, tags$h4("Please click choices and press Delete to change")),
          column(6, selectInput("WSclass", "Choose one or more Classes", c(unique(wsdivs$WSClass)), selected = "K2 Senior", multiple = TRUE)),
          column(6, selectInput("wsattyear", "Pick one or more Years", c("All", unique(wsmenus$Year)), selected = "2023", multiple = TRUE))
        ),
        fluidRow(column(12, plotOutput("WSattendance")))
      ),
      tabPanel(
        "WS Class by year",
        fluidRow(
          column(10, tags$h2("DW and Waterside Results Database - WS Class by Year")),
          column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
        ),
        fluidRow(
          column(12, tags$h4("Please click choices and press Delete to change")),
          column(6, selectInput("WS2class", "Choose one or more Classes", c(unique(wsdivs$WSClass)), selected = "K2 Senior", multiple = TRUE)),
          column(6, selectInput("WSRace2", "Choose one or more Races", WatersideRaceL, selected = "A", multiple = TRUE))
        ),
        fluidRow(column(12, plotOutput("WS1attendance1")))
      ),
      tabPanel(
        "WS Winning Times",
        fluidRow(
          column(10, tags$h2("DW and Waterside Results Database - WS Winning Times")),
          column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
        ),
        fluidRow(
          #       column(12, tags$h4("WS Winning Times")),
          column(3, selectInput("attWStimeclass", "Choose Class", c(unique(wsdivsclass$WSClass)), selected = "K2 Senior", multiple = TRUE)),
          column(3, selectInput("WSPosition", "Choose Position:", c(unique(wsdivsposition$WSPosition)), selected = "1", multiple = FALSE)),
          column(3, selectInput("attWSnyear", "Plot after Year", c(unique(menus$Year)), selected = "1990", multiple = FALSE)),
          column(3, selectInput("WSWSRace2", "Choose Race", WatersideRaceL, selected = "A", multiple = FALSE))
        ),
        fluidRow(column(12, plotOutput("WSTime"))),
        fluidRow(column(12, tags$h4("Shows Times for Place over selected years.  NB Waterside Races Started in 1968.  Results are not available for all Years")))
      ),

      # column(3,selectInput("MaxA","With Race A Time below (hours)",WatersideATimeList,selected = "2.5",multiple = FALSE))

      tabPanel(
        "WS A, B, C, D,Comparison",
        fluidRow(
          column(10, tags$h2("DW and Waterside Results Database -  WS A B C D Comparison")),
          column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
        ),
        fluidRow(
          column(12, tags$h4("These plots are of all crews and compare WS times for A B C & D in the selected year.  The plots require data for all races in the selected year or an error message will be generated")),
          column(12, tags$h4("Plotting is not available for 1998 (No Race C), 2001 (No Races), 2012 (No Race A), 2014 (No Race B), 2018, 2020 & 2021 (No Results)")),
          column(6, selectInput("comattyear", "Pick Year", c(unique(wsmenus$Year)), selected = "2023", multiple = FALSE)),
          column(3, selectInput("MaxA", "With Race A Time below (hours)", WatersideATimeList, selected = "2.5", multiple = FALSE))
        ),
        fluidRow(column(12, plotOutput("WCompGraphs"))),
        column(12, tags$h4("Faster boats are more predictable. As the time for Waterside A is reduced the plotted line appears as a better fit. These plots compare times for Waterside A against Waterside B,C and D.  The regression line has been plotted with the intercept to zero"))
        ),
      tabPanel(
        "WS & DW Crews Table",
        fluidRow(
          column(10, tags$h2("DW and Waterside Results Database")),
          column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
        ),
        fluidRow(column(12, tags$h4("The Display Lists WS Times with DW Time for crews in each year where available"))),
        fluidRow(column(12, DT::dataTableOutput("tableABCDWS")))
      ),
      tabPanel(
        "WS Distribution",
        fluidRow(
          column(10, tags$h2("DW and Waterside Results Database - WS Distribution")),
          column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
        ),
        fluidRow(
          column(12, tags$h4("Distribution shows the  range of times for the Year, Race and Class")),
          #  column(12, tags$h4("WS Distribution")),
          column(3, selectInput("attnwsyear2", "Choose one or more Years", c(unique(wsmenus$Year)), selected = "2023", multiple = TRUE)),
          column(3, selectInput("attDrace", "Choose Race", c(unique(wsmenus$WSRace)), selected = "A", multiple = FALSE)),
          column(3, selectInput("attWSclass1", "Choose one or more Classes", c(unique(wsmenus$WSClass)), selected = "K2 Senior", multiple = TRUE)),
          fluidRow(column(12, plotOutput("WSDistribution")))
        ),
        fluidRow(column(12, tags$h4("The box plot(2) shows statistical data in which a rectangle is drawn to represent the second and
                                         third quartiles (a quartile is 25% of results) with a horizontal line inside to indicate the median value. The lower and upper quartiles are shown as vertical lines top and
                                         bottom of the rectangle.  Points above or below the vertical lines are outliers")))
      )
    ),
    navbarMenu(
      "DW times - WS, Flow and Weather",
      tabPanel(
        "DW Time vs Place",
        fluidRow(
          column(10, tags$h2("DW and Waterside Results Database - DW time vs Place")),
          column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
        ),
        #   fluidRow(column(12, tags$h4("DW time vs Place"))),
        column(3, selectInput("attclassTP", "Choose Class", c(unique(divsclass$Class)), selected = "Senior", multiple = FALSE)),
        column(3, selectInput("attyearTP", "Pick one or more Years", c(unique(menus$Year)), selected = "2023", multiple = TRUE)),
        #   column(3,selectInput("PositionTP1", "Choose Position:", c(unique(divsposition$Position)), selected = "100", multiple = FALSE)),
        #    column(3,selectInput("PositionTP1", "Limit display up to Position:", c(unique(divsposition$Position)), selected = "100", multiple = FALSE)),
        fluidRow(column(12, plotOutput("GraphsTP"))),
        fluidRow(column(12, plotOutput("GraphsTPL"))),
        fluidRow(column(12, tags$h4("These charts illustrate that different years show different profiles which can be a measure of the race conditions.
                                                A sharp increase in times for those at the back of the field suggest they missed the tide and were caught by the tide window.
                                                The number of boats displayed may be a reflection of the number entered or a lower completion rate")))
      ),
      tabPanel(
        "DW 1st vs Weather",
        fluidRow(
          column(10, tags$h2("DW and Waterside Results Database - Weather Factors affecting Winning Times")),
          column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
        ),
        #  fluidRow(column(12, tags$h4("Weather Factors affecting Winning Times"))),
        fluidRow(column(12, plotOutput("Graphs"))),
        fluidRow(column(12, tags$h4("These charts illustrate that for the Winning Senior Crew the most important factor for race times is flow.
                                         Strong winds will also affect times and completion rates.  NB Wind direction or duration is not known")))
      ),
      tabPanel(
        "DW 1st vs Flow",
        fluidRow(
          column(10, tags$h2("DW and Waterside Results Database")),
          column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
        ),
        fluidRow(
          column(12, tags$h4("How Flow, Temperature and Wind can both affect completion rates and DW Winning Times.  These charts allow you to select the different weather parameters.")),
          column(3, selectInput("attnfyear", "Select a Year after 1970 for plot", c(unique(menus$Year)), selected = "1970", multiple = FALSE)),
          column(3, selectInput("comrate", "With Completion Rate above", c(unique(Completion$CompletionRate)), multiple = FALSE)),
          column(3, selectInput("windg", "With Wind below", c(unique(Wind$MaxWind)), selected = "46.5", multiple = FALSE)),
          column(3, selectInput("tempg", "With Temp above", c(unique(Temp$MinTemperature)), selected = "-2", multiple = FALSE))
        ),
        fluidRow(column(12, plotOutput("FGraphs"))),
        fluidRow(column(12, tags$h4("The formula on charts indicate that first place with no flow would be about 18.5 hours.  100cm/sec flow reduces the time by 1.59 hours
                                                (Before your changes). You may change the parameters for these plots but weather is not available before 1970.")))
      ),
      tabPanel(
        "DW Weather Table",
        fluidRow(column(12, tags$h4("The Regression Number, when multipled by the WS time, gives the average time for Senior Crews in DW for the year and conditions"))),
        fluidRow(column(12, tags$h4("EasterOffset is a measure of an early or late Easter - The date of Easter can vary by as much as 31 days.  High Tide is quoted as GMT, the race may be either GMT or BST"))),
        fluidRow(column(12, DT::dataTableOutput("Wtable")))
      ),
      tabPanel(
        "DW Weather Box Plots",
        fluidRow(
          column(10, tags$h2("DW and Waterside Results Database - Weather Box Plots Showing the Range of conditions")),
          column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
        ),

        #       fluidRow(column(12, tags$h4("Weather Box Plots Showing the Range of conditions"))),
        fluidRow(column(12, plotOutput("WeatherBox"))),
        fluidRow(column(12, tags$h4("Box plots show statistical data where a rectangle is drawn to represent the second and third quartiles with a horizontal line inside to
indicate the MEDIAN value. The lower and upper quartiles are shown as vertical lines top and bottom of the rectangle.  Points above or below the vertical lines are outliers.
Mean, MEDIAN, and mode are three kinds of averages. ... The mean is the average you're used to, where you add up all the values and then divide by the number of values. The MEDIAN is the middle value in the list of numbers.
                                         ")))
      ),


      ### New Section EasterOffset

      tabPanel(
        "DW Easter Offset",
        fluidRow(
          column(10, tags$h2("DW Easter Offset")),
          column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
        ),
        fluidRow(column(12, tags$h4("Easter Offset is a measure of an early or late Easter - The date of Easter can vary by as much as 31 days"))),
        fluidRow(column(12, plotOutput("GraphsEasterO"))),
        fluidRow(column(12, tags$h4("These plots are presented to demonstrate what effect, if any, the date of Easter has on the event. Of course the earlier Easter is the less daylight there may be and the earlier training and other build up races will be.  I will leave it to the reader to determine if they believe there is any significant effect."))),
        fluidRow(column(12, tags$h4("Thus is the minimum temperature lower earlier in the year? Do you see more strong winds earlier in the year? Lastly does flow tend to be faster earlier in the year?"))),
        fluidRow(column(12, tags$h4("A Colour code has  been added to the Completion Rate chart.  This tends to demonstrate that low completion rates (60% or less) may be related to slower times.  It is interesting to see that these slower times do fall into an apparent line and thus perhaps the date or Easter may be relevant in hard years.  Logically strong head winds with cold temperature and/or poor flow might be expected to create a slower year"))),
        fluidRow(column(12, tags$h4("Please note that data for these graphs, other than completion rate, are for the winning senior crew only.  FirstSenior is the winning time.")))
      ),
      tabPanel(
        "DW Flow vs Year",
        fluidRow(
          column(10, tags$h2("DW and Waterside Results Database")),
          column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
        ),
        fluidRow(column(12, tags$h4("DW Weather vs Year with the River Thames Status colour codes"))),
        fluidRow(column(12, plotOutput("WeatherYearB"))),
        fluidRow(column(12, tags$h4("A Green Status is defined as less than 65 cm/sec.  Amber is Less than 100 cm/sec and Red is above 120 cm/sec with Red/Amber (shown as Orange) between 99 and 120.
 The official record was set in 2023. Please be aware that flow rates change over the race weekend and the chart is most representative of the Senior event.
                                    Black indicates no race - details below"))),
        fluidRow(column(12, tags$h5("2000	Race Abandoned Old Windsor - Unofficial Course Record"))),
        fluidRow(column(12, tags$h5("2001	Foot & Mouth Disease Prevents Race"))),
        fluidRow(column(12, tags$h5("2016	Race Shortened to Teddington for 4 day race (High Winds)"))),
        fluidRow(column(12, tags$h5("2018	Race Abandoned Reading (High Flow)"))),
        fluidRow(column(12, tags$h5("2020	Coronavirus Prevents Race"))),
        fluidRow(column(12, tags$h5("2021	Coronavirus Prevents Race"))),
        fluidRow(column(12, tags$h5("2023	Restricted Senior Race with all 4 Day crews stopping at Reading"))),
        fluidRow(column(12, tags$h4("Kingston Flow Rate Web Site", h5("Shoothill Gauge Map", a("Current Reading", target = "_blank", href = "http://www.gaugemap.co.uk/#!Map/Summary/1249/1382")))))
      ),

      # A Green Status is defined as less than 65 cm/sec.  Amber is Less than 100 cm/sec and Red is above 99 cm/sec with Red/Amber between 99 and 120.  Note that the unofficial record was set in 2000 but the race was abandoned.  The race was cancelled in 2001 due to a foot and mouth outbreak.  In 2018 the race was stopped at Reading. The official record was set in 1979.

      tabPanel(
        "DW Times vs Waterside D",
        fluidRow(
          column(10, tags$h2("DW and Waterside Results Database")),
          column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
        ),
        fluidRow(
          column(12, tags$h4("Enter your time for Waterside D.  The line will then show the average times for Senior crews depending on flow.  You can vary the conditions to see if there is any effect")),
          column(3, selectInput("WSDT", "Please select your nearest WS D time", WatersideDTimeList, selected = "5.0", multiple = FALSE)),
          column(3, selectInput("comrateR", "With Completion Rate above", c(unique(Completion$CompletionRate)), multiple = FALSE)),
          column(3, selectInput("windgR", "With Wind below", c(unique(Wind$MaxWind)), selected = "46.5", multiple = FALSE)),
          column(3, selectInput("tempgR", "With Temp above", c(unique(Temp$MinTemperature)), selected = "-2", multiple = FALSE))
        ),
        fluidRow(column(12, plotOutput("FGraphsR"))),
        fluidRow(column(12, tags$h4("The formula and line shown on the chart indicates the typical time for a Senior crew with different conditions and is based on hundreds of crews over many years.
                                         The conditions on Waterside D will be a major factor in these indicators and thus a tail wind and a full canal will reduce race times. This will then affect predicted times for DW.
                                         It follows that the conditions on DW will also be a major factor in times and again a tail wind and a full canal may improve your time but of course a strong head wind will lead to slower times"))),
        fluidRow(column(12, tags$h4("The objective is to demonstrate what times might be expected in the prevailing conditions depending on your WS D time.
                                         The points on the chart are plotted from the regression formula (best fit formula). These are shown on the DW Weather tab.  They show the value when a race was run under the parameters set.
                                     The regression points will be above or below the line depending on the race conditions in the specific years shown.  The line is the average of all races."))),
        fluidRow(column(12, tags$h4("The points on the chart are NOT race times.  They are a predicted time based on all the crews in that year.  It follows that if there were no crews capable doing a fast
                            WS D time in that year there would be no fast crew racing DW that year.  The reality is that to take advantage of the conditions a fast crew must be racing.  You can read a predicted time for any waterside D time by using the left hand vertical scale")))
      ),
      tabPanel(
        "WS & DW Comparison",
        fluidRow(column(10, tags$h2("DW and Waterside Results Database - WS & DW Comparison")), column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))),
        fluidRow(sidebarLayout(
          sidebarPanel(
            fluidRow(
              column(6, selectInput("comattDWyear", "Pick Year", c(unique(wsmenus$Year)), selected = "2023", multiple = FALSE)),
              column(12, tags$h4("Please Note that these plots require data for all races in the selected year or an error message will be generated")),
              column(12, tags$h4("Plotting is not available for 1993, 1998 (No WS Race C), 2001 (No Races), 2012 (No WS Race A), 2014 (No WS Race B), 2018, 2020 & 2021 (No Results)")),
            ),
            fluidRow(column(12, DT::dataTableOutput("Etable"))),
            fluidRow(column(12, tags$h4("The plots compare times for Waterside A,B,C and D against DW with D also shown for the 4 day classes.
                                             The regression line has been plotted with the intercept to zero.")))
          ),
          mainPanel(
            fluidRow(column(12, plotOutput("CompGraphs"))),
            fluidRow(column(12, tags$h4("The plots compare times for Waterside A,B,C and D against DW with D also shown for the 4 day classes.
                                             The regression line has been plotted with the intercept to zero.")))
          )
        ))
      )
    ),
    tabPanel(
      "Portage & Flow Information",
      tabsetPanel(
        tabPanel("Portaging Distances", tags$iframe(style = "height:1000px; width:100%", src = "PortagingDistance.pdf")),
  
        tabPanel(
          "Google Maps",
          tabsetPanel(
            tabPanel(
              "Description", fluidRow(column(12, tags$h4("Google Maps can provide detail on the course. You may Pan and Zoom with these maps to view more detail."))),
              fluidRow(column(12, tags$h4("Click top left (box left of Map name) to show layers and options.  At the bottom left you can then toggle between Satellite and Map."))),
              fluidRow(column(12, tags$h4("You may select a full size map at top right."))),
              fluidRow(column(12, tags$h4("The portage route may be varied by the organisers depending on the year and conditions. You must obey directions from the marshalls.")))
            ),
            tabPanel(
              "Fobney",
              fluidRow(column(12, tags$h4("The get out at Fobney is simple.  Get out before the lock and move towards the bridge over the river on the right.  The best get in is under the bridge.  Care is required to ensure you do not get your rudder stuck in the mud or slip into the river.
                                                            This get in can be difficult as the muddy bank may be under the water but is steep. "))),
              fluidPage(HTML('<embed src="https://www.google.com/maps/d/embed?mid=1h3DFNOUrlZxQskT-Yr96qc3HyBmvbYK2&hl=en" width="860" height="640"></embed>'))
            ),
            tabPanel(
              "County",
              fluidRow(column(12, tags$h4("The get out at County is on a steel grating.  The get in is also on a grating.  When you are ready to set off ensure the front of the boat is pointing towards the centre of the river.  The river can be fast and rough on this section, at least until the bridge but you must get clear of the bank.  A capsize on this stretch is a problem as it will not be easy to get back in the boat without assistance"))),
              fluidPage(HTML('<embed src="https://www.google.com/maps/d/embed?mid=1gs_vEH_TH_FvlW_ZifT1EOmq6A7__rDU&hl=en" width="860" height="640"></embed>'))
            ),
            tabPanel(
              "Blakes",
              fluidRow(column(12, tags$h4("The get out area at Blakes is very short.  Get out before the lock and cross the island.  Remember to turn right on the Thames and go with the flow to the false portage at the Wokingham Waterside Centre"))),
              fluidPage(HTML('<embed src="https://www.google.com/maps/d/embed?mid=1bMZ21s7rNCkMoFpjwVKWHv3AO1fFcheh&hl=en" width="860" height="640"></embed>'))
            ),
            tabPanel(
              "Sonning",
              fluidRow(column(12, tags$h4("Sonning is a simple portage.  The best low get out is near the lock"))),
              fluidPage(HTML('<<embed src="https://www.google.com/maps/d/embed?mid=1AJphnxYKm_i_S5hctLq_MFW_q5JPk4C0&hl=en" width="640" height="480"></embed>'))
            ),
            tabPanel(
              "Shiplake",
              fluidRow(column(12, tags$h4("Get out before the lock and cross the island onto the pontoon"))),
              fluidPage(HTML('<embed src="https://www.google.com/maps/d/embed?mid=1mlC8xzWh40NDovOU4Q63II8jEYoo_LAY" width="860" height="640"></embed>'))
            ),
            tabPanel(
              "Marsh",
              fluidRow(column(12, tags$h4("Approach Marsh on the right hand bank of the river and keep right of the lock gates.  Once on the Pontoon cross the lock and go over the long wooden pathway which will take you to the opposite bank.  You will need to continue on the path past the cottage for some distance. The get in is very high"))),
              fluidPage(HTML('<embed src="https://www.google.com/maps/d/embed?mid=1Q4Tb1UwLBPx4g1-K4vg2r8NLZpOMlOxf&hl=en" width="860" height="640"></embed>'))
            ),
            tabPanel(
              "Hambleden",
              fluidRow(column(12, tags$h4("Approach Hambleden on the right hand bank of the river. The best get out is near the lock gates.  There can be rough water just after the get in with the water from the weir pushing boats to the right bank so care will be required in high flow conditions.  Further on stay to the left of islands until Hurley"))),
              fluidPage(HTML('<embed src="https://www.google.com/maps/d/embed?mid=1Cr4GOgPpQ_iDytn9vS8GRaaw4fVnkF_q&hl=en" width="860" height="640"></embed>'))
            ),
            tabPanel(
              "Hurley",
              fluidRow(column(12, tags$h4("Stay right of river into Hurley and pass under the bridge before getting out on the left bank.  The normal route would cross the island to the canoe portage on the left.  In high flow conditions point the boat into mid stream to avoid overhanging trees.  In some years you may be asked to portage the lock."))),
              fluidPage(HTML('<embed src="https://www.google.com/maps/d/embed?mid=1bBre_BlOaqziVBxcxDp_ckCpAVsJERu2&hl=en" width="860" height="640"></embed>'))
            ),
            tabPanel(
              "Temple",
              fluidRow(column(12, tags$h4("Temple is only 1/2 a mile from Hurley.  Cross the river to the left."))),
              fluidPage(HTML('<embed src="https://www.google.com/maps/d/embed?mid=1VL06M6fXMDUHqAeionFEqJ2bkNIhB732&hl=en" width="860" height="640"></embed>'))
            ),
            tabPanel(
              "Marlow",
              fluidRow(column(12, tags$h4("Aim to go under the centre of Marlow bridge and then to the left of the river. The weir on the right may not be visible because of moored boats!.  When you can see the lock approach the steps on the left but keep clear or the weir.  "))),
              fluidPage(HTML('<embed src="https://www.google.com/maps/d/embed?mid=1dg-S_tuTM4LIEMstBOOS-X9CEi-bRcQk&hl=en" width="860" height="640"></embed>'))
            ),
            tabPanel(
              "Cookham",
              fluidRow(column(12, tags$h4("Cookham can be difficult.  Do not cut the corner when you see Cookham Bridge.  You must be at least in the middle of the bridge or use the right hand arch.  It may well be very dark and a little mist can make the entrance to the lock cut impossible to see.  Approach with care as the weir is on the left.  The lock cut can also be very dark and it is quite long.  The lock itself can be difficult to see as may be the portage."))),
              fluidPage(HTML('<embed src="https://www.google.com/maps/d/embed?mid=1UENnxFmaQBBiIMkDMRLAScFYEduanFPt&hl=en" width="860" height="640"></embed>'))
            ),
            tabPanel(
              "Bolters",
              fluidRow(column(12, tags$h4("Do not use Bolters Lock.  Keep clear of the weir face and then stear left onto the Portage.  Cross the island into the river.  You can go further down the island if you wish.  In high flow make sure you can get out into the middle of the river and avoid any overhanging trees."))),
              fluidPage(HTML('<embed src="https://www.google.com/maps/d/embed?mid=1iIUWw2UwMTjLvSneqjFsqxsbcBN_Let_&hl=en" width="860" height="640"></embed>'))
            ),
            tabPanel(
              "Bray",
              fluidRow(column(12, tags$h4("Stay river left into Bray"))),
              fluidPage(HTML('<embed src="https://www.google.com/maps/d/embed?mid=1p8hplAmqKyqe9xCeR72GHzoeAQ4l4Yam&hl=en" width="860" height="640"></embed>'))
            ),
            tabPanel(
              "Boveney",
              fluidRow(column(12, tags$h4("Portage the rollers to the right of the lock.  As simple as it gets"))),
              fluidPage(HTML('<embed src="https://www.google.com/maps/d/embed?mid=1eEzMn7Vw3Ecj7SyOLkKDQPyVdjX1kkPp&hl=enn" width="860" height="640"></embed>'))
            ),
            tabPanel(
              "Romney",
              fluidRow(column(12, tags$h4("You will know Windsor as you see the castle.  Take the middle of the bridge.  In high flow stear right after the bridge to cut any nasty standing wave at right angles. You may be required to portage the bridge.  The lock cut is quite long but the lock gates may be open for you."))),
              fluidPage(HTML('<embed src="https://www.google.com/maps/d/embed?mid=1AFTdlpoNwaxKi3qoBChVOkN0SsBdvbpI&hl=en" width="860" height="640"></embed>'))
            ),
            tabPanel(
              "Old Windsor",
              fluidRow(column(12, tags$h4("You will need to use the zoom function for this map.  The problem is the weir.  You must be on the right hand bank after the stone bridge.  Boats trapped on this lock caused the race to be aborted in 2000.  The lock cut is long but get out left on the steps before the lock or perhaps as with Romney the lock gates may be open for you."))),
              fluidPage(HTML('<embed src="https://www.google.com/maps/d/embed?mid=1-dXqEdt-9Zwr2BFqwHjCq2cWF-sLkVNn&hl=en" width="860" height="640"></embed>'))
            ),
            tabPanel(
              "Bell",
              fluidRow(column(12, tags$h4("Bell is visible as you approach the weir.  Move to the right to avoid the weir.  Best get out is near the lock gates."))),
              fluidPage(HTML('<embed src="https://www.google.com/maps/d/embed?mid=15M4io_OW4mD8xA1Joe3du5rX-WIefED-&hl=en" width="860" height="640"></embed>'))
            ),
            tabPanel(
              "Penton Hook",
              fluidRow(column(12, tags$h4("Move to the left to avoid the weir.  Best get out is near the lock gates."))),
              fluidPage(HTML('<embed src="https://www.google.com/maps/d/embed?mid=1GZQmJNOKsA1W6C02IsWt-gOzBcgmi--L&hl=en" width="860" height="640"></embed>'))
            ),
            tabPanel(
              "Chertsey",
              fluidRow(column(12, tags$h4("Approach from  the left to avoid the weir but then move right to the island for the canoe portage.  Cross the island to enter the flow"))),
              fluidPage(HTML('<embed src="https://www.google.com/maps/d/embed?mid=11AcD-dtytjg8OCmq5EwrTTtaBCglz_ZA&hl=en" width="860" height="640"></embed>'))
            ),
            tabPanel(
              "Shepperton",
              fluidRow(column(12, tags$h4("Approach from  the left to avoid the weir but then move to the island on the right. Portage near the lock gates.  The get in is high"))),
              fluidPage(HTML('<embed src="https://www.google.com/maps/d/embed?mid=1jYoXPcd475jliyi7fXYQ-xw0qBVYguaF&hl=en" width="860" height="640"></embed>'))
            ),
            tabPanel(
              "Sunbury",
              fluidRow(column(12, tags$h4("Keep right to avoid the weir but then move to the rollers on the left of the lock gates. Rollers can be slippy"))),
              fluidPage(HTML('<embed src="https://www.google.com/maps/d/embed?mid=1jOYDsRyBXLFmuiQHRFanEORJAxHyW7oe&hl=en" width="860" height="640"></embed>'))
            ),
            tabPanel(
              "East Molesey",
              fluidRow(column(12, tags$h4("Keep right to avoid the weir but then move to the rollers on the left of the lock gates. Rollers can be slippy "))),
              fluidPage(HTML('<embed src="https://www.google.com/maps/d/embed?mid=1oYu2esTbyxf8Q5E_-ZONMlfC08PrSmJu&hl=en" width="860" height="640"></embed>'))
            ),
            tabPanel(
              "Teddington",
              fluidRow(column(12, tags$h4("Keep right to avoid the weir but then move to the rollers on the left of the lock gates.   You may wish to stop on the island for support before the tideway.  When getting back in beware using the slipway if you have an understern rudder.  If you use the slipway make sure you have enough deep water to keep your rudder off the bottom.  The safer option is to use the left hand side wall. "))),
              fluidPage(HTML('<embed src="https://www.google.com/maps/d/embed?mid=1Hm8wjhld_juyO_2mmMwvW3Bgn66VREp7&hl=en" width="860" height="640"></embed>'))
            ),
            tabPanel(
              "Richmond Half Lock",
              fluidRow(column(12, tags$h4("You should not need this but if you are late the portage is on the left.  It is slippy.  You must now keep to the right on the tideway until Westminster."))),
              fluidPage(HTML('<embed src="https://www.google.com/maps/d/embed?mid=1MP9F1Dhe5OJBtgtYyJAQgrXa_Q-Fpd1a&hl=en" width="860" height="640"></embed>'))
            )
          )
        ),

        tabPanel(
          "Time Sheet Senior",
          fluidRow(column(12, tags$h5("Find the time for your crew at the first time point.  The time at the next time point should be in the same column  (Plus or Minus 1 column).  Boat speeds will slow up towards Great Bedwyn after the faster start (without portages) but should then be stable till Newbury.  Speeds after Newbury are dependent on flow.  A typical pattern is shown in bold type"))),
          tags$iframe(style = "height:1000px; width:100%", src = "TimeSheet2.pdf")
        ),
        
        tabPanel(
          "River Flow",
          tabsetPanel(
            tabPanel(
              "Description", fluidRow(column(12, tags$h3("Shoothill Gauge Map Tabs are available above for Flow Monitoring stations on the DW course"))),
              fluidRow(column(12, tags$h4("The initial presentation is for a week.  You may change this by using the Graph Options button in the charts.  The flow can vary significantly over time. You may use the mouse on the plot to find the reading over time"))),
              fluidRow(column(12, tags$h4(""))),
              fluidRow(column(12, tags$h3("Other sources of information"))),
              fluidRow(column(12, tags$h4("Shoothill Gauge Map", tags$a(href = "https://www.gaugemap.co.uk/#!Map/Summary/1249/1382", "Click here")))),
              fluidRow(column(12, tags$h4("UK wide flow rates", tags$a(href = "http://www.gaugemap.co.uk/#!Map/Summary/14503/9937", "Click here")))),
              fluidRow(column(12, tags$h4("Teddington source Code from the EA", tags$a(href = "http://environment.data.gov.uk/flood-monitoring/id/measures/3400TH-flow--i-15_min-m3_s", "Click here")))),
              fluidRow(column(12, tags$h4(""))),
              fluidRow(column(12, tags$h4("The link for UK wide flow rates for app users is http://www.riverapp.net")))
            ),
            tabPanel(
              "Kingston/Teddington",
              fluidRow(column(12, tags$h4("The flow varies significantly  around high tide.  It appears flow is held back just before high tide and then released as the tidal flow receeds.  Flow decreases about 1 hour before high tide and reaches a maximum about 1 hour after high tide"))),
              fluidPage(HTML('<embed src="https://www.gaugemap.co.uk/#!Map/Summary/1249/1382" width="660" height="560"></embed>'))
            ),
            tabPanel(
              "Walton",
              fluidRow(column(12, tags$h4(""))),
              fluidPage(HTML('<embed src="https://www.gaugemap.co.uk/#!Map/Summary/1224/1352" width="660" height="560"></embed>'))
            ),
            tabPanel(
              "Stains",
              fluidRow(column(12, tags$h4(" "))),
              fluidPage(HTML('<embed src="https://www.gaugemap.co.uk/#!Map/Summary/1192/1307" width="660" height="560"></embed>'))
            ),
            tabPanel(
              "Maidenhead",
              fluidRow(column(12, tags$h4("In conditions of high water the river will pass to the flood relief Jubilee River just above the flow monitor and weir. "))),
              fluidPage(HTML('<embed src="https://www.gaugemap.co.uk/#!Map/Summary/1132/1230" width="660" height="560"></embed>'))
            ),
            tabPanel(
              "Reading",
              fluidRow(column(12, tags$h4(" "))),
              fluidPage(HTML('<embed src="https://www.gaugemap.co.uk/#!Map/Summary/1070/1139" width="660" height="560"></embed>'))
            ),
            tabPanel(
              "Newbury",
              fluidRow(column(12, tags$h4(" "))),
              fluidPage(HTML('<embed src="https://www.gaugemap.co.uk/#!Map/Summary/1085/1161" width="660" height="560"></embed>'))
            ),
            tabPanel(
              "Mole",
              fluidRow(column(12, tags$h4("This river enters the Thames after Walton"))),
              fluidPage(HTML('<embed src="https://www.gaugemap.co.uk/#!Map/Summary/1245/1377" width="660" height="560"></embed>'))
            )
          )
        )
      )
    ),
    navbarMenu(
      "Split Time Analysis - Senior Doubles",
      tabPanel(
        "DW Winners",
        fluidRow(
          column(10, tags$h2("DW and Waterside Results Database")),
          column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
        ),
        fluidRow(
          column(12, tags$h4("DW Winning Profiles Times - Please select your choices; press Delete to change the crew.  The Flow Rate is included after the crew names")),
          column(3, selectInput("attsplitcrew", "Choose Crew(s):", SplitName, selected = "BakerCapps229", multiple = TRUE))
        ),
        fluidRow(column(12, tags$h3("Time Plot"))),
        fluidRow(column(12, plotOutput("ProfileTime"))),
        fluidRow(column(12, tags$h4("The Fastest Time for the course was set in 2000 by Steve Baker and Duncan Capps however the race was abandoned in that year. The official DW record
                                     held by Tim Cornish and Brian Greenham since 1979 has now been taken by Keith Moule and Tom Sharpe (2023).  The chart shows the times along the course. 
                                    You may compare the three fastest times useing this system"))),
        fluidRow(column(12, tags$h3("Average Speeds"))),
        fluidRow(column(12, plotOutput("ProfileTimeSplit"))),
        fluidRow(column(12, tags$h4("The chart above shows the average speed along the course measured from Devizes"))),
        fluidRow(column(12, tags$h3("Sector Speeds Speeds"))),
        fluidRow(column(12, plotOutput("ProfileTimeSplitSpeed"))),
        fluidRow(column(12, tags$h4("The chart above shows the average speed for the last section calculated from the previous time point"))),
        fluidRow(column(12, tags$h3("Affect of Flow on River and Tideway"))),
        fluidRow(column(12, plotOutput("ProfileTimeSplitSpeedIncrease"))),
        fluidRow(column(12, tags$h4("The chart above shows the speed change between the Canal up to Newbury and the rivers from Newbury to Teddington and the then Tideway."))),
        fluidRow(column(12, tags$h3("Flow vs Speed"))),
        fluidRow(column(12, plotOutput("ProfileTimeSplitSpeedIncreaseXY"))),
        fluidRow(column(12, tags$h4("The chart above shows the speed change between the Canal the rivers from Newbury to Teddington and the Tideway depending on Flow.  It demonstrates that the speed will increase depending on flow but in high flow years the difference between the River and Tidway tends to be less.  This chart suggests the average flow on the river in high flow conditions is only about 2.5 miles/hour.  The Speed on the Tideway appears to reach about 3 mph in conditions of high flow, but typically adds an average of less than 1.5 mph")))
      ),
    #NewTP  
      tabPanel(
        "DW Winners Detailed Time Profiles",
        fluidRow(
          column(10, tags$h2("DW and Waterside Results Database")),
          column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
        ),
        fluidRow(
          column(12, tags$h4("Detailed Winning Profile Times - The table includes a selection of years from 1975 representing different flow rates.  The Times for Lawler and Brown
                             have been included as they had the fastest recorded times to Reading however the race was won Hendron and King despite being 56 mins slower at Reading. 
                             Baker and Capps are also included in the year 2000 with the fastest time for the course however this race was abanded at Old Windsor and times for that year are not recognised by DW.")),
          fluidRow(column(12, DT::dataTableOutput("WinTimPro")))
         )
            ),
      
      tabPanel(
        "Split Time Plotting",
        tabsetPanel(
          tabPanel(
            "DW Checkpoint Plot By Place",
            fluidRow(
              column(10, tags$h2("DW and Waterside Results Database - Split by Place")),
              column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
            ),
            column(3, selectInput("attsplitcrewcheck1", "Choose Crew Places:", c(unique(Places$Place)), selected = "1", multiple = TRUE)),
    #        column(3, selectInput("splityearP1", "Choose Crew Year(s): ", SplitYear, selected = "2023", multiple = TRUE)),
            column(3, selectInput("splityearP1", "Choose Crew Year(s): ", c(unique(SplitYearNew$Year)), selected = "2023", multiple = TRUE)),
            fluidRow(column(12, tags$h4("DW Checkpoint Graph plots speeds at each point. Shown By Place, The Crew names and Position are available in the Table display.  The plot to the left has checkpoint locations while the right side shows larger sections of the course. "))),
            fluidRow(column(12, plotOutput("plotCheckPoint1"))),
            fluidRow(column(12, tags$h4("The Box Plots show the range of speeds for all crews between the time points in the specified Year(s)"))),
            fluidRow(column(12, plotOutput("plotCheckBox"))),
         #   fluidRow(column(12, verbatimTextOutput("SummaryCrewSeniorComparison"))),
          ),
          tabPanel(
            "DW Checkpoint Plot By Year",
            fluidRow(
              column(10, tags$h2("DW and Waterside Results Database - Split By Year")),
              column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
            ),
            column(3, selectInput("attsplitcrewcheck2", "Choose Crew Places:", c(unique(Places$Place)), selected = "1", multiple = TRUE)),
            column(3, selectInput("splityearP2", "Choose Crew Year(s): ", c(unique(SplitYearNew$Year)), selected = "2023", multiple = TRUE)),
            fluidRow(column(12, tags$h4("DW Checkpoint Graph plots speeds at each point. Shown By Year, The Crew names and Position are available in the Table display"))),
            fluidRow(column(12, plotOutput("plotCheckPoint2"))),
            
            fluidRow(column(12, tags$h4("Time and speeds for sections between time points may be less accurate than those over the whole race.
                                              Different sections on the course have different issues which can affect boat speed."))),
            fluidRow(column(12, tags$h4("The precise location of the time point as well as the measured distance will affect the calculated speeds."))),
            fluidRow(column(12, tags$h4("Shiplake and Chertsey can be fast if portaging into the flow rather than over the lock. Boveney can be even faster over the rollers.
                                               Rollers are also used in the final section into Teddington at Sunbury and Mosley.
                                               However the steps at Marlow and the lock cut at Cookham, which is quite long and can be very dark and this may slow up crews approaching the lock at night.
                                               While the split speeds may be subject to some error they remain useful when comparing different crews over the same section. ")))
                                            

          ),
          tabPanel(
            "DW Checkpoint Plot By Paddler",
            fluidRow(
              column(10, tags$h2("DW and Waterside Results Database - Split By Paddlers")),
              column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
            ),
            column(6, textInput("paddlerSplit", "Enter Paddler - The Name is Case Sensitive", value = "")),
            fluidRow(column(12, tags$h4("DW Checkpoint Graph plots speeds for Senior Competitors at each check point. Shown By Paddler, Crew pairs may  also be entered. Crews may compare their times over the years from 2007 to current year in these graphs. The Crew names and Position are available in the Table display"))),
            fluidRow(column(12, plotOutput("plotCheckPoint3"))),
            fluidRow(column(9, tags$h5("Individual Check Points on Left")), column(3, tags$h5("Grouped Check Points on Right"))),
            fluidRow(column(12, tags$h4("The plots on the right are speeds for each sector.  Note that speeds are generally faster to Pewsey than the following sections up to Aldermaston.
                                         Reasons could be the paddlers are fresh and the absence of portages. Speeds on the Kennet River and Thames are dependent on river flow."))),
            fluidRow(column(12, tags$h3("The graph below shows progress times for the crew over the course"))),
            fluidRow(column(12, plotOutput("plotCheckPoint3Time")))
          ),
          tabPanel(
            "DW Checkpoint Progress Plot By Place",
            fluidRow(
              column(10, tags$h2("DW and Waterside Results Database - Progress")),
              column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
            ),
            column(3, selectInput("splityearP1P", "Choose Crew Year(s): ", c(unique(SplitYearNew$Year)), selected = "2023", multiple = TRUE)),
            column(3, selectInput("attsplitcrewProgress", "Highlight Crew Places:", c(unique(Places$Place)), selected = "1", multiple = TRUE)),
            fluidRow(column(12, tags$h4("The Progress Plot show the boats time at each time point.  Selecting multiple places illustrates where boats meet on the course with the faster boats catching the slower crews.
                                        All crews are shown in light blue. Selected crews are shown in red."))),
            fluidRow(column(12, plotOutput("plotProgress"))),
            fluidRow(column(12, tags$h4("The Box Plots show the range of time for all crews at time points"))),
            fluidRow(column(12, plotOutput("plotCheckBox1")))
          ),
          tabPanel(
            "DW Checkpoint Frequency By Checkpoint",
            fluidRow(
              column(10, tags$h2("DW and Waterside Results Database - Frequency by Checkpoint")),
              column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
            ),
            column(3, selectInput("splityearP1F", "Choose Year(s): ", c(unique(SplitYearNew$Year)), selected = "2023", multiple = TRUE)),
            column(3, selectInput("TimePointInput", "Choose Time Point(s): ", TimeP, selected = "A_Devizes", multiple = TRUE)),
            fluidRow(column(12, tags$h4("The Frequency Plots show the number of crews at each time point/2 minutes.  Times shown are the time of day"))),
            fluidRow(column(12, plotOutput("plotCheckBoxFHunAll"))),
            fluidRow(column(12, tags$h4("The start time for most Years is 07:00.  The departure time for crews will depend on their expected time to Teddington to catch the tide.  
                                        For this reason combining years may give a missleading picture as the tide times vary with years.  Boats on the right of the plot may represent 
                                        those aiming for the second tide but fast crews will also start latter. As from 2023 only one tide is being allowed.  
                                        Competitors arriving after the cutoff are expected to retire from the race")))
          
          ),
          tabPanel(
            "DW Race and Tideway Time Spreads",
            fluidRow(
              column(10, tags$h2("DW and Waterside Results Database - Race and Tideway Time Spreads")),
              column(2, tags$img(src = "DW Logo.jpg", height = "80px", align = "left"))
            ),
            fluidRow(column(12, tags$h4("New Cutoff Times will be imposed in 2022 (shown in brackets).  The default times entered below are set above any cutoff time.  A vertical line indicates the 2022 cutoff"))),
            fluidRow(column(2, selectInput("splityearP1FRT", "Choose Year(s): ", c(unique(SplitYearNew$Year)), selected = "2023", multiple = TRUE)),
            column(2, selectInput("NewburyCutoffInput", "Cutoff Newbury: (10)", NewburyCutoff, selected = "9.9", multiple = FALSE)),
            column(2, selectInput("ReadingCutoffInput", "Cutoff Reading: (16)", ReadingCutoff, selected = "17", multiple = FALSE)),
            column(2, selectInput("MarlowCutoffInput", "Cutoff Marlow: (20.5)", MarlowCutoff, selected = "22", multiple = FALSE)),
            column(2, selectInput("OldWindsorCutoffInput", "Cutoff Old Windsor (24): ", OldWindsorCutoff, selected = "25.5", multiple = FALSE)),
            column(2, selectInput("SheppertonCutoffInput", "Cutoff Shepperton: (27.5)", SheppertonCutoff, selected = "29", multiple = FALSE))),
      #      fluidRow(column(2, selectInput("CutYesNo","Apply Cutoff Times to Display", c("Y","N"),"Y", multiple=FALSE))),
            fluidRow(
              column(12, tags$h5("The Plots show the range of race times for crews to Checkpoints for the Senior Class. The charts are intended to demonstrate what effect a cutoff time would have on past years. 
                                 If a cutoff is applied it will affect the boat at all checkpoints.  To view which boats would have been affected you may decrease the checkpoint cutoff times.  It may help to change the Zoom level for this display"))
            ),
            fluidRow(column(12, plotOutput("plotCheckBoxFHunRT"))),
            fluidRow(column(12, plotOutput("plotCheckBoxFHunRT1"))),
            fluidRow(column(12, plotOutput("plotCheckBoxFHunRT2"))),
            fluidRow(
              column(4, tags$h5("Only years from 2007 are available for these plots"))),
        fluidRow(column(12, tags$h4("Summary Data for TimePoints - NB The data will change depending on the selected years - Cutoff times are not applied to the summary data"))),
        fluidRow(column(12, tags$h5("Cutoff times are in place for each timepoint for 2022. viz Newbury 10 hours, Reading 16 hours, Marlow 20.5 hours, Old Windsor 25 hours and Shepperton 27.5 hours"))),
        column(3, verbatimTextOutput("SummaryRace")),
        column(3, verbatimTextOutput("SummaryTideway")),
        column(3, verbatimTextOutput("SummaryPewsey")),
        column(3, verbatimTextOutput("SummaryNewbury")),
        column(3, verbatimTextOutput("SummaryReading")),
        column(3, verbatimTextOutput("SummaryMarlow")),
        column(3, verbatimTextOutput("SummaryOldWindsor")),
        column(3, verbatimTextOutput("SummaryShepperton")),
        column(3, verbatimTextOutput("SummaryTeddington")),
        
          )
          
        )
      ),
      tabPanel(
        "Split Time Tables",
        tabsetPanel(
          tabPanel(
            "DW Check Point Speed Table",
            fluidRow(column(12, tags$h4("DW Check Point Table shows speeds at each Point.   Data for 2024 has  been taken from the tracker and does not include time allowed for stoppages"))),
            column(6, selectInput("splityear", "Choose Crew Year(s): ", c(unique(SplitYearNew$Year)), selected = "2024", multiple = TRUE)),
            fluidRow(column(12, DT::dataTableOutput("tableCheckPoint")))
          ),
          tabPanel(
            "DW Check Point Time Table",
            fluidRow(column(12, tags$h4("DW Check Point Table shows speeds at each Point"))),
            column(6, selectInput("splityearTT", "Choose Crew Year(s): ", c(unique(SplitYearNew$Year)), selected = "2023", multiple = TRUE)),
            fluidRow(column(12, DT::dataTableOutput("tableCheckPointTT")))
          )
         
        )
      ),
    tabPanel("Teddington CutOff",
      tabsetPanel(
        tabPanel("DW Check Point Times with Cutoff Times Plot",
        fluidRow(column(12, plotOutput("TeddingtonCutoff"))),
        fluidRow(column(12, tags$h4("Red indicates the First High Tide with Green showing the Second High Tide.  Blue indicates the times of Boats.  Black is the Stop Time for crews prior to 2020"))),
        fluidRow(column(12, tags$h4("Boats arriving after the Morning Teddington Time Cut Off (4hrs * after High Tide) will now be required to retire.  Prior to 2022 a second tide window of 1.5 hours was also available."))),
        fluidRow(column(12, tags$h5("* Crews arriving for the first tide on the Sunday may only pass through Teddington during a period starting 30 minutes before high water and ending 3 hours and 30 minutes 
        after high water (06:00 - 10:00 BST in 2017). The tide window at Teddington for the Sunday evening tide will be 2 hours long. It will open 30 minutes before high water and close 1 hour 
        and 30 minutes after high water (18:15 - 20:15 BST in 2017).  As from 2023 only the first tide is allowed")))
        
        
         ),
        tabPanel("DW Check Point Times with Cutoff Times Table",
        fluidRow(column(12, DT::dataTableOutput("tableTeddingtonCutoff")))
                )
      ))        
  ),
    
    navbarMenu(
      HTML('<span style="color: Purple;font-weight: bold;"> History </span>'),
 #      "History",
      tabPanel("Key Dates", fluidRow(column(12, tags$iframe(style = "height:1000px; width:100%", src = "Key Dates.pdf")))),
      tabPanel("Trophy Dates", fluidRow(column(12, tags$iframe(style = "height:1000px; width:100%", src = "Trophies - Year of Introduction.pdf")))),
      tabPanel("The Early History by David Keane", fluidRow(column(12, tags$iframe(style = "height:1000px; width:100%", src = "DW David Keane.pdf")))),
      tabPanel("The Early History by A M Keane in the 1970 Program", fluidRow(column(12, tags$iframe(style = "height:1000px; width:100%", src = "History of the race 1970 Program.pdf")))),
      tabPanel("History to 2021", fluidRow(column(12, tags$iframe(style = "height:1000px; width:100%", src = "History to 2021.pdf")))),
      tabPanel("Original Portage Map", fluidRow(column(12, tags$iframe(style = "height:1000px; width:100%", src = "Original Portage Map.pdf")))),
      tabPanel("Folding Boats", fluidRow(column(12, tags$iframe(style = "height:1000px; width:100%", src = "Folding Boats.pdf")))),
      tabPanel(
        "Photos",
        fluidRow(column(4, tags$h3("Photographs of some past competitors"))),
        fluidRow(column(4, tags$h4(" "))),
        fluidRow(
          column(4, tags$img(src = "1962 Ted Tandy & Gillie Howe at Devizes.jpg", width = "100%")),
          column(4, tags$img(src = "1962 Ted Tandy & Gillie Howe.jpg", width = "100%")),
          column(4, tags$img(src = "1965 Record Cook and Stimson.jpg", width = "100%"))
        ),
        fluidRow(column(4, tags$h4("1962 Ted Tandy & Gillie Howe leaving Devizes")), column(4, tags$h4("1962 Ted Tandy & Gillie Howe")), column(4, tags$h4("1965 Cook and Stimson"))),
        fluidRow(column(12, tags$h4("Gillie (GR) Howe completed the event 10 times with an average 2nd place including a folding boat.
                                       He was the first member of the 1000 mile club.  As can be seen in these early pictures no bouyancy aids (life jackets) are being worn and in latter
                                       years many of those which were used were air bag types which were not inflated"))),
        fluidRow(column(4, tags$h3(" "))),
        fluidRow(
          column(4, tags$img(src = "1965 The first Glass Boat - Accord.jpg", width = "100%")),
          column(4, tags$img(src = "1966 approximate - From the Boat Number D N Aterton & J Babden.jpg", width = "100%")),
          column(4, tags$img(src = "1969 Paganelli and Evans leaving Devizes.jpg", width = "100%"))
        ),
        fluidRow(column(4, tags$h4("1965 The first Glass Boat - Accord")), column(4, tags$h4("1966 approximate - From the Boat Number D N Aterton & J Babden")), column(4, tags$h4("1969 Paganelli and Evans leaving Devizes"))),
        fluidRow(column(4, tags$h3(" "))),
        fluidRow(
          column(4, tags$img(src = "1970 Croften and The long Run.jpg", width = "100%")),
          column(4, tags$img(src = "1970 Pape and Serensen.jpg", width = "100%")),
          column(4, tags$img(src = "1978 Brian Greenham & Tim Cornish.jpg", width = "100%"))
        ),
        fluidRow(column(4, tags$h4("1970 Croften and The long Run")), column(4, tags$h4("1970 Pape and Serensen one of the first International Competitors")), column(4, tags$h4("1978 Brian Greenham & Tim Cornish"))),
        fluidRow(column(12, tags$h4("The Long Run is a reference to the Crofton flight of locks, which as can been seen were dry until the canal was restored.
                                        This required competitors to portage 1850 meters which the faster or fitter crews would run.  Some crews continue to run this section although it can be paddled in a similar time."))),
        fluidRow(column(4, tags$h3(" "))),
        fluidRow(
          column(4, tags$img(src = "1979 Brain Greenham and Tim Cornish Portage.jpg", width = "100%")),
          column(4, tags$img(src = "1979 Brain Greenham and Tim Cornish.jpg", width = "100%")),
          column(4, tags$img(src = "1984 Cornish & Viljoen at Westminster.jpg", width = "100%"))
        ),
        fluidRow(column(4, tags$h4("1979 Brain Greenham and Tim Cornish Portage")), column(4, tags$h4("1979 Brain Greenham and Tim Cornish")), column(4, tags$h4("1984 Cornish & Viljoen at Westminster"))),
        fluidRow(column(12, tags$h4("Brain Greenham and Tim Cornish hold the record for the DW wich was set in 1979 in a time of 15:34.
                                       This time was beaten in 2000 by Steve Baker and Duncan Capps with a time of 15:17, however as the race was abandomed in 2000 this time is not recognised by DW and is therefore unofficial"))),
        fluidRow(column(4, tags$h3(" "))),
        fluidRow(
          column(5, tags$img(src = "1984 Cornish & Viljoen.jpg", width = "100%")),
          column(4, tags$img(src = "1989 Paul & Micheal Wells.jpg", width = "100%")),
          column(3, tags$img(src = "1975 First Ladies Crew.jpg", width = "100%"))
        ),
        fluidRow(column(4, tags$h4("1984 Cornish & Viljoen")), column(4, tags$h4("1989 Paul & Micheal Wells")), column(4, tags$h4("1975 First Ladies Crew - Maurene Hassack & Jo Saxby"))),
        fluidRow(column(12, tags$h4("The ladies were not allowed to enter formally but as was the custom at that time, they were allowed the facilities of the race and given a completion certificate.
          The following year Maurene paddled with her neighbour Diana Johnson in the new Senior Ladies sub class."))),
        fluidRow(column(4, tags$h3(" "))),
        fluidRow(
          column(8, tags$img(src = "1000 Mile Club.jpg", width = "100%")),
          column(4, tags$img(src = "Eric Draper and Ernie Flood.jpg", width = "100%"))
        ),
        fluidRow(column(6, tags$h4("1000 Mile Club gathering taken at the inaugural presentation of awards")), column(6, tags$h4("1950 Eric Draper and Ernie Flood with their Cockle canoe"))),
        fluidRow(column(12, tags$h4("Ernie Flood is bending over the canoe and Eric Draper is standing behind Ernie with his arms folded. Both men were from the Coalporters ARC.
                                      The clothing worn is so different from the modern gear. Eric Draper was born in April 1929 and was aged about 21 when he took part in the first race in 1950.
                                      The rubber canoes used were ex-services as this was what was available and affordable but suffered drag and were heavy when lifting out of the water to go
                                      round the locks. Notice the bottom of the picture on the right of the boat.  This appears to show Ernie's leg indicating a rip in the Canvas.
                                      The three back rests suggest it could be used by 3 people and subsequently a three man version of the MK2 was developed into a three man 17ft 4 in version - the MK 2**.
                                      So far as is known this is the only time this type of boat has been used on DW.

                                      Designed by Fred Goatley from the Isle Of Wight, the Mark 2 was designed during a retirement period some 9 months before 'Blondie' Hasler, and his use of it for the Bordeaux raid
                                      (Operation Frankton the attack on Bordeaux in late 1942.) by the 'Cockleshell Heroes'")))
      )
    ),
    navbarMenu(
      HTML('<span style="color: Purple;font-weight: bold;"> Technical Admin </span>'),
      #  "Technical Admin",
      tabPanel("Import DW Direct to R", fluidRow(column(12, tags$iframe(style = "height:1000px; width:100%", src = "ImportDWDirecttoR.pdf")))),
      tabPanel("Import WS Direct to R", fluidRow(column(12, tags$iframe(style = "height:1000px; width:100%", src = "ImportWSDirecttoR.pdf")))),
      tabPanel("R Code Files List", fluidRow(column(12, tags$iframe(style = "height:1000px; width:100%", src = "RCode Files List.pdf")))),
      tabPanel("R Weather Data File", fluidRow(column(12, tags$iframe(style = "height:1000px; width:100%", src = "Maintenance of the ExportRWeather2019.pdf")))),
      tabPanel("R Split Time File", fluidRow(column(12, tags$iframe(style = "height:1000px; width:100%", src = "Maintenance of the Split Time Data.pdf"))))
    ),
    tabPanel(
      HTML('<span style="color: black;font-weight: bold;"> Feedback</span>'),
   #   "Feedback",
      fluidRow(column(12, tags$h4("Feedback is welcome as the best method to improve the accuracy of the results and system."))),
      
      fluidRow(column(12, tags$h4("Corrections & Comments", tags$a(href = "http://www.canoeraceresults.co.uk/extranet/devizes.nsf/4180a9361f29b17f80257a9a00509ff3?OpenForm", "Please Click here"))))
    )
  )
)



#----------------SERVER FUNCTION ---------------------


server <- function(input, output, session) {

  # DW Thousand Mile Paddlers table with reactive filtering
  output$tmctable <- DT::renderDataTable({
    DT::datatable(
      {
        data <- datapipe1 %>% filter(Total >= 8)
        data # i.e. render this dataset as a table
      },
      #  filter = list(position = 'top', clear = FALSE),
      options = list(pageLength = 125, search = list(smart = FALSE)),
    
      rownames = FALSE
    ) %>% formatStyle("Senior", target = "row", Color = styleEqual(c(8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21), c("red", "red", "red", "red", "red", "red", "red", "red", "red", "red", "red", "red", "red", "grey")))
  })
  
  output$tmctableLadies <- DT::renderDataTable({
    DT::datatable(
      {
        data <- datapipe1Ladies %>% filter(Total >= 8)
        data # i.e. render this dataset as a table
      },
      #  filter = list(position = 'top', clear = FALSE),
      options = list(pageLength = 125, search = list(smart = FALSE)),
      
      rownames = FALSE
    ) %>% formatStyle("Senior", target = "row", Color = styleEqual(c(8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21), c("red", "red", "red", "red", "red", "red", "red", "red", "red", "red", "red", "red", "red", "grey")))
  })
  
  
  output$WinTimPro <- DT::renderDataTable({
    DT::datatable(
      {
        data <- WinTime_data
        data # i.e. render this dataset as a table
      },
      #  filter = list(position = 'top', clear = FALSE),
      options = list(pageLength = 125, search = list(smart = FALSE)),
      
      rownames = FALSE
    )
    })
  
  output$tmctableSen <- DT::renderDataTable({
    DT::datatable(
      {
        data <- datapipe1Sen %>% filter(Senior >= 8)
        data # i.e. render this dataset as a table
      },
      #  filter = list(position = 'top', clear = FALSE),
      options = list(pageLength = 125, search = list(smart = FALSE)),
      
      rownames = FALSE
    ) #%>% formatStyle("Senior", target = "row", Color = styleEqual(c(1, 2, 3, 4, 5, 6, 7, 8), c("red", "red", "red", "red", "red", "red", "red", "red", "red", "red", "red", "red", "red", "grey")))
  })
  output$tmctable7 <- DT::renderDataTable({
    DT::datatable(
      {
        data <- datapipe1 %>% filter(Total >= 7 & Total <8)
        data # i.e. render this dataset as a table
      },
      #  filter = list(position = 'top', clear = FALSE),
      options = list(pageLength = 125, search = list(smart = FALSE)),
      
      rownames = FALSE
    )# %>% formatStyle("Senior", target = "row", Color = styleEqual(c(8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 1), c("red", "red", "red", "red", "red", "red", "red", "red", "red", "red", "red", "red", "red", "grey")))
  })

  #  formatStyle(
  #   'V6',
  #  backgroundColor = styleEqual(c(0, 1), c('gray', 'yellow'))


  # DW Database sidebar table with reactive filtering
  output$tableSB <- DT::renderDataTable({
    DT::datatable(
      {
        data <- table_dataCrews
        data # i.e. render this dataset as a table
      },
      options = list(pageLength = 15, search = list(smart = FALSE)),
      rownames = FALSE
    )
  })

  # datatable() (e.g., options = list(search = list(smart = FALSE)))
 # attchartdata <- main_data %>%
  #  select(Record, Class, DecimalTime) %>%
   # filter(Class %in% input$attRecordclass, Record != "", Record != "Veteran (155 years) 74 & 81")
#  ggplot(attchartdata, aes(fill = Record, x = Record, y = DecimalTime, color = Record)) +
  # DW Database table with reactive filtering
  output$tableCrewsR <- DT::renderDataTable({
    DT::datatable(
      {
        data <- table_dataRecords
#        if (input$YearCC != "All") {
 #         data <- data[data$Year %in% input$YearCC, ]
  #      }
        if (input$attRecordclassR != "All") {
          data <- data[data$Class %in% input$attRecordclassR, ]
        }
#        if (input$LadiesCC != "All") {
 #         data <- data[data$Ladies %in% input$LadiesCC, ]
  #      }
   #     if (input$divCC != "All") {
    #      data <- data[data$Military %in% input$divCC, ]
#        }
 #       if (input$PLCC != "All") {
  #        data <- data[data$Position == input$PLCC, ]
   #     }
        #
        #  if (input$BLCC != "All") {data <- data[data$Position %in% input$BLCC,]}
        #  filter(Class %in% input$atttimeclass, Position == input$Position, Year >= input$attnyear)
        data
      },
      options = list(
        search = list(regex = TRUE, caseInsensitive = FALSE, smart = FALSE),
        pageLength = 25
      ),
      rownames = FALSE
    ) %>% formatStyle("Class", target = "row", Color = styleEqual(c("Senior", "Junior", "Singles", "Vet/Junior", "Endeavour"), c("Blue", "Red", "Green", "brown", "grey")))
  })


  # DW Database table with reactive filtering
  output$tableCrews <- DT::renderDataTable({
    DT::datatable(
      {
        data <- table_dataCrews
        if (input$YearCC != "All") {
          data <- data[data$Year %in% input$YearCC, ]
        }
        if (input$ClassCC != "All") {
          data <- data[data$Class %in% input$ClassCC, ]
        }
        if (input$LadiesCC != "All") {
          data <- data[data$Ladies %in% input$LadiesCC, ]
        }
        if (input$divCC != "All") {
          data <- data[data$Military %in% input$divCC, ]
        }
        if (input$PLCC != "All") {
          data <- data[data$Position == input$PLCC, ]
        }
        if (input$BTCC != "All") {
          data <- data[data$BoatType == input$BTCC, ]
        }

        data
      },
      options = list(
        search = list(regex = TRUE, caseInsensitive = FALSE, smart = FALSE),
        pageLength = 25
      ),
      rownames = FALSE
    ) %>%
      formatRound("DecimalTime", 2) %>%
      formatStyle("Class", target = "row", Color = styleEqual(c("Senior", "Junior", "Singles", "Vet/Junior", "Endeavour"), c("Blue", "Red", "Green", "brown", "grey")))
  })


  # DW Database table with reactive filtering
  output$tableCrewstwo <- DT::renderDataTable({
    DT::datatable(
      {
        data <- table_dataCrewstwo
        data
      },
      extensions = "RowGroup",
      options = list(rowGroup = list(dataSrc = 0), search = list(regex = TRUE, caseInsensitive = FALSE, smart = FALSE), pageLength = 20),
      filter = list(position = "top", clear = FALSE),
      rownames = FALSE
    ) %>%
      formatRound("DecimalTime", 2) %>%
      formatStyle("Class", target = "row", Color = styleEqual(c(
        "Senior", "Junior", "Singles", "Vet/Junior",
        "Endeavour"
      ), c("Blue", "Red", "Green", "brown", "grey"))) %>%
      formatStyle("Name1", Color = "white")
  })
 

  datapipe <- table_dataCrewstwo %>% 
    mutate(SeniorC = case_when(Class == "Senior" ~ 1)) %>%
    mutate(JuniorC = case_when(Class == "Junior" ~ 1)) %>%
    mutate(SinglesC = case_when(Class == "Singles" ~ 1)) %>%
    mutate(VetJuniorC = case_when(Class == "Vet/Junior" ~ 1)) %>%
    mutate(EndeavourC = case_when(Class == "Endeavour" ~ 1)) %>%
    mutate(KayakC = case_when(BoatType == "Kayak" ~ 1)) %>%
    mutate(CanadianC = case_when(BoatType == "Canadian" ~ 1)) %>%
    mutate(Folding.BoatC = case_when(BoatType == "Folding Boat" ~ 1))
  data


  datapipe1 <- datapipe %>% 
#  datapipe1 <- reactive({datapipe %>% filter(Class %in% input$SelectClass)}) %>%
    group_by(Name1) %>%
    summarise(
      Classes = length(unique(Class)), From = min(Year), To = max(Year), Years = max(Year) - min(Year), 
      
      Senior = sum(SeniorC, na.rm = TRUE), Junior = sum(JuniorC, na.rm = TRUE), Singles = sum(SinglesC, na.rm = TRUE), VetJunior = sum(VetJuniorC, na.rm = TRUE), Endeavour = sum(EndeavourC, na.rm = TRUE),
#      Kayak = sum(KayakC, na.rm = TRUE), Canadian = sum(CanadianC, na.rm = TRUE), Folding = sum(Folding.BoatC, na.rm = TRUE), Total = sum(Senior, Junior, Singles, VetJunior, Endeavour),
      Kayak = sum(KayakC, na.rm = TRUE), Canadian = sum(CanadianC, na.rm = TRUE), Folding = sum(Folding.BoatC, na.rm = TRUE), Total = sum(Senior, Junior, Singles, VetJunior, Endeavour)
    ) %>%
    rename(Crew = Name1)
  
  datapipeLadies <- table_dataCrewstwoLadies %>% 
    mutate(LadiesC = case_when(Ladies == "Ladies" ~ 1)) %>%
    mutate(MixedC = case_when(Ladies == "Mixed" ~ 1)) %>%
    mutate(SeniorC = case_when(Class == "Senior" ~ 1)) %>%
    mutate(JuniorC = case_when(Class == "Junior" ~ 1)) %>%
    mutate(SinglesC = case_when(Class == "Singles" ~ 1)) %>%
    mutate(VetJuniorC = case_when(Class == "Vet/Junior" ~ 1)) %>%
    mutate(EndeavourC = case_when(Class == "Endeavour" ~ 1)) %>%
    mutate(KayakC = case_when(BoatType == "Kayak" ~ 1)) %>%
    mutate(CanadianC = case_when(BoatType == "Canadian" ~ 1)) %>%
    mutate(Folding.BoatC = case_when(BoatType == "Folding Boat" ~ 1))
  data
  
  
  datapipe1Ladies <- datapipeLadies %>% 
    #  datapipe1 <- reactive({datapipe %>% filter(Class %in% input$SelectClass)}) %>%
    group_by(Name1) %>%
    summarise(
      Classes = length(unique(Class)), From = min(Year), To = max(Year), Years = max(Year) - min(Year), 
      
      Ladies = sum(LadiesC, na.rm = TRUE), Mixed = sum(MixedC, na.rm = TRUE),Senior = sum(SeniorC, na.rm = TRUE), Junior = sum(JuniorC, na.rm = TRUE), Singles = sum(SinglesC, na.rm = TRUE), VetJunior = sum(VetJuniorC, na.rm = TRUE), Endeavour = sum(EndeavourC, na.rm = TRUE),
      #      Kayak = sum(KayakC, na.rm = TRUE), Canadian = sum(CanadianC, na.rm = TRUE), Folding = sum(Folding.BoatC, na.rm = TRUE), Total = sum(Senior, Junior, Singles, VetJunior, Endeavour),
      Kayak = sum(KayakC, na.rm = TRUE), Canadian = sum(CanadianC, na.rm = TRUE), Folding = sum(Folding.BoatC, na.rm = TRUE), Total = sum(Senior, Junior, Singles, VetJunior, Endeavour)
    ) %>%
    rename(Crew = Name1)
  
  datapipe1Sen <- datapipe %>% filter(Class == "Senior")%>%
    #  datapipe1 <- reactive({datapipe %>% filter(Class %in% input$SelectClass)}) %>%
    group_by(Name1) %>%
    summarise(
      From = min(Year), To = max(Year), Years = max(Year) - min(Year), Average_Time = round(mean(DecimalTime), 1), 
      Average_Place = round(mean(Position), 1),
      Senior = sum(SeniorC, na.rm = TRUE),
      Kayak = sum(KayakC, na.rm = TRUE), Canadian = sum(CanadianC, na.rm = TRUE), Folding = sum(Folding.BoatC, na.rm = TRUE), Total = sum(Senior)
    ) %>%
    rename(Crew = Name1)
  
  

  datapipeclub <- table_dataCrews %>%
    mutate(SeniorCl = case_when(Class == "Senior" ~ 1)) %>%
    mutate(JuniorCl = case_when(Class == "Junior" ~ 1)) %>%
    mutate(SinglesCl = case_when(Class == "Singles" ~ 1)) %>%
    mutate(VetJuniorCl = case_when(Class == "Vet/Junior" ~ 1)) %>%
    mutate(EndeavourCl = case_when(Class == "Endeavour" ~ 1)) %>%
    mutate(KayakCl = case_when(BoatType == "Kayak" ~ 1)) %>%
    mutate(CanadianCl = case_when(BoatType == "Canadian" ~ 1)) %>%
    mutate(Folding.BoatCl = case_when(BoatType == "Folding Boat" ~ 1))
  data
  
  datapipeclub1 <- datapipeclub %>%
    group_by(Club) %>%
    summarise(
      Classes = length(unique(Class)), From = min(Year), To = max(Year), Years = max(Year) - min(Year),
      Senior = sum(SeniorCl, na.rm = TRUE), Junior = sum(JuniorCl, na.rm = TRUE), Singles = sum(SinglesCl, na.rm = TRUE), VetJunior = sum(VetJuniorCl, na.rm = TRUE), Endeavour = sum(EndeavourCl, na.rm = TRUE),
      Kayak = sum(KayakCl, na.rm = TRUE), Canadian = sum(CanadianCl, na.rm = TRUE), Folding = sum(Folding.BoatCl, na.rm = TRUE), Total = sum(Senior, Junior, Singles, VetJunior, Endeavour)
    )# %>% filter(Total >25) 
  
  

  output$tableCrewstwoDev1 <- DT::renderDataTable({
    DT::datatable(
      {
        datapipe1
      },
      options = list(search = list(regex = TRUE, caseInsensitive = FALSE, smart = FALSE), pageLength = 10),
   #   filter = list(position = "top", clear = FALSE),
      rownames = FALSE
    )
  })












  output$tableCrewstwoExperimental <- DT::renderDataTable({
    DT::datatable(
      {
        data <- table_dataCrewstwo %>% mutate(Total = case_when(Name1 != "Senior" | Name1 != "Singles" | Name1 != "Junior" | Name1 != "Vet/Junior" | Name1 != "Endeavour" ~ 1))
        data %>%
          mutate(Senior = case_when(Class == "Senior" ~ 1)) %>%
          mutate(Junior = case_when(Class == "Junior" ~ 1)) %>%
          mutate(Singles = case_when(Class == "Singles" ~ 1)) %>%
          mutate(VetJunior = case_when(Class == "Vet/Junior" ~ 1)) %>%
          mutate(Endeavour = case_when(Class == "Endeavour" ~ 1)) %>%
          mutate(Total = case_when(Name1 != "Senior" | Name1 != "Singles" | Name1 != "Junior" | Name1 != "Vet/Junior" | Name1 != "Endeavour" ~ 1)) %>%
          mutate(Kayak = case_when(BoatType == "Kayak" ~ 1)) %>%
          mutate(Canadian = case_when(BoatType == "Canadian" ~ 1)) %>%
          mutate(Folding.Boat = case_when(BoatType == "Folding Boat" ~ 1))
        data %>%
          group_by(Name1) %>%
          summarise(Total = n(), First_Raced = min(Year), Last_Raced = max(Year), Number_of_years = max(Year) - min(Year)) %>%
          arrange(desc(Total))
      },
      extensions = "RowGroup",
      options = list(search = list(regex = TRUE, caseInsensitive = FALSE, smart = FALSE), pageLength = 10),
      filter = list(position = "top", clear = FALSE),
      rownames = FALSE
    )
  })

  #  output$tableCrewstwoSeniorB <- DT::renderDataTable({

  # DT::datatable({data <- table_dataCrewstwo
  # if (input$Namesum != "All") {data <- data[data$Name1 == input$Namesum,]}


  output$tableCrewstwoExperimental2 <- DT::renderDataTable({
    DT::datatable(
      {
        data <- table_dataCrewstwo %>%
          mutate(Total = case_when(Name1 != "Senior" | Name1 != "Singles" | Name1 != "Junior" | Name1 != "Vet/Junior" | Name1 != "Endeavour" ~ 1)) %>%
          mutate(Senior = case_when(Class == "Senior" ~ 1)) %>%
          mutate(Junior = case_when(Class == "Junior" ~ 1)) %>%
          mutate(Singles = case_when(Class == "Singles" ~ 1)) %>%
          mutate(VetJunior = case_when(Class == "Vet/Junior" ~ 1)) %>%
          mutate(Endeavour = case_when(Class == "Endeavour" ~ 1)) %>%
          mutate(Total = case_when(Name1 != "Senior" | Name1 != "Singles" | Name1 != "Junior" | Name1 != "Vet/Junior" | Name1 != "Endeavour" ~ 1)) %>%
          mutate(Kayak = case_when(BoatType == "Kayak" ~ 1)) %>%
          mutate(Canadian = case_when(BoatType == "Canadian" ~ 1)) %>%
          mutate(Folding.Boat = case_when(BoatType == "Folding Boat" ~ 1))
        data %>%
          group_by(Name1, Class, BoatType) %>%
          summarise(First_Raced = min(Year), Last_Raced = max(Year), Total = sum(Total), Senior = sum(Senior), Junior = sum(Junior), Singles = sum(Singles), VetJunior = sum(VetJunior), Endeavour = sum(Endeavour), Kayak = sum(Kayak), Canadian = sum(Canadian), Folding.Boat = sum(Folding.Boat))
      },
      options = list(search = list(regex = TRUE, caseInsensitive = FALSE, smart = FALSE), pageLength = 10),
      filter = list(position = "top", clear = FALSE),
      rownames = FALSE
    )
  })


  a <- DT::datatable(
    {
      data <- table_dataCrewstwo
      data %>%
        filter(Name1 != "") %>%
        group_by(Name1, Class) %>%
        summarise(Mean_Position = mean(Position), Best_Position = min(Position), Mean_DecimalTime = mean(DecimalTime), Best_DecimalTime = min(DecimalTime), Completions = n())
    },

    #    options = list(search = list(regex = TRUE, caseInsensitive = FALSE, smart = FALSE),pageLength = 10),
    options = list(dom = "t", pageLength = 10),
    filter = list(position = "top", clear = FALSE),
    rownames = FALSE
  ) %>%
    formatRound("Best_DecimalTime", 2) %>%
    formatRound("Mean_DecimalTime", 2) %>%
    formatRound("Mean_Position", 2) # %>%formatStyle("Class", target = "row", Color = styleEqual(c("Senior", "Junior","Singles","Vet/Junior","Endeavour"),                        #                                                                                      c("Blue","Red","Green",'brown', 'grey')))


  # output$tableCrewstwoGroup


  b <- DT::datatable(
    {
      data <- table_dataCrewstwo
      data %>%
        filter(Name1 != "") %>%
        group_by(Name1) %>%
        summarise(Mean_Position = mean(Position), Best_Position = min(Position), Mean_DecimalTime = mean(DecimalTime), Best_DecimalTime = min(DecimalTime), Completions = n())
    },

    #   options = list(search = list(regex = TRUE, caseInsensitive = FALSE, smart = FALSE),pageLength = 10),
    options = list(dom = "t", pageLength = 10),
    filter = list(position = "top", clear = FALSE),
    rownames = FALSE
  ) %>%
    formatRound("Best_DecimalTime", 2) %>%
    formatRound("Mean_DecimalTime", 2) %>%
    formatRound("Mean_Position", 2) # %>%formatStyle("Class", target = "row", Color = styleEqual(c("Senior", "Junior","Singles","Vet/Junior","Endeavour"),                        #                                                                                      c("Blue","Red","Green",'brown', 'grey')))


  # output$tableCrewstwoGroup <- DT::renderDataTable ({if(input$SelectG == "No"){(b)}else if(input$SelectG == "Yes"){(a)}})
  output$tableCrewstwoGroup <- DT::renderDataTable(a)
  output$tableCrewstwoGroupx <- DT::renderDataTable(b)




  #  output$tableCrewstwoGroup <- DT::renderDataTable(a)

  # DW Summary Database table with reactive filtering  Both A and B combine crew 1 and 2 into Name1
  output$tableCrewstwoSeniorB <- DT::renderDataTable({
    DT::datatable(
      {
        data <- table_dataCrewstwo#%>%
          #select(Name1, Year, Class, Name, Time, Club, Position, Trophies, BoatType, DecimalTime, Ladies, Military)
        if (input$Namesum != "All") {
          data <- data[data$Name1 == input$Namesum, ]
        }
        if (input$Clubsum != "All") {
          data <- data[data$Club == input$Clubsum, ]
        }
        {
          data <- data[data$Year >= input$YearCCtwosum, ]
        }
        {
          data <- data[data$Year <= input$YearCCtwosumL, ]
        }
        if (input$ClassCCtwosum != "All") {
          data <- data[data$Class %in% input$ClassCCtwosum, ]
        }
        if (input$LadiesCCtwosum != "All") {
          data <- data[data$Ladies %in% input$LadiesCCtwosum, ]
        }
        if (input$divCCtwosum != "All") {
          data <- data[data$Military %in% input$divCCtwosum, ]
        }
        if (input$att2boatsum != "All") {
          data <- data[data$BoatType %in% input$att2boatsum, ]
        }
        {
          data <- data[between(data$Position, as.numeric(input$PLCCtwosum), as.numeric(input$PLCCtwosumL)), ]
        }
        data %>% summarise(
          Min_Position = min(Position), Max_Position = max(Position), Average_Position = mean(Position),
          Min_Time = min(DecimalTime), Max_Time = max(DecimalTime), Average_Time = mean(DecimalTime),
          Median_Time = median(DecimalTime), if (input$Namesum != "All") {
            Completions <- n()
          } else {
            Completions <- n() / 2
          }
        )
      },
      #    filter = list(position = "top", clear = FALSE),
      options = list(dom = "t", pageLength = 10),
      colnames = c("Min_Position", "Max_Position", "Average_Position", "Min_Time", "Max_Time", "Average_Time", "Median_Time", "Completions"),
      rownames = FALSE
    ) %>%
      formatRound("Min_Position", 2) %>%
      formatRound("Max_Position", 2) %>%
      formatRound("Average_Position", 2) %>%
      formatRound("Min_Time", 2) %>%
      formatRound("Max_Time", 2) %>%
      formatRound("Average_Time", 2) %>%
      formatRound("Median_Time", 2)
  })




  # DW Database table with reactive filtering (Working)Both A and B combine crew 1 and 2 into Name1.--- This seems correct
  output$tableCrewstwoSeniorA <- DT::renderDataTable({
    DT::datatable(
      {
        data <- table_dataCrewstwo
        if (input$Namesum != "All") {
          data <- data[data$Name1 == input$Namesum, ]
        }
        if (input$Clubsum != "All") {
          data <- data[data$Club == input$Clubsum, ]
        }
        {
          data <- data[data$Year >= input$YearCCtwosum, ]
        }
        {
          data <- data[data$Year <= input$YearCCtwosumL, ]
        }
        if (input$ClassCCtwosum != "All") {
          data <- data[data$Class %in% input$ClassCCtwosum, ]
        }
        if (input$LadiesCCtwosum != "All") {
          data <- data[data$Ladies %in% input$LadiesCCtwosum, ]
        }
        if (input$divCCtwosum != "All") {
          data <- data[data$Military %in% input$divCCtwosum, ]
        }
        if (input$att2boatsum != "All") {
          data <- data[data$BoatType %in% input$att2boatsum, ]
        }
        {
          data <- data[between(data$Position, as.numeric(input$PLCCtwosum), as.numeric(input$PLCCtwosumL)), ]
        }
        data
      },

      #   filter = list(position = "top", clear = FALSE),
      #    options = list(dom = 't',pageLength = 10),
      options = list(searching = FALSE, search = list(regex = TRUE, caseInsensitive = FALSE, smart = FALSE), pageLength = 10),
      rownames = FALSE
    ) %>%
      formatRound("DecimalTime", 2) %>%
      formatStyle("Class", target = "row", Color = styleEqual(c("Senior", "Junior", "Singles", "Vet/Junior", "Endeavour"), c("Blue", "Red", "Green", "brown", "grey")))
  })
  # Buttons
  # extensions = 'Buttons',options = list(dom = 'Bfrtip',
  #                                      buttons =
  #                                       list('copy', 'print', list(
  #                                        extend = 'collection',
  #                                       buttons = c('csv', 'excel', 'pdf'),
  #                                      text = 'Download'
  #                                   )),searching = FALSE,search = list(regex = TRUE, caseInsensitive = FALSE, smart = FALSE),pageLength = 10),







  output$tableCrewsCrews <- DT::renderDataTable({
    DT::datatable(
      {
        data <- table_dataCrewsCrews
        #  if (input$YearCCNC != "All") {data <- data[data$Year %in% input$YearCCNC,]}
        if (input$ClassCCNC != "All") {
          data <- data[data$Class %in% input$ClassCCNC, ]
        }
        #    if (input$LadiesCC != "All") {data <- data[data$Ladies %in% input$LadiesCC,]}
        #   if (input$divCC != "All") {data <- data[data$Military %in% input$divCC,]}
        #  if (input$PLCC != "All") {data <- data[data$Position == input$PLCC,]}
        # if (input$BTCC != "All") {data <- data[data$BoatType == input$BTCC,]}

        data
      },
      options = list(
        search = list(regex = TRUE, caseInsensitive = FALSE, smart = FALSE),
        pageLength = 25
      ),
      rownames = FALSE
    ) %>% formatStyle("Class", target = "row", Color = styleEqual(c("Senior", "Junior", "Singles", "Vet/Junior", "Endeavour"), c("Blue", "Red", "Green", "brown", "grey")))
  })



  # DW Club Database table with reactive filtering
  output$ClubNumbertable <- DT::renderDataTable({
    DT::datatable(
      {
        data <- datapipeclub1
        data # i.e. render this dataset as a table
      },
      #  filter = list(position = 'top', clear = FALSE),
      options = list(pageLength = 25, search = list(smart = FALSE)),
      rownames = FALSE
    ) 
  })
  
  
  
  
  
  
  
  
  
  
  output$tableClub <- DT::renderDataTable({
    DT::datatable(
      {
        data <- table_dataCrewsClub
        if (input$YearCCNCL != "All") {
          data <- data[data$Year %in% input$YearCCNCL, ]
        }
        if (input$ClassCCNCL != "All") {
          data <- data[data$Class %in% input$ClassCCNCL, ]
        }
       
        data
      },
      extensions = "RowGroup",
      options = list(
        rowGroup = list(dataSrc = 0),
        search = list(regex = TRUE, caseInsensitive = FALSE, smart = FALSE),
        pageLength = 25
      ),
      rownames = FALSE
    ) %>%
      formatStyle("Class", target = "row", Color = styleEqual(c("Senior", "Junior", "Singles", "Vet/Junior", "Endeavour"), c("Blue", "Red", "Green", "brown", "grey"))) %>%
      formatStyle("Club", Color = "white")
  })







  # DW Place Database table with reactive filtering
  output$tablePlace <- DT::renderDataTable({
    DT::datatable(
      {
        data <- table_dataCrewsPlace
        if (input$YearCCNP != "All") {
          data <- data[data$Year %in% input$YearCCNP, ]
        }
        if (input$ClassCCNP != "All") {
          data <- data[data$Class %in% input$ClassCCNP, ]
        }
        #    if (input$LadiesCC != "All") {data <- data[data$Ladies %in% input$LadiesCC,]}
        #   if (input$divCC != "All") {data <- data[data$Military %in% input$divCC,]}
        if (input$PLCCNP != "All") {
          data <- data[data$Position == input$PLCCNP, ]
        }
        # if (input$BTCC != "All") {data <- data[data$BoatType == input$BTCC,]}

        data
      },
      options = list(search = list(regex = TRUE, caseInsensitive = FALSE, smart = FALSE), pageLength = 25),
      rownames = FALSE
    ) %>% formatStyle("Class", target = "row", Color = styleEqual(c("Senior", "Junior", "Singles", "Vet/Junior", "Endeavour"), c("Blue", "Red", "Green", "brown", "grey")))
  })



  # DW Time Database table with reactive filtering
  output$tableTime <- DT::renderDataTable({
    DT::datatable(
      {
        data <- table_dataCrewsTime
        if (input$ClassCCNT != "All") {
          data <- data[data$Class %in% input$ClassCCNT, ]
        }
        data
      },
      options = list(
        search = list(regex = TRUE, caseInsensitive = FALSE, smart = FALSE),
        pageLength = 25
      ),
      rownames = FALSE
    ) %>% formatStyle("Class", target = "row", Color = styleEqual(c("Senior", "Junior", "Singles", "Vet/Junior", "Endeavour"), c("Blue", "Red", "Green", "brown", "grey")))
  })




  # DW YearDatabase table with reactive filtering
  output$tableYear <- DT::renderDataTable({
    DT::datatable(
      {
        data <- table_dataCrewsYear
        if (input$YearCCNY != "All") {
          data <- data[data$Year %in% input$YearCCNY, ]
        }
        if (input$ClassCCNY != "All") {
          data <- data[data$Class %in% input$ClassCCNY, ]
        }
        #    if (input$LadiesCC != "All") {data <- data[data$Ladies %in% input$LadiesCC,]}
        #   if (input$divCC != "All") {data <- data[data$Military %in% input$divCC,]}
        #  if (input$PLCC != "All") {data <- data[data$Position == input$PLCC,]}
        # if (input$BTCC != "All") {data <- data[data$BoatType == input$BTCC,]}
        data
      },
      extensions = "RowGroup",
      options = list(
        rowGroup = list(dataSrc = 0),
        search = list(regex = TRUE, caseInsensitive = FALSE, smart = FALSE),
        pageLength = 25
      ),
      rownames = FALSE
    ) %>%
      formatStyle("Class", target = "row", Color = styleEqual(c("Senior", "Junior", "Singles", "Vet/Junior", "Endeavour"), c("Blue", "Red", "Green", "brown", "grey"))) %>%
      formatStyle("Year", Color = "white")
  })


  # DW YearDatabase table with reactive filtering
  output$tableCanadians <- DT::renderDataTable({
    DT::datatable(
      {
        data <- table_dataCrewsCanadians
        if (input$YearCCNCA != "All") {
          data <- data[data$Year %in% input$YearCCNCA, ]
        }
        if (input$ClassCCNCA != "All") {
          data <- data[data$Class %in% input$ClassCCNCA, ]
        }
        #    if (input$LadiesCC != "All") {data <- data[data$Ladies %in% input$LadiesCC,]}
        #   if (input$divCC != "All") {data <- data[data$Military %in% input$divCC,]}
        #  if (input$PLCC != "All") {data <- data[data$Position == input$PLCC,]}
        # if (input$BTCC != "All") {data <- data[data$BoatType == input$BTCC,]}

        data
      },
      extensions = "RowGroup",
      options = list(
        rowGroup = list(dataSrc = 0),
        search = list(regex = TRUE, caseInsensitive = FALSE, smart = FALSE),
        pageLength = 25
      ),
      rownames = FALSE
    ) %>%
      formatStyle("Class", target = "row", Color = styleEqual(c("Senior", "Junior", "Singles", "Vet/Junior", "Endeavour"), c("Blue", "Red", "Green", "brown", "grey"))) %>%
      formatStyle("Year", Color = "white")
  })

  # DW YearDatabase table with reactive filtering
  output$tableFoldingBoat <- DT::renderDataTable({
    DT::datatable(
      {
        data <- table_dataCrewsFoldingBoat
        if (input$YearCCNFB != "All") {
          data <- data[data$Year %in% input$YearCCNFB, ]
        }
        if (input$ClassCCNFB != "All") {
          data <- data[data$Class %in% input$ClassCCNFB, ]
        }
        #    if (input$LadiesCC != "All") {data <- data[data$Ladies %in% input$LadiesCC,]}
        #   if (input$divCC != "All") {data <- data[data$Military %in% input$divCC,]}
        #  if (input$PLCC != "All") {data <- data[data$Position == input$PLCC,]}
        # if (input$BTCC != "All") {data <- data[data$BoatType == input$BTCC,]}

        data
      },
      extensions = "RowGroup",
      options = list(
        rowGroup = list(dataSrc = 0),
        search = list(regex = TRUE, caseInsensitive = FALSE, smart = FALSE),
        pageLength = 25
      ),
      rownames = FALSE
    ) %>%
      formatStyle("Class", target = "row", Color = styleEqual(c("Senior", "Junior", "Singles", "Vet/Junior", "Endeavour"), c("Blue", "Red", "Green", "brown", "grey"))) %>%
      formatStyle("Year", Color = "white")
  })

  # DW YearDatabase table with reactive filtering
  output$tableLadies <- DT::renderDataTable({
    DT::datatable(
      {
        data <- table_dataCrewsLadies
        if (input$YearCCNL != "All") {
          data <- data[data$Year %in% input$YearCCNL, ]
        }
        if (input$ClassCCNL != "All") {
          data <- data[data$Class %in% input$ClassCCNL, ]
        }
        #    if (input$LadiesCC != "All") {data <- data[data$Ladies %in% input$LadiesCC,]}
        #   if (input$divCC != "All") {data <- data[data$Military %in% input$divCC,]}
        #  if (input$PLCC != "All") {data <- data[data$Position == input$PLCC,]}
        # if (input$BTCC != "All") {data <- data[data$BoatType == input$BTCC,]}

        data
      },
      extensions = "RowGroup",
      options = list(
        rowGroup = list(dataSrc = 0),
        search = list(regex = TRUE, caseInsensitive = FALSE, smart = FALSE),
        pageLength = 25
      ),
      rownames = FALSE
    ) %>%
      formatStyle("Class", target = "row", Color = styleEqual(c("Senior", "Junior", "Singles", "Vet/Junior", "Endeavour"), c("Blue", "Red", "Green", "brown", "grey"))) %>%
      formatStyle("Year", Color = "white")
  })





  # DW Database table with reactive filtering
  output$table <- DT::renderDataTable({
    DT::datatable(
      {
        data <- table_data
        if (input$Year != "All") {
          data <- data[data$Year %in% input$Year, ]
        }
        if (input$Class != "All") {
          data <- data[data$Class %in% input$Class, ]
        }
        if (input$Ladies != "All") {
          data <- data[data$Ladies %in% input$Ladies, ]
        }
        if (input$div != "All") {
          data <- data[data$Military %in% input$div, ]
        }
        if (input$BT != "All") {
          data <- data[data$BoatType %in% input$BT, ]
        }

        data # i.e. render this dataset as a table
      },
      # filter = list(position = 'top', clear = FALSE),
      options = list(
        search = list(regex = TRUE, caseInsensitive = FALSE),
        pageLength = 25
      ),
      rownames = FALSE
    )
  })

  data # i.e. render this dataset as a table


  # DW Database table with reactive filtering
  output$tableExceptions <- DT::renderDataTable({
    DT::datatable(
      {
        data <- table_dataExceptions
        data # i.e. render this dataset as a table
        # unite(table_dataPosition,"SubClasses", c("Veteran","Military","Ladies","SubClass"), sep = ", ")
      },
      options = list(
        search = list(regex = TRUE, caseInsensitive = FALSE),
        pageLength = 75
      ),
      rownames = FALSE
    )
  })
  # renderDataTable(..., options = list(search = list(caseInsensitve = FALSE, regex = TRUE)))

  # DW Database table with reactive filtering
  
  output$tableABCD <- DT::renderDataTable({
    DT::datatable(
      
      {
#        data <- datapipeABCD
        data <- datapipeABCD
        data # i.e. render this dataset as a table
  #       unite(table_dataPosition,"SubClasses", c("Veteran","Military","Ladies","SubClass"), sep = ", ")
      },
      options = list(
        search = list(regex = TRUE, caseInsensitive = FALSE),
        pageLength = 200
      ),
      rownames = FALSE
    )# %>% formatStyle("Class", target = "row", Color = styleEqual(c("Senior", "Junior", "Singles", "Vet/Junior", "Endeavour"), c("Blue", "Red", "Green", "brown", "grey")))
  })
 
  #New list correct 
  output$tableABCDWS <- DT::renderDataTable({
    DT::datatable(
      {
        data <- datapipeABCDcom
        data 
      },
      options = list(
        search = list(regex = TRUE, caseInsensitive = FALSE),
        pageLength = 200
      ),
      rownames = FALSE
    ) %>%
       formatRound(columns = c(4:12), digits = 2)%>%formatRound(columns = c(3), digits = 0)
  })
  
  
 # datatable(
  #  mtcars2, colnames = c('model' = 1),
   # filter = list(position = 'top', clear = FALSE),
    #options = list(
     # search = list(regex = TRUE, caseInsensitive = FALSE, search = 'M[ae]'),
      #pageLength = 5
  #  )
  

  
  
  
  
  
  output$tableVisitors <- DT::renderDataTable({
    DT::datatable(
      {
        data <- table_dataVisitors
        data # i.e. render this dataset as a table
        # unite(table_dataPosition,"SubClasses", c("Veteran","Military","Ladies","SubClass"), sep = ", ")
      },
      options = list(
        search = list(regex = TRUE, caseInsensitive = FALSE),
        pageLength = 200
      ),
      rownames = FALSE
    ) %>% formatStyle("Class", target = "row", Color = styleEqual(c("Senior", "Junior", "Singles", "Vet/Junior", "Endeavour"), c("Blue", "Red", "Green", "brown", "grey")))
  })
  # renderDataTable(..., options = list(search = list(caseInsensitve = FALSE, regex = TRUE)))


  # DW Database Time Point Test
  output$tableCheckPoint <- DT::renderDataTable({
    DT::datatable(
      {
        data <- DW2017ST
        if (input$splityear != "All") {
          data <- data[data$Year %in% input$splityear, ]
        }
        data
      },
      options = list(
        search = list(regex = TRUE, caseInsensitive = FALSE),
        pageLength = 200
      ),
      rownames = FALSE
    )
  })

  # Original Data
  output$tableCheckPointTT <- DT::renderDataTable({
    DT::datatable(
      {
        data <- DW2017STT
        if (input$splityearTT != "All") {
          data <- data[data$Year %in% input$splityearTT, ]
        }
        data
      },
      options = list(
        search = list(regex = TRUE, caseInsensitive = FALSE),
        pageLength = 200
      ),
      rownames = FALSE
    )
  })

  # Original Data Test
  #  output$tableCheckPointTD <- DT::renderDataTable({
  #   DT::datatable({data <- DW2017SN
  #  if (input$splityearTD != "All") {data <- data[data$Year %in% input$splityearTD,]}
  # data},
  # options = list(
  # search = list(regex = TRUE, caseInsensitive = FALSE),
  # pageLength = 200
  #    ),
  #   rownames = FALSE)})



  # WS Database table with reactive filtering

  output$WStable <- DT::renderDataTable({
    DT::datatable(
      {
        data <- wstable_data
        if (input$WSYear != "All") {
          data <- data[data$Year %in% input$WSYear, ]
        }
        if (input$WSCCRace != "All") {
          data <- data[data$WSRace %in% input$WSCCRace, ]
        }
        if (input$WScl != "All") {
          data <- data[data$WSClass %in% input$WScl, ]
        }
        if (input$WPLCC != "All") {
          data <- data[data$WSPosition == input$WPLCC, ]
        }
        data # i.e. render this dataset as a table
      },
      #  filter = list(position = 'top', clear = FALSE),
      options = list(
        search = list(regex = TRUE, caseInsensitive = FALSE),
        pageLength = 50
      ),
      rownames = FALSE
    ) %>%
      formatRound(columns = "WSDecimalTime", digits = 2) %>%
      formatStyle("WSClass", target = "row", Color = styleEqual
      (
        c("Canadian Doubles", "Canadian Singles", "Junior/Veteran", "K1 Junior", "K1 Ladies", "K1 Senior", "K1 Veteran", "K2 Junior", "K2 Junior Ladies", "K2 Ladies", "K2 Mixed", "K2 Senior", "K2 Veteran"),
        c("Green", "Grey", "Red", "Blue", "Blue", "Blue", "Blue", "Red", "Red", "Red", "Red", "Red", "Red")
      ))
  })

  output$WStableCrews <- DT::renderDataTable({
    DT::datatable(
      {
        data <- wstable_dataCrewsCrew
        # if (input$WSYearNC != "All") {data <- data[data$Year %in% input$WSYearNC,]}
        if (input$WSCCRaceNC != "All") {
          data <- data[data$WSRace %in% input$WSCCRaceNC, ]
        }
        if (input$WSclNC != "All") {
          data <- data[data$WSClass %in% input$WSclNC, ]
        }
        #   if (input$WPLCC != "All") {data <- data[data$WSPosition == input$WPLCC,]}
        data # i.e. render this dataset as a table
      },
      #  filter = list(position = 'top', clear = FALSE),
      options = list(
        search = list(regex = TRUE, caseInsensitive = FALSE),
        pageLength = 50
      ),
      rownames = FALSE
    ) %>% formatStyle("WSClass", target = "row", Color = styleEqual
    (
      c("Canadian Doubles", "Canadian Singles", "Junior/Veteran", "K1 Junior", "K1 Ladies", "K1 Senior", "K1 Veteran", "K2 Junior", "K2 Junior Ladies", "K2 Ladies", "K2 Mixed", "K2 Senior", "K2 Veteran"),
      c("Green", "Grey", "Red", "Blue", "Blue", "Blue", "Blue", "Red", "Red", "Red", "Red", "Red", "Red")
    ))
  })

  output$WStableClubs <- DT::renderDataTable({
    DT::datatable(
      {
        data <- wstable_dataCrewsClub
        if (input$WSYearNCl != "All") {
          data <- data[data$Year %in% input$WSYearNCl, ]
        }
        if (input$WSCCRaceNCl != "All") {
          data <- data[data$WSRace %in% input$WSCCRaceNCl, ]
        }
        if (input$WSclNCl != "All") {
          data <- data[data$WSClass %in% input$WSclNCl, ]
        }
        #  if (input$WPLCCNCl != "All") {data <- data[data$WSPosition == input$WPLCCNCl,]}
        data # i.e. render this dataset as a table
      },
      #  filter = list(position = 'top', clear = FALSE),
      options = list(
        search = list(regex = TRUE, caseInsensitive = FALSE),
        pageLength = 50
      ),
      rownames = FALSE
    ) %>% formatStyle("WSClass", target = "row", Color = styleEqual
    (
      c("Canadian Doubles", "Canadian Singles", "Junior/Veteran", "K1 Junior", "K1 Ladies", "K1 Senior", "K1 Veteran", "K2 Junior", "K2 Junior Ladies", "K2 Ladies", "K2 Mixed", "K2 Senior", "K2 Veteran"),
      c("Green", "Grey", "Red", "Blue", "Blue", "Blue", "Blue", "Red", "Red", "Red", "Red", "Red", "Red")
    ))
  })

  output$WStableTimeYear <- DT::renderDataTable({
    DT::datatable(
      {
        data <- wstable_dataCrewsTimeY
        if (input$WSYearNT != "All") {
          data <- data[data$Year %in% input$WSYearNT, ]
        }
        if (input$WSCCRaceNT != "All") {
          data <- data[data$WSRace %in% input$WSCCRaceNT, ]
        }
        if (input$WSclNT != "All") {
          data <- data[data$WSClass %in% input$WSclNT, ]
        }
        #    if (input$WPLCC != "All") {data <- data[data$WSPosition == input$WPLCC,]}
        data # i.e. render this dataset as a table
      },
      #  filter = list(position = 'top', clear = FALSE),
      options = list(
        search = list(regex = TRUE, caseInsensitive = FALSE),
        pageLength = 50
      ),
      rownames = FALSE
    ) %>% formatStyle("WSClass", target = "row", Color = styleEqual
    (
      c("Canadian Doubles", "Canadian Singles", "Junior/Veteran", "K1 Junior", "K1 Ladies", "K1 Senior", "K1 Veteran", "K2 Junior", "K2 Junior Ladies", "K2 Ladies", "K2 Mixed", "K2 Senior", "K2 Veteran"),
      c("Green", "Grey", "Red", "Blue", "Blue", "Blue", "Blue", "Red", "Red", "Red", "Red", "Red", "Red")
    ))
  })

  output$WStableTime <- DT::renderDataTable({
    DT::datatable(
      {
        data <- wstable_dataCrewsTime
        #  if (input$WSYearT != "All") {data <- data[data$Year %in% input$WSYearT,]}
        if (input$WSCCRaceT != "All") {
          data <- data[data$WSRace %in% input$WSCCRaceT, ]
        }
        if (input$WSclT != "All") {
          data <- data[data$WSClass %in% input$WSclT, ]
        }
        #    if (input$WPLCC != "All") {data <- data[data$WSPosition == input$WPLCC,]}
        data # i.e. render this dataset as a table
      },
      #  filter = list(position = 'top', clear = FALSE),
      options = list(
        search = list(regex = TRUE, caseInsensitive = FALSE),
        pageLength = 50
      ),
      rownames = FALSE
    ) %>% formatStyle("WSClass", target = "row", Color = styleEqual
    (
      c("Canadian Doubles", "Canadian Singles", "Junior/Veteran", "K1 Junior", "K1 Ladies", "K1 Senior", "K1 Veteran", "K2 Junior", "K2 Junior Ladies", "K2 Ladies", "K2 Mixed", "K2 Senior", "K2 Veteran"),
      c("Green", "Grey", "Red", "Blue", "Blue", "Blue", "Blue", "Red", "Red", "Red", "Red", "Red", "Red")
    ))
  })




  output$WStableYear <- DT::renderDataTable({
    DT::datatable(
      {
        data <- wstable_dataCrewsYear
        if (input$WSYearY != "All") {
          data <- data[data$Year %in% input$WSYearY, ]
        }
        if (input$WSCCRaceY != "All") {
          data <- data[data$WSRace %in% input$WSCCRaceY, ]
        }
        if (input$WSclY != "All") {
          data <- data[data$WSClass %in% input$WSclY, ]
        }
        #  if (input$WPLCC != "All") {data <- data[data$WSPosition == input$WPLCC,]}
        data # i.e. render this dataset as a table
      },
      #  filter = list(position = 'top', clear = FALSE),
      options = list(
        search = list(regex = TRUE, caseInsensitive = FALSE),
        pageLength = 50
      ),
      rownames = FALSE
    ) %>% formatStyle("WSClass", target = "row", Color = styleEqual
    (
      c("Canadian Doubles", "Canadian Singles", "Junior/Veteran", "K1 Junior", "K1 Ladies", "K1 Senior", "K1 Veteran", "K2 Junior", "K2 Junior Ladies", "K2 Ladies", "K2 Mixed", "K2 Senior", "K2 Veteran"),
      c("Green", "Grey", "Red", "Blue", "Blue", "Blue", "Blue", "Red", "Red", "Red", "Red", "Red", "Red")
    ))
  })






  # options = list(
  order <- list(list(2, "asc"), list(4, "desc"))




  # filtered inputs for main table
  menus_filtered1 <- reactive({
    if ("All" %in% input$Year) {
      menus
    } else {
      menus %>% filter(Year %in% input$Year)
    }
  })
  observe({
    updateSelectInput(session, "Ladies", choices = c("All", menus_filtered1()$Ladies), selected = "All")
  })
  menus_filtered2 <- reactive({
    if ("All" %in% input$Ladies) {
      menus_filtered1()
    } else {
      menus_filtered1() %>% filter(Ladies %in% c(input$Ladies))
    }
  })
  observe({
    updateSelectInput(session, "div", choices = c("All", menus_filtered2()$Military), selected = "All")
  })



  output$WSattendance <- renderPlot({
    attchartdata <- wsmain_data %>%
      select(WSRace, Year, WSClass) %>%
      filter(WSClass %in% input$WSclass, Year == input$wsattyear)


    ggplot(attchartdata, aes(WSRace)) +
      geom_bar(aes(fill = WSClass), stat = "count") +
      coord_flip() +
      facet_wrap(~Year)
  })

  output$WS1attendance1 <- renderPlot({
    attchartdata <- wsmain_data %>%
      select(WSRace, Year, WSClass) %>%
      filter(WSClass %in% input$WS2class, WSRace %in% input$WSRace2)
    ggplot(attchartdata, aes(Year)) +
      geom_bar(aes(fill = WSClass), stat = "count") +
      scale_x_continuous(breaks = seq(0, 2050, 5))
  })


  #  output$top3sout <- renderText({
  #   paste("Number of Completions:",
  #        top3s$n[top3s$Name2 == input$paddler]
  # )


  #  })
 #  reference  https://www.r-graph-gallery.com/275-add-text-labels-with-ggplot2.html

  output$positions <- renderPlot({
    poschartdata <- main_data %>%
      select(Year, Position, Class, Name, DecimalTime, Flow, BoatType) %>%
      filter(str_detect(Name, input$paddler))
    a <- ggplot(poschartdata) +
      geom_point(aes(Year, Position, color = Class), position = position_jitter(width = 0.1, height = 0.1), size = 5) +
      scale_y_continuous(breaks = seq(2, 200, 2)) +
      scale_x_continuous(breaks = seq(1940, 2050, 5))+geom_text(aes(x=Year, y=Position,label=Flow),
                                                                nudge_x = 0.25, nudge_y = 0.25, 
                                                                check_overlap = T
      )
    b <- ggplot(poschartdata) +
      geom_point(aes(Year, DecimalTime, color = BoatType), position = position_jitter(width = 0.1, height = 0.1), size = 5) +
      scale_y_continuous(breaks = seq(2, 200, 1)) +
      scale_x_continuous(breaks = seq(0, 2050, 5))+geom_text(aes(x=Year, y=DecimalTime,label=Flow),
                                                             nudge_x = 0.25, nudge_y = 0.25, 
                                                             check_overlap = T
      )
    c <- ggplot(poschartdata) +
      geom_point(aes(Flow, DecimalTime, color = Class), position = position_jitter(width = 0.1, height = 0.1), size = 5) +
      scale_y_continuous(breaks = seq(2, 200, 1)) +
      scale_x_continuous(breaks = seq(0, 2050, 50))+geom_text(aes(x=Flow, y=DecimalTime,label=Year),
        nudge_x = 0.25, nudge_y = 0.25, 
       check_overlap = T
      )

    ggarrange(a, b, c, ncol = 3, nrow = 1)
  })


  output$TimePlacePositions <- renderPlot({
    poschartdata <- main_data %>%
      select(Year, Position, Class, Name, DecimalTime, Flow, BoatType) %>%
      filter(str_detect(Name, input$paddler))


    ggplot(poschartdata, aes(x = Class, y = DecimalTime, fill = Class)) +
      geom_boxplot() +
      ggtitle("Plot 3 - Box Plot Time") +
      stat_summary(fun = mean, geom = "point", size = 4)
  })

  output$TimePlacePositions1 <- renderPlot({
    poschartdata <- main_data %>%
      select(Year, Position, Class, Name, DecimalTime, Flow, BoatType) %>%
      filter(str_detect(Name, input$paddler))
    ggplot(poschartdata, aes(x = Class, y = Position, fill = Class)) +
      geom_boxplot() +
      ggtitle("Plot 3 - Box Plot Position") +
      stat_summary(fun = mean, geom = "point", size = 4)
  })


  output$TimePlacePositionsClub <- renderPlot({
    poschartdata <- main_data %>%
      select(Year, Position, Class, Club, DecimalTime, Flow, BoatType) %>%
      filter(str_detect(Club, input$regclub))


    ggplot(poschartdata, aes(x = Class, y = DecimalTime, fill = Class)) +
      geom_boxplot() +
      ggtitle("Plot 3 - Box Plot Time") +
      stat_summary(fun = mean, geom = "point", size = 4)
  })

  output$TimePlacePositions1Club <- renderPlot({
    poschartdata <- main_data %>%
      select(Year, Position, Class, Club, DecimalTime, Flow, BoatType) %>%
      filter(str_detect(Club, input$regclub))
    ggplot(poschartdata, aes(x = Class, y = Position, fill = Class)) +
      geom_boxplot() +
      ggtitle("Plot 3 - Box Plot Position") +
      stat_summary(fun = mean, geom = "point", size = 4)
  })


  output$SummaryEndCrew <- renderPrint({
    pos1Crew <- main_data %>%
      select(Name, Position, DecimalTime, Class) %>%
      filter(Position != "0") %>%
      filter(Class == "Endeavour") %>%
      filter(str_detect(Name, input$paddler))
    summary(pos1Crew[2:3])
  })


  output$SummaryJunCrew <- renderPrint({
    pos1Crew <- main_data %>%
      select(Name, Position, DecimalTime, Class) %>%
      filter(Position != "0") %>%
      filter(Class == "Junior") %>%
      filter(str_detect(Name, input$paddler))
    summary(pos1Crew[2:3])
  })

  output$SummarySenCrew <- renderPrint({
    pos1Crew <- main_data %>%
      select(Name, Position, DecimalTime, Class) %>%
      filter(Position != "0") %>%
      filter(Class == "Senior") %>%
      filter(str_detect(Name, input$paddler))
    summary(pos1Crew[2:3])
  })

  output$SummarySinCrew <- renderPrint({
    pos1Crew <- main_data %>%
      select(Name, Position, DecimalTime, Class) %>%
      filter(Position != "0") %>%
      filter(Class == "Singles") %>%
      filter(str_detect(Name, input$paddler))
    summary(pos1Crew[2:3])
  })

  output$SummaryVJCrew <- renderPrint({
    pos1Crew <- main_data %>%
      select(Name, Position, DecimalTime, Class) %>%
      filter(Position != "0") %>%
      filter(Class == "Vet/Junior") %>%
      filter(str_detect(Name, input$paddler))
    summary(pos1Crew[2:3])
  })



  output$SummaryEndClub <- renderPrint({
    pos1Crew <- main_data %>%
      select(Club, Position, DecimalTime, Class) %>%
      filter(Position != "0") %>%
      filter(Class == "Endeavour") %>%
      filter(str_detect(Club, input$regclub))
    summary(pos1Crew[2:3])
  })


  output$SummaryJunClub <- renderPrint({
    pos1Crew <- main_data %>%
      select(Club, Position, DecimalTime, Class) %>%
      filter(Position != "0") %>%
      filter(Class == "Junior") %>%
      filter(str_detect(Club, input$regclub))
    summary(pos1Crew[2:3])
  })

  output$SummarySenClub <- renderPrint({
    pos1Crew <- main_data %>%
      select(Club, Position, DecimalTime, Class) %>%
      filter(Position != "0") %>%
      filter(Class == "Senior") %>%
      filter(str_detect(Club, input$regclub))
    summary(pos1Crew[2:3])
  })

  output$SummarySinClub <- renderPrint({
    pos1Crew <- main_data %>%
      select(Club, Position, DecimalTime, Class) %>%
      filter(Position != "0") %>%
      filter(Class == "Singles") %>%
      filter(str_detect(Club, input$regclub))
    summary(pos1Crew[2:3])
  })

  output$SummaryVJClub <- renderPrint({
    pos1Crew <- main_data %>%
      select(Club, Position, DecimalTime, Class) %>%
      filter(Position != "0") %>%
      filter(Class == "Vet/Junior") %>%
      filter(str_detect(Club, input$regclub))
    summary(pos1Crew[2:3])
  })






  output$top3clubs <- renderText({
    paste(
      "Number of Completions:",
      top3clubs$n[top3clubs$Club == input$regclub]
    )
  })

  output$clubpositions <- renderPlot({
    poschartdata <- main_data %>%
      select(Year, Position, Class, Club, Name2, DecimalTime) %>%
      filter(str_detect(Club, input$regclub))
    a <- ggplot(poschartdata) +
      geom_point(aes(Year, Position, color = Class), position = position_jitter(width = 0.1, height = 0.1), size = 5) +
      scale_y_continuous(breaks = seq(0, 200, 5)) +
      scale_x_continuous(breaks = seq(0, 2050, 5))
    b <- ggplot(poschartdata) +
      geom_point(aes(Year, DecimalTime, color = Class), position = position_jitter(width = 0.1, height = 0.1), size = 5) +
      scale_y_continuous(breaks = seq(2, 200, 1)) +
      scale_x_continuous(breaks = seq(0, 2050, 5))
    c <- ggplot(poschartdata, aes(Year)) +
      geom_bar(aes(fill = Class), width = 0.4) +
      scale_y_continuous(breaks = seq(0, 200, 1)) +
      scale_x_continuous(breaks = seq(0, 2050, 5))

    ggarrange(a, b, c, ncol = 3, nrow = 1)
  })
  # ggplot(attchartdata, aes(Ladies))+geom_bar(aes(fill = Military), stat = "count")+coord_flip()+facet_wrap(~Year) + ggtitle("Plot 1 - By Sex")

  output$WSClubPositions <- renderPlot({
    poschartdata <- wsmain_data %>%
      select(Year, WSPosition, WSClass, WSClub, WSName2, WSDecimalTime, WSRace) %>%
      filter(str_detect(WSClub, input$WSregclub), WSRace %in% input$WSPCRace)
    a <- ggplot(poschartdata) +
      geom_point(aes(Year, WSPosition, color = WSClass), position = position_jitter(width = 0.1, height = 0.1), size = 5) +
      scale_y_continuous(breaks = seq(0, 200, 5)) +
      scale_x_continuous(breaks = seq(0, 2050, 5))
    b <- ggplot(poschartdata) +
      geom_point(aes(Year, WSDecimalTime, color = WSClass), position = position_jitter(width = 0.1, height = 0.1), size = 5) +
      scale_y_continuous(breaks = seq(2, 200, 1)) +
      scale_x_continuous(breaks = seq(0, 2050, 5))
    c <- ggplot(poschartdata, aes(Year)) +
      geom_bar(aes(fill = WSClass), width = 0.4) +
      scale_y_continuous(breaks = seq(0, 200, 1)) +
      scale_x_continuous(breaks = seq(0, 2050, 5))
    ggarrange(a, b, c, ncol = 3, nrow = 1)
  })

# Not Working
 # output$clubplot <- renderPlot({
  #  attchartdata <- datapipeclub1 %>%
   #   select(Year,  Club, Senior, Junior, Singles, VetJunior, Endeavour,Total) %>%
    #  filter(Total > 24)


  #  ggplot(attchartdata, aes(x = Club)) +
   #   geom_bar(aes(fill = Senior), stat = "identity") +
    #  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
     # labs(y = "Completions", x = "Largest Clubs shown by Number of Completions")
#  })
 
 
    output$clubplot <- renderPlot({
    attchartdata <- main_data %>% 
     select(Year, Class, Club1, C1) %>% 
    filter(Year %in% input$attclubyear, Class %in% input$attclubclass, C1 > 24 & C1 < 900)
      ggplot(attchartdata, aes(x = reorder(Club1, -table(Club1)[Club1]))) +
      geom_bar(aes(fill = Class), stat = "count") +
      geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
     labs(y = "Completions", x = "Largest Clubs shown by Number of Completions")
   })
    output$clubplotPre <- renderPlot({
      attchartdata <- main_data %>% 
        select(Year, Class, Club1, C1) %>% 
       filter( Class %in% input$attclubclass, Year < 1971, C1 > 10 & C1 < 900)
     
      ggplot(attchartdata, aes(x = reorder(Club1, -table(Club1)[Club1]))) +
        geom_bar(aes(fill = Class), stat = "count") +
        geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(y = "Completions", x = "Largest Clubs shown by Number of Completions")
    })








  output$WSclubplot <- renderPlot({
    attchartdata <- wsmain_data %>%
      select(Year, WSClub, WSRace, WSClass) %>%
      filter(Year %in% input$attWSclubyear, WSRace %in% input$WSCRace)
    ggplot(attchartdata, aes(x = reorder(WSClub, -table(WSClub)[WSClub]))) +
      geom_bar(aes(fill = WSRace), stat = "count") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(y = "Completions", x = "WaterSided Clubs shown by Number of Completions")
  })

  output$WSTime <- renderPlot({
    poschartdata <- wsmain_data %>%
      select(Year, WSPosition, WSClass, WSDecimalTime, WSRace) %>%
      filter(WSClass %in% input$attWStimeclass, WSPosition == input$WSPosition, Year >= input$attWSnyear, WSRace %in% input$WSWSRace2)
    ggplot(poschartdata) +
      geom_line(aes(Year, WSDecimalTime, color = WSClass), size = 5) +
      scale_y_continuous(breaks = seq(1, 200, 0.2)) +
      scale_x_continuous(breaks = seq(0, 2050, 5))
  })

  output$Record <- renderPlot({
    attchartdata <- main_data %>%
      select(Record, Class, DecimalTime) %>%
      filter(Class %in% input$attRecordclass, Record != "", Record != "Veteran (155 years) 74 & 81")
    ggplot(attchartdata, aes(fill = Record, x = Record, y = DecimalTime, color = Record)) +
      geom_bar(stat = "identity") +
      labs(y = "Record Time") +
      theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
      scale_y_continuous(breaks = seq(0, 40, 2)) +
      theme(text = element_text(
        size = 12,
        #       family="Comic Sans MS"))
        #   family="CM Roman"))
        #       family="TT Times New Roman"))
        #      family="Sans"))
        family = "Serif"
      ))
  })

  # output$TrophyTime <- renderPlot({poschartdata <- main_data %>% select(Year, Class, DecimalTime, Trophies) %>% filter(Class %in% input$attTrophytimeclass,   str_detect(Trophies, input$attTrophies))
  # ggplot(poschartdata, aes(fill = Trophies, x=Year, y=DecimalTime, color = Trophies)) +
  # geom_bar(stat="identity",  width = 0.2)+ scale_y_continuous(breaks=seq(0,70,2))+ scale_x_continuous(breaks=seq(0,2050,5))+coord_flip()
  # geom_bar(stat="identity")+coord_flip()+ labs( y="Trophy Time")

  # })


  output$Visitor <- renderPlot({
    attchartdata <- main_data %>%
      select(Country, Class) %>%
      filter(Country != "")
    ggplot(attchartdata, aes(x = Country, fill = Class, color = Class)) +
      geom_bar(stat = "Count") +
      labs(y = "Completions")
  })





  output$Mattendance <- renderPlot({
    attchartdata <- main_data %>%
      select(BoatType, Ladies, Year, Military, Veteran, Class, Club, SubClass) %>%
      filter(BoatType %in% input$region, Year %in% input$attyear, Military %in% input$attdiv, Class %in% input$attclass)
    bxp <- ggplot(attchartdata, aes(Ladies)) +
      geom_bar(aes(fill = Military), stat = "count") +
      coord_flip() +
      facet_wrap(~Year) +
      ggtitle("Plot 1 - By Sex") +
      labs(y = "Completions") +
      labs(x = "Sex")
    bxp
    b <- ggplot(attchartdata, aes(Veteran)) +
      geom_bar(aes(fill = Military), stat = "count") +
      coord_flip() +
      facet_wrap(~Year) +
      ggtitle("Plot 2 - By Age Group") +
      labs(y = "Completions") +
      labs(x = "Age Group")
    b
    dp <- ggplot(attchartdata, aes(Class)) +
      geom_bar(aes(fill = Military), stat = "count") +
      coord_flip() +
      facet_wrap(~Year) +
      ggtitle("Plot 3 - By Class") +
      labs(y = "Completions") +
      labs(x = "Civilian or Arm")
    dp
    # attchartdata <- main_data %>%
    #    select(BoatType, Ladies, Year, Military, Class, Club, SubClass) %>%
    #    filter(BoatType %in% input$region, Year %in% input$attyear, Military %in% input$attdiv, Class %in% input$attclass, SubClass != "")

    # sc <- ggplot(attchartdata, aes(SubClass))+geom_bar(aes(fill = BoatType), stat = "count")+coord_flip()+facet_wrap(~Year) + ggtitle("Plot 4 - By Other Subclass")
    # sc
    ggarrange(bxp, b, dp, ncol = 3, nrow = 1)
  })
  #  ggarrange(sc,  ncol = 1, nrow = 1)})



  output$tmcchart <- renderPlot({
    poschartdata <- datapipe1 %>%
      filter(Total >= 8) %>%
      select(Crew, Senior, Junior, Singles, VetJunior, Endeavour)%>%
      filter()
    dat <- melt(poschartdata, id = "Crew")
    ggplot(dat, aes(Crew, value, fill = variable)) +
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(y = "Completions", x = "") +
      labs(title = "Chart by All Classes") +
      scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24))
  })

  output$tmcchart1 <- renderPlot({
    poschartdata <- datapipe1 %>%
      filter(Total >= 8) %>%
      select(Crew, input$ClassTC)
    dat <- melt(poschartdata, id = "Crew")
    ggplot(dat, aes(Crew, value, fill = variable)) +
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(y = "Completions", x = "") +
      labs(title = "Chart by Selected") +
      scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24))
  })
  
#  select(Year, WSClub, WSRace, WSClass) %>%
 #   filter(Year %in% input$attWSclubyear, WSRace %in% input$WSCRace)ClassTC

  output$tmcchart2 <- renderPlot({
    poschartdata <- datapipe1 %>%
      filter(Total >= 8) %>%
      select(Crew, Junior)
    dat <- melt(poschartdata, id = "Crew")
    ggplot(dat, aes(Crew, value, fill = variable)) +
      geom_bar(stat = "identity", fill = "#E69F00") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(y = "Completions", x = "") +
      labs(title = "Chart by Junior Class") +
      scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24))
  })

  output$tmcchart3 <- renderPlot({
    poschartdata <- datapipe1 %>%
      filter(Total >= 8) %>%
      select(Crew, Singles)
    dat <- melt(poschartdata, id = "Crew")
    ggplot(dat, aes(Crew, value, fill = variable)) +
      geom_bar(stat = "identity", fill = "#009E73") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(y = "Completions", x = "") +
      labs(title = "Chart by Singles Class") +
      scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24))
  })

  output$tmcchart4 <- renderPlot({
    poschartdata <- datapipe1 %>%
      filter(Total >= 8) %>%
      select(Crew, VetJunior)
    dat <- melt(poschartdata, id = "Crew")
    ggplot(dat, aes(Crew, value, fill = variable)) +
      geom_bar(stat = "identity", fill = "#0072B2") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(y = "Completions", x = "") +
      labs(title = "Chart by Vet Junior Class") +
      scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24))
  })

  output$tmcchart5 <- renderPlot({
    poschartdata <- datapipe1 %>%
      filter(Total >= 8) %>%
      select(Crew, Endeavour)
    dat <- melt(poschartdata, id = "Crew")
    ggplot(dat, aes(Crew, value, fill = variable)) +
      geom_bar(stat = "identity", fill = "#CC79A7") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(y = "Completions", x = "") +
      labs(title = "Chart by Endeavour Class") +
      scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24))
  })




  #  output$tmcchartBT <- renderPlot({poschartdata <- datapipe1%>%filter(Total >= 8) %>% select(Crew, Canadian, Singles)
  # dat <- melt(poschartdata,id="Crew")
  # ggplot(dat,aes(Crew,value, fill=variable))+geom_bar(stat="identity")+theme(axis.text.x=element_text(angle=90, hjust=1))+ labs(y = "Completions", x="Paddler") + labs(title = "Chart by class")+ scale_y_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20,22,24))
  #  })

  output$tmcchartBT1 <- renderPlot({
    poschartdata <- datapipe1 %>%
      filter(Total >= 8) %>%
      select(Crew, Kayak, Canadian, Folding)
    dat <- melt(poschartdata, id = "Crew")
    ggplot(dat, aes(Crew, value, fill = variable)) +
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(y = "Completions", x = "Paddler") +
      labs(title = "Chart by Boat type") +
      scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24))
  })




  output$Time <- renderPlot({
    poschartdata <- main_data %>%
      select(Year, Position, Class, DecimalTime) %>%
      filter(Class %in% input$atttimeclass, Position == input$Position, Year >= input$attnyear)
    ggplot(poschartdata) +
      geom_line(aes(Year, DecimalTime, color = Class), size = 3) +
      scale_x_continuous(breaks = seq(0, 2050, 2))
  })

  output$TimePlace <- renderPlot({
    poschartdata <- main_data %>%
      select(Year, Position, Class, DecimalTime) %>%
      filter(Class %in% input$atttimeclass, Position == input$Position, Year >= input$attnyear)



    ggplot(poschartdata, aes(x = Class, y = DecimalTime, fill = Class)) +
      geom_boxplot() +
      ggtitle("Plot 3 - Box Plot Position") +
      stat_summary(fun = mean, geom = "point", size = 4) +
      geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5)
  })


  output$SummarySen <- renderPrint({
    pos1 <- main_data %>%
      select(Year, Position, Class, DecimalTime) %>%
      filter(Class == "Senior", Position == input$Position, Year >= input$attnyear)
    summary(pos1["DecimalTime"])
  })

  output$SummaryJun <- renderPrint({
    pos2 <- main_data %>%
      select(Year, Position, Class, DecimalTime) %>%
      filter(Class == "Junior", Position == input$Position, Year >= input$attnyear)
    summary(pos2["DecimalTime"])
  })

  output$SummarySin <- renderPrint({
    pos2 <- main_data %>%
      select(Year, Position, Class, DecimalTime) %>%
      filter(Class == "Singles", Position == input$Position, Year >= input$attnyear)
    summary(pos2["DecimalTime"])
  })

  output$SummaryVJun <- renderPrint({
    pos2 <- main_data %>%
      select(Year, Position, Class, DecimalTime) %>%
      filter(Class == "Vet/Junior", Position == input$Position, Year >= input$attnyear)
    summary(pos2["DecimalTime"])
  })

  output$SummaryEnd <- renderPrint({
    pos2 <- main_data %>%
      select(Year, Position, Class, DecimalTime) %>%
      filter(Class == "Endeavour", Position == input$Position, Year >= input$attnyear)
    summary(pos2["DecimalTime"])
  })






  output$ProfileTime <- renderPlot({
    poschartdatasp <- SplitTimeData %>%
      select(Distance, Crew, Time, Flow) %>%
      filter(Crew %in% input$attsplitcrew)

    ggplot(poschartdatasp) +
      geom_point(aes(Distance, Time, color = Crew), size = 3) +
      scale_x_continuous(breaks = seq(0, 130, 5))
  })
  
#  geom_point(data = poschartdatasp, aes(x = TimePoint, y = value),size = 5, colour = "light blue") +
 #   geom_point(data = poschartdataspselect, aes(x = TimePoint, y = value),size = 5, colour = "red") +
  
  
  output$plotCheckBox <- renderPlot({
    poschartdatasp <- DW2017S %>%
      select(Year, TimePoint, value) %>%
      filter(Year %in% input$splityearP1)

    ggplot(poschartdatasp, aes(x = 0, y = value, fill = TimePoint)) +
      geom_boxplot() +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      ) +
      labs(y = "Speed mph") +
      scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))+ geom_vline(xintercept = 0.1875,colour="black", size = 1)
  })
  
  
#  output$SummaryCrewSeniorComparison <- renderPrint({
 #   pos1CrewA <- DW2017S %>%
  #    select(Year, TimePoint, value, Class) %>%
   #   filter(Year %in% input$splityearP1)%>%
     # filter(Position != "0") %>%
    #  filter(Class == "Senior")%>%
     # filter(Year %in% input$splityearP1)
     
  #  summary(pos1Crew[2:3])
#  })
  

  output$plotCheckBox1 <- renderPlot({
    poschartdatasp <- DW2017STDev %>%
      select(Year, TimePoint, value) %>%
      filter(Year %in% input$splityearP1P)

    ggplot(poschartdatasp, aes(x = 0, y = value, fill = TimePoint)) +
      geom_boxplot() +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      ) +
      labs(y = "Time 24 hour extended")
  })

  # splityearP1P



  # Distribution Plots


  output$plotCheckBoxFHun <- renderPlot({
    poschartdatasp <- DW2017STDev %>%
      select(Year, TimePoint, value) %>%
      filter(Year %in% input$splityearP1F, TimePoint == input$TimePointInput)
    ggplot(poschartdatasp, aes(value, fill = Year)) +
      geom_histogram(fill = "red", binwidth = 0.033) +
      xlab("Time 24 hour clock - extended to 48 hours") +
      scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
      scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50))
  })

  output$plotCheckBoxFHunAll <- renderPlot({
    poschartdatasp <- DW2017STDev %>%
      select(Year, TimePoint, value) %>%
      filter(Year %in% input$splityearP1F, TimePoint %in% input$TimePointInput)
    ggplot(poschartdatasp, aes(value, fill = TimePoint)) +
      geom_histogram( binwidth = 0.033) +
      xlab("Time 24 hour clock - extended to 48 hours  (25 represents 1am day two)") +
      scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
      scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50))
  })
  
  
#  filter(CompletionRate >= input$comrateR, MaxWind <= input$windgR, MinTemperature >= input$tempgR)
 # column(3, selectInput("OldWindsorCutoffInput", "Choose Cutoff Time at Old Windsor: ", OldWindsorCutoff, selected = "27.5", multiple = FALSE))),
  
  output$plotCheckBoxFHunRT <- renderPlot({
    poschartdatasp <- DW2017SN %>%
      select(Year, TidewayTime, RaceTime, PewseyTime,NewburyTime,ReadingTime,MarlowTime,OldWindsorTime,SheppertonTime) %>%
 #     filter(Year %in% input$splityearP1FRT, ReadingTime <= input$ReadingCutoffInput)
     filter(Year %in% input$splityearP1FRT, NewburyTime < input$NewburyCutoffInput, ReadingTime <= input$ReadingCutoffInput, MarlowTime <= input$MarlowCutoffInput, OldWindsorTime <= input$OldWindsorCutoffInput, 
                                         SheppertonTime <= input$SheppertonCutoffInput)
        
    a <- ggplot(poschartdatasp, aes(x = RaceTime)) +
      geom_histogram(fill = "red", binwidth = 0.25) +
      scale_x_continuous(breaks = c(16, 8, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 42, 44, 46, 48, 50, 60))
    b <- ggplot(poschartdatasp, aes(TidewayTime)) +
      geom_histogram(fill = "blue", binwidth = 0.1) +
      scale_x_continuous(breaks = c(1.5, 2.0, 2.5, 3.0, 3.5, 4, 4.5, 5, 6, 7, 8))
    c <- ggplot(poschartdatasp, aes(PewseyTime)) +
      geom_histogram(fill = "dark green", binwidth = 0.1) +
      
      scale_x_continuous(breaks = c(1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4, 2.6, 2.8, 3.0, 3.2, 3.4, 3.6, 3.8, 4, 4.2, 4.4, 4.6, 4.8, 5, 6, 7, 8))
 #   d <- ggplot(poschartdatasp, aes(NewburyTime)) +
  #    geom_histogram(fill = "dark green", binwidth = 0.2) +
   #   scale_x_continuous(breaks = c(4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5,8,8.5,9,9.5,10,11))
#    e <- ggplot(poschartdatasp, aes(ReadingTime)) +
 #     geom_histogram(fill = "dark green", binwidth = 0.2) +
  #    scale_x_continuous(breaks = c(7,8,9,10,11,12,13,14,15,16,17,18))
#    f <- ggplot(poschartdatasp, aes(MarlowTime)) +
 #     geom_histogram(fill = "dark green", binwidth = 0.2) +
  #    scale_x_continuous(breaks = c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22))
#    g <- ggplot(poschartdatasp, aes(OldWindsorTime)) +
 #     geom_histogram(fill = "dark green", binwidth = 0.2) +
  #    scale_x_continuous(breaks = c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27))
#    h <- ggplot(poschartdatasp, aes(SheppertonTime)) +
 #     geom_histogram(fill = "dark green", binwidth = 0.2) +
  #    scale_x_continuous(breaks = c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30))
    
    
 #   ggarrange(a, b, c, d,  e,  f,  g,  h,  ncol = 4, nrow = 2)%>%
    ggarrange(a, b, c,   ncol = 3, nrow = 1)
   
  })
  output$plotCheckBoxFHunRT1 <- renderPlot({
    poschartdatasp <- DW2017SN %>%
      select(Year, TidewayTime, RaceTime, PewseyTime,NewburyTime,ReadingTime,MarlowTime,OldWindsorTime,SheppertonTime) %>%
  #    filter(Year %in% input$splityearP1FRT, OldWindsorTime <= input$OldWindsorCutoffInput)
      filter(Year %in% input$splityearP1FRT, NewburyTime < input$NewburyCutoffInput, ReadingTime <= input$ReadingCutoffInput, MarlowTime <= input$MarlowCutoffInput, OldWindsorTime <= input$OldWindsorCutoffInput,
             SheppertonTime <= input$SheppertonCutoffInput) 
#    a <- ggplot(poschartdatasp, aes(x = RaceTime)) +
 #     geom_histogram(fill = "red", binwidth = 0.25) +
  #    scale_x_continuous(breaks = c(16, 8, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 42, 44, 46, 48, 50, 60))
#    b <- ggplot(poschartdatasp, aes(TidewayTime)) +
 #     geom_histogram(fill = "blue", binwidth = 0.1) +
  #    scale_x_continuous(breaks = c(1.5, 2.0, 2.5, 3.0, 3.5, 4, 4.5, 5, 6, 7, 8))
#    c <- ggplot(poschartdatasp, aes(PewseyTime)) +
 #     geom_histogram(fill = "dark green", binwidth = 0.1) +
  #    scale_x_continuous(breaks = c(1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4, 2.6, 2.8, 3.0, 3.2, 3.4, 3.6, 3.8, 4, 4.2, 4.4, 4.6, 4.8, 5, 6, 7, 8))
       d <- ggplot(poschartdatasp, aes(NewburyTime)) +
         geom_histogram(fill = "dark green", binwidth = 0.1) +
         scale_x_continuous(breaks = c(4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5,8,8.5,9,9.5,9.9))+ geom_vline(xintercept = 10,colour="black", size = 1)
        e <- ggplot(poschartdatasp, aes(ReadingTime)) +
         geom_histogram(fill = "dark green", binwidth = 0.2) +
        scale_x_continuous(breaks = c(7,8,9,10,11,12,13,14,15,16,17,18))+ geom_vline(xintercept = 16,colour="black", size = 1)
        f <- ggplot(poschartdatasp, aes(MarlowTime)) +
         geom_histogram(fill = "dark green", binwidth = 0.2) +
        scale_x_continuous(breaks = c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22))+ geom_vline(xintercept = 20.5,colour="black", size = 1)
  #      g <- ggplot(poschartdatasp, aes(OldWindsorTime)) +
   #      geom_histogram(fill = "dark green", binwidth = 0.2) +
    #    scale_x_continuous(breaks = c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27))
     #   h <- ggplot(poschartdatasp, aes(SheppertonTime)) +
      #   geom_histogram(fill = "dark green", binwidth = 0.2) +
       # scale_x_continuous(breaks = c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30))
    
    
    #   ggarrange(a, b, c, d,  e,  f,  g,  h,  ncol = 4, nrow = 2)%>%
    ggarrange(d, e, f, ncol = 3, nrow = 1)
    
  })
  output$plotCheckBoxFHunRT2 <- renderPlot({
    poschartdatasp <- DW2017SN %>%
      select(Year, TidewayTime, RaceTime, PewseyTime,NewburyTime,ReadingTime,MarlowTime,OldWindsorTime,SheppertonTime,TeddingtonTime) %>%
#      filter(Year %in% input$splityearP1FRT, OldWindsorTime <= input$OldWindsorCutoffInput)
      filter(Year %in% input$splityearP1FRT, NewburyTime < input$NewburyCutoffInput, ReadingTime <= input$ReadingCutoffInput, MarlowTime <= input$MarlowCutoffInput, OldWindsorTime <= input$OldWindsorCutoffInput, 
             SheppertonTime <= input$SheppertonCutoffInput) 
    #    a <- ggplot(poschartdatasp, aes(x = RaceTime)) +
    #     geom_histogram(fill = "red", binwidth = 0.25) +
    #    scale_x_continuous(breaks = c(16, 8, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 42, 44, 46, 48, 50, 60))
    #    b <- ggplot(poschartdatasp, aes(TidewayTime)) +
    #     geom_histogram(fill = "blue", binwidth = 0.1) +
    #    scale_x_continuous(breaks = c(1.5, 2.0, 2.5, 3.0, 3.5, 4, 4.5, 5, 6, 7, 8))
    #    c <- ggplot(poschartdatasp, aes(PewseyTime)) +
    #     geom_histogram(fill = "dark green", binwidth = 0.1) +
    #    scale_x_continuous(breaks = c(1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4, 2.6, 2.8, 3.0, 3.2, 3.4, 3.6, 3.8, 4, 4.2, 4.4, 4.6, 4.8, 5, 6, 7, 8))
    #    d <- ggplot(poschartdatasp, aes(NewburyTime)) +
    #     geom_histogram(fill = "dark green", binwidth = 0.2) +
    #    scale_x_continuous(breaks = c(4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5,8,8.5,9,9.5,10,11))
    i <- ggplot(poschartdatasp, aes(TeddingtonTime)) +
      geom_histogram(fill = "dark green", binwidth = 0.2) +
      scale_x_continuous(breaks = c(15,18,20,22,24,26,28,30,32,34,36,38,40))
 #   f <- ggplot(poschartdatasp, aes(MarlowTime)) +
  #    geom_histogram(fill = "dark green", binwidth = 0.2) +
   #   scale_x_continuous(breaks = c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22))
   g <- ggplot(poschartdatasp, aes(OldWindsorTime)) +
      geom_histogram(fill = "dark green", binwidth = 0.2) +
     scale_x_continuous(breaks = c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27))+ geom_vline(xintercept = 24,colour="black", size = 1)
   h <- ggplot(poschartdatasp, aes(SheppertonTime)) +
      geom_histogram(fill = "dark green", binwidth = 0.2) +
      scale_x_continuous(breaks = c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30))+ geom_vline(xintercept = 27.5,colour="black", size = 1)
 #  x <-column(3, verbatimTextOutput("SummaryPewseyTime"))
    
    #   ggarrange(a, b, c, d,  e,  f,  g,  h,  ncol = 4, nrow = 2)%>%
    ggarrange(g,h,i,  ncol = 3, nrow = 1)
 #  column(3, verbatimTextOutput("SummaryJunCrew")),
  })
  

  # ggplot(diamonds, aes(x = carat)) + geom_histogram(fill = "red", bins = 30)


  # a <- ggplot(poschartdata, aes(DecimalTime,fill = Class)) +
  #  geom_histogram(binwidth=.5, position="dodge")+ ggtitle("Plot 1 - By Individual Class")

  # ggplot(poschartdata, aes(SubClass)) +geom_histogram(aes(fill = SubClass), stat = "count", width = 0.5)+ geom_text(stat='count', aes(label=..count..), vjust=-1)
  ################################################# New below##########################



  output$plotProgress <- renderPlot({
    poschartdatasp <- DW2017STDev %>%
      select(Year, TimePoint, value, Crew, Place) %>%
      filter(Year %in% input$splityearP1P)#, Place %in% input$attsplitcrewProgress)
    poschartdataspselect <- DW2017STDev %>%
      select(Year, TimePoint, value, Crew, Place) %>%
      filter(Year %in% input$splityearP1P, Place %in% input$attsplitcrewProgress)
    ggplot(poschartdatasp) +
      geom_point(data = poschartdatasp, aes(x = TimePoint, y = value),size = 5, colour = "light blue") +
      geom_point(data = poschartdataspselect, aes(x = TimePoint, y = value),size = 3, colour = "red") +
 #     scale_colour_gradientn(colours = rainbow(4)) +
      labs(y = "Arrival Time - 24 hour clock extended") +
      scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50))
  })
  # ggplot(df, aes(trt, outcome)) +geom_point()








  output$plotCheckPoint1 <- renderPlot({
    poschartdatasp <- DW2017S %>%
      select(Year, Place, TimePoint, value, Crew) %>%
      filter(Place %in% input$attsplitcrewcheck1, Year %in% input$splityearP1)
    ggplot(poschartdatasp) +
      geom_point(aes(x = TimePoint, y = value, colour = Place), size = 5) +
      scale_colour_gradientn(colours = rainbow(4)) +
      labs(y = "Speed mph")+ geom_vline(xintercept = 12.5,colour="black", size = 1)
  })



  output$plotCheckPoint2 <- renderPlot({
    poschartdatasp <- DW2017S %>%
      select(Year, Place, TimePoint, value, Crew) %>%
      filter(Place %in% input$attsplitcrewcheck2, Year %in% input$splityearP2)
    ggplot(poschartdatasp) +
      geom_point(aes(x = TimePoint, y = value, colour = Year), size = 5) +
      scale_colour_gradientn(colours = rainbow(4)) +
      labs(y = "Speed mph")+ geom_vline(xintercept = 12.5,colour="black", size = 1)
  })



  output$plotCheckPoint3Time <- renderPlot({
    poschartdatasp <- DW2017STime %>%
      select(Year, Place, TimePoint, value, Crew) %>%
      filter(str_detect(Crew, input$paddlerSplit))
    ggplot(poschartdatasp) +
      geom_point(aes(x = TimePoint, y = value, colour = Year), size = 3) +
      scale_colour_gradientn(colours = rainbow(4)) +
      labs(y = "Progress Time")
  })

  output$plotCheckPoint3 <- renderPlot({
    poschartdatasp <- DW2017S %>%
      select(Year, Place, TimePoint, value, Crew) %>%
      filter(str_detect(Crew, input$paddlerSplit))
    ggplot(poschartdatasp) +
      geom_point(aes(x = TimePoint, y = value, colour = Year), size = 3) +
      scale_colour_gradientn(colours = rainbow(4)) +
      labs(y = "Speed mph")+ geom_vline(xintercept = 12.5,colour="black", size = 1)
  })
  
  
  
  
  
  # we are here

  
 # output$plotCheckPoint1 <- renderPlot({
  #  poschartdatasp <- DW2017S %>%
   #   select(Year, Place, TimePoint, value, Crew) %>%
    #  filter(Place %in% input$attsplitcrewcheck1, Year %in% input$splityearP1)
#    ggplot(poschartdatasp) +
 #     geom_point(aes(x = TimePoint, y = value, colour = Place), size = 5) +
  #    scale_colour_gradientn(colours = rainbow(4)) +
   #   labs(y = "Speed mph")+ geom_vline(xintercept = 12.5,colour="black", size = 1)
#  })
  
  
  
  output$TeddingtonCutoff <- renderPlot({
    
  #  poschartdatasp <- plotCutOff%>%
   #    select(Year, L_Teddington, HighTide)
    ggplot(plotCutOff,aes(x = Year)) +
      geom_point( aes(y = L_Teddington), colour="blue", size = 5)+
      geom_point( aes(y = HighTide2Decimal), colour="green", size = 15,shape = 95)+
      geom_point( aes(y = Stop1), colour="black", size = 15,shape = 95)+
      geom_point( aes(y = Stop2), colour="black", size = 15, shape = 95)+
      geom_point( aes(y = HighTideDecimal), colour="red", size = 15,shape = 95)+ labs(y = "Teddington Check Point")+
      scale_x_continuous(breaks = seq(2006, 2024, 2)) 
  })  
 
  #  ggplot(poschartdataspHT, aes(Year,HighTide)) +
   #   geom_point(colour="Red", size = 5)
    
  #    scale_colour_gradientn(colours = rainbow(4)) +
   #   + labs(y = "Time Point")#+ geom_vline(xintercept = 12.5,colour="black", size = 1)
#  })

  output$tableTeddingtonCutoff <- DT::renderDataTable({
    DT::datatable(
      { data <- DWHighTideB %>% filter(Year > 2006)
        data # i.e. render this dataset as a table
        # unite(table_dataPosition,"SubClasses", c("Veteran","Military","Ladies","SubClass"), sep = ", ")
      },
      options = list(
        search = list(regex = TRUE, caseInsensitive = FALSE),
        pageLength = 75
      )
     # rownames = FALSE
    )%>% formatRound(columns = "HighTideDecimal", digits = 2)%>%formatRound(columns = "HighTide2Decimal", digits = 2)%>% formatRound(columns = "Stop1", digits = 2)%>%formatRound(columns = "Stop2", digits = 2)
  })
  
  
  
  
  
  
  
  

  output$plotCheckPointCrew <- renderPlot({
    poschartdatasp <- DW2017S %>%
      select(Year, Place, TimePoint, value) %>%
      filter(Place %in% input$attsplitcrewcheck, Year %in% input$splityearP)

    ggplot(poschartdatasp) +
      geom_point(aes(x = TimePoint, y = value, colour = Place), size = 5) +
      scale_colour_gradientn(colours = rainbow(4)) +
      labs(y = "Speed mph")
  })


  output$ProfileTimeSplit <- renderPlot({
    poschartdatasp <- SplitTimeData %>%
      select(Distance, Crew, Speed, Flow) %>%
      filter(Crew %in% input$attsplitcrew)

    ggplot(poschartdatasp) +
      geom_point(aes(Distance, Speed, color = Crew), size = 5) +
      scale_x_continuous(breaks = seq(0, 130, 5))
  })

  output$ProfileTimeSplitSpeed <- renderPlot({
    poschartdatasp <- Sptest2 %>%
      select(SplitTimePoint, Crew, SplitSpeed, Flow) %>%
      filter(Crew %in% input$attsplitcrew)


    ggplot(poschartdatasp, aes(fill = SplitTimePoint, x = Crew, y = SplitSpeed)) +
      geom_bar(stat = "identity", position = "dodge2", na.rm = FALSE, )
  })

  output$ProfileTimeSplitSpeedIncrease <- renderPlot({
    poschartdatasp <- Sptest3 %>%
      select(SpeedIncrease, Crew, Section, Flow) %>%
      filter(Crew %in% input$attsplitcrew)


    ggplot(poschartdatasp, aes(fill = Section, x = Crew, y = SpeedIncrease)) +
      geom_bar(stat = "identity", position = "dodge2")
  })

  output$ProfileTimeSplitSpeedIncreaseXY <- renderPlot({
    poschartdatasp <- Sptest3 %>% select(SpeedIncrease, Crew, Section, Flow)
    #  %>% filter(Crew %in% input$attsplitcrew)
    ggplot(poschartdatasp) +
      geom_point(aes(Flow, SpeedIncrease, color = Section), size = 3) +
      scale_x_continuous(breaks = seq(0, 250, 10))

    # ggplot(poschartdatasp, aes(fill = Section, x=Flow, y = SpeedIncrease)) +geom_bar(stat="identity", position = "dodge2")
  })
  
#  plotCutOff <- merge(DWHighTideB,DWHighTimeA, by="Year")%>% filter(Year > 2006)
#  output$TeddingtonCutoff <- renderPlot({
#  ggplot(plotCutOff,aes(x = Year)) +
 #   geom_point( aes(y = L_Teddington), colour="blue", size = 5)+
  #  geom_point( aes(y = HighTide2Decimal), colour="green", size = 15,shape = 95)+
   # geom_point( aes(y = Stop1), colour="black", size = 15,shape = 95)+
    #geom_point( aes(y = Stop2), colour="black", size = 15, shape = 95)+
    #geom_point( aes(y = HighTideDecimal), colour="red", size = 15,shape = 95)+ labs(y = "Teddington Check Point")+
    #scale_x_continuous(breaks = seq(2006, 2024, 2)) 
#}) 
  

  output$WeatherYearB <- renderPlot({
    poschartdata <- WMMain_Data %>%
      select(Year, SeniorFlow) %>%
      filter(Year > 1970)
    poschartdataBY <- WMMain_Data %>%
      select(Year, SeniorFlow) %>%
      filter(Year %in% BlackyearsList)

    ggplot(poschartdata, aes(Year, SeniorFlow)) +
      geom_point(aes(colour = cut(SeniorFlow, c(-Inf, 65, 100, 120, Inf))),
        size = 5
      ) +
      scale_color_manual(
        name = "SeniorFlow",
        values = c(
          "(-Inf,65]" = "green",
          "(65,100]" = "yellow",
          "(100,120]" = "orange",
          "(120, Inf]" = "red"
        ),
        labels = c("<= 65", "> 80 <= 100", "> 100 < 120", "> 120")
      )+
      geom_point(data=poschartdataBY, aes(Year, SeniorFlow),colour = "black",size = 5)
  })
  
#  output$WeatherYearB <- renderPlot({
 #   poschartdataBY <- WMMain_Data %>%
 #     select(Year, SeniorFlow) %>%
  #    filter(Year %in% BlackyearsList)
#  ggplot(poschartdataBY, aes(Year, SeniorFlow)) +
 #   geom_point(colour = "black",size = 5)
#  })
               

  

#  attTrophiesT is trophy
  library(stringr)
  
  output$tableCrewsRT <- DT::renderDataTable({
    DT::datatable(
      {
        data <- table_dataTrophies
        data <- data[data$Class %in% input$attTrophytimeclassT, ]
        
        # Split values in the "Trophies" column
        search_terms <- unlist(str_split(input$attTrophiesT, ","))
        
        # Create a regular expression pattern to match any of the search terms
        pattern <- paste(search_terms, collapse = "|")
        
        # Filter data based on search terms
        data <- data[str_detect(data$Trophies, pattern), ]
        
        data
      },
      options = list(
        search = list(regex = TRUE, caseInsensitive = FALSE, smart = FALSE),
        pageLength = 100
      ),
      rownames = FALSE
    ) %>% formatStyle("Class", target = "row", Color = styleEqual(c("Senior", "Junior", "Singles", "Vet/Junior", "Endeavour"), c("Blue", "Red", "Green", "brown", "grey")))
  })

  output$TrophyTime <- renderPlot({
    poschartdata <- main_data %>%
      select(Year, Class, DecimalTime, Trophies) %>%
      filter(Class %in% input$attTrophytimeclass, str_detect(Trophies, input$attTrophies))
    ggplot(poschartdata, aes(fill = Trophies, x = Year, y = DecimalTime, color = Trophies)) +
      geom_bar(stat = "identity", width = 0.2) +
      scale_y_continuous(breaks = seq(0, 70, 2)) +
      scale_x_continuous(breaks = seq(0, 2050, 5)) #+coord_flip()
    # geom_bar(stat="identity")+coord_flip()+ labs( y="Trophy Time")
  })


  #  Record <- renderPlot({attchartdata <- main_data %>% select(Record, Class, DecimalTime) %>% filter( Class %in% input$attRecordclass, Record != "", Record != "Veteran (155 years) 74 & 81")
  # ggplot(attchartdata, aes(fill = Record, x=Record, reorder(DecimalTime,-table(DecimalTime)[DecimalTime]), color = Record)) +
  #  geom_bar(stat="identity")+coord_flip()+ labs( y="Record Time")
  # })


 # column(3, selectInput("attnyear2", "Choose one or more Years", c(unique(menus$Year)), selected = "2019", multiple = TRUE)),
#  column(3, selectInput("atttimeclass1", "Choose one or more Classes", c(unique(divsclass$Class)), selected = "Senior", multiple = TRUE)),
#  fluidRow(column(12, plotOutput("Distribution"))),



  output$Distribution <- renderPlot({
    poschartdata <- main_data %>%
      select(Year, Class, DecimalTime) %>%
      filter(Year %in% input$attnyear2, Class %in% input$atttimeclass1)

    a <- ggplot(poschartdata, aes(DecimalTime, fill = Class)) +
      geom_histogram(binwidth = .5, position = "dodge") +
      ggtitle("Plot 1 - By Individual Class")

    b <- ggplot(poschartdata, aes(DecimalTime, fill = Class)) +
      geom_histogram(binwidth = .5) +
      ggtitle("Plot 2 - By Combined Class")

    c <- ggplot(poschartdata, aes(x = Class, y = DecimalTime, fill = Class)) +
      geom_boxplot() +
      ggtitle("Plot 3 - Box Plot Class - Mean as Diamond")
    ggarrange(a, b, c, ncol = 3, nrow = 1)
    
  })
  
a1<-  output$SummaryPewsey <- renderPrint({
    pos1dist <- DW2017SN %>%
      filter(Year %in% input$splityearP1FRT, OldWindsorTime <= input$OldWindsorCutoffInput)%>%
      select(Year, PewseyTime)
    summary(na.omit(pos1dist))
  })
a2<-  output$SummaryRace <- renderPrint({
    pos1dist <- DW2017SN %>%
      filter(Year %in% input$splityearP1FRT, OldWindsorTime <= input$OldWindsorCutoffInput)%>%
      select(Year, RaceTime) 
    summary(na.omit(pos1dist))
  })
a3<-  output$SummaryTideway <- renderPrint({
    pos1dist <- DW2017SN %>%
      
      filter(Year %in% input$splityearP1FRT, OldWindsorTime <= input$OldWindsorCutoffInput)%>%
      select(Year,TidewayTime)
    summary(na.omit(pos1dist))
  })
a4<-  output$SummaryNewbury <- renderPrint({
    pos1dist <- DW2017SN %>%
      filter(Year %in% input$splityearP1FRT, OldWindsorTime <= input$OldWindsorCutoffInput)%>%
      select(Year, NewburyTime)
     
    summary(na.omit(pos1dist))
  })
a5<-  output$SummaryReading <- renderPrint({
    pos1dist <- DW2017SN %>%
      filter(Year %in% input$splityearP1FRT, OldWindsorTime <= input$OldWindsorCutoffInput)%>%
      select(Year, ReadingTime)
    summary(na.omit(pos1dist))
  })
a6<-  output$SummaryMarlow <- renderPrint({
    pos1dist <- DW2017SN %>%
      filter(Year %in% input$splityearP1FRT, OldWindsorTime <= input$OldWindsorCutoffInput)%>%
      select(Year, MarlowTime) 
    summary(na.omit(pos1dist))
  })
a7<-  output$SummaryOldWindsor <- renderPrint({
    pos1dist <- DW2017SN %>%
      filter(Year %in% input$splityearP1FRT, OldWindsorTime <= input$OldWindsorCutoffInput)%>%
      select(Year, OldWindsorTime)
    summary(na.omit(pos1dist))
  })
a8<-  output$SummaryShepperton <- renderPrint({
    pos1dist <- DW2017SN %>%
      filter(Year %in% input$splityearP1FRT, OldWindsorTime <= input$OldWindsorCutoffInput)%>%
      select(Year, SheppertonTime) 
    summary(na.omit(pos1dist))
  })
a9<-  output$SummaryTeddington <- renderPrint({
    pos1dist <- DW2017SN %>%
      filter(Year %in% input$splityearP1FRT, OldWindsorTime <= input$OldWindsorCutoffInput)%>%
      select(Year, TeddingtonTime) 
    summary(na.omit(pos1dist))
  })
  
#ggarrange(a1, a2, a3, a4, a5, a6, a7, a8, a9,  ncol = 3, nrow = 1)
  
  
  
 # attnyear2SC
  output$SummaryJunCrewD <- renderPrint({
    pos1dist <- main_data %>%
      select(Year, Position, DecimalTime, Class) %>%
      filter(Year %in% input$attnyear2)%>%
  #  filter(Position != "0") %>%
      filter(Class == "Junior") 
   #   filter(str_detect(Name, input$paddler))
    summary(pos1dist[2:3])
  })
  output$SummarySenCrewD <- renderPrint({
    pos1dist <- main_data %>%
      select(Year, Position, DecimalTime, Class) %>%
      filter(Year %in% input$attnyear2)%>%
      #  filter(Position != "0") %>%
      filter(Class == "Senior") 
    #   filter(str_detect(Name, input$paddler))
    summary(pos1dist[2:3])
  })
  output$SummaryVJCrewD <- renderPrint({
    pos1dist <- main_data %>%
      select(Year, Position, DecimalTime, Class) %>%
      filter(Year %in% input$attnyear2)%>%
      #  filter(Position != "0") %>%
      filter(Class == "Vet/Junior") 
    #   filter(str_detect(Name, input$paddler))
    summary(pos1dist[2:3])
  })
  output$SummarySinCrewD <- renderPrint({
    pos1dist <- main_data %>%
      select(Year, Position, DecimalTime, Class) %>%
      filter(Year %in% input$attnyear2)%>%
      #  filter(Position != "0") %>%
      filter(Class == "Singles") 
    #   filter(str_detect(Name, input$paddler))
    summary(pos1dist[2:3])
  })
  output$SummaryEndD <- renderPrint({
    pos1dist <- main_data %>%
      select(Year, Position, DecimalTime, Class) %>%
      filter(Year %in% input$attnyear2)%>%
      #  filter(Position != "0") %>%
      filter(Class == "Endeavour") 
    #   filter(str_detect(Name, input$paddler))
    summary(pos1dist[2:3])
  })

  output$SubClassDistribution <- renderPlot({
    poschartdata <- main_data %>%
      select(Year, Class, DecimalTime, Military, BoatType, Ladies, Veteran) %>%
      filter(Year %in% input$attnyear2SC, Class %in% input$atttimeclass1SC)

    a <- ggplot(poschartdata, aes(x = Military, y = DecimalTime, fill = Military)) +
      geom_boxplot() +
      ggtitle("Plot 1 - Box Plot Military - Mean as Diamond")

    b <- ggplot(poschartdata, aes(x = BoatType, y = DecimalTime, fill = BoatType)) +
      geom_boxplot() +
      ggtitle("Plot 2 - Box Plot Boat Type - Mean as Diamond")

    c <- ggplot(poschartdata, aes(x = Ladies, y = DecimalTime, fill = Ladies)) +
      geom_boxplot() +
      ggtitle("Plot  - Box Plot Ladies - Mean as Diamond")

    d <- ggplot(poschartdata, aes(x = Veteran, y = DecimalTime, fill = Veteran)) +
      geom_boxplot() +
      ggtitle("Plot 4 - Box Plot Veteran - Mean as Diamond")
    ggarrange(a, b, c, d, ncol = 4, nrow = 1)
  })
  
  output$SummaryMilitaryA <- renderPrint({
    poschartdata <- main_data %>%
      select(Year, Class, DecimalTime, Military, BoatType, Ladies, Veteran) %>%
      filter(Year %in% input$attnyear2SC, Class %in% input$atttimeclass1SC)%>%
      filter(Military == "Army") 
    summary(poschartdata[3])
  })
  output$SummaryMilitaryN <- renderPrint({
    poschartdata <- main_data %>%
      select(Year, Class, DecimalTime, Military, BoatType, Ladies, Veteran) %>%
      filter(Year %in% input$attnyear2SC, Class %in% input$atttimeclass1SC)%>%
      filter(Military == "Navy") 
    summary(poschartdata[3])
  })
  output$SummaryMilitaryR <- renderPrint({
    poschartdata <- main_data %>%
      select(Year, Class, DecimalTime, Military, BoatType, Ladies, Veteran) %>%
      filter(Year %in% input$attnyear2SC, Class %in% input$atttimeclass1SC)%>%
      filter(Military == "RAF") 
    summary(poschartdata[3])
  })
  
  output$SummaryBoatTypeC <- renderPrint({
    poschartdata <- main_data %>%
      select(Year, Class, DecimalTime, Military, BoatType, Ladies, Veteran) %>%
      filter(Year %in% input$attnyear2SC, Class %in% input$atttimeclass1SC)%>%
      filter(BoatType == "Canadian") 
    summary(poschartdata[3])
  })
  output$SummaryBoatTypeF <- renderPrint({
    poschartdata <- main_data %>%
      select(Year, Class, DecimalTime, Military, BoatType, Ladies, Veteran) %>%
      filter(Year %in% input$attnyear2SC, Class %in% input$atttimeclass1SC)%>%
      filter(BoatType == "Folding Boat") 
    summary(poschartdata[3])
  })
  output$SummaryBoatTypeK <- renderPrint({
    poschartdata <- main_data %>%
      select(Year, Class, DecimalTime, Military, BoatType, Ladies, Veteran) %>%
      filter(Year %in% input$attnyear2SC, Class %in% input$atttimeclass1SC)%>%
      filter(BoatType == "Kayak") 
    summary(poschartdata[3])
  })
  output$SummaryLadiesL <- renderPrint({
    poschartdata <- main_data %>%
      select(Year, Class, DecimalTime, Military, BoatType, Ladies, Veteran) %>%
      filter(Year %in% input$attnyear2SC, Class %in% input$atttimeclass1SC)%>%
      filter(Ladies == "Ladies") 
    summary(poschartdata[3])
  })
  output$SummaryLadiesM <- renderPrint({
    poschartdata <- main_data %>%
      select(Year, Class, DecimalTime, Military, BoatType, Ladies, Veteran) %>%
      filter(Year %in% input$attnyear2SC, Class %in% input$atttimeclass1SC)%>%
      filter(Ladies == "Male") 
    summary(poschartdata[3])
  })
  output$SummaryLadiesMix <- renderPrint({
    poschartdata <- main_data %>%
      select(Year, Class, DecimalTime, Military, BoatType, Ladies, Veteran) %>%
      filter(Year %in% input$attnyear2SC, Class %in% input$atttimeclass1SC)%>%
      filter(Ladies == "Mixed") 
    summary(poschartdata[3])
  })
  
  output$SummaryVeteranC <- renderPrint({
    poschartdata <- main_data %>%
      select(Year, Class, DecimalTime, Military, BoatType, Ladies, Veteran) %>%
      filter(Year %in% input$attnyear2SC, Class %in% input$atttimeclass1SC)%>%
      filter(Veteran == "Century") 
    summary(poschartdata[3])
  })
  output$SummaryVeteranS <- renderPrint({
    poschartdata <- main_data %>%
      select(Year, Class, DecimalTime, Military, BoatType, Ladies, Veteran) %>%
      filter(Year %in% input$attnyear2SC, Class %in% input$atttimeclass1SC)%>%
      filter(Veteran == "Senior") 
    summary(poschartdata[3])
  })
  output$SummaryVeteranV <- renderPrint({
    poschartdata <- main_data %>%
      select(Year, Class, DecimalTime, Military, BoatType, Ladies, Veteran) %>%
      filter(Year %in% input$attnyear2SC, Class %in% input$atttimeclass1SC)%>%
      filter(Veteran == "Veteran") 
    summary(poschartdata[3])
  })


  output$WSDistribution <- renderPlot({
    poschartdata <- wsmain_data %>%
      select(Year, WSClass, WSRace, WSTime, WSDecimalTime) %>%
      filter(Year %in% input$attnwsyear2, WSRace %in% input$attDrace, WSClass %in% input$attWSclass1)


    b <- ggplot(poschartdata, aes(WSDecimalTime, fill = WSClass)) +
      geom_histogram(binwidth = .2) +
      ggtitle("Plot 1 - By Combined Class")

    c <- ggplot(poschartdata, aes(x = WSClass, y = WSDecimalTime, fill = WSClass)) +
      geom_boxplot() +
      ggtitle("Plot 2 - Box Plot Class - Mean as Diamond") +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 4) +
      geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5)
    ggarrange(b, c, ncol = 2, nrow = 1)
  })



 # output$clubplot <- renderPlot({
  #  attchartdata <- main_data %>%
   #   select(Year, Class, Club1, C1) %>%
    #  filter(Year %in% input$attclubyear, Class %in% input$attclubclass, C1 > 24 & C1 < 900)
#    ggplot(attchartdata, aes(x = reorder(Club1, -table(Club1)[Club1]))) +
 #     geom_bar(aes(fill = Class), stat = "count") +
  #    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
   #   labs(y = "Completions", x = "Largest Clubs shown by Number of Completions")
 # })
  ########################################################################################################################################  This is new
  # Test
  output$DWRN1 <- renderPlot({
    poschartdata <- RND_data %>%
      select(Year, Result, value) %>%
      #  filter(Result == "Doubles")
      filter(Result %in% input$ResultsTypeC)
    ggplot(poschartdata) +
      geom_col(aes(x = Year, y = value, fill = Result, colour = Result))
  })

  output$RNDtable <- DT::renderDataTable({
    DT::datatable(
      {
        data <- RND_datatable
      },
      options = list(pageLength = 60),
      rownames = FALSE
    )
  })


  output$RetirePlot <- renderPlot({
    poschartdata <- Retire_data %>%
      select(Year, Result, value) %>%
      filter(Year %in% input$RetireYearS)
    ggplot(poschartdata, aes(x = Result, y = value, fill = Year)) +
      geom_col() +
      scale_fill_gradientn(colours = rainbow(4))
  })

  # ggplot(data = Titanic, aes(x = Class, y = Freq, fill = Survived)) + geom_col()


  output$RetireTable <- DT::renderDataTable({
    DT::datatable(
      {
        data <- Retire_datatable
      },
      options = list(pageLength = 60),
      rownames = FALSE
    )
  })
  
  output$SexAgeT <- DT::renderDataTable({
    DT::datatable(
      {
        data <- ClassAge
      },
      options = list(pageLength = 60),
      rownames = FALSE
    )
  })
  
           output$SexAge1 <- renderPlot({
            poschartdata <- ClassAge %>%
             select(Year, Class, Age1, Age2, AgeB, Sex, Place, Time)%>%
              filter(Class %in% input$SAclass, Year %in% input$AgeYearS)
        d <- ggplot(poschartdata) +
         geom_point(aes(Age1, Place), color = "black", size = 3) +
         geom_point(aes(Age2, Place), color = "black", size = 3) +
          scale_x_continuous(breaks = seq(10, 100, 5))+labs(x = "Age")+
       ggtitle("Plot 1 - Age plotted against Position")
        e <- ggplot(poschartdata) +
        geom_point(aes(Age1, Time), color = "red", size = 3) +
          geom_point(aes(Age2, Time), color = "red", size = 3) +
          scale_x_continuous(breaks = seq(10, 100, 5))+labs(x = "Age")+
                ggtitle("Plot 2 - Age plotted against Time")
         ggarrange(d, e, ncol = 2, nrow = 1)
      })
           
output$SexAge2 <- renderPlot({
             poschartdata <- ClassAge %>%
               select(Year, Class, Age1, Age2, AgeB, Sex, Place, Time, BoatType)%>%
               filter(Class %in% input$SAclass, Year %in% input$AgeYearS)
             d <- ggplot(poschartdata) +
               geom_point(aes(Age1, Time, color = BoatType), size = 3) +
               geom_point(aes(Age2, Time, color = BoatType), size = 3) +
               scale_x_continuous(breaks = seq(10, 100, 5))+labs(x = "Age")+
               ggtitle("Plot 3 - Age plotted against Time")
             e <- ggplot(poschartdata) +
               geom_point(aes(AgeB, Time, color = Class), size = 3) +labs(x = "Total Age")+
               scale_x_continuous(breaks = seq(10, 200, 5))+
               ggtitle("Plot 4 - Total Age plotted against Time")
             ggarrange(d, e, ncol = 2, nrow = 1)
           })           
           
           
           
           
           
           
  output$Mattendance1N <- renderPlot({
    poschartdata <- main_data %>%
      select(Year, Class) %>%
      filter(Year > input$attyearTY, Class %in% input$att2class)
    ggplot(poschartdata, aes(Class)) +
      geom_bar(aes(fill = Class), stat = "count") +
      geom_text(stat = "count", aes(label = ..count..), vjust = -1)
  })
  #############################################################
  output$Mattendance1 <- renderPlot({
    poschartdata <- main_data %>%
      select(Year, Class) %>%
      filter(Year > input$attyearTY, Class %in% input$att2class)
    # filter(Year > input$attyearT,Class %in% input$attMclassT)
    ggplot(poschartdata, aes(Year)) +
      geom_bar(aes(fill = Class), stat = "count") +
      geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
      scale_x_continuous(breaks = seq(0, 2050, 5))
  })





  output$MattendanceL <- renderPlot({
    poschartdata <- main_data %>%
      select(Year, Ladies, Class) %>%
      filter(Ladies %in% input$attLclass, Class %in% input$attL1class)
    ggplot(poschartdata, aes(Year)) +
      geom_bar(aes(fill = Ladies), stat = "count") +
      geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
      scale_x_continuous(breaks = seq(0, 2050, 5))
  })

  output$MattendanceLT <- renderPlot({
    poschartdata <- main_data %>%
      select(Year, Ladies, Class) %>%
      filter(Year > input$attyearT, Class %in% input$attMclassT)
    ggplot(poschartdata, aes(Ladies)) +
      geom_bar(aes(fill = Ladies), stat = "count") +
      geom_text(stat = "count", aes(label = ..count..), vjust = -1)
  })

  output$MattendanceLS <- renderPlot({
    poschartdata <- main_data %>%
      select(Year, Ladies, Class, Position) %>%
      filter(Ladies %in% input$attLclass, Class %in% input$attL1class)
    #  ggplot(poschartdata, aes(Year)) +geom_bar(aes(fill = Ladies), stat = "count")+ scale_x_continuous(breaks=seq(0,2050,5))
    ggplot(poschartdata) +
      geom_point(aes(Year, Position, color = Ladies), position = position_jitter(width = 0.1, height = 0.1), size = 2) +
      scale_y_continuous(breaks = seq(2, 200, 10)) +
      scale_x_continuous(breaks = seq(0, 2050, 5)) +
      scale_colour_manual(values = c("red", "green", "blue"))
  })

  output$MattendanceM <- renderPlot({
    poschartdata <- main_data %>%
      select(Year, Military, Class) %>%
      filter(Military %in% input$attMArm, Class %in% input$attMclass)
    ggplot(poschartdata, aes(Year)) +
      geom_bar(aes(fill = Military), stat = "count") +
      geom_text(stat = "count", aes(label = ..count..), vjust = -1) +
      scale_x_continuous(breaks = seq(0, 2050, 5))
  })



  output$MattendanceMP <- renderPlot({
    poschartdata <- main_data %>%
      select(Year, Military, Class, Position) %>%
      filter(Military %in% input$attMArm, Class %in% input$attMclass)
    # ggplot(poschartdata, aes(Year)) +geom_bar(aes(fill = Military), stat = "count")+ scale_x_continuous(breaks=seq(0,2050,5))
    ggplot(poschartdata) +
      geom_point(aes(Year, Position, color = Military), position = position_jitter(width = 0.1, height = 0.1), size = 2) +
      scale_y_continuous(breaks = seq(2, 200, 10)) +
      scale_x_continuous(breaks = seq(0, 2050, 5))
  })


  output$MattendanceMPY <- renderPlot({
    poschartdata <- main_data %>%
      select(Year, Military, Class, Position, DecimalTime) %>%
      filter(Year > input$attyearT, Class %in% input$att2class)
    # ggplot(poschartdata, aes(Year)) +geom_bar(aes(fill = Military), stat = "count")+ scale_x_continuous(breaks=seq(0,2050,5))
    ggplot(poschartdata) +
      geom_point(aes(Year, DecimalTime, color = Class), position = position_jitter(width = 0.1, height = 0.1), size = 2) +
      scale_y_continuous(breaks = seq(2, 200, 10)) +
      scale_x_continuous(breaks = seq(0, 2050, 5))
  })



  output$MattendanceSL <- renderPlot({
    poschartdata <- main_data %>%
      select(Year, Ladies, Class, SubClass) %>%
      filter(SubClass %in% input$attSubclass, Class %in% input$attL2class)
    ggplot(poschartdata, aes(Year)) +
      geom_histogram(aes(fill = SubClass), stat = "count", width = 0.5) +
      geom_text(stat = "count", aes(label = ..count..), vjust = -1)
  })




  output$MattendanceSLO <- renderPlot({
    poschartdata <- main_data %>%
      select(Year, Veteran, Ladies, BoatType, Class, Position) %>%
      filter(Class %in% input$attL2classCL, Ladies %in% input$attSubclassSEX, BoatType %in% input$attSubclassBOAT)
    ggplot(poschartdata, aes(Year)) +
      geom_histogram(aes(fill = Veteran), stat = "count", width = 0.5) +
      geom_text(stat = "count", aes(label = ..count..), vjust = -1)
  })



  output$MattendanceSLT <- renderPlot({
    poschartdata <- main_data %>%
      select(Year, Veteran, BoatType, Ladies, Class, SubClass) %>%
      filter(SubClass %in% input$attSubclass, Class %in% input$attL2class)
    ggplot(poschartdata, aes(SubClass)) +
      geom_bar(aes(fill = SubClass), stat = "count") +
      geom_text(stat = "count", aes(label = ..count..), vjust = -1)
  })

  # Totals
  # Military
  output$MattendanceMT <- renderPlot({
    poschartdata <- main_data %>%
      select(Year, Military, Class, Ladies, SubClass, BoatType, Veteran) %>%
      filter(Year > input$attyearT, Class %in% input$attMclassT, Military != "Civilian")
    ggplot(poschartdata, aes(Military)) +
      geom_bar(aes(fill = Military), stat = "count") +
      geom_text(stat = "count", aes(label = ..count..), vjust = -1)
  })

  # Ladies
  output$MattendanceLT <- renderPlot({
    poschartdata <- main_data %>%
      select(Year, Military, Class, Ladies, SubClass, BoatType, Veteran) %>%
      filter(Year > input$attyearT, Class %in% input$attMclassT)
    a <- ggplot(poschartdata, aes(Ladies)) +
      geom_bar(aes(fill = Ladies), stat = "count") +
      geom_text(stat = "count", aes(label = ..count..), vjust = -1)
    # })


    # BoatType
    #  output$MattendanceSLOPT <- renderPlot({poschartdata <- main_data %>%
    #   select(Year,Military,Class,Ladies,SubClass,BoatType,Veteran)%>%
    #  filter(Year > input$attyearT, Class %in% input$attMclassT)
    b <- ggplot(poschartdata, aes(BoatType)) +
      geom_bar(aes(fill = BoatType), stat = "count") +
      geom_text(stat = "count", aes(label = ..count..), vjust = -1)
    #  })

    # Veteran
    #  output$MattendanceSLOT <- renderPlot({poschartdata <- main_data %>%
    #   select(Year,Military,Class,Ladies,SubClass,BoatType,Veteran)%>%
    #  filter(Year > input$attyearT,Class %in% input$attMclassT)
    c <- ggplot(poschartdata, aes(Veteran)) +
      geom_histogram(aes(fill = Veteran), stat = "count", width = 0.5) +
      geom_text(stat = "count", aes(label = ..count..), vjust = -1)
    ggarrange(a, b, c, ncol = 3, nrow = 1)
  })

  # SubClass
  output$MattendanceSLToT <- renderPlot({
    poschartdata <- main_data %>%
      select(Year, Military, Class, Ladies, SubClass, BoatType, Veteran) %>%
      filter(Year > input$attyearT, Class %in% input$attMclassT, SubClass != "")
    ggplot(poschartdata, aes(SubClass)) +
      geom_histogram(aes(fill = SubClass), stat = "count", width = 0.5) +
      geom_text(stat = "count", aes(label = ..count..), vjust = -1)

    #  ggarrange(a,b,c,ncol = 3, nrow = 1)
  })



  output$MattendanceSLOP <- renderPlot({
    poschartdata <- main_data %>%
      select(Year, Veteran, Ladies, BoatType, Class, Position) %>%
      filter(Class %in% input$attL2classCL, Ladies %in% input$attSubclassSEX, BoatType %in% input$attSubclassBOAT)
    # ggplot(poschartdata, aes(Year)) +geom_histogram(aes(fill = Veteran), stat = "count", width = 0.5)+ scale_x_continuous(breaks=seq(0,2050,5))
    ggplot(poschartdata) +
      geom_point(aes(Year, Position, color = Veteran), position = position_jitter(width = 0.1, height = 0.1), size = 2) +
      scale_y_continuous(breaks = seq(2, 200, 10)) +
      scale_x_continuous(breaks = seq(0, 2050, 5)) +
      scale_colour_manual(values = c("red", "green", "blue", "darkgreen", "orange", "lightblue", "yellow", "magenta"))
  })

  output$Mattendance1BT <- renderPlot({
    poschartdata <- main_data %>%
      select(Year, Class, BoatType) %>%
      filter(BoatType %in% input$att2boat)
    #  filter(Year > input$attyearT,Class %in% input$attMclassT)
    ggplot(poschartdata, aes(Year)) +
      geom_bar(aes(fill = Class), stat = "count") +
      geom_text(stat = "count", aes(label = ..count..), vjust = -1)
  })




  output$Wtable <- DT::renderDataTable({
    DT::datatable(
      {
        data <- WMain_Data
      },
      options = list(pageLength = 60),
      rownames = FALSE
    )
  })


  output$Etable <- DT::renderDataTable({
    DT::datatable(
      {
        data <- WMMain_Data %>%
          filter(Year == input$comattDWyear)
      },
      options = list(pageLength = 1, dom = "t"),
      rownames = FALSE,
      colnames = c("Year", "Flow", "Wind", "Temp", "Completion")
    )
  })







  output$FGraphsR <- renderPlot({
    poschartdata <- WMain_Data %>%
      select(Year, SeniorFlow, FirstSenior, MaxWind, MinTemperature, CompletionRate, Regression_D) %>%
      filter(CompletionRate >= input$comrateR, MaxWind <= input$windgR, MinTemperature >= input$tempgR)

    poschartdata$RegressionU <- poschartdata$Regression * as.numeric(input$WSDT)

    lm_eqn <- function(SeniorFlow, Regression_D, poschartdata) {
      m <- lm(Regression_D ~ SeniorFlow, poschartdata)
      eq <- substitute(
        italic(Regression_D) == a + italic(SeniorFlow) %*% b * "," ~ ~ italic(r)^2 ~ "=" ~ r2,
        list(
          a = format(unname(coef(m))[1], digits = 3),
          b = format(unname(coef(m))[2], digits = 3),
          r2 = format(summary(m)$r.squared, digits = 3)
        )
      )
      as.character(as.expression(eq))
    }



    ggplot(poschartdata, aes(x = SeniorFlow, y = RegressionU)) +
      geom_point(color = "Red", size = 3) +
      stat_smooth(method = "lm") +
      geom_text(x = 80, y = 19, label = lm_eqn(SeniorFlow, Regression_D, poschartdata), color = "black", size = 4, parse = TRUE) +
      geom_text(x = 80, y = 23, label = lm_eqn(SeniorFlow, Regression_D, poschartdata), color = "black", size = 4, parse = TRUE) +
      geom_text(x = 80, y = 16, label = lm_eqn(SeniorFlow, Regression_D, poschartdata), color = "black", size = 4, parse = TRUE) +
      geom_text(x = 80, y = 27, label = lm_eqn(SeniorFlow, Regression_D, poschartdata), color = "black", size = 4, parse = TRUE)
  })


  output$FGraphs <- renderPlot({
    poschartdata <- WMain_Data %>%
      select(Year, SeniorFlow, FirstSenior, MaxWind, MinTemperature, CompletionRate) %>%
      filter(Year >= input$attnfyear, CompletionRate >= input$comrate, MaxWind <= input$windg, MinTemperature >= input$tempg)

    lm_eqn <- function(SeniorFlow, FirstSenior, poschartdata) {
      m <- lm(FirstSenior ~ SeniorFlow, poschartdata)
      eq <- substitute(
        italic(FirstSenior) == a + italic(SeniorFlow) %*% b * "," ~ ~ italic(r)^2 ~ "=" ~ r2,
        list(
          a = format(unname(coef(m))[1], digits = 3),
          b = format(unname(coef(m))[2], digits = 3),
          r2 = format(summary(m)$r.squared, digits = 3)
        )
      )
      as.character(as.expression(eq))
    }



    d <- ggplot(poschartdata, aes(x = SeniorFlow, y = FirstSenior)) +
      geom_point(color = "Red") +
      stat_smooth(method = "lm") +
      geom_text(x = 80, y = 15.5, label = lm_eqn(SeniorFlow, FirstSenior, poschartdata), color = "black", size = 4, parse = TRUE)


    # p1 <- d + geom_text(x = 15, y = 300, label = lm_eqn(df), parse = TRUE)

    z <- ggplot(poschartdata) +
      geom_point(aes(SeniorFlow, FirstSenior, color = MaxWind), size = 3) +
      scale_colour_gradientn(colours = rainbow(4))


    y <- ggplot(poschartdata) +
      geom_point(aes(SeniorFlow, FirstSenior, color = MinTemperature), size = 3) +
      scale_colour_gradientn(colours = rainbow(4))


    ggarrange(d, z, y, ncol = 3, nrow = 1)
  })



  output$Graphs <- renderPlot({
    poschartdata <- WMain_Data %>%
      select(Year, SeniorFlow, FirstSenior, MaxWind, MinTemperature, CompletionRate)%>%filter(Year > 1975)

    a <- ggplot(poschartdata) +
      geom_point(aes(FirstSenior, SeniorFlow, color = MaxWind), size = 3) +
      scale_colour_gradientn(colours = rainbow(4)) +
      ggtitle("Plot 1 - Flow is most important for Time")
    b <- ggplot(poschartdata) +
      geom_point(aes(FirstSenior, MaxWind, color = SeniorFlow), size = 3) +
      scale_colour_gradientn(colours = rainbow(4)) +
      ggtitle("Plot 2 - Wind direction can be critical")
    c <- ggplot(poschartdata) +
      geom_point(aes(FirstSenior, MinTemperature, color = MaxWind), size = 3) +
      scale_colour_gradientn(colours = rainbow(4)) +
      ggtitle("Plot 3 - Times not related to Min Temperature")

    d <- ggplot(poschartdata) +
      geom_point(aes(CompletionRate, SeniorFlow, color = MaxWind), size = 3) +
      scale_colour_gradientn(colours = rainbow(4)) +
      ggtitle("Plot 4 - Completion Rate not related to Flow")
    e <- ggplot(poschartdata) +
      geom_point(aes(CompletionRate, MaxWind, color = MinTemperature), size = 3) +
      scale_colour_gradientn(colours = rainbow(4)) +
      ggtitle("Plot 5 - Completion related to Max head Wind")
    f <- ggplot(poschartdata) +
      geom_point(aes(CompletionRate, MinTemperature, color = MaxWind), size = 3) +
      scale_colour_gradientn(colours = rainbow(4)) +
      ggtitle("Plot 6 - Completion sometimes related to Min Temp")
    g <- ggplot(poschartdata) +
      geom_point(aes(MaxWind, MinTemperature, color = Year), size = 3) +
      scale_colour_gradientn(colours = rainbow(4)) +
      ggtitle("Plot 7 - High temperatures related to low wind ??")


    ggarrange(a, b, c, ncol = 3, nrow = 1)
  })

  output$GraphsTP <- renderPlot({
    poschartdata <- main_data %>%
      select(Year, DecimalTime, Position, Class) %>%
      filter(Class %in% input$attclassTP, Year %in% input$attyearTP, Position > 0)
    #    select(Year, DecimalTime, Position,Class) %>% filter(Class %in% input$attclassTP, Year %in% input$attyearTP, Position > 0)
    ggplot(poschartdata) +
      geom_point(aes(x = Position, y = DecimalTime, color = Year), size = 3) +
      scale_colour_gradientn(colours = rainbow(4)) +
      scale_x_continuous(breaks = seq(0, 200, 10)) +
      ggtitle("Showing all Places")
  })

  output$GraphsTPL <- renderPlot({
    poschartdata <- main_data %>%
      select(Year, DecimalTime, Position, Class) %>%
      filter(Class %in% input$attclassTP, Year %in% input$attyearTP, Position > 0, Position < 51)
    #    select(Year, DecimalTime, Position,Class) %>% filter(Class %in% input$attclassTP, Year %in% input$attyearTP, Position > 0)
    ggplot(poschartdata) +
      geom_point(aes(x = Position, y = DecimalTime, color = Year), size = 3) +
      scale_colour_gradientn(colours = rainbow(4)) +
      scale_y_continuous(breaks = seq(0, 50, 2)) +
      scale_x_continuous(breaks = seq(0, 200, 10)) +
      ggtitle("Showing 1st 50 Places")
  })
  # Year >= input$attnyear


  ## New Code EasterOffset

  output$GraphsEasterO <- renderPlot({
    poschartdata <- WMain_Data %>%
      select(Year, SeniorFlow, FirstSenior, MaxWind, MinTemperature, CompletionRate, EasterOffset) %>%
      filter(Year > 1970)
    ggplot(poschartdata) +
      geom_point(aes(x = MinTemperature, y = CompletionRate, color = MinTemperature), size = 2)
    a <- ggplot(poschartdata) +
      geom_point(aes(EasterOffset, MinTemperature, color = FirstSenior), size = 3) +
      scale_colour_gradientn(colours = rainbow(4))
    b <- ggplot(poschartdata) +
      geom_point(aes(EasterOffset, MaxWind, color = FirstSenior), size = 3) +
      scale_colour_gradientn(colours = rainbow(4))
    c <- ggplot(poschartdata) +
      geom_point(aes(EasterOffset, SeniorFlow, color = FirstSenior), size = 3) +
      scale_colour_gradientn(colours = rainbow(4))
    d <- ggplot(poschartdata) +
      geom_point(aes(EasterOffset, CompletionRate, color = FirstSenior), size = 3) +
      scale_colour_gradientn(colours = rainbow(4))
    e <- ggplot(poschartdata) +
      geom_point(aes(EasterOffset, FirstSenior, color = FirstSenior), size = 3) +
      scale_colour_gradientn(colours = rainbow(4))
    f <- ggplot(poschartdata) +
      geom_point(aes(EasterOffset, Year, color = FirstSenior), size = 3) +
      scale_colour_gradientn(colours = rainbow(4))
    ggarrange(a, b, c, d, e, f, ncol = 3, nrow = 2)
  })







  #  z <-  ggplot(poschartdata)+
  #   geom_point(aes(SeniorFlow,FirstSenior, color = MaxWind), size = 3)+scale_colour_gradientn(colours=rainbow(4))


  #  y <-  ggplot(poschartdata)+
  #   geom_point(aes(SeniorFlow,FirstSenior, color = MinTemperature), size = 3)+scale_colour_gradientn(colours=rainbow(4))


  # ggarrange(d,z,y,ncol = 3, nrow = 1)})












  #  ggplot(poschartdata)+geom_point(aes(x = Position, y = DecimalTime, color = Year), size = 2)+scale_colour_gradientn(colours=rainbow(4)) +  scale_x_continuous(breaks=seq(0,200,10))+ ggtitle("Showing all Places")
  # })







  output$WeatherBox <- renderPlot({
    poschartdata <- WMain_Data %>% select(Year, SeniorFlow, MaxWind, MinTemperature, CompletionRate)

    a <- ggplot(poschartdata, aes(x = 0, y = SeniorFlow, fill = Year)) +
      geom_boxplot() +
      ggtitle("Plot 1 - Box Plot Senior Flow - Mean as Diamond") +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 4) +
      geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5)

    b <- ggplot(poschartdata, aes(x = 0, y = MaxWind, fill = Year)) +
      geom_boxplot() +
      ggtitle("Plot 2 - Box Plot Max Wind - Mean as Diamond") +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 4) +
      geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5)

    c <- ggplot(poschartdata, aes(x = 0, y = MinTemperature, fill = Year)) +
      geom_boxplot() +
      ggtitle("Plot 3 - Box Plot Min Temperature - Mean as Diamond") +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 4) +
      geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5)

    d <- ggplot(poschartdata, aes(x = 0, y = CompletionRate, fill = Year)) +
      geom_boxplot() +
      ggtitle("Plot 4 - Box Plot Completion Rate - Mean as Diamond") +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 4) +
      geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5)

    ggarrange(a, b, c, d, ncol = 4, nrow = 1)
  })

  output$WCompGraphs <- renderPlot({
    poschartdata <- datapipeABCDWS %>%
      select(Year, WatersideA, WatersideB, WatersideC, WatersideD) %>%
      filter(Year == input$comattyear, WatersideA <= input$MaxA, WatersideA > 0, WatersideB > 0,WatersideC > 0, WatersideD > 0) 
    na.test <- function(WatersideB) {
      if (colSums(!is.na(WatersideB) == 0)) {
        stop("The some variable in the dataset has all missing value, remove the column to proceed")
      }
    }


    lm_eqn <- function(WatersideA, WatersideB, poschartdata) {
      m <- lm(WatersideB ~ 0 + WatersideA, poschartdata)
      eq <- substitute(
        italic(WatersideB) == italic(WatersideA) %*% a,
        list(
          a = format(unname(coef(m))[1], digits = 3),
          b = format(unname(coef(m))[2], digits = 3),
          r2 = format(summary(m)$r.squared, digits = 3)
        )
      )
      as.character(as.expression(eq))
    }

    b <- ggplot(poschartdata, aes(x = WatersideA, y = WatersideB)) +
      geom_point(color = "red") +
      stat_smooth(method = "lm") +
      annotate("text", x = 2.0, y = 4.0, label = lm_eqn(poschartdata$watersideA, poschartdata$WatersideB, poschartdata), color = "black", size = 4, parse = TRUE)

  #  na.test <- function(WatersideC) {
   #   if (colSums(!is.na(WatersideC) == 0)) {
    #    stop("The some variable in the dataset has all missing value, remove the column to proceed")
     # }
#    }
    # na.test (WatersideC)


    lm_eqn <- function(WatersideA, WatersideC, poschartdata) {
      m <- lm(WatersideC ~ 0 + WatersideA, poschartdata)
      eq <- substitute(
        italic(WatersideC) == italic(WatersideA) %*% a,
        list(
          a = format(unname(coef(m))[1], digits = 3),
          b = format(unname(coef(m))[2], digits = 3),
          r2 = format(summary(m)$r.squared, digits = 3)
        )
      )
      as.character(as.expression(eq))
    }

    c <- ggplot(poschartdata, aes(x = WatersideA, y = WatersideC)) +
      geom_point(color = "red") +
      stat_smooth(method = "lm") +
      annotate("text", x = 2.0, y = 4.5, label = lm_eqn(poschartdata$watersideA, poschartdata$WatersideC, poschartdata), color = "black", size = 4, parse = TRUE)
      geom_text(label = lm_eqn(poschartdata$watersideA, poschartdata$WatersideC, poschartdata), color="black", size = 5, parse=TRUE)

 #   na.test <- function(WatersideD) {
  #    if (colSums(!is.na(WatersideD) == 0)) {
   #     stop("The some variable in the dataset has all missing value, remove the column to proceed")
    #  }
  #  }
    # na.test (WatersideD)


    lm_eqn <- function(WatersideA, WatersideD, poschartdata) {
      m <- lm(WatersideD ~ 0 + WatersideA, poschartdata)
      eq <- substitute(
        italic(WatersideD) == italic(WatersideA) %*% a,
        list(
          a = format(unname(coef(m))[1], digits = 3),
          b = format(unname(coef(m))[2], digits = 3),
          r2 = format(summary(m)$r.squared, digits = 3)
        )
      )
      as.character(as.expression(eq))
    }

    d <- ggplot(poschartdata, aes(x = WatersideA, y = WatersideD)) +
      geom_point(color = "red") +
      stat_smooth(method = "lm") +
      annotate("text", x = 2.0, y = 7.0, label = lm_eqn(poschartdata$watersideA, poschartdata$WatersideD, poschartdata), color = "black", size = 4, parse = TRUE)

    a <- ggplot(poschartdata) +
      geom_point(aes(x = WatersideA, y = WatersideB, color = "WatersideB"), size = 2) +
      geom_point(aes(x = WatersideA, y = WatersideC, color = "WatersideC"), size = 2) +
      geom_point(aes(x = WatersideA, y = WatersideD, color = "WatersideD"), size = 2)+ labs(y = "Waterside Races")

     ggarrange(a, b, c, d, ncol = 4, nrow = 1)
  })



  #################################################################


  output$CompGraphs <- renderPlot({
    poschartdata <- datapipeABCDcom %>%
      select(Year, WatersideA, WatersideB, WatersideC, WatersideD, DWSenior, DWJunior, DWEndeavour, DWVetJunior, DWSingles) %>%
      filter(Year == input$comattDWyear, WatersideA > 0, WatersideB > 0,WatersideC > 0, WatersideD > 0)

    b <- ggplot(poschartdata) +
      geom_point(aes(WatersideD, DWSenior, color = "DWSenior"), size = 2) +
      geom_point(aes(WatersideD, DWJunior, color = "DWJunior"), size = 2) +
      geom_point(aes(WatersideD, DWEndeavour, color = "DWEndeavour"), size = 2) +
      geom_point(aes(WatersideD, DWVetJunior, color = "DWVetJunior"), size = 2) +
      geom_point(aes(WatersideD, DWSingles, color = "DWSingles"), size = 2)

    c <- ggplot(poschartdata, aes(x = WatersideA, y = DWSenior)) +
      geom_point(aes(x = WatersideA, y = DWSenior)) +
      stat_smooth(method = "lm", col = "red")

    lm_eqn <- function(WatersideA, DWSenior, poschartdata) {
      m <- lm(DWSenior ~ 0 + WatersideA, poschartdata)
      eq <- substitute(
        italic(DWSenior) == italic(WatersideA) %*% a,
        list(
          a = format(unname(coef(m))[1], digits = 3),
          b = format(unname(coef(m))[2], digits = 3)
        )
      )
      as.character(as.expression(eq))
    }


    d <- ggplot(poschartdata, aes(x = WatersideA, y = DWSenior)) +
      geom_point(color = "red") +
      stat_smooth(method = "lm") +
      annotate("text", x = 2.7, y = 35, label = lm_eqn(poschartdata$watersideA, poschartdata$DWSenior, poschartdata), color = "black", size = 4, parse = TRUE)

    lm_eqn <- function(WatersideD, DWSenior, poschartdata) {
      m <- lm(DWSenior ~ 0 + WatersideD, poschartdata)
      eq <- substitute(
        italic(DWSenior) == italic(WatersideD) %*% a,
        list(
          a = format(unname(coef(m))[1], digits = 3),
          b = format(unname(coef(m))[2], digits = 3)
        )
      )
      as.character(as.expression(eq))
    }

    e <- ggplot(poschartdata, aes(x = WatersideD, y = DWSenior)) +
      geom_point(color = "red") +
      geom_smooth(method = "lm") +
      annotate("text", x = 7, y = 35, label = lm_eqn(poschartdata$DWSenior, poschartdata$watersideB, poschartdata), color = "black", size = 4, parse = TRUE)


    lm_eqn <- function(WatersideB, DWSenior, poschartdata) {
      m <- lm(DWSenior ~ 0 + WatersideB, poschartdata)
      eq <- substitute(
        italic(DWSenior) == italic(WatersideB) %*% a,
        list(
          a = format(unname(coef(m))[1], digits = 3),
          b = format(unname(coef(m))[2], digits = 3)
        )
      )
      as.character(as.expression(eq))
    }

    g <- ggplot(poschartdata, aes(x = WatersideB, y = DWSenior)) +
      geom_point(color = "red") +
      geom_smooth(method = "lm") +
      annotate("text", x = 3, y = 35, label = lm_eqn(poschartdata$DWSenior, poschartdata$watersideB, poschartdata), color = "black", size = 4, parse = TRUE)


    lm_eqn <- function(WatersideC, DWSenior, poschartdata) {
      m <- lm(DWSenior ~ 0 + WatersideC, poschartdata)
      eq <- substitute(
        italic(DWSenior) == italic(WatersideC) %*% a,
        list(
          a = format(unname(coef(m))[1], digits = 3),
          b = format(unname(coef(m))[2], digits = 3)
        )
      )
      as.character(as.expression(eq))
    }

    h <- ggplot(poschartdata, aes(x = WatersideC, y = DWSenior)) +
      geom_point(color = "red") +
      geom_smooth(method = "lm") +
      annotate("text", x = 4, y = 35, label = lm_eqn(poschartdata$DWSenior, poschartdata$watersideC, poschartdata), color = "black", size = 4, parse = TRUE)



    lm_eqn <- function(WatersideD, DWJunior, poschartdata) {
      m <- lm(DWJunior ~ 0 + WatersideD, poschartdata)
      eq <- substitute(
        italic(DWJunior) == italic(WatersideD) %*% a,
        list(
          a = format(unname(coef(m))[1], digits = 3),
          b = format(unname(coef(m))[2], digits = 3)
        )
      )
      as.character(as.expression(eq))
    }



    f <- ggplot(poschartdata, aes(x = WatersideD, y = DWJunior)) +
      geom_point(color = "red") +
      geom_smooth(method = "lm") +
      annotate("text", x = 7, y = 32, label = lm_eqn(poschartdata$DWJunior, poschartdata$watersideB, poschartdata), color = "black", size = 4, parse = TRUE)




    ggarrange(d, g, h, e, b, f, ncol = 2, nrow = 3)
  })


  output$WSPositions <- renderPlot({
    poschartdata <- wsmain_data %>%
      select(Year, WSPosition, WSClass, WSName2, WSTime, WSDecimalTime, WSRace) %>%
      filter(str_detect(WSName2, input$WSpaddler), WSRace %in% input$WSPRace)
    a <- ggplot(poschartdata) +
      geom_point(aes(Year, WSPosition, color = WSClass), position = position_jitter(width = 0.1, height = 0.1), size = 3) +
      scale_y_continuous(breaks = seq(2, 200, 2)) +
      scale_x_continuous(breaks = seq(0, 2050, 5))
    b <- ggplot(poschartdata) +
      geom_point(aes(Year, WSDecimalTime, color = WSClass), position = position_jitter(width = 0.1, height = 0.1), size = 3) +
      scale_y_continuous(breaks = seq(1, 200, 0.2)) +
      scale_x_continuous(breaks = seq(0, 2050, 5))
    ggarrange(a, b, ncol = 2, nrow = 1)
  })

  output$WSTimePlacePositions1 <- renderPlot({
    poschartdata <- wsmain_data %>%
      select(Year, WSPosition, WSClass, WSName2, WSDecimalTime, WSRace) %>%
      filter(str_detect(WSName2, input$WSpaddler)) %>%
      filter(WSRace != "S")
    ggplot(poschartdata, aes(x = WSRace, y = WSDecimalTime, fill = WSClass)) +
      geom_boxplot() +
      ggtitle("Plot 3 - Box Plot Time")
  })













  #
  output$SummaryWSACrew <- renderPrint({
    pos1Crew <- wsmain_data %>%
      select(WSName2, WSRace, WSDecimalTime, WSClass) %>%
      filter(WSRace == "A") %>%
      filter(str_detect(WSName2, input$WSpaddler))
    summary(pos1Crew[3])
  })


  output$SummaryWSBCrew <- renderPrint({
    pos1Crew <- wsmain_data %>%
      select(WSName2, WSRace, WSDecimalTime, WSClass) %>%
      filter(WSRace == "B") %>%
      filter(str_detect(WSName2, input$WSpaddler))
    summary(pos1Crew[3])
  })

  output$SummaryWSCCrew <- renderPrint({
    pos1Crew <- wsmain_data %>%
      select(WSName2, WSRace, WSDecimalTime, WSClass) %>%
      filter(WSRace == "C") %>%
      filter(str_detect(WSName2, input$WSpaddler))
    summary(pos1Crew[3])
  })

  output$SummaryWSDCrew <- renderPrint({
    pos1Crew <- wsmain_data %>%
      select(WSName2, WSRace, WSDecimalTime, WSClass) %>%
      filter(WSRace == "D") %>%
      filter(str_detect(WSName2, input$WSpaddler))
    summary(pos1Crew[3])
  })



  # new

  output$WSclubpositionsN <- renderPlot({
    poschartdata <- wsmain_data %>%
      select(Year, WSPosition, WSClass, WSName2, WSTime, WSDecimalTime, WSRace, WSClub) %>%
      filter(str_detect(WSClub, input$WSregclub), WSRace %in% input$WSPRace)
    a <- ggplot(poschartdata) +
      geom_point(aes(Year, WSPosition, color = WSClass), position = position_jitter(width = 0.1, height = 0.1), size = 3) +
      scale_y_continuous(breaks = seq(2, 200, 2)) +
      scale_x_continuous(breaks = seq(0, 2050, 5))
    b <- ggplot(poschartdata) +
      geom_point(aes(Year, WSDecimalTime, color = WSClass), position = position_jitter(width = 0.1, height = 0.1), size = 3) +
      scale_y_continuous(breaks = seq(1, 200, 0.2)) +
      scale_x_continuous(breaks = seq(0, 2050, 5))
    ggarrange(a, b, ncol = 2, nrow = 1)
  })

  output$WSclubpositionsN1 <- renderPlot({
    poschartdata <- wsmain_data %>%
      select(Year, WSPosition, WSClass, WSName2, WSDecimalTime, WSRace, WSClub) %>%
      filter(str_detect(WSClub, input$WSregclub)) %>%
      filter(WSRace != "S")
    ggplot(poschartdata, aes(x = WSRace, y = WSDecimalTime, fill = WSClass)) +
      geom_boxplot() +
      ggtitle("Plot 3 - Box Plot Time")
  })


  output$SummaryWSAClub <- renderPrint({
    pos1Crew <- wsmain_data %>%
      select(WSName2, WSRace, WSDecimalTime, WSClass, WSClub) %>%
      filter(WSRace == "A") %>%
      filter(str_detect(WSClub, input$WSregclub))
    summary(pos1Crew[3])
  })


  output$SummaryWSBClub <- renderPrint({
    pos1Crew <- wsmain_data %>%
      select(WSName2, WSRace, WSDecimalTime, WSClass, WSClub) %>%
      filter(WSRace == "B") %>%
      filter(str_detect(WSClub, input$WSregclub))
    summary(pos1Crew[3])
  })

  output$SummaryWSCClub <- renderPrint({
    pos1Crew <- wsmain_data %>%
      select(WSName2, WSRace, WSDecimalTime, WSClass, WSClub) %>%
      filter(WSRace == "C") %>%
      filter(str_detect(WSClub, input$WSregclub))
    summary(pos1Crew[3])
  })

  output$SummaryWSDClub <- renderPrint({
    pos1Crew <- wsmain_data %>%
      select(WSName2, WSRace, WSDecimalTime, WSClass, WSClub) %>%
      filter(WSRace == "D") %>%
      filter(str_detect(WSClub, input$WSregclub))
    summary(pos1Crew[3])
  })





  output$CompletionR <- renderPlot({
    poschartdata <- WMain_Data %>%
      select(Year, SeniorFlow, FirstSenior, MaxWind, MinTemperature, CompletionRate)



    ggplot(poschartdata, aes(Year, CompletionRate, fill = CompletionRate)) +
      geom_bar(stat = "identity") +
      scale_fill_gradientn(colours = rainbow(4))
  })

  output$CompletionR1 <- renderPlot({
    poschartdata <- WMain_Data %>%
      select(Year, SeniorFlow, FirstSenior, MaxWind, MinTemperature, CompletionRate)


    d <- ggplot(poschartdata) +
      geom_point(aes(CompletionRate, SeniorFlow, color = MaxWind), size = 3) +
      scale_colour_gradientn(colours = rainbow(4)) +
      ggtitle("Plot 4 - Completion Rate not related to Flow")
    e <- ggplot(poschartdata) +
      geom_point(aes(CompletionRate, MaxWind, color = MinTemperature), size = 3) +
      scale_colour_gradientn(colours = rainbow(4)) +
      ggtitle("Plot 5 - Completion related to Max head Wind")
    f <- ggplot(poschartdata) +
      geom_point(aes(CompletionRate, MinTemperature, color = MaxWind), size = 3) +
      scale_colour_gradientn(colours = rainbow(4)) +
      ggtitle("Plot 6 - Completion sometimes related to Min Temp")
    ggarrange(d, e, f, ncol = 3, nrow = 1)
  })
}

shinyApp(ui, server)


#output$SummarySenCrew <- renderPrint({
 # pos1Crew <- main_data %>%
  #  select(Name, Position, DecimalTime, Class) %>%
   # filter(Position != "0") %>%
#    filter(Class == "Senior") %>%
 #   filter(str_detect(Name, input$paddler))
 # summary(pos1Crew[2:3])
#})
