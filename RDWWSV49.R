
## DW marathon results app

#-----------LIBRARY LOADING---------------
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

#--------------Thousand Mile Club data-------------
Thousand_data <- read.csv("ExportR10002019.csv") %>% 
  select(Crew, Total, Senior, Junior, Singles, VetJunior, Endeavour, Canadian, Folding,Kayak) %>% 
  arrange(Crew) 

#--------------Read Comparison data-------------

Comparison_data2 <- read.csv("ExportRABCD2019.csv") %>% 
  select(Name, Year, Races, WatersideA, WatersideB, WatersideC,WatersideD, DWSenior, DWJunior, DWEndeavour, DWVetJunior, DWSingles) %>% 
  arrange(desc(Year)) 


Comparison_data <- read.csv("ExportRABCD2019.csv") %>% 
  select(Year, Races, WatersideA, WatersideB, WatersideC,WatersideD, DWSenior, DWJunior, DWEndeavour, DWVetJunior, DWSingles) %>% 
  arrange(desc(Year)) 

Comparison_data[Comparison_data==0] <- NA
Comparison_data[Comparison_data>3000] <- NA

commenus <- Comparison_data %>% 
  select(Year, WatersideA) %>% 
  arrange(desc(Year), WatersideA) %>% 
  unique()






Comparison_data$WatersideA <- as.numeric(as.character(Comparison_data$WatersideA))
Comparison_data$WatersideB <- as.numeric(as.character(Comparison_data$WatersideB))
Comparison_data$WatersideC <- as.numeric(as.character(Comparison_data$WatersideC))
Comparison_data$WatersideD <- as.numeric(as.character(Comparison_data$WatersideD))
Comparison_data$year <- as.numeric(as.character(Comparison_data$Year))
Comparison_data$DWSenior <- as.numeric(as.character(Comparison_data$DWSenior))
Comparison_data$DWJunior <- as.numeric(as.character(Comparison_data$DWJunior))
Comparison_data$DWEndeavour <- as.numeric(as.character(Comparison_data$DWEndeavour))
Comparison_data$DWVetJunior <- as.numeric(as.character(Comparison_data$DWVetJunior))
Comparison_data$DWSingles <- as.numeric(as.character(Comparison_data$DWSingles))
#Comparison_data$CompletionRate <- as.numeric(as.character(Comparison_data$CompletionRate))



#WaterA <- Comparison_data %>% select(WatersideA) %>% arrange(WatersideA) %>% unique()
#WaterB <- Comparison_data %>% select(WatersideB) %>% arrange(WatersideB) %>% unique()
#WaterC <- Comparison_data %>% select(WatersideC) %>% arrange(WatersideC) %>% unique()
#WaterD <- Comparison_data %>% select(WatersideD) %>% arrange(WatersideD) %>% unique()




#-------------WaterSide DATASET LOADING--------------------

wsmain_data <- read.csv("ExportRWS2019.csv") %>% 
  # mutate(WSNotes = case_when(is.na(WSNotes) ~ "Finish", TRUE ~ WSNotes)) %>% 
  mutate(WSTime = format(WSTime, format = "%H:%M:%S")) %>% 
 # select(WSBoatType, Year,  WSRace, WSClass,  WSName, WSPosition, WSTime,  WSClub, WSDecimalTime,  WSNotes) %>% 
  select(Year,  WSRace, WSClass,  WSName, WSPosition, WSTime,  WSClub, WSDecimalTime,  WSNotes) %>% 
 arrange(desc(Year), WSRace, WSClass,   WSTime, WSPosition, WSName,   WSTime) %>% 
 mutate(WSName2 = paste(WSName)) 



wsmain_data$WSPosition <- as.numeric(as.character(wsmain_data$WSPosition))
wsmain_data$Year <- as.numeric(as.character(wsmain_data$Year))
wsmain_data$WSRace %<>% as.character
#wsmain_data$WSBoatType %<>% as.character
wsmain_data$WSClass %<>% as.character
wsmain_data$WSRace %<>% as.character
wsmain_data$WSName %<>% as.character
wsmain_data$WSClub %<>% as.character
wsmain_data$WSNotes %<>% as.character



wstable_data <- wsmain_data %>% 
  
 # select(-WSName2, -WSBoatType)
select(-WSName2)

wsmenus <- wsmain_data %>% 
  select(Year, WSRace, WSClass) %>% 
  arrange(desc(Year), WSRace, WSClass) %>%
  unique()

#menus <- main_data %>% 
 # select(Year, Ladies, Military, Class, SubClass, Trophies, BoatType) %>% 
  #arrange(desc(Year), Ladies, Military, Class, SubClass, Trophies, BoatType) %>% 
  #unique()

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

#WSBoatTypes <- wsmain_data %>% 
 # select(WSBoatType) %>% 
  #arrange(WSBoatType) %>% 
#  unique()

WSATime <- wsmain_data %>%
  select(WSTime) %>% 
  arrange(WSTime) %>% 
  unique()

wsAddYears <- wsmain_data %>%
  select(Year) %>% 
  arrange(desc(Year)) %>% 
  unique() 

wstop3s <- wsmain_data %>% 
  select(WSName2, WSPosition) %>% 
  filter(WSPosition %in%(c(1,2,3))) %>% 
  group_by(WSName2) %>% 
  add_tally() %>% 
  select(-WSPosition) %>% 
  unique()

wscompleteCount <- wsmain_data %>% 
  select(WSName2, WSPosition) %>% 
  filter(!is.na(WSPosition)) %>% 
  select(-WSPosition) %>% 
  group_by(WSName2) %>% 
  add_tally() %>% 
  unique()

wsfaveLadiess <- wsmain_data %>%
  select(WSName2, WSRace) %>% 
  group_by(WSName2, WSRace) %>% 
  add_tally() %>% 
  filter(n>1) %>% 
  arrange(WSName2, -n) %>% 
  unique()

# Weather Table

WMain_Data <- read.csv("ExportRWeather2019.csv") %>% select(EasterSunday, Year, SeniorFlow, FirstSenior, MaxWind, MinTemperature, CompletionRate, Regression_D,Regression_C,Regression_B,Regression_A) %>% 
  arrange(desc(Year),desc(SeniorFlow), desc(FirstSenior), desc(MaxWind), desc(MinTemperature), desc(CompletionRate) ) %>% unique()


Flow <- WMain_Data %>% select(SeniorFlow) %>% arrange(SeniorFlow) %>% unique()

FirstPlace <- WMain_Data %>% select(FirstSenior) %>% arrange(FirstSenior) %>% unique()

Wind <- WMain_Data %>% select(MaxWind) %>% arrange(MaxWind) %>%unique()

Temp <- WMain_Data %>% select(MinTemperature) %>%arrange(MinTemperature) %>% unique()

Completion <- WMain_Data %>% select(CompletionRate) %>% arrange(CompletionRate) %>% unique()

WMMain_Data <- read.csv("ExportRWeather2019.csv") %>% select(Year, SeniorFlow, MaxWind, MinTemperature, CompletionRate) %>% 
  arrange(desc(Year),desc(SeniorFlow),  desc(MaxWind), desc(MinTemperature), desc(CompletionRate) ) %>% unique()

# DW Data


main_data <- read.csv("ExportRDW2019.csv")%>%
  
  select(Year, Class, Position, Name, Club, Time, Flow, Veteran, Ladies, BoatType, Military, SubClass, Trophies, Record, DecimalTime, Notes, C1, Club1,Country,Flow) %>% 
  arrange(desc(Year), Position, Time, Ladies, Military, Trophies,Veteran,desc(Flow)) %>% 
  mutate(Name2 = paste(Name))


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
main_data$Position  %<>% as.numeric(as.character(main_data$Position))
main_data$Flow  %<>% as.numeric(as.character(main_data$Flow))

table_data <- main_data %>% 
  select(-Name2,-C1)

menus <- main_data %>% 
  select(Year, Ladies, Military, Class, SubClass, Trophies, BoatType) %>% 
  arrange(desc(Year), Ladies, Military, Class, SubClass, Trophies, BoatType) %>% 
  unique()

table_dataCrews <- main_data %>% 
  select(Year, Class,  Name, Position, Time, Flow, Club, Veteran,Ladies,Military,SubClass, Trophies, BoatType) %>% 
  arrange(desc(Year), Position)

table_dataRecords <- main_data %>% 
  select(Record, Year, Class, Name,  Position, Time, Club) %>% 
 # select(Record !="")%>%
  filter(Record != "") %>% 
  arrange(Class, Record)

table_dataExceptions <- main_data %>% 
  select(Year, Class, Name,  Time, Club, Notes, Record) %>% 
  # select(Record !="")%>%
  filter(Notes != "") %>% 
  arrange(Year, Class)

table_dataVisitors <- main_data %>% 
  select(Country, Year, Class, Name,  Time, Club, Trophies, Record) %>% 
  filter(Country != "") %>% 
  arrange(Country, Year, Class)


wstable_dataCrews <- wsmain_data %>% 
  select(Year, WSRace, WSClass,  WSPosition, WSName, WSClub, WSTime) %>% 
  arrange(desc(Year))

#lists in order
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

top3s <- main_data %>% 
  select(Name2, Position) %>% 
  #arrange(Class,Position, Year)%>%
  group_by(Name2) %>% 
  add_tally()  %>%
  select(-Position) %>% 
  unique()

top3clubs <- main_data %>% 
  select(Club, Position) %>% 
  group_by(Club) %>% 
  add_tally() %>% 
  select(-Position) %>% 
  unique()


faveLadiess <- main_data %>%
  select(Name2, Ladies) %>% 
  group_by(Name2, Ladies) %>% 
  add_tally() %>% 
  filter(n>1) %>% 
  arrange(Name2, -n) %>% 
  unique()

data_setsAGE <- c("Senior","Veteran","Century","Over 50")
data_setsdw <- c("DW","WS","Comparison","Weather")
data_sets <- c("Army","Canadian","Century","Civilian","European","Ladies","Ladies C2","Mixed","Navy","Over 50","Overseas","Police","RAF","Reserve","Scouts","Services","Tyne","U17 School","University","Vet Ladies","Veteran")
data_setsM <- c("Army","Navy","RAF","Army Reserve","Navy Reserve","RAF Reserve","Civilian Reserve")
WatersideATimeList <- c("1.5","1.6","1.7","1.8","1.9","2.0","2.1","2.2","2.3","2.4","2.5","2.6","2.7","2.8","2.9","3.0","3.1","3.2","3.3","3.4","3.5","3.6","3.7","3.8","3.9","4.0")
WatersideRaceL <- c("A","B","C","D","S")
WatersideDTimeList <- c("4.0","4.25","4.5","4.75","5.0","5.25","5.5","5.75","6.0","6.25","6.5","6.75","7.0","7.25","7.5","7.75","8.0")
TrophyList <- c("Army","Canadian","Century","Civilian","European","Home Built","Ladies","Lee","Mixed","Navy","Over 50","Overseas","Police","RAF","Reserve","Scouts","Services","Trophies","Tyne","U17 School","University","Vet Ladies","Veteran")


#-------------UI SECTION ----
ui <- fluidPage(
  #shinythemes::themeSelector(),
  
  theme = shinytheme("flatly"),
  
  list(tags$head(HTML('<link rel="icon", href="DW Logo.jpg", 
                      type="image/png" />'))),
  div(style="padding: 1px 0px; width: '100%'",
      titlePanel(
        title="", windowTitle="DW and WS Results Database"
      )
  ),
  
  
  navlistPanel(widths = c(2, 10),
               
               
               
              # fluidRow(sidebarLayout(
               #  sidebarPanel(column(12, tags$img(src="P1030108C.jpg", width = "100%")),
                #              (column(12, tags$h4("Congratulations to all those Completing this gruelling event:")))),
               
            #  fluidRow(column(10, tags$h1("DW and Waterside Results Database in the R programming language")),
             #          column(2, tags$img(src="DW Logo.JPG", height = "80px", align = "left"))),
              
               
               
    
    
     tabPanel("Introduction",
              
              fluidRow(column(10, tags$h2("Welcome to the DW and Waterside Results Database with R programming")),
                                 column(2, tags$img(src="DW Logo.jpg", height = "80px", align = "left"))),
             #column(12, tags$h2("Welcome to the DW and WS Results Database in the R Programming Language "  )),
             fluidRow(sidebarLayout(
               sidebarPanel(column(12, tags$img(src="P1030108C.jpg", width = "100%")),
                            (column(12, tags$h4("Congratulations to all those Completing this gruelling event:"))),
                            (column(12, tags$img(src="1000 Mile Club.jpg", width = "100%"))),
                            (column(12, tags$h4("The 1000 Mile Club 2011")))),
               mainPanel
               
               (fluidRow(column(12, tags$h4("What Why How"))),
                         fluidRow(column(12, tags$h4("What - This application is written in R code following the British Canoeing MRC app for Race Results."))), 
                         fluidRow(column(12, tags$h4("Why - It is written in R code to enable dynamic graphics and statistics to be generated by the user.  The provision of an additional code base could offer an alternative future for the DW results database subject to a willing database administrator."))),
                         fluidRow(column(12, tags$h4("How - The IBM Notes database (Web enabled in 2005) generates structured CSV files which are used by the application.  Please note that all comments and corrections must be made using the Master Notes system.  The R system data will only be updated annually."))),
                         fluidRow(column(12, tags$h4("Code - The code is available to interested parties on GitHub."))),
                         fluidRow(column(12, tags$h4("Please note that the database only shows completions. It follows the conventions of David Keane; a past DW Chairman who collated the results and includes individuals who completed the course but for whom the existing rules did not allow formal participation.  "))),
                         fluidRow(column(12, tags$h5("Acknowledgments-James Smythe (British Canoeing) and Callum Staff (data.giving.decisions)"))),
                
                         fluidRow(column(12, tags$h5("Version 4.5 Jan 2020"))))))),
     
     navbarMenu("DW Section",
    tabPanel("Thousand Mile Paddlers",
             fluidRow(column(12, tags$h4("The list includes paddlers who have completed the course 8 or more times irrespective of their class or the year. It therefore includes paddlers who are not recognised by DW"))),
             # Create a new row for the table.
             fluidRow(column(12, DT::dataTableOutput("tmctable")))),
    
   
    
    tabPanel("Thousand Mile Chart By Class",
             
             fluidRow(column(10, tags$h2("DW and Waterside Results Database")),
                      column(2, tags$img(src="DW Logo.jpg", height = "80px", align = "left"))),
             fluidRow(column(12, tags$h4("The list includes paddlers who have completed the course 8 or more times irrespective of their class or the year. It therefore includes paddlers who are not recognised by DW"))),
           #fluidRow(column(2,selectInput("ClassTC","Pick one or more classes",c("All",unique(menus$Class)),selected = "All",multiple = TRUE))),
             fluidRow(column(12, plotOutput("tmcchart"))),
             fluidRow(column(12, plotOutput("tmcchart1"))),
             fluidRow(column(12, plotOutput("tmcchart2"))),
             fluidRow(column(12, plotOutput("tmcchart3"))),
             fluidRow(column(12, plotOutput("tmcchart4"))),
             fluidRow(column(12, plotOutput("tmcchart5")))),
    
  
    tabPanel("Thousand Mile Chart By Boat Type",
             
             fluidRow(column(10, tags$h2("DW and Waterside Results Database")),
                      column(2, tags$img(src="DW Logo.jpg", height = "80px", align = "left"))),
             fluidRow(column(12, tags$h4("The list includes paddlers who have completed the course 8 or more times irrespective of their class or the year. It therefore includes paddlers who are not recognised by DW"))),
             # Create a new row for the table.
          #   fluidRow(column(12, plotOutput("tmcchartBT"))),
             fluidRow(column(12, plotOutput("tmcchartBT1")))),
  
    

    
 #   tabPanel("Paddler History", 
  #           fluidRow(
   #            column(7, tags$h3(textOutput("paddlername"), tags$h5("Please note, paddlers may appear as duplicates in this system if they have raced for multiple clubs or if their names were entered incorrectly at races"))),
    #           column(5,selectInput("paddler", "Choose Paddler: (hit backspace to clear and type in a name)", c(unique(paddlers$Name2)), selected = "JAMES BELL ( LON )", multiple = FALSE))
     #        ),
      #       fluidRow(column(3, tags$h3("Races Entered"), tags$h4(textOutput("no_races")), tags$h4(textOutput("comprate"))),
       #               column(9, plotOutput("races"))
        #     ),
         #    
          #   fluidRow(column(3, tags$h3("Positions"), tags$h4(textOutput("top3s")), tags$h4(textOutput("medals"))),
  #                    column(9, plotOutput("positions"))
   #          ),
    #         
     #        fluidRow(column(3, tags$h3("Home & Away"), tags$h4("Top races by no. of entries"), tableOutput("faveraces")),
      #                column(9, plotOutput("travels"))
       #      )
#    ),
    
    
    
    
    
    
    
    
    
    
    
    tabPanel("DW Crews & Clubs Table",
             fluidRow(column(12, tags$h4("DW Crews & Clubs - Please click choices and press Delete to change. - Note: A Position of 0 indicates a completion outside the normal race rules."))),
             fluidRow(column(2,selectInput("YearCC", "Pick Year",c("All",unique(menus$Year)),selected = "All",multiple = FALSE)),
                      column(2,selectInput("ClassCC","Pick one or more classes",c("All",unique(menus$Class)),selected = "All",multiple = TRUE)),
                      column(2,selectInput("LadiesCC","Ladies or Mixed",c("All",unique(menus$Ladies)),selected = "All",multiple = TRUE)),
                      column(2,selectInput("divCC","Civilian or Military",c("All",unique(menus$Military)),selected = "All",multiple = TRUE)),
                      column(2,selectInput("BTCC", "Choose Boat Type:", c("All",unique(menus$BoatType)), selected = "All", multiple = TRUE))),
             fluidRow(column(12, DT::dataTableOutput("tableCrews"))),
             fluidRow(column(12, tags$h5("The value shown for Flow is more representative of that for the Senior Class.  The Flow can change significantly over the course of the race")))
             ),
   
   tabPanel("DW Records Table",
            fluidRow(column(12, tags$h4("DW Records Table - Please note this table includes records that are not formally recognised by DW"))),
            fluidRow(column(12, DT::dataTableOutput("tableRecords"))),
            fluidRow(column(12, tags$h4("Please Note the Olderst Veteran Crew is (155 years) aged 74 & 81 - The oldest Veteran ladies Crew is (117 years) - These records are based on age and not time.
            These records were set when Senior competitors all raced in the same Class.  The Introduction of the Endeavour class now prevents a Senior crew from taking more than 1 day ")))),
    
tabPanel("DW Records Chart", 
         fluidRow(column(10, tags$h2("DW and Waterside Results Database")),
                  column(2, tags$img(src="DW Logo.jpg", height = "80px", align = "left"))),
         fluidRow(column(12, tags$h4("DW Records Chart"))),
         column(3,selectInput("attRecordclass", "Choose Class", c(unique(divsclass$Class)), selected = "Senior", multiple = TRUE)),
         fluidRow(column(12, plotOutput("Record")))),

   
   tabPanel("DW Exceptions Table",
            fluidRow(column(12, tags$h4("DW Exceptions Table - Please note this table includes individuals who completed the course but for whom the existing rules may not have allowed formal participation."))),
            fluidRow(column(12, DT::dataTableOutput("tableExceptions")))),
            #fluidRow(column(12, tags$h4("Please Note the Olderst Veteran Crew is (155 years) aged 74 & 81 - The oldest Veteran ladies Crew is (117 years) - These records are based on age and not time.
             #                           These records were set when Senior competitors all raced in the same Class.  The Introduction of the Endeavour class now prevents a Senior crew from taking more than 1 day ")))),
   


    
   tabPanel("DW Paddler History", 
             fluidRow(column(10, tags$h2("DW and Waterside Results Database")),
                      column(2, tags$img(src="DW Logo.jpg", height = "80px", align = "left"))),
            fluidRow(
              column(12, tags$h3(textOutput("DW Paddlername"), tags$h4("Please check the DW Database for the correct Name. - Note: A Position of 0 indicates a completion outside the normal race rules"))),
              column(12,textInput("paddler", "Enter Paddler - The Name is Case Sensitive (Last Name  First Name/Initial)", value = ""))),
              column(12, tags$h3(textOutput("DW Paddlername 2"), tags$h5("You may check for crew doubles if you use the exact format shown in the DW Crews and Clubs table eg Hendron Richard & King James
                                                                         , Cornish Tim & Greenham Brian, Lewis Richard & Phillips Mark C, Morrissey Jim, White Ian.  These all demonstrate a good relationship between Time and Flow.
                                                                         They also illustrate that fast paddlers are more likely to benefit form Flow."))),
            fluidRow(column(12, plotOutput("positions"))),
            fluidRow(column(12, tags$h5("The value shown for Flow is more representative of that for the Senior Class.  The Flow can change significantly over the course of the race")))
    ),

#    tabPanel("DW Paddler History vs Flow", 
 #        fluidRow(column(10, tags$h2("DW and Waterside Results Database")),
  #                column(2, tags$img(src="DW Logo.jpg", height = "80px", align = "left"))),
   #      fluidRow(
    #       column(12, tags$h3(textOutput("DW Paddlername Flow"), tags$h4("These charts indicates the effect of flow on individual paddlers.  The Boat Type is also illustrated")))),
          # column(6, tags$h4("Please click choices and press Delete to change"))),
      #   fluidRow(
       #    column(6,textInput("paddlerF", "Enter Paddler - The Name is Case Sensitive (Last Name  First Name/Initial)", value = ""))),
         # column(3, tags$h4("Please click choices and press Delete to change")),
   
                 #column(6,selectInput("att2boatF", "Choose Boat Type", c(unique(boattypes$BoatType)), selected = "Kayak", multiple = TRUE))),
         #fluidRow(column(12, plotOutput("positionsF"))),
        # fluidRow(column(12, tags$h4("These displays may be more interesting to those who have completed the course more than once. ")))
#
 #   ),

    
    tabPanel("DW Club History", 
             fluidRow(column(10, tags$h2("DW and Waterside Results Database")),
                      column(2, tags$img(src="DW Logo.jpg", height = "80px", align = "left"))),
             fluidRow(
               column(12, tags$h3(textOutput("DW Clubname"), tags$h4("Please check the DW Database for the correct Name. - Note: A Position of 0 indicates a completion outside the normal race rules"))),
               column(12,textInput("regclub", "Enter Club - The Name is Case Sensitive", value = ""))),
             fluidRow(column(12, plotOutput("clubpositions")))
    ),
    
    tabPanel("DW Clubs", 
             fluidRow(column(10, tags$h2("DW and Waterside Results Database")),
                      column(2, tags$img(src="DW Logo.jpg", height = "80px", align = "left"))),
             fluidRow(column(12, tags$h4("DW Clubs"))),
             fluidRow(
               column(12, tags$h3(textOutput("DW Clubs"))),
               column(3,selectInput("attclubyear","Pick Year(s)",c(unique(menus$Year)),selected = "2019",multiple = TRUE)),
               column(3,selectInput("attclubclass", "Choose Class(es)", c(unique(divsclass$Class)), selected = "Senior", multiple = TRUE))),
             fluidRow(column(12, plotOutput("clubplot"))),
             column(12, tags$h4("The largest group (Independents) have not indicated membership of a club and are excluded. The chart only includes clubs with a total of more than 24 completions based on the 1st named club"))),
    
    
    tabPanel("DW Attendance", 
             fluidRow(column(10, tags$h2("DW and Waterside Results Database")),
                      column(2, tags$img(src="DW Logo.jpg", height = "80px", align = "left"))),
             fluidRow(column(12, tags$h4("Civilian and Military Attendance - Click choices and press Delete to change")),
                      column(3,selectInput("attdiv", "Choose Civilian and/or Arms", c(unique(menus$Military)), selected = "Civilian", multiple = TRUE)),
                      column(3,selectInput("attclass", "Choose Class", c(unique(divsclass$Class)), selected = "Senior", multiple = TRUE)),
                      column(3,selectInput("region", "Choose Boat Type:", c(unique(boattypes$BoatType)), selected = "Kayak", multiple = TRUE)),
                      column(3,selectInput("attyear","Pick one or more Years",c(unique(menus$Year)),selected = "2019",multiple = TRUE))),
             fluidRow(column(12, plotOutput("Mattendance")))),
    
    tabPanel("DW Class by Year", 
             fluidRow(column(10, tags$h2("DW and Waterside Results Database")),
                      column(2, tags$img(src="DW Logo.jpg", height = "80px", align = "left"))),
             fluidRow(
               column(12, tags$h4("Please click choices and press Delete to change")),
               column(3,selectInput("att2class", "Choose Class", c(unique(menus$Class)), selected = "Senior", multiple = TRUE))),
             fluidRow(column(12, plotOutput("Mattendance1"))),
             fluidRow(column(12, tags$h4("Shows the number of boats completing the course each year"))),
             fluidRow(column(12, plotOutput("Mattendance1N"))),
             fluidRow(column(12, tags$h4("Shows the total number of boats completing the course")))
             
             ),
    
    tabPanel("DW Military by Year", 
             fluidRow(column(10, tags$h2("DW and Waterside Results Database")),
                      column(2, tags$img(src="DW Logo.jpg", height = "80px", align = "left"))),
             fluidRow(
               column(12, tags$h4("Please click choices and press Delete to change")),
               column(3,selectInput("attMArm", "Choose Arm", data_setsM, selected = "Army", multiple = TRUE)),
               column(3,selectInput("attMclass", "Choose Class", c(unique(menus$Class)), selected = "Senior", multiple = TRUE))),
             fluidRow(column(12, plotOutput("MattendanceM"))),
             fluidRow(column(12, tags$h4("Shows the number of boats completing the course"))), 
      fluidRow(column(12, plotOutput("MattendanceMP"))),
      fluidRow(column(12, tags$h4("Shows the Positions of boats completing the course")))), 
    
    
    tabPanel("DW Sex by Year", 
             fluidRow(column(10, tags$h2("DW and Waterside Results Database")),
                      column(2, tags$img(src="DW Logo.jpg", height = "80px", align = "left"))),
             fluidRow(
               column(12, tags$h4("Please click choices and press Delete to change")),
               column(3,selectInput("attLclass", "Choose Sex", c(unique(menus$Ladies)), selected = "Male", multiple = TRUE)),
               column(3,selectInput("attL1class", "Choose Class", c(unique(menus$Class)), selected = "Senior", multiple = TRUE))),
             fluidRow(column(12, plotOutput("MattendanceL"))),
             fluidRow(column(12, tags$h4("Shows the number of boats completing the course"))),
            fluidRow(column(12, plotOutput("MattendanceLS"))),
            fluidRow(column(12, tags$h4("Shows the positions of boats completing the course by sex")))),
    
    tabPanel("DW Minor Subclass by Year", 
             fluidRow(column(10, tags$h2("DW and Waterside Results Database")),
                      column(2, tags$img(src="DW Logo.jpg", height = "80px", align = "left"))),
             fluidRow(column(12, tags$h4("DW Minor SubClass"))),
             fluidRow(
               column(12, tags$h4("Please click choices and press Delete to change")),
               column(3,selectInput("attSubclass", "Choose Subclass", c(unique(divssubclass$SubClass)), selected = "Overseas", multiple = TRUE)),
               column(3,selectInput("attL2class", "Choose Class", c(unique(menus$Class)), selected = "Senior", multiple = TRUE))),
             fluidRow(column(12, plotOutput("MattendanceSL"))),
             fluidRow(column(12, tags$h4("Shows the number of boats completing the course. The Subclass Options have been reduced for this Chart.  Age, Services and Sex subclasses are shown separately in the application"))),
            fluidRow(column(12, plotOutput("MattendanceSLT"))),
            fluidRow(column(12, tags$h4("Shows the total number of boats completing the course. ")))),



tabPanel("DW Age group by Year", 
         fluidRow(column(10, tags$h2("DW and Waterside Results Database")),
                  column(2, tags$img(src="DW Logo.jpg", height = "80px", align = "left"))),
         fluidRow(column(12, tags$h4("DW Age group by Year"))),
         fluidRow(
           column(12, tags$h4("Please click choices and press Delete to change")),
           column(4,selectInput("attL2classCL", "Choose Class", c(unique(menus$Class)), selected = "Senior", multiple = TRUE)),
           column(4,selectInput("attSubclassSEX", "Choose Male, Female or Mixed", c(unique(divsLadies$Ladies)), selected = "Male", multiple = TRUE)),
           column(4,selectInput("attSubclassBOAT", "Choose Boat Type", c(unique(boattypes$BoatType)), selected = "Kayak", multiple = TRUE))),
         fluidRow(column(12, plotOutput("MattendanceSLO"))),
         fluidRow(column(12, tags$h4("Shows the number of boats completing the course."))),
         fluidRow(column(12, plotOutput("MattendanceSLOP"))),
         fluidRow(column(12, tags$h4("Shows the Positions of boats completing the course by Age group.")))),



    tabPanel("DW Boat Type by Year", 
             fluidRow(column(10, tags$h2("DW and Waterside Results Database")),
                      column(2, tags$img(src="DW Logo.jpg", height = "80px", align = "left"))),
             fluidRow(column(12, tags$h4("Boat types include Kayak, Canadian and Folding Boats"))),
             fluidRow(
               column(12, tags$h4("Please click choices and press Delete to change")),
               column(3,selectInput("att2boat", "Choose Boat Type", c(unique(boattypes$BoatType)), selected = "Kayak", multiple = TRUE))),
             fluidRow(column(12, plotOutput("Mattendance1BT"))),
             fluidRow(column(12, tags$h4("Shows the number of boats completing the course")))),
    
    
    
    tabPanel("DW Winning Times", 
             fluidRow(column(10, tags$h2("DW and Waterside Results Database")),
                      column(2, tags$img(src="DW Logo.jpg", height = "80px", align = "left"))),
             fluidRow(
               column(12, tags$h4("DW Winning Times")),
               column(3,selectInput("atttimeclass", "Choose Class", c(unique(divsclass$Class)), selected = "Senior", multiple = TRUE)),
               column(3,selectInput("Position", "Choose Position:", c(unique(divsposition$Position)), selected = "1", multiple = FALSE)),
               column(3,selectInput("attnyear","Plot after Year",c(unique(menus$Year)),selected = "1948",multiple = FALSE))),
             fluidRow(column(12, plotOutput("Time"))),
             fluidRow(column(12, tags$h4("Shows Times for Place over selected years.  NB Before 1971 No bank feeding was permitted.  The Endeavour Crews have been included to illustrate the fastest time for the Class")))),
    
    tabPanel("DW Trophy Times", 
             fluidRow(column(10, tags$h2("DW and Waterside Results Database")),
                      column(2, tags$img(src="DW Logo.jpg", height = "80px", align = "left"))),
             fluidRow(
               column(12, tags$h4("Trophy Times")),
               column(3,selectInput("attTrophytimeclass", "Choose Class", c(unique(divsclass$Class)), selected = "Senior", multiple = TRUE)),
               column(3,selectInput("attTrophies", "Choose Trophy:", TrophyList, selected = "Reserve", multiple = FALSE))),
             fluidRow(column(12, plotOutput("TrophyTime"))),
             fluidRow(column(12,tags$h4("Where a Trophy is selected all trophies won by the crew are shown. Senior Doubles Veteran is for the Oldest competitor to complete the course,  Lee is over 35,  Centuary is both paddlers over 50. The Services Trophy will be won by a regular in either Army, Navy or RAF.")))),
  
tabPanel("DW Visitors Table",
         fluidRow(column(12, tags$h4("DW Visitors Table."))),
         fluidRow(column(12, DT::dataTableOutput("tableVisitors")))),

  
          
    tabPanel("DW Visitors Chart", 
             fluidRow(column(10, tags$h2("DW and Waterside Results Database")),
                      column(2, tags$img(src="DW Logo.jpg", height = "80px", align = "left"))),
             fluidRow(
               column(12, tags$h4("DW Visitor Countries"))),
             fluidRow(column(12, plotOutput("Visitor")))),
    
    
    tabPanel("DW Distribution", 
             fluidRow(column(10, tags$h2("DW and Waterside Results Database")),
                      column(2, tags$img(src="DW Logo.jpg", height = "80px", align = "left"))),
             fluidRow(
               column(12, tags$h4("Distribution shows the  range of times for the Year and Class")),
               column(3,selectInput("attnyear2","Plot for Year",c(unique(menus$Year)),selected = "2019",multiple = TRUE)),
               column(3,selectInput("atttimeclass1", "Choose one or more Classes", c(unique(divsclass$Class)), selected = "Senior", multiple = TRUE)),
               fluidRow(column(12, plotOutput("Distribution")))),
             fluidRow(column(12, tags$h4("The box plot(3) shows statistical data on a plot in which a rectangle is drawn to represent the second and third quartiles with a horizontal line inside to indicate the median value. The lower and upper quartiles are shown as vertical lines top and bottom of the rectangle.  Points above or below the vertical lines are outliers; perhaps because they missed the tide")))),
    

  
    

    tabPanel("DW Completion Rate", 
             fluidRow(column(10, tags$h2("DW and Waterside Results Database")),
                      column(2, tags$img(src="DW Logo.jpg", height = "80px", align = "left"))),
             fluidRow(column(12, tags$h4("Completion Rates - These plots are for Senior Crews"))),
             fluidRow(column(12, plotOutput("CompletionR"))),
             fluidRow(column(12, plotOutput("CompletionR1"))),
             column(12, tags$h4("These plots suggest the largest factor in low completion rates is a strong head wind. Temperatures below zero also affect completion rates.  Flow rates either high or 
                                low do not seem to be critical provided the race is considered safe to complete")))),
   
   
     
   navbarMenu("Waterside Section",
    
              
           #   tabPanel("WaterSide Database",
            #           fluidRow(column(12, tags$h4("Please click choices and press Delete to change"))),
             #          fluidRow(column(4,selectInput("WSYear","Pick one or more Years",c("All",unique(wsmenus$Year)),selected = "All",multiple = TRUE)),
              #                  column(4,selectInput("WSRace","Select Race",c("All",unique(wsmenus$WSRace)),selected = "All",multiple = TRUE)),
               #                 column(4,selectInput("WSCl","Select Class",c("All",unique(wsmenus$WSClass)),selected = "All",multiple = TRUE))),
                #       # Create a new row for the table.
                 #      fluidRow(column(12, DT::dataTableOutput("WStable")))),
              
   
    
   tabPanel("WaterSide Crews & Clubs",
           
             fluidRow(column(12, tags$h4("Please click choices and press Delete to change"))),
                       fluidRow(column(4,selectInput("WSYear","Pick one or more Years",c("All",unique(wsmenus$Year)),selected = "All",multiple = TRUE)),
                                column(4,selectInput("WSCCRace", "Choose one or more Races", c("All",unique(wsmenus$WSRace)), selected = "All", multiple = TRUE)),
                                column(4,selectInput("WScl", "Choose one or more Classes", c("All",unique(wsmenus$WSClass)), selected = "All", multiple = TRUE))),
            fluidRow(column(12, DT::dataTableOutput("WStable")))),
 
   
    
    tabPanel("WS Paddler History", 
             fluidRow(column(10, tags$h2("DW and Waterside Results Database")),
                      column(2, tags$img(src="DW Logo.jpg", height = "80px", align = "left"))),
             fluidRow(
               column(12, tags$h4("WS Paddler History")),
               column(12, tags$h3(textOutput("WS paddlerName"))),
               column(6,textInput("WSpaddler", "Enter Paddler - Last Name  First Name/Initial", value = "")),
               column(6,selectInput("WSPRace", "Choose one or more Races", WatersideRaceL, selected = "A", multiple = TRUE))),
             fluidRow(column(3, tags$h4("WS Positions"), tags$h4(textOutput("WS top3s")), tags$h4(textOutput("WS medals")))),
             fluidRow(column(12, plotOutput("WSPositions")))),
    
    tabPanel("WS Club History", 
             fluidRow(
            
               column(10, tags$h2("DW and Waterside Results Database")),
                      column(2, tags$img(src="DW Logo.jpg", height = "80px", align = "left"))),
             fluidRow(column(12, tags$h4("WS Club History")),
               column(12, tags$h3(textOutput("WS Clubname"), tags$h4("Note: A Position of 0 indicates a completion outside the normal race rules"))),
               column(6,textInput("WSregclub", "Enter Club - The Name is Case Sensitive", value = "")),
               column(6,selectInput("WSPCRace", "Choose one or more Races", WatersideRaceL, selected = "A", multiple = TRUE))),
             fluidRow(column(12, plotOutput("WSclubpositions")))
    ),
    tabPanel("WS Clubs", 
             fluidRow(column(10, tags$h2("DW and Waterside Results Database")),
                      column(2, tags$img(src="DW Logo.jpg", height = "80px", align = "left"))),
             fluidRow(   column(12, tags$h4("WaterSide Clubs")),
               column(12, tags$h3(textOutput("WS Clubs"))),
               column(3,selectInput("attWSclubyear","Pick Year(s)",c(unique(menus$Year)),selected = "2019",multiple = TRUE)),
               column(6,selectInput("WSCRace", "Choose one or more Races", WatersideRaceL, selected = "A", multiple = TRUE))),
             fluidRow(column(12, plotOutput("WSclubplot")))),
    
    
    tabPanel("WS Attendance", 
             fluidRow(column(10, tags$h2("DW and Waterside Results Database")),
                      column(2, tags$img(src="DW Logo.jpg", height = "80px", align = "left"))),
             fluidRow(column(12, tags$h4("Attendance Numbers for each Race by Class(s)")),
               column(12, tags$h4("Please click choices and press Delete to change")),
               column(6,selectInput("WSclass", "Choose one or more Classes", c(unique(wsdivs$WSClass)), selected = "K2 Senior", multiple = TRUE)),
               column(6,selectInput("wsattyear","Pick one or more Years",c("All",unique(wsmenus$Year)),selected = "2019",multiple = TRUE))),
             fluidRow(column(12, plotOutput("WSattendance")))),
    
    
    tabPanel("WS Class by year", 
             fluidRow(column(10, tags$h2("DW and Waterside Results Database")),
                      column(2, tags$img(src="DW Logo.jpg", height = "80px", align = "left"))),
             fluidRow(
               column(12, tags$h4("Please click choices and press Delete to change")),
               column(6,selectInput("WS2class", "Choose one or more Classes", c(unique(wsdivs$WSClass)), selected = "K2 Senior", multiple = TRUE)),
               column(6,selectInput("WSRace2", "Choose one or more Races", WatersideRaceL, selected = "A", multiple = TRUE))),
             fluidRow(column(12, plotOutput("WS1attendance1")))),
    
    
    tabPanel("WS Winning Times", 
             fluidRow(column(10, tags$h2("DW and Waterside Results Database")),
                      column(2, tags$img(src="DW Logo.jpg", height = "80px", align = "left"))),
             fluidRow(
               column(12, tags$h4("WS Winning Times")),
               column(3,selectInput("attWStimeclass", "Choose Class", c(unique(wsdivsclass$WSClass)), selected = "K2 Senior", multiple = TRUE)),
               column(3,selectInput("WSPosition", "Choose Position:", c(unique(wsdivsposition$WSPosition)), selected = "1", multiple = FALSE)),
               column(3,selectInput("attWSnyear","Plot after Year",c(unique(menus$Year)),selected = "1990",multiple = FALSE)),
               column(3,selectInput("WSWSRace2", "Choose Race", WatersideRaceL, selected = "A", multiple = FALSE))),
             fluidRow(column(12, plotOutput("WSTime"))),
             fluidRow(column(12, tags$h4("Shows Times for Place over selected years.  NB Waterside Races Started in 1968.  Results are not available for all Years")))),
    
    
    
    tabPanel("WS A, B, C, D,Comparison",
             fluidRow(column(10, tags$h2("DW and Waterside Results Database")),
                      column(2, tags$img(src="DW Logo.jpg", height = "80px", align = "left"))),
             fluidRow(
               column(12, tags$h4("These plots are of all crews and compare WS times for A B C & D in the selected year.  The plots require data for all races in the selected year or an error message will be generated")),
               
               column(6,selectInput("comattyear","Pick Year",c(unique(commenus$Year)),selected = "2019",multiple = FALSE)),
               column(3,selectInput("MaxA","With Race A Time below (hours)",WatersideATimeList,selected = "2.5",multiple = FALSE))
             ),
             fluidRow(column(12, plotOutput("WCompGraphs"))),
             column(12, tags$h4("Faster boats are more predictable. As the time for Waterside A is reduced the plotted line appears as a better fit. These plots compare times for Waterside A against Waterside B,C and D.  The regression line has been plotted with the intercept to zero"))),
    
   tabPanel("WS & DW Crews Table",
            fluidRow(column(10, tags$h2("DW and Waterside Results Database")),
                     column(2, tags$img(src="DW Logo.jpg", height = "80px", align = "left"))),
            fluidRow(column(12, tags$h4("The Display Lists WS Times with DW Time for crews in each year where available"))),
            fluidRow(column(12, DT::dataTableOutput("WStable2")))),
    
    tabPanel("WS Distribution", 
             fluidRow(column(10, tags$h2("DW and Waterside Results Database")),
                      column(2, tags$img(src="DW Logo.jpg", height = "80px", align = "left"))),
             fluidRow(column(12, tags$h4("Distribution shows the  range of times for the Year, Race and Class")),
               column(12, tags$h4("WS Distribution")),
               column(3,selectInput("attnwsyear2","Plot for Year(s)",c(unique(menus$Year)),selected = "2019",multiple = TRUE)),
               column(3,selectInput("attDrace","Choose Race",c(unique(wsmenus$WSRace)),selected = "A",multiple = TRUE)),
               column(3,selectInput("attWSclass1", "Choose one or more Classes", c(unique(wsmenus$WSClass)), selected = "K2 Senior", multiple = TRUE)),
               fluidRow(column(12, plotOutput("WSDistribution")))),
             fluidRow(column(12, tags$h4("The box plot(2) shows statistical data on a plot in which a rectangle is drawn to represent the second and 
                                         third quartiles with a horizontal line inside to indicate the median value. The lower and upper quartiles are shown as vertical lines top and 
                                         bottom of the rectangle.  Points above or below the vertical lines are outliers"))))),

navbarMenu("DW times - WS, Flow and Weather",
           
           tabPanel("DW time vs Place",
                    fluidRow(column(10, tags$h2("DW and Waterside Results Database")),
                             column(2, tags$img(src="DW Logo.jpg", height = "80px", align = "left"))),
                    fluidRow(column(12, tags$h4("DW time vs Place"))),
                    column(3,selectInput("attclassTP", "Choose Class", c(unique(divsclass$Class)), selected = "Senior", multiple = FALSE)),
                    column(3,selectInput("attyearTP","Pick one or more Years",c(unique(menus$Year)),selected = "2019",multiple = TRUE)),
                 #   column(3,selectInput("PositionTP1", "Choose Position:", c(unique(divsposition$Position)), selected = "100", multiple = FALSE)),
                #    column(3,selectInput("PositionTP1", "Limit display up to Position:", c(unique(divsposition$Position)), selected = "100", multiple = FALSE)),
                    fluidRow(column(12, plotOutput("GraphsTP"))),
                    fluidRow(column(12, plotOutput("GraphsTPL"))),
                    fluidRow(column(12, tags$h4("These charts illustrate that different years show different profiles which can be a measure of the race conditions.  
                                                A sharp increase in times for those at the back of the field suggest they missed the tide and were caught by the tide window. 
                                                The number of boats displayed may be a reflection of the number entered or a lower completion rate")))),
           
           
           
           
           tabPanel("DW 1st vs Weather",
                    fluidRow(column(10, tags$h2("DW and Waterside Results Database")),
                             column(2, tags$img(src="DW Logo.jpg", height = "80px", align = "left"))),
                    fluidRow(column(12, tags$h4("Weather Factors affecting Winning Times"))),
                    fluidRow(column(12, plotOutput("Graphs"))),
                    fluidRow(column(12, tags$h4("These charts illustrate that for the Winning Senior Crew the most important factor for race times is flow. 
                                         Strong winds will also affect times and completion rates.  NB Wind direction or duration is not known")))),
           
           
           
           tabPanel("DW 1st vs Flow",
                    fluidRow(column(10, tags$h2("DW and Waterside Results Database")),
                             column(2, tags$img(src="DW Logo.jpg", height = "80px", align = "left"))),
                    fluidRow(
                      column(12, tags$h4("How Flow, Temperature and Wind can both affect completion rates and DW Winning Times.  These charts allow you to select the different weather parameters.")),
                      column(3,selectInput("attnfyear","Select a Year after 1970 for plot",c(unique(menus$Year)),selected = "1970",multiple = FALSE)),
                      column(3,selectInput("comrate","With Completion Rate above",c(unique(Completion$CompletionRate)),multiple = FALSE)),
                      column(3,selectInput("windg","With Wind below",c(unique(Wind$MaxWind)),selected = "46.5",multiple = FALSE)),
                      column(3,selectInput("tempg","With Temp above",c(unique(Temp$MinTemperature)),selected = "-2",multiple = FALSE))),
                    fluidRow(column(12, plotOutput("FGraphs"))),
                    fluidRow(column(12, tags$h4("The formula on charts indicate that first place with no flow would be about 18.5 hours.  100cm/sec flow reduces the time by 1.59 hours
                                                (Before your changes). You may change the parameters for these plots but weather is not available before 1970.")))),
           
           
           
           
           tabPanel("DW Weather Table",
                   
                    fluidRow(column(12, tags$h4("The Regression Number, when multipled by the WS time, gives the average time for Senior Crews in DW for the year and conditions"))),
                    fluidRow(column(12, DT::dataTableOutput("Wtable")))),
           
           tabPanel("DW Weather Box Plots",
                    fluidRow(column(10, tags$h2("DW and Waterside Results Database")),
                             column(2, tags$img(src="DW Logo.jpg", height = "80px", align = "left"))),
                   
                    fluidRow(column(12, tags$h4("Weather Box Plots Showing the Range of conditions"))),
                    fluidRow(column(12, plotOutput("WeatherBox"))),
                    fluidRow(column(12, tags$h4("Box plots show statistical data where a rectangle is drawn to represent the second and third quartiles with a horizontal line inside to 
indicate the MEDIAN value. The lower and upper quartiles are shown as vertical lines top and bottom of the rectangle.  Points above or below the vertical lines are outliers. 
Mean, MEDIAN, and mode are three kinds of averages. ... The mean is the average you're used to, where you add up all the values and then divide by the number of values. The MEDIAN is the middle value in the list of numbers.
                                         ")))),
           
           tabPanel("DW Weather vs Year",
                    fluidRow(column(10, tags$h2("DW and Waterside Results Database")),
                             column(2, tags$img(src="DW Logo.jpg", height = "80px", align = "left"))),
                    fluidRow(column(12, tags$h4("DW Weather vs Year with the River Thames Status colour codes"))),
                    fluidRow(column(12, plotOutput("WeatherYear"))),
                    fluidRow(column(12, tags$h4("A Green Status is defined as less than 65 cm/sec.  Amber is Less than 100 cm/sec and Red is above 120 cm/sec with Red/Amber (shown as Orange) between 99 and 120.  
  Note that the unofficial record was set in 2000 but the race was abandoned.  The race was cancelled in 2001 due to a foot and mouth outbreak.  
  In 2018 the race was stopped at Reading. The official record was set in 1979. Please be aware that flow rates change over the race weekend and the chart is most representative of the Senior event"  ))) ,
                    fluidRow(column(12, tags$h4("Kingston Flow Rate Web Site", h5("Shoothill Gauge Map", a("Current Reading", target="_blank", href="http://www.gaugemap.co.uk/#!Map/Summary/1249/1382")) )))
               
                   ),
           
       #A Green Status is defined as less than 65 cm/sec.  Amber is Less than 100 cm/sec and Red is above 99 cm/sec with Red/Amber between 99 and 120.  Note that the unofficial record was set in 2000 but the race was abandoned.  The race was cancelled in 2001 due to a foot and mouth outbreak.  In 2018 the race was stopped at Reading. The official record was set in 1979.    
    
tabPanel("DW Times vs Waterside D",
         fluidRow(column(10, tags$h2("DW and Waterside Results Database")),
                  column(2, tags$img(src="DW Logo.jpg", height = "80px", align = "left"))),
         fluidRow(
           column(12, tags$h4("Enter your time for Waterside D.  The line will then show the average times for Senior crews depending on flow.  You can vary the conditions to see if there is any effect")),
           column(3,selectInput("WSDT","Please select your nearest WS D time",WatersideDTimeList,selected = "5.0",multiple = FALSE)),
           column(3,selectInput("comrateR","With Completion Rate above",c(unique(Completion$CompletionRate)),multiple = FALSE)),
           column(3,selectInput("windgR","With Wind below",c(unique(Wind$MaxWind)),selected = "46.5",multiple = FALSE)),
           column(3,selectInput("tempgR","With Temp above",c(unique(Temp$MinTemperature)),selected = "-2",multiple = FALSE))),
         fluidRow(column(12, plotOutput("FGraphsR"))),

         fluidRow(column(12, tags$h4("The formula and line shown on the chart indicates the typical time for a Senior crew with different conditions and is based on hundreds of crews over many years. 
                                         The conditions on Waterside D will be a major factor in these indicators and thus a tail wind and a full canal will reduce race times. This will then affect predicted times for DW.  
                                         It follows that the conditions on DW will also be a major factor in times and again a tail wind and a full canal may improve your time but of course a strong head wind will lead to slower times"))),
         fluidRow(column(12, tags$h4("The objective is to demonstrate what times might be expected in the prevailing conditions depending on your WS D time.  
                                         The points on the chart are plotted from the regression formula (best fit formula) shown on the DW Weather tab.  They show the value when a race was run under the parameters set. 
                                     The regression points can be above or below the line depending.  The line is the average of all races. The points provide an indication of the variation from the line which depend on the conditions on the individual race."))),

fluidRow(column(12, tags$h4("The points on the chart are NOT race times.  They are a predicted time based on all the crews in that year.  It follows that if there were no crews capable doing a fast
                            WS D time in that year there would be no fast crew racing DW that year.  The reality is that to take advantage of the conditions a fast crew must be racing.")))),
    
   




 tabPanel("WS & DW Comparison",
             fluidRow(column(10, tags$h2("DW and Waterside Results Database")),column(2, tags$img(src="DW Logo.jpg", height = "80px", align = "left"))),
             fluidRow(sidebarLayout(sidebarPanel(
               
               fluidRow(
                 column(6,selectInput("comattDWyear","Pick Year",c(unique(commenus$Year)),selected = "2019",multiple = FALSE)),              
                 column(12, tags$h4("Please Note that these plots require data for all races in the selected year or an error message will be generated"))
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
               
               
               
              
                 )))
    
             ),
navbarMenu("Information",
tabPanel("Portaging Distances",fluidRow(column(12, tags$iframe(style="height:1000px; width:100%", src="PortagingDistance.pdf")))),
tabPanel("Time Sheet Senior",fluidRow(column(12, tags$iframe(style="height:1000px; width:100%", src="TimeSheet2.pdf"))))

         ),
navbarMenu("History",
           tabPanel("Key Dates",fluidRow(column(12, tags$iframe(style="height:1000px; width:100%", src="Key Dates.pdf")))),
           tabPanel("Trophy Dates",fluidRow(column(12, tags$iframe(style="height:1000px; width:100%", src="Trophies - Year of Introduction.pdf")))),
           tabPanel("The Early History by David Keane",fluidRow(column(12, tags$iframe(style="height:1000px; width:100%", src="DW David Keane.pdf")))),
           tabPanel("History to 2019",fluidRow(column(12, tags$iframe(style="height:1000px; width:100%", src="History to 2019.pdf")))),
           tabPanel("Original Portage Map",fluidRow(column(12, tags$iframe(style="height:1000px; width:100%", src="Original Portage Map.pdf")))),
           tabPanel("Photos",
                    fluidRow(column(4, tags$h3("Photographs of some past competitors"))),
                    fluidRow(column(4, tags$h4(" "))),
                    fluidRow(
             column(4, tags$img(src="1962 Ted Tandy & Gillie Howe at Devizes.jpg", width = "100%")),
             column(4, tags$img(src="1962 Ted Tandy & Gillie Howe.jpg", width = "100%")),
             column(4, tags$img(src="1965 Record Cook and Stimson.jpg", width = "100%"))
           ),
           fluidRow(column(4, tags$h4("1962 Ted Tandy & Gillie Howe leaving Devizes")),column(4, tags$h4("1962 Ted Tandy & Gillie Howe")),column(4, tags$h4("1965 Cook and Stimson"))),
           
           fluidRow(column(12, tags$h4("Gillie (GR) Howe completed the event 10 times with an average 2nd place including a folding boat.  
                                       He was the first member of the 1000 mile club.  As can be seen in these early pictures no bouyancy aids (life jackets) are being worn and in latter
                                       years many of those which were used were not inflated"))),
           fluidRow(column(4, tags$h3(" "))),
           
           fluidRow(
             column(4, tags$img(src="1965 The first Glass Boat - Accord.jpg", width = "100%")),
             column(4, tags$img(src="1966 approximate - From the Boat Number D N Aterton & J Babden.jpg", width = "100%")),
             column(4, tags$img(src="1969 Paganelli and Evans leaving Devizes.jpg", width = "100%"))
           ),
           fluidRow(column(4, tags$h4("1965 The first Glass Boat - Accord")),column(4, tags$h4("1966 approximate - From the Boat Number D N Aterton & J Babden")),column(4, tags$h4("1969 Paganelli and Evans leaving Devizes"))),
           fluidRow(column(4, tags$h3(" "))),
           
           fluidRow(
             column(4, tags$img(src="1970 Croften and The long Run.jpg", width = "100%")),
             column(4, tags$img(src="1970 Pape and Serensen.jpg", width = "100%")),
             column(4, tags$img(src="1978 Brian Greenham & Tim Cornish.jpg", width = "100%"))),
             fluidRow(column(4, tags$h4("1970 Croften and The long Run")),column(4, tags$h4("1970 Pape and Serensen one of the first International Competitors")),column(4, tags$h4("1978 Brian Greenham & Tim Cornish"))),
             
             fluidRow(column(12, tags$h4("The shot of the Long Run is a reference to the Crofton flight of locks which as can been seen were dry until the canal was restored. 
                                        This required competitors to portage 1850 meters which the faster crews would run.  Some crews continue to run this section although it can be paddled in a similar time."))
           ),
           
           fluidRow(column(4, tags$h3(" "))),
           fluidRow(
             column(4, tags$img(src="1979 Brain Greenham and Tim Cornish Portage.jpg", width = "100%")),
             column(4, tags$img(src="1979 Brain Greenham and Tim Cornish.jpg", width = "100%")),
             column(4, tags$img(src="1984 Cornish & Viljoen at Westminster.jpg", width = "100%"))
           ),
           fluidRow(column(4, tags$h4("1979 Brain Greenham and Tim Cornish Portage")),column(4, tags$h4("1979 Brain Greenham and Tim Cornish")),column(4, tags$h4("1984 Cornish & Viljoen at Westminster"))),
           fluidRow(column(12, tags$h4("Brain Greenham and Tim Cornish hold the record for the DW wich was set in 1979 in a time of 15:34.  
                                       This time was beaten in 2000 by Steve Baker and Duncan Capps with a time of 15:17, however as the race was abandomed in 2000 this time was not recognised by DW and is therefore unofficial"))),
           fluidRow(column(4, tags$h3(" "))),
            fluidRow(
              column(5, tags$img(src="1984 Cornish & Viljoen.jpg", width = "100%")),
              column(4, tags$img(src="1989 Paul & Micheal Wells.jpg", width = "100%")),
              column(3, tags$img(src="1975 First Ladies Crew.jpg", width = "100%"))
            ),
          fluidRow(column(4, tags$h4("1984 Cornish & Viljoen")),column(4, tags$h4("1989 Paul & Micheal Wells")),column(4, tags$h4("1975 First Ladies Crew - Maurene Hassack & Jo Saxby"))),
          fluidRow(column(12, tags$h4("The ladies were not allowed to enter formally but as was the custom at that time, they were allowed the facilities of the race and given a completion certificate.  
          The following year Maurene paddled with her neighbour Diana Johnson in the new Senior Ladies sub class."))),
          
         
          fluidRow(column(4, tags$h3(" "))),
          fluidRow(
            column(8, tags$img(src="1000 Mile Club.jpg", width = "100%")),
            column(4, tags$img(src="Eric Draper and Ernie Flood.jpg", width = "100%"))
          ),
          fluidRow(column(6, tags$h4("1000 Mile Club gathering taken at the inaugural presentation of awards")),column(6, tags$h4("1950 Eric Draper and Ernie Flood with their Cockle canoe"))),
          fluidRow(column(12, tags$h4("Ernie Flood is bending over the canoe and Eric Draper is standing behind Ernie with his arms folded. Both men were from the Coalporters ARC. 
                                      The clothing worn is so different from the modern gear. Eric Draper was born in April 1929 and was aged about 21 when he took part in the first race in 1950. 
                                      The rubber canoes used were ex-services as this was what was available and affordable but suffered drag and were heavy when lifting out of the water to go 
                                      round the locks. Notice the bottom of the picture on the right of the boat.  This appears to show Ernie's leg indicating a rip in the Canvas.  
                                      The three back rests suggest it could be used by 3 people and subsequently a three man version of the MK2 was developed into a three man 17ft 4 in version - the MK 2**. 
                                      So far as is known this is the only time this type of boat has been used on DW.  
                                      
                                      Designed by Fred Goatley from the Isle Of Wight, the Mark 2 was designed during a retirement period some 9 months before 'Blondie' Hasler, and his use of it for the Bordeaux raid 
                                      (Operation Frankton the attack on Bordeaux in late 1942.) by the 'Cockleshell Heroes'")))
          )),
          navbarMenu("Technical Admin",
                     tabPanel("Import DW Direct to R",fluidRow(column(12, tags$iframe(style="height:1000px; width:100%", src="ImportDWDirecttoR.pdf")))),
                     tabPanel("Import WS Direct to R",fluidRow(column(12, tags$iframe(style="height:1000px; width:100%", src="ImportWSDirecttoR.pdf"))))
                     
          

      #     TabPanel("Photo",fluidRow(column(12, tags$img(src="DW 1979 - 1.jpg", width = "100%")),
       #    (column(12, tags$h4("Congratulations to all those Completing this gruelling event:")))))
           
)
))



#----------------SERVER FUNCTION ---------------------


server <- function(input, output, session)
{
  output$choose_dataset <- renderUI({
    selectInput("dataset", "Data set", as.list(data_setsdw))
  })

  # DW Thousand Mile Paddlers table with reactive filtering
  output$tmctable <- DT::renderDataTable({
    DT::datatable({data <- Thousand_data
    data #i.e. render this dataset as a table
    },
  #  filter = list(position = 'top', clear = FALSE),
    options = list(pageLength = 125, search = list(smart = FALSE)), 
    rownames = FALSE)})
  
  # DW Database sidebar table with reactive filtering
  output$tableSB <- DT::renderDataTable({
    DT::datatable({data <- table_dataCrews
    data #i.e. render this dataset as a table
    },

    options = list(pageLength = 15, search = list(smart = FALSE)), 
    rownames = FALSE)})

#datatable() (e.g., options = list(search = list(smart = FALSE))) 
  
  
  
  # DW Database table with reactive filtering
  output$tableCrews <- DT::renderDataTable({
    DT::datatable({data <- table_dataCrews 
    if (input$YearCC != "All") {data <- data[data$Year %in% input$YearCC,]}
    if (input$ClassCC != "All") {data <- data[data$Class %in% input$ClassCC,]}
    if (input$LadiesCC != "All") {data <- data[data$Ladies %in% input$LadiesCC,]}
    if (input$divCC != "All") {data <- data[data$Military %in% input$divCC,]}
    if (input$BTCC != "All") {data <- data[data$BoatType %in% input$BTCC,]}
    
    data #i.e. render this dataset as a table
   # unite(table_dataCrews,"SubClasses", c("Veteran","Military","Ladies","SubClass"), sep = ", ")
    },
   # filter = list(position = 'top', clear = FALSE),
    options = list(
      search = list(regex = TRUE, caseInsensitive = FALSE, smart = FALSE
                    ),
      pageLength = 25
    ),
    rownames = FALSE)})
  
  # DW Database table with reactive filtering
  output$table <- DT::renderDataTable({
    DT::datatable({data <- table_data 
    if (input$Year != "All") {data <- data[data$Year %in% input$Year,]}
    if (input$Class != "All") {data <- data[data$Class %in% input$Class,]}
    if (input$Ladies != "All") {data <- data[data$Ladies %in% input$Ladies,]}
    if (input$div != "All") {data <- data[data$Military %in% input$div,]}
    if (input$BT != "All") {data <- data[data$BoatType %in% input$BT,]}
    
    data #i.e. render this dataset as a table
    },
   # filter = list(position = 'top', clear = FALSE),
    options = list(
      search = list(regex = TRUE, caseInsensitive = FALSE),
      pageLength = 25
    ),
    rownames = FALSE)})
  
 
  #renderDataTable(..., options = list(search = list(caseInsensitve = FALSE, regex = TRUE)))
  
  # DW Database table with reactive filtering
  output$tableRecords <- DT::renderDataTable({
    DT::datatable({data <- table_dataRecords 
    data #i.e. render this dataset as a table
   # unite(table_dataPosition,"SubClasses", c("Veteran","Military","Ladies","SubClass"), sep = ", ")
    },
  
   options = list(
      search = list(regex = TRUE, caseInsensitive = FALSE),
      pageLength = 75
    ),
    rownames = FALSE)})
  #renderDataTable(..., options = list(search = list(caseInsensitve = FALSE, regex = TRUE)))
  
  # DW Database table with reactive filtering
  output$tableExceptions <- DT::renderDataTable({
    DT::datatable({data <- table_dataExceptions 
    data #i.e. render this dataset as a table
    # unite(table_dataPosition,"SubClasses", c("Veteran","Military","Ladies","SubClass"), sep = ", ")
    },
    
    options = list(
      search = list(regex = TRUE, caseInsensitive = FALSE),
      pageLength = 75
    ),
    rownames = FALSE)})
  #renderDataTable(..., options = list(search = list(caseInsensitve = FALSE, regex = TRUE)))
  
  # DW Database table with reactive filtering
  output$tableVisitors <- DT::renderDataTable({
    DT::datatable({data <- table_dataVisitors 
    data #i.e. render this dataset as a table
    # unite(table_dataPosition,"SubClasses", c("Veteran","Military","Ladies","SubClass"), sep = ", ")
    },
    
    options = list(
      search = list(regex = TRUE, caseInsensitive = FALSE),
      pageLength = 200
    ),
    rownames = FALSE)})
  #renderDataTable(..., options = list(search = list(caseInsensitve = FALSE, regex = TRUE)))
  
  
  
  
  # WS Database table with reactive filtering
  
  output$WStable <- DT::renderDataTable({
    DT::datatable({data <- wstable_data
    if (input$WSYear != "All") {data <- data[data$Year %in% input$WSYear,]}
    if (input$WSCCRace != "All") {data <- data[data$WSRace %in% input$WSCCRace,]}
    if (input$WScl != "All") {data <- data[data$WSClass %in% input$WScl,]} 
    data #i.e. render this dataset as a table
    },
  #  filter = list(position = 'top', clear = FALSE),
    options = list(
      search = list(regex = TRUE, caseInsensitive = FALSE),
      pageLength = 50 ), 
    rownames = FALSE)
  })
  
  output$WStable2 <- DT::renderDataTable({
    DT::datatable({data <- Comparison_data2},
    
    options = list(
      search = list(regex = TRUE, caseInsensitive = FALSE),
      pageLength = 50 ), 
    rownames = FALSE)
  })
  
  
  
  
  
 # options = list(
    order = list(list(2, 'asc'), list(4, 'desc'))
  
  
  
  #filtered inputs for main wstable
  wsmenus_filtered1 <- reactive({if ("All" %in% input$Year) { wsmenus} else {wsmenus %>% filter(Year %in% input$Year)}})
  observe({updateSelectInput(session, "WSRace", choices = c("All", wsmenus_filtered1()$WSRace), selected = "All")})
  wsmenus_filtered2 <- reactive({if ("All" %in% input$WSCCRace) {wsmenus_filtered1()} else {wsmenus_filtered1() %>% filter(WSCCRace %in% c(input$WSCCRace))}})
  observe({updateSelectInput(session, "WSCl", choices = c("All",wsmenus_filtered2()$WSClass), selected = "All")})
  #filtered inputs for main table
  menus_filtered1 <- reactive({if ("All" %in% input$Year) {menus} else {menus %>% filter(Year %in% input$Year)}})
  observe({updateSelectInput(session, "Ladies", choices = c("All", menus_filtered1()$Ladies), selected = "All")})
  menus_filtered2 <- reactive({if ("All" %in% input$Ladies) {menus_filtered1()} else {menus_filtered1() %>% filter(Ladies %in% c(input$Ladies))}})
  observe({updateSelectInput(session, "div", choices = c("All",menus_filtered2()$Military), selected = "All")})
  
  
  
  output$WSattendance <- renderPlot({
    
    attchartdata <- wsmain_data %>% 
      select( WSRace, Year, WSClass) %>%
      filter(WSClass %in% input$WSclass,Year == input$wsattyear)
    
    
    ggplot(attchartdata, aes(WSRace))+geom_bar(aes(fill = WSClass), stat = "count")+coord_flip()+facet_wrap(~Year)
    
  })
  
  output$WS1attendance1 <- renderPlot({
    
    attchartdata <- wsmain_data %>% 
      select( WSRace, Year, WSClass) %>%
      filter(WSClass %in% input$WS2class, WSRace %in% input$WSRace2)
    ggplot(attchartdata, aes(Year))+geom_bar(aes(fill = WSClass), stat = "count")+ scale_x_continuous(breaks=seq(0,2050,5))
    
    
    
    
    
  })
  
  
  output$top3sout <- renderText({
    paste("Number of Completions:",
          top3s$n[top3s$Name2 == input$paddler]
    )
    
    
  })
  
  
  output$positions <- renderPlot({poschartdata <- main_data %>% select(Year, Position, Class, Name2, DecimalTime, Flow, BoatType) %>% filter(str_detect(Name2, input$paddler))
  a <-  ggplot(poschartdata)+geom_point(aes(Year, Position, color = Class), position=position_jitter(width=0.1, height=0.1), size = 5)+scale_y_continuous(breaks=seq(2,200,2))+ scale_x_continuous(breaks=seq(0,2050,5))
  b <-  ggplot(poschartdata)+geom_point(aes(Year, DecimalTime, color = BoatType), position=position_jitter(width=0.1, height=0.1), size = 5)+scale_y_continuous(breaks=seq(2,200,1))+ scale_x_continuous(breaks=seq(0,2050,5))
  c <-  ggplot(poschartdata)+geom_point(aes(Flow, DecimalTime, color = Class), position=position_jitter(width=0.1, height=0.1), size = 5)+scale_y_continuous(breaks=seq(2,200,1))+ scale_x_continuous(breaks=seq(0,2050,25))
  
  ggarrange(a, b,c, ncol = 3, nrow = 1)
  
  })
  
 # output$positionsF <- renderPlot({poschartdata <- main_data %>% select(Year, Position, Class, Name2, DecimalTime, Flow,BoatType) %>% filter(str_detect(Name2, input$paddlerF))
  #a <-  ggplot(poschartdata)+geom_point(aes(Flow, DecimalTime, color = Class), position=position_jitter(width=0.1, height=0.1), size = 5)+scale_y_continuous(breaks=seq(2,200,1))+ scale_x_continuous(breaks=seq(0,2050,5))
#  b <-  ggplot(poschartdata)+geom_point(aes(Flow, DecimalTime, color = BoatType), position=position_jitter(width=0.1, height=0.1), size = 5)+scale_y_continuous(breaks=seq(2,200,1))+ scale_x_continuous(breaks=seq(0,2050,5))
  #c <-  ggplot(poschartdata)+geom_point(aes(Flow, DecimalTime, color = Name2), position=position_jitter(width=0.1, height=0.1), size = 5)+scale_y_continuous(breaks=seq(2,200,1))+ scale_x_continuous(breaks=seq(0,2050,5))
 # ggarrange(a,b, ncol = 2, nrow = 1)
#  
 # })
  
  
  output$top3clubs <- renderText({
    paste("Number of Completions:",
          top3clubs$n[top3clubs$Club == input$regclub]
    )
  })
  
  output$clubpositions <- renderPlot({poschartdata <- main_data %>% select(Year, Position, Class, Club, Name2, DecimalTime) %>% filter(str_detect(Club, input$regclub))
  a <-  ggplot(poschartdata)+geom_point(aes(Year, Position, color = Class), position=position_jitter(width=0.1, height=0.1), size = 5)+ scale_y_continuous(breaks=seq(0,200,5)) + scale_x_continuous(breaks=seq(0,2050,5))
  b <-  ggplot(poschartdata)+geom_point(aes(Year, DecimalTime, color = Class), position=position_jitter(width=0.1, height=0.1), size = 5)+scale_y_continuous(breaks=seq(2,200,1))+ scale_x_continuous(breaks=seq(0,2050,5))
  c <- ggplot(poschartdata,aes(Year))+geom_bar(aes(fill = Class), width = 0.4)+scale_y_continuous(breaks=seq(0,200,1))+ scale_x_continuous(breaks=seq(0,2050,5))
  
  ggarrange(a, b, c, ncol = 3, nrow = 1)
  
  })
  #ggplot(attchartdata, aes(Ladies))+geom_bar(aes(fill = Military), stat = "count")+coord_flip()+facet_wrap(~Year) + ggtitle("Plot 1 - By Sex")
  
  output$WSclubpositions <- renderPlot({poschartdata <- wsmain_data %>% select(Year, WSPosition, WSClass, WSClub, WSName2, WSDecimalTime, WSRace) %>% filter(str_detect(WSClub, input$WSregclub), WSRace %in% input$WSPCRace)
  a <-  ggplot(poschartdata)+geom_point(aes(Year, WSPosition, color = WSClass), position=position_jitter(width=0.1, height=0.1), size = 5)+ scale_y_continuous(breaks=seq(0,200,5))   + scale_x_continuous(breaks=seq(0,2050,5))
  b <-  ggplot(poschartdata)+geom_point(aes(Year, WSDecimalTime, color = WSClass), position=position_jitter(width=0.1, height=0.1), size = 5)+scale_y_continuous(breaks=seq(2,200,1))+ scale_x_continuous(breaks=seq(0,2050,5))
  c <- ggplot(poschartdata,aes(Year))+geom_bar(aes(fill = WSClass), width = 0.4)+scale_y_continuous(breaks=seq(0,200,1))+ scale_x_continuous(breaks=seq(0,2050,5))
  ggarrange(a, b, c, ncol = 3, nrow = 1)
  
  })
  
  
  output$clubplot <- renderPlot({attchartdata <- main_data %>% select(Year, Class, Club1,C1) %>% filter(Year %in% input$attclubyear, Class %in% input$attclubclass, C1>24 & C1<900)
  
 
  ggplot(attchartdata,aes(x=reorder(Club1,-table(Club1)[Club1])))+geom_bar(aes(fill = Class),stat="count")+theme(axis.text.x=element_text(angle=90, hjust=1))+ labs(y = "Completions", x="Largest Clubs shown by Number of Completions") 

  })
  
  
  
  
  
  
  
  
  output$WSclubplot <- renderPlot({attchartdata <- wsmain_data %>% select(Year, WSClub, WSRace, WSClass) %>% filter(Year %in% input$attWSclubyear, WSRace %in% input$WSCRace)
  ggplot(attchartdata,aes(x=reorder(WSClub,-table(WSClub)[WSClub])))+geom_bar(aes(fill = WSRace),stat="count") +theme(axis.text.x=element_text(angle=90, hjust=1))+ labs(y = "Completions", x="WaterSided Clubs shown by Number of Completions") 
  
  })
  
  output$WSTime <- renderPlot({poschartdata <- wsmain_data %>% select(Year, WSPosition, WSClass, WSDecimalTime, WSRace) %>% 
    filter(WSClass %in% input$attWStimeclass, WSPosition == input$WSPosition, Year >= input$attWSnyear, WSRace %in% input$WSWSRace2)
  ggplot(poschartdata)+geom_line(aes(Year, WSDecimalTime, color = WSClass),  size = 5) + scale_y_continuous(breaks=seq(1,200,0.2))+ scale_x_continuous(breaks=seq(0,2050,5))
  
  })
  
  output$Record <- renderPlot({attchartdata <- main_data %>% select(Record, Class, DecimalTime) %>% filter( Class %in% input$attRecordclass, Record != "", Record != "Veteran (155 years) 74 & 81")
  
  
  ggplot(attchartdata, aes(fill = Record, x=Record, reorder(DecimalTime,-table(DecimalTime)[DecimalTime]), color = Record)) +
    geom_bar(stat="identity")+coord_flip()+ labs( y="Record Time") 
  
  
  # ggplot(attchartdata)+geom_point(aes(Record, DecimalTime, color = Class), position=position_jitter(width=0.1, height=0.1), size = 5)+scale_y_continuous(breaks=seq(2,200,1))+  theme(axis.text.x=element_text(angle=90,hjust=1))+coord_flip()
  })
  
  output$Visitor <- renderPlot({attchartdata <- main_data %>% select(Country, Class) %>% filter( Country != "" )
  ggplot(attchartdata, aes(x=Country, fill = Class, color = Class)) + geom_bar(stat="Count")+ labs(y = "Completions")
  })
  
  
  
  
  
  output$Mattendance <- renderPlot({
    attchartdata <- main_data %>% 
      select(BoatType, Ladies, Year, Military, Veteran, Class, Club, SubClass) %>% 
      filter(BoatType %in% input$region, Year %in% input$attyear, Military %in% input$attdiv, Class %in% input$attclass)
    bxp <- ggplot(attchartdata, aes(Ladies))+geom_bar(aes(fill = Military), stat = "count")+coord_flip()+facet_wrap(~Year) + ggtitle("Plot 1 - By Sex")+ labs(y = "Completions") +labs(x = "Sex") 
    bxp
    b <- ggplot(attchartdata, aes(Veteran))+geom_bar(aes(fill = Military), stat = "count")+coord_flip()+facet_wrap(~Year) + ggtitle("Plot 2 - By Age Group")+ labs(y = "Completions") +labs(x = "Age Group") 
    b
    dp <- ggplot(attchartdata, aes(Class))+geom_bar(aes(fill = Military), stat = "count")+coord_flip()+facet_wrap(~Year)+ ggtitle("Plot 3 - By Class")+ labs(y = "Completions") +labs(x = "Civilian or Arm") 
    dp
    #attchartdata <- main_data %>%
    #    select(BoatType, Ladies, Year, Military, Class, Club, SubClass) %>%
    #    filter(BoatType %in% input$region, Year %in% input$attyear, Military %in% input$attdiv, Class %in% input$attclass, SubClass != "")
    
    # sc <- ggplot(attchartdata, aes(SubClass))+geom_bar(aes(fill = BoatType), stat = "count")+coord_flip()+facet_wrap(~Year) + ggtitle("Plot 4 - By Other Subclass")
    #sc
    ggarrange(bxp, b,dp,  ncol = 3, nrow = 1)})
  #  ggarrange(sc,  ncol = 1, nrow = 1)})
  
  
  
    output$tmcchart <- renderPlot({poschartdata <- Thousand_data %>%select(Crew, Senior, Junior, Singles, VetJunior, Endeavour)
  dat <- melt(poschartdata,id="Crew") 
  ggplot(dat,aes(Crew,value, fill=variable))+geom_bar(stat="identity")+theme(axis.text.x=element_text(angle=90, hjust=1))+ labs(y = "Completions", x="") + labs(title = "Chart by All Classes")+ scale_y_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20,22,24))})

  output$tmcchart1 <- renderPlot({poschartdata <- Thousand_data %>%select(Crew, Senior)
  dat <- melt(poschartdata,id="Crew")
  ggplot(dat,aes(Crew,value, fill=variable))+geom_bar(stat="identity", fill="#FF9999")+theme(axis.text.x=element_text(angle=90, hjust=1))+ labs(y = "Completions", x="") + labs(title = "Chart by Senior Class")+ scale_y_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20,22,24))})

  output$tmcchart2 <- renderPlot({poschartdata <- Thousand_data %>%select(Crew, Junior)
  dat <- melt(poschartdata,id="Crew")
  ggplot(dat,aes(Crew,value, fill= variable))+geom_bar(stat="identity", fill="#E69F00") +theme(axis.text.x=element_text(angle=90, hjust=1))+ labs(y = "Completions", x="") + labs(title = "Chart by Junior Class")+ scale_y_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20,22,24))})
  
  output$tmcchart3 <- renderPlot({poschartdata <- Thousand_data %>%select(Crew, Singles)
  dat <- melt(poschartdata,id="Crew")
  ggplot(dat,aes(Crew,value, fill=variable))+geom_bar(stat="identity", fill="#009E73")+theme(axis.text.x=element_text(angle=90, hjust=1))+ labs(y = "Completions", x="") + labs(title = "Chart by Singles Class")+ scale_y_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20,22,24))})
  
  output$tmcchart4 <- renderPlot({poschartdata <- Thousand_data %>%select(Crew, VetJunior)
  dat <- melt(poschartdata,id="Crew")
  ggplot(dat,aes(Crew,value, fill=variable))+geom_bar(stat="identity", fill="#0072B2")+theme(axis.text.x=element_text(angle=90, hjust=1))+ labs(y = "Completions", x="") + labs(title = "Chart by Vet Junior Class")+ scale_y_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20,22,24))})
  
  output$tmcchart5 <- renderPlot({poschartdata <- Thousand_data %>%select(Crew, Endeavour)
  dat <- melt(poschartdata,id="Crew")
  ggplot(dat,aes(Crew,value, fill=variable))+geom_bar(stat="identity", fill="#CC79A7")+theme(axis.text.x=element_text(angle=90, hjust=1))+ labs(y = "Completions", x="") + labs(title = "Chart by Endeavour Class")+ scale_y_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20,22,24))})
  
  

  
  output$tmcchartBT <- renderPlot({poschartdata <- Thousand_data %>% select(Crew, Canadian, Singles) 
  dat <- melt(poschartdata,id="Crew")
  ggplot(dat,aes(Crew,value, fill=variable))+geom_bar(stat="identity")+theme(axis.text.x=element_text(angle=90, hjust=1))+ labs(y = "Completions", x="Paddler") + labs(title = "Chart by class")+ scale_y_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20,22,24))
  })
  
  output$tmcchartBT1 <- renderPlot({poschartdata <- Thousand_data %>% select(Crew, Kayak, Canadian, Folding) 
  dat <- melt(poschartdata,id="Crew")
  ggplot(dat,aes(Crew,value, fill=variable))+geom_bar(stat="identity")+theme(axis.text.x=element_text(angle=90, hjust=1))+ labs(y = "Completions", x="Paddler")+ labs(title = "Chart by Boat type") + scale_y_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20,22,24))
  })

 
  
  
  output$Time <- renderPlot({poschartdata <- main_data %>% select(Year, Position, Class, DecimalTime) %>% 
    filter(Class %in% input$atttimeclass, Position == input$Position, Year >= input$attnyear)
  ggplot(poschartdata)+geom_line(aes(Year, DecimalTime, color = Class),  size = 3)+ scale_x_continuous(breaks=seq(0,2050,5))
  })
  
  output$WeatherYear <- renderPlot({poschartdata <- main_data %>% select(Year, Flow, DecimalTime)%>%
    filter(Year > 1970)

  ggplot(poschartdata,aes(Year, Flow))+ geom_point(aes(colour = cut(Flow, c(-Inf, 65, 100, 120, Inf))),
                                                           size = 5) +
                                                   scale_color_manual(name = "Flow",
                                                                     values = c("(-Inf,65]" = "green",
                                                                               "(65,100]" = "yellow",
                                                                               "(100,120]" = "orange",
                                                                            "(120, Inf]" = "red"),
                                                                  labels = c("<= 80", "> 80 <= 100", "> 100 < 120", "> 120"))
  })
  
 # ggplot(mtcars, aes(wt, mpg)) + 
  #geom_point(aes(colour = cut(qsec, c(-Inf, 17, 19, Inf))),
   #          size = 5) +
  #  scale_color_manual(name = "qsec",
   #                    values = c("(-Inf,17]" = "black",
    #                              "(17,19]" = "yellow",
     #                             "(19, Inf]" = "red"),
      #                 labels = c("<= 17", "17 < qsec <= 19", "> 19"))
  
  
  
  output$TrophyTime <- renderPlot({poschartdata <- main_data %>% select(Year, Class, DecimalTime, Trophies) %>% filter(Class %in% input$attTrophytimeclass,   str_detect(Trophies, input$attTrophies))
  ggplot(poschartdata, aes(fill = Trophies, x=Year, y=DecimalTime, color = Trophies)) + 
    geom_bar(stat="identity",  width = 0.2)+ scale_y_continuous(breaks=seq(0,70,2))+ scale_x_continuous(breaks=seq(0,2050,5))+coord_flip()
#geom_bar(stat="identity")+coord_flip()+ labs( y="Trophy Time") 
  
  })
  
  
#  output$Record <- renderPlot({attchartdata <- main_data %>% select(Record, Class, DecimalTime) %>% filter( Class %in% input$attRecordclass, Record != "", Record != "Veteran (155 years) 74 & 81")
 # ggplot(attchartdata, aes(fill = Record, x=Record, reorder(DecimalTime,-table(DecimalTime)[DecimalTime]), color = Record)) +
  #  geom_bar(stat="identity")+coord_flip()+ labs( y="Record Time") 
   #})
  
  
  
  
  
  output$Distribution <- renderPlot({poschartdata <- main_data %>% select(Year, Class, DecimalTime) %>% 
    filter(Year %in% input$attnyear2, Class %in% input$atttimeclass1)
  
  a <- ggplot(poschartdata, aes(DecimalTime,fill = Class)) +
    geom_histogram(binwidth=.5, position="dodge")+ ggtitle("Plot 1 - By Individual Class")
  
  b <- ggplot(poschartdata, aes(DecimalTime,fill = Class)) +
    geom_histogram(binwidth=.5)+ ggtitle("Plot 2 - By Combined Class")
  
  c <- ggplot(poschartdata, aes(x=Class,y=DecimalTime, fill = Class)) +
    geom_boxplot()+ ggtitle("Plot 3 - Box Plot Class - Mean as Diamond")+ stat_summary(fun.y=mean, geom="point", shape=23, size=4)+geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5)
  ggarrange(a, b, c, ncol = 3, nrow = 1)
  })
  
  
  
  output$WSDistribution <- renderPlot({poschartdata <- wsmain_data %>% select(Year, WSClass, WSRace, WSTime, WSDecimalTime) %>% 
    filter(Year %in% input$attnwsyear2, WSRace %in% input$attDrace, WSClass %in% input$attWSclass1)
  
  
  b <- ggplot(poschartdata, aes(WSDecimalTime,fill = WSClass)) +
    geom_histogram(binwidth=.2)+ ggtitle("Plot 1 - By Combined Class")
  
  c <- ggplot(poschartdata, aes(x=WSClass,y=WSDecimalTime, fill = WSClass)) +
    geom_boxplot()+ ggtitle("Plot 2 - Box Plot Class - Mean as Diamond")+ stat_summary(fun.y=mean, geom="point", shape=23, size=4)+geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5)
  ggarrange( b, c, ncol = 2, nrow = 1)
  })
  
  
  
  output$clubplot <- renderPlot({attchartdata <- main_data %>% select(Year, Class, Club1,C1) %>% filter(Year %in% input$attclubyear, Class %in% input$attclubclass, C1>24 & C1<900)
  ggplot(attchartdata,aes(x=reorder(Club1,-table(Club1)[Club1])))+geom_bar(aes(fill = Class),stat="count")+theme(axis.text.x=element_text(angle=90, hjust=1))+ labs(y = "Completions", x="Largest Clubs shown by Number of Completions") 
  
  }) 
 
 #Test
  output$Mattendance1 <- renderPlot({poschartdata <- main_data %>% 
    select(Year,Class)%>%
    filter(Class %in% input$att2class)
  ggplot(poschartdata, aes(Year)) +geom_bar(aes(fill = Class), stat = "count")+geom_text(stat='count', aes(label=..count..), vjust=-1)+ scale_x_continuous(breaks=seq(0,2050,5))
  })  
  
  output$Mattendance1N <- renderPlot({poschartdata <- main_data %>% 
    select(Year,Class)%>%
    filter(Class %in% input$att2class)
  ggplot(poschartdata, aes(Class)) +geom_bar(aes(fill = Class), stat = "count")+geom_text(stat='count', aes(label=..count..), vjust=-1)
  })  
  
  
  
  
  
  
  
  output$MattendanceL <- renderPlot({poschartdata <- main_data %>% 
    select(Year,Ladies,Class)%>%
    filter(Ladies %in% input$attLclass, Class %in% input$attL1class)
  ggplot(poschartdata, aes(Year)) +geom_bar(aes(fill = Ladies), stat = "count")+geom_text(stat='count', aes(label=..count..), vjust=-1)+ scale_x_continuous(breaks=seq(0,2050,5))
  })  
  
  output$MattendanceLS <- renderPlot({poschartdata <- main_data %>% 
    select(Year,Ladies,Class,Position)%>%
    filter(Ladies %in% input$attLclass, Class %in% input$attL1class)
#  ggplot(poschartdata, aes(Year)) +geom_bar(aes(fill = Ladies), stat = "count")+ scale_x_continuous(breaks=seq(0,2050,5))
  ggplot(poschartdata)+geom_point(aes(Year, Position, color = Ladies), position=position_jitter(width=0.1, height=0.1), size = 2)+ scale_y_continuous(breaks=seq(2,200,10))+ scale_x_continuous(breaks=seq(0,2050,5))+ scale_colour_manual(values = c("red", "green", "blue"))
  
  })  
  
  output$MattendanceM <- renderPlot({poschartdata <- main_data %>% 
    select(Year,Military,Class)%>%
    filter(Military %in% input$attMArm, Class %in% input$attMclass)
  ggplot(poschartdata, aes(Year)) +geom_bar(aes(fill = Military), stat = "count")+ geom_text(stat='count', aes(label=..count..), vjust=-1) +scale_x_continuous(breaks=seq(0,2050,5))
  }) 
  
  output$MattendanceMP <- renderPlot({poschartdata <- main_data %>% 
    select(Year,Military,Class,Position)%>%
    filter(Military %in% input$attMArm, Class %in% input$attMclass)
 # ggplot(poschartdata, aes(Year)) +geom_bar(aes(fill = Military), stat = "count")+ scale_x_continuous(breaks=seq(0,2050,5))
  ggplot(poschartdata)+geom_point(aes(Year, Position, color = Military), position=position_jitter(width=0.1, height=0.1), size = 2)+ scale_y_continuous(breaks=seq(2,200,10))+ scale_x_continuous(breaks=seq(0,2050,5))
  
  }) 
  
  
  output$MattendanceSL <- renderPlot({poschartdata <- main_data %>% 
    select(Year,Ladies,Class,SubClass)%>%
    filter(SubClass %in% input$attSubclass, Class %in% input$attL2class)
  ggplot(poschartdata, aes(Year)) +geom_histogram(aes(fill = SubClass), stat = "count", width = 0.5)+ scale_x_continuous(breaks=seq(0,2050,5))
  }) 
  
  output$MattendanceSLT <- renderPlot({poschartdata <- main_data %>% 
    select(Year,Ladies,Class,SubClass)%>%
    filter(SubClass %in% input$attSubclass, Class %in% input$attL2class)
  ggplot(poschartdata, aes(SubClass)) +geom_bar(aes(fill = SubClass), stat = "count")+geom_text(stat='count', aes(label=..count..), vjust=-1)
  })  
  
  
  output$MattendanceSLO <- renderPlot({poschartdata <- main_data %>% 
    select(Year,Veteran,Ladies,BoatType,Class,Position)%>%
    filter(Class %in% input$attL2classCL, Ladies %in% input$attSubclassSEX, BoatType %in% input$attSubclassBOAT)
  ggplot(poschartdata, aes(Year)) +geom_histogram(aes(fill = Veteran), stat = "count", width = 0.5)+ scale_x_continuous(breaks=seq(0,2050,5))
  }) 
  
  output$MattendanceSLOP <- renderPlot({poschartdata <- main_data %>% 
    select(Year,Veteran,Ladies,BoatType,Class,Position)%>%
    filter(Class %in% input$attL2classCL, Ladies %in% input$attSubclassSEX, BoatType %in% input$attSubclassBOAT)
  #ggplot(poschartdata, aes(Year)) +geom_histogram(aes(fill = Veteran), stat = "count", width = 0.5)+ scale_x_continuous(breaks=seq(0,2050,5))
  ggplot(poschartdata)+geom_point(aes(Year, Position, color = Veteran), position=position_jitter(width=0.1, height=0.1),size =2)+ scale_y_continuous(breaks=seq(2,200,10))+ scale_x_continuous(breaks=seq(0,2050,5))+ scale_colour_manual(values = c("red", "green", "blue", "darkgreen","orange","lightblue","yellow","magenta"))
  }) 
 
  output$Mattendance1BT <- renderPlot({poschartdata <- main_data %>% 
    select(Year,Class,BoatType)%>%
    filter(BoatType %in% input$att2boat)
  ggplot(poschartdata, aes(Year)) +geom_bar(aes(fill = Class), stat = "count")+ scale_x_continuous(breaks=seq(0,2050,5))
  })
  
  
  
  
  output$Wtable <- DT::renderDataTable({DT::datatable({data <- WMain_Data 
  },options = list(pageLength = 60), rownames = FALSE)
  })
  
  
  output$Etable <- DT::renderDataTable({DT::datatable({data <- WMMain_Data %>% 
    filter (Year == input$comattDWyear)
  },options = list(pageLength = 1, dom = 't'), rownames = FALSE,colnames = c('Year', 'Flow', 'Wind', 'Temp', 'Completion')  
  )})
  
 

  



  output$FGraphsR <- renderPlot({poschartdata <- WMain_Data %>% 
    select(Year, SeniorFlow, FirstSenior, MaxWind, MinTemperature, CompletionRate, Regression_D) %>% 
    filter(CompletionRate >= input$comrateR, MaxWind <= input$windgR, MinTemperature >= input$tempgR) 
  
  poschartdata$RegressionU <- poschartdata$Regression * as.numeric(input$WSDT)
  
  lm_eqn = function(SeniorFlow, Regression_D, poschartdata){
    m <- lm(Regression_D ~ SeniorFlow, poschartdata);
    eq <- substitute(italic(Regression_D) == a + italic(SeniorFlow) %*% b *","~~italic(r)^2~"="~r2,
                     list(a = format(unname(coef(m))[1], digits = 3), 
                          b = format(unname(coef(m))[2], digits = 3),
                          r2 = format(summary(m)$r.squared, digits = 3)))
    as.character(as.expression(eq));                 
  }
  
  
  
  ggplot(poschartdata,aes(x =SeniorFlow,y = RegressionU))+geom_point(color = "Red", size = 3)+stat_smooth(method = "lm")+
    geom_text( x = 80, y = 19, label = lm_eqn(SeniorFlow, Regression_D, poschartdata), color="black", size = 4, parse=TRUE)+
    geom_text( x = 80, y = 23, label = lm_eqn(SeniorFlow, Regression_D, poschartdata), color="black", size = 4, parse=TRUE)+
    geom_text( x = 80, y = 16, label = lm_eqn(SeniorFlow, Regression_D, poschartdata), color="black", size = 4, parse=TRUE)+
    geom_text( x = 80, y = 27, label = lm_eqn(SeniorFlow, Regression_D, poschartdata), color="black", size = 4, parse=TRUE)
  })
  
  
  output$FGraphs <- renderPlot({poschartdata <- WMain_Data %>% 
    select(Year, SeniorFlow, FirstSenior, MaxWind, MinTemperature, CompletionRate) %>% 
    filter(Year >= input$attnfyear, CompletionRate >= input$comrate, MaxWind <= input$windg, MinTemperature >= input$tempg)
  
  lm_eqn = function(SeniorFlow, FirstSenior, poschartdata){
    m <- lm(FirstSenior ~ SeniorFlow, poschartdata);
    eq <- substitute(italic(FirstSenior) == a + italic(SeniorFlow) %*% b *","~~italic(r)^2~"="~r2,
                     list(a = format(unname(coef(m))[1], digits = 3), 
                          b = format(unname(coef(m))[2], digits = 3),
                          r2 = format(summary(m)$r.squared, digits = 3)))
    as.character(as.expression(eq));                 
  }
  
  
  
  d <- ggplot(poschartdata,aes(x =SeniorFlow,y = FirstSenior))+geom_point(color = "Red")+stat_smooth(method = "lm")+
    geom_text( x = 80, y = 15.5, label = lm_eqn(SeniorFlow, FirstSenior, poschartdata), color="black", size = 4, parse=TRUE)
  
  
  # p1 <- d + geom_text(x = 15, y = 300, label = lm_eqn(df), parse = TRUE)
  
  z <-  ggplot(poschartdata)+
    geom_point(aes(SeniorFlow,FirstSenior, color = MaxWind), size = 3)+scale_colour_gradientn(colours=rainbow(4))
  
  
  y <-  ggplot(poschartdata)+
    geom_point(aes(SeniorFlow,FirstSenior, color = MinTemperature), size = 3)+scale_colour_gradientn(colours=rainbow(4))
  
  
  ggarrange(d,z,y,ncol = 3, nrow = 1)})
  
  
  
  output$Graphs <- renderPlot({poschartdata <- WMain_Data %>% 
    select(Year, SeniorFlow, FirstSenior, MaxWind, MinTemperature, CompletionRate)  
  
  a <-  ggplot(poschartdata)+
    geom_point(aes(FirstSenior,SeniorFlow, color = MaxWind), size = 3)+scale_colour_gradientn(colours=rainbow(4))+ ggtitle("Plot 1 - Flow is most important for Time")
  b <-  ggplot(poschartdata)+
    geom_point(aes(FirstSenior, MaxWind, color = SeniorFlow),  size = 3)+scale_colour_gradientn(colours=rainbow(4))+ ggtitle("Plot 2 - Wind direction can be critical")
  c <-  ggplot(poschartdata)+
    geom_point(aes(FirstSenior, MinTemperature, color = MaxWind),  size = 3)+scale_colour_gradientn(colours=rainbow(4))+ ggtitle("Plot 3 - Times not related to Min Temperature")
  
  d <-  ggplot(poschartdata)+
    geom_point(aes(CompletionRate, SeniorFlow, color = MaxWind),  size = 3)+scale_colour_gradientn(colours=rainbow(4))+ ggtitle("Plot 4 - Completion Rate not related to Flow")
  e <-  ggplot(poschartdata)+
    geom_point(aes(CompletionRate, MaxWind, color = MinTemperature),  size = 3)+scale_colour_gradientn(colours=rainbow(4))+ ggtitle("Plot 5 - Completion related to Max head Wind")
  f <-  ggplot(poschartdata)+
    geom_point(aes(CompletionRate, MinTemperature, color = MaxWind),  size = 3)+scale_colour_gradientn(colours=rainbow(4))+ ggtitle("Plot 6 - Completion sometimes related to Min Temp")
  g <-  ggplot(poschartdata)+
    geom_point(aes(MaxWind, MinTemperature, color = Year),  size = 3)+scale_colour_gradientn(colours=rainbow(4))+ ggtitle("Plot 7 - High temperatures related to low wind ??")
  
  
  ggarrange(a,b,c,ncol = 3, nrow = 1)})
  
  output$GraphsTP <- renderPlot({poschartdata <- main_data %>% 
    select(Year, DecimalTime, Position,Class) %>% filter(Class %in% input$attclassTP, Year %in% input$attyearTP, Position > 0)
#    select(Year, DecimalTime, Position,Class) %>% filter(Class %in% input$attclassTP, Year %in% input$attyearTP, Position > 0)
 ggplot(poschartdata)+geom_point(aes(x = Position, y = DecimalTime, color = Year), size = 2)+scale_colour_gradientn(colours=rainbow(4)) +  scale_x_continuous(breaks=seq(0,200,10))+ ggtitle("Showing all Places")
  })
 
  output$GraphsTPL <- renderPlot({poschartdata <- main_data %>% 
    select(Year, DecimalTime, Position,Class) %>% filter(Class %in% input$attclassTP, Year %in% input$attyearTP, Position > 0, Position <51)
  #    select(Year, DecimalTime, Position,Class) %>% filter(Class %in% input$attclassTP, Year %in% input$attyearTP, Position > 0)
  ggplot(poschartdata)+geom_point(aes(x = Position, y = DecimalTime, color = Year), size = 2)+scale_colour_gradientn(colours=rainbow(4)) +  scale_y_continuous(breaks=seq(0,50,2))+  scale_x_continuous(breaks=seq(0,200,10))+ ggtitle("Showing 1st 50 Places")
  })
  #Year >= input$attnyear
  
  
  output$WeatherBox <- renderPlot({poschartdata <- WMain_Data %>% select(Year, SeniorFlow, MaxWind, MinTemperature, CompletionRate) 
  
  a <- ggplot(poschartdata, aes(x= 0,y=SeniorFlow, fill = Year))+
    geom_boxplot()+ ggtitle("Plot 1 - Box Plot Senior Flow - Mean as Diamond")+ stat_summary(fun.y=mean, geom="point", shape=23, size=4)+geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5)
  
  b <- ggplot(poschartdata, aes(x= 0,y=MaxWind, fill = Year))+
    geom_boxplot()+ ggtitle("Plot 2 - Box Plot Max Wind Flow - Mean as Diamond")+ stat_summary(fun.y=mean, geom="point", shape=23, size=4)+geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5)
  
  c <- ggplot(poschartdata, aes(x= 0,y=MinTemperature, fill = Year))+
    geom_boxplot()+ ggtitle("Plot 3 - Box Plot Min Temperature - Mean as Diamond")+ stat_summary(fun.y=mean, geom="point", shape=23, size=4)+geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5)
  
  d <- ggplot(poschartdata, aes(x= 0,y=CompletionRate, fill = Year))+
    geom_boxplot()+ ggtitle("Plot 4 - Box Plot Completion Rate - Mean as Diamond")+ stat_summary(fun.y=mean, geom="point", shape=23, size=4)+geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5)
  
  ggarrange(a,b,c,d, ncol = 4, nrow = 1)
  })
  
  output$WCompGraphs <- renderPlot({poschartdata <- Comparison_data %>% 
    select(Year, WatersideA, WatersideB, WatersideC,WatersideD) %>%
    filter(Year == input$comattDWyear, WatersideA <= input$MaxA)
  
  na.test <-  function (WatersideB) {
    if (colSums(!is.na(WatersideB) == 0)){
      stop ("The some variable in the dataset has all missing value, remove the column to proceed")
    }
  }
  #lm_eqn = function(WatersideA, DWSenior, poschartdata){
  # m <- lm(DWSenior ~ 0 + WatersideA, poschartdata);
  #eq <- substitute(italic(DWSenior) == italic(WatersideA) %.% a,
  #                list(a = format(coef(m)[1], digits = 3), 
  #                    b = format(coef(m)[2], digits = 3)))
  #  as.character(as.expression(eq));                 
  #}
  
  
  lm_eqn = function(WatersideA, WatersideB, poschartdata){
    m <- lm(WatersideB ~ 0 + WatersideA, poschartdata);
    eq <- substitute(italic(WatersideB) == italic(WatersideA) %*% a,
                     list(a = format(unname(coef(m))[1], digits = 3), 
                          b = format(unname(coef(m))[2], digits = 3),
                          r2 = format(summary(m)$r.squared, digits = 3)))
    as.character(as.expression(eq));                 
  }
  
  b <- ggplot(poschartdata,aes(x =WatersideA,y = WatersideB))+geom_point(color="red")+stat_smooth(method = "lm")+
    annotate("text", x = 2.0,y = 4.0, label = lm_eqn(poschartdata$watersideA, poschartdata$WatersideB, poschartdata), color="black", size = 4, parse=TRUE)
  
  na.test <-  function (WatersideC) {
    if (colSums(!is.na(WatersideC) == 0)){
      stop ("The some variable in the dataset has all missing value, remove the column to proceed")
    }
  }
  #na.test (WatersideC)
  
  
  lm_eqn = function(WatersideA, WatersideC, poschartdata){
    m <- lm(WatersideC ~ 0 + WatersideA, poschartdata);
    eq <- substitute(italic(WatersideC) == italic(WatersideA) %*% a,
                     list(a = format(unname(coef(m))[1], digits = 3), 
                          b = format(unname(coef(m))[2], digits = 3),
                          r2 = format(summary(m)$r.squared, digits = 3)))
    as.character(as.expression(eq));                 
  }
  
  c <- ggplot(poschartdata,aes(x =WatersideA,y = WatersideC))+geom_point(color="red")+stat_smooth(method = "lm")+
    annotate("text", x = 2.0,y = 4.5, label = lm_eqn(poschartdata$watersideA, poschartdata$WatersideC, poschartdata), color="black", size = 4, parse=TRUE)
  # geom_text(label = lm_eqn(poschartdata$watersideA, poschartdata$WatersideC, poschartdata), color="black", size = 5, parse=TRUE)
  
  na.test <-  function (WatersideD) {
    if (colSums(!is.na(WatersideD) == 0)){
      stop ("The some variable in the dataset has all missing value, remove the column to proceed")
    }
  }
  #na.test (WatersideD)
  
  
  lm_eqn = function(WatersideA, WatersideD, poschartdata){
    m <- lm(WatersideD ~ 0 + WatersideA, poschartdata);
    eq <- substitute(italic(WatersideD) == italic(WatersideA) %*% a,
                     list(a = format(unname(coef(m))[1], digits = 3), 
                          b = format(unname(coef(m))[2], digits = 3),
                          r2 = format(summary(m)$r.squared, digits = 3)))
    as.character(as.expression(eq));                
  }
  
  d <- ggplot(poschartdata,aes(x =WatersideA,y = WatersideD))+geom_point(color="red")+stat_smooth(method = "lm")+
    annotate("text", x = 2.0, y = 7.0, label = lm_eqn(poschartdata$watersideA, poschartdata$WatersideD, poschartdata), color="black", size = 4, parse=TRUE)
  
  a <-  ggplot(poschartdata)+
    geom_point(aes(x=WatersideA, y=WatersideB, color= "WatersideB"),  size = 2)+
    geom_point(aes(WatersideA, WatersideC, color= "WatersideC"),  size = 2)+
    geom_point(aes(WatersideA, WatersideD, color= "WatersideD"),  size = 2)
  
  
  ggarrange(a,b,c,d,ncol = 4, nrow = 1)
  
  })
  
  
  
  #################################################################
  
  
  output$CompGraphs <- renderPlot({poschartdata <- Comparison_data %>% 
    select(Year, WatersideA, WatersideB, WatersideC,WatersideD,DWSenior, DWJunior, DWEndeavour, DWVetJunior, DWSingles) %>%
    filter(Year == input$comattDWyear)
  
  b <-  ggplot(poschartdata)+
    geom_point(aes(WatersideD, DWSenior, color= "DWSenior"),  size = 2)+ 
    geom_point(aes(WatersideD, DWJunior, color= "DWJunior"),  size = 2)+ 
    geom_point(aes(WatersideD, DWEndeavour, color= "DWEndeavour"),  size = 2)+ 
    geom_point(aes(WatersideD, DWVetJunior, color= "DWVetJunior"),  size = 2)+ 
    geom_point(aes(WatersideD, DWSingles, color= "DWSingles"),  size = 2)
  
  c <- ggplot(poschartdata, aes(x = WatersideA, y = DWSenior)) + 
    geom_point(aes(x = WatersideA, y = DWSenior)) +
    stat_smooth(method = "lm", col = "red")
  
  lm_eqn = function(WatersideA, DWSenior, poschartdata){
    m <- lm(DWSenior ~ 0 + WatersideA, poschartdata);
    eq <- substitute(italic(DWSenior) == italic(WatersideA) %*% a,
                     list(a = format(unname(coef(m))[1], digits = 3), 
                          b = format(unname(coef(m))[2], digits = 3)))
    as.character(as.expression(eq));                 
  }
  
  
  d <- ggplot(poschartdata,aes(x =WatersideA,y = DWSenior))+geom_point(color="red")+stat_smooth(method = "lm")+
    annotate("text", x = 2.7, y = 35, label = lm_eqn(poschartdata$watersideA, poschartdata$DWSenior, poschartdata), color="black", size = 4, parse=TRUE)
  
  lm_eqn = function(WatersideD, DWSenior, poschartdata){
    m <- lm(DWSenior ~ 0 + WatersideD, poschartdata);
    eq <- substitute(italic(DWSenior) == italic(WatersideD)  %*% a,
                     list(a = format(unname(coef(m))[1], digits = 3), 
                          b = format(unname(coef(m))[2], digits = 3)))
    as.character(as.expression(eq));                 
  }
  
  e <- ggplot(poschartdata,aes(x = WatersideD, y = DWSenior))+geom_point(color="red")+geom_smooth(method = "lm")+
    annotate("text", x = 7, y = 35, label = lm_eqn(poschartdata$DWSenior, poschartdata$watersideB, poschartdata), color="black", size = 4, parse=TRUE)
  
  
  lm_eqn = function(WatersideB, DWSenior, poschartdata){
    m <- lm(DWSenior ~ 0 + WatersideB, poschartdata);
    eq <- substitute(italic(DWSenior) == italic(WatersideB)  %*% a,
                     list(a = format(unname(coef(m))[1], digits = 3), 
                          b = format(unname(coef(m))[2], digits = 3)))
    as.character(as.expression(eq));                 
  }
  
  g <- ggplot(poschartdata,aes(x = WatersideB, y = DWSenior))+geom_point(color="red")+geom_smooth(method = "lm")+
    annotate("text", x = 3, y = 35, label = lm_eqn(poschartdata$DWSenior, poschartdata$watersideB, poschartdata), color="black", size = 4, parse=TRUE)
  
  
  lm_eqn = function(WatersideC, DWSenior, poschartdata){
    m <- lm(DWSenior ~ 0 + WatersideC, poschartdata);
    eq <- substitute(italic(DWSenior) == italic(WatersideC)  %*% a,
                     list(a = format(unname(coef(m))[1], digits = 3), 
                          b = format(unname(coef(m))[2], digits = 3)))
    as.character(as.expression(eq));                 
  }
  
  h <- ggplot(poschartdata,aes(x = WatersideC, y = DWSenior))+geom_point(color="red")+geom_smooth(method = "lm")+
    annotate("text", x = 4, y = 35, label = lm_eqn(poschartdata$DWSenior, poschartdata$watersideC, poschartdata), color="black", size = 4, parse=TRUE)
  
  
  
  lm_eqn = function(WatersideD, DWJunior, poschartdata){
    m <- lm(DWJunior ~ 0 + WatersideD, poschartdata);
    eq <- substitute(italic(DWJunior) == italic(WatersideD) %*% a,
                     list(a = format(unname(coef(m))[1], digits = 3), 
                          b = format(unname(coef(m))[2], digits = 3)))
    as.character(as.expression(eq));                 
  }
  
  
  
  f <- ggplot(poschartdata,aes(x = WatersideD, y = DWJunior))+geom_point(color="red")+geom_smooth(method = "lm")+
    annotate("text", x = 7, y = 32, label = lm_eqn(poschartdata$DWJunior, poschartdata$watersideB, poschartdata), color="black", size = 4, parse=TRUE)
  
  
  # g <- DT::renderDataTable({DT::datatable({data <- WMain_Data %>% 
  #  filter (Year %in% input$EYear)
  #  },options = list(pageLength = 1, dom = 't'), rownames = FALSE  
  # )})
  
  
  
  ggarrange(d,g,h,e,b,f, ncol = 2, nrow = 3)
  
  })
  
  
  output$WSPositions <- renderPlot({poschartdata <- wsmain_data %>% select(Year, WSPosition, WSClass, WSName2, WSTime, WSDecimalTime, WSRace) %>%filter(str_detect(WSName2,input$WSpaddler),  WSRace %in% input$WSPRace)
  a <- ggplot(poschartdata)+geom_point(aes(Year, WSPosition, color = WSClass), position=position_jitter(width=0.1, height=0.1), size = 3)+ scale_y_continuous(breaks=seq(2,200,2))+ scale_x_continuous(breaks=seq(0,2050,5))
  b <- ggplot(poschartdata)+geom_point(aes(Year, WSDecimalTime, color = WSClass), position=position_jitter(width=0.1, height=0.1), size = 3) + scale_y_continuous(breaks=seq(1,200,0.2))+ scale_x_continuous(breaks=seq(0,2050,5))
  ggarrange(a, b, ncol = 2, nrow = 1)
  })
  
  
  
  
  
  
  output$CompletionR <- renderPlot({poschartdata <- WMain_Data %>% 
    select(Year, SeniorFlow, FirstSenior, MaxWind, MinTemperature, CompletionRate)  
  
  
  
  ggplot(poschartdata, aes(Year,CompletionRate,fill = CompletionRate)) +geom_bar(stat = "identity")  + scale_colour_gradientn(colours=rainbow(4)) })
  
  output$CompletionR1 <- renderPlot({poschartdata <- WMain_Data %>% 
    select(Year, SeniorFlow, FirstSenior, MaxWind, MinTemperature, CompletionRate)  
  
  
  d <-  ggplot(poschartdata)+
    geom_point(aes(CompletionRate, SeniorFlow, color = MaxWind),  size = 3)+scale_colour_gradientn(colours=rainbow(4))+ ggtitle("Plot 4 - Completion Rate not related to Flow")
  e <-  ggplot(poschartdata)+
    geom_point(aes(CompletionRate, MaxWind, color = MinTemperature),  size = 3)+scale_colour_gradientn(colours=rainbow(4))+ ggtitle("Plot 5 - Completion related to Max head Wind")
  f <-  ggplot(poschartdata)+
    geom_point(aes(CompletionRate, MinTemperature, color = MaxWind),  size = 3)+scale_colour_gradientn(colours=rainbow(4))+ ggtitle("Plot 6 - Completion sometimes related to Min Temp")
  ggarrange(d,e,f,ncol = 3, nrow = 1)})
  
  
  
  #i.e. render this dataset as a table
  output$wstable <- DT::renderDataTable({DT::datatable({data <- wstable_Data},options = list(pageLength = 15), rownames = FALSE)})
  
  
  
}

shinyApp(ui, server)