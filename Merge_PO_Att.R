rm(SS_Att)

library(readxl)
library(stringr)
SS_Att <- read_excel("~/Desktop/Jays Season Tickets/Season-by-season Attendance.xls")

SS_Att<-SS_Att[!(SS_Att$Rk<2013),]
SS_Att$date<-paste(SS_Att$Date,SS_Att$Rk)

d<-data.frame(str_split_fixed(SS_Att$date, " ", 3))
SS_Att$date<-paste(d$X2,d$X3,d$X4) ##Form new data with Month and Day

X2016$`YEAR_ID`<-as.integer(2016) ##Add column that is missing for rbind later

SS_Att<-SS_Att[!complete.cases(SS_Att),] ##Eliminates the away games from att

PO_Odds<-do.call("rbind",list(X2013,X2014,X2015,X2016)) ##Forms one data.frame for all of the playoff odds
  
PO_Odds$Month<-as.character(PO_Odds$MONTH) ##Converts the month number to chr for matching
PO_Odds$Month[PO_Odds$Month=="3"]<-"May"
PO_Odds$Month[PO_Odds$Month=="4"]<-"Apr"
PO_Odds$Month[PO_Odds$Month=="5"]<-"May"
PO_Odds$Month[PO_Odds$Month=="6"]<-"Jun"
PO_Odds$Month[PO_Odds$Month=="7"]<-"Jul"
PO_Odds$Month[PO_Odds$Month=="8"]<-"Aug"
PO_Odds$Month[PO_Odds$Month=="9"]<-"Sep"
PO_Odds$Month[PO_Odds$Month=="10"]<-"Oct"

PO_Odds$date<-paste(PO_Odds$Month,PO_Odds$DAY,PO_Odds$YEAR_ID)##Formats date

SS_Att<-merge(SS_Att,PO_Odds,by="date") ##Merge playoff odds with the corresponding dates of attendance
