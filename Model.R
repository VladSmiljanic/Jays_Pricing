library(readxl) 
library(readr)
library(stringr)
Data_16 <- read_excel("~/Desktop/Jays Season Tickets/2016_Season Feb 19.xlsx", 
                      sheet = "2016-combined (1)", col_types = c("numeric", 
                                                                 "text", "text", "numeric", "text", 
                                                                 "numeric", "text", "text", "numeric", 
                                                                 "numeric", "numeric", "text", "numeric", 
                                                                 "numeric", "text", "text", "numeric"))

##Sec needs to be chr type due to subsections having A,B,C,D where we sit.

PO_16 <- read_csv("~/Desktop/Jays Season Tickets/2016.csv") ##2016 playoff odds for Jays

##Problem is that the playoff data goes by date and doesn't go by game played. 
## Problem 2: we need to get rid of away games from the list. Use our stub hub data to subtract away games 
##Specifically by keeping home games. Attendance from 2016 has the dates adjusted from previously.

df.Att <- read_excel("~/Desktop/Jays Season Tickets/Season-by-season Attendance.xls")
df.Att<-df.Att[(df.Att$Rk==2016),] ##Only 2016
d<-data.frame(str_split_fixed(df.Att$Date, " ", 3))
df.Att$date<-paste(d$X2,d$X3) ##Form new data with Month and Day

##Now we got to change the months around for odds...ugh I should have just done this in excel...

PO_16$Month<-as.character(PO_16$MONTH) ##Converts the month number to chr for matching
PO_16$Month[PO_16$Month=="3"]<-"May"
PO_16$Month[PO_16$Month=="4"]<-"Apr"
PO_16$Month[PO_16$Month=="5"]<-"May"
PO_16$Month[PO_16$Month=="6"]<-"Jun"
PO_16$Month[PO_16$Month=="7"]<-"Jul"
PO_16$Month[PO_16$Month=="8"]<-"Aug"
PO_16$Month[PO_16$Month=="9"]<-"Sep"
PO_16$Month[PO_16$Month=="10"]<-"Oct"

PO_16$date<-paste(PO_16$Month,PO_16$DAY)##Formats date
PO_16<-merge(PO_16,df.Att,by="date") ##Merge playoff odds with the corresponding dates of attendance

##NEED THE DELETE AWAY GAMES FROM PLAYOFF ODDS....I think this works. left with 81 observations
## Also solved other problems I was having.
home.games<-complete.cases(PO_16$`H/A`)
PO_16<-PO_16[!home.games,]

##Need adjust the date of game by the game number it is played so to match our playoff odds with it.
d.g<-levels(factor(Data_16$`Date of Game`))
game.num<-rank(d.g)
order.data<-data.frame(d.g,game.num)
names(order.data)[names(order.data)=="d.g"]<-"Date of Game"
Data_16<-merge(Data_16,order.data,by="Date of Game")

##Need to update game number in PO_16 to reflect order of home games

PO_16$game.num<-rank(PO_16$`Gm#`)

##Merge playoff odds with stub hub data according to game.num

Data_16<-merge(Data_16,PO_16,by="game.num",x.all=TRUE)

## Now we have our data with the playoff odds information in it. We need to add promos from the other sheet now as well.
## Make dummy variables for the home opener, canada day, jersey giveaway and others. 

Data_16$HO<-ifelse(Data_16$game.num %in% c(1,2),1,0) #First game
Data_16$CD<-ifelse(Data_16$date %in% c("Jun 30","Jul 1"),1,0) ## Canada day
Data_16$Ga1<-ifelse(Data_16$date %in% c("May 7","Jun 21","Jul 10","Aug 28","Sept 24"),1,0) ## Bigger promos
Data_16$Ga2<-ifelse(Data_16$date %in% c("Apr 23","May 5","May 25","May 29","June 11","June 14","Jul 7","Jul 24","Jul 30","Aug 13"),1,0) ## Bigger promos

S17 <- read_excel("~/Desktop/Jays Season Tickets/2017Schedule.xlsx") ##2017 schedule
S17$Opp[S17$Opp=="Red Sex"]<-"Red Sox"

##Now to add important games for 2017 season
S17$HO<-ifelse(S17$Game==1,1,0) #First game
S17$CD<-ifelse(S17$Date %in% c("Sat, 7/1","Sun, 7/2"),1,0) ## Canada day
S17$Ga1<-ifelse(S17$Date %in% c("Sun, 7/30","Fri, 6/30"),1,0) ## Bigger promos
S17$Ga2<-ifelse(S17$Date %in% c("Sun, 6/4","Sun, 7/9","Sun, 5/14","Sun, 4/16","Sun, 5/28"),1,0) ## Bigger promos

our.seats<-subset(Data_16,Sec=="113A"|Sec=="130A" & rows>3 & rows<7)

our.seats$price<-our.seats$displayPricePerTicketAmount
our.seats$f.day<-factor(our.seats$`Day of Week`)

##We find that red sox and yankees are the only teams that matter. Set yanks, red sox and other for opponents
our.seats$opp<-ifelse(our.seats$Opponent %in% c("Red Sox","Yankees"),1,0)

##First pitch I'll make some dummies for as well
times<-levels(factor(our.seats$`First Pitch`))
our.seats$L<-ifelse(our.seats$`First Pitch`==times[1],1,0) ##Lunch Start
our.seats$LL<-ifelse(our.seats$`First Pitch`==times[2],1,0) ##Late Lunch Start
our.seats$A<-ifelse(our.seats$`First Pitch`==times[3],1,0) ##Afternoon
our.seats$E<-ifelse(our.seats$`First Pitch`==times[4],1,0) ##Evening

#Adjust 2017 data accordingly
#Need to add start times as dummies
times<-levels(factor(S17$Time))
S17$L<-ifelse(S17$Time==times[2],1,0) ##Lunch Start
S17$LL<-ifelse(S17$Time==times[1],1,0)  ##Late Lunch Start
S17$A<-ifelse(S17$Time==times[3],1,0)  ##Afternoon
S17$E<-ifelse(S17$Time==times[4],1,0)  ##Evening

d<-data.frame(str_split_fixed(S17$Date, "/", 2))
d<-data.frame(str_split_fixed(d$X1, " ", 2))
S17$Month<-d$X2

S17$f.day<-factor(S17$Day)
our.seats$Month<-factor(our.seats$MONTH)
S17$opp<-ifelse(S17$Opp %in% c("Red Sox","Yankees"),1,0)

model<-lm(price~HO+CD+Ga1+Ga2+L+LL+A+E+Month+PLAYOFF_RT,data=our.seats)
summary(model)

S17$PLAYOFF_RT<-0.48#Using fangraphs playoff odds
output<-predict(model,S17)
par(mfrow=c(1,1))
plot(output,type="l")
lines(S17$Price,lty=3)
abline(50,0,col=2)
sum(output-50)
#What if we used last years playoff odds into this model?

test<-merge(x=S17,y=PO_16[,c("game.num","PLAYOFF_RT")],by.x="Game",by.y ="game.num",all.x=TRUE)#Using fangraphs playoff odds
S17$PLAYOFF_RT<-test$PLAYOFF_RT.y
output<-predict(model,S17)
sum(output-50)
plot(output,type="l")
lines(S17$Price,lty=3)
abline(50,0,col=2)

#With johnny's model

plot(J_Pricing$J.Price,type="l")
lines(output,col=3)
abline(50,0,col=2)
sum(J_Pricing$J.Price-50)
