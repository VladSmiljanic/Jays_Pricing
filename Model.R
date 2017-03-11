library(readxl) 
library(readr)
library(stringr)
Data_16 <- read_excel("~/Desktop/Jays Season Tickets/2016_Season Feb 19.xlsx", 
                      sheet = "2016-combined (1)", col_types = c("numeric", 
                                                                 "text", "text", "numeric", "text", 
                                                                 "numeric", "text", "text", "numeric", 
                                                                 "numeric", "numeric", "text", "numeric", 
                                                                 "numeric", "text", "text", "numeric"))

##Sec needsto the chr type due to subsections having A,B,C,D where we sit.

PO_16 <- read_csv("~/Desktop/Jays Season Tickets/2016.csv") ##2016 playoff odds for Jays

##Problem is that the playoff data goes by date and doesn't go by game played. 
##It does keep track of the record, so we if a dates record increase by one in W or L then they played game.
##Create variables that is a sum of W and L and delete row if it didn't go up from previous. Loop?
## Problem 2: we need to get rid of away games from the list. Use our stub hub data to subtract away games 
##Specifically by keeping home games. Attendance from 2016 has the dates adjusted from previously.

df.Att <- read_excel("~/Desktop/Jays Season Tickets/Season-by-season Attendance.xls")
df.Att<-df.Att[(df.Att$Rk==2016),] ##Only 2016
d<-data.frame(str_split_fixed(df.Att$Date, " ", 3))
df.Att$date<-paste(d$X2,d$X3) ##Form new data with Month and Day

away.games<-complete.cases(df.Att)

##NEED THE DELETE AWAY GAMES....tired left it here

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

##Need adjust the date of game by the game number it is played so to match our playoff odds with it.
"Date of Game"<-levels(factor(Data_16$`Date of Game`))
game.num<-order(`Date of Game`)
order.data<-data.frame(`Date of Game`,game.num)
names(order.data)[names(order.data)=="Date.of.Game"]<-"Date of Game"
Data_16<-merge(Data_16,order.data,by="Date of Game")

PO_16$game.num<-PO_16$W+PO_16$L

##Find all of the rows that need to be deleted from playoff odds due to no games played on those dates.
##Delete first due to zero games played and it will track game number for odds being inputted.
rows.to.delete<-c(1)
t<-2
for(i in 2:length(PO_16$YEAR_ID)){
  if (PO_16$game.num[i]==PO_16$game.num[i-1]){
    rows.to.delete[t]<-i
    t<-t+1
  }
}

PO_16[-rows.to.delete,]

##Merge playoff odds with stub hub data according to game.num

Data_16<-merge(Data_16,PO_16,by="game.num",x.all=TRUE)


S17 <- read_excel("~/Desktop/Jays Season Tickets/2017Schedule.xlsx") ##2017 schedule
S17$Opp[S17$Opp=="Red Sex"]<-"Red Sox"

##Don't have game number for the season 2016 data to match the playoff odds with the correct time...

our.seats<-subset(Data_16,Sec=="113A"|Sec=="130A"&rows>2&rows<15)

our.seats$price<-our.seats$displayPricePerTicketAmount
our.seats$f.day<-factor(our.seats$`Day of Week`)
our.seats$f.opp<-factor(our.seats$Opponent)
our.seats$f.pitch<-factor(our.seats$`First Pitch`)
our.seats$promo<-our.seats$Promo


output<-predict(model,S17)
plot(output,type="l",xaxt="n",col=3)
axis(1, at = seq(1, 81, by = 2), las=2)
lines(1:81,S17$J.Price,lty=2,col=2)
lines(1:81,model_feb20_2017$`ModeledPrice(USD)`,lty=3,col=3)
abline(45,0)

S17$f.opp[S17$f.opp=="Braces"|S17$f.opp=="Brewers"|S17$f.opp=="Cubs"|S17$f.opp=="Pirates"|S17$f.opp=="Red Sex"|S17$f.opp=="Reds"]<-"Angels"
##I misspelled some of the team names that is why they are misspelled above. NEED TO UPDATE
