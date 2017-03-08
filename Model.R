require(boot)
library(readxl) 
Data_16 <- read_excel("~/Desktop/Jays Season Tickets/2016_Season Feb 19.xlsx", 
                      sheet = "2016-combined (1)", col_types = c("numeric", 
                                                                 "text", "text", "numeric", "text", 
                                                                 "numeric", "text", "text", "numeric", 
                                                                 "numeric", "numeric", "text", "numeric", 
                                                                 "numeric", "text", "text", "numeric"))

##Sec needs to the chr type due to subsections having A,B,C,D where we sit.

S17 <- read_excel("~/Desktop/Jays Season Tickets/2017Schedule.xlsx") ##2017 schedule
S17$Opp[S17$Opp=="Red Sex"]<-"Red Sox"

po16<-SS_Att[(SS_Att$Rk==2016),] ##Pull the 2016 playoff odds

##Don't have game number for the season 2016 data to match the playoff odds with the correct time...

our.seats<-subset(Data_16,Sec=="113A"|Sec=="130A"&rows>2&rows<15)

our.seats$price<-our.seats$displayPricePerTicketAmount
our.seats$f.day<-factor(our.seats$`Day of Week`)
our.seats$f.opp<-factor(our.seats$Opponent)
our.seats$f.pitch<-factor(our.seats$`First Pitch`)
our.seats$promo<-our.seats$Promo

model<-lm(price~f.day+f.opp+promo+f.pitch,data=our.seats)
summary(model)

boot.rx <- function(data,i) {coef(lm(price~f.day+f.opp+f.pitch+f.promo,subset = i, data=data))}
output.rx <- boot(our.seats, boot.rx, 999)
output.rx

output<-predict(model,S17)
plot(output,type="l",xaxt="n",col=3)
axis(1, at = seq(1, 81, by = 2), las=2)
lines(1:81,S17$J.Price,lty=2,col=2)
lines(1:81,model_feb20_2017$`ModeledPrice(USD)`,lty=3,col=3)
abline(45,0)

plot(change$game,change$diff,type="l")
abline(0,0)

require(fpp)
ari<-auto.arima(S17$J.Price)


S17$f.opp[S17$f.opp=="Braces"|S17$f.opp=="Brewers"|S17$f.opp=="Cubs"|S17$f.opp=="Pirates"|S17$f.opp=="Red Sex"|S17$f.opp=="Reds"]<-"Angels"
##I misspelled some of the team names that is why they are misspelled above. NEED TO UPDATE
