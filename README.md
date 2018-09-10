# One-Day-International-cricket-data
player wise, countery wise and match wise details of ODI from 1971 to 2010.

########################    C O D E S

odi=read.csv('c:/Users/Administrator/Documents/odi-batting.csv')
View(odi)
hr=read.csv('C:/Users/Administrator/Documents/HR Analytics.csv')
View(hr)

#1. unique jobrole
jobrole=c()
for (i in hr$JobRole) {
  if(i%in%jobrole){
    
  }
  else{
    jobrole=c(jobrole,i)
  }
}
print(jobrole)

#1. no of matches Sachin played using pipeline
nrow(odi%>%filter(Player=='Sachin R Tendulkar'))

#2.filering more then one item in a column
ip=c('Sachin R Tendulkar','Ashish Nehra','Virender Sehwag')
nrow(odi%>%filter(odi$Player%in%ip))

#3. how many times did sachin played aainst australiya
nrow(odi%>%filter(Player=='Sachin R Tendulkar',Versus=='Australia'))

#4.sachin vs australia with century
nrow(odi%>%filter(Player=='Sachin R Tendulkar',Versus=='Australia',Runs>99,Runs<200))

#5.sachin vcs ausi half century
nrow(odi%>%filter(Player=='Sachin R Tendulkar',Versus=='Australia',Runs>50,Runs<100))



######################## G R O U P I N G############################

#6. compute player wise total runs
odi%>%group_by(Player)%>%summarise(total_runs=sum(Runs))

#7. compute player wise total runs and identify top 10 players
odi%>%group_by(Player)%>%summarise(total_runs=sum(Runs))%>%arrange(-total_runs)%>%head(10)

#8.  compute player wise total runs and identify bottom 10 players
odi%>%group_by(Player)%>%summarise(total_runs=sum(Runs))%>%arrange(-total_runs)%>%tail(10)

#9. compute player wise total runs and identify top&bottom 10 players of indian player
odi%>%filter(Country=='India')%>%group_by(Player)%>%summarise(total_run=sum(Runs))%>%arrange(-total_run)%>%head(10)
odi%>%filter(Country=='India')%>%group_by(Player)%>%summarise(total_run=sum(Runs))%>%arrange(total_run)%>%head(10)

#10. compute player wise total runs and identify top&bottom 10 players of indian player versus australia
odi%>%filter(Country=='India',Versus=='Australia')%>%group_by(Player)%>%summarise(total_runs=sum(Runs))%>%arrange(-total_runs)%>%head(10)
odi%>%filter(Country=='India',Versus=='Australia')%>%group_by(Player)%>%summarise(total_runs=sum(Runs))%>%arrange(total_runs)%>%head(10)

#11. top 10 players according to no of matches
odi%>%group_by(Player)%>%
  summarise(total_match=n())%>%arrange(total_match)%>%head(10)



############################################################################################

library(hflights)
View(hflights)

carrier=hflights$UniqueCarrier
carrier
lut=c("AA" = "American", "AS" = "Alaska", "B6" = "JetBlue", "CO" = "Continental", 
      "DL" = "Delta", "OO" = "SkyWest", "UA" = "United", "US" = "US_Airways", 
      "WN" = "Southwest", "EV" = "Atlantic_Southeast", "F9" = "Frontier", 
      "FL" = "AirTran", "MQ" = "American_Eagle", "XE" = "ExpressJet", "YV" = "Mesa")

hflights$Carrier=lut[hflights$UniqueCarrier]
View(hflights$Carrier)
glimpse(hflights)
########################################################################################

#12.Players summary
odi%>%group_by(Player)%>%summarise(total_run=sum(Runs,na.rm = T),total_match=n())

#13.centuries,ducks,fifies, missed centuries, double century of indian players

a=odi%>%filter(Country=='India')%>%group_by(Player)%>%
  summarise(duck=sum(Runs==0,na.rm=T), 
            fifty=sum(Runs>49&Runs<100,na.rm=T),
            century=sum(Runs>99&Runs<200,na.rm = T),
            missed=sum(Runs>90&Runs<200.,na.rm = T),
            double=sum(Runs>199,na.rm = T))
View(a)
#14.same as 13 but of all players
b=odi%>%filter(Player=='Sachin R Tendulkar')%>%group_by(Player)%>%
                               summarise(ducks=sum(Runs==0,na.rm = T),
                                     under_fifty=sum(Runs<50,na.rm=T),
                                     fifty=sum(Runs>49&Runs<100,na.rm = T),
                                     missed=sum(Runs>90&Runs<100,na.rm = T),
                                     century=sum(Runs>99&Runs<200),double=sum(Runs>199))
View(b)
