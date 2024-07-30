library(imputeTS) # for NA interpolation
library(lubridate)
#Deal with the manure temperature
temp.cover<-read.csv("Input/manure temp_VAC.csv",header=T)
temp.no<-read.csv("Input/manure temp_VA.csv",header=T)
#The cover was installed on June 11, 2020 to Nov. 10, 2020
temp.cover<-temp.cover[c(1:146),c(2:9,13)]
temp.no<-temp.no[c(1:146),c(2:9,13)]
#Air temperature
temp.air<-read.csv("Input/Air temp_VA.csv",header=T)
# move air temperature to cover 
temp.cover$temp.air<-temp.air$XTEMP[4:149]

#add date to the dataframe
temp.cover$Date<-as.Date(c(0:(length(temp.cover[,1])-1)),origin = "2020-06-18")
temp.no$Date<-as.Date(c(0:(length(temp.cover[,1])-1)),origin = "2020-06-18")

#Draw the figure for covered manure tank
par(mar=c(4,5,4,5))
plot(temp.cover$Date,temp.cover$temp0.5,ylim=c(-10,30)
     ,type="l",col="black",lwd=2
     ,xlab="Date (2020 Jun. - 2020 Nov.)"
     ,ylab="Temperature (°C)") #0.5m
lines(temp.cover$Date,temp.cover$temp1.5,col="blue",lwd=2) #1.5m
lines(temp.cover$Date,temp.cover$temp2.5,col="red",lwd=1.5) #2.5m
lines(temp.cover$Date,temp.cover$temp.air,col="grey",lwd=2) #air
legend("bottomleft",c("Air temperature"
                   ,"Manure temperature at 0.5m"
                   ,"Manure temperature at 1.5m"
                   ,"Manure temperature at 2.5m")
       ,col=c("grey","black","blue","red")
       ,lty=1,lwd=2,bty="n")
lines(temp.cover$Date,((temp.cover$Depth*100)*(1/5)-10),lty=2)
axis(side=4,at=c(-10,0,10,20,30)
     ,labels = c(0,50,100,150,200))
mtext("Manure depth (cm)", side = 4,line=2.5) 

#Draw the figure for uncovered manure tank
par(mar=c(4,5,4,5))
plot(temp.no$Date,temp.no$temp0.5,ylim=c(-10,30)
     ,type="l",col="black",lwd=2
     ,xlab="Date (2020 Jun. - 2020 Nov.)"
     ,ylab="Temperature (°C)") #0.5m
lines(temp.no$Date,temp.no$temp1.5,col="blue",lwd=2) #1.5m
lines(temp.no$Date,temp.no$temp2.5,col="red",lwd=1.5) #2.5m
lines(temp.cover$Date,temp.cover$temp.air,col="grey",lwd=2) #air
legend("bottomleft",c("Air temperature"
                      ,"Manure temperature at 0.5m"
                      ,"Manure temperature at 1.5m"
                      ,"Manure temperature at 2.5m")
       ,col=c("grey","black","blue","red")
       ,lty=1,lwd=2,bty="n")
lines(temp.no$Date,((temp.no$Depth*100)*(1/5)-10),lty=2)
axis(side=4,at=c(-10,0,10,20,30)
     ,labels = c(0,50,100,150,200))
mtext("Manure depth (cm)", side = 4,line=2.5) 
