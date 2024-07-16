library(imputeTS) # for NA interpolation
library(lubridate)
#Deal with the manure temperature
temp<-read.csv("C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 9_Effect of cover on manure T/3. result/manure temp Andersfalt.csv",header=T)
#Air temperature
temp.air<-read.csv("C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 9_Effect of cover on manure T/3. result/Air temp Andersfalt.csv",header=T)
# #Give data, and time first
time<-strsplit(temp$Time,"\\s")
time<-as.data.frame(matrix(unlist(time),ncol=2,byrow=TRUE))
hour<-matrix(unlist(strsplit(time[,2],":")),ncol=2,byrow=TRUE)[,1]
#for date, my goal is to have year, month, day and DOY in four columns
#need to convert date to the same form first, now I have  "mm/dd/yyyy"
time$V1<-as.character(mdy(time$V1))
DOY<-yday(as.Date(time$V1))
D<-as.data.frame(matrix(unlist(strsplit(time$V1,"-")),ncol=3,byrow=TRUE))
colnames(D)<-c("Year","Month","Day")
# # #combine the date and temperature data
temp<-cbind(time[,1],D,hour,DOY,temp[,-1])
colnames(temp)[1]<-c("Date")

temp0.5<-tapply(temp$Temp0.5,temp$Date,function(x) round(mean(x,na.rm=TRUE),2))
temp1.5<-tapply(temp$Temp1.5,temp$Date,function(x) round(mean(x,na.rm=TRUE),2))
temp2.5<-tapply(temp$Temp2.5,temp$Date,function(x) round(mean(x,na.rm=TRUE),2))
temp<-as.data.frame(cbind(unique(temp$Date),temp0.5,temp1.5,temp2.5,temp.air$XTEMP))
colnames(temp)[c(1,5)]<-c("Date","temp.air")

temp$Date<-as.Date(temp$Date,"%Y-%m-%d")
rownames(temp)<-c(1:355)
#Add depth data to
temp$depth<-NA
temp$depth[c(1,36,103,131,146,179,216,249,268)]<-c(61,162,400,400,400,400,130,52,49)
temp$depth<-na_interpolation(temp$depth)

#Remove data after July 4, 2021 
#because it looks like the thermocouples are in air. 
temp<-temp[c(-298:-355),]

#Draw the figure
par(mar=c(4,5,4,5))
plot(temp[,1],temp[,2],ylim=c(-10,30)
     ,type="l",col="black",lwd=2
     ,xlab="Date (2020 Sept - 2021 Jul.)"
     ,ylab="Temperature (°C)") #0.5m
lines(temp[,1],temp[,3],col="blue",lwd=2) #1.5m
lines(temp[,1],temp[,4],col="red",lwd=2) #2.5m
lines(temp[,1],temp[,5],col="grey",lwd=2) #air
legend("topleft",c("Air temperature"
                   ,"Manure temperature at 0.5m"
                   ,"Manure temperature at 1.5m"
                   ,"Manure temperature at 2.5m")
       ,col=c("grey","black","blue","red")
       ,lty=1,lwd=2,bty="n")
lines(temp[,1],(temp$depth-100)/10,lty=2)
axis(side=4,at=c(-10,0,10,20,30)
     ,labels = c(0,100,200,300,400))
mtext("Manure depth (cm)", side = 4,line=2.5)
