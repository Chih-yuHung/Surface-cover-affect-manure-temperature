library(tidyverse); library(lubridate)
library(openxlsx2)

air_temp_LA <- read.csv("Input/Air temp_LA.csv")
LA_env <- air_temp_LA %>%
  separate(DATUM, into = c("Year", "Month"),sep = 4) %>%
  separate(Month, into = c("Month", "Day"),sep = 2) %>%
  select(Year, Month, Day, DOY = DAGNR, AirTmin1 = LTEMP, AirTmax1 = HTEMP, 
         SR = SOLIN, precip = NED, wind = XVH)
LA_env$Date <- paste(LA_env$Year, LA_env$Month, LA_env$Day, sep = "-")
LA_env$MonthDay <- paste(LA_env$Month, LA_env$Day, sep = "-")
LA_env$Date <- as.Date(LA_env$Date)

#Determine AirTmin2 and AirTmax0
LA_env <- LA_env %>%
  transform(AirTmin2 = lead(AirTmin1)) %>%
  transform(AirTmax0 = lag(AirTmax1))

#Determine Srmax
rad.h <- read_csv("Input/Hourly global radiation Göteborg Sol.csv",
                  col_types = cols(Datum = col_date(format = "%m/%d/%Y")))
colnames(rad.h) <- c("Date", "Time", "SR")
rad.h <- rad.h %>%
  separate(Date, into = c("Year", "MonthDay"),sep = 5)
rad.max <- tapply(rad.h$SR,rad.h$MonthDay,max)
rad.max1 <- data.frame(MonthDay = names(rad.max), Srmax=rad.max)
rad.max1 <- rad.max1 %>%
  mutate(Srmax = Srmax * 0.0036, Srmax = round(Srmax, digits = 1))
LA_env <- merge(LA_env, rad.max1, by = "MonthDay")

#Determine cloud cover and SR
LA_env$cloud<-ifelse(LA_env$SR<LA_env$Srmax,((1-LA_env$SR/LA_env$Srmax)/0.72)^(1/3.2),1)
LA_env <- LA_env %>%
  mutate(SR = Srmax * cloud, SR = round(SR, digits = 1)) %>%
  mutate(cloud = round(cloud, digits = 1))
  
#Determine RH
weather <- read_csv("Input/Relative humidity daily data Ängelholm-Barkåkra.csv", 
                    col_types = cols(Datum = col_date(format = "%m/%d/%Y")))
colnames(weather) <- c("Date", "Time", "RH")
weather <- weather %>% 
  filter(between(Date, as.Date('2020-09-11'), as.Date('2021-08-31')))
weather$Time <- as.POSIXct(strptime(weather$Time, "%H:%M:%S"))
weather$Hour <- hour(weather$Time)
RH <- weather %>%
  filter(Hour == 6 | Hour == 15) %>%
  select(-Time) %>%
  pivot_wider(names_from = Hour,
              values_from = RH)
colnames(RH) <- c("Date", "RH.6", "RH.15")
LA_env <- merge(LA_env, RH, by = "Date")

#Add Date ID to the input
Date.ID<-as.numeric(as.Date(0:354,origin="2020-09-11"),by="days")
LA_env$'Date ID'<-as.numeric(as.Date(0:353,origin="2020-09-11"),by="days")
LA_env <- LA_env %>%
  select(c("Year", "Month", "Day", "DOY", "AirTmax1", "AirTmin1", "AirTmin2",
           "AirTmax0", "SR", "Srmax", "precip", "RH.6", "RH.15", "wind", "cloud",
           "Date ID"))

#Save daily env input_LA
write.csv(LA_env,"Input/daily env input_LA.csv", row.names = FALSE)
