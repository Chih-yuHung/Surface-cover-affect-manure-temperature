library(tidyverse); library(lubridate)
library(openxlsx2)

air_temp_LA <- read.csv("Input/Air temp_LA.csv")
LA_env <- air_temp_LA %>%
  separate(DATUM, into = c("Year", "Month"),sep = 4) %>%
  separate(Month, into = c("Month", "Day"),sep = 2) %>%
  select(Year, Month, Day, DOY = DAGNR, AirTmin1 = LTEMP, AirTmax1 = HTEMP, 
         SR = SOLIN, precip = NED, wind = XVH)
LA_env$Date <- paste(LA_env$Year, LA_env$Month, LA_env$Day, sep = "-")
LA_env$Date <- as.Date(LA_env$Date)

#Determine AirTmin2 and AirTmax0
LA_env <- LA_env %>%
  transform(AirTmin2 = lead(AirTmin1)) %>%
  transform(AirTmax0 = lag(AirTmax1))




#Determine Srmax
rad.h <- read_csv("Input/Hourly global radiation Göteborg Sol.csv",
                  col_types = cols(Datum = col_date(format = "%m/%d/%Y")))
colnames(rad.h) <- c("Date", "Time", "SR")
rad.max <- tapply(rad.h$SR,rad.h$Date,max)
rad.max1 <- data.frame(Date = names(rad.max), Srmax=rad.max)
rad.max1$Date <- as.Date(rad.max1$Date)
rad.max1 <- rad.max1 %>% 
  filter(between(Date, as.Date('2020-09-11'), as.Date('2021-08-31'))) %>%
  mutate_at(vars(Srmax), ~ . * 0.0036)
rad.max1$cloud<-ifelse(LA_env$SR<rad.max1$Srmax,((1-LA_env$SR/rad.max1$Srmax)/0.72)^(1/3.2),1)
write_xlsx(rad.max1,"Output/Radiation test.xlsx")
write_xlsx(LA_env,"Output/SR test.xlsx")

merge.LA <- merge(LA_env, rad.max1, by="Date")



  
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
  select(-HM, -Time, -hour) %>%
  pivot_wider(names_from = Hour,
              values_from = RH)
colnames(RH) <- c("Date", "RH.6", "RH.15")
