#Compile weather data

library(suncalc)
library(lubridate)
library(ggplot2)
library(okmesonet)
library(dplyr)
library(data.table)

### Weather patterns, using data from ERIC Station year 2011, 2015, 
##and 2019 on dates 6/22-6/26

#Obtain data from OK Mesonet website
updatestn() #get latest information on mesonet stations
okstations = updatestn() #save latest information into okstations

mass = 16
# For Loop Weather Data ---------------------------------------------------
stats = c("ERIC", 
             "MANG", 
             "HOLL")
wdavg = NULL
for(s in stats){
  for(i in 1:3){ #beginning of weather data loop
    if(i == 1){
      beginTime = "2020-07-13 00:00:00"
      endTime = "2020-07-13 23:55:00"
    } else if(i == 2){
      beginTime = "2020-07-14 00:00:00"
      endTime = "2020-07-14 23:55:00"
    } else if(i == 3){
      beginTime = "2020-07-15 00:00:00"
      endTime = "2020-07-15 23:55:00"
    }
    stid <- "ERIC"
    stations <- read.csv("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Heatbursts and Behavior/data/geoinfo.csv", stringsAsFactors = F)
    lat = stations$nlat[which(stations$stid == stid)]
    lon = stations$elon[which(stations$stid == stid)]
    
    w = NULL
    w <- okmts(begintime=beginTime,
               endtime=endTime, 
               variables = c("TAIR", "RELH","PRES"),
               station="eric",  
               localtime=T) #Need to download the data into UTC, the time will appear as CDT
    w$DT = as.POSIXct(w$TIME, tz = "UTC")
    w$DTL = w$DT #Saves datetime into a new vector for local datetime
    w$DATE = as.POSIXct(substr(w$TIME,0,10))
    w$MONTH = as.factor(substr(w$TIME,6,7))
    w$DAY = as.factor(substr(w$TIME,9,10))
    w$YMD = w$DATE
    attributes(w$DTL)$tzone = "America/Chicago" #changes datetime to Central Time Zone
    w$YMDL <- as_date(w$DTL) #gives local (oklahoma) ymd date
    w$station = s
    
    sr1 <- getSunlightTimes(date = as_date(w$YMD[1]), lat = lat, lon = lon, tz = "America/Chicago", keep = c("sunrise"))
    sr2 <- getSunlightTimes(date = as_date(w$YMD[1]-86400), lat = lat, lon = lon, tz = "America/Chicago", keep = c("sunrise"))
    sr1 <- sr1$sunrise
    sr2 <- sr2$sunrise
    w$MAS <- ifelse(w$DT<sr1, difftime(w$DT, sr2, units = "mins"), difftime(w$DT, sr1, units = "mins"))
    w$MAS <- round(w$MAS, 0)
    
    wd = w
    
    labels <- seq(0,1435,5)
    wd$bins <- cut(wd$MAS,seq(0,1440,5), labels = labels, right = FALSE)               #make 5 minute bins
    wd$bins <- as.numeric(as.character(wd$bins))
    wd$DEW <- wd$TAIR-((100-wd$RELH)/5)
    
    wdsum <- data.frame(bin1 = wd$bins, 
                        bin2 = wd$bins+5,
                        time = wd$TIME,
                        date = wd$YMD, 
                        dateLocal = wd$YMDL,
                        monthLocal = substr(wd$YMDL,6,7),
                        dayLocal = substr(wd$YMDL, 9,10),
                        year = substr(wd$YMDL,1,4),
                        DEW = wd$DEW,
                        TAIR = wd$TAIR,
                        RELH = wd$RELH, 
                        PRES = wd$PRES,
                        station = wd$station)

    if(i == 1){
      wdsum$model = "pre_hb"
    } else if(i == 2){
      wdsum$model = "hb"
    } else if(i == 3){
      wdsum$model = "post_hb"
    }
    #wdsum_total = rbind(wdsum2011,wdsum2015,wdsum2019)
    print(i) #prints year that is done getting data for
    
    if(i == 1) {
      wdsum_total <- wdsum
    } else {
      wdsum_total <- rbind(wdsum_total,wdsum)
    }
    #print(paste0("Year Completed:", sep = " ", years[i]))
    
    
  } #end of weather data loop
  # setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Heatbursts and Behavior/data/")
  # write.csv(wdsum_total, paste0(s,"_weather.csv",sep = ""), row.names = FALSE)
  if(s == "ERIC"){
    wdavg = wdsum_total
  } else {
    wdavg = rbind(wdavg, wdsum_total)
  }
  print(s)
} #end of stations loop

####Removing faulty dataframes
rm(wdsum)
rm(wdsum_total)
rm(wdavg)
rm(wd)
rm(w)

wdavg$TEWL = 0 #total evaporative water loss increases until it reaches 2.4
wdavg$EWL = NULL

source("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Heatbursts and Behavior/src/ewl_calculations.R")
wdavg$EWL = sapply(wdavg, function(x) ewl_albright(mass,
                                                   wdavg$TAIR)) #albright equation

# wdavg$EWL = sapply(wdavg, function(x) ewl_mckechnie(mass,
#                                                     tuc,
#                                                     wdavg$TAIR))
# 
# wdavg$EWL = sapply(wdavg, function(x), ewl_gravilov(mass,
#                                                     tuc,
#                                                     wdavg$TAIR))
# tewl_equation = function(tewl,
#                          ewl,
#                          bin1,
#                          bin2){

# wdavg$TEWL = sapply(wdavg, function(x) tewl_equation(wdavg$TEWL,
#                                         wdavg$EWL,
#                                         wdavg$bin1,
#                                         wdavg$bin2))
for(j in 1:length(wdavg$bin1)){ #moved EWL equations after summarising data to see if the average EWL differs at all.
  if(j == 1){
    wdavg$TEWL[j] = 0 + wdavg$EWL[j]
  } else if(wdavg$bin1[j] == 0 & wdavg$bin2[j] == 5){
    wdavg$TEWL[j] = 0
  } else if(wdavg$bin1[j] == 0 && wdavg$bin1[j-1] == 1435){
    wdavg$TEWL[j] = 0
  } else if(wdavg$bin1[j] == 5 && wdavg$bin1[j-1] != 0){
    wdavg$TEWL[j] = 0
  } else if(is.na(wdavg$TAIR[j])==TRUE){
    wdavg$EWL[j] = 0
  } else {
    wdavg$TEWL[j] = wdavg$TEWL[j-1]+wdavg$EWL[j]
  }
}

#Use station ID to access folder and get a list of data files
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Heatbursts and Behavior/data/")
fname = "OK_SE_hb_weather"
save(wdavg, file = paste0(fname, ".Rdata"))

setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Heatbursts and Behavior/data/")
write.csv(wdavg, "hb_weather_data.csv", col.names = TRUE)

wdavg = wdsum %>% group_by(dayMonthLocal,
                           bin1,
                           bin2) %>%
  summarise(DEW = mean(DEW),
            TAIR = mean(TAIR),
            RELH = mean(RELH),
            PRES = mean(PRES) #,
            # EWL = mean(EWL),
            # TEWL = mean(TEWL)
  )
# # rain <- wd[wd$RAIN>0 & wd$TIME != 0,]
# # raindays <- unique(rain$YMD)
# # wd <- wd[!wd$YMD %in% raindays,]
# 
# #wd <- wd[wd$MAS<6*60,] #subset to sunrise data only

#Graph some data.
load("~/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Heatbursts and Behavior/data/hb_weather.Rdata") #Load wdsum data so you do not need to run code again:

Song_volume = 85
Song_detection = 30
Song_freq = 7000
source("~/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Heatbursts and Behavior/src/Atmospheric_sound_attenuation.R")

wdsum$CallRad <- mapply(aud_range,
                        Song_volume,
                        Song_detection,
                        Song_freq,
                        wdsum$TAIR,
                        wdsum$RELH,
                        wdsum$PRES)

wdtemp <- wdsum[which(wdsum$bin1<=600),]
wdtemp$dateLocal <- as.factor(wdtemp$dateLocal )

wmeans = wdtemp %>% #Finds Means of data *cannot use dataframe name in dplyr!
  group_by(model, bin1) %>%
  dplyr::summarize(n = n(), #need to use dplyr:: because other libraries have the summarize function
                   CallRadMean = mean(CallRad), 
                   CallRadSE = (sd(CallRad)/sqrt(n)),
                   TAIRMean = mean(TAIR),
                   RELHMean = mean(RELH),
                   PRESMean = mean(PRES),
                   DEWMean = mean(DEW)
  )
ggplot( data = wmeans, aes(x=bin1, y=CallRadMean, group=model, color = model)) +
  geom_line()+
  theme_classic()

ggplot( data = wmeans, aes(x=bin1, y=TAIRMean, group=model, color = model)) +
  geom_line()+
  theme_classic()

ggplot( data = wmeans, aes(x=bin1, y=DEWMean, group=model, color = model)) +
  geom_line() +
  theme_classic()+
  labs(x= "Min from Sunrise",
       y = "Dewpoint \nTemperature")

ggplot( data = wdtemp, aes(x=bin1, y=RELH, group=dateLocal, color = dateLocal)) +
  geom_line() +
  theme_classic()

ggplot( data = wdtemp, aes(x=DEW, y=CallRad, group=dateLocal, color = dateLocal)) +
  geom_line()

ggplot( data = wdtemp, aes(x=DEW, y=CallRad)) +
  geom_point()

ggplot( data = wdtemp, aes(x=TAIR, y=DEW, group=dateLocal, color = dateLocal)) +
  geom_line() +
  theme_classic()+
  labs(x= "Air Temperature (C)",
       y = "Dewpoint \nTemperature")

ggplot( data = wdtemp, aes(x=TAIR, y=RELH, group=dateLocal, color = dateLocal)) +
  geom_line() +
  theme_classic()+
  labs(x= "Air Temperature (C)",
       y = "Relative \nHumidity (%)")
