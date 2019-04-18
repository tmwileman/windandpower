### Please set working directory to one with all sent files
setwd("~/TRENDI/Gavin Project")
source("DataAnalyticsFunctions.R")
installpkg("readr")
library(readr)
installpkg("chron")
library(chron)
installpkg("dplyr")
library(dplyr)
installpkg("ggplot2")
library(ggplot2)
installpkg("lubridate")
library(lubridate)
installpkg("corrplot")
library(corrplot)
installpkg("caret")
library(caret)
installpkg("zoo")
library(zoo)
########################################################################################################################
########################################################################################################################
########################################################################################################################
### STEP 1: CLEANING ###################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################
### Wind energy generation data
### Read in the wind generation data
wo17 <- read.csv('hwo17.csv', stringsAsFactors = FALSE) 
wo16 <- read.csv('hwo16.csv', stringsAsFactors = FALSE)

### Bind the two wind energy generation tables
wo16$Year <- 2016
wo17$Year <- 2017
wo16$time.date.stamp <- as.chron(wo16$time.date.stamp, "%m/%d/%Y %H:%M")
wo16$Date <- as.Date(wo16$time.date.stamp, "%m/%d/%Y %H:%M")
wo16$Date <- format(wo16$Date, "%b-%d")
wo17$time.date.stamp <- dmy_hms(wo17$time.date.stamp)
wo16$Hour <- hour(wo16$time.date.stamp) + 1
wo17$Hour <- hour(wo17$time.date.stamp) + 1

### Bind the two wind energy generation tables
wind <- rbind(wo17, wo16)

########################################################################################################################
### Real-time-market data
### Read in the real-time-market data
rtm.jan16_1 <- read.csv('jan16_1.csv')
rtm.jan16_2 <- read.csv('jan16_2.csv')
rtm.feb16 <- read.csv('feb16.csv')
rtm.mar16_1 <- read.csv('mar16_1.csv')
rtm.mar16_2 <- read.csv('mar16_2.csv')
rtm.apr16 <- read.csv('apr16.csv')
rtm.may16_1 <- read.csv('may16_1.csv')
rtm.may16_2 <- read.csv('may16_2.csv')
rtm.jun16 <- read.csv('jun16.csv')
rtm.jul16_1 <- read.csv('jul16_1.csv')
rtm.jul16_2 <- read.csv('jul16_2.csv')
rtm.aug16_1 <- read.csv('aug16_1.csv')
rtm.aug16_2 <- read.csv('aug16_2.csv')
rtm.sep16 <- read.csv('sep16.csv')
rtm.oct16_1 <- read.csv('oct16_1.csv')
rtm.oct16_2 <- read.csv('oct16_2.csv')
rtm.nov16 <- read.csv('nov16.csv')
rtm.dec16_1 <- read.csv('dec16_1.csv')
rtm.dec16_2 <- read.csv('dec16_2.csv')
rtm.jan17 <- read.csv('Jan 17 MKT.csv')
rtm.feb17 <- read.csv('Feb 17 MKT.csv')
rtm.mar17 <- read.csv('Mar 17 MKT.csv')
rtm.apr17 <- read.csv('Apr 17 MKT.csv')
rtm.may17 <- read.csv('May 17 MKT.csv')
rtm.jun17 <- read.csv('Jun 17 MKT.csv')
rtm.jul17 <- read.csv('Jul 17 MKT.csv')
rtm.aug17 <- read.csv('Aug 17 MKT.csv')
rtm.sep17 <- read.csv('Sep 17 MKT.csv')
rtm.oct17 <- read.csv('Oct 17 MKT.csv')
rtm.nov17 <- read.csv('Nov 17 MKT.csv')
rtm.dec17 <- read.csv('Dec 17 MKT.csv')

### Bind the real time market data into a single table
rtmkt <- rbind(rtm.jan16_1, rtm.jan16_2, rtm.feb16, rtm.mar16_1, rtm.mar16_2, rtm.apr16, rtm.may16_1, rtm.may16_2, rtm.jun16, rtm.jul16_1, rtm.jul16_2, rtm.aug16_1, rtm.aug16_2, rtm.sep16, rtm.oct16_1, rtm.nov16, rtm.dec16_1, rtm.dec16_2, rtm.jan17, rtm.feb17, rtm.mar17, rtm.apr17, rtm.may17, rtm.jun17, rtm.jul17, rtm.aug17, rtm.sep17, rtm.oct17, rtm.nov17, rtm.dec17)

### Transfor the real-time-market dates into the appropriate format
rtmkt <- rtmkt %>% filter(Delivery.Date != "1-Jan-18")
rtmkt$Delivery.Date <- mdy(rtmkt$Delivery.Date)
#View(rtmkt %>% dplyr::select(Delivery.Date, Delivery.Date1) %>% filter(is.na(Delivery.Date1) == TRUE))
rtmkt$Year <- format(rtmkt$Delivery.Date, format = "%Y")
rtmkt$Delivery.Date <- format(rtmkt$Delivery.Date, format = "%b-%d")

### Change column names ...
colnames(rtmkt)[colnames(rtmkt)=="Delivery.Date"] <- "Date"
colnames(rtmkt)[colnames(rtmkt)=="Delivery.Hour"] <- "Hour"
colnames(rtmkt)[colnames(rtmkt)=="Settlement.Point.Price"] <- "RTM.Settlement.Point.Price"

### Read in Day-Ahead-Market data
dam.jan16 <- read.csv('DAMjan16.csv')
dam.feb16 <- read.csv('DAMfeb16.csv')
dam.mar16 <- read.csv('DAMmar16.csv')
dam.apr16 <- read.csv('DAMapr16.csv')
dam.may16 <- read.csv('DAMmay16.csv')
dam.jun16 <- read.csv('DAMjun16.csv')
dam.jul16 <- read.csv('DAMjul16.csv')
dam.aug16 <- read.csv('DAMaug16.csv')
dam.sep16 <- read.csv('DAMsep16.csv')
dam.oct16 <- read.csv('DAMoct16.csv')
dam.nov16 <- read.csv('DAMnov16.csv')
dam.dec16 <- read.csv('DAMdec16.csv')
dam.jan17 <- read.csv('DAMjan17.csv')
dam.feb17 <- read.csv('DAMfeb17.csv')
dam.mar17 <- read.csv('DAMmar17.csv')
dam.apr17 <- read.csv('DAMapr17.csv')
dam.may17 <- read.csv('DAMmay17.csv')
dam.jun17 <- read.csv('DAMjun17.csv')
dam.jul17 <- read.csv('DAMjul17.csv')
dam.aug17 <- read.csv('DAMaug17.csv')
dam.sep17 <- read.csv('DAMsep17.csv')
dam.oct17 <- read.csv('DAMoct17.csv')
dam.nov17 <- read.csv('DAMnov17.csv')
dam.dec17 <- read.csv('DAMdec17.csv')

### Bind the day-ahead-market datainto a single table
damkt <- rbind(dam.jan16, dam.feb16, dam.mar16, dam.apr16, dam.may16, dam.jun16, dam.jul16, dam.aug16, dam.sep16, dam.oct16, dam.nov16, dam.dec16, dam.jan17, dam.feb17, dam.mar17, dam.apr17, dam.may17, dam.jun17, dam.jul17, dam.aug17, dam.sep17, dam.oct17, dam.nov17, dam.dec17)

### Transform the day-ahead-market dates and times into the appropriate format
damkt$Delivery.Date <- mdy(damkt$Delivery.Date)
damkt$Year <- format(damkt$Delivery.Date, format = "%Y")
damkt$Delivery.Date <- format(damkt$Delivery.Date, format = "%b-%d")
damkt$Hour.Ending <- strptime(damkt$Hour.Ending, format = "%H:%M")
damkt$Hour.Ending <- hour(damkt$Hour.Ending) + 1

### Change column names ...
colnames(damkt)[colnames(damkt)=="Delivery.Date"] <- "Date"
colnames(damkt)[colnames(damkt)=="Hour.Ending"] <- "Hour"
colnames(damkt)[colnames(damkt)=="Settlement.Point"] <- "Settlement.Point.Name"
colnames(damkt)[colnames(damkt)=="Settlement.Point.Price"] <- "DAM.Settlement.Point.Price"

########################################################################################################################
### Merge the Day-Ahead and Real-Time market data and clean
rtmkt$Settlement.Point.Name <- as.character(rtmkt$Settlement.Point.Name)
damkt$Settlement.Point.Name <- as.character(damkt$Settlement.Point.Name)
mkt <- merge(rtmkt, damkt, by = c("Date", "Hour", "Year", "Settlement.Point.Name"), all.x = TRUE)
mkt <- dplyr::select(mkt, -Delivery.Interval, -Repeated.Hour.Flag.x, -Repeated.Hour.Flag.y)
mkt <- filter(mkt, Settlement.Point.Name %in% c("HB_SOUTH", "LZ_WEST", "HB_NORTH", "LZ_HOUSTON", "LZ_NORTH", "LZ_SOUTH", "HB_WEST", "HB_HOUSTON"))

########################################################################################################################
### Merge the market and wind generation data
### Left joined the two tables on date and hour
mkt$Year <- as.numeric(mkt$Year)
wind.mkt <- merge(mkt, wind, by = c("Date", "Hour", "Year"), all.x = TRUE)

### Remove non regional load zones and busses from data
#wind.mkt <- filter(wind.mkt, Settlement.Point.Name %in% c("HB_SOUTH", "LZ_WEST", "HB_NORTH", "LZ_HOUSTON", "LZ_NORTH", "LZ_SOUTH", "HB_WEST", "HB_HOUSTON"))

########################################################################################################################
### Weather data
### Read in the weather data
weather <- read.csv('weather16-17.csv', na.strings = "")

### dplyr::Select only the needed fields; station_name and dry bulb temp

# Drop VRB from direction
# Drop s from HOURLYPrecip
# Change T to O in HOURLYPrecip
weather$HOURLYWindDirection <- as.character(weather$HOURLYWindDirection)
weather$HOURLYWindDirection <- gsub("VRB","0",weather$HOURLYWindDirection)
weather$HOURLYWindDirection <- as.numeric(weather$HOURLYWindDirection)

weather$HOURLYPrecip <- as.character(weather$HOURLYPrecip)
weather$HOURLYPrecip <- gsub(".*s.*", NA_character_, weather$HOURLYPrecip)
weather$HOURLYPrecip <- gsub("T","0", weather$HOURLYPrecip)
weather$HOURLYPrecip <- as.numeric(weather$HOURLYPrecip)

weather$HOURLYVISIBILITY <- as.character(weather$HOURLYVISIBILITY)
weather$HOURLYVISIBILITY <- gsub(".*V.*", NA_character_, weather$HOURLYVISIBILITY)
weather$HOURLYVISIBILITY <- as.numeric(weather$HOURLYVISIBILITY)

### Format the date/time of the weather data
weather$DATE <- as.POSIXct(weather$DATE, format = "%m/%d/%Y %H:%M")
weather$POSIX <- weather$DATE
weather$DATE <- format(weather$POSIX, format = "%b-%d")
weather$Year <- year(weather$POSIX)
#weather$Year <- format(weather$mdy, format = "%Y")
weather$Hour <- hour(weather$POSIX) + 1

weather$HOURLYDRYBULBTEMPF <- as.numeric(weather$HOURLYDRYBULBTEMPF)
weather$HOURLYWindSpeed <- as.numeric(weather$HOURLYWindSpeed)
weather$HOURLYWindDirection <- as.numeric(weather$HOURLYWindDirection)
weather$HOURLYVISIBILITY <- as.numeric(weather$HOURLYVISIBILITY)
weather$HOURLYWETBULBTEMPF <- as.numeric(weather$HOURLYWETBULBTEMPF)
weather$HOURLYRelativeHumidity <- as.numeric(weather$HOURLYRelativeHumidity)
weather$HOURLYWindGustSpeed <- as.numeric(weather$HOURLYWindGustSpeed)
weather$HOURLYStationPressure <- as.numeric(weather$HOURLYStationPressure)
weather$HOURLYPressureChange <- as.numeric(weather$HOURLYPressureChange)
weather$HOURLYPrecip <- as.numeric(weather$HOURLYPrecip)

### We have market data that is updated every hour
### But our weather data is measured more frequently than that
### To make like comparisons, we are going to take the mean of the weather features grouped by the hour and location
weather <- weather %>%
  dplyr::select(STATION_NAME, DATE, POSIX, Year, Hour, HOURLYDRYBULBTEMPF, HOURLYWindSpeed, HOURLYWindDirection, HOURLYVISIBILITY, HOURLYWETBULBTEMPF, HOURLYRelativeHumidity, HOURLYStationPressure, HOURLYPrecip) %>%
  group_by(STATION_NAME, Year, DATE, Hour) %>%
  mutate(HOURLYWindSpeed.MEAN = mean(HOURLYWindSpeed, na.rm = TRUE),
         HOURLYDRYBULBTEMPF.MEAN = mean(HOURLYDRYBULBTEMPF, na.rm = TRUE),
         HOURLYWindDirection.MEAN = mean(HOURLYWindDirection, na.rm = TRUE),
         HOURLYVISIBILITY.MEAN = mean(HOURLYVISIBILITY, na.rm = TRUE),
         HOURLYWETBULBTEMPF.MEAN = mean(HOURLYWETBULBTEMPF, na.rm = TRUE),
         HOURLYRelativeHumidity.MEAN = mean(HOURLYRelativeHumidity, na.rm = TRUE),
         HOURLYStationPressure.MEAN = mean(HOURLYStationPressure, na.rm = TRUE),
         HOURLYPrecip.MEAN = mean(HOURLYPrecip, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(HOURLYPrecip.MEAN = case_when(is.na(HOURLYPrecip.MEAN) == TRUE ~ 0,
                                       TRUE ~ HOURLYPrecip.MEAN))
### Reassigning column name on weather to match the corresponding column of the table we are joining it to
colnames(weather)[colnames(weather)=="DATE"] <- "Date"
### Ensuring right data type
weather$STATION_NAME <- as.character(weather$STATION_NAME)
########################################################################################################################
### Join weather data with wind and market data
### Create a column in the market data aligning hubs and load zones with regional weather data
values <- c("SAN ANTONIO INTERNATIONAL AIRPORT TX US", "DAL FTW WSCMO AIRPORT TX US", "MIDLAND INTERNATIONAL AIRPORT TX US", "HOUSTON INTERCONTINENTAL AIRPORT TX US", "MIDLAND INTERNATIONAL AIRPORT TX US", "HOUSTON INTERCONTINENTAL AIRPORT TX US", "DAL FTW WSCMO AIRPORT TX US", "SAN ANTONIO INTERNATIONAL AIRPORT TX US")
index <- c("HB_SOUTH", "HB_NORTH", "HB_WEST", "HB_HOUSTON", "LZ_WEST","LZ_HOUSTON", "LZ_NORTH", "LZ_SOUTH")
wind.mkt$STATION_NAME <- values[match(wind.mkt$Settlement.Point.Name, index)]

### Merge the data
#data <- merge(wind.mkt, weather, by = c("Date", "Hour", "Year", "STATION_NAME"))
### dplyr's left join is working more reliably in this scenario than "merge"
data <- left_join(wind.mkt, weather, by = c("Date", "Hour", "Year", "STATION_NAME"))


### Clean up colmn names
colnames(data)[colnames(data)=="ERCOT.Load..MW"] <- "ERCOT.Load.MW"
colnames(data)[colnames(data)=="Total.Wind.Output..MW"] <- "Total.Wind.Output.MW"
colnames(data)[colnames(data)=="Wind.Output....of.Load"] <- "Wind.Output.Percent.Of.Load"
colnames(data)[colnames(data)=="X1.hr.MW.change"] <- "One.Hr.MW.Change"
colnames(data)[colnames(data)=="X1.hr...change"] <- "One.Hr.MW.Percent.Change"
colnames(data)[colnames(data)=="HOURLYDRYBULBTEMPF"] <- "Hourly.Dry.Bulb.Temp.F"
colnames(data)[colnames(data)=="HOURLYWindSpeed"] <- "Hourly.Wind.Speed"
colnames(data)[colnames(data)=="HOURLYWindDirection"] <- "Hourly.Wind.Direction"
colnames(data)[colnames(data)=="Wind.Output....of.Installed"] <- "Wind.Output.Percent.Of.Installed"
colnames(data)[colnames(data)=="Wind.Output....of.Load"] <- "Wind.Output.Percent.Of.Load"
colnames(data)[colnames(data)=="STATION_NAME"] <- "Station.Name"
colnames(data)[colnames(data)=="time.date.stamp"] <- "Time.Date.Stamp"

### Make some columns numeric, remove commas first, other NA values
### Blocked off lines are to make sure our operations are performing correctly
data$RTM.Settlement.Point.Price <- gsub(",","",data$RTM.Settlement.Point.Price)
data$RTM.Settlement.Point.Price <- as.numeric(data$RTM.Settlement.Point.Price)
data <- data %>% mutate(One.Hr.MW.Change = case_when(One.Hr.MW.Change %in% c("","#VALUE!") ~ NA_character_,
                                                     TRUE ~ One.Hr.MW.Change),
                        One.Hr.MW.Percent.Change = case_when(One.Hr.MW.Percent.Change %in% c("","#VALUE!") ~ NA_character_,
                                                             TRUE ~ One.Hr.MW.Percent.Change)
)
data$One.Hr.MW.Change <- as.numeric(data$One.Hr.MW.Change)
#data$One.Hr.MW.Change1 <- as.numeric(data$One.Hr.MW.Change)
### View command are blocked off as they are only to debug past errors in running the code
#View(data %>% dplyr::select(One.Hr.MW.Change,One.Hr.MW.Change1) %>% filter(is.na(One.Hr.MW.Change1) == TRUE))
data$One.Hr.MW.Percent.Change <- as.numeric(data$One.Hr.MW.Percent.Change)
#View(data %>% dplyr::select(One.Hr.MW.Change,One.Hr.MW.Percent.Change1) %>% filter(is.na(One.Hr.MW.Percent.Change1) == TRUE))
#data$RTM.Settlement.Point.Price1 <- as.numeric(data$RTM.Settlement.Point.Price)
#data %>% dplyr::select(RTM.Settlement.Point.Price, RTM.Settlement.Point.Price1) %>% filter(is.na(RTM.Settlement.Point.Price1) == TRUE) #filter(RTM.Settlement.Point.Price != as.character(RTM.Settlement.Point.Price1))
data$Hourly.Dry.Bulb.Temp.F <- as.numeric(data$Hourly.Dry.Bulb.Temp.F)
data$Hourly.Wind.Direction <- as.numeric(data$Hourly.Wind.Direction)

### Organize data and remove unnecessary columns
### 360 degrees are too many for levels, and there is no linear relationship to radial direction
### To take advantage of wind direction we are going to assign factor levels instead
### We are also getting rid of subhourly observations here that we have already transformed into hourly observations
#data <- dplyr::select(data, Time.Date.Stamp, Year, Date, Hour, Settlement.Point.Name, Settlement.Point.Type, RTM.Settlement.Point.Price, DAM.Settlement.Point.Price, ERCOT.Load.MW, Total.Wind.Output.MW, Wind.Output.Percent.Of.Load, Wind.Output.Percent.Of.Installed, Wind.Output.Percent.Of.Load, One.Hr.MW.Change, One.Hr.MW.Percent.Change, Hourly.Dry.Bulb.Temp.F, Hourly.Wind.Speed, Hourly.Wind.Direction)
data <- data %>% 
  dplyr::select(-Hourly.Dry.Bulb.Temp.F, -Hourly.Wind.Direction, -Hourly.Wind.Speed) %>%
  group_by(Settlement.Point.Name, Settlement.Point.Type, Date, Hour) %>%
  mutate(RTM.Settlement.Point.Price.MEAN = mean(RTM.Settlement.Point.Price, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(HOURLYWindDirection.General = case_when(HOURLYWindDirection.MEAN <= 45 ~ "NNE",
                                                 HOURLYWindDirection.MEAN <= 90 ~ "NEE",
                                                 HOURLYWindDirection.MEAN <= 135 ~ "SEE",
                                                 HOURLYWindDirection.MEAN <= 180 ~ "SSE",
                                                 HOURLYWindDirection.MEAN <= 225 ~ "SSW",
                                                 HOURLYWindDirection.MEAN <= 270 ~ "SWW",
                                                 HOURLYWindDirection.MEAN <= 315 ~ "NWW",
                                                 HOURLYWindDirection.MEAN <= 360 ~ "NNW",
                                                 TRUE ~ NA_character_
  )
  )
### Ensure right data types
data$HOURLYWindDirection.General <- as.factor(data$HOURLYWindDirection.General)
data$Settlement.Point.Name <- as.factor(data$Settlement.Point.Name)
data$ERCOT.Load.MW <- as.numeric(data$ERCOT.Load.MW)
data$Total.Wind.Installed..MW <- as.numeric(data$Total.Wind.Installed..MW)
data$Total.Wind.Output.MW <- as.numeric(data$Total.Wind.Output.MW)

### Extract Month from Date variable
data$Month <- vapply(strsplit(data$Date,"-"), `[`, 1, FUN.VALUE=character(1))
### Add tomorrow's date so we can later join tomorrow's RTM price
data$Time.Date.Stamp <- as.Date(data$Time.Date.Stamp)
data$Date.Tomorrow <- data$Time.Date.Stamp + 1
########################################################################################################################
### dplyr::Select columns we want
data.select <- data[c("POSIX","Date","Date.Tomorrow","Month","Hour","Year","Settlement.Point.Name","Settlement.Point.Type","DAM.Settlement.Point.Price","Time.Date.Stamp","ERCOT.Load.MW","Total.Wind.Output.MW","Total.Wind.Installed..MW","Wind.Output.Percent.Of.Load","Wind.Output.Percent.Of.Installed","One.Hr.MW.Change","One.Hr.MW.Percent.Change","Station.Name","HOURLYWindSpeed.MEAN","HOURLYDRYBULBTEMPF.MEAN","HOURLYWindDirection.MEAN","HOURLYVISIBILITY.MEAN","HOURLYWETBULBTEMPF.MEAN","HOURLYRelativeHumidity.MEAN","HOURLYStationPressure.MEAN","HOURLYPrecip.MEAN","RTM.Settlement.Point.Price.MEAN","HOURLYWindDirection.General")]
### Remove duplicates and NAs
data.full <- unique(data.select)
paste("data.full has",nrow(data.full),"rows")
data.full <- na.omit(data.full)
### Create df to join onto OG so we don't have extra columns
data_to_join <- data.full[c("Year","Time.Date.Stamp","Hour","Settlement.Point.Name","Settlement.Point.Type","RTM.Settlement.Point.Price.MEAN")]
### Join data
data.full <- left_join(x = data.full, y = data_to_join, by = c("Year","Date.Tomorrow" = "Time.Date.Stamp","Hour","Settlement.Point.Name","Settlement.Point.Type"))
paste("data.full has",nrow(data.full),"rows")
#######################
### To check join was successful:
#View(data.full %>% dplyr::select(Time.Date.Stamp, Date.Tomorrow, Settlement.Point.Name, Hour, Settlement.Point.Type, RTM.Settlement.Point.Price.MEAN.Today, RTM.Settlement.Point.Price.MEAN.Tomorrow) %>%filter(Settlement.Point.Name == "HB_HOUSTON"))
### Because we converted to hourly data, we will have duplicate observations. We will remove them here, and several more times. 
data.full <- unique(data.full)
paste("data.full has",nrow(data.full),"rows")
data.full <- na.omit(data.full)
### Renaming new columns from join
colnames(data.full)[colnames(data.full)=="RTM.Settlement.Point.Price.MEAN.y"] <- "RTM.Settlement.Point.Price.MEAN.Tomorrow"
colnames(data.full)[colnames(data.full)=="RTM.Settlement.Point.Price.MEAN.x"] <- "RTM.Settlement.Point.Price.MEAN.Today"
### Creating frame without variables we won't use in models
data.modeling <- data.full %>% dplyr::select(-Date, -Date.Tomorrow, -Time.Date.Stamp, -Year, -Station.Name, -POSIX)
data.modeling <- unique(data.modeling)
data.IV <- data.full %>% dplyr::select(-Date, -Date.Tomorrow, -Time.Date.Stamp, -Station.Name, -POSIX)
data.IV <- unique(data.IV)
####################################################################################################################################
### We want to add a rolling average for each of the hourly weather observations, as well as the market prices
### We want to reorder the observations by Settlement.Point.Name, Settlement.Point.Type, Time.Date.Stamp, Hour
### Need to first make Hour of type factor, reorder in chronological order
data.full$Hour <- as.factor(data.full$Hour)
### Reordering is done automatically:
levels(data.full$Hour)
### Reordering dataframe
data.ordered <- data.full %>% dplyr::select(-POSIX) 
data.ordered <- unique(data.ordered)
data.ordered <- na.omit(data.ordered)
data.ordered <- data.ordered %>% arrange(Settlement.Point.Name,Settlement.Point.Type,Time.Date.Stamp,Hour)
data.ordered$Order <- seq.int(nrow(data.ordered))
########################################################################################################################
### Drop variables that won't be included in any models
data.rolling <- data.ordered %>% dplyr::select(-Date, -Date.Tomorrow, -Time.Date.Stamp, -Year, -Station.Name)
data.rolling <- data.rolling %>% arrange(Order)
### Making table with 24H rolling averages 
data.rolling <- data.rolling %>%
  group_by(Settlement.Point.Name, Settlement.Point.Type) %>%
  arrange(Order) %>%
  mutate(DAMPrice.Rolling = rollapply(data = DAM.Settlement.Point.Price,
                                      width = 24,
                                      FUN = mean,
                                      align = "right",
                                      fill = NA,
                                      na.rm = T
  ),
  WindSpeed.Rolling = rollapply(data = HOURLYWindSpeed.MEAN,
                                width = 24,
                                FUN = mean,
                                align = "right",
                                fill = NA,
                                na.rm = T
  ),
  DryBulb.Rolling = rollapply(data = HOURLYDRYBULBTEMPF.MEAN,
                              width = 24,
                              FUN = mean,
                              align = "right",
                              fill = NA,
                              na.rm = T
  ),
  WindDirection.Rolling = rollapply(data = HOURLYWindDirection.MEAN,
                                    width = 24,
                                    FUN = mean,
                                    align = "right",
                                    fill = NA,
                                    na.rm = T
  ),
  Visibility.Rolling = rollapply(data = HOURLYVISIBILITY.MEAN,
                                 width = 24,
                                 FUN = mean,
                                 align = "right",
                                 fill = NA,
                                 na.rm = T
  ),
  WetBulb.Rolling = rollapply(data = HOURLYWETBULBTEMPF.MEAN,
                              width = 24,
                              FUN = mean,
                              align = "right",
                              fill = NA,
                              na.rm = T
  ),
  Humidity.Rolling = rollapply(data = HOURLYRelativeHumidity.MEAN,
                               width = 24,
                               FUN = mean,
                               align = "right",
                               fill = NA,
                               na.rm = T
  ),
  StationPressure.Rolling = rollapply(data = HOURLYStationPressure.MEAN,
                                      width = 24,
                                      FUN = mean,
                                      align = "right",
                                      fill = NA,
                                      na.rm = T
  ),
  Precip.Rolling = rollapply(data = HOURLYPrecip.MEAN,
                             width = 24,
                             FUN = mean,
                             align = "right",
                             fill = NA,
                             na.rm = T
  ),
  RTMPrice.Rolling = rollapply(data = RTM.Settlement.Point.Price.MEAN.Today,
                               width = 24,
                               FUN = mean,
                               align = "right",
                               fill = NA,
                               na.rm = T
  )
  )

### Finally, remove NAs
data.rolling <- na.omit(data.rolling)
### Save the data
#write.csv(data, file = "windtopower_project.csv")
### RData is more compact than CSVs and preserves more information
save(data.full, data.modeling, data.IV, data.rolling, file = "windtopower_tables7.RData")
########################################################################################################################
########################################################################################################################
########################################################################################################################
### Plots
########################################################################################################################
########################################################################################################################
########################################################################################################################

ggplot(data.full, aes(x = Total.Wind.Output.MW, y = RTM.Settlement.Point.Price.MEAN.Tomorrow)) +
  geom_smooth(y ~ x)

library(stargazer)
mod <- lm(RTM.Settlement.Point.Price.MEAN.Tomorrow ~ Total.Wind.Output.MW, data = data.full)
mod2 <- lm(RTM.Settlement.Point.Price.MEAN.Tomorrow ~ Total.Wind.Output.MW + ERCOT.Load.MW + HOURLYDRYBULBTEMPF.MEAN + HOURLYWindSpeed.MEAN, data = data.full)

stargazer(mod, mod2, type = "text")

ggplot(data.full, aes(x = Date)) +
  geom_line(aes(y = DAM.Settlement.Point.Price, color = "royalblue1")) +
  geom_line(aes(y = RTM.Settlement.Point.Price.MEAN.Today, color = "red1", alpha = 0.4))

ggplot(data.full, aes(x = Total.Wind.Output.MW, y = HOURLYWindSpeed.MEAN, color = HOURLYDRYBULBTEMPF.MEAN)) +
  geom_point()


ggplot(data.full, aes(x = Time.Date.Stamp, y = HOURLYDRYBULBTEMPF.MEAN, color = Station.Name)) +
  geom_smooth() +
  geom_smooth(aes(y = ERCOT.Load.MW/500, color = "ERCOT Load MW")) +
  scale_y_continuous(sec.axis = sec_axis(~.*500, name = "ERCOT Load MW"))

ggplot(data.full, aes(x = Time.Date.Stamp, y = HOURLYDRYBULBTEMPF.MEAN)) +
  geom_smooth() +
  geom_smooth(aes(y = HOURLYWindSpeed.MEAN*8.5, color = "Wind Speed")) +
  scale_y_continuous(sec.axis = sec_axis(~./8.5, name = "Wind Speed"))

########################################################################################################################
########################################################################################################################
########################################################################################################################
### Lasso P1
########################################################################################################################
########################################################################################################################
########################################################################################################################
#load required packages and functions
source("DataAnalyticsFunctions.R")
installpkg("tidyverse")
library(tidyverse)
installpkg("dplyr")
library(dplyr)
installpkg("ggplot2")
library(ggplot2)
installpkg("caret")
library(caret)
installpkg("glmnet")
library(glmnet)
installpkg("MASS")
library(MASS)


#a function to calculate adjusted R square
myAR2 <- function(fit.col, real.col, model){
  residuals <- fit.col - real.col
  residuals.sq <- residuals^2
  SSE <- sum(residuals.sq)
  total <- real.col - mean(real.col)
  total.sq <- total^2
  TSS <- sum(total.sq)
  r.sq <- 1 - SSE/TSS
  p <- length(coef(model)) - 1
  n <- length(residuals)
  adj.r.sq <- 1-(1-r.sq)*((n-1)/(n-p-1))
  return(adj.r.sq)
}

#Load data
load('windtopower_tables7.RData')
df = as.data.frame(data.rolling)
df = na.omit(df)
df$Order = NULL

#Randomly dplyr::select half of the dataset
set.seed(101)
sample <- sample.int(n = nrow(df), size = floor(.50*nrow(df)), replace = F)
train_data = df[sample,]
test_data = df[-sample,]

#Create Matrix for lasso
Mx<- model.matrix(RTM.Settlement.Point.Price.MEAN.Tomorrow~ .^2, data=train_data)[,-1]
My<- train_data$RTM.Settlement.Point.Price.MEAN.Tomorrow

#Lasso and lambda theory calculation
num.features <- ncol(Mx)
num.n <- nrow(Mx)
w <-sd(My)
lambda.theory <- 2*w*sqrt(log(num.features/0.01)/num.n)
lassoTheory <- glmnet(Mx,My, family="gaussian",lambda = lambda.theory)
features.theory <- support(lassoTheory$beta)
data.theory <- data.frame(Mx[,features.theory],My)

#K-fold preperation
nfold <- 5
n <- nrow(train_data)
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]
foldid.fix = foldid

#Create empty dataframes of results
PL.OOS = data.frame(PL.theory=rep(NA,nfold)) 
L.OOS = data.frame(L.theory=rep(NA,nfold)) 
OOS = data.frame(regression=rep(NA,nfold), interaction=rep(NA,nfold), stepwise =rep(NA,nfold), null = rep(NA,nfold))


####### Start 5-fold Cross Validation #######
#Use a for loop to run through the 5-fold trails
for(k in 1:nfold){ 
  train <- which(foldid.fix!=k) # train on all but fold `k'
  
  ### This is the CV for the Post Lasso Estimates
  if ( length(features.theory) == 0){ 
    rtheory <- lm(RTM.Settlement.Point.Price.MEAN.Tomorrow~1, data=train_data, subset=train) 
  } else {rtheory <- lm(My~., data=data.theory, subset=train) }
  predtheory <- predict(rtheory, newdata=data.theory[-train,])
  PL.OOS$PL.theory[k] <- myAR2(predtheory,My[-train],rtheory)
  
  ### This is the CV for the Lasso estimates  
  lassoTheory <- glmnet(Mx[train,],My[train], lambda = lambda.theory)
  predlassotheory <- predict(lassoTheory, newx=Mx[-train,])
  L.OOS$L.theory[k] <- myAR2(predlassotheory,My[-train],lassoTheory)
  
  ### This is the CV for regression
  model.regression <- glm(RTM.Settlement.Point.Price.MEAN.Tomorrow~. , data=train_data, subset=train)
  pred.regression <- predict(model.regression, newdata=train_data[-train,])
  OOS$regression[k] <- myAR2(pred.regression, train_data$RTM.Settlement.Point.Price.MEAN.Tomorrow[-train], model.regression)
  
  ### This is the CV for regression with interaction
  model.interaction <-glm(RTM.Settlement.Point.Price.MEAN.Tomorrow ~.^2, data=train_data, subset=train)
  pred.interaction <- predict(model.interaction, newdata=train_data[-train,])
  OOS$interaction[k] <- myAR2(pred.interaction, train_data$RTM.Settlement.Point.Price.MEAN.Tomorrow[-train], model.interaction)
  
  ### This is the CV for stepwise  
  model.step = stepAIC(object = model.regression, direction = "both", trace = F)
  pred.step = predict(model.step,newdata = train_data[-train,]) 
  OOS$stepwise[k] <- myAR2(pred.step,train_data$RTM.Settlement.Point.Price.MEAN.Tomorrow[-train],model.step)
  
  ### This is the CV for null
  model.null <- glm(RTM.Settlement.Point.Price.MEAN.Tomorrow ~1 , data = train_data, subset = train)
  pred.null   <- predict(model.null, newdata=train_data[-train,])
  OOS$null[k] <- myAR2(pred.null, train_data$RTM.Settlement.Point.Price.MEAN.Tomorrow[-train], model.null)
  
  print(paste("Iteration",k,"of",nfold,"(thank you for your patience)"))
}

#Combine adjusted R-squares of all models into one matrix
R2performance <- cbind(PL.OOS,L.OOS,OOS)
m.OOS <- melt(t(as.matrix(R2performance)))

#Plot the result
ggplot(data = m.OOS, aes(x = Var2, y = value, fill = Var1)) +
  geom_col(position = "dodge") +
  ylab("OOS R-Squared") +
  xlab("Fold") +
  ylim(c(-0.02,0.3))+
  guides(fill = guide_legend(title = "Model"))

########################################################################################################################
########################################################################################################################
########################################################################################################################
### Lasso P2
########################################################################################################################
########################################################################################################################
########################################################################################################################

installpkg("tidyverse")
library(tidyverse)
installpkg("dplyr")
library(dplyr)
installpkg("ggplot2")
library(ggplot2)
installpkg("caret")
library(caret)
installpkg("glmnet")
library(glmnet)
installpkg("MASS")
library(MASS)

source("DataAnalyticsFunctions.R")

myAR2 <- function(fit.col, real.col, model){
  residuals <- fit.col - real.col
  residuals.sq <- residuals^2
  SSE <- sum(residuals.sq)
  total <- real.col - mean(real.col)
  total.sq <- total^2
  TSS <- sum(total.sq)
  r.sq <- 1 - SSE/TSS
  p <- length(coef(model)) - 1
  n <- length(residuals)
  adj.r.sq <- 1-(1-r.sq)*((n-1)/(n-p-1))
  return(adj.r.sq)
}

#load data
load('windtopower_tables7.RData')
df = as.data.frame(data.rolling)
df <- df[sample(nrow(df), 100000), ]
df <- df[ , -24]
df <- na.omit(df)

#Partition data to training and test sets
#set.seed(101)
#sample <- sample.int(n = nrow(df), size = floor(.40*nrow(df)), replace = F)
#train_data = df[sample,]
#test_data = df[-sample,]

#Create Matrix
Mx<- model.matrix(RTM.Settlement.Point.Price.MEAN.Tomorrow~ .^2, data=df)[,-1]
My<- df$RTM.Settlement.Point.Price.MEAN.Tomorrow

##Lasso and lambda determination
num.features <- ncol(Mx)
num.n <- nrow(Mx)
w <-sd(My)
lambda.theory <- 2*w*sqrt(log(num.features/0.01)/num.n)
lassoTheory <- glmnet(Mx,My, family="gaussian",lambda = lambda.theory)
lasso <- glmnet(Mx,My)

lassoCV <- cv.glmnet(Mx,My)
features.min <- support(lasso$beta[,which.min(lassoCV$cvm)])
length(features.min)
features.1se <- support(lasso$beta[,which.min( (lassoCV$lambda-lassoCV$lambda.1se)^2)])
length(features.1se) 
features.theory <- support(lassoTheory$beta)
length(features.theory)

data.min <- data.frame(Mx[,features.min],My)
data.1se <- data.frame(Mx[,features.1se],My)
data.theory <- data.frame(Mx[,features.theory],My)


# k-fold preperation
nfold <- 5
n <- nrow(df)
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]
foldid.fix = foldid

### create an empty dataframe of results
PL.OOS = data.frame(PL.min=rep(NA,nfold), PL.1se=rep(NA,nfold), PL.theory=rep(NA,nfold)) 
L.OOS = data.frame(L.min=rep(NA,nfold), L.1se=rep(NA,nfold), L.theory=rep(NA,nfold)) 
OOS = data.frame(regression=rep(NA,nfold), interaction=rep(NA,nfold), stepwise =rep(NA,nfold), null = rep(NA,nfold))


##################################Start K-fold of Lasso##################################
#Use a for loop to run through the nfold trails
for(k in 1:nfold){ 
  train <- which(foldid.fix!=k) # train on all but fold `k'
  
  ### This is the CV for the Post Lasso Estimates
  rmin <- lm(My~., data=data.min, subset=train)
  if (length(features.1se) == 0){  r1se <- lm(RTM.Settlement.Point.Price.MEAN.Tomorrow ~ 1, data=df, subset=train) 
  } else {r1se <- lm(My~., data=data.1se, subset=train)
  }
  if ( length(features.theory) == 0){ 
    rtheory <- lm(RTM.Settlement.Point.Price.MEAN.Tomorrow~1, data=df, subset=train) 
  } else {rtheory <- lm(My~., data=data.theory, subset=train) }
  
  predmin <- predict(rmin, newdata=data.min[-train,])
  pred1se  <- predict(r1se, newdata=data.1se[-train,])
  predtheory <- predict(rtheory, newdata=data.theory[-train,])
  PL.OOS$PL.min[k] <- myAR2(predmin,My[-train],rmin)
  PL.OOS$PL.1se[k] <- myAR2(pred1se,My[-train],r1se)
  PL.OOS$PL.theory[k] <- myAR2(predtheory,My[-train],rtheory)
  
  ### This is the CV for the Lasso estimates  
  lassomin  <- glmnet(Mx[train,],My[train], lambda = lassoCV$lambda.min)
  lasso1se  <- glmnet(Mx[train,],My[train], lambda = lassoCV$lambda.1se)
  lassoTheory <- glmnet(Mx[train,],My[train], lambda = lambda.theory)
  
  predlassomin <- predict(lassomin, newx=Mx[-train,])
  predlasso1se  <- predict(lasso1se, newx=Mx[-train,])
  predlassotheory <- predict(lassoTheory, newx=Mx[-train,])
  L.OOS$L.min[k] <- myAR2(predlassomin,My[-train],lassomin)
  L.OOS$L.1se[k] <- myAR2(predlasso1se,My[-train],lasso1se)
  L.OOS$L.theory[k] <- myAR2(predlassotheory,My[-train],lassoTheory)
  
  ## We will loop this nfold times (I setup for 10)
  ## this will print the progress (iteration that finished)
  print(paste("Iteration",k,"of",nfold,"(thank you for your patience)"))
}

##################################Start K-fold of regressions##################################
for(k in 1:nfold){ 
  train <- which(foldid.fix!=k) # train on all but fold `k'
  
  ### This is the CV for regression  
  model.regression <- glm(RTM.Settlement.Point.Price.MEAN.Tomorrow~. , data=df, subset=train)
  pred.regression <- predict(model.regression, newdata=df[-train,])
  OOS$regression[k] <- myAR2(pred.regression, df$RTM.Settlement.Point.Price.MEAN.Tomorrow[-train], model.regression)
  
  ### This is the CV for regression with interaction
  model.interaction <-glm(RTM.Settlement.Point.Price.MEAN.Tomorrow ~.^2, data=df, subset=train)
  pred.interaction <- predict(model.interaction, newdata=df[-train,])
  OOS$interaction[k] <- myAR2(pred.interaction, df$RTM.Settlement.Point.Price.MEAN.Tomorrow[-train], model.interaction)
  
  ### This is the CV for stepwise  
  model.step = stepAIC(object = model.regression, direction = "both", trace = F)
  pred.step = predict(model.step,newdata = df[-train,]) 
  OOS$stepwise[k] <- myAR2(pred.step,df$RTM.Settlement.Point.Price.MEAN.Tomorrow[-train],model.step)
  
  ### This is the CV for null
  model.null <- glm(RTM.Settlement.Point.Price.MEAN.Tomorrow ~1 , data = df, subset = train)
  pred.null   <- predict(model.null, newdata=df[-train,])
  OOS$null[k] <- myAR2(pred.null, df$RTM.Settlement.Point.Price.MEAN.Tomorrow[-train], model.null)
  
  ## We will loop this nfold times (I setup for 10)
  ## this will print the progress (iteration that finished)
  print(paste("Iteration",k,"of",nfold,"(thank you for your patience)"))
}

### Lets list the mean of the results stored in the dataframe OOS
### we have nfold values in OOS f or each model, this computes the mean of them)
R2performance <- cbind(PL.OOS,L.OOS,OOS)
m.OOS <- as.matrix(R2performance)
rownames(m.OOS) <- c(1:nfold)
barplot(t(as.matrix(OOS)), beside=TRUE, legend=TRUE, args.legend=c(xjust=1, yjust=0.5),
        ylab= bquote( "Out of Sample " ~ R^2), xlab="Fold", names.arg = c(1:5))
installpkg("reshape2")
library(reshape2)
m.OOS <- melt(t(as.matrix(R2performance)))

ggplot(data = m.OOS, aes(x = Var2, y = value, fill = Var1)) +
  geom_col(position = "dodge") +
  ylab("OOS R-Squared") +
  xlab("Fold") +
  guides(fill = guide_legend(title = "Model"))
model.eval.20.percent <- as.data.frame(R2performance)
########################################################################################################################
########################################################################################################################
########################################################################################################################
### Lasso P2
########################################################################################################################
########################################################################################################################
########################################################################################################################
installpkg("glmnet")
installpkg("keras")
load("windtopower_tables7.RData")
installpkg("dplyr")
library(dplyr)
installpkg("glmnet")
library(glmnet)
installpkg("keras")
library(keras)
install_keras()
### NN

### Create a market difference column measuring whether the RTM was above or below the DAM price
data.rolling$mkt.dif <- data.rolling$RTM.Settlement.Point.Price.MEAN.Tomorrow - data.rolling$DAM.Settlement.Point.Price
data.rolling$mkt.dif.scale <- scale(data.rolling$RTM.Settlement.Point.Price.MEAN.Tomorrow - data.rolling$DAM.Settlement.Point.Price)
data.rolling$mkt.dif.performance <- case_when(
  data.rolling$mkt.dif  < 0 ~ 0,
  data.rolling$mkt.dif  >= 0 ~ 1
)

### Accept only complete cases into the model
data.rolling <- data.rolling[complete.cases(data.rolling),]


holdout.indices <- sample(nrow(data.rolling), 50000)
nn.data.holdout <- data.rolling[holdout.indices,]
nn.data <- data.rolling[-holdout.indices,]

### define the matrix excluding RTM, total wind installed, settlement point type, wind direction, visibility, and order.
x.holdout<- model.matrix(mkt.dif.performance ~ . -RTM.Settlement.Point.Price.MEAN.Today - Total.Wind.Installed..MW - Settlement.Point.Type - HOURLYWindDirection.General - HOURLYPrecip.MEAN - One.Hr.MW.Percent.Change - HOURLYVISIBILITY.MEAN - Order - DAM.Settlement.Point.Price, data=nn.data.holdout)[,-1]
y.holdout<- nn.data.holdout$mkt.dif.performance

x.data<- model.matrix(mkt.dif.performance ~ . -RTM.Settlement.Point.Price.MEAN.Today - Total.Wind.Installed..MW - Settlement.Point.Type - HOURLYWindDirection.General - HOURLYPrecip.MEAN - One.Hr.MW.Percent.Change - HOURLYVISIBILITY.MEAN - Order - DAM.Settlement.Point.Price, data=nn.data)[,-1]
y.data<- nn.data$mkt.dif.performance

#rescale (to be between 0 and 1)
x_train <- x.data %*% diag(1/apply(x.data, 2, function(x) max(x, na.rm = TRUE)))
y_train <- as.numeric(y.data)
x_test <- x.holdout %*% diag(1/apply(x.data, 2, function(x) max(x, na.rm = TRUE)))
y_test <- as.numeric(y.holdout)

#rescale (unit variance and zero mean)
mean <- apply(x.data,2,mean)
std <- apply(x.data,2,sd)
x_train <- scale(x.data,center = mean, scale = std)
y_train <- as.numeric(y.data)
x_test <- scale(x.holdout,center = mean, scale = std)
y_test <- as.numeric(y.holdout)

num.inputs <- ncol(x_test)

### Create a model that includes 4 layers, regularization, and dropout.
model <- keras_model_sequential() %>%
  layer_dense(units=16,kernel_regularizer = regularizer_l1(0.001), activation="relu",input_shape = c(num.inputs)) %>%
  layer_dropout(rate=0.2) %>%
  layer_dense(units=16,kernel_regularizer = regularizer_l1(0.001), activation="relu") %>%
  layer_dropout(rate=0.2) %>%
  layer_dense(units=16,kernel_regularizer = regularizer_l1(0.001), activation="relu") %>%
  layer_dropout(rate=0.2) %>%
  layer_dense(units=1,activation="sigmoid")

summary(model)

model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

history <- model %>% fit(
  x_train, y_train,
  epochs = 10, batch_size = 128,
  validation_split = 0.2
)

results.NN1 <- model %>% evaluate(x_train,y_train)
results.NN1

results.NN1 <- model %>% evaluate(x_test,y_test)
results.NN1

pred.NN1 <- model%>% predict(x_test)



