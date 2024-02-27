library(DBI)
library(RSQLite)
library(tidyverse)

#############PLOTS DATA###############

##CAT DATA####

#CONNECT DATABASE
CAT_LFMC <- dbConnect(SQLite(), "raw_data/CAT_DATA/lfmc.sqlite")
#dbDisconnect(CAT_LFMC)

#EXTRACT DATABASE TABLES
TABLES<-dbListTables(CAT_LFMC)

for(i in 1:length(TABLES)){
  assign(TABLES[i], dbReadTable(CAT_LFMC, TABLES[i]))
}
#REMOVE EMPTY TABLES
rm("phenology","soil_measurements","tdr_sensor")

names(lfmc)
names(sites)
names(sites_species)
names(species)

#CHOOSE CORRECT CAT PLOTS
CAT_SITES<-data.frame(sites[1:9,])
str(CAT_SITES)

#TRANSFORM DATA TO CORRECT FORMAT 

CAT_SITES$Longitude<-gsub(",",".",CAT_SITES$Longitude)  #COMMA TO DOTS
CAT_SITES$Longitude<-gsub("\ua0","",CAT_SITES$Longitude)#REMOVE EMPTY SPACE
CAT_SITES$Longitude<-as.numeric(CAT_SITES$Longitude)    #SET AS NUMERIC

CAT_SITES$Latitude<-gsub(",",".",CAT_SITES$Latitude)  #COMMA TO DOTS
CAT_SITES$Latitude<-gsub("\ua0","",CAT_SITES$Latitude)#REMOVE EMPTY SPACE
CAT_SITES$Latitude<-as.numeric(CAT_SITES$Latitude)    #SET AS NUMERIC

str(CAT_SITES)

##FRENCH DATA####

FR_SITES<-read.csv("raw_data/FR_DATA/RH_Sites_Coordinates.csv", sep = ";")
str(FR_SITES)

FR_SITES$WGS84_Longitude<-gsub(",",".",FR_SITES$WGS84_Longitude)#COMMA TO DOTS
FR_SITES$WGS84_Longitude<-as.numeric(FR_SITES$WGS84_Longitude)  #SET AS NUMERIC

FR_SITES$WGS84_Latitude<-gsub(",",".",FR_SITES$WGS84_Latitude)#COMMA TO DOTS
FR_SITES$WGS84_Latitude<-as.numeric(FR_SITES$WGS84_Latitude)  #SET AS NUMERIC

str(FR_SITES)

##MERGE CAT AND FR DATA####

CAT_FR_SITES<-data.frame(ID = c(1:(nrow(CAT_SITES)+nrow(FR_SITES))),
                         site_name = c(CAT_SITES$LocalityName,FR_SITES$SiteCode),
                         LON = c(CAT_SITES$Longitude,FR_SITES$WGS84_Longitude),
                         LAT = c(CAT_SITES$Latitude,FR_SITES$WGS84_Latitude),
                         start_date = c(CAT_SITES$StartYear,FR_SITES$Starting_Date),
                         end_date = c(CAT_SITES$EndYear,FR_SITES$Last_Year))

str(CAT_FR_SITES)

write.csv(CAT_FR_SITES,"data/CAT_FR_SITES.csv", row.names = F)
