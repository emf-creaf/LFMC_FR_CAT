library(readxl)

##CAT VEGETATION DATA####
CAT_VEG_DATA <- read_excel("raw_data/CAT_DATA/SPIF_plotvegdata.xlsx", sheet = "vegetation_data")
str(CAT_VEG_DATA)

#FREMCH VEGETATION DATA####
FR_VEG_DATA<- read.csv("raw_data/FR_DATA/RH_Sites_coverage.csv", sep = ";")
str(FR_VEG_DATA)
FR_VEG_DATA$Date<-as.Date(FR_VEG_DATA$Date)

# SITES_FR_VEG<-as.data.frame(FR_VEG_DATA$Code_Site)
# SITES_FR_SITES<-as.data.frame(CAT_FR_SITES$SITE_NAME[CAT_FR_SITES$SOURCE=="FRA_RH"])




