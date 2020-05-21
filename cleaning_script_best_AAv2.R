#===========================================================================================================#
#-----Smoking by Occupational Group Analysis Script #2----
# Author: Lydia Maher
# Last updated: 04/01/2017
#===========================================================================================================#

# The Smoking by Occupational Group project analyses the Health Survey for England dataset for the years 2001-2015. Analysis is comprised of two scripts, 2014_associations.R and cleaning_script_best.R. 

# 2014_associations.R looks at  the 2014 dataset and calculates associations between the predictors and the response. This informed our choice of variable in the analysis, but does not need to be re-run when examining the results of this project. If you are interested in further examining the entire dataset and the associations between all the predictors and the response, please see the "categorical_correlations" and "continuous_correlations" spreadsheets in the project folder. 

# This script, cleaning_script_best.R, takes the insights from 2014_associations.R and uses it to inform the final choice of predictors ("smoking status", "drinking status", "education", "general health", "sex", "age", "equivalised ", "year", "goverment region", "marital status", "occupational group"). It is important to note that data cleansing was undertaken by both myself and Anne Alarilla and thus the final dataset cannot be completely replicated by running this script. However, this script does include the steps undertaken to standardize variable names and values and the compilation of the different years into one dataset before cluster, psu and government region variables were added to the dataset. Outside of this analysis, the "marital status" variable was added and the individual years were compiled together the second time.

# To work with the final dataset and model, begin at "3. Final Dataset" and read in the full_data_v4.sav file. Steps for imputation are descirbed in that section, but were not actually used in the production of the final model (model_6).

#=======================#
#---- Contents ----
# 1. Housekeeping
#  a. Load libraries
#  b. Define functions
# 2. Data Cleansing
#  a. Read in Each Year as its Own Dataframe
#  b. Standardizing Variable Names and Values
#  c. Compiling All Years Together
# 3. Final Dataset
#  a. Dropping Unnecessary Variables
#  b. Imputing Variables
# 4. Model Selection
#  a. Plot of Random Effects
#=======================#

# ====================================================#
# --------- Housekeeping ---------------------
# ====================================================#
# Cleanup
rm(list=ls())

# --------- Load libraries --------------------
library(dplyr)
library(tidyr)
library(haven)
library(tidyverse)
library(Hmisc)
library(ltm)
library(nlme)
library(lme4)
library(bootstrap)
library(car)
library(RColorBrewer)
library(sjPlot)
library(xlsx)
library(lmtest)
library(foreign)
#LDM added in last package #AA agree 
# --------- Define Mode function --------------------
Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

# ==================================================================#
# --------- Data Cleansing --------------------
# ==================================================================#

# --------- Read in Each Year as its Own Dataframe --------------------
DataDirectory="G:/Cancer Information/Statistical Information Team/Information and Risk/Risk factor projects/Smoking prevalence R&M vs M&P/data/"
OutputDirectory="G:/Cancer Information/Statistical Information Team/Information and Risk/Risk factor projects/Smoking prevalence R&M vs M&P/output/"
setwd("G:/Cancer Information/Statistical Information Team/Information and Risk/Risk factor projects/Smoking prevalence R&M vs M&P/data")


###Load all files
sav.files  <- list.files(path=DataDirectory,
                         recursive=T,
                         pattern="*.sav"
                         ,full.names=F)

readSavFile <- function(f) {
  sav.fl <- read_sav(f, user_na = FALSE)
}

files_names <- c()
years <- c()
years_hes <- c()

for (i in 1:length(sav.files)){
  files_names[i] <-sub(".*[/](\\d{2})[/].*", "\\1", sav.files[i])
  years[i] <-sub(".*(\\d{2}).*", "\\1", sav.files[i])
  years_hes[i] <-paste0("hes_20", years[i])
}
years <- paste0("20", years)

for(i in sav.files){
  position <- which(sav.files == i)
  assign(years_hes[position], readSavFile(i))}

# --------- Standardizing Variable Names and Values --------------------

# some years are missing some variables while other have different names for the same variable
# these vectors contain the right names 
columns_2015 <- c("dnoft3", "topqual3", "genhelf2", "Sex", "eqv5", "smoking_1", "nssec3", "ag16g10", "Gor1", "origin2", "MarStatD", "cigsta3")
columns_2014 <- c("dnoft3", "topqual3", "genhelf2", "Sex", "eqv5", "smoking_1", "nssec3", "ag16g10", "gor1", "origin2", "MarStatD", "cigsta3")
columns_2013 <- c("dnoft3", "topqual3", "genhelf2", "Sex", "eqv5", "smoking_1", "nssec3", "ag16g10", "gor1", "Origin", "MarStatD", "cigsta3" )
columns_2012 <- c("dnoft3", "topqual3", "genhelf2", "Sex", "eqv5","smoking_1", "nssec3", "ag16g10", "gor1", "Origin", "marstatc", "cigsta3")
columns_2011 <- c("dnoft3", "topqual3", "genhelf2", "Sex", "eqv5", "smoking_1", "nssec3", "ag16g10", "gor1", "Origin", "marstatc", "cigsta3")
columns_2010 <- c("dnoft3", "topqual3", "genhelf2", "sex","eqv5", "smoking_1", "nssec3", "ag16g10", "gor", "origin", "marstatc", "cigsta3")
columns_2009 <- c("dnoft3", "topqual3", "genhelf2", "sex","eqv5", "smoking_1", "nssec3", "ag16g10", "GOR07","origin", "marstatc","cigsta3")
columns_2008 <- c("dnoft3", "topqual3", "genhelf2", "sex", "eqv5", "smoking_1", "nssec3","ag16g10", "GOR", "origin", "marstatc", "cigsta3")
columns_2007 <- c("dnoft2", "topqual3", "genhelf2", "sex","eqv5", "smoking_1", "nssec3", "ag16g10", "gor07","ethinda", "marstatc", "cigsta3")
columns_2006 <- c("dnoft2", "topqual3", "genhelf2", "sex", "eqv5", "smoking_1", "nssec3", "ag16g10", "gor06", "ethinda", "marstatc", "cigsta3")
columns_2005 <- c("dnoft2", "topqual3", "genhelf2", "sex", "eqv5", "smoking_1", "nssec3", "ag16g10", "gor", "ethinda", "marstatb", "cigsta3")
columns_2004 <- c("dnoft2", "topqual3", "genhelf2", "sex", "eqv5", "smoking_1", "nssec3", "ag16g10", "gor", "ethcind", "marstatb", "cigsta3")
columns_2003 <- c("dnoft2", "topqual3", "genhelf2", "sex", "eqv5", "smoking_1", "nssec3", "ag16g10", "gor", "ethnici", "marstatb", "cigsta3")
columns_2002 <- c("dnoft2", "topqual3", "genhelf2", "sex","eqv5", "smoking_1", "nssec3", "ag16g10", "gor", "ethnici", "marstatb", "cigsta3")
columns_2001 <- c("dnoft2", "topqual3", "genhelf2", "sex","eqv5", "smoking_1", "nssec3", "ag16g10", "gora", "ethnici", "marstatb", "cigsta3")
grep("gor1", colnames(hes_2011))
#hes_2014[, 980]

# selecting the right variables for each year based on the vectors created above
hes_2001 <- hes_2001[,columns_2001]
hes_2002 <- hes_2002[,columns_2002]
hes_2003 <- hes_2003[,columns_2003]
hes_2004 <- hes_2004[,columns_2004]
hes_2005 <- hes_2005[,columns_2005]
hes_2006 <- hes_2006[,columns_2006]
hes_2007 <- hes_2007[,columns_2007]
hes_2008 <- hes_2008[,columns_2008]
hes_2009 <- hes_2009[,columns_2009]
hes_2010 <- hes_2010[,columns_2010]
hes_2011 <- hes_2011[,columns_2011]
hes_2012 <- hes_2012[,columns_2012]
hes_2013 <- hes_2013[,columns_2013]
hes_2014 <- hes_2014[,columns_2014]
hes_2015 <- hes_2015[,columns_2015]

# add year to each dataset
hes_2001$year <- 2001
hes_2002$year <- 2002
hes_2003$year <- 2003
hes_2004$year <- 2004
hes_2005$year <- 2005
hes_2006$year <- 2006
hes_2007$year <- 2007
hes_2008$year <- 2008
hes_2009$year <- 2009
hes_2010$year <- 2010
hes_2011$year <- 2011
hes_2012$year <- 2012
hes_2013$year <- 2013
hes_2014$year <- 2014
hes_2015$year <- 2015

#add filler values to years which don't have certain values

colnames(hes_2007)[colnames(hes_2007) == "dnoft2"] <- "dnoft3"
colnames(hes_2006)[colnames(hes_2006) == "dnoft2"] <- "dnoft3"
colnames(hes_2005)[colnames(hes_2005) == "dnoft2"] <- "dnoft3"
colnames(hes_2004)[colnames(hes_2004) == "dnoft2"] <- "dnoft3"
colnames(hes_2003)[colnames(hes_2003) == "dnoft2"] <- "dnoft3"
colnames(hes_2002)[colnames(hes_2002) == "dnoft2"] <- "dnoft3"
colnames(hes_2001)[colnames(hes_2001) == "dnoft2"] <- "dnoft3"

colnames(hes_2014)[colnames(hes_2014) == "Sex"] <- "sex"
colnames(hes_2013)[colnames(hes_2013) == "Sex"] <- "sex"
colnames(hes_2012)[colnames(hes_2012) == "Sex"] <- "sex"
colnames(hes_2011)[colnames(hes_2011) == "Sex"] <- "sex"
colnames(hes_2015)[colnames(hes_2015) == "Sex"] <- "sex"


colnames(hes_2001)[colnames(hes_2001) == "gora"] <- "gor"
colnames(hes_2006)[colnames(hes_2006) == "gor06"] <- "gor"
colnames(hes_2007)[colnames(hes_2007) == "gor07"] <- "gor"
colnames(hes_2008)[colnames(hes_2008) == "GOR"] <- "gor"
colnames(hes_2009)[colnames(hes_2009) == "GOR07"] <- "gor"
colnames(hes_2011)[colnames(hes_2011) == "gor1"] <- "gor"
colnames(hes_2012)[colnames(hes_2012) == "gor1"] <- "gor"
colnames(hes_2013)[colnames(hes_2013) == "gor1"] <- "gor"
colnames(hes_2014)[colnames(hes_2014) == "gor1"] <- "gor"
colnames(hes_2015)[colnames(hes_2015) == "Gor1"] <- "gor"


colnames(hes_2001)[colnames(hes_2001) == "ethnici"] <- "ethnicity"
colnames(hes_2002)[colnames(hes_2002) == "ethnici"] <- "ethnicity"
colnames(hes_2003)[colnames(hes_2003) == "ethnici"] <- "ethnicity"
colnames(hes_2004)[colnames(hes_2004) == "ethcind"] <- "ethnicity"
colnames(hes_2005)[colnames(hes_2005) == "ethinda"] <- "ethnicity"
colnames(hes_2006)[colnames(hes_2006) == "ethinda"] <- "ethnicity"
colnames(hes_2007)[colnames(hes_2007) == "ethinda"] <- "ethnicity"
colnames(hes_2008)[colnames(hes_2008) == "origin"] <- "ethnicity"
colnames(hes_2009)[colnames(hes_2009) == "origin"] <- "ethnicity"
colnames(hes_2010)[colnames(hes_2010) == "origin"] <- "ethnicity"
colnames(hes_2011)[colnames(hes_2011) == "Origin"] <- "ethnicity"
colnames(hes_2012)[colnames(hes_2012) == "Origin"] <- "ethnicity"
colnames(hes_2013)[colnames(hes_2013) == "Origin"] <- "ethnicity"
colnames(hes_2014)[colnames(hes_2014) == "origin2"] <- "ethnicity"
colnames(hes_2015)[colnames(hes_2015) == "origin2"] <- "ethnicity"

colnames(hes_2006)[colnames(hes_2006) == "marstatc"] <- "marstatb"
colnames(hes_2007)[colnames(hes_2007) == "marstatc"] <- "marstatb"
colnames(hes_2008)[colnames(hes_2008) == "marstatc"] <- "marstatb"
colnames(hes_2009)[colnames(hes_2009) == "marstatc"] <- "marstatb"
colnames(hes_2010)[colnames(hes_2010) == "marstatc"] <- "marstatb"
colnames(hes_2011)[colnames(hes_2011) == "marstatc"] <- "marstatb"
colnames(hes_2012)[colnames(hes_2012) == "marstatc"] <- "marstatb"
colnames(hes_2013)[colnames(hes_2013) == "MarStatD"] <- "marstatb"
colnames(hes_2014)[colnames(hes_2014) == "MarStatD"] <- "marstatb"
colnames(hes_2015)[colnames(hes_2015) == "MarStatD"] <- "marstatb"

#recode variables#

hes_2001$gor<- gsub ("A", "1", hes_2001$gor, ignore.case = TRUE)
#LDM checked how this changes the labelling, doesn't at all, not sure if this is a problem or not? #AA don't think it is as long
#as we remember what they mean# 
hes_2001$gor<- gsub ("B", "2", hes_2001$gor, ignore.case = TRUE)
hes_2001$gor<- gsub ("D", "3", hes_2001$gor, ignore.case = TRUE)
hes_2001$gor<- gsub ("E", "4", hes_2001$gor, ignore.case = TRUE)
hes_2001$gor<- gsub ("F", "5", hes_2001$gor, ignore.case = TRUE)
hes_2001$gor<- gsub ("G", "6", hes_2001$gor, ignore.case = TRUE)
hes_2001$gor<- gsub ("H", "7", hes_2001$gor, ignore.case = TRUE)
hes_2001$gor<- gsub ("J", "8", hes_2001$gor, ignore.case = TRUE)
hes_2001$gor<- gsub ("K", "9", hes_2001$gor, ignore.case = TRUE)
hes_2001$gor<- gsub ("W", "10", hes_2001$gor, ignore.case = TRUE)
hes_2001$gor[hes_2001$gor == ""]<- NA #cases that are just spaces were recoded as NAs
hes_2001$gor[hes_2001$gor == "10"]<- NA #10 is Scotland so it's not relevant
table(hes_2001$gor)

hes_2002$gor<- gsub ("A", "1", hes_2002$gor, ignore.case = TRUE)
hes_2002$gor<- gsub ("B", "2", hes_2002$gor, ignore.case = TRUE)
hes_2002$gor<- gsub ("D", "3", hes_2002$gor, ignore.case = TRUE)
hes_2002$gor<- gsub ("E", "4", hes_2002$gor, ignore.case = TRUE)
hes_2002$gor<- gsub ("F", "5", hes_2002$gor, ignore.case = TRUE)
hes_2002$gor<- gsub ("G", "6", hes_2002$gor, ignore.case = TRUE)
hes_2002$gor<- gsub ("H", "7", hes_2002$gor, ignore.case = TRUE)
hes_2002$gor<- gsub ("J", "8", hes_2002$gor, ignore.case = TRUE)
hes_2002$gor<- gsub ("K", "9", hes_2002$gor, ignore.case = TRUE)
table(hes_2002$gor)

hes_2010$gor<- gsub ("A", "1", hes_2010$gor, ignore.case = TRUE)
hes_2010$gor<- gsub ("B", "2", hes_2010$gor, ignore.case = TRUE)
hes_2010$gor<- gsub ("D", "3", hes_2010$gor, ignore.case = TRUE)
hes_2010$gor<- gsub ("E", "4", hes_2010$gor, ignore.case = TRUE)
hes_2010$gor<- gsub ("F", "5", hes_2010$gor, ignore.case = TRUE)
hes_2010$gor<- gsub ("G", "6", hes_2010$gor, ignore.case = TRUE)
hes_2010$gor<- gsub ("H", "7", hes_2010$gor, ignore.case = TRUE)
hes_2010$gor<- gsub ("J", "8", hes_2010$gor, ignore.case = TRUE)
hes_2010$gor<- gsub ("K", "9", hes_2010$gor, ignore.case = TRUE)
table(hes_2002$gor)


hes_2006$marstatb <- gsub("3", "2", hes_2006$marstatb)
hes_2006$marstatb <- gsub("4", "3", hes_2006$marstatb)
hes_2006$marstatb <- gsub("5", "4", hes_2006$marstatb)
hes_2006$marstatb <- gsub("6", "5", hes_2006$marstatb)
hes_2006$marstatb <- gsub("7", "6", hes_2006$marstatb)
#LDM what was catgeory 3 originally? just making sure that it's a good idea to combine this category into another
#LDM looks good otherwise
#AA will double check coding#

hes_2007$marstatb <- gsub("3", "2", hes_2007$marstatb)
hes_2007$marstatb <- gsub("4", "3", hes_2007$marstatb)
hes_2007$marstatb <- gsub("5", "4", hes_2007$marstatb)
hes_2007$marstatb <- gsub("6", "5", hes_2007$marstatb)
hes_2007$marstatb <- gsub("7", "6", hes_2007$marstatb)

hes_2008$marstatb <- gsub("3", "2", hes_2008$marstatb)
hes_2008$marstatb <- gsub("4", "3", hes_2008$marstatb)
hes_2008$marstatb <- gsub("5", "4", hes_2008$marstatb)
hes_2008$marstatb <- gsub("6", "5", hes_2008$marstatb)
hes_2008$marstatb <- gsub("7", "6", hes_2008$marstatb)

hes_2009$marstatb <- gsub("3", "2", hes_2009$marstatb)
hes_2009$marstatb <- gsub("4", "3", hes_2009$marstatb)
hes_2009$marstatb <- gsub("5", "4", hes_2009$marstatb)
hes_2009$marstatb <- gsub("6", "5", hes_2009$marstatb)
hes_2009$marstatb <- gsub("7", "6", hes_2009$marstatb)

hes_2010$marstatb <- gsub("3", "2", hes_2010$marstatb)
hes_2010$marstatb <- gsub("4", "3", hes_2010$marstatb)
hes_2010$marstatb <- gsub("5", "4", hes_2010$marstatb)
hes_2010$marstatb <- gsub("6", "5", hes_2010$marstatb)
hes_2010$marstatb <- gsub("7", "6", hes_2010$marstatb)

hes_2011$marstatb <- gsub("3", "2", hes_2011$marstatb)
hes_2011$marstatb <- gsub("4", "3", hes_2011$marstatb)
hes_2011$marstatb <- gsub("5", "4", hes_2011$marstatb)
hes_2011$marstatb <- gsub("6", "5", hes_2011$marstatb)
hes_2011$marstatb <- gsub("7", "6", hes_2011$marstatb)

hes_2012$marstatb <- gsub("3", "2", hes_2012$marstatb)
hes_2012$marstatb <- gsub("4", "3", hes_2012$marstatb)
hes_2012$marstatb <- gsub("5", "4", hes_2012$marstatb)
hes_2012$marstatb <- gsub("6", "5", hes_2012$marstatb)
hes_2012$marstatb <- gsub("7", "6", hes_2012$marstatb)


hes_2001$ethnicity<- ifelse(hes_2001$ethnicity == "1", "0", "1")
hes_2002$ethnicity<- ifelse(hes_2002$ethnicity == "1", "0", "1")
hes_2003$ethnicity<- ifelse(hes_2003$ethnicity == "1", "0", "1")
hes_2004$ethnicity<- ifelse(hes_2004$ethnicity == "1", "0", "1")
hes_2005$ethnicity<- ifelse(hes_2005$ethnicity == "1", "0", "1")
hes_2006$ethnicity<- ifelse(hes_2006$ethnicity == "1", "0", "1")
hes_2007$ethnicity<- ifelse(hes_2007$ethnicity == "1", "0", "1")


hes_2008$ethnicity <- gsub("2", "1", hes_2008$ethnicity)
hes_2008$ethnicity <- gsub("3", "1", hes_2008$ethnicity)
hes_2008$ethnicity<- ifelse(hes_2008$ethnicity == "1", "0", "1")


hes_2009$ethnicity <- gsub("2", "1", hes_2009$ethnicity)
hes_2009$ethnicity <- gsub("3", "1", hes_2009$ethnicity)
hes_2009$ethnicity<- ifelse(hes_2009$ethnicity == "1", "0", "1")


hes_2010$ethnicity <- gsub("2", "1", hes_2010$ethnicity)
hes_2010$ethnicity <- gsub("3", "1", hes_2010$ethnicity)
hes_2010$ethnicity<- ifelse(hes_2010$ethnicity == "1", "0", "1")

hes_2011$ethnicity <- gsub("2", "1", hes_2011$ethnicity)
hes_2011$ethnicity <- gsub("3", "1", hes_2011$ethnicity)
hes_2011$ethnicity <- gsub("4", "1", hes_2011$ethnicity)
hes_2011$ethnicity<- ifelse(hes_2011$ethnicity == "1", "0", "1")

hes_2012$ethnicity <- gsub("2", "1", hes_2012$ethnicity)
hes_2012$ethnicity <- gsub("3", "1", hes_2012$ethnicity)
hes_2012$ethnicity <- gsub("4", "1", hes_2012$ethnicity)
hes_2012$ethnicity<- ifelse(hes_2012$ethnicity == "1", "0", "1")

hes_2013$ethnicity <- gsub("2", "1", hes_2013$ethnicity)
hes_2013$ethnicity <- gsub("3", "1", hes_2013$ethnicity)
hes_2013$ethnicity <- gsub("4", "1", hes_2013$ethnicity)
hes_2013$ethnicity<- ifelse(hes_2013$ethnicity == "1", "0", "1")

hes_2014$ethnicity<- ifelse(hes_2014$ethnicity == "1", "0", "1")
hes_2015$ethnicity<- ifelse(hes_2015$ethnicity == "1", "0", "1")

hes_2004$eqv5[hes_2004$eqv5== "-1"]<-NA 
hes_2011$eqv5[hes_2011$eqv5 == "-90"]<-NA 
#LDM what's the reasoning for turning these values into NAs? #AA cos they were already NA's anyway just not being recognised) 



# --------- Compiling All Years Together --------------------
full_data <- rbind(hes_2015, hes_2014, hes_2013, hes_2012, hes_2011, hes_2010, hes_2009, hes_2008, hes_2007, hes_2006, hes_2005, hes_2004, hes_2003, hes_2002, hes_2001)

test_data<- full_data

test_data2<- as.data.frame(full_data)

# ==================================================================#
# --------- Final Dataset --------------------
# ==================================================================#


#changing the data type of all the variables as they are not factors so that it treats the variables as they should be# 

test_data$dnoft3<- as.factor(test_data$dnoft3)
test_data$topqual3<- as.factor(test_data$topqual3)
test_data$genhelf2<- as.factor(test_data$genhelf2)
test_data$sex<- as.factor(test_data$sex)
test_data$eqv5<-as.factor(test_data$eqv5)
test_data$nssec3<- as.factor(test_data$nssec3)
test_data$ag16g10<- as.factor(test_data$ag16g10)
test_data$gor <- as.factor(test_data$gor)
test_data$marstatb <- as.factor(test_data$marstatb)
test_data$ethnicity<- as.factor(test_data$ethnicity)

full_data2<-test_data

full_data3<-as.data.frame(full_data2)





#LDM agree that year isn't a factor
#LDM can confirm all other variables turned into factors and included in model 

# --------- Imputing Variables --------------------

# NA crosstabs, to see how evenly NAs are spread and whether imputation is appropriate

# table(full_data_dropped$nssec3, full_data_dropped$smoking_1, useNA = "ifany")
# table(full_data_dropped$nssec3, full_data_dropped$dnoft3, useNA = "ifany")
# table(full_data_dropped$nssec3, full_data_dropped$topqual3, useNA = "ifany")
# table(full_data_dropped$nssec3, full_data_dropped$genhelf2, useNA = "ifany")
# table(full_data_dropped$nssec3, full_data_dropped$Sex, useNA = "ifany")
# table(full_data_dropped$nssec3, full_data_dropped$ag16g10, useNA = "ifany")
# table(full_data_dropped$nssec3, full_data_dropped$eqv5, useNA = "ifany")
# table(full_data_dropped$nssec3, full_data_dropped$gor, useNA = "ifany")
# table(full_data_dropped$nssec3, full_data_dropped$marstatb, useNA = "ifany")
# 
# full_data_imputation <- full_data_dropped
# 
# drinking_mode <-Mode(full_data_dropped$dnoft3, na.rm = TRUE)
# topqual_mode <-Mode(full_data_dropped$topqual3, na.rm = TRUE)
# genhelf_mode <-Mode(full_data_dropped$genhelf2, na.rm = TRUE)
# age_mode <-Mode(full_data_dropped$ag16g10, na.rm = TRUE)
# eqv_mode <-Mode(full_data_dropped$eqv5, na.rm = TRUE)
# gor_mode <-Mode(full_data_dropped$gor, na.rm = TRUE)
# marstat_mode <-Mode(full_data_dropped$marstatb, na.rm = TRUE)
# nssec_mode <-Mode(full_data_dropped$nssec3, na.rm = TRUE)
# 
# names_list <- c("dnoft3", "topqual3", "genhelf2", "ag16g10", "eqv5","gor", "marstatb", "nssec3")
# test_imputation <- full_data_imputation[, names_list]
# imputation_list <- c(drinking_mode, topqual_mode, genhelf_mode, age_mode, eqv_mode, gor_mode, marstat_mode, nssec_mode)
# 
# # for loop to impute all the modes calculated above into a "test" 
# for (i in 1:ncol(test_imputation)){
#   for(x in 1:nrow(full_data_imputation)){
#     if(is.na(test_imputation[x, i]) == TRUE){
#       test_imputation[x, i] <- imputation_list[i]
#     }
#   }
# }
# 
# full_data_imputation$dnoft3 <- test_imputation$dnoft3
# full_data_imputation$topqual3 <- test_imputation$topqual3
# full_data_imputation$genhelf2 <- test_imputation$genhelf2
# full_data_imputation$ag16g10 <- test_imputation$ag16g10
# full_data_imputation$eqv5 <- test_imputation$eqv5
# full_data_imputation$gor <- test_imputation$gor
# full_data_imputation$marstatb <- test_imputation$marstatb
# full_data_imputation$nssec3 <- test_imputation$nssec3

# ==================================================================#
# --------- Model Selection --------------------
# ==================================================================#
# this is the final model, denoted model_6 simply due to previous iterations which have not been included in this script
# this model includes nssec3 as a random intercept and nssec*year as a random slope and all the other variables as fixed effects
model_9 <- glmer(smoking_1 ~ dnoft3 + topqual3 + genhelf2 + sex + ag16g10 +eqv5 + year + gor +marstatb+ ethnicity+ (1 + year|nssec3) , data = na.omit(full_data2), na.action = na.exclude, family = "binomial", REML = FALSE)
summary(model_9)


# save as RDS so you can quickly re-load instead of having to continually recreate the model if running multiple times
saveRDS(model_7, "../model_v1.rds")

#to write out datasets for further examination
write.csv(full_data_dropped, file = "non_imputed.csv")
write.csv(full_data_imputation, file = "imputed.csv")

#ranef returns the estimated deviation
ranef(model_7)

#coef returns the 'summed up' version -i.e. if you also have a fixed effect, it sums the combining the fixed effects with the random effects (zero centred)
coef(model_7)

## Checking Assumptions 
# not very relevant due to being a logistic regression
# homogeneity of variance
plot(model_7)

# normality of variance
qqnorm((resid(model_7)))

## Likelihood Ratio Test to assess significance of random terms in model
#m7 <- glmer(smoking_1 ~ dnoft3 + topqual3 + genhelf2 + Sex + ag16g10 +eqv5 + year + marstatb  +gor+ ethnicity+ (1 + year|nssec3) , data = na.omit(full_data_dropped), family = "binomial", REML = FALSE)
m1 <- update(model_7, .~dnoft3 + topqual3 + genhelf2 + sex + ag16g10 +eqv5 + year +gor+ marstatb+ ethnicity + (1|nssec3))
m2 <- update(model_7, .~dnoft3 + topqual3 + genhelf2 + sex + ag16g10 +eqv5 + year +gor+ marstatb+ ethnicity + (year-1|nssec3))
m0 <- glm(smoking_1 ~ dnoft3 + topqual3 + genhelf2 + sex + ag16g10 +eqv5 + year + marstatb +gor + ethnicity, data = na.omit(full_data2), na.action = na.exclude, family = "binomial")

model8_1 <- update(model_8, .~dnoft3 + topqual3 + genhelf2 + sex + ag16g10 +eqv5 + year +gor+ marstatb+ (1|nssec3))
model8_0 <- glm(smoking_1 ~ dnoft3 + topqual3 + genhelf2 + sex + ag16g10 +eqv5 + year + marstatb +gor, data = na.omit(full_data2), na.action = na.exclude, family = "binomial")
saveRDS(model8_1, "../model_v8.rds")


anova(model_8, model8_1, model8_0) #everything a factor apart from year#

lrtest(m0, model_7)
lrtest(m0, m1)
lrtest(m1, model_7)

#saveRDS(model_8, "../model_8_noethnicity.rds")
#saveRDS(model_8, "../model_8_m1_noethnicity.rds")
#saveRDS(model_8, "../model_8_m1_noethnicity.rds")

#saveRDS(m0, "m0.rds")
#save as RDS so you can quickly re-load instead of having to continually recreate the model if running multiple times
model_7 <-readRDS("model_v1.rds")
m1 <- readRDS("model_v1_noslope.rds")
saveRDS(m6, "m6.rds")
#saveRDS(m1, "m1.rds")
#m6 <- readRDS("m6.rds")
#m1 <- readRDS("m1.rds")

#recreating the model after fixing ethnicity coding# 
model_9 <- glmer(smoking_1 ~ dnoft3 + topqual3 + genhelf2 + sex + ag16g10 +eqv5 + year + gor +marstatb+ ethnicity+ (1 + year|nssec3) , data = na.omit(full_data2), na.action = na.exclude, family = "binomial", REML = FALSE)
summary(model_9)
saveRDS(model_9, "../model_9.rds")

m3 <- update(model_9, .~dnoft3 + topqual3 + genhelf2 + sex + ag16g10 +eqv5 + year +gor+ marstatb+ ethnicity + (1|nssec3))
saveRDS(m3, "../model_9RI.rds")

m4 <- glm(smoking_1 ~ dnoft3 + topqual3 + genhelf2 + sex + ag16g10 +eqv5 + year + marstatb +gor + ethnicity, data = na.omit(full_data2), na.action = na.exclude, family = "binomial")
saveRDs(m4, "../model_9FE.rds" )
anova(model_9, m3, m4)


model<- readRDS("model_9.rds")
summary(model)

basic<- readRDS("m0.rds")
summary (basic)

m3<- readRDS("model_9RI.rds")
summary(m3)

m4<- readRDS("model_9FE.rds")
summary(m4)

anova(model, m3, basic)


# --------- Plot of Random Effects --------------------
sjp.glmer(m1, facet.grid = FALSE, type = "re", show.ci = TRUE)
