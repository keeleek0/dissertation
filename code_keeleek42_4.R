##############################################################################
##############################################################################
#
#				"Dissertation keeleek42"
#       Part 4 - the new variables 1995 - 2018

#       Stefan Keel
#
#       Contact: keels@tcd.ie
#
#       Version: XXXX
#	 
#dataset (and creation) is available on github.com/keeleek42/dissertation
##############################################################################
##############################################################################

#remove objects
rm(list=ls())

#detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", 
                      "package:utils", "package:datasets", "package:methods", 
                      "package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, 
                                  TRUE, FALSE)]
  
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) 
    detach(package,  character.only=TRUE)
}

detachAllPackages()

#load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

#load any necessary packages/libraries
lapply(c('foreign', 
         'maptools', #CRAN v0.9-9
         'RColorBrewer', #CRAN v1.1-2
         'Matching', 
         'ebal', 
         'xtable',
         'survey', 
         'texreg', 
         'rgdal', #CRAN v1.4-8
         'dplyr',
         'ggplot2',
         'raster',
         'gpclib', #CRAN v1.5-6
         'rgeos',
         'readxl',
         'caret'),  #CRAN v0.5-2
       pkgTest) 

#setting the working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


##############################################################################
##############################################################################

##############################################################################
##############################################################################
#
# Code for Paper and Appendix
#
##############################################################################
##############################################################################

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Data Manipulation and Testing of new/more accurate variables
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#load data
data <- read_xlsx("Data/data_complete_ch.xlsx")
data <- as.data.frame(data)

#adding for tertiary sekund (higschool and baccalauratea) and null + basic
data$p_edu_tert_sekund <- apply(data[, c("p_edu_basic", "p_edu_matura", 
                                         "p_edu_null")], 1, sum)

data$p_edu_tert_sekund_m <- apply(data[, c("p_edu_basic_m", "p_edu_matura_m", 
                                           "p_edu_null_m")], 1, sum)


data$p_edu_null_basic <- apply(data[, c("p_edu_basic", "p_edu_null")], 1, sum)

data$p_edu_null_basic_m <- apply(data[, c("p_edu_basic_m", "p_edu_null_m")], 1, sum)

#rename
data <- data%>%
  rename("yes_p_per" = "Ja in %",
         "GPS_vshare" = "GPS",
         "SPS_vshare" = "SP",
         "CVP_vshare" = "CVP",
         "FDP_vshare" = "EVP",
         "SVP_vshare" = "SVP")

#factors:
data$treated <- as.factor(data$treated)
data$gender_mn <- as.factor(data$gender_mn)
data$damage_extent <- as.factor(data$damage_extent)

#only focus on data after 08-20-2018 (first protest of greta thunberg)
data <- data %>%
  filter(date <= as.Date("2018-08-20"))

#establish data
data <- data

#copy
adata0 <- data

#change units on some of the geographical variables
#source: Baccini and Leemann
adata0$kuenst.sh <- 100* (adata0$kuenstl/adata0$Punktflaeche)
adata0$wasser.sh <- 100* (adata0$wasser/adata0$Punktflaeche)
adata0$gras.sh <- 100* (adata0$gras/adata0$Punktflaeche)
adata0$gebuesch.sh <- 100* (adata0$gebuesch/adata0$Punktflaeche)
adata0$baum.sh <- 100* (adata0$baum/adata0$Punktflaeche)
adata0$vlose.sh <- 100* (adata0$vlose/adata0$Punktflaeche)
adata0$alti_mun_m <- 1000* (adata0$alti_mun_km)
adata0$alti_mun_m2 <- adata0$alti_mun_m^2
adata0$rainfall_surf_1000 <- 1000*adata0$rainfall_surf

adata0.2 <- adata0

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# PART ONE: INCOME
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#3 models for first table: Income
mod_3.1.1 <- lm(yes_p_per ~ treated + p_0_to_30 + p_30_to_40 + p_40_to_50 + 
                  p_50_to_75 + p_75_to_open +
                  factor(vote_code) + factor(mncode), data = adata0.2)

mod_3.1.2 <- lm(yes_p_per ~ treated + GPS_vshare + SPS_vshare + CVP_vshare + 
                  FDP_vshare + SVP_vshare + rainfall_1000 + vlose.sh + wasser.sh + 
                  gras.sh + kuenst.sh + 
                  factor(vote_code) + factor(mncode), data = adata0.2)

mod_3.1.3 <- lm(yes_p_per ~ treated + p_0_to_30 + p_30_to_40 + p_40_to_50 + 
                  p_50_to_75 + p_75_to_open +
                  GPS_vshare + SPS_vshare + CVP_vshare + 
                  FDP_vshare + SVP_vshare + rainfall_1000 + vlose.sh + wasser.sh + 
                  gras.sh + kuenst.sh +
                  factor(vote_code) + factor(mncode), data = adata0.2)


models_list_3.1.1 <- list(
  mod_3.1.1, 
  mod_3.1.2, 
  mod_3.1.3)

screenreg(models_list_3.1.1, stars = c(0.01, 0.05, 0.1), omit = "factor",
          custom.coef.names = c("Intercept", "Disaster", "Inc. =<30", "Inc. 30-40", "Inc. 40-50",
                                "Inc. 50-75", "Inc. 75<","Green Party %",
                                "Social Democrats %" , "Christian Democrats %", 
                                "Liberal Democrats %", "Swiss People's Party %",
                                "Rainfall", "No vegetation", "Share of Water", 
                                "Share of Gras", "Artificial" ), digits=2)

sink("Replication of Tables D.txt")
print("######")
print("######")
print("Table 1 - Income: before 2018-08-20")
print("######")
print("######")
screenreg(models_list_3.1.1, stars = c(0.01, 0.05, 0.1), omit = "factor",
          custom.coef.names = c("Intercept", "Disaster", "Inc. =<30", "Inc. 30-40", "Inc. 40-50",
                                "Inc. 50-75", "Inc. 75<","Green Party %",
                                "Social Democrats %" , "Christian Democrats %", 
                                "Liberal Democrats %", "Swiss People's Party %",
                                "Rainfall", "No vegetation", "Share of Water", 
                                "Share of Gras", "Artificial" ), digits=2)

sink()

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# PART TWO: AGE
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#3 models for first table: AGE
mod_3.2.1 <- lm(yes_p_per ~ treated + `p_18-30` + `p_31-45` + 
                  `p_46-65` + `p_66-85` + `p_86-100` +
                  factor(vote_code) + factor(mncode), data = adata0.2)

mod_3.2.3 <- lm(yes_p_per ~ treated + `p_18-30` + `p_31-45` + 
                  `p_46-65` + `p_66-85` + `p_86-100` +
                  GPS_vshare + SPS_vshare + CVP_vshare + 
                  FDP_vshare + SVP_vshare + rainfall_1000 + vlose.sh + wasser.sh + 
                  gras.sh + kuenst.sh +
                  factor(vote_code) + factor(mncode), data = adata0.2)


models_list_3.2.1 <- list(
  mod_3.2.1, 
  mod_3.1.2, 
  mod_3.2.3)

screenreg(models_list_3.2.1, stars = c(0.01, 0.05, 0.1), omit = "factor",
          custom.coef.names = c("Intercept", "Disaster", "Age 18-30", 
                                "Age 31-45", "Age 46-65", "Age 66-85", 
                                "Age 86-100",
                                "Green Party %", "Social Democrats %", "Christian Democrats %", 
                                "Liberal Democrats %", "Swiss People's Party %",
                                "Rainfall", "No vegetation", "Share of Water", 
                                "Share of Gras", "Artificial"), digits=2)

sink("Replication of Tables D.txt", append = TRUE)
print("######")
print("######")
print("Table 2 - Age: before 2018-08-20")
print("REFERENCE: AGE 0-17")
print("######")
print("######")
screenreg(models_list_3.2.1, stars = c(0.01, 0.05, 0.1), omit = "factor",
          custom.coef.names = c("Intercept", "Disaster", "Age 18-30", 
                                "Age 31-45", "Age 46-65", "Age 66-85", 
                                "Age 86-100",
                                "Green Party %", "Social Democrats %", "Christian Democrats %", 
                                "Liberal Democrats %", "Swiss People's Party %",
                                "Rainfall", "No vegetation", "Share of Water", 
                                "Share of Gras", "Artificial"), digits=2)

sink()

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# PART TWO: EDUCATION
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#3 models for first table: EDUCATION
mod_3.3.1 <- lm(yes_p_per ~ treated + 
                  p_edu_matura + p_edu_tert +
                  factor(vote_code) + factor(mncode), data = adata0.2)

summary(mod_3.3.1)

mod_3.3.3 <- lm(yes_p_per ~ treated + 
                  p_edu_matura + p_edu_tert +
                  GPS_vshare + SPS_vshare + CVP_vshare + 
                  FDP_vshare + SVP_vshare + rainfall_1000 + vlose.sh + wasser.sh + 
                  gras.sh + kuenst.sh +
                  factor(vote_code) + factor(mncode), data = adata0.2)


models_list_3.3.1 <- list(
  mod_3.3.1, 
  mod_3.1.2, 
  mod_3.3.3)

screenreg(models_list_3.3.1, stars = c(0.01, 0.05, 0.1), omit = "factor",
          custom.coef.names = c("Intercept", "Disaster", "Edu MATURA", "Edu TERTIARY", 
                                "Green Party %", "Social Democrats %", "Christian Democrats %", 
                                "Liberal Democrats %", "Swiss People's Party %",
                                "Rainfall", "No vegetation", "Share of Water", 
                                "Share of Gras", "Artificial"), digits=2)


sink("Replication of Tables D.txt", append = TRUE)
print("######")
print("######")
print("Table 3 - EDUCATION: before 2018-08-20")
print("REFERENCE: Edu BASIC")
print("######")
print("######")
screenreg(models_list_3.3.1, stars = c(0.01, 0.05, 0.1), omit = "factor",
          custom.coef.names = c("Intercept", "Disaster", "Edu MATURA", "Edu TERTIARY", 
                                "Green Party %", "Social Democrats %", "Christian Democrats %", 
                                "Liberal Democrats %", "Swiss People's Party %",
                                "Rainfall", "No vegetation", "Share of Water", 
                                "Share of Gras", "Artificial"), digits=2)

sink()

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# PART TWO: GENDER
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
adata0.2$gender_mn <- relevel(adata0.2$gender_mn, ref = "0")
#reference is male


#3 models for first table: Gender
mod_3.4.1 <- lm(yes_p_per ~ treated + gender_mn +
                  factor(vote_code) + factor(mncode), data = adata0.2)



mod_3.4.3 <- lm(yes_p_per ~ treated + gender_mn +
                  GPS_vshare + SPS_vshare + CVP_vshare + 
                  FDP_vshare + SVP_vshare + rainfall_1000 + vlose.sh + wasser.sh + 
                  gras.sh + kuenst.sh +
                  factor(vote_code) + factor(mncode), data = adata0.2)


models_list_3.4.1 <- list(
  mod_3.4.1, 
  mod_3.1.2, 
  mod_3.4.3)

screenreg(models_list_3.4.1, stars = c(0.01, 0.05, 0.1), omit = "factor",
          custom.coef.names = c("Intercept", "Disaster", "Female Share", 
                                "Green Party %", "Social Democrats %", "Christian Democrats %", 
                                "Liberal Democrats %", "Swiss People's Party %",
                                "Rainfall", "No vegetation", "Share of Water", 
                                "Share of Gras", "Artificial"), digits=2)


sink("Replication of Tables D.txt", append = TRUE)
print("######")
print("######")
print("Table 4 - Gender: before 2018-08-20")
print("REFERENCE: 1 = Female / 0 = Male")
print("######")
print("######")
screenreg(models_list_3.4.1, stars = c(0.01, 0.05, 0.1), omit = "factor",
          custom.coef.names = c("Intercept", "Disaster", "Female Share", 
                                "Green Party %", "Social Democrats %", "Christian Democrats %", 
                                "Liberal Democrats %", "Swiss People's Party %",
                                "Rainfall", "No vegetation", "Share of Water", 
                                "Share of Gras", "Artificial"), digits=2)

sink()

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# PART TWO: RELIGION
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#3 models for first table: Religion
mod_3.5.1 <- lm(yes_p_per ~ treated + p_ch_protest + p_ch_romancath + p_ch_christcath +
                  p_ch_christortho + p_ch_christother + p_ch_jewish + p_ch_muslim +
                  p_ch_religionother + p_ch_atheist +
                  factor(vote_code) + factor(mncode), data = adata0.2)



mod_3.5.3 <- lm(yes_p_per ~ treated + p_ch_protest + p_ch_romancath + p_ch_christcath +
                  p_ch_christortho + p_ch_christother + p_ch_jewish + p_ch_muslim +
                  p_ch_religionother + p_ch_atheist +
                  GPS_vshare + SPS_vshare + CVP_vshare + 
                  FDP_vshare + SVP_vshare + rainfall_1000 + vlose.sh + wasser.sh + 
                  gras.sh + kuenst.sh +
                  factor(vote_code) + factor(mncode), data = adata0.2)


models_list_3.5.1 <- list(
  mod_3.5.1, 
  mod_3.1.2, 
  mod_3.5.3)

screenreg(models_list_3.5.1, stars = c(0.01, 0.05, 0.1), omit = "factor",
          custom.coef.names = c("Intercept", "Disaster", "Protestant", "Catholic Roman",
                                "Catholic Christian", "Catholic Orthodox", "Catholic Other",
                                "Jewish", "Muslim", "Religion Other", "Atheist",
                                "Green Party %", "Social Democrats %", "Christian Democrats %", 
                                "Liberal Democrats %", "Swiss People's Party %",
                                "Rainfall", "No vegetation", "Share of Water", 
                                "Share of Gras", "Artificial"), digits=2)


sink("Replication of Tables D.txt", append = TRUE)
print("######")
print("######")
print("Table 5 - Religion: before 2018-08-20")
print("REFERENCE: Religion Undisclosed")
print("######")
print("######")
screenreg(models_list_3.5.1, stars = c(0.01, 0.05, 0.1), omit = "factor",
          custom.coef.names = c("Intercept", "Disaster", "Protestant", "Catholic Roman",
                                "Catholic Christian", "Catholic Orthodox", "Catholic Other",
                                "Jewish", "Muslim", "Religion Other", "Atheist",
                                "Green Party %", "Social Democrats %", "Christian Democrats %", 
                                "Liberal Democrats %", "Swiss People's Party %",
                                "Rainfall", "No vegetation", "Share of Water", 
                                "Share of Gras", "Artificial"), digits=2)

sink()

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# PART TWO: DAMAGE EXTENT
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#3 models for first table: Damage Extent
adata0.2$damage_extent <- relevel(adata0.2$damage_extent, ref = "1")

mod_3.6.1 <- lm(yes_p_per ~ treated + damage_extent +
                  factor(vote_code) + factor(mncode), data = adata0.2)



mod_3.6.3 <- lm(yes_p_per ~ treated + damage_extent +
                  GPS_vshare + SPS_vshare + CVP_vshare + 
                  FDP_vshare + SVP_vshare + rainfall_1000 + vlose.sh + wasser.sh + 
                  gras.sh + kuenst.sh +
                  factor(vote_code) + factor(mncode), data = adata0.2)


models_list_3.6.1 <- list(
  mod_3.6.1, 
  mod_3.1.2, 
  mod_3.6.3)

screenreg(models_list_3.6.1, stars = c(0.01, 0.05, 0.1), omit = "factor",
          custom.coef.names = c("Intercept", "Disaster", "Damage Extent 2", "Damage Extent 3",
                                "Green Party %", "Social Democrats %", "Christian Democrats %", 
                                "Liberal Democrats %", "Swiss People's Party %",
                                "Rainfall", "No vegetation", "Share of Water", 
                                "Share of Gras", "Artificial"), digits=2)


sink("Replication of Tables D.txt", append = TRUE)
print("######")
print("######")
print("Table 6 - Damage Extent: before 2018-08-20")
print("REFERENCE: Damage Extent 1 (LOW)")
print("######")
print("######")
screenreg(models_list_3.6.1, stars = c(0.01, 0.05, 0.1), omit = "factor",
          custom.coef.names = c("Intercept", "Disaster", "Damage Extent 2", "Damage Extent 3",
                                "Green Party %", "Social Democrats %", "Christian Democrats %", 
                                "Liberal Democrats %", "Swiss People's Party %",
                                "Rainfall", "No vegetation", "Share of Water", 
                                "Share of Gras", "Artificial"), digits=2)

sink()
