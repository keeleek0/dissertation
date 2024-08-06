##############################################################################
##############################################################################
#
#				"Dissertation keeleek0"
#       Part 5 full - plugging in all variables
#
#       Stefan Keel
#
#       Contact: keels@tcd.ie
#
#       Version: 2024-07-23
#	 
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
#packages and versions
packages_used <- c('texreg', # 1.39-3
                   'dplyr', # 1.1-3
                   'readxl') # 1.4-3

#load packages
lapply(packages_used, pkgTest)

#check and print versions of the packages
#if any troubles arise it might be due to versions
#see above which versions were used.
package_versions <- sapply(packages_used, packageVersion)
print(package_versions)

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

#reducing the model
adata0 <- adata0[adata0$vote_code!=103 & 
                   adata0$vote_code!=105 & 
                   adata0$vote_code!=110,]

adata0.2 <- adata0

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# PART ONE: Full
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#relevel
adata0.2$damage_extent <- relevel(adata0.2$damage_extent, ref = "1")


#3 models for first table: Income
mod_5.1.1 <- lm(yes_p_per ~ treated + p_0_to_30 + p_30_to_40 + p_40_to_50 + 
                  p_50_to_75 + p_75_to_open +
                  `p_18-30` + `p_31-45` + 
                  `p_46-65` + `p_66-85` + `p_86-100` + 
                  btert_share + btertII_share +
                  gender_mn + p_ch_protest + p_ch_romancath + p_ch_christcath +
                  p_ch_christortho + p_ch_christother + p_ch_jewish + p_ch_muslim +
                  p_ch_religionother + p_ch_atheist +
                  damage_extent +
                  factor(vote_code) + factor(mncode), data = adata0.2)

mod_5.1.2 <- lm(yes_p_per ~ treated + GPS_vshare + SPS_vshare + CVP_vshare + 
                  FDP_vshare + SVP_vshare + rainfall_1000 + vlose.sh + wasser.sh + 
                  gras.sh + kuenst.sh + 
                  factor(vote_code) + factor(mncode), data = adata0.2)

mod_5.1.3 <- lm(yes_p_per ~ treated + p_0_to_30 + p_30_to_40 + p_40_to_50 + 
                  p_50_to_75 + p_75_to_open +
                  `p_18-30` + `p_31-45` + 
                  `p_46-65` + `p_66-85` + `p_86-100` + 
                  btert_share + btertII_share +
                  gender_mn + p_ch_protest + p_ch_romancath + p_ch_christcath +
                  p_ch_christortho + p_ch_christother + p_ch_jewish + p_ch_muslim +
                  p_ch_religionother + p_ch_atheist +
                  damage_extent +
                  GPS_vshare + SPS_vshare + CVP_vshare + 
                  FDP_vshare + SVP_vshare + rainfall_1000 + vlose.sh + wasser.sh + 
                  gras.sh + kuenst.sh +
                  factor(vote_code) + factor(mncode), data = adata0.2)


models_list_5.1.1 <- list(
  mod_5.1.1, 
  mod_5.1.2, 
  mod_5.1.3)

screenreg(models_list_5.1.1, stars = c(0.01, 0.05, 0.1), omit = "factor",
          custom.coef.names = c("Intercept", "Disaster", "Inc. =<30", 
                                "Inc. 30-40", "Inc. 40-50",
                                "Inc. 50-75", "Inc. 75<",
                                "Age 18-30", 
                                "Age 31-45", "Age 46-65", "Age 66-85", 
                                "Age 86-100", 
                                "Tertiary", "Tert II",
                                "Female",
                                "Protestant", "Catholic Roman",
                                "Catholic Christian", "Catholic Orthodox", "Catholic Other",
                                "Jewish", "Muslim", "Religion Other", "Atheist",
                                "Damage Extent 2", "Damage Extent 3",
                                "Green Party %",
                                "Social Democrats %" , "Christian Democrats %", 
                                "Liberal Democrats %", "Swiss People's Party %",
                                "Rainfall", "No vegetation", "Share of Water", 
                                "Share of Gras", "Artificial" ), digits=2)

sink("Replication of Tables E.txt")
print("######")
print("######")
print("Table 1 - Full")
print("######")
print("######")
screenreg(models_list_5.1.1, stars = c(0.01, 0.05, 0.1), omit = "factor",
          custom.coef.names = c("Intercept", "Disaster", "Inc. =<30", 
                                "Inc. 30-40", "Inc. 40-50",
                                "Inc. 50-75", "Inc. 75<",
                                "Age 18-30", 
                                "Age 31-45", "Age 46-65", "Age 66-85", 
                                "Age 86-100", 
                                "Tertiary", "Tert II",
                                "Female",
                                "Protestant", "Catholic Roman",
                                "Catholic Christian", "Catholic Orthodox", "Catholic Other",
                                "Jewish", "Muslim", "Religion Other", "Atheist",
                                "Damage Extent 2", "Damage Extent 3",
                                "Green Party %",
                                "Social Democrats %" , "Christian Democrats %", 
                                "Liberal Democrats %", "Swiss People's Party %",
                                "Rainfall", "No vegetation", "Share of Water", 
                                "Share of Gras", "Artificial" ), digits=2)

sink()

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# PART TWO: 1995 - 2018
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#only focus on data before 08-20-2018 (first protest of greta thunberg)
adata0.3 <- adata0.2 %>%
  filter(date <= ("2018-08-20"))

#3 models for first table: Income
mod_5.2.1 <- lm(yes_p_per ~ treated + p_0_to_30 + p_30_to_40 + p_40_to_50 + 
                  p_50_to_75 + p_75_to_open +
                  `p_18-30` + `p_31-45` + 
                  `p_46-65` + `p_66-85` + `p_86-100` + 
                  btert_share + btertII_share +
                  gender_mn + p_ch_protest + p_ch_romancath + p_ch_christcath +
                  p_ch_christortho + p_ch_christother + p_ch_jewish + p_ch_muslim +
                  p_ch_religionother + p_ch_atheist +
                  damage_extent +
                  factor(vote_code) + factor(mncode), data = adata0.3)

mod_5.2.2 <- lm(yes_p_per ~ treated + GPS_vshare + SPS_vshare + CVP_vshare + 
                  FDP_vshare + SVP_vshare + rainfall_1000 + vlose.sh + wasser.sh + 
                  gras.sh + kuenst.sh + 
                  factor(vote_code) + factor(mncode), data = adata0.3)

mod_5.2.3 <- lm(yes_p_per ~ treated + p_0_to_30 + p_30_to_40 + p_40_to_50 + 
                  p_50_to_75 + p_75_to_open +
                  `p_18-30` + `p_31-45` + 
                  `p_46-65` + `p_66-85` + `p_86-100` + 
                  btert_share + btertII_share +
                  gender_mn + p_ch_protest + p_ch_romancath + p_ch_christcath +
                  p_ch_christortho + p_ch_christother + p_ch_jewish + p_ch_muslim +
                  p_ch_religionother + p_ch_atheist +
                  damage_extent +
                  GPS_vshare + SPS_vshare + CVP_vshare + 
                  FDP_vshare + SVP_vshare + rainfall_1000 + vlose.sh + wasser.sh + 
                  gras.sh + kuenst.sh +
                  factor(vote_code) + factor(mncode), data = adata0.3)


models_list_5.2.1 <- list(
  mod_5.2.1, 
  mod_5.2.2, 
  mod_5.2.3)

screenreg(models_list_5.2.1, stars = c(0.01, 0.05, 0.1), omit = "factor",
          custom.coef.names = c("Intercept", "Disaster", "Inc. =<30", 
                                "Inc. 30-40", "Inc. 40-50",
                                "Inc. 50-75", "Inc. 75<",
                                "Age 18-30", 
                                "Age 31-45", "Age 46-65", "Age 66-85", 
                                "Age 86-100", 
                                "Tertiary", "Tert II",
                                "Female",
                                "Protestant", "Catholic Roman",
                                "Catholic Christian", "Catholic Orthodox", "Catholic Other",
                                "Jewish", "Muslim", "Religion Other", "Atheist",
                                "Damage Extent 2", "Damage Extent 3",
                                "Green Party %",
                                "Social Democrats %" , "Christian Democrats %", 
                                "Liberal Democrats %", "Swiss People's Party %",
                                "Rainfall", "No vegetation", "Share of Water", 
                                "Share of Gras", "Artificial" ), digits=2)

sink("Replication of Tables E.txt", append = TRUE)
print("######")
print("######")
print("Table 1 - Full: Before 2018")
print("######")
print("######")
screenreg(models_list_5.2.1, stars = c(0.01, 0.05, 0.1), omit = "factor",
          custom.coef.names = c("Intercept", "Disaster", "Inc. =<30", 
                                "Inc. 30-40", "Inc. 40-50",
                                "Inc. 50-75", "Inc. 75<",
                                "Age 18-30", 
                                "Age 31-45", "Age 46-65", "Age 66-85", 
                                "Age 86-100", 
                                "Tertiary", "Tert II",
                                "Female",
                                "Protestant", "Catholic Roman",
                                "Catholic Christian", "Catholic Orthodox", "Catholic Other",
                                "Jewish", "Muslim", "Religion Other", "Atheist",
                                "Damage Extent 2", "Damage Extent 3",
                                "Green Party %",
                                "Social Democrats %" , "Christian Democrats %", 
                                "Liberal Democrats %", "Swiss People's Party %",
                                "Rainfall", "No vegetation", "Share of Water", 
                                "Share of Gras", "Artificial" ), digits=2)

sink()


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# PART three: > 2018
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#only focus on data after 08-20-2018 (first protest of greta thunberg)
adata0.4 <- adata0.2 %>%
  filter(date >= ("2018-08-20"))

#3 models for first table: Income
mod_5.3.1 <- lm(yes_p_per ~ treated + p_0_to_30 + p_30_to_40 + p_40_to_50 + 
                  p_50_to_75 + p_75_to_open +
                  `p_18-30` + `p_31-45` + 
                  `p_46-65` + `p_66-85` + `p_86-100` + 
                  btert_share + btertII_share +
                  gender_mn + p_ch_protest + p_ch_romancath + p_ch_christcath +
                  p_ch_christortho + p_ch_christother + p_ch_jewish + p_ch_muslim +
                  p_ch_religionother + p_ch_atheist +
                  damage_extent +
                  factor(vote_code) + factor(mncode), data = adata0.4)

mod_5.3.2 <- lm(yes_p_per ~ treated + GPS_vshare + SPS_vshare + CVP_vshare + 
                  FDP_vshare + SVP_vshare + rainfall_1000 + vlose.sh + wasser.sh + 
                  gras.sh + kuenst.sh + 
                  factor(vote_code) + factor(mncode), data = adata0.4)

mod_5.3.3 <- lm(yes_p_per ~ treated + p_0_to_30 + p_30_to_40 + p_40_to_50 + 
                  p_50_to_75 + p_75_to_open +
                  `p_18-30` + `p_31-45` + 
                  `p_46-65` + `p_66-85` + `p_86-100` + 
                  btert_share + btertII_share +
                  gender_mn + p_ch_protest + p_ch_romancath + p_ch_christcath +
                  p_ch_christortho + p_ch_christother + p_ch_jewish + p_ch_muslim +
                  p_ch_religionother + p_ch_atheist +
                  damage_extent +
                  GPS_vshare + SPS_vshare + CVP_vshare + 
                  FDP_vshare + SVP_vshare + rainfall_1000 + vlose.sh + wasser.sh + 
                  gras.sh + kuenst.sh +
                  factor(vote_code) + factor(mncode), data = adata0.4)


models_list_5.3.1 <- list(
  mod_5.3.1, 
  mod_5.3.2, 
  mod_5.3.3)

screenreg(models_list_5.3.1, stars = c(0.01, 0.05, 0.1), omit = "factor",
          custom.coef.names = c("Intercept", "Disaster", "Inc. =<30", 
                                "Inc. 30-40", "Inc. 40-50",
                                "Inc. 50-75", "Inc. 75<",
                                "Age 18-30", 
                                "Age 31-45", "Age 46-65", "Age 66-85", 
                                "Age 86-100", 
                                "Tertiary", "Tert II",
                                "Female",
                                "Protestant", "Catholic Roman",
                                "Catholic Christian", "Catholic Orthodox", "Catholic Other",
                                "Jewish", "Muslim", "Religion Other", "Atheist",
                                "Damage Extent 2", "Damage Extent 3",
                                "Green Party %",
                                "Social Democrats %" , "Christian Democrats %", 
                                "Liberal Democrats %", "Swiss People's Party %",
                                "Rainfall", "No vegetation", "Share of Water", 
                                "Share of Gras", "Artificial" ), digits=2)

sink("Replication of Tables E.txt", append = TRUE)
print("######")
print("######")
print("Table 1 - Full: after 2018")
print("######")
print("######")
screenreg(models_list_5.3.1, stars = c(0.01, 0.05, 0.1), omit = "factor",
          custom.coef.names = c("Intercept", "Disaster", "Inc. =<30", 
                                "Inc. 30-40", "Inc. 40-50",
                                "Inc. 50-75", "Inc. 75<",
                                "Age 18-30", 
                                "Age 31-45", "Age 46-65", "Age 66-85", 
                                "Age 86-100", 
                                "Tertiary", "Tert II",
                                "Female",
                                "Protestant", "Catholic Roman",
                                "Catholic Christian", "Catholic Orthodox", "Catholic Other",
                                "Jewish", "Muslim", "Religion Other", "Atheist",
                                "Damage Extent 2", "Damage Extent 3",
                                "Green Party %",
                                "Social Democrats %" , "Christian Democrats %", 
                                "Liberal Democrats %", "Swiss People's Party %",
                                "Rainfall", "No vegetation", "Share of Water", 
                                "Share of Gras", "Artificial" ), digits=2)

sink()
