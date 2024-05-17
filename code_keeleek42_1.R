##############################################################################
##############################################################################
#
#				"Dissertation keeleek42"
#
#       Stefan Keel
#       Part 1 - Replication with new data
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
# Data Manupilation and Replication 1
# Test of Baccini and Leemann with extended data / time
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
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


#3 models for first table
mod_1 <- lm(yes_p_per ~ treated + GPS_vshare + SPS_vshare + CVP_vshare + 
             FDP_vshare + SVP_vshare + factor(vote_code) + factor(mncode), 
            data = adata0)

mod_2 <- lm(yes_p_per ~ treated +  rainfall_1000  + vlose.sh + wasser.sh + 
             gras.sh + kuenst.sh + factor(vote_code) + factor(mncode), data = adata0)

mod_3 <- lm(yes_p_per ~ treated + GPS_vshare + SPS_vshare + CVP_vshare + 
             FDP_vshare + SVP_vshare + rainfall_1000 + vlose.sh + wasser.sh + 
              gras.sh + kuenst.sh + factor(vote_code) + factor(mncode), data = adata0)

screenreg(list(mod_1,mod_2,mod_3), stars=c(0.01,0.05,0.1), omit="factor", 
          reorder.coef=c(2:7,8:12,1),
          custom.coef.names = c("Intercept", "Disaster" ,"Green Party %",
                                "Social Democrats %" , "Christian Democrats %", 
                                "Liberal Democrats %", "Swiss People's Party %",
                                "Rainfall", "No vegetation", "Share of Water", 
                                "Share of Gras", "Artificial"), digits=2) 

sink("Replication of Tables A.txt")
print("######")
print("######")
print("Table 1")
print("######")
print("######")
screenreg(list(mod_1,mod_2,mod_3), stars=c(0.01,0.05,0.1), omit="factor", 
          reorder.coef=c(2:7,8:12,1), 
          custom.coef.names = c("Intercept", "Disaster","Green Party %",
                                "Social Democrats %" , "Christian Democrats %", 
                                "Liberal Democrats %", "Swiss People's Party %",
                                "Rainfall", "No vegetation",
                                "Share of Water", "Share of Gras", "Artificial"), 
          digits=2) 

sink()

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Run models for Table 2 in paper
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

#so there are no unnecessary NA
adata0 <- subset(adata0, select = -c(disaster_ID, disaster_date, main_process))

adata1 <- na.omit(adata0)

treatment1   <- adata1$treated 
X1           <- cbind(adata1$vlose.sh, adata1$wasser.sh, adata1$gras.sh, 
                     adata1$kuenst.sh, adata1$alti_mun_m, adata1$rainfall_surf_1000, 
                     adata1$steep_percent, adata1$SPS_vshare, adata1$CVP_vshare,
                     adata1$GPS_vshare, adata1$FDP_vshare, adata1$SVP_vshare,
                     adata1$vote1, adata1$vote2, adata1$vote3, adata1$vote4, 
                     adata1$vote5, adata1$vote6, adata1$vote7, adata1$vote8,
                     adata1$vote9, adata1$vote11, adata1$vote12,
                     adata1$vote13, adata1$vote14, adata1$vote15, adata1$vote16,
                     adata1$vote17, adata1$vote18, adata1$vote19, adata1$Area_50years_km2)

colnames(X1) <- c("no vegetation", "water", "gras",
                 "artificial", "altitude", "rainfall", 
                 "steepness", "SPS", "CVP", "GPS", "FDP", "SVP", 
                 "vote1", "vote2", "vote3", "vote4", "vote5",
                 "vote6", "vote7", "vote8",
                 "vote9", "vote11", "vote12",
                 "vote13", "vote14", "vote15", "vote16",
                 "vote17", "vote18", "vote19", "Area_50years_km2")


# Entropy Balancing
eb.out1 <- ebalance(Treatment = treatment1, X = X1)
weight.L1 <- rep(1,dim(X1)[1])
weight.L1[treatment1==0] <- eb.out1$w

Yl1  <- adata1$yes_p_per

mb.ent.before1 <- MatchBalance(treatment1 ~ adata1$Area_50years_km2 + adata1$vlose.sh + 
                                adata1$wasser.sh+ adata1$gras.sh  + adata1$kuenst.sh + 
                                adata1$alti_mun_m + adata1$rainfall_surf_1000 + 
                                adata1$steep_percent + adata1$SPS_vshare + adata1$CVP_vshare + 
                                adata1$GPS_vshare + adata1$FDP_vshare + 
                                adata1$SVP_vshare, weights = rep(1,length(weight.L1)), 
                              ks = FALSE)

mb.ent.after1 <- MatchBalance(treatment1 ~ adata1$Area_50years_km2 + adata1$vlose.sh + 
                               adata1$wasser.sh + adata1$gras.sh + adata1$kuenst.sh + 
                               adata1$alti_mun_m + adata1$rainfall_surf_1000 + 
                               adata1$steep_percent + adata1$SPS_vshare + adata1$CVP_vshare + 
                               adata1$GPS_vshare + adata1$FDP_vshare + 
                               adata1$SVP_vshare, weights=weight.L1, 
                             ks = FALSE)

bloc.pre1 <- baltest.collect(matchbal.out = mb.ent.before1, 
                            var.names = c( "Flooding Risk","Surface: % No vegetation", 
                                         "Surface: % Water","Surface: % Gras",
                                         "Surface: % Artificial",
                                         "Altitude (in m)", "Rainfall (per sqkm)", 
                                         "Steepness in %", "Social Democrats (%)", 
                                         "Christian Democrats (%)", "Greens (%)", 
                                         "Liberals (%)", "Swiss People's Party (%)"), 
                            after = FALSE)

bloc.post1 <- baltest.collect(matchbal.out = mb.ent.after1, 
                             var.names = c("Flooding Risk","Surface: % No vegetation", 
                                         "Surface: % Water", "Surface: % Gras",
                                         "Surface: % Artificial",
                                         "Altitude (in m)", "Rainfall (per sqkm)", 
                                         "Steepness in %", "Social Democrats (%)", 
                                         "Christian Democrats (%)", "Greens (%)",
                                         "Liberals (%)", "Swiss People's Party (%)"), 
                             after = FALSE)

rowN1 <- rownames(bloc.post1)
blocL1 <- matrix(NA,length(rowN1),6)
rownames(blocL1) <- rowN1
blocL1[,c(1:3)] <- round(bloc.pre1[,c(1,2,6)],2)
blocL1[,c(4:5)] <- round(bloc.post1[,c(1,2)],2)

bloc.post1


# get treatment effect estimate
dat1 <- data.frame(Yl1, weight.L1, treatment1, adata1$SPS_vshare,
                  adata1$GPS_vshare, adata1$CVP_vshare, adata1$FDP_vshare,
                  adata1$SVP_vshare, adata1$disaster_treat_monthcount, adata1$mncode, 
                  adata1$tert_share, adata1$tertII_share,
                  adata1$p_0_to_30, adata1$p_30_to_40, adata1$p_40_to_50,
                  adata1$p_50_to_75, adata1$p_75_to_open)

des1 <- svydesign(id=~1,weights=~weight.L1, data= dat1)
mod_1eb <- svyglm(Yl1 ~ treatment1 , design = des1)

summary(mod_1eb)
blocL1
for (i in 1:dim(blocL1)[1]){
  # bloc.post[,6] holds the t-values and they are all essentially 1, i.e. showing no difference
  if (round(bloc.post1[i,6],2)==1.00) blocL1[i,6] <- "\\checkmark"
}

sink("Replication of Tables A.txt", append = TRUE)
print("######")
print("######")
print("### Table 2 (ATT)")
print("######")
print("######")
summary(mod_1eb)
blocL1
sink()

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Run models for Table 3 in paper
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

adata1$left <- adata1$SPS_vshare + adata1$GPS_vshare 
adata1$leftINTtreat <- adata1$left * adata1$treated
adata1$disaster_treat_monthcount[treatment1==0] <- 0
adata1$timeINTtreat <- adata1$disaster_treat_monthcount * adata1$treated

dat2 <- data.frame(Yl1, weight.L1, treatment1, adata1$leftINTtreat, adata1$left,
                  adata1$SPS_vshare, adata1$GPS_vshare, adata1$CVP_vshare,
                  adata1$FDP_vshare, adata1$SVP_vshare, adata1$disaster_treat_monthcount,
                  adata1$timeINTtreat, adata1$mncode,
                  adata1$tert_share, adata1$tertII_share,
                  adata1$p_0_to_30, adata1$p_30_to_40, adata1$p_40_to_50,
                  adata1$p_50_to_75, adata1$p_75_to_open)

des2 <- svydesign(id=~1,weights=~weight.L1, data = dat2)


#mods
mod_2.1eb <- svyglm(Yl1 ~ treatment1, design = des2)
mod_2.2eb <- svyglm(Yl1 ~ treatment1 + adata1.tert_share, design = des2)
mod_2.3eb <- svyglm(Yl1 ~ treatment1 + adata1.tert_share +
                      adata1.tert_share:treatment1, design = des2)

#summary
models_list_edu <- list(
  mod_2.1eb,
  mod_2.2eb,
  mod_2.3eb)

screenreg(models_list_edu, stars = c(0.01, 0.05, 0.1))

sink("Replication of Tables A.txt", append = TRUE)
print("######")
print("######")
print("Table 3 (Heterogeneity in Space)")
print("######")
print("######")
screenreg(models_list_edu, stars = c(0.01, 0.05, 0.1))
sink()



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Run models for Table 4 in paper
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

mod_4t <- svyglm(Yl1 ~ treatment1 , design = des2)
mod_5t <- svyglm(Yl1 ~ treatment1 +  adata1.disaster_treat_monthcount + adata1.timeINTtreat, 
                design = des2)

screenreg(list(mod_4t, mod_5t),  stars=c(0.01,0.05,0.1), 
          custom.coef.names = c("Constant","Treatment","Time betw. Flood and Vote"), 
          custom.model.names = c("Model V","Model VIII"))

sink("Replication of Tables A.txt", append = TRUE)
print("######")
print("######")
print("Table 4 (Heterogeneity in Time)")
print("######")
print("######")
screenreg(list(mod_4t, mod_5t),  stars=c(0.01,0.05,0.1), 
          custom.coef.names = c("Constant","Treatment","Time betw. Flood and Vote"), 
          custom.model.names = c("Model V","Model VIII"))
sink()

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Figure 2
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Generating Quantities, then plotting them 
# Education
vL1 <- vcov(mod_2.3eb)
bL1 <- coef(mod_2.3eb)
BETA1 <- mvrnorm(n = 5000,mu = bL1, Sigma = vL1)
head(BETA1)
educ <- seq(1,100,by = 1)/100
Yhat1 <- BETA1[,c(2,4)] %*% t(cbind(1,educ))

Y.3 <- matrix(NA,100,3)
for (i in 1:100){
  Y.3[i,] <- quantile(Yhat1[,i], c(0.025, 0.5, 0.975))
}

# time
vL2 <- vcov(mod_5t)
bL2 <- coef(mod_5t)
BETA2 <- mvrnorm(n = 5000,mu = bL2, Sigma = vL2)
head(BETA2)
timeL2 <- seq(1,120,by = 1)/12
Yhat2 <- BETA2[,c(2,3)] %*% t(cbind(1,timeL2))

Y.7 <- matrix(NA,120,3)
for (i in 1:120){
  Y.7[i,] <- quantile(Yhat2[,i], c(0.025, 0.5, 0.975))
}
# Plot
pdf("Marginaleffects.pdf", width=10,height=5)
par(mfrow=c(1,2))
plot(educ,colMeans(Yhat1), col="blue", type = "l",
     lwd=3, bty="n", ylab="Estimated Effect of Exposure",
     xlab="Share of People with Tertiary Education")
points(educ,Y.3[,1], type="l", lty=3,
       col="blue", lwd=1) 
points(educ,Y.3[,3], type="l", lty=3,
       col="blue", lwd=1) 

for (i in 1:5000){
  points(educ,Yhat1[i,], type="l",
         col=rgb(0,0,255,2,maxColorValue = 255), lwd=2) 
}

# 2nd plot
plot(timeL2,colMeans(Yhat2), col="blue", type = "l",
     lwd=3, bty="n", ylab="Estimated Effect of Exposure",
     xlab="Months since Exposure")
points(timeL2,Y.7[,1], type="l", lty=3,
       col="blue", lwd=2) 
points(timeL2,Y.7[,3], type="l", lty=3,
       col="blue", lwd=2) 

for (i in 1:5000){
  points(timeL2,Yhat2[i,], type="l",
         col=rgb(0,0,255,2,maxColorValue = 255), lwd=2) 
}
dev.off()

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Significance test in FN 12
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

set.seed(42)
# significance check:
vL3 <- vcov(mod_5t)
bL3 <- coef(mod_5t)
BETA3 <- mvrnorm(n = 10000,mu = bL3, Sigma = vL3)
# After 10 months no significant difference
x0 <- c(1,0,0)
x1 <- c(1,1,10)
p0 <- BETA3 %*% x0
p1 <- BETA3 %*% x1
diff01 <- p0 - p1

sort(diff01)[c(500, 9501)] # 90
sink("Replication of Tables A.txt", append = TRUE)
print("######")
print("######")
print("Footnote 12, 90% CI")
print("######")
print("######")
sort(diff01)[c(500, 9501)] # 90
sink()

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Table A2
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

sink("Replication of Tables A.txt", append = TRUE)
print("######")
print("######")
print("Table A1")
print("######")
print("######")
print("See above where Table 1 is replicated - they are identical.")
sink()

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Table A2
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

#take out the nuclear plants vote as they might be confounding
#data <- data[data$vote_code!=105 & data$vote_code!=110,]

# Choosing and renaming columns
adata0.1 <- data

#change units on some of the geographical variables
#source: Baccini and Leemann
adata0.1$kuenst.sh <- 100* adata0.1$kuenstl/adata0.1$Punktflaeche
adata0.1$wasser.sh <- 100* adata0.1$wasser/adata0.1$Punktflaeche
adata0.1$gras.sh <- 100* adata0.1$gras/adata0.1$Punktflaeche
adata0.1$gebuesch.sh <- 100* adata0.1$gebuesch/adata0.1$Punktflaeche
adata0.1$baum.sh <- 100* adata0.1$baum/adata0.1$Punktflaeche
adata0.1$vlose.sh <- 100* adata0.1$vlose/adata0.1$Punktflaeche
adata0.1$alti_mun_m <- 1000* adata0.1$alti_mun_km
adata0.1$alti_mun_m2 <- adata0.1$alti_mun_m^2
adata0.1$rainfall_surf_1000 <- 1000*adata0.1$rainfall_surf


#model 1-3 with turnout as outcome
#3 models for first table
mod_1.1 <- lm(`Beteiligung in %` ~ treated + GPS_vshare + SPS_vshare + CVP_vshare + 
              FDP_vshare + SVP_vshare + factor(vote_code) + factor(mncode), data=adata0.1)

mod_2.1 <- lm(`Beteiligung in %` ~ treated +  rainfall_1000  + vlose.sh + wasser.sh + 
              gras.sh + kuenst.sh + factor(vote_code) + factor(mncode), data=adata0.1)

mod_3.1 <- lm(`Beteiligung in %` ~ treated + GPS_vshare + SPS_vshare + CVP_vshare + 
              FDP_vshare + SVP_vshare + rainfall_1000 + vlose.sh + wasser.sh + 
              gras.sh + kuenst.sh + factor(vote_code) + factor(mncode), data=adata0.1)


screenreg(list(mod_1.1, mod_2.1, mod_3.1), stars=c(0.01,0.05,0.1), omit="factor", 
          reorder.coef=c(2:7,8:12,1),
          custom.coef.names = c("Intercept", "Disaster" ,"Green Party %",
                                "Social Democrats %" , "Christian Democrats %", 
                                "Liberal Democrats %", "Swiss People's Party %",
                                "Rainfall", "No vegetation", "Share of Water", 
                                "Share of Gras", "Artificial"), digits=2) 


sink("Replication of Tables A.txt", append = TRUE)
print("######")
print("######")
print("Table A2")
print("######")
print("######")
screenreg(list(mod_1.1, mod_2.1, mod_3.1), stars=c(0.01,0.05,0.1), omit="factor", 
          reorder.coef=c(2:7,8:12,1),
          custom.coef.names = c("Intercept", "Disaster" ,"Green Party %",
                                "Social Democrats %" , "Christian Democrats %", 
                                "Liberal Democrats %", "Swiss People's Party %",
                                "Rainfall", "No vegetation", "Share of Water", 
                                "Share of Gras", "Artificial"), digits=2)
sink()


