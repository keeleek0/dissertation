###creating the master-dataset
#####
#this R script creates the dataset for the analysis.
#it loads different datasets from the Swiss Govt. Statistical Office and
#transforms them so they can be added to the data_wide (master-dataset)
#most datasets ar xlsx or psx. Some pre cleaning has been done in xlsx.
#the "raw" datafiles are still available and in feasibly named folders.

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
lapply(c('pxR', 'openxlsx', 'sp', 'tidyr', 'lubridate', 'stringdist',
         'dplyr', 'zoo', 'lubridate'),  pkgTest)

#setting the working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#setup complete
############

#File with all votes from Switzerland
#read px and create dataframe
px <- read.px( 'Data_Sources/data_votes_ch.px' )
data   <-  as.data.frame(px) 

#renaming columns
colnames(data) <- c('result', 'date_vote', 'municipality', 'value')

#extracting date of votings
data$date <- gsub('^(\\d{4}-\\d{2}-\\d{2}).*', '\\1', data$date_vote)
data$date_vote <- gsub('\\d{4}-\\d{2}-\\d{2}\\s*', '', data$date_vote)
colnames(data) <- c('result', 'vote', 'municipality', 'value', 'date')

#convert Date to Date type
data$date <- as.Date(data$date)

###narrow down to the votes defined (environmental votes)
#define the votes to keep
keepvotes <- c(
  "Bundesgesetz über die Ziele im Klimaschutz, die Innovation und die Stärkung der Energiesicherheit (KlG)",
  "Bundesgesetz über die Verminderung von Treibhausgasemissionen (CO2-Gesetz)",
  "Bundesbeschluss über die Genehmigung des Umfassenden Wirtschaftspartnerschaftsabkommens zwischen den EFTA-Staaten und Indonesien",
  "Energiegesetz (EnG)",
  "Volksinitiative «Für den geordneten Ausstieg aus der Atomenergie»",
  "Volksinitiative «Für eine nachhaltige und ressourceneffiziente Wirtschaft (Grüne Wirtschaft)»",
  "Volksinitiative «Energie- statt Mehrwertsteuer»",
  "Volksinitiative «Verbandsbeschwerderecht: Schluss mit der Verhinderungspolitik - Mehr Wachstum für die Schweiz!»",
  "Gegenentwurf zur Volksinitiative «Avanti - für sichere und leistungsfähige Autobahnen»",
  "Volksinitiative «Strom ohne Atom - Für eine Energiewende und die schrittweise Stilllegung der Atomkraftwerke»",
  "Volksinitiative «für einen autofreien Sonntag pro Jahreszeit - ein Versuch für vier Jahre»",
  "Elektrizitätsmarktgesetz",
  "Volksinitiative «für eine gesicherte AHV - Energie statt Arbeit besteuern!»",
  "Volksinitiative «für mehr Verkehrssicherheit durch Tempo 30 innerorts mit Ausnahmen (Strassen für alle)»",
  "Verfassungsartikel über eine Energielenkungsabgabe für die Umwelt",
  "Verfassungsartikel über eine Förderabgabe für erneuerbare Energien",
  "Volksinitiative «für einen Solarrappen»",
  "Volksinitiative «für die Halbierung des motorisierten Strassenverkehrs zur Erhaltung und Verbesserung von Lebensräumen (Verkehrshalbierungs-Initiative)»",
  "Bundesgesetz über eine leistungsabhängige Schwerverkehrsabgabe"
)

#subset dataframe to keep only rows with exact terms
data <- data[data$vote %in% keepvotes, ]

#long to wide
#pivot the dataframe to wide format
data_wide <- pivot_wider(data, names_from = result, values_from = value)

#codes for votes
unique_votes <- unique(data_wide$vote)

for (i in 1:length(unique_votes)) {
  data_wide <- mutate(data_wide, !!paste0("vote", i) := as.numeric(vote == unique_votes[i]))
}

start_code <- 100
vote_code_mapping <- setNames(start_code + seq_along(unique_votes), unique_votes)
data_wide$vote_code <- vote_code_mapping[data_wide$vote]




#make sense of votes: Sometimes YES = NO for the environment
#YES = YES
#Bundesgesetz über die Ziele im Klimaschutz, die Innovation und die Stärkung der Energiesicherheit (KlG)
#Bundesgesetz über die Verminderung von Treibhausgasemissionen (CO2-Gesetz)
#Energiegesetz (EnG)
#Volksinitiative «Für eine nachhaltige und ressourceneffiziente Wirtschaft (Grüne Wirtschaft)
#Volksinitiative «Energie- statt Mehrwertsteuer»
#Volksinitiative «für einen autofreien Sonntag pro Jahreszeit - ein Versuch für vier Jahre»
#Elektrizitätsmarktgesetz
#Volksinitiative «für eine gesicherte AHV - Energie statt Arbeit besteuern!
#Volksinitiative «für mehr Verkehrssicherheit durch Tempo 30 innerorts mit Ausnahmen (Strassen für alle)
#Verfassungsartikel über eine Energielenkungsabgabe für die Umwelt
#Verfassungsartikel über eine Förderabgabe für erneuerbare Energien
#Volksinitiative «für einen Solarrappen
#Volksinitiative «für die Halbierung des motorisierten Strassenverkehrs zur Erhaltung und Verbesserung von Lebensräumen (Verkehrshalbierungs-Initiative)»",
#Bundesgesetz über eine leistungsabhängige Schwerverkehrsabgabe




#YES = NO
#Bundesbeschluss über die Genehmigung des Umfassenden Wirtschaftspartnerschaftsabkommens zwischen den EFTA-Staaten und Indonesien
#Volksinitiative «Für den geordneten Ausstieg aus der Atomenergie
#Volksinitiative «Strom ohne Atom - Für eine Energiewende und die schrittweise Stilllegung der Atomkraftwerke»"
#Volksinitiative «Verbandsbeschwerderecht: Schluss mit der Verhinderungspolitik - Mehr Wachstum für die Schweiz!
#Gegenentwurf zur Volksinitiative «Avanti - für sichere und leistungsfähige Autobahnen»


#change YES to NO interpretation
yestono <- data_wide$vote %in% c("Bundesbeschluss über die Genehmigung des Umfassenden Wirtschaftspartnerschaftsabkommens zwischen den EFTA-Staaten und Indonesien",
                                 "Volksinitiative «Für den geordneten Ausstieg aus der Atomenergie»",
                                 "Volksinitiative «Verbandsbeschwerderecht: Schluss mit der Verhinderungspolitik - Mehr Wachstum für die Schweiz!»",
                                 "Volksinitiative «Strom ohne Atom - Für eine Energiewende und die schrittweise Stilllegung der Atomkraftwerke»",
                                 "Gegenentwurf zur Volksinitiative «Avanti - für sichere und leistungsfähige Autobahnen»")

data_wide$`Ja in %`[yestono] <- 100 - data_wide$`Ja in %`[yestono]



#creating the year
data_wide$year <- year(data_wide$date)


#getting rid of not needed summarisation of municipalities
data_wide <- data_wide[!grepl(">>|- Zürich|Kanton|- Appenzell Innerrhoden|- Bern / Berne|- Uri|- Obwalden|- Zug|- Fribourg / Freiburg|
                              FR-autres|- Basel-Stadt|- Basel-Landschaft|
                              - Graubünden / Grigioni / Grischun|- Aargau|- Thurgau|
                              - Ticino|TI-altri|- Vaud|VD-autres|- Valais / Wallis|
                              VS-autres|GE-autres|- Jura|- Appenzell Ausserrhoden|- Genève|- Glarus|- Luzern|- Schaffhausen|- Schwyz|- Solothurn|- Ticino", data_wide$municipality), ]

#these caused trouble in the above code. therefore one-by-one
data_wide <- data_wide[!grepl("- Graubünden / Grigioni / Grischun", 
                              data_wide$municipality), ]
data_wide <- data_wide[!grepl("- Neuchâtel", data_wide$municipality), ]
data_wide <- data_wide[!grepl("- Nidwalden", data_wide$municipality), ]
data_wide <- data_wide[!grepl("- St. Gallen", data_wide$municipality), ]

#municipalities without vote data are extracted/deleted
data_wide <- data_wide[!grepl("Meienried", data_wide$municipality), ]
data_wide <- data_wide[!grepl("Niedermuhlern", data_wide$municipality), ]
data_wide <- data_wide[!grepl("Deisswil", data_wide$municipality), ]
data_wide <- data_wide[!grepl("Hellsau", data_wide$municipality), ]

#get rid of special characters
data_wide$municipality <- gsub("\\.+\\b", "", data_wide$municipality)


#get rid of summarising voting areas and double-tap from above to be sure
data_wide <- data_wide[!grepl("Verwaltungskreis", data_wide$municipality), ] 
data_wide <- data_wide[!grepl("Bezirk", data_wide$municipality), ] 
data_wide <- data_wide[!grepl("Anderes", data_wide$municipality), ] 
data_wide <- data_wide[!grepl("Ausland", data_wide$municipality), ] 
data_wide <- data_wide[!grepl("l'étranger", data_wide$municipality), ] 
data_wide <- data_wide[!grepl("autres", data_wide$municipality), ] 
data_wide <- data_wide[!grepl("District", data_wide$municipality), ] 
data_wide <- data_wide[!grepl("Korrespondenzweg", data_wide$municipality), ] 
data_wide <- data_wide[!grepl("Wahlkreis", data_wide$municipality), ] 
data_wide <- data_wide[!grepl("Arrondissement", data_wide$municipality), ] 
data_wide <- data_wide[!grepl("Distretto", data_wide$municipality), ] 
data_wide <- data_wide[!grepl("Region", data_wide$municipality), ] 
data_wide <- data_wide[!grepl("corrispondenza", data_wide$municipality), ] 
data_wide <- data_wide[!grepl("estero", data_wide$municipality), ] 


#creating mean difference yes_vote
average_ja_in_percent <- data_wide %>%
  group_by(vote_code) %>%
  summarise(average_ja_in_percent = mean(`Ja in %`, na.rm = TRUE))

data_wide <- data_wide %>%
  left_join(average_ja_in_percent, by = "vote_code") %>%
  mutate(yes_vote_meanD = `Ja in %` - average_ja_in_percent)



###match with municipality codes
#loading municipalities and codes from disaster dataset
mncode_df <- read.xlsx("Data_Sources/data_mncode_mnname.xlsx")

#create matches
matches <- match(data_wide$municipality, mncode_df$mnname)
data_wide$mncode <- ifelse(is.na(matches), NA, mncode_df$mnnr[matches])

####checking of NAs in the municipality Codes
which(is.na(data_wide$mncode))



###Coordinates
#coordinates here we come
coordinates <- read.csv("Data_Sources/data_coordinates_lv95.csv", sep = ";", 
                        header = TRUE)

#transforming the swiss coordinates to WGS84
#example LV95 coordinates
lv95_coords <- data.frame(Easting = coordinates$E,
                          Northing = coordinates$N)

#create SpatialPoints object for LV95 coordinates
lv95_points <- SpatialPoints(lv95_coords, 
                             proj4string = CRS("+proj=somerc +lat_0=46.9524056 +lon_0=7.43958333 +ellps=bessel +x_0=2600000 +y_0=1200000 +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs"))

#transform LV95 coordinates to WGS84
wgs84_points <- spTransform(lv95_points, CRS("+proj=longlat +datum=WGS84"))

#extract Latitude and Longitude
coordinates$latitude <- coordinates(wgs84_points)[, 2]
coordinates$longitude <- coordinates(wgs84_points)[, 1]

#matching coordinates
matches_coordinates <- match(data_wide$mncode, coordinates$BFS.Nr)

data_wide$mn_latitude <- ifelse(is.na(matches_coordinates), NA, 
                             coordinates$latitude[matches_coordinates])

data_wide$mn_longitude <- ifelse(is.na(matches_coordinates), NA, 
                                 coordinates$longitude[matches_coordinates])

#checking for NAs, Should only be 1-19 as its the reference/test "Schweiz"
which(is.na(data_wide$mn_longitude))

####income variable here we come
income <- read.xlsx('Data_Sources/data_income.xlsx')

#hard match
#composite key by concatenating mncode and year
data_wide$composite_key <- paste(data_wide$mncode, data_wide$year, sep = "_")
income$composite_key <- paste(income$mncode, income$year, sep = "_")

#match based on the composite key
matches_income <- match(data_wide$composite_key, income$composite_key)

#add matched columns from income to data_wide
data_wide$p_0_to_30 <- income$p_0til30[matches_income]
data_wide$n_0_to_30 <- income$"0.-.30'000"[matches_income]
data_wide$p_30_to_40 <- income$p_30til40[matches_income]
data_wide$n_30_to_40 <- income$"30'001.-.40'000"[matches_income] 
data_wide$p_40_to_50 <- income$p_40til50[matches_income]
data_wide$n_40_to_50 <- income$"40'001.-.50'000"[matches_income]
data_wide$p_50_to_75 <- income$p_50til75[matches_income]
data_wide$n_50_to_75 <- income$"50'001.-.75'000"[matches_income]
data_wide$p_75_to_open <- income$p_75tilopen[matches_income]
data_wide$n_75_to_open <- income$"75'000.-.plus"[matches_income]
data_wide$n_total <- income$Total[matches_income]


#to percentage
data_wide <- data_wide %>%
  mutate(
    p_0_to_30 = p_0_to_30 * 100,
    p_30_to_40 = p_30_to_40 * 100,
    p_40_to_50 = p_40_to_50 * 100,
    p_50_to_75 = p_50_to_75 * 100,
    p_75_to_open = p_75_to_open * 100)

#expecting a lot of NA
which(is.na(data_wide$n_total))


#find rows with NA values in the specified columns
na_rows_income <- which(is.na(data_wide$p_0_to_30) |
                   is.na(data_wide$n_0_to_30) |
                   is.na(data_wide$p_30_to_40) |
                   is.na(data_wide$n_30_to_40) |
                   is.na(data_wide$p_40_to_50) |
                   is.na(data_wide$n_40_to_50) |
                   is.na(data_wide$p_50_to_75) |
                   is.na(data_wide$n_50_to_75) |
                   is.na(data_wide$p_75_to_open) |
                   is.na(data_wide$n_75_to_open) |
                   is.na(data_wide$n_total))

#creating a new dataset with only NA rows from data_wide
data_wide_na_income <- data_wide[na_rows_income, ]

# Function to perform fuzzy matching based on Jaccard similarity
fuzzy_match_jaccard <- function(x, y) {
  distances <- stringdist::stringdistmatrix(x, y, method = "jaccard")
  # Find the index of the minimum distance for each row
  index <- apply(distances, 1, which.min)
  # Extract the corresponding values from y
  matched_values <- y[index]
  return(matched_values)
}

#loop through each row of data_wide_na and fill NA values
for (i in 1:nrow(data_wide_na_income)) {
  #extract municipality and year from data_wide_na
  municipality <- data_wide_na_income$municipality[i]
  year <- data_wide_na_income$year[i]
  
  #perform fuzzy match for municipality and year
  matched_row <- income[income$year == year, ]
  matched_municipality <- fuzzy_match_jaccard(municipality, matched_row$mnname)
  
  #ff a match is found, fill the NA values
  if (!is.na(matched_municipality)) {
    data_wide_na_income$p_0_to_30[i] <- matched_row$p_0til30[matched_row$mnname == matched_municipality]
    data_wide_na_income$n_0_to_30[i] <- as.numeric(gsub(",", "", matched_row$"0.-.30'000"[matched_row$mnname == matched_municipality]))
    data_wide_na_income$p_30_to_40[i] <- matched_row$p_30til40[matched_row$mnname == matched_municipality]
    data_wide_na_income$n_30_to_40[i] <- as.numeric(gsub(",", "", matched_row$"30'001.-.40'000"[matched_row$mnname == matched_municipality]))
    data_wide_na_income$p_40_to_50[i] <- matched_row$p_40til50[matched_row$mnname == matched_municipality]
    data_wide_na_income$n_40_to_50[i] <- as.numeric(gsub(",", "", matched_row$"40'001.-.50'000"[matched_row$mnname == matched_municipality]))
    data_wide_na_income$p_50_to_75[i] <- matched_row$p_50til75[matched_row$mnname == matched_municipality]
    data_wide_na_income$n_50_to_75[i] <- as.numeric(gsub(",", "", matched_row$"50'001.-.75'000"[matched_row$mnname == matched_municipality]))
    data_wide_na_income$p_75_to_open[i] <- matched_row$p_75tilopen[matched_row$mnname == matched_municipality]
    data_wide_na_income$n_75_to_open[i] <- as.numeric(gsub(",", "", matched_row$"75'000.-.plus"[matched_row$mnname == matched_municipality]))
    data_wide_na_income$n_total[i] <- as.numeric(gsub(",", "", matched_row$Total[matched_row$mnname == matched_municipality]))
  }
}

#assigning the matched values back to data_wide
data_wide[na_rows_income, ] <- data_wide_na_income

#should be 0
which(is.na(data_wide$n_total))



###Getting the Risk for each municipality and other variables from the dataset
#of baccini and leemann
old_data <- read.xlsx('Data_Sources/data_leemann_baccini.xlsx')

#match based on the composite key
matches_risk <- match(data_wide$mncode, old_data$gdenr)

#adding matched columns from income to data_wide
data_wide$atert_share <- old_data$Tert_share[matches_risk]
data_wide$atertII_share <- old_data$Tert_sekun_share[matches_risk]
data_wide$rainfall_1000 <- old_data$rainfall_1000[matches_risk]
data_wide$alti_mun_km <- old_data$alti_mun_km[matches_risk]
data_wide$Area_50years_km2 <- old_data$Area_50years_km2[matches_risk]
data_wide$canton <- old_data$canton[matches_risk]
data_wide$rainfall_1000 <- old_data$rainfall_1000[matches_risk]
data_wide$steep_degree <- old_data$steep_degree[matches_risk]
data_wide$steep_percent <- old_data$steep_percent[matches_risk]
data_wide$Polygonflaeche <- old_data$Polygonflaeche[matches_risk] 
data_wide$Punktflaeche <- old_data$Punktflaeche[matches_risk]
data_wide$kuenstl <- old_data$kuenstl[matches_risk]
data_wide$gras <- old_data$gras[matches_risk]
data_wide$gebuesch <- old_data$gebuesch[matches_risk]
data_wide$baum <- old_data$baum[matches_risk]
data_wide$vlose <- old_data$vlose[matches_risk]
data_wide$wasser <- old_data$wasser[matches_risk]
data_wide$rainfall_surf <- old_data$rainfall_surf[matches_risk]
data_wide$rainfall_1000_2 <- old_data$rainfall_1000_2[matches_risk]
data_wide$AbsoluteHazardarea_km2 <- old_data$AbsoluteHazardarea_km2[matches_risk]
data_wide$RelativeHazardArea <- old_data$RelativeHazardArea[matches_risk]

#match based on the composite key for municipality
matches_municipality <- match(data_wide$municipality, old_data$gemeindenamen)

#update matched columns from old_data to data_wide only where they havent been matched yet
data_wide$atert_share[is.na(data_wide$atert_share)] <- old_data$Tert_share[matches_municipality]
data_wide$atertII_share[is.na(data_wide$atertII_share)] <- old_data$Tert_sekun_share[matches_municipality]
data_wide$rainfall_1000[is.na(data_wide$rainfall_1000)] <- old_data$rainfall_1000[matches_municipality]
data_wide$alti_mun_km[is.na(data_wide$alti_mun_km)] <- old_data$alti_mun_km[matches_municipality]
data_wide$Area_50years_km2[is.na(data_wide$Area_50years_km2)] <- old_data$Area_50years_km2[matches_municipality]
data_wide$canton[is.na(data_wide$canton)] <- old_data$canton[matches_municipality]
data_wide$rainfall_1000[is.na(data_wide$rainfall_1000)] <- old_data$rainfall_1000[matches_municipality]
data_wide$steep_degree[is.na(data_wide$steep_degree)] <- old_data$steep_degree[matches_municipality]
data_wide$steep_percent[is.na(data_wide$steep_percent)] <- old_data$steep_percent[matches_municipality]
data_wide$Polygonflaeche[is.na(data_wide$Polygonflaeche)] <- old_data$Polygonflaeche[matches_municipality] 
data_wide$Punktflaeche[is.na(data_wide$Punktflaeche)] <- old_data$Punktflaeche[matches_municipality]
data_wide$kuenstl[is.na(data_wide$kuenstl)] <- old_data$kuenstl[matches_municipality]
data_wide$gras[is.na(data_wide$gras)] <- old_data$gras[matches_municipality]
data_wide$gebuesch[is.na(data_wide$gebuesch)] <- old_data$gebuesch[matches_municipality]
data_wide$baum[is.na(data_wide$baum)] <- old_data$baum[matches_municipality]
data_wide$vlose[is.na(data_wide$vlose)] <- old_data$vlose[matches_municipality]
data_wide$wasser[is.na(data_wide$wasser)] <- old_data$wasser[matches_municipality]
data_wide$rainfall_surf[is.na(data_wide$rainfall_surf)] <- old_data$rainfall_surf[matches_municipality]
data_wide$rainfall_1000_2[is.na(data_wide$rainfall_1000_2)] <- old_data$rainfall_1000_2[matches_municipality]
data_wide$AbsoluteHazardarea_km2[is.na(data_wide$AbsoluteHazardarea_km2)] <- old_data$AbsoluteHazardarea_km2[matches_municipality]
data_wide$RelativeHazardArea[is.na(data_wide$RelativeHazardArea)] <- old_data$RelativeHazardArea[matches_municipality]


#match based on the composite key for municipality (as dataset provides 2 
#sources for municipality names)
matches_municipality2 <- match(data_wide$municipality, old_data$gemeinde)

#update matched columns from old_data to data_wide only where they haven't been matched yet
data_wide$atert_share[is.na(data_wide$atert_share)] <- old_data$Tert_share[matches_municipality2]
data_wide$atertII_share[is.na(data_wide$atertII_share)] <- old_data$Tert_sekun_share[matches_municipality2]
data_wide$rainfall_1000[is.na(data_wide$rainfall_1000)] <- old_data$rainfall_1000[matches_municipality2]
data_wide$alti_mun_km[is.na(data_wide$alti_mun_km)] <- old_data$alti_mun_km[matches_municipality2]
data_wide$Area_50years_km2[is.na(data_wide$Area_50years_km2)] <- old_data$Area_50years_km2[matches_municipality2]
data_wide$canton[is.na(data_wide$canton)] <- old_data$canton[matches_municipality2]
data_wide$rainfall_1000[is.na(data_wide$rainfall_1000)] <- old_data$rainfall_1000[matches_municipality2]
data_wide$steep_degree[is.na(data_wide$steep_degree)] <- old_data$steep_degree[matches_municipality2]
data_wide$steep_percent[is.na(data_wide$steep_percent)] <- old_data$steep_percent[matches_municipality2]
data_wide$Polygonflaeche[is.na(data_wide$Polygonflaeche)] <- old_data$Polygonflaeche[matches_municipality2] 
data_wide$Punktflaeche[is.na(data_wide$Punktflaeche)] <- old_data$Punktflaeche[matches_municipality2]
data_wide$kuenstl[is.na(data_wide$kuenstl)] <- old_data$kuenstl[matches_municipality2]
data_wide$gras[is.na(data_wide$gras)] <- old_data$gras[matches_municipality2]
data_wide$gebuesch[is.na(data_wide$gebuesch)] <- old_data$gebuesch[matches_municipality2]
data_wide$baum[is.na(data_wide$baum)] <- old_data$baum[matches_municipality2]
data_wide$vlose[is.na(data_wide$vlose)] <- old_data$vlose[matches_municipality2]
data_wide$wasser[is.na(data_wide$wasser)] <- old_data$wasser[matches_municipality2]
data_wide$rainfall_surf[is.na(data_wide$rainfall_surf)] <- old_data$rainfall_surf[matches_municipality2]
data_wide$rainfall_1000_2[is.na(data_wide$rainfall_1000_2)] <- old_data$rainfall_1000_2[matches_municipality2]
data_wide$AbsoluteHazardarea_km2[is.na(data_wide$AbsoluteHazardarea_km2)] <- old_data$AbsoluteHazardarea_km2[matches_municipality2]
data_wide$RelativeHazardArea[is.na(data_wide$RelativeHazardArea)] <- old_data$RelativeHazardArea[matches_municipality2]

#checking for sure that NAs are good (should only be 1-19)
which(is.na(data_wide$RelativeHazardArea))
which(is.na(data_wide$atert_share))
which(is.na(data_wide$atertII_share))

#############
#education new

#to of atert_share and II 100%
data_wide$atert_share <- data_wide$atert_share * 100
data_wide$atertII_share <- data_wide$atertII_share * 100

#loadeading the Excel file
edu_new <- read.xlsx('Data_Sources/data_education_detail.xlsx')

#ceate Tert II sum of mean percentage (sek 1 + sek 2 + in education)
sum_edu <- edu_new %>%
  filter(edu_level %in% c("Sekundarstufe I", "Sekundarstufe II", "In Ausbildung")) %>%
  group_by(edu_mncode, mnname) %>%
  summarise(edu_percent_sum = sum(edu_percent_mean, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(edu_level = "Tert II")

# Append sum_edu to edu_new
edu_new <- bind_rows(edu_new, sum_edu)

# Arrange by edu_mncode
edu_new <- edu_new %>%
  arrange(edu_mncode)

# Filter and summarize for Tertiärstufe and Tert II
edu_filtered <- edu_new %>%
  filter(edu_level %in% c("Tertiärstufe", "Tert II")) %>%
  group_by(edu_mncode, mnname) %>%
  summarise(
    Tertiärstufe_sum = sum(edu_percent_sum[edu_level == "Tertiärstufe"], na.rm = TRUE),
    Tert_II_sum = sum(edu_percent_sum[edu_level == "Tert II"], na.rm = TRUE)
  ) %>%
  ungroup()

#creating keys
data_wide$comp_key_edu <- paste0(data_wide$mncode)
edu_filtered$comp_key_edu <- paste0(edu_filtered$edu_mncode)

#matching
matches_edu_new <- match(data_wide$comp_key_edu, edu_filtered$comp_key_edu)

#adding matched columns from edu_filtered to data_wide
data_wide$btert_share <- edu_filtered$Tertiärstufe_sum[matches_edu_new]
data_wide$btertII_share <- edu_filtered$Tert_II_sum[matches_edu_new]

#na check
which(is.na(data_wide$btert_share))
which(is.na(data_wide$btertII_share))

#more keys
data_wide$comp_key_edu2 <- paste0(data_wide$municipality)
edu_filtered$comp_key_edu2 <- paste0(edu_filtered$mnname)

#matching
matches_edu_new2 <- match(data_wide$comp_key_edu2, edu_filtered$comp_key_edu2)

#add matched columns from edu_filtered to data_wide only where there are NAs in data_wide
data_wide$btert_share[is.na(data_wide$btert_share)] <- 
  edu_filtered$Tertiärstufe_sum[matches_edu_new2][is.na(data_wide$btert_share)]

data_wide$btertII_share[is.na(data_wide$btertII_share)] <- 
  edu_filtered$Tert_II_sum[matches_edu_new2][is.na(data_wide$btertII_share)]

#na check
which(is.na(data_wide$btert_share))
which(is.na(data_wide$btertII_share))

#na rows
na_rows_edu_new <- which(is.na(data_wide$btert_share))
data_wide_na_edu_new <- data_wide[na_rows_edu_new, ]

#jaccard fuzzy match function
fuzzy_match_jaccard <- function(x, y) {
  distances <- stringdist::stringdistmatrix(x, y, method = "jaccard")
  index <- apply(distances, 1, which.min)
  matched_values <- y[index]
  return(matched_values)
}

#fuzzy matching
matched_municipalities <- 
  fuzzy_match_jaccard(data_wide_na_edu_new$municipality, edu_filtered$mnname)

#matching lengths and remove NAs from the matches
matched_municipalities <- matched_municipalities[!is.na(matched_municipalities)]
na_rows_edu_new <- na_rows_edu_new[!is.na(matched_municipalities)]

#update the data
data_wide$btert_share[na_rows_edu_new] <- 
  edu_filtered$Tertiärstufe_sum[match(matched_municipalities, edu_filtered$mnname)]

data_wide$btertII_share[na_rows_edu_new] <- 
  edu_filtered$Tert_II_sum[match(matched_municipalities, edu_filtered$mnname)]

#na check
which(is.na(data_wide$btert_share))
which(is.na(data_wide$btertII_share))


#if missing values in btert_share, then take atert_share and atertII_share
non_na_btert_share <- is.na(data_wide$btert_share) | data_wide$btert_share == 0
data_wide$btert_share[non_na_btert_share] <- data_wide$atert_share[non_na_btert_share]

#replace NA and 0 values in btertII_share with corresponding atertII_share
non_na_btertII_share <- is.na(data_wide$btertII_share) | data_wide$btertII_share == 0
data_wide$btertII_share[non_na_btertII_share] <- data_wide$atertII_share[non_na_btertII_share]


#na check
which(is.na(data_wide$btert_share))
which(is.na(data_wide$btertII_share))


###Party Strength
###Getting the party strength for each municipality on board
party_strength <- read.xlsx('Data_Sources/data_party_strength.xlsx')

party_strength <- party_strength
party_strength$year <- zoo::na.locf(party_strength$year)
party_strength$mnname <- zoo::na.locf(party_strength$mnname)
party_strength$mncode <- zoo::na.locf(party_strength$mncode)

#removing leading zeros in mncode until a non-zero number is encountered
party_strength$mncode <- gsub("^0+", "", party_strength$mncode)

party_strength <- party_strength[!grepl('>>', party_strength$mnname), ]
party_strength$mnname <- gsub('\\.+', '', party_strength$mnname)


#reshaping long to wide
party_wide <- pivot_wider(
  data = party_strength,
  names_from = party_name,
  values_from = party_share
)

party_wide <- party_wide[, !names(party_wide) %in% "NA"]

#creating copies of years for assigning later on
###2019
party_wide_2019 <- party_wide[party_wide$year == "2019", ]

#2023
party_wide_2019$year <- "2023"
party_wide <- rbind(party_wide, party_wide_2019)

#2021
party_wide_2019$year <- "2021"
party_wide <- rbind(party_wide, party_wide_2019)

#2017
party_wide_2019$year <- "2017"
party_wide <- rbind(party_wide, party_wide_2019)


###2015 to 2016
party_wide_2015 <- party_wide[party_wide$year == "2015", ]

#2016
party_wide_2015$year <- "2016"
party_wide <- rbind(party_wide, party_wide_2015)


###2007 to 2008
party_wide_2007 <- party_wide[party_wide$year == "2007", ]

#2008
party_wide_2007$year <- "2008"
party_wide <- rbind(party_wide, party_wide_2007)


###2003 to 2001
party_wide_2003 <- party_wide[party_wide$year == "2003", ]

#2004
party_wide_2003$year <- "2004"
party_wide <- rbind(party_wide, party_wide_2003)

#2002
party_wide_2003$year <- "2002"
party_wide <- rbind(party_wide, party_wide_2003)

#2001
party_wide_2003$year <- "2001"
party_wide <- rbind(party_wide, party_wide_2003)


###1999 to 1998
party_wide_1999 <- party_wide[party_wide$year == "1999", ]

#2000
party_wide_1999$year <- "2000"
party_wide <- rbind(party_wide, party_wide_1999)

#1998
party_wide_1999$year <- "1998"
party_wide <- rbind(party_wide, party_wide_1999)


#creating keys
data_wide$composite_key2 <- paste(data_wide$mncode, data_wide$year, sep = "_")
party_wide$composite_key2 <- paste(party_wide$mncode, party_wide$year, sep = "_")

#matchng on keys
matches_party <- match(data_wide$composite_key2, party_wide$composite_key2)

#aading matched columns from party_wide to data_wide
data_wide$CVP <- party_wide$CVP[matches_party]
data_wide$SP <- party_wide$SP[matches_party]
data_wide$SVP <- party_wide$SVP[matches_party]
data_wide$LPS <- party_wide$LPS[matches_party] 
data_wide$LdU <- party_wide$LdU[matches_party]
data_wide$EVP <- party_wide$EVP[matches_party]
data_wide$CSP <- party_wide$CSP[matches_party]
data_wide$GLP <- party_wide$GLP[matches_party]
data_wide$BDP <- party_wide$BDP[matches_party]
data_wide$PdA <- party_wide$PdA[matches_party]
data_wide$PSA <- party_wide$PSA[matches_party]
data_wide$POCH <- party_wide$POCH[matches_party]
data_wide$GPS <- party_wide$GPS[matches_party]
data_wide$FGA <- party_wide$FGA[matches_party]
data_wide$Sol <- party_wide$Sol.[matches_party]
data_wide$Rep <- party_wide$Rep.[matches_party]
data_wide$SD <- party_wide$SD[matches_party]
data_wide$EDU <- party_wide$EDU[matches_party]
data_wide$FPS <- party_wide$FPS[matches_party]
data_wide$Lega <- party_wide$Lega[matches_party]
data_wide$MCR <- party_wide$MCR[matches_party]
data_wide$Sep <- party_wide$Sep.[matches_party]
data_wide$Übrige <- party_wide$Übrige[matches_party]

#checking
which(is.na(data_wide$Rep))

#creating more keys
data_wide$composite_key3 <- paste(data_wide$municipality, data_wide$year, sep = "_")
party_wide$composite_key3 <- paste(party_wide$mnname, party_wide$year, sep = "_")

#matching on keys
matches_party2 <- match(data_wide$composite_key3, party_wide$composite_key3)

#adding matched columns from party_wide to data_wide only where there are NAs in data_wide
data_wide$CVP[is.na(data_wide$CVP)] <- party_wide$CVP[matches_party2][is.na(data_wide$CVP)]
data_wide$SP[is.na(data_wide$SP)] <- party_wide$SP[matches_party2][is.na(data_wide$SP)]
data_wide$SVP[is.na(data_wide$SVP)] <- party_wide$SVP[matches_party2][is.na(data_wide$SVP)]
data_wide$LPS[is.na(data_wide$LPS)] <- party_wide$LPS[matches_party2][is.na(data_wide$LPS)]
data_wide$LdU[is.na(data_wide$LdU)] <- party_wide$LdU[matches_party2][is.na(data_wide$LdU)]
data_wide$EVP[is.na(data_wide$EVP)] <- party_wide$EVP[matches_party2][is.na(data_wide$EVP)]
data_wide$CSP[is.na(data_wide$CSP)] <- party_wide$CSP[matches_party2][is.na(data_wide$CSP)]
data_wide$GLP[is.na(data_wide$GLP)] <- party_wide$GLP[matches_party2][is.na(data_wide$GLP)]
data_wide$BDP[is.na(data_wide$BDP)] <- party_wide$BDP[matches_party2][is.na(data_wide$BDP)]
data_wide$PdA[is.na(data_wide$PdA)] <- party_wide$PdA[matches_party2][is.na(data_wide$PdA)]
data_wide$PSA[is.na(data_wide$PSA)] <- party_wide$PSA[matches_party2][is.na(data_wide$PSA)]
data_wide$POCH[is.na(data_wide$POCH)] <- party_wide$POCH[matches_party2][is.na(data_wide$POCH)]
data_wide$GPS[is.na(data_wide$GPS)] <- party_wide$GPS[matches_party2][is.na(data_wide$GPS)]
data_wide$FGA[is.na(data_wide$FGA)] <- party_wide$FGA[matches_party2][is.na(data_wide$FGA)]
data_wide$Sol[is.na(data_wide$Sol)] <- party_wide$Sol.[matches_party2][is.na(data_wide$Sol)]
data_wide$Rep[is.na(data_wide$Rep)] <- party_wide$Rep.[matches_party2][is.na(data_wide$Rep)]
data_wide$SD[is.na(data_wide$SD)] <- party_wide$SD[matches_party2][is.na(data_wide$SD)]
data_wide$EDU[is.na(data_wide$EDU)] <- party_wide$EDU[matches_party2][is.na(data_wide$EDU)]
data_wide$FPS[is.na(data_wide$FPS)] <- party_wide$FPS[matches_party2][is.na(data_wide$FPS)]
data_wide$Lega[is.na(data_wide$Lega)] <- party_wide$Lega[matches_party2][is.na(data_wide$Lega)]
data_wide$MCR[is.na(data_wide$MCR)] <- party_wide$MCR[matches_party2][is.na(data_wide$MCR)]
data_wide$Sep[is.na(data_wide$Sep)] <- party_wide$Sep.[matches_party2][is.na(data_wide$Sep)]
data_wide$Übrige[is.na(data_wide$Übrige)] <- party_wide$Übrige[matches_party2][is.na(data_wide$Übrige)]

# Find rows with NA values in the specified columns
na_rows_party <- which(is.na(data_wide$CVP))

# Create a new dataset with only NA rows from data_wide
data_wide_na_party <- data_wide[na_rows_party, ]

# Function to perform fuzzy matching based on Jaccard similarity
fuzzy_match_jaccard <- function(x, y) {
  distances <- stringdist::stringdistmatrix(x, y, method = "jaccard")
  # Find the index of the minimum distance for each row
  index <- apply(distances, 1, which.min)
  # Extract the corresponding values from y
  matched_values <- y[index]
  return(matched_values)
}

# Loop through each row of data_wide_na and fill NA values
for (i in 1:nrow(data_wide_na_party)) {
  # Extract municipality and year from data_wide_na
  municipality <- data_wide_na_party$municipality[i]
  year <- data_wide_na_party$year[i]
  
  # Perform fuzzy match for municipality and year
  matched_row <- party_wide[party_wide$year == year, ]
  matched_municipality <- fuzzy_match_jaccard(municipality, matched_row$mnname)
  
  # Update data_wide with matched values only where there are NA values
  if (!is.na(matched_municipality)) {
    data_wide$CVP[na_rows_party[i]] <- matched_row$CVP[matched_row$mnname == matched_municipality]
    data_wide$SP[na_rows_party[i]] <- matched_row$SP[matched_row$mnname == matched_municipality]
    data_wide$SVP[na_rows_party[i]] <- matched_row$SVP[matched_row$mnname == matched_municipality]
    data_wide$LPS[na_rows_party[i]] <- matched_row$LPS[matched_row$mnname == matched_municipality]
    data_wide$LdU[na_rows_party[i]] <- matched_row$LdU[matched_row$mnname == matched_municipality]
    data_wide$EVP[na_rows_party[i]] <- matched_row$EVP[matched_row$mnname == matched_municipality]
    data_wide$CSP[na_rows_party[i]] <- matched_row$CSP[matched_row$mnname == matched_municipality]
    data_wide$GLP[na_rows_party[i]] <- matched_row$GLP[matched_row$mnname == matched_municipality]
    data_wide$BDP[na_rows_party[i]] <- matched_row$BDP[matched_row$mnname == matched_municipality]
    data_wide$PdA[na_rows_party[i]] <- matched_row$PdA[matched_row$mnname == matched_municipality]
    data_wide$PSA[na_rows_party[i]] <- matched_row$PSA[matched_row$mnname == matched_municipality]
    data_wide$POCH[na_rows_party[i]] <- matched_row$POCH[matched_row$mnname == matched_municipality]
    data_wide$GPS[na_rows_party[i]] <- matched_row$GPS[matched_row$mnname == matched_municipality]
    data_wide$FGA[na_rows_party[i]] <- matched_row$FGA[matched_row$mnname == matched_municipality]
    data_wide$Sol[na_rows_party[i]] <- matched_row$Sol.[matched_row$mnname == matched_municipality]
    data_wide$Rep[na_rows_party[i]] <- matched_row$Rep.[matched_row$mnname == matched_municipality]
    data_wide$SD[na_rows_party[i]] <- matched_row$SD[matched_row$mnname == matched_municipality]
    data_wide$EDU[na_rows_party[i]] <- matched_row$EDU[matched_row$mnname == matched_municipality]
    data_wide$FPS[na_rows_party[i]] <- matched_row$FPS[matched_row$mnname == matched_municipality]
    data_wide$Lega[na_rows_party[i]] <- matched_row$Lega[matched_row$mnname == matched_municipality]
    data_wide$MCR[na_rows_party[i]] <- matched_row$MCR[matched_row$mnname == matched_municipality]
    data_wide$Sep[na_rows_party[i]] <- matched_row$Sep.[matched_row$mnname == matched_municipality]
    data_wide$Übrige[na_rows_party[i]] <- matched_row$Übrige[matched_row$mnname == matched_municipality]
  }
}

#checking
which(is.na(data_wide$Rep))


###Education by canton and matching to municipality
#edu <- read.xlsx('Data_Sources/data_education.xlsx')

#data_wide <- data_wide %>% 
  #mutate(canton = ifelse(canton == "Appenzell Ausserrhoden", "AR", canton))

#data_wide <- data_wide %>% 
 # mutate(canton = ifelse(canton == "Appenzell Innerrhoden", "AI", canton))

#data_wide <- data_wide %>% 
 # mutate(canton = ifelse(canton == "St. Gallen", "SG", canton))

#2022 copy to 2023
#edu_2022 <- edu[edu$edu_year == 2022, ]
#edu_2023 <- edu_2022
#edu_2023$edu_year <- 2023
#edu <- rbind(edu, edu_2023)

#2010 copy to 2008
#edu_2010 <- edu[edu$edu_year == 2010, ]
#edu_2008 <- edu_2010
#edu_2008$edu_year <- 2008
#edu <- rbind(edu, edu_2008)

#2000 to 2004, 2003, 2002, 2001 and 1998
#2004
#edu_2004 <- edu[edu$edu_year == 2000, ]
#edu_2004$edu_year <- 2004
#edu <- rbind(edu, edu_2004)

#2003
#edu_2003 <- edu[edu$edu_year == 2000, ]
#edu_2003$edu_year <- 2003
#edu <- rbind(edu, edu_2003)

#2002
#edu_2002 <- edu[edu$edu_year == 2000, ]
#edu_2002$edu_year <- 2002
#edu <- rbind(edu, edu_2002)

#2001
#edu_2001 <- edu[edu$edu_year == 2000, ]
#edu_2001$edu_year <- 2001
#edu <- rbind(edu, edu_2001)

#1998
#edu_1998 <- edu[edu$edu_year == 2000, ]
#edu_1998$edu_year <- 1998
#edu <- rbind(edu, edu_1998)


#matching
#composite keys for data_wide and party_wide
#data_wide$composite_key4 <- paste(data_wide$canton, data_wide$year, sep = "_")
#edu$composite_key4 <- paste(edu$canton, edu$edu_year, sep = "_")

#matching
#matches_edu <- match(data_wide$composite_key4, edu$composite_key4)

#data_wide$p_edu_null <- edu$p_edu_null[matches_edu]
#data_wide$p_edu_null_m <- edu$p_edu_null_m[matches_edu]
#data_wide$p_edu_basic_m <- edu$p_edu_basic_m[matches_edu]
#data_wide$p_edu_matura_m <- edu$p_edu_matura_m[matches_edu]
#data_wide$p_edu_tert_m <- edu$p_edu_tert_m[matches_edu]
#data_wide$canton_edu_pax_total <- edu$canton_edu_pax_total[matches_edu]
#data_wide$edu_basic_pax_total <- edu$edu_basic_pax_total[matches_edu]
#data_wide$p_edu_basic <- edu$p_edu_basic[matches_edu]
#data_wide$edu_matura_pax_total <- edu$edu_matura_pax_total[matches_edu]
#data_wide$p_edu_matura <- edu$p_edu_matura[matches_edu]
#data_wide$edu_tert_pax_total <- edu$edu_tert_pax_total[matches_edu]
#data_wide$p_edu_tert <- edu$p_edu_tert[matches_edu]
#data_wide$canton_edu_male_total <- edu$canton_edu_male_total[matches_edu]
#data_wide$p_canton_edu_male <- edu$p_canton_edu_male[matches_edu]
#data_wide$edu_male_basic_pax_total <- edu$edu_male_basic_pax_total[matches_edu]
#data_wide$p_edu_male_basic <- edu$p_edu_male_basic[matches_edu]
#data_wide$edu_male_matura_pax <- edu$edu_male_matura_pax[matches_edu]
#data_wide$p_edu_male_matura <- edu$p_edu_male_matura[matches_edu]
#data_wide$edu_male_tert_pax <- edu$edu_male_tert_pax[matches_edu]
#data_wide$p_edu_male_tert <- edu$p_edu_male_tert[matches_edu]
#data_wide$canton_edu_female_total <- edu$canton_edu_female_total[matches_edu]
#data_wide$p_canton_edu_female <- edu$p_canton_edu_female[matches_edu]
#data_wide$edu_female_basic_pax_total <- edu$edu_female_basic_pax_total[matches_edu]
#data_wide$p_edu_female_basic <- edu$p_edu_female_basic[matches_edu]
#data_wide$edu_female_matura_pax <- edu$edu_female_matura_pax[matches_edu]
#data_wide$p_edu_female_matura <- edu$p_edu_female_matura[matches_edu]
#data_wide$p_edu_female_tert_pax <- edu$p_edu_female_tert_pax[matches_edu]
#data_wide$p_edu_female_tert <- edu$p_edu_female_tert[matches_edu]

#to percentage
# Multiply each column by 100
#data_wide$p_edu_null <- data_wide$p_edu_null * 100
#data_wide$p_edu_null_m <- data_wide$p_edu_null_m * 100
#data_wide$p_edu_basic_m <- data_wide$p_edu_basic_m * 100
#data_wide$p_edu_matura_m <- data_wide$p_edu_matura_m * 100
#data_wide$p_edu_tert_m <- data_wide$p_edu_tert_m * 100
#data_wide$p_edu_basic <- data_wide$p_edu_basic * 100
#data_wide$p_edu_matura <- data_wide$p_edu_matura * 100
#data_wide$p_edu_tert <- data_wide$p_edu_tert * 100
#data_wide$p_canton_edu_male <- data_wide$p_canton_edu_male * 100
#data_wide$p_edu_male_basic <- data_wide$p_edu_male_basic * 100
#data_wide$p_edu_male_matura <- data_wide$p_edu_male_matura * 100
#data_wide$p_edu_male_tert <- data_wide$p_edu_male_tert * 100
#data_wide$p_canton_edu_female <- data_wide$p_canton_edu_female * 100
#data_wide$p_edu_female_basic <- data_wide$p_edu_female_basic * 100
#data_wide$p_edu_female_matura <- data_wide$p_edu_female_matura * 100
#data_wide$p_edu_female_tert <- data_wide$p_edu_female_tert * 100
#data_wide$p_edu_female_tert <- data_wide$p_edu_female_tert * 100

#checking for nas
#which(is.na(data_wide$p_edu_female_basic))

###age and municipality

age <- read.xlsx('Data_Sources/Data_Age_2022_2010.xlsx')

#summarize age ranges and add as new columns in the original dataset
age <- age %>%
  mutate(`0-17` = rowSums(select(., `0`:`17`)),
         `18-30` = rowSums(select(., `18`:`30`)),
         `31-45` = rowSums(select(., `31`:`45`)),
         `46-65` = rowSums(select(., `46`:`65`)),
         `66-85` = rowSums(select(., `66`:`85`)),
         `86-100` = rowSums(select(., `86`:`100`)))

#compute proportions for each age range
age <- age %>%
  mutate(`p_0-17` = (`0-17` / age_total) * 100,
         `p_18-30` = (`18-30` / age_total) * 100,
         `p_31-45` = (`31-45` / age_total) * 100,
         `p_46-65` = (`46-65` / age_total) * 100,
         `p_66-85` = (`66-85` / age_total) * 100,
         `p_86-100` = (`86-100` / age_total) * 100)

#only keep percentage
age <- age %>%
  select(-`0`:-`100`)

#reorder age
age <- age %>%
  select(age_year, mnname, everything())


#filter data for the years 2010-2015
filtered_data <- filter(age, age_year >= 2010 & age_year <= 2015)

#calculate the averages for each mnname in each cell starting from age_total
averages <- filtered_data %>%
  group_by(mnname) %>%
  summarize(across(starts_with("age_total"):starts_with("p_86-100"), mean))

#add a new row for age_year 2009
averages <- mutate(averages, age_year = 2009)

#reorder columns to match the original data frame
averages <- averages %>%
  select(age_year, mnname, everything())

#create a function to duplicate rows and change the age_year
duplicate_and_change_age <- function(data, start_year, end_year) {
  result <- data
  for (year in start_year:end_year) {
    temp <- data
    temp$age_year <- year
    result <- bind_rows(result, temp)
  }
  return(result)
}

#duplicate rows and change age_year from 1995 to 2008
averages <- duplicate_and_change_age(averages, 1995, 2008)

#reorder
averages <- averages %>%
  select(age_year, mnname, everything())


#Filter data for the years 2016-2022
filtered_data2 <- filter(age, age_year >= 2016 & age_year <= 2022)

#Calculate the averages for each mnname in each cell starting from age_total
averages2 <- filtered_data2 %>%
  group_by(mnname) %>%
  summarize(across(starts_with("age_total"):starts_with("p_86-100"), mean))

#Add a new row for age_year 2023
averages2 <- mutate(averages2, age_year = 2023)

# Reorder columns to match the original data frame
averages2 <- averages2 %>%
  select(age_year, mnname, everything())

#merge them
averages <- bind_rows(averages, averages2)

#merge to original
age <- bind_rows(age, averages)


#get rid of zero and create mncode
age$mnname <- str_replace(age$mnname, "^0+(?=[1-9])", "")
#extract numbers from mnname column and store in mncode column
age$mncode <- as.numeric(str_extract(age$mnname, "\\d+"))
age$mnname <- str_replace(age$mnname, "\\d+\\s*", "")



###add age to main
# Create composite keys for data_wide and party_wide
data_wide$composite_key5 <- paste(data_wide$mncode, data_wide$year, sep = "_")
age$composite_key5 <- paste(age$mncode, age$age_year, sep = "_")

# Match based on the composite key
matches_age <- match(data_wide$composite_key5, age$composite_key5)

# Add matched columns from party_wide to data_wide
data_wide$age_total <- age$age_total[matches_age]
data_wide$`0-17` <- age$`0-17`[matches_age]
data_wide$`18-30` <- age$`18-30`[matches_age]
data_wide$`31-45` <- age$`31-45`[matches_age]
data_wide$`46-65` <- age$`46-65`[matches_age]
data_wide$`66-85` <- age$`66-85`[matches_age]
data_wide$`86-100` <- age$`86-100`[matches_age]
data_wide$`p_0-17` <- age$`p_0-17`[matches_age]
data_wide$`p_18-30` <- age$`p_18-30`[matches_age]
data_wide$`p_31-45` <- age$`p_31-45`[matches_age]
data_wide$`p_46-65` <- age$`p_46-65`[matches_age]
data_wide$`p_66-85` <- age$`p_66-85`[matches_age]
data_wide$`p_86-100` <- age$`p_86-100`[matches_age]


# Create composite keys for data_wide and party_wide
data_wide$composite_key6 <- paste(data_wide$municipality, data_wide$year, sep = "_")
age$composite_key6 <- paste(age$mnname, age$age_year, sep = "_")

# Match based on the composite key
matches_age2 <- match(data_wide$composite_key6, age$composite_key6)

# Add matched columns from party_wide to data_wide only where there are NAs in data_wide
data_wide$age_total[is.na(data_wide$age_total)] <- age$age_total[matches_age2][is.na(data_wide$age_total)]
data_wide$`0-17`[is.na(data_wide$`0-17`)] <- age$`0-17`[matches_age2][is.na(data_wide$`0-17`)]
data_wide$`18-30`[is.na(data_wide$`18-30`)] <- age$`18-30`[matches_age2][is.na(data_wide$`18-30`)]
data_wide$`31-45`[is.na(data_wide$`31-45`)] <- age$`31-45`[matches_age2][is.na(data_wide$`31-45`)]
data_wide$`46-65`[is.na(data_wide$`46-65`)] <- age$`46-65`[matches_age2][is.na(data_wide$`46-65`)]
data_wide$`66-85`[is.na(data_wide$`66-85`)] <- age$`66-85`[matches_age2][is.na(data_wide$`66-85`)]
data_wide$`86-100`[is.na(data_wide$`86-100`)] <- age$`86-100`[matches_age2][is.na(data_wide$`86-100`)]
data_wide$`p_0-17`[is.na(data_wide$`p_0-17`)] <- age$`p_0-17`[matches_age2][is.na(data_wide$`p_0-17`)]
data_wide$`p_18-30`[is.na(data_wide$`p_18-30`)] <- age$`p_18-30`[matches_age2][is.na(data_wide$`p_18-30`)]
data_wide$`p_31-45`[is.na(data_wide$`p_31-45`)] <- age$`p_31-45`[matches_age2][is.na(data_wide$`p_31-45`)]
data_wide$`p_46-65`[is.na(data_wide$`p_46-65`)] <- age$`p_46-65`[matches_age2][is.na(data_wide$`p_46-65`)]
data_wide$`p_66-85`[is.na(data_wide$`p_66-85`)] <- age$`p_66-85`[matches_age2][is.na(data_wide$`p_66-85`)]
data_wide$`p_86-100`[is.na(data_wide$`p_86-100`)] <- age$`p_86-100`[matches_age2][is.na(data_wide$`p_86-100`)]


#na rows with NA values in the specified columns
na_rows_age <- which(is.na(data_wide$`p_86-100`))

#creating a new dataset with only NA rows from data_wide
data_wide_na_age <- data_wide[na_rows_age, ]

#fuzzy to perform fuzzy matching based on Jaccard similarity
fuzzy_match_jaccard <- function(x, y) {
  distances <- stringdist::stringdistmatrix(x, y, method = "jaccard")
  # Find the index of the minimum distance for each row
  index <- apply(distances, 1, which.min)
  # Extract the corresponding values from y
  matched_values <- y[index]
  return(matched_values)
}

#looping through each row of data_wide_na and fill NA values
for (i in 1:nrow(data_wide_na_age)) {
  # Extract municipality and year from data_wide_na
  municipality <- data_wide_na_age$municipality[i]
  year <- data_wide_na_age$year[i]
  
  # Perform fuzzy match for municipality and year
  matched_row <- age[age$age_year == year, ]
  matched_municipality <- fuzzy_match_jaccard(municipality, matched_row$mnname)
  
  # Update data_wide with matched values only where there are NA values
  if (!is.na(matched_municipality)) {
    data_wide$age_total[na_rows_age[i]] <- matched_row$age_total[matched_row$mnname == matched_municipality]
    data_wide$`0-17`[na_rows_age[i]] <- matched_row$`0-17`[matched_row$mnname == matched_municipality]
    data_wide$`18-30`[na_rows_age[i]] <- matched_row$`18-30`[matched_row$mnname == matched_municipality]
    data_wide$`31-45`[na_rows_age[i]] <- matched_row$`31-45`[matched_row$mnname == matched_municipality]
    data_wide$`46-65`[na_rows_age[i]] <- matched_row$`46-65`[matched_row$mnname == matched_municipality]
    data_wide$`66-85`[na_rows_age[i]] <- matched_row$`66-85`[matched_row$mnname == matched_municipality]
    data_wide$`86-100`[na_rows_age[i]] <- matched_row$`86-100`[matched_row$mnname == matched_municipality]
    data_wide$`p_0-17`[na_rows_age[i]] <- matched_row$`p_0-17`[matched_row$mnname == matched_municipality]
    data_wide$`p_18-30`[na_rows_age[i]] <- matched_row$`p_18-30`[matched_row$mnname == matched_municipality]
    data_wide$`p_31-45`[na_rows_age[i]] <- matched_row$`p_31-45`[matched_row$mnname == matched_municipality]
    data_wide$`p_46-65`[na_rows_age[i]] <- matched_row$`p_46-65`[matched_row$mnname == matched_municipality]
    data_wide$`p_66-85`[na_rows_age[i]] <- matched_row$`p_66-85`[matched_row$mnname == matched_municipality]
    data_wide$`p_86-100`[na_rows_age[i]] <- matched_row$`p_86-100`[matched_row$mnname == matched_municipality]
  }
}

#checking for na
which(is.na(data_wide$`p_86-100`))


###gender and religion per municipality 2000
gender_religion <- read.xlsx('Data_Sources/data_gender_religion.xlsx')


#compute proportions for each group
gender_religion <- gender_religion %>%
  mutate(
    p_chchm_total = (chchm_total / chch_total) * 100,
    p_chchm_protest = (chchm_protest / chchm_total) * 100,
    p_chchm_romancath = (chchm_romancath / chchm_total) * 100,
    p_chchm_christcath = (chchm_christcath / chchm_total) * 100,
    p_chchm_christortho = (chchm_christortho / chchm_total) * 100,
    p_chchm_christother = (chchm_christother / chchm_total) * 100,
    p_chchm_jewish = (chchm_jewish / chchm_total) * 100,
    p_chchm_muslim = (chchm_muslim / chchm_total) * 100,
    p_chchm_religionother = (chchm_religionother / chchm_total) * 100,
    p_chchm_atheist = (chchm_atheist / chchm_total) * 100,
    p_chchm_religion_undisclosed = (chchm_religion_undisclosed / chchm_total) * 100,
    p_chchf_total = (chchf_total / chch_total) * 100,
    p_chchf_protest = (chchf_protest / chchf_total) * 100,
    p_chchf_romancath = (chchf_romancath / chchf_total) * 100,
    p_chchf_christcath = (chchf_christcath / chchf_total) * 100,
    p_chchf_christortho = (chchf_christortho / chchf_total) * 100,
    p_chchf_christother = (chchf_christother / chchf_total) * 100,
    p_chchf_jewish = (chchf_jewish / chchf_total) * 100,
    p_chchf_muslim = (chchf_muslim / chchf_total) * 100,
    p_chchf_religionother = (chchf_religionother / chchf_total) * 100,
    p_chchf_atheist = (chchf_atheist / chchf_total) * 100,
    p_chchf_religion_undisclosed = (chchf_religion_undisclosed / chchf_total) * 100
  )


#get rid of zero and create mncode
gender_religion$mnname <- str_replace(gender_religion$mnname, "^0+(?=[1-9])", "")
#extract numbers from mnname column and store in mncode column
gender_religion$mncode <- as.numeric(str_extract(gender_religion$mnname, "\\d+"))
gender_religion$mnname <- str_replace(gender_religion$mnname, "\\d+\\s*", "")


#match based on the composite key
matches_gerel <- match(data_wide$mncode, gender_religion$mncode)

#matched proportions from gender_religion to data_wide
data_wide$p_chchm_total <- gender_religion$p_chchm_total[matches_gerel]
data_wide$p_chchm_protest <- gender_religion$p_chchm_protest[matches_gerel]
data_wide$p_chchm_romancath <- gender_religion$p_chchm_romancath[matches_gerel]
data_wide$p_chchm_christcath <- gender_religion$p_chchm_christcath[matches_gerel]
data_wide$p_chchm_christortho <- gender_religion$p_chchm_christortho[matches_gerel]
data_wide$p_chchm_christother <- gender_religion$p_chchm_christother[matches_gerel]
data_wide$p_chchm_jewish <- gender_religion$p_chchm_jewish[matches_gerel]
data_wide$p_chchm_muslim <- gender_religion$p_chchm_muslim[matches_gerel]
data_wide$p_chchm_religionother <- gender_religion$p_chchm_religionother[matches_gerel]
data_wide$p_chchm_atheist <- gender_religion$p_chchm_atheist[matches_gerel]
data_wide$p_chchm_religion_undisclosed <- gender_religion$p_chchm_religion_undisclosed[matches_gerel]
data_wide$p_chchf_total <- gender_religion$p_chchf_total[matches_gerel]
data_wide$p_chchf_protest <- gender_religion$p_chchf_protest[matches_gerel]
data_wide$p_chchf_romancath <- gender_religion$p_chchf_romancath[matches_gerel]
data_wide$p_chchf_christcath <- gender_religion$p_chchf_christcath[matches_gerel]
data_wide$p_chchf_christortho <- gender_religion$p_chchf_christortho[matches_gerel]
data_wide$p_chchf_christother <- gender_religion$p_chchf_christother[matches_gerel]
data_wide$p_chchf_jewish <- gender_religion$p_chchf_jewish[matches_gerel]
data_wide$p_chchf_muslim <- gender_religion$p_chchf_muslim[matches_gerel]
data_wide$p_chchf_religionother <- gender_religion$p_chchf_religionother[matches_gerel]
data_wide$p_chchf_atheist <- gender_religion$p_chchf_atheist[matches_gerel]
data_wide$p_chchf_religion_undisclosed <- gender_religion$p_chchf_religion_undisclosed[matches_gerel]

#checking for nas
which(is.na(data_wide$p_chchf_atheist))

#match based on the composite key for municipality
matches_municipality <- match(data_wide$municipality, gender_religion$mnname)

#update matched columns from old_data to data_wide only where they haven't been matched yet
data_wide$p_chchm_total[is.na(data_wide$p_chchm_total)] <- gender_religion$p_chchm_total[matches_municipality][is.na(data_wide$p_chchm_total)]
data_wide$p_chchm_protest[is.na(data_wide$p_chchm_protest)] <- gender_religion$p_chchm_protest[matches_municipality][is.na(data_wide$p_chchm_protest)]
data_wide$p_chchm_romancath[is.na(data_wide$p_chchm_romancath)] <- gender_religion$p_chchm_romancath[matches_municipality][is.na(data_wide$p_chchm_romancath)]
data_wide$p_chchm_christcath[is.na(data_wide$p_chchm_christcath)] <- gender_religion$p_chchm_christcath[matches_municipality][is.na(data_wide$p_chchm_christcath)]
data_wide$p_chchm_christortho[is.na(data_wide$p_chchm_christortho)] <- gender_religion$p_chchm_christortho[matches_municipality][is.na(data_wide$p_chchm_christortho)]
data_wide$p_chchm_christother[is.na(data_wide$p_chchm_christother)] <- gender_religion$p_chchm_christother[matches_municipality][is.na(data_wide$p_chchm_christother)]
data_wide$p_chchm_jewish[is.na(data_wide$p_chchm_jewish)] <- gender_religion$p_chchm_jewish[matches_municipality][is.na(data_wide$p_chchm_jewish)]
data_wide$p_chchm_muslim[is.na(data_wide$p_chchm_muslim)] <- gender_religion$p_chchm_muslim[matches_municipality][is.na(data_wide$p_chchm_muslim)]
data_wide$p_chchm_religionother[is.na(data_wide$p_chchm_religionother)] <- gender_religion$p_chchm_religionother[matches_municipality][is.na(data_wide$p_chchm_religionother)]
data_wide$p_chchm_atheist[is.na(data_wide$p_chchm_atheist)] <- gender_religion$p_chchm_atheist[matches_municipality][is.na(data_wide$p_chchm_atheist)]
data_wide$p_chchm_religion_undisclosed[is.na(data_wide$p_chchm_religion_undisclosed)] <- gender_religion$p_chchm_religion_undisclosed[matches_municipality][is.na(data_wide$p_chchm_religion_undisclosed)]
data_wide$p_chchf_total[is.na(data_wide$p_chchf_total)] <- gender_religion$p_chchf_total[matches_municipality][is.na(data_wide$p_chchf_total)]
data_wide$p_chchf_protest[is.na(data_wide$p_chchf_protest)] <- gender_religion$p_chchf_protest[matches_municipality][is.na(data_wide$p_chchf_protest)]
data_wide$p_chchf_romancath[is.na(data_wide$p_chchf_romancath)] <- gender_religion$p_chchf_romancath[matches_municipality][is.na(data_wide$p_chchf_romancath)]
data_wide$p_chchf_christcath[is.na(data_wide$p_chchf_christcath)] <- gender_religion$p_chchf_christcath[matches_municipality][is.na(data_wide$p_chchf_christcath)]
data_wide$p_chchf_christortho[is.na(data_wide$p_chchf_christortho)] <- gender_religion$p_chchf_christortho[matches_municipality][is.na(data_wide$p_chchf_christortho)]
data_wide$p_chchf_christother[is.na(data_wide$p_chchf_christother)] <- gender_religion$p_chchf_christother[matches_municipality][is.na(data_wide$p_chchf_christother)]
data_wide$p_chchf_jewish[is.na(data_wide$p_chchf_jewish)] <- gender_religion$p_chchf_jewish[matches_municipality][is.na(data_wide$p_chchf_jewish)]
data_wide$p_chchf_muslim[is.na(data_wide$p_chchf_muslim)] <- gender_religion$p_chchf_muslim[matches_municipality][is.na(data_wide$p_chchf_muslim)]
data_wide$p_chchf_religionother[is.na(data_wide$p_chchf_religionother)] <- gender_religion$p_chchf_religionother[matches_municipality][is.na(data_wide$p_chchf_religionother)]
data_wide$p_chchf_atheist[is.na(data_wide$p_chchf_atheist)] <- gender_religion$p_chchf_atheist[matches_municipality][is.na(data_wide$p_chchf_atheist)]
data_wide$p_chchf_religion_undisclosed[is.na(data_wide$p_chchf_religion_undisclosed)] <- gender_religion$p_chchf_religion_undisclosed[matches_municipality][is.na(data_wide$p_chchf_religion_undisclosed)]

#check for nas
which(is.na(data_wide$p_chchf_atheist))

#fuzzy match for the win
#find rows with NA values in the specified columns
na_rows_gerel <- which(is.na(data_wide$p_chchm_total))

#create a new dataset with only NA rows from data_wide
data_wide_na_gerel <- data_wide[na_rows_gerel, ]

#perform fuzzy matching based on Jaccard similarity
fuzzy_match_jaccard <- function(x, y) {
  distances <- stringdist::stringdistmatrix(x, y, method = "jaccard")
  #find the index of the minimum distance for each row
  index <- apply(distances, 1, which.min)
  #extract the corresponding values from y
  matched_values <- y[index]
  return(matched_values)
}

#loop through each row of data_wide_na_gerel and fill NA values
for (i in 1:nrow(data_wide_na_gerel)) {
  # Extract municipality from data_wide_na_gerel
  municipality <- data_wide_na_gerel$municipality[i]
  
  #perform fuzzy match for municipality
  matched_municipality <- fuzzy_match_jaccard(municipality, gender_religion$mnname)
  
  #update data_wide with matched values only where there are NA values
  if (!is.na(matched_municipality)) {
    data_wide$p_chchm_total[na_rows_gerel[i]] <- gender_religion$p_chchm_total[gender_religion$mnname == matched_municipality]
    data_wide$p_chchm_protest[na_rows_gerel[i]] <- gender_religion$p_chchm_protest[gender_religion$mnname == matched_municipality]
    data_wide$p_chchm_romancath[na_rows_gerel[i]] <- gender_religion$p_chchm_romancath[gender_religion$mnname == matched_municipality]
    data_wide$p_chchm_christcath[na_rows_gerel[i]] <- gender_religion$p_chchm_christcath[gender_religion$mnname == matched_municipality]
    data_wide$p_chchm_christortho[na_rows_gerel[i]] <- gender_religion$p_chchm_christortho[gender_religion$mnname == matched_municipality]
    data_wide$p_chchm_christother[na_rows_gerel[i]] <- gender_religion$p_chchm_christother[gender_religion$mnname == matched_municipality]
    data_wide$p_chchm_jewish[na_rows_gerel[i]] <- gender_religion$p_chchm_jewish[gender_religion$mnname == matched_municipality]
    data_wide$p_chchm_muslim[na_rows_gerel[i]] <- gender_religion$p_chchm_muslim[gender_religion$mnname == matched_municipality]
    data_wide$p_chchm_religionother[na_rows_gerel[i]] <- gender_religion$p_chchm_religionother[gender_religion$mnname == matched_municipality]
    data_wide$p_chchm_atheist[na_rows_gerel[i]] <- gender_religion$p_chchm_atheist[gender_religion$mnname == matched_municipality]
    data_wide$p_chchm_religion_undisclosed[na_rows_gerel[i]] <- gender_religion$p_chchm_religion_undisclosed[gender_religion$mnname == matched_municipality]
    data_wide$p_chchf_total[na_rows_gerel[i]] <- gender_religion$p_chchf_total[gender_religion$mnname == matched_municipality]
    data_wide$p_chchf_protest[na_rows_gerel[i]] <- gender_religion$p_chchf_protest[gender_religion$mnname == matched_municipality]
    data_wide$p_chchf_romancath[na_rows_gerel[i]] <- gender_religion$p_chchf_romancath[gender_religion$mnname == matched_municipality]
    data_wide$p_chchf_christcath[na_rows_gerel[i]] <- gender_religion$p_chchf_christcath[gender_religion$mnname == matched_municipality]
    data_wide$p_chchf_christortho[na_rows_gerel[i]] <- gender_religion$p_chchf_christortho[gender_religion$mnname == matched_municipality]
    data_wide$p_chchf_christother[na_rows_gerel[i]] <- gender_religion$p_chchf_christother[gender_religion$mnname == matched_municipality]
    data_wide$p_chchf_jewish[na_rows_gerel[i]] <- gender_religion$p_chchf_jewish[gender_religion$mnname == matched_municipality]
    data_wide$p_chchf_muslim[na_rows_gerel[i]] <- gender_religion$p_chchf_muslim[gender_religion$mnname == matched_municipality]
    data_wide$p_chchf_religionother[na_rows_gerel[i]] <- gender_religion$p_chchf_religionother[gender_religion$mnname == matched_municipality]
    data_wide$p_chchf_atheist[na_rows_gerel[i]] <- gender_religion$p_chchf_atheist[gender_religion$mnname == matched_municipality]
    data_wide$p_chchf_religion_undisclosed[na_rows_gerel[i]] <- gender_religion$p_chchf_religion_undisclosed[gender_religion$mnname == matched_municipality]
  }
}

which(is.na(data_wide$p_chchf_atheist))

#create the religion share of each municipality
data_wide <- data_wide %>%
  rowwise() %>%
  mutate(p_ch_protest = p_chchm_protest + p_chchf_protest,
         p_ch_romancath = p_chchm_romancath + p_chchf_romancath,
         p_ch_christcath = p_chchm_christcath + p_chchf_christcath,
         p_ch_christortho = p_chchm_christortho + p_chchf_christortho,
         p_ch_christother = p_chchm_christother + p_chchf_christother,
         p_ch_jewish = p_chchm_jewish + p_chchf_jewish,
         p_ch_muslim = p_chchm_muslim + p_chchf_muslim,
         p_ch_religionother = p_chchm_religionother + p_chchf_religionother,
         p_ch_atheist = p_chchm_atheist + p_chchf_atheist,
         p_ch_religion_undisclosed = p_chchm_religion_undisclosed + p_chchf_religion_undisclosed
  ) %>%
  ungroup()


#create binary for male vs. female (due to collinearity)
data_wide <- data_wide %>%
  mutate(gender_mn = ifelse(p_chchf_total >= 50, 1, 0))

which(is.na(data_wide$p_chchm_total))

###Disaster Data matching

disaster <- read.xlsx('Data_Sources/data_disaster_1995_2022.xlsx')
disaster$date <- as.Date(disaster$date, origin = "1899-12-30")

#initialising empty vectors to store matched values
disaster_ID <- rep(NA, nrow(data_wide))
disaster_date <- rep(NA, nrow(data_wide))
main_process <- rep(NA, nrow(data_wide))
damage_extent <- rep(NA, nrow(data_wide))
treated <- rep(NA, nrow(data_wide))

#looping over each row of data_wide
for (i in seq_len(nrow(data_wide))) {
  # Define the range of dates to consider
  date_range <- data_wide$date[i] - 365
  
  #matching indices based on mncode and date criteria within the defined date range
  match_indices <- which(data_wide$mncode[i] == disaster$mncode & 
                           disaster$date <= data_wide$date[i] & 
                           disaster$date >= date_range)
  
  #assigning values if there are matches
  if (length(match_indices) > 0) {
    disaster_ID[i] <- disaster$ID[match_indices]
    disaster_date[i] <- disaster$date[match_indices]
    main_process[i] <- disaster$main_process[match_indices]
    damage_extent[i] <- disaster$damage_extent[match_indices]
    treated[i] <- disaster$treated[match_indices]
  }
}


#matching values to data_wide
data_wide$disaster_ID <- disaster_ID
data_wide$disaster_date <- disaster_date
data_wide$main_process <- main_process
data_wide$damage_extent <- damage_extent
data_wide$treated <- treated

which(is.na(data_wide$treated)) #33858 NA


#step 2: matching with mnname
#initialising empty vectors to store matched values
disaster_ID2 <- rep(NA, nrow(data_wide))
disaster_date2 <- rep(NA, nrow(data_wide))
main_process2 <- rep(NA, nrow(data_wide))
damage_extent2 <- rep(NA, nrow(data_wide))
treated2 <- rep(NA, nrow(data_wide))

#matching loop
for (i in seq_len(nrow(data_wide))) {
  #checking if data_wide$treated is NA
  if(is.na(data_wide$treated[i])) {
    #defining the range of dates to consider
    date_range <- data_wide$date[i] - 365
    
    #matching indices based on mnname and date criteria within the defined date range
    match_indices2 <- which(data_wide$municipality[i] == disaster$mnname & 
                              disaster$date <= data_wide$date[i] & 
                              disaster$date >= date_range)
    
    #assign values if there are matches
    if (length(match_indices2) > 0) {
      disaster_ID2[i] <- disaster$ID[match_indices2]
      disaster_date2[i] <- disaster$date[match_indices2]
      main_process2[i] <- disaster$main_process[match_indices2]
      damage_extent2[i] <- disaster$damage_extent[match_indices2]
      treated2[i] <- disaster$treated[match_indices2]
    }
  }
}


#matching values to data_wide only if treated is NA
data_wide$disaster_ID[is.na(data_wide$treated)] <- disaster_ID2[is.na(data_wide$treated)]
data_wide$disaster_date[is.na(data_wide$treated)] <- disaster_date2[is.na(data_wide$treated)]
data_wide$main_process[is.na(data_wide$treated)] <- main_process2[is.na(data_wide$treated)]
data_wide$damage_extent[is.na(data_wide$treated)] <- damage_extent2[is.na(data_wide$treated)]
data_wide$treated[is.na(data_wide$treated)] <- treated2[is.na(data_wide$treated)]

which(is.na(data_wide$treated)) #33744

#daycounter disaster to votedate
data_wide$disaster_treat_daycount <- ifelse(is.na(data_wide$disaster_date), 0, as.numeric(data_wide$date - data_wide$disaster_date))


###daycounter after event for all votes within mncode
# Convert character dates to Date objects
data_wide$date <- as.Date(data_wide$date)
data_wide$disaster_date <- as.Date(data_wide$disaster_date)

#daycount to monthcount
data_wide$disaster_treat_daycount <- round(data_wide$disaster_treat_daycount/30.44)

data_wide <- data_wide %>%
  rename(disaster_treat_monthcount = disaster_treat_daycount)

# Convert numeric dates to Date objects
data_wide$date <- as.Date(as.character(data_wide$date), format = "%Y-%m-%d")
data_wide$disaster_date <- as.Date(as.character(data_wide$disaster_date), format = "%Y-%m-%d")

#coding damage extent
data_wide <- data_wide %>%
  mutate(damage_extent = case_when(
    is.na(damage_extent) ~ "0",
    damage_extent == "dmgLOW" ~ "1",
    damage_extent == "dmgMEDIUM" ~ "2",
    damage_extent == "dmgHIGH" ~ "3",
    TRUE ~ as.character(damage_extent)  # Keep other values unchanged
  ))


#final steps
#treated to 0
data_wide$treated[is.na(data_wide$treated)] <- 0
data_wide$disaster_treat_monthcount[is.na(data_wide$disaster_treat_monthcount)]

###tidying up
#deleting Schweiz as control for setup
data_wide <- data_wide[data_wide$municipality != "Schweiz", ]

#getting rid of unused columns
delete_columns <- c("composite_key", "composite_key2", "composite_key3", 
                    "composite_key4", "composite_key5", "composite_key6",
                    "average_ja_in_percent", "n_0_to_30", "n_30_to_40",
                    "n_40_to_50", "n_50_to_75", "n_75_to_open", "n_total",
                    "canton_edu_pax_total", "edu_basic_pax_total", "edu_matura_pax_total",
                    "edu_tert_pax_total", "canton_edu_male_total", "edu_male_basic_pax_total", 
                    "edu_male_matura_pax", "edu_male_tert_pax", "canton_edu_female_total", 
                    "edu_female_basic_pax_total", "edu_female_matura_pax", 
                    "age_total", "0-17", "18-30",
                    "31-45", "46-65", "66-85", "86-100", "comp_key_edu", 
                    "comp_key_edu2")

data_wide <- select(data_wide, -one_of(delete_columns))

#to numerical
non_numeric_cols <- c("vote", "municipality", "date", "canton", "disaster_date", "main_process")
numeric_cols <- names(data_wide)[!names(data_wide) %in% non_numeric_cols]
data_wide[, numeric_cols] <- lapply(data_wide[, numeric_cols], function(x) as.numeric(as.character(x)))

#to character
data_wide$damage_extent <- as.character(data_wide$damage_extent)
data_wide$gender_mn <- as.character(data_wide$gender_mn)

#last to % changes
data_wide <- data_wide %>%
  mutate(
    atert_share = atert_share / 100,
    atertII_share = atertII_share / 100)

data_wide <- data_wide %>%
  mutate(
    btert_share = btert_share / 100,
    btertII_share = btertII_share / 100)


# Specify the file name
data_complete_ch <- "data_complete_ch.xlsx"

# Write data_wide to an Excel file in the specified directory
write.xlsx(data_wide, data_complete_ch)

