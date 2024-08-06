library(readxl)

#load
file_path <- '/Users/sire/Desktop/dissertation/Data/Data_sources/data_disaster_1995_2022.xlsx'
df <- read_excel(file_path)

#date column to date type
df$date <- as.Date(df$date)

#extract the year
df$year <- format(df$date, "%Y")

#count freq
yearly_counts <- as.data.frame(table(df$year))
colnames(yearly_counts) <- c("year", "frequency")
yearly_counts$year <- as.numeric(as.character(yearly_counts$year))

#colour adjustement
n <- nrow(yearly_counts)
bar_colors <- colorRampPalette(c(rgb(0, 0, 255, alpha = 0.2, maxColorValue = 255), rgb(0, 0, 255, alpha = 1, maxColorValue = 255)))(n)

#create plot
pdf("Frequency_Over_Years.pdf", width=10, height=5)
par(mar = c(8, 4, 4, 2) + 0.1)  # Adjust margins to make space for rotated labels

barplot(height = yearly_counts$frequency, names.arg = yearly_counts$year, 
        col = bar_colors, border = "black", xlab = "Year", ylab = "Frequency", 
        main = "Natural Disaster Frequency Over Years",
        space = 0.5, las=2)

dev.off()
