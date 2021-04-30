###############################################################################
###############################################################################
#
# This code for updating the Salmon Coast Field Station's Sea Lice Monitoring 
# Database was added in April 2021 to facilitate in season updates.
#
# It is included here to make the data checking steps transparent.
#
# Please send feedback/comments to stephanie dot j dot peacock @gmail.com
#
###############################################################################
###############################################################################

library(googlesheets4)
library(repmis)

###############################################################################
# Import data
###############################################################################

# Import existing data from GitHub
fishDat <- source_data(url = "https://raw.githubusercontent.com/sjpeacock/Sea-lice-database/master/Data/BroughtonSeaLice_fishData.csv")

# Import new data from Google Sheet
newDat <- read_sheet("https://docs.google.com/spreadsheets/d/10lKRyU4awJVBM9lt5fXlooOWThxdTEyZRFcldiofpyA/edit#gid=0")
newDat <- newDat[!is.na(newDat$year), ]

# Truncate to include only data not already in GitHub
newDat <- newDat[as.Date(paste(newDat$year, newDat$month, newDat$day, sep = "-")) > max(as.Date(paste(fishDat$year, fishDat$month, fishDat$day, sep = "-"))), ]

###############################################################################
# Run checks
###############################################################################
warnings <- NA

# Do headers match exactly?
boop <- sum(c(1:length(names(newDat)) - match(names(newDat), names(fishDat))))
boop <- boop + (dim(fishDat)[2] - dim(newDat)[2])
if(boop > 0){
	warnings <- c(warnings, "headers don't match")
	print("WARNING: headers don't match") 
}

# Are fish_id consecutive?
if(min(newDat$fish_id) != (max(fishDat$fish_id) + 1)){
	print("fish_id not consecutive") 
	warnings <- c(warnings, "fish_id not consecutive")
}
# Check there's no duplicates
if(sum(fishDat$fish_id %in% newDat$fish_id) > 0){
	print("WARNING: Duplicate fish_id")
	warnings <- c(warnings, "Duplicate fish_id")
}

# site_id not always consecutive, but make sure there's not duplicates
if(sum(fishDat$site_id %in% newDat$site_id) > 0){
	print("WARNING: Duplicate site_id")
	warnings <- c(warnings, "Duplicate site_id")
}

# Locations match
if(sum(newDat$location %in% unique(fishDat$location) == FALSE) > 0){
	print("WARNING: location names don't match")
	warnings <- c(warnings, "location names don't match")
}

# Species match
if(sum(newDat$species %in% unique(fishDat$species) == FALSE) > 0){
	print("WARNING: species don't match")
	warnings <- c(warnings, "species don't match")
}

# Check lengths & heights to see if they conform to existing data
if(sum(newDat$length < 10, newDat$length > 200) > 0){
	print("WARNING: Check lengths?")
	warnings <- c(warnings, "Check lengths")
}

if(sum(newDat$height < 2, newDat$height > 30) > 0){
	print("WARNING: Check heights?")
	warnings <- c(warnings, "Check heights")
}

if(sum(newDat$length/newDat$height < min(fishDat$length/fishDat$height, na.rm = TRUE) | newDat$length/newDat$height > max(fishDat$length/fishDat$height, na.rm = TRUE))>0){
	print("WARNING: length/height out of range")
	warnings <- c(warnings, "length/height out of range")
}

# For each louse stage, check that the numbers are reasonable
for(i in 11:26){
	if(sum(!is.na(newDat[, i])) > 0){
	if(max(newDat[, i], na.rm = TRUE) > 20){
	print(paste("WARNING: ", names(newDat)[i], "seems high."))
	warnings <- c(warnings, paste(names(newDat)[i], "seems high."))
	}}
}

# For presence/absence metric, check that they're just zero or one
for(i in c(27:29, 32:35)){
	if(sum(!is.na(newDat[, i])) > 0){
		if(sum(newDat[!is.na(newDat[, i]), i] != 1) > 0){
			print(paste("WARNING: ", names(newDat)[i], "not NA/1."))
			warnings <- c(warnings, paste(names(newDat)[i], "not NA/1."))
	}}
}

	
if(sum(!is.na(newDat$eroded_gill)) > 0){
	if(sum(newDat$eroded_gill[!is.na(newDat$eroded_gill)] %in% c(1, 2) == FALSE) > 0){
		print("WARNING: eroded_gill not NA, 1, or 2")
		warnings <- c(warnings, "eroded_gill not NA, 1, or 2")
}}

warnings

# If there are no warnings, then merge data

###############################################################################
# Merge data
###############################################################################

fishDat <- rbind(fishDat, newDat)

write.csv(fishDat, file = "BroughtonSeaLice_fishData.csv")
