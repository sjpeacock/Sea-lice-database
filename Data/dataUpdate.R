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
library(httpuv)

###############################################################################
###############################################################################
# Fish data
###############################################################################
###############################################################################

###############################################################################
# Import data
###############################################################################

# Import existing data from GitHub
fishDat <- source_data(url = "https://raw.githubusercontent.com/sjpeacock/Sea-lice-database/master/Data/BroughtonSeaLice_fishData.csv")

# Import new data from Google Sheet
gs4_deauth() # Don't need authorization to access this sheet
fishDat.new <- read_sheet("https://docs.google.com/spreadsheets/d/1SgLEdwhlX_TXRXJWB-wm7o5T1J9N0bLld9pcoCKaAqY/edit#gid=0", sheet = "fish_data")

fishDat.new <- fishDat.new[!is.na(fishDat.new$year), ]

# Truncate to include only data not already in GitHub
fishDat.new <- fishDat.new[as.Date(paste(fishDat.new$year, fishDat.new$month, fishDat.new$day, sep = "-")) > max(as.Date(paste(fishDat$year, fishDat$month, fishDat$day, sep = "-"))), ]

###############################################################################
# Run checks
###############################################################################
warnings <- NA

# Do headers match exactly?
boop <- sum(c(1:length(names(fishDat.new)) - match(names(fishDat.new), names(fishDat))))
boop <- boop + (dim(fishDat)[2] - dim(fishDat.new)[2])
if(boop > 0){
	warnings <- c(warnings, "headers don't match")
	print("WARNING: headers don't match") 
}

# Are fish_id consecutive?
if(min(fishDat.new$fish_id) != (max(fishDat$fish_id) + 1)){
	print("fish_id not consecutive") 
	warnings <- c(warnings, "fish_id not consecutive")
}
# Check there's no duplicates
if(sum(fishDat$fish_id %in% fishDat.new$fish_id) > 0){
	print("WARNING: Duplicate fish_id")
	warnings <- c(warnings, "Duplicate fish_id")
}

# site_id not always consecutive, but make sure there's not duplicates
if(sum(fishDat$site_id %in% fishDat.new$site_id) > 0){
	print("WARNING: Duplicate site_id")
	warnings <- c(warnings, "Duplicate site_id")
}

# Locations match
if(sum(fishDat.new$location %in% unique(fishDat$location) == FALSE) > 0){
	print("WARNING: location names don't match")
	warnings <- c(warnings, "location names don't match")
}

# Species match
if(sum(fishDat.new$species %in% unique(fishDat$species) == FALSE) > 0){
	print("WARNING: species don't match")
	warnings <- c(warnings, "species don't match")
}

# Check lengths & heights to see if they conform to existing data
if(sum(fishDat.new$length < 10, fishDat.new$length > 200, na.rm = TRUE) > 0){
	print("WARNING: Check lengths?")
	warnings <- c(warnings, "Check lengths")
}

if(sum(fishDat.new$height < 2, fishDat.new$height > 30, na.rm = TRUE) > 0){
	print("WARNING: Check heights?")
	warnings <- c(warnings, "Check heights")
}

if(sum(fishDat.new$length/fishDat.new$height < min(fishDat$length/fishDat$height, na.rm = TRUE) | fishDat.new$length/fishDat.new$height > max(fishDat$length/fishDat$height, na.rm = TRUE), na.rm = TRUE) >0){
	print("WARNING: length/height out of range")
	warnings <- c(warnings, "length/height out of range")
}

# For each louse stage, check that the numbers are reasonable
for(i in 11:26){
	if(sum(!is.na(fishDat.new[, i])) > 0){
	if(max(fishDat.new[, i], na.rm = TRUE) > 20){
	print(paste("WARNING: ", names(fishDat.new)[i], "seems high."))
	warnings <- c(warnings, paste(names(fishDat.new)[i], "seems high."))
	}}
}

# For presence/absence metric, check that they're just zero or one
for(i in c(27:29, 32:35)){
	if(sum(!is.na(fishDat.new[, i])) > 0){
		if(sum(fishDat.new[!is.na(fishDat.new[, i]), i] != 1) > 0){
			print(paste("WARNING: ", names(fishDat.new)[i], "not NA/1."))
			warnings <- c(warnings, paste(names(fishDat.new)[i], "not NA/1."))
	}}
}

	
if(sum(!is.na(fishDat.new$eroded_gill)) > 0){
	if(sum(fishDat.new$eroded_gill[!is.na(fishDat.new$eroded_gill)] %in% c(1, 2) == FALSE) > 0){
		print("WARNING: eroded_gill not NA, 1, or 2")
		warnings <- c(warnings, "eroded_gill not NA, 1, or 2")
}}

warnings

# If there are no warnings, then merge data

###############################################################################
# Merge data
###############################################################################

fishDat.combined <- rbind(fishDat, fishDat.new)

write.csv(fishDat.combined, file = "BroughtonSeaLice_fishData.csv", row.names = FALSE)

###############################################################################
###############################################################################
# Site data
###############################################################################
###############################################################################

###############################################################################
# Import data
###############################################################################

# Import existing data from GitHub
siteDat <- source_data(url = "https://raw.githubusercontent.com/sjpeacock/Sea-lice-database/master/Data/BroughtonSeaLice_siteData.csv")

# Import new data from Google Sheet
siteDat.new <- read_sheet("https://docs.google.com/spreadsheets/d/1SgLEdwhlX_TXRXJWB-wm7o5T1J9N0bLld9pcoCKaAqY/edit#gid=1365873183", sheet = "site_data", col_types = "iiiicddddiiiiiiiiiddddcc")
siteDat.new <- siteDat.new[!is.na(siteDat.new$year), ]

# Truncate to include only data not already in GitHub
siteDat.new <- siteDat.new[as.Date(paste(siteDat.new$year, siteDat.new$month, siteDat.new$day, sep = "-")) > max(as.Date(paste(siteDat$year, siteDat$month, siteDat$day, sep = "-"))), ]

###############################################################################
# Run checks
###############################################################################
siteWarnings <- NA

# Match headers

# In 2021, salinity and temperature were measured at different depths
# We only kept surface measurements in past, so only store those for now
siteDat.new$salt <- siteDat.new$salt_surf
siteDat.new$temp <- siteDat.new$temp_surf

colInd <- match(names(siteDat), names(siteDat.new))
# Check: cbind(names(siteDat), names(siteDat.new)[colInd])
siteDat.new <- siteDat.new[, colInd]


# site_id not always consecutive, but make sure there's not duplicates
if(sum(siteDat$site_id %in% siteDat.new$site_id) > 0){
	print("WARNING: Duplicate site_id")
	siteWarnings <- c(siteWarnings, "Duplicate site_id")
}

# Locations match
if(sum(siteDat.new$location %in% unique(siteDat$location) == FALSE) > 0){
	print("WARNING: location names don't match")
	siteWarnings <- c(siteWarnings, "location names don't match")
}

# Salt and temp are reasonable range
if(length(which(siteDat.new$salt < 0 | siteDat.new$salt > 33)) > 0){
	print("WARNING: suspicious salinity values")
	siteWarnings <- c(siteWarnings, "suspicious salinity values")
}

if(length(which(siteDat.new$temp < 4 | siteDat.new$temp > 20)) > 0){
	print("WARNING: suspicious temperature values")
	siteWarnings <- c(siteWarnings, "suspicious temperature values")
}

# Numbers of fish captured
if(sum(siteDat.new$salmon_examined != (siteDat.new$pink_examined + siteDat.new$chum_examined + siteDat.new$sockeye_examined)) > 0){
	print("WARNING: total examined not sum of pink, chum, sockeye")
	siteWarnings <- c(siteWarnings, "total examined not sum of pink, chum, sockeye")
	# cbind(siteDat.new$salmon_examined, siteDat.new$pink_examined + siteDat.new$chum_examined + siteDat.new$sockeye_examined)
}

if(length(which(siteDat.new[, c("pink_examined", "chum_examined", "sockeye_examined")] > 150)) > 0){
	print("WARNING: more than 150 sampled for one species")
	siteWarnings <- c(siteWarnings, "more than 150 sampled for one species")
}

if(length(which(siteDat.new[, c("morts_recovery", "morts_other")] > 50)) > 0){
	print("WARNING: high mortalities; check")
	siteWarnings <- c(siteWarnings, "high mortalities; check")
}

# Check latitude and longitude
if(length(which(siteDat.new$latitude > 51 | siteDat.new$latitude < 50.6)) > 0){
	print("WARNING: latitude out of range; check")
	siteWarnings <- c(siteWarnings, "latitude out of range; check")
}

if(length(which(siteDat.new$longitude < -126.9 | siteDat.new$longitude > -126.2)) > 0){
	print("WARNING: longitude out of range; check")
	siteWarnings <- c(siteWarnings, "longitude out of range; check")
}

siteWarnings

# If there are no warnings, then merge data

###############################################################################
# Merge data
###############################################################################

siteDat.combined <- rbind(siteDat, siteDat.new)

write.csv(siteDat.combined, file = "BroughtonSeaLice_siteData.csv", row.names = FALSE)

