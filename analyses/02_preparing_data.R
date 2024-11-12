# Preparing data

#### 1. Preparing inventory data (first and second cycle) ####
file_path <- here::here("data", "raw-data")

# Both cycle of measurements for inventory and regeneration data
file_name1 <- "ArboreoCiclo1_v04mar24.xlsx"
file_name2 <- "ArboreoCiclo2_v04mar24.xlsx"
file_name3 <- "RegCiclo1_v04mar24.xlsx"
file_name4 <- "RegCiclo2_v04mar24.xlsx"

# Loading
cycle_one_arb <- readxl::read_xlsx(file.path(file_path, file_name1))
cycle_one_arb$source <- "INV1"
cycle_two_arb <- readxl::read_xlsx(file.path(file_path, file_name2))
cycle_two_arb$source <- "INV2"
cycle_one_reg <- readxl::read_xlsx(file.path(file_path, file_name3))
cycle_one_reg$source <- "INV1"
cycle_two_reg <- readxl::read_xlsx(file.path(file_path, file_name4))
cycle_two_reg$source <- "INV2"
all_data <- list(cycle_one_arb,
                 cycle_one_reg,
                 cycle_two_arb,
                 cycle_two_reg)


# Overall edits

# Necessary columns
cols <- c("plot", "FT", "date", "lat dec", 
          "long dec", "family", "spp", "spp.author", "source")
for(i in seq_along(all_data)){
  all_data[[i]] <- all_data[[i]][, cols]
}

# Back to data.frame for easier manipulation
all_data <- do.call(rbind.data.frame, all_data)

# Renaming two columns
colnames(all_data)[which(colnames(all_data) == "lat dec")] <- "lat"
colnames(all_data)[which(colnames(all_data) == "long dec")] <- "long"

# Remove 'morta' (dead) class
all_data <- all_data[!grepl("morta", all_data$family, ignore.case = TRUE) & 
           !grepl("morta", all_data$spp.author, ignore.case = TRUE) &
             !grepl("morta", all_data$spp, ignore.case = TRUE), ]

# Substitute RES (Restinga) by FOD
all_data$FT <- gsub("RES", "FOD", all_data$FT)
# table(all_data$FT)


#### Match these names against the BFO ####
# These can be matched using only the canonical name since all of them are unique
all_data1 <- unique(all_data$spp.author)
all_data1 <- plantR::fixAuthors(all_data1)
all_data1 <- all_data1[!is.na(all_data1$tax.author), ]
all_data1 <- plantR::prepSpecies(x = all_data1,
                                 tax.names = c("tax.name", "tax.author"),
                                 use.authors = FALSE,
                                 db = "bfo",
                                 sug.dist = 0.9)

all_data <- dplyr::left_join(all_data, all_data1[, c("orig.name", "scientificNameFull")],
                              by = c("spp.author" = "orig.name"))
all_data$spp.author <- all_data$scientificNameFull
all_data <- all_data[, 1:9]

# Make genus column
all_data$genus <- gsub("^([A-Za-z]+).*",
                       "\\1",
                       all_data$spp.author)
# table(all_data$genus)


#### 2. Preparing herbarium data ####
file_name5 <- "occurrence.txt"
herb <- data.table::fread(file.path(file_path, file_name5))
herb <- as.data.frame(herb)

# Preparing columns dayIdentified, monthIdentfied, yearIdentified

split_date <- function(x) {
  parts <- unlist(strsplit(x, "/"))
  
  # Initialize day, month, and year as NA
  day <- month <- year <- NA
  
  # Assign values based on the number of parts
  if (length(parts) == 3) {
    day <- as.numeric(parts[1])
    month <- as.numeric(parts[2])
    year <- as.numeric(parts[3])
  } else if (length(parts) == 2) {
    month <- as.numeric(parts[1])
    year <- as.numeric(parts[2])
  } else if (length(parts) == 1 && nchar(parts[1]) == 4) {
    year <- as.numeric(parts[1])
  }
  
  return(c(day, month, year))
}

# Apply the function to the column and create new columns
date_split <- t(sapply(herb$dateIdentified, split_date))
herb$dayIdentified <- date_split[, 1]
herb$monthIdentified <- date_split[, 2]
herb$yearIdentified <- date_split[, 3]

# formatDwc
herb <- plantR::formatDwc(splink_data = herb,
                          drop = TRUE)

# formatOcc
herb1 <- plantR::formatOcc(herb)
# formatLoc
herb1 <- plantR::formatLoc(herb1)
# formatCoord
herb1 <- plantR::formatCoord(herb1)
# formatTax
herb1 <- plantR::formatTax(tax = herb1,
                           db = "bfo",
                           split.letters = TRUE,
                           parallel = TRUE,
                           cores = 8)
# validateLoc
herb1 <- plantR::validateLoc(herb1)

# Objects needed to validateCoord
worldMap <- plantR::worldMap
world <- plantR::world
latamMap <- plantR::latamMap
landBuff <- plantR::landBuff
shoreLines <- plantR::shoreLines
islandBuff <- plantR::islandsBuff

# validateCoord
herb1 <- plantR::validateCoord(herb1)

# validateTax
herb1 <- plantR::validateTax(herb1)

# validateDup
herb1 <- plantR::validateDup(herb1) # no duplicates in the data per se

# Removing data from outside of SC state
herb1 <- herb1[herb1$stateProvince.new %in% "santa catarina", ]

##### USING JABOT DATA TO GENERATE THIS STEP #####
# Which registers came from IFFSC?
# herb1$is_iffsc <- grepl("IFFSC", herb1$occurrenceRemarks)
# # From the TRUE ones, how much of them are from the floristic inventory?
# herb1$is_floristic <- FALSE
# 
# # Defining the classes #### CHECK GASPER IF ANY IS MISSING ####
# classes <- c("Herbácea", "Coleta Extra", "Florística", "Epífita")
# 
# # Filter rows where `is_iffsc == TRUE` and check for the presence of classes
# herb1$is_floristic[herb1$is_iffsc == TRUE] <- grepl(paste(classes, 
#                                                           collapse = "|"), 
#                                                     herb1$occurrenceRemarks[herb1$is_iffsc == TRUE])

# Some minor class changes
herb1$decimalLatitude.new1 <- as.numeric(herb1$decimalLatitude.new1)
herb1$decimalLongitude.new1 <- as.numeric(herb1$decimalLongitude.new1)
herb1$yearIdentified <- as.integer(herb1$yearIdentified)
herb1$monthIdentified <- as.integer(herb1$monthIdentified)
herb1$dayIdentified <- as.integer(herb1$dayIdentified)
herb1$eventDate <- as.Date(herb1$eventDate)

# Creating the column 'genus' for herbarium data
herb1$genus <- gsub("^([A-Za-z]+).*",
                    "\\1",
                    herb1$suggestedName)

# Removing Fungi and algae
library(WorldFlora)
data(vascular.families)
herb1 <- herb1[tolower(herb1$family.new1) %in% tolower(vascular.families$Family), ]

# Adjusting family.new1 column to be title-mode
herb1$family.new1 <- stringr::str_to_title(herb1$family.new1)


#### Save everything ####
file_path1 <- here::here("data", "derived-data")
file_name6 <- "all_data.rds"
file_name7 <- "herb_data.rds"
saveRDS(all_data, file.path(file_path1, file_name6))
saveRDS(herb1, file.path(file_path1, file_name7))

rm(list=ls())


