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
cycle_one_arb$source <- "ARB1"
cycle_two_arb <- readxl::read_xlsx(file.path(file_path, file_name2))
cycle_two_arb$source <- "ARB2"
cycle_one_reg <- readxl::read_xlsx(file.path(file_path, file_name3))
cycle_one_reg$source <- "REG1"
cycle_two_reg <- readxl::read_xlsx(file.path(file_path, file_name4))
cycle_two_reg$source <- "REG2"
all_data <- list(cycle_one_arb,
                 cycle_one_reg,
                 cycle_two_arb,
                 cycle_two_reg)


# Overall edits

# Necessary columns
cols <- c("plot", "FT", "date", "lat dec", 
          "long dec", "family", "spp.author", "source")
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
           !grepl("morta", all_data$spp.author, ignore.case = TRUE), ]

# Substitute RES (Restinga) by FOD
all_data$FT <- gsub("RES", "FOD", all_data$FT)
# table(all_data$FT)

# Make genus column
all_data$genus <- gsub("^([A-Za-z]+).*",
                       "\\1",
                       all_data$spp.author)
# table(all_data$genus)


#### 2. Preparing herbarium data ####
file_name5 <- "occurrence.txt"
herb <- data.table::fread(file.path(file_path, file_name5))
herb <- as.data.frame(herb)


# Clean and prepare data using plantR
colnames(herb)[which(colnames(herb) == "year")] <- "yearIdentified"
colnames(herb)[which(colnames(herb) == "month")] <- "monthIdentified"
colnames(herb)[which(colnames(herb) == "day")] <- "dayIdentified"

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

# Which registers came from IFFSC?
herb1$is_iffsc <- grepl("IFFSC", herb1$occurrenceRemarks)

# From the TRUE ones, how much of them are from the floristic inventory?
herb1$is_floristic <- FALSE

# Defining the classes #### CHECK GASPER IF ANY IS MISSING ####
classes <- c("Herbácea", "Coleta Extra", "Florística", "Epífita")

# Filter rows where `is_iffsc == TRUE` and check for the presence of classes
herb1$is_floristic[herb1$is_iffsc == TRUE] <- grepl(paste(classes, 
                                                          collapse = "|"), 
                                                    herb1$occurrenceRemarks[herb1$is_iffsc == TRUE])

# Some minor class issues
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

#### Save everything ####
file_path1 <- here::here("data", "derived-data")
file_name6 <- "all_data.rds"
file_name7 <- "herb_data.rds"
saveRDS(all_data, file.path(file_path1, file_name6))
saveRDS(herb1, file.path(file_path1, file_name7))

rm(list=ls())


