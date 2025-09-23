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

# How many individuals were measured in each Cycle and source (reg or inv)?
# Each tree id is a combination of plot/subplot it was found and tree id number
# Here, we desconsider multiple stems and dead trees
# Regeneration component has no dead individual status
# Tree component Cycle 1
length(unique(paste0(cycle_one_arb$plot[cycle_one_arb$status.tree %in% "Viva"],
                     "_",
                     cycle_one_arb$subplot[cycle_one_arb$status.tree %in% "Viva"],
                     "_",
                     cycle_one_arb$tree[cycle_one_arb$status.tree %in% "Viva"]))) # 99,042
# Reg component Cycle 1
length(unique(paste0(cycle_one_reg$plot,
                     "_",
                     cycle_one_reg$subunit,
                     "_",
                     cycle_one_reg$tree))) # 66,933
# Tree component Cycle 2
length(unique(paste0(cycle_two_arb$plot[cycle_two_arb$status.tree %in% "Viva"],
                     "_",
                     cycle_two_arb$subplot[cycle_two_arb$status.tree %in% "Viva"],
                     "_",
                     cycle_two_arb$tree[cycle_two_arb$status.tree %in% "Viva"]))) # 79,104
# Reg component Cycle 2
length(unique(paste0(cycle_two_reg$plot,
                     "_",
                     cycle_two_reg$subunit,
                     "_",
                     cycle_two_reg$tree))) # 70,938


# Overall edits

# Necessary columns only
cols <- c("plot", "FT", "date", "lat dec", 
          "long dec", "family", "spp", "spp.author", "source")
for(i in seq_along(all_data)){
  all_data[[i]] <- all_data[[i]][, cols]
}

# Back to data.frame for easier manipulation
all_data <- do.call(rbind.data.frame, all_data)

# Names that need to be removed from inventory data
to_remove <- c("Athenaea pogogena",
               "Cinnamodendron axillare",
               "Dendropanax cuneatus",
               "Schinus polygama",
               "Mollinedia widgrenii",
               "Cyclolobium brasiliense",
               "Phytolacca thyrsiflora",
               "Inga barbata",
               "Mollinedia engleriana",
               "Zanthoxylum caribaeum")
all_data <- all_data[!all_data$spp %in% to_remove, ]

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

# Renaming columns for prepSpecies 
names(all_data1)[which(names(all_data1) == "tax.name")] <- "scientificName"
names(all_data1)[which(names(all_data1) == "tax.author")] <- "scientificNameAuthorship"

all_data1 <- plantR::prepSpecies(x = all_data1,
                                 tax.names = c("scientificName", 
                                               "scientificNameAuthorship"),
                                 db = "bfo",
                                 use.authors = TRUE,
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

# Names that need to be changed after name-matching with Flora
names_path <- here::here("data", "raw-data")
names_name <- "names_to_fix.csv"
to_fix <- read.csv(file.path(names_path, names_name), sep = ';', fileEncoding = "UTF-8")
to_fix$Nome_novo <- gsub("Ã¼", "ü", to_fix$Nome_novo, fixed = TRUE)
pattern <- to_fix$Nome_atual
replacement <- to_fix$Nome_novo

for(i in 1:nrow(to_fix)){
  all_data$spp.author <- gsub(pattern[i], 
                              replacement[i], 
                              all_data$spp.author, 
                              fixed = TRUE)
}


#### 2. Preparing herbarium data #### IPT data
file_name5 <- "occurrence.txt"
herb <- data.table::fread(file.path(file_path, file_name5))
herb <- as.data.frame(herb)

# Preparing columns dayIdentified, monthIdentfied, yearIdentified
split_date <- function(x) {
  parts <- unlist(strsplit(x, "/"))

  day <- month <- year <- NA

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

# Formatting DwC data in a data.frame to be used inside plantR
herb <- plantR::formatDwc(splink_data = herb,
                          drop = TRUE)

# Format collector and determiner names, numbers, dates, and collection codes
herb1 <- plantR::formatOcc(herb)
# Standardizes names of administrative levels from species occurrences
herb1 <- plantR::formatLoc(herb1)
# Formats geographical coordinates to decimal degrees and replaces missing
# coordinates by the coordinates obtained from the gazetteer
herb1 <- plantR::formatCoord(herb1)
# Edits and standardizes the names of plant species
herb1 <- plantR::formatTax(herb1,
                           tax.names = c("scientificName", 
                                         "scientificNameAuthorship"),
                           db = "bfo",
                           use.authors = TRUE,
                           sug.dist = 0.9)

# Fixing names here too
pattern1 <- c("Eugenia suberosa Cambess.", "Myrcia ilheosensis Kiaersk.")
replacement1 <- c("Eugenia verticillata (Vell.) Angely", "Myrcia florida Lem.")
to_fix2 <- data.frame(Nome_atual = pattern1,
                      Nome_novo = replacement1)
to_fix <- rbind(to_fix, to_fix2)
pattern <- to_fix$Nome_atual
replacement <- to_fix$Nome_novo

for(i in 1:length(pattern)){
  herb1$scientificNameFull <- gsub(pattern[i],
                                   replacement[i], 
                                   herb1$scientificNameFull,
                                   fixed = TRUE)
}

# # Compares the resolution of the locality information provided in the original
# # record with the resolution retrieved from the gazetteer
# herb1 <- plantR::validateLoc(herb1)
# 
# # Objects needed to validateCoord
# worldMap <- plantR::worldMap
# world <- plantR::world
# latamMap <- plantR::latamMap
# landBuff <- plantR::landBuff
# shoreLines <- plantR::shoreLines
# islandBuff <- plantR::islandsBuff
# 
# # Cross-check the coordinates and check for cases where it falls near the sea
# # shore, open sea, and country boundaries. It tests for swapped/inverted coordinates,
# # searches for cultivated individuals, and finally detects spatial outliers.
# herb1 <- plantR::validateCoord(herb1)
# 
# # Assign a confidence level to the identification of species based on the name of
# # the person who provided the identification
# herb1 <- plantR::validateTax(herb1)
# 
# # Searches for duplicates and homogenize the information, leaving only one occurrence
# # for each group of duplicate.
# herb1 <- plantR::validateDup(herb1) # no duplicates in the data per se

# Some minor class changes
# herb1$decimalLatitude.new1 <- as.numeric(herb1$decimalLatitude.new1)
# herb1$decimalLongitude.new1 <- as.numeric(herb1$decimalLongitude.new1)
herb1$yearIdentified <- as.integer(herb1$yearIdentified)
herb1$monthIdentified <- as.integer(herb1$monthIdentified)
herb1$dayIdentified <- as.integer(herb1$dayIdentified)
herb1$eventDate <- as.Date(herb1$eventDate)

# Creating the column 'genus' for herbarium data
herb1$genus <- gsub("^([A-Za-z]+).*",
                    "\\1",
                    herb1$scientificNameFull)

# Removing Fungi and algae
library(WorldFlora)
data(vascular.families)
herb1 <- herb1[tolower(herb1$family.new) %in% tolower(vascular.families$Family), ]

# Adjusting family.new1 column to be title-mode
herb1$family.new <- stringr::str_to_title(herb1$family.new)

#### Save everything ####
file_path1 <- here::here("data", "derived-data")
file_name6 <- "all_data.rds"
file_name7 <- "herb_data.rds"
saveRDS(all_data, file.path(file_path1, file_name6))
saveRDS(herb1, file.path(file_path1, file_name7))
rm(list=ls())


