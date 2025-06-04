
# Using data previously downloaded/prepared to compare FlorestaSC data

# For internals
devtools::load_all()

# From World Flora Online
library(WorldFlora)
data("vascular.families")

# Loading data
file_path <- here::here("data", "derived-data")
file_name1 <- "all_data.rds" # inventory plus reg, both cycles
file_name2 <- "herb_data_lifeforms.rds"
all_data <- readRDS(file.path(file_path, file_name1))
herb_data <- readRDS(file.path(file_path, file_name2))


# Summarizing data --------------------------------------------------------

# What's the proportion of FURB herbarium data that comes from IFFSC?
# (desconsidering Funghi) 
table(herb_data$is_iffsc)/dim(herb_data)[1] # ~40% (39.29%)

# For following analyses, remove everything that is not from Santa Catarina State
# and not from IFFSC
herb_data <- herb_data[herb_data$stateProvince.new %in% "santa catarina", ]

##### All that has been collected in the Forest Inventory #####

# Define filter conditions for each taxon rank
filter_conditions_family <- function(family) {
  family %in% "NI"
}

filter_conditions_genus <- function(genus) {
  grepl("aceae$", genus) | 
    grepl("^NI$", genus)
}

filter_conditions_spp <- function(spp_author) {
  grepl("aceae$| sp\\. ", spp_author) | 
    grepl("^\\w+$", spp_author) | 
    grepl("^NI$", spp_author)
}

# Define the source and veg.type
# source <- c("CYCLE1", "CYCLE2")
source <- c("INV1", "INV2")
veg.type <- c("FOD", "FOM", "FED")

# Summarize everything using the internal function .summInv
# Summarize families
inv_families <- .summInv(all_data, "family", source, veg.type, filter_conditions_family)
# Summarize genera
inv_genuses <- .summInv(all_data, "genus", source, veg.type, filter_conditions_genus)
# Summarize species
inv_spp <- .summInv(all_data, "spp.author", source, veg.type, filter_conditions_spp)

# All families: 104
length(unique(unlist(inv_families)))
# All genuses: 333
length(unique(unlist(inv_genuses)))
# All species: 918
length(unique(unlist(inv_spp)))


##### All that has been collected in the Floristic Inventory #####
# Families: 208
floristic_families <- unique(herb_data$family.new[herb_data$is_floristic %in% TRUE])

# Genuses: 1079
floristic_genuses <- unique(herb_data$genus[herb_data$is_floristic %in% TRUE &
                                               !grepl("aceae$", herb_data$genus)])

# Species, subspspecies, and varieties: 2679
floristic_spp <- unique(herb_data$scientificNameFull[herb_data$is_floristic %in% TRUE &
                                                        herb_data$taxon.rank %in% c("species",
                                                                                     "subspecies",
                                                                                     "variety")])

##### All that has been collected altogether in the FlorestaSC project #####
# Families: 218
all_families <- unique(c(unique(unlist(inv_families)), floristic_families))
# Genuses: 1117
all_genuses <- unique(c(unique(unlist(inv_genuses)), floristic_genuses))
# Species, subspecies, and varieties: 2,871
all_spp <- sort(unique(c(unique(unlist(inv_spp)), 
                    floristic_spp[!is.na(floristic_spp)])))

# Subsetting species from herb data to simplify the summarization of floristic data
summ_flor <- herb_data[herb_data$scientificNameFull %in% floristic_spp, ]

# What is exclusive to FlorestaSC when compared to the whole herbaria?
exclusive_iffsc <- unique(herb_data$scientificNameFull[herb_data$taxon.rank %in% c("species",
                                                                                   "variety",
                                                                                   "subspecies") &
                                                         herb_data$is_iffsc %in% TRUE])
exclusive_herba <- unique(herb_data$scientificNameFull[herb_data$taxon.rank %in% c("species",
                                                                                   "variety",
                                                                                   "subspecies") &
                                                         herb_data$is_iffsc %in% c(NA, FALSE)])

# 386 spp/subsp/var are exclusive to FlorestaSC when comparing with the whole
# herbarium FURB
length(setdiff(exclusive_iffsc, exclusive_herba))


# Categorize taxons by FT -------------------------------------------------

# Get missing plot FT from other file
file_path1 <- here::here("data", "raw-data")
file_name3 <- "missing_plots_iffsc.ods"
missing_plots <- readODS::read_ods(file.path(file_path1, file_name3))
missing_plots$plot <- as.character(missing_plots$plot)
names(missing_plots)[which(names(missing_plots) == "região")] <- "FT"

# Merge with summ_flor file
df_merged <- dplyr::left_join(summ_flor, missing_plots, by = "plot", suffix = c(".df1", ".df2"))
df_merged$FT <- ifelse(is.na(df_merged$FT.df1), df_merged$FT.df2, df_merged$FT.df1)
summ_flor <- df_merged[, -which(colnames(df_merged) %in% c("FT.df2", "FT.df1"))]

# Now we need to get FT directly from a map for missing plots:
# 01, 04, 9997, arborea, coleta extra, mh
cols <- c("plot", "FT", "decimalLongitude.new", "decimalLatitude.new")
missing_ft <- summ_flor[summ_flor$is_iffsc %in% TRUE &
                          is.na(summ_flor$FT), cols]
missing_ft <- terra::vect(missing_ft, geom = c("decimalLongitude.new",
                                               "decimalLatitude.new"),
                          crs = "epsg:4326")

# Load the state shapefile
file_name4 <- "maps/Klein_3classes.shp"
klein_map <- terra::vect(file.path(file_path1, file_name4))
klein_map <- terra::project(klein_map, missing_ft)
klein_map <- klein_map['Fitofision']

# Extract the FT from the map
missing_ft_new = terra::extract(klein_map, missing_ft)
missing_ft_new$Fitofision <- gsub("Floresta Estacional Decidual", "FED",
                                  missing_ft_new$Fitofision)
missing_ft_new$Fitofision <- gsub("Floresta Ombrofila Densa", "FOD",
                                  missing_ft_new$Fitofision)
missing_ft_new$Fitofision <- gsub("Floresta Ombrofila Mista", "FOM",
                                  missing_ft_new$Fitofision)

# Now get it back
missing_ft_filled <- terra::as.data.frame(missing_ft, geom = "XY")
missing_ft_filled$FT <- missing_ft_new$Fitofision

# If missing, it is FOD
rep_these <- which(is.na(missing_ft_filled$FT))
missing_ft_filled$FT[rep_these] <- "FOD"

# Finally, add the new filled information to summ_flor
matched_indices <- match(summ_flor$plot, missing_ft_filled$plot) # Find matching rows
summ_flor$FT[is.na(summ_flor$FT)] <- missing_ft_filled$FT[matched_indices][is.na(summ_flor$FT)]

# # Things to fix in the herbarium (done)
# to_gasper <- summ_flor[summ_flor$plot %in% c("mh", "coleta extra", "9997"), ]
# 
# # Load jabot 'projeto' field
# file_name5 <- "furb_herbaria_jabot.csv"
# jabot <- data.table::fread(file.path(file_path1, file_name5))
# to_gasper1 <- dplyr::left_join(to_gasper[, c("catalogNumber", "plot", "FT")],
#                                jabot[, c("codbarras", "projeto")],
#                                by = c("catalogNumber" = "codbarras"))
# file_name6 <- "fix_jabot.csv"
# write.csv(to_gasper1, file.path(file_path, file_name6))

# FOD # 93 families
fod_fam <- unique(c(inv_families$family_INV1_FOD,
                    inv_families$family_INV2_FOD,
                    summ_flor$family.new[summ_flor$FT %in% "FOD" &
                                            summ_flor$is_floristic %in% TRUE]))
# FOD # 844 genera
fod_gen <- unique(c(inv_genuses$genus_INV1_FOD,
                    inv_genuses$genus_INV2_FOD,
                    summ_flor$genus[summ_flor$FT %in% "FOD" &
                                      summ_flor$is_floristic %in% TRUE]))
# FOD # 2357 species/subspecies/varieties
fod_spp <- unique(c(inv_spp$spp.author_INV1_FOD,
                    inv_spp$spp.author_INV2_FOD,
                    summ_flor$scientificNameFull[summ_flor$FT %in% "FOD" &
                                            summ_flor$is_floristic %in% TRUE]))

# FOM # 82 families
fom_fam <- unique(c(inv_families$family_INV1_FOM,
                    inv_families$family_INV2_FOM,
                    summ_flor$family.new[summ_flor$FT %in% "FOM" &
                                            summ_flor$is_floristic %in% TRUE]))
# FOM # 650 genera
fom_gen <- unique(c(inv_genuses$genus_INV1_FOM,
                    inv_genuses$genus_INV2_FOM,
                    summ_flor$genus[summ_flor$FT %in% "FOM" &
                                      summ_flor$is_floristic %in% TRUE]))
# FOM # 1584 species/subspecies/varieties
fom_spp <- unique(c(inv_spp$spp.author_INV1_FOM,
                    inv_spp$spp.author_INV2_FOM,
                    summ_flor$scientificNameFull[summ_flor$FT %in% "FOM" &
                                            summ_flor$is_floristic %in% TRUE]))
# FED # 65 families
fed_fam <- unique(c(inv_families$family_INV1_FED,
                    inv_families$family_INV2_FED,
                    summ_flor$family.new[summ_flor$FT %in% "FED" &
                                            summ_flor$is_floristic %in% TRUE]))
# FED # 398 genera
fed_gen <- unique(c(inv_genuses$genus_INV1_FED,
                    inv_genuses$genus_INV2_FED,
                    summ_flor$genus[summ_flor$FT %in% "FED" &
                                      summ_flor$is_floristic %in% TRUE]))
# FED # 662 species/subspecies/varieties
fed_spp <- unique(c(inv_spp$spp.author_INV1_FED,
                    inv_spp$spp.author_INV2_FED,
                    summ_flor$scientificNameFull[summ_flor$FT %in% "FED" &
                                            summ_flor$is_floristic %in% TRUE]))


# Making a data.frame that comprises all species separated by:
# Forest inventory, floristic inventory, vegetation type
summarizedTable <- data.frame(
  species = all_spp,
  INV_all = all_spp %in% unique(unlist(inv_spp)),
  FLOR_all = all_spp %in% floristic_spp,
  # First cycle of forest sampling
  INV_FOD1 = all_spp %in% inv_spp$spp.author_INV1_FOD,
  INV_FOM1 = all_spp %in% inv_spp$spp.author_INV1_FOM,
  INV_FED1 = all_spp %in% inv_spp$spp.author_INV1_FED,
  # Second cycle of forest sampling
  INV_FOD2 = all_spp %in% inv_spp$spp.author_INV2_FOD,
  INV_FOM2 = all_spp %in% inv_spp$spp.author_INV2_FOM,
  INV_FED2 = all_spp %in% inv_spp$spp.author_INV2_FED,
  # First cycle of floristic sampling
  FLOR_FOD1 = all_spp %in% unique(summ_flor$scientificNameFull[summ_flor$source %in%
                                                                 "FLOR1" &
                                                                 summ_flor$FT %in% "FOD"]),
  FLOR_FOM1 = all_spp %in% unique(summ_flor$scientificNameFull[summ_flor$source %in%
                                                                 "FLOR1" &
                                                                 summ_flor$FT %in% "FOM"]),
  FLOR_FED1 = all_spp %in% unique(summ_flor$scientificNameFull[summ_flor$source %in%
                                                                 "FLOR1" &
                                                                 summ_flor$FT %in% "FED"]),
  # Second cycle of floristic sampling
  FLOR_FOD2 = all_spp %in% unique(summ_flor$scientificNameFull[summ_flor$source %in%
                                                                 "FLOR2" &
                                                                 summ_flor$FT %in% "FOD"]),
  FLOR_FOM2 = all_spp %in% unique(summ_flor$scientificNameFull[summ_flor$source %in%
                                                                 "FLOR2" &
                                                                 summ_flor$FT %in% "FOM"]),
  FLOR_FED2 = all_spp %in% unique(summ_flor$scientificNameFull[summ_flor$source %in%
                                                                 "FLOR2" &
                                                                 summ_flor$FT %in% "FED"])
)

# Add the family to the spreadsheet
summarizedTable$genus <- gsub("^([A-Za-z]+).*",
                              "\\1",
                              summarizedTable$species)
summarizedTable$family <- plantR::getFamily(summarizedTable$genus)

# Get higher category from WorldFlora
summarizedTable <- dplyr::left_join(summarizedTable,
                                    vascular.families[, c("Group", "Family")],
                                    by = c("family" = "Family"))

# # From herbarium data
# summarizedTable <- dplyr::left_join(summarizedTable,
#                                     herb_data[, c("scientificNameFull",
#                                                   "family.new")],
#                                     by = c("species" = "scientificNameFull"),
#                                     multiple = "first")
# # From inventory data
# get_these <- dplyr::left_join(summarizedTable[is.na(summarizedTable$family.new), ],
#                               all_data[, c("spp.author", "family")],
#                               by = c("species" = "spp.author"),
#                               multiple = "first")
# get_these$family.new <- get_these$family
# get_these <- get_these[, -which(names(get_these) == "family")]
# 
# # Merge both and reorder stuff
# summarizedTable <- rbind.data.frame(summarizedTable, get_these)
# summarizedTable <- summarizedTable[order(summarizedTable$species), ]
# names(summarizedTable)[which(names(summarizedTable) == "family.new")] <- "family"
# summarizedTable <- summarizedTable[, c(length(summarizedTable), 1:(length(summarizedTable) -1 ))]
# # Some duplicates for some unknown reason...removing them
# summarizedTable <- summarizedTable[!duplicated(summarizedTable$species), ]

# There are 6,587 species of Tracheophyta in Santa Catarina (14/05/2025)
# The FlorestaSC project alone collected...
length(summarizedTable$species) / 6587
# ~50% of everything that occurs in the State

# How many species were posteriorly identified?

# Loading information from the first cycle (Gasper et al. 2014 papers)
file_path <- here::here("data", "raw-data")
file_name <- "cycle1.xlsx"

# Angiosperms and gymnosperms
c1_angym <- readxl::read_xlsx(file.path(file_path, file_name), sheet = 4)
# Ferns and allies
c1_ferns <- readxl::read_xlsx(file.path(file_path, file_name), sheet = 5)
# Prepare this columns the same way as c1_angym
names(c1_ferns)[5:7] <- c("fito1", "fito2", "fito3")

# Make long format
c1_ferns <- tidyr::pivot_longer(data = c1_ferns,
                                cols = c("fito1", "fito2", "fito3"),
                                names_to = NULL,
                                values_to = "Fito",
                                values_drop_na = TRUE)

# Bind both of them
c1_all <- rbind.data.frame(c1_angym, c1_ferns)

# Make full scientific name
c1_all$scientificName <- ifelse(is.na(c1_all$Epíteto), c1_all$Gênero,
                                paste(c1_all$Gênero, c1_all$Epíteto))

# Check names against BFO
c1_all <- plantR::prepSpecies(c1_all,
                              tax.names = c("scientificName", "Autor"),
                              use.authors = TRUE)

# Names that need to be changed after name-matching with Flora
names_path <- here::here("data", "raw-data")
names_name <- "names_to_fix.csv"
to_fix <- read.csv(file.path(names_path, names_name), sep = ';', fileEncoding = "UTF-8")
to_fix$Nome_novo <- gsub("Ã¼", "ü", to_fix$Nome_novo, fixed = TRUE)
pattern <- to_fix$Nome_atual
replacement <- to_fix$Nome_novo

for(i in 1:nrow(to_fix)){
  c1_all$scientificNameFull <- gsub(pattern[i],
                                    replacement[i],
                                    c1_all$scientificNameFull,
                                    fixed = TRUE)
  }


# Make genus column
c1_all$genus <- gsub("^([A-Za-z]+).*",
                     "\\1",
                     c1_all$scientificNameFull)

# Save for Gasper
# file_path <- here::here("data", "derived-data")
# write.csv(c1_all[c1_all$tax.notes %in% "not found", ], 
#           file.path(file_path, "not_found.csv"))

# Taxon vectors from C1 for comparison with C1 updated
c1_fam <- unique(c1_all$Família) # 184
c1_gen <- unique(c1_all$genus) # 827
c1_spp <- unique(c1_all$scientificNameFull[c1_all$taxon.rank %in% c("species",
                                                                    "subspecies",
                                                                    "variety")]) # 2044
# C1 updated vectors for comparison
# 196 families
c1_fam_new <- unique(c(inv_families$family_INV1_FOD,
                       inv_families$family_INV1_FED,
                       inv_families$family_INV1_FOM,
                       summ_flor$family.new[summ_flor$source %in% "FLOR1" &
                                           summ_flor$is_floristic %in% TRUE]))
# 891 genera
c1_gen_new <- unique(c(inv_genuses$genus_INV1_FOD,
                       inv_genuses$genus_INV1_FED,
                       inv_genuses$genus_INV1_FOM,
                       summ_flor$genus[summ_flor$source %in% "FLOR1" &
                                         summ_flor$is_floristic %in% TRUE]))
# 2591 species/subsp./var.
c1_spp_new <- unique(c(inv_spp$spp.author_INV1_FOD,
                       inv_spp$spp.author_INV1_FED,
                       inv_spp$spp.author_INV1_FOM,
                       summ_flor$scientificNameFull[summ_flor$source %in% "FLOR1" &
                                                      summ_flor$is_floristic %in% TRUE &
                                                      summ_flor$taxon.rank %in% c("species",
                                                                                  "subspecies",
                                                                                  "variety")]))


# Get all columns ending with "1"
cycle1_columns <- grep("1$", names(summarizedTable), value = TRUE)  
# Unique species in ANY cycle1 column
species_in_any_cycle1 <- summarizedTable$species[rowSums(summarizedTable[, cycle1_columns]) > 0]
total_unique_cycle1 <- length(unique(species_in_any_cycle1))
# Get all columns ending with "2"
cycle2_columns <- grep("2$", names(summarizedTable), value = TRUE)  
# Unique species in ANY cycle2 column
species_in_any_cycle2 <- summarizedTable$species[rowSums(summarizedTable[, cycle2_columns]) > 0]
total_unique_cycle2 <- length(unique(species_in_any_cycle2))

# For FOD1 (INV_FOD1 + FLOR_FOD1)
species_in_FOD1 <- summarizedTable$species[summarizedTable$INV_FOD1 | summarizedTable$FLOR_FOD1]
unique_FOD1 <- length(unique(species_in_FOD1))
# For FOM1 (INV_FOM1 + FLOR_FOM1)
species_in_FOM1 <- summarizedTable$species[summarizedTable$INV_FOM1 | summarizedTable$FLOR_FOM1]
unique_FOM1 <- length(unique(species_in_FOM1))
# For FED1 (INV_FED1 + FLOR_FED1)
species_in_FED1 <- summarizedTable$species[summarizedTable$INV_FED1 | summarizedTable$FLOR_FED1]
unique_FED1 <- length(unique(species_in_FED1))
# For FOD2 (INV_FOD2 + FLOR_FOD2)
species_in_FOD2 <- summarizedTable$species[summarizedTable$INV_FOD2 | summarizedTable$FLOR_FOD2]
unique_FOD2 <- length(unique(species_in_FOD2))
# For FOM2 (INV_FOM2 + FLOR_FOM2)
species_in_FOM2 <- summarizedTable$species[summarizedTable$INV_FOM2 | summarizedTable$FLOR_FOM2]
unique_FOM2 <- length(unique(species_in_FOM2))
# For FED2 (INV_FED2 + FLOR_FED2)
species_in_FED2 <- summarizedTable$species[summarizedTable$INV_FED2 | summarizedTable$FLOR_FED2]
unique_FED2 <- length(unique(species_in_FED2))



# Comparison of pre-post updated names
# New families: 20
length(setdiff(c1_fam_new, c1_fam))
# New genus: 117
length(setdiff(c1_gen_new, c1_gen))
# New species: 714
length(setdiff(c1_spp_new, c1_spp))

# Preparing objects that are going to be needed in the following script
inv_spp1 <- unique(unlist(inv_spp[grepl("INV1", names(inv_spp))]))
inv_spp2 <- unique(unlist(inv_spp[grepl("INV2", names(inv_spp))]))
flor_spp1 <- unique(summ_flor$scientificNameFull[summ_flor$source %in% "FLOR1"])
flor_spp2 <- unique(summ_flor$scientificNameFull[summ_flor$source %in% "FLOR2"])

# Saving them
file_path1 <- here::here("data", "derived-data")
file_path2 <- here::here("outputs")
file_name3 <- "inv_spp1.rds"
file_name4 <- "inv_spp2.rds" 
file_name5 <- "flor_spp1.rds" 
file_name6 <- "flor_spp2.rds" 
file_name7 <- "summary_table.xlsx"

saveRDS(inv_spp1, file.path(file_path1, file_name3))
saveRDS(inv_spp2, file.path(file_path1, file_name4))
saveRDS(flor_spp1, file.path(file_path1, file_name5))
saveRDS(flor_spp2, file.path(file_path1, file_name6))
writexl::write_xlsx(summarizedTable, file.path(file_path2, file_name7))


# BONUSES!!!!! ------------------------------------------------------------
families <- herb_data[, c("family.new", "scientificNameFull", "taxon.rank", "is_floristic")]
families <- families[families$taxon.rank %in% c("subspecies",
                                                "species",
                                                "variety") &
                       families$is_floristic %in% TRUE, ]

families <- dplyr::left_join(families, vascular.families[, c("Group", "Family")],
                             by = c("family.new" = "Family"))
families1 <- families[!duplicated(families$scientificNameFull), ]

# Cycle 2 separated by vegetation types (FOD, FOM, FED)
aggregate(scientificNameFull ~ FT, 
          data = herb_data[herb_data$source %in% "FLOR1", ], 
          FUN = function(x) length(unique(x)))
# 428 spp/subsp/var FED
# 1270 spp/subsp/var FOD
# 957 spp/subsp/var FOM
rm(list=ls())

##### Some summaries again #####
summ <- readxl::read_xlsx(here::here("outputs", "summary_table.xlsx"))






