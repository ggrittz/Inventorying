
# Using data previously downloaded/prepared to compare FlorestaSC data

# For internals
devtools::load_all()

# Necessary library for plotting
library(ggplot2)

# Loading data
file_path <- here::here("data", "derived-data")
file_name1 <- "all_data.rds" # inventory plus reg, both cycles
file_name2 <- "herb_data_lifeforms.rds" # only herbaria data from SC
all_data <- readRDS(file.path(file_path, file_name1))
herb_data <- readRDS(file.path(file_path, file_name2))



# Summarizing data --------------------------------------------------------

# What's the proportion of FURB herbarium data that comes from IFFSC?
# (desconsidering Funghi and data from the outside of SC State)
table(herb_data$is_iffsc)/dim(herb_data)[1] # ~47%

##### All that has been collected in the Forest Inventory #####

# Define the filter conditions for each taxon rank
filter_conditions_family <- function(family) {
  family %in% "NI"
}

filter_conditions_genus <- function(genus) {
  grepl("aceae$", genus) | grepl("^NI$", genus)
}

filter_conditions_spp <- function(spp_author) {
  grepl("aceae$| sp\\. ", spp_author) | grepl("^\\w+$", spp_author) | grepl("^NI$", spp_author)
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
# All genuses: 334
length(unique(unlist(inv_genuses)))
# All species: 941
length(unique(unlist(inv_spp)))


##### All that has been collected in the Floristic Inventory #####
# Families: 201
floristic_families <- unique(herb_data$family.new1[herb_data$is_floristic %in% TRUE])
floristic_families <- floristic_families[!is.na(floristic_families)]
# Genuses: 1029
floristic_genuses <- unique(herb_data$genus[herb_data$is_floristic %in% TRUE &
                                               !grepl("aceae$", herb_data$genus)]) 
floristic_genuses <- floristic_genuses[!is.na(floristic_genuses)]
# Species, subspspecies, and varieties: 2740
floristic_spp <- unique(herb_data$scientificNameFull[herb_data$is_floristic %in% TRUE &
                                                        herb_data$taxon.rank %in% c("species",
                                                                                     "subspecies",
                                                                                     "variety")])
floristic_spp <- floristic_spp[!is.na(floristic_spp)]


##### All that has been collected altogether in the FlorestaSC project #####
# Families: 212
all_families <- unique(c(unique(unlist(inv_families)), floristic_families))
# Genuses: 1068
all_genuses <- unique(c(unique(unlist(inv_genuses)), floristic_genuses))
# Species, subspecies, and varieties: 2,947
all_spp <- sort(unique(c(unique(unlist(inv_spp)), 
                    floristic_spp[!is.na(floristic_spp)])))


# Subsetting species from herb data to simplify the summarization of floristic data
summ_flor <- herb_data[herb_data$scientificNameFull %in% floristic_spp, ]

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

# Today, there are 6106 species of Tracheophyta in Santa Catarina state
# The FlorestaSC project alone collected...
length(summarizedTable$species) / 6106
# 48% of everything that occurs in the State


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
