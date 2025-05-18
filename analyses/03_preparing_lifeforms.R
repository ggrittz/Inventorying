
# Preparing life forms ----------------------------------------------------

# Loading
# FURB herbarium data previously cleaned through plantR and only SC State registers
file_path <- here::here("data", "derived-data")
file_name <- "herb_data.rds"
herb_data <- readRDS(file.path(file_path, file_name))

# JABOT data to obtain life forms (DwC omits the column with the information)
file_path1 <- here::here("data", "raw-data")
file_name1 <- "furb_herbaria_jabot.csv"
jabot_data <- read.csv(file.path(file_path1, file_name1), sep = ";")


##### CHECK HERE: there are two species which are being considered floristic
##### but are not


# Get plot vegetation type from the sample unit in the end
file_path2 <- here::here("data", "raw-data")
file_name2 <- "UAs_IFFSC.xlsx"
plot_data <- readxl::read_xlsx(file.path(file_path2, file_name2))
plot_data$UA <- as.character(plot_data$UA)

# Filter only the necessary herbarium data columns
cols <- c("catalogNumber", "eventDate", "year", "month", "day",
          "yearIdentified", "monthIdentified", "dayIdentified",
          "dateIdentified", "decimalLatitude.new", "decimalLongitude.new",
          "stateProvince.new", "taxon.rank", "scientificNameStatus", 
          "suggestedName", "suggestedAuthorship", "scientificNameFull", 
          "family.new", "genus")

# "geo.check1" "decimalLongitude.new1", "decimalLatitude.new1" "family.new"
herb_data1 <- herb_data[, cols]

# Edit JABOT life forms to merge with FURB herbarium
lifeform <- jabot_data[, c("codbarras", "projeto")]

# Removing empty
lifeform[lifeform == ""] <- NA

# Prepare the column
lifeform$projeto.edited <- tolower(plantR::rmLatin(lifeform$projeto))

# Tag what is forest inventory and what is floristic
# There are things like iffsc - 0, iffsc - ima-furb convenio 001-2001, and iffsc ua 11
# Should these be kept?
lifeform <- lifeform[grepl("iffsc - 1|iffsc - 2", lifeform$projeto.edited), ]
lifeform$is_iffsc <- TRUE

# Create new columns splitting projeto.edited column
# This is the cycle
lifeform$cycle <- sapply(strsplit(lifeform$projeto.edited, split = "\\/"), function(x) x[1])
# This is the sample unit
lifeform$sample.unit <- sapply(strsplit(lifeform$projeto.edited, split = "\\/"), function(x) x[2])
lifeform$sample.unit <- gsub("^[a-zA-Z]?([0-9]+)[a-zA-Z]?$", "\\1", lifeform$sample.unit)

# This is the potential source/lifeform column
lifeform$projeto.edited.new1 <- sapply(strsplit(lifeform$projeto.edited, split = "\\/"), function(x) x[3])
lifeform$projeto.edited.new1 <- gsub("^(\\S+).*", "\\1", lifeform$projeto.edited.new1)
# This is the potential source/lifeform column
lifeform$projeto.edited.new2 <- sapply(strsplit(lifeform$projeto.edited, split = "\\/"), function(x) x[4])
lifeform$projeto.edited.new2 <- gsub("^(\\S+).*", "\\1", lifeform$projeto.edited.new2)

# Lines below were used to investigate some issues in the db and fix it #
# How many '/'?
# lifeform$separation <- grepl("^[^/]*(/[^/]*){2}[^/]*$", lifeform$projeto)

# Subsetting bad formatted cells to investigate further...
# incomplete <- lifeform[lifeform$separation == TRUE, ]

# Choosing classes that are not considered floristic inventory to remove
##### CHECK HERE LATER FOR MORE EDITS IF NECESSARY #####
to_tag <- c("^fito", "^regen")

# Tag what is floristic only, i.e., not including fito and regen data
lifeform$is_floristic <- !grepl(paste0(to_tag, collapse = "|"),
                                lifeform$projeto.edited.new1) &
  !grepl(paste0(to_tag, collapse = "|"),
         lifeform$projeto.edited.new2)

# Mark the registers that contain life forms somewhere
# Load the previously prepared look up table for life forms
file_name2 <- "lookup_table_lifeforms_edited.csv"
lookup_table <- read.csv(file.path(file_path, file_name2), sep = ";",
                         fileEncoding = "Latin1")
lookup_table[lookup_table == ""] <- NA
lookup_table <- lookup_table[!is.na(lookup_table$new_name2), ]

# Tag registers that contain life form
patterns <- unique(lookup_table$old_name)
patterns <- paste0(patterns, collapse = "|")

# Getting life forms from both columns
lifeform$has_lifeform1 <- sapply(1:nrow(lifeform), function(i) {
  any(grepl(patterns, lifeform$projeto.edited.new1[i]))
})

lifeform$has_lifeform2 <- sapply(1:nrow(lifeform), function(i) {
  any(grepl(patterns, lifeform$projeto.edited.new2[i]))
})

# Populate a new column with the info for the register: does it have a lifeform?
lifeform$has_lifeform <- lifeform$has_lifeform1 | lifeform$has_lifeform2
table(lifeform$has_lifeform) # 26,603 life forms available

# Substitute new life forms names to standardize

# Create a lookup vector to match old names with new names
lookup_vec <- setNames(lookup_table$new_name2, lookup_table$old_name)

# Replace values in lifeform$projeto.edited.new1 and 2
lifeform$projeto.edited.new1 <- lookup_vec[match(lifeform$projeto.edited.new1, names(lookup_vec))]
lifeform$projeto.edited.new2 <- lookup_vec[match(lifeform$projeto.edited.new2, names(lookup_vec))]

# Any cases of double life form for the same register?
problems = lifeform[!is.na(lifeform$projeto.edited.new1) & 
                      !is.na(lifeform$projeto.edited.new2), ] # no

# Manually solve these cases by looking at the original information
# Solution: keep $projeto.edited.new1 life form

# Populate final column
lifeform$final_lifeForm <- ifelse(is.na(lifeform$projeto.edited.new1), lifeform$projeto.edited.new2, 
                        ifelse(is.na(lifeform$projeto.edited.new2), lifeform$projeto.edited.new1, 
                               lifeform$projeto.edited.new1))

# Get FT from plot metadata
lifeform <- dplyr::left_join(lifeform, plot_data[, c("UA", "RF reduzido")],
                             by = c("sample.unit" = "UA"))
names(lifeform)[which(names(lifeform) == "RF reduzido")] <- "FT"

# Considering Restinga to be FOD
lifeform$FT <- gsub("Restinga", "FOD", lifeform$FT)

# Getting phytoregion information for plot 9997 from Santa Catarina map #
# file_name4 <- "Klein_3classes.shp"
# missing_phyto <- lifeform[lifeform$sample.unit %in% "9997", ]
# 
# klein_map <- terra::vect(file.path(file_path2, file_name4))


# Renaming columns and elements before merging with herbarium data
colnames(lifeform)[which(colnames(lifeform) == "sample.unit")] <- "plot"
colnames(lifeform)[which(colnames(lifeform) == "cycle")] <- "source"

lifeform$source <- gsub("^iffsc - 1$", "FLOR1", lifeform$source)
lifeform$source <- gsub("^iffsc - 2$", "FLOR2", lifeform$source)

# Only relevant info
cols <- c("codbarras", "is_iffsc", "is_floristic", 
          "source", "plot", "FT", "final_lifeForm")
lifeform <- lifeform[, cols]

# Merge
herb_data1 <- dplyr::left_join(herb_data1, lifeform,
                               by = c("catalogNumber" = "codbarras"))
colnames(herb_data1)[which(colnames(herb_data1) == "final_lifeForm")] <- "lifeForm"

# Save
path_to_save <- file_path
name_to_save <- "herb_data_lifeforms.rds"
saveRDS(herb_data1, file.path(path_to_save, name_to_save))

rm(list=ls())




