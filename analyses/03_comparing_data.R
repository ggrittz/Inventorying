#### Comparisons ####

# Loading data
file_path <- here::here("data", "derived-data")
file_name1 <- "all_data.rds" # inventory plus reg, both cycles
file_name2 <- "herb_data.rds" # only herbaria data from SC

all_data <- readRDS(file.path(file_path, file_name1))
herb_data <- readRDS(file.path(file_path, file_name2))

# Make checklist
herb_checklist = plantR::checkList(x = herb_data, n.vouch = 5)

# Filter herbarium data columns
cols <- c("eventDate", "yearIdentified", "monthIdentified", "dayIdentified",
          "dateIdentified", "decimalLongitude.new1", "decimalLatitude.new1",
          "geo.check1", "taxon.rank", "scientificNameStatus", "suggestedName",
          "authorship", "scientificNameFull", "family.new1", "genus",
          "is_iffsc", "is_floristic")
herb_data1 <- herb_data[, cols]


#### Summarising data ####

# All that has been collected in IFFSC (reg+arb inventory only)
length(unique(all_data$family)) # 105 famílias
length(unique(all_data$genus)) # 372 gêneros
length(unique(all_data$spp.author)) # 1109 espécies

# All that has been collected in IFFSC (floristic inventory only)
length(unique(herb_data1$family.new1[herb_data1$is_floristic %in% TRUE])) # 236 famílias
length(unique(herb_data1$genus[herb_data1$is_floristic %in% TRUE])) # 1034 gêneros
length(unique(herb_data1$suggestedName[herb_data1$is_floristic %in% TRUE])) # 2848 espécies






