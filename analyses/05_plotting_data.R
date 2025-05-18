

# Plotting data -----------------------------------------------------------
library(ggplot2)
library(tidyr)
library(dplyr)
library(gridExtra)
library(grid)
library(VennDiagram)

# Read necessary data
file_path <- here::here("data", "derived-data")
file_path1 <- here::here("outputs")
file_name1 <- "inv_spp1.rds"
file_name2 <- "inv_spp2.rds" 
file_name3 <- "flor_spp1.rds" 
file_name4 <- "flor_spp2.rds" 
file_name5 <- "herb_data_lifeforms.rds"
file_name6 <- "summary_table.xlsx"

inventory_spp1 <- readRDS(file.path(file_path, file_name1))
inventory_spp2 <- readRDS(file.path(file_path, file_name2))
floristic_spp1 <- readRDS(file.path(file_path, file_name3))
floristic_spp2 <- readRDS(file.path(file_path, file_name4))
herb_data <- readRDS(file.path(file_path, file_name5))
summ_table <- readxl::read_xlsx(file.path(file_path1, file_name6))


# Pie chart for life forms ------------------------------------------------
to_pie <- data.frame(table(herb_data$lifeForm[herb_data$is_floristic %in% TRUE]))
to_pie$Var1 <- as.character(to_pie$Var1)

# Calculate the percentage for each value
percentages <- round(100 * to_pie$Freq / sum(to_pie$Freq), 2)

# Combine category names with both values and percentages
labels <- paste(to_pie$Var1, "\n", to_pie$Freq, " (", percentages, "%)", sep = "")

# Define the SVG file name and dimensions
svg("figures/pie_chart.svg", width = 10, height = 8)  # Adjust the dimensions as needed

# Create the pie chart with both value and percentage labels
colors <- RColorBrewer::brewer.pal(n = length(to_pie$Var1), name = "Blues")

pie(to_pie$Freq, labels = labels, 
    main = paste0("Growth forms represented in the Floristic Inventory samples", 
                 "\n", 
                 "(", "N", " = ", sum(to_pie$Freq), ")"),
    col = colors,
    cex = 1.25)

dev.off()

# Venn Diagram for ecoregions ---------------------------------------------
summ_table_filtered <- summ_table %>% 
  select(species, matches("_(FOM|FOD|FED)\\d*$"))
names(summ_table_filtered) <- gsub("(FOM|FOD|FED)\\d*$", "\\1", 
                                   names(summ_table_filtered))

result <- summ_table_filtered %>% 
  pivot_longer(
    cols = -species,
    names_to = "source",
    values_to = "present"
  ) %>%
  filter(present) %>%  # Keep only rows where present = TRUE
  mutate(er = gsub(".*_(FOM|FOD|FED)$", "\\1", source)) %>%  # Extract FOM/FOD/FED
  select(species, er)  # Keep only these two columns
result <- result[!duplicated(result), ]

# Split species by their er type
FOM_spp <- unique(result$species[result$er == "FOM"])
FOD_spp <- unique(result$species[result$er == "FOD"])
FED_spp <- unique(result$species[result$er == "FED"])


# Custom function to create Venn grobs with your exact parameters
create_venn_grob <- function(venn_list, is_3way = TRUE) {
  grid.newpage()
  
  if (is_3way) {
    # Parameters for FOM/FOD/FED Venn (3-way)
    venn_grob <- venn.diagram(
      x = venn_list,
      filename = NULL,
      disable.logging = TRUE,
      category.names = names(venn_list),
      print.mode = c("raw", "percent"),
      imagetype = "none",
      fontfamily = "sans",
      cat.fontfamily = "sans",
      main.fontfamily = "sans",
      height = 10,
      width = 10,
      units = "in",
      cex = 2,
      cat.cex = 2.5,
      lwd = 1.5,
      alpha = 0.75,
      margin = 0.075,
      cat.dist = c(0.075, 0.075, 0.05),
      cat.pos = c(330, 30, 180),
      col = "black",
      fill = c("#BDD7E7", "#6BAED6", "#3182BD")
    )
  } else {
    # Parameters for Forest/Floristic Venn (2-way)
    venn_grob <- venn.diagram(
      x = venn_list,
      filename = NULL,
      disable.logging = TRUE,
      category.names = names(venn_list),
      print.mode = c("raw", "percent"),
      imagetype = "none",
      fontfamily = "sans",
      cat.fontfamily = "sans",
      main.fontfamily = "sans",
      height = 10,
      width = 10,
      units = "in",
      cex = 2,
      cat.cex = 2.5,
      lwd = 1.5,
      alpha = 0.75,
      margin = 0.075,
      cat.pos = c(180, 180),
      col = "black",
      fill = c("#BDD7E7", "#6BAED6")
    )
  }
  
  grid.draw(venn_grob)
  grid.grab()
}

# Create grobs with your exact parameters
grob_er <- create_venn_grob(
  list(
    "Amf" = FOM_spp,
    "SMcf" = FOD_spp,
    "APAf" = FED_spp
  ),
  is_3way = TRUE
)

grob_inv_flor <- create_venn_grob(
  list(
    "Forest Inventory" = unique(c(inventory_spp1, inventory_spp2)),
    "Floristic Inventory" = unique(c(floristic_spp1, floristic_spp2))
  ),
  is_3way = FALSE
)

# Combine side-by-side with labels
combined <- grid.arrange(
  grob_er,
  grob_inv_flor,
  ncol = 2
)

# Save
ggsave("figures/combined_venn.svg", 
       combined, 
       width = 20, 
       height = 10, 
       units = "in")


# Species increment through Floristic Inventory ---------------------------

# Count the number of samples per year
count_floristic <- as.data.frame(table(herb_data$eventDate[herb_data$is_floristic %in% TRUE &
                                                             herb_data$taxon.rank %in% c("species",
                                                                                         "subspecies",
                                                                                         "variety") &
                                                             herb_data$stateProvince.new %in% "santa catarina"]))
count_other <- as.data.frame(table(herb_data$eventDate[herb_data$is_floristic %in% c(FALSE, NA) &
                                                         herb_data$taxon.rank %in% c("species",
                                                                                     "subspecies",
                                                                                     "variety") &
                                                         herb_data$stateProvince.new %in% "santa catarina"]))
count_all <- as.data.frame(table(herb_data$eventDate[herb_data$taxon.rank %in% c("species",
                                                                                 "subspecies",
                                                                                 "variety") &
                                                       herb_data$stateProvince.new %in% "santa catarina"]))

# Conversions
count_floristic$Var1 <- as.Date(count_floristic$Var1)
count_other$Var1 <- as.Date(count_other$Var1)
count_all$Var1 <- as.Date(count_all$Var1)



##### How did a Floristic Inventory increased richness in the FURB herbarium? ####

# Filter data for all records that are species/subsp./var. and contain a date
to_plot_all <- herb_data[herb_data$taxon.rank %in% c("species", "subspecies", "variety") &
                           herb_data$stateProvince.new %in% "santa catarina", 
                         c("eventDate", "scientificNameFull", "is_floristic")]
to_plot_all <- to_plot_all[!is.na(to_plot_all$eventDate), ]

# The exclusive contribution of FlorestaSC floristic inventory to the number of SC species
length(setdiff(unique(to_plot_all$scientificNameFull[to_plot_all$is_floristic %in% TRUE]),
               unique(to_plot_all$scientificNameFull[to_plot_all$is_floristic %in% c(FALSE, NA)])))
# 328 species, or 7% of the 4389 SC species in FURB


# Get unique species over the years for all records
unique_years_all <- unique(to_plot_all$eventDate)
cumulative_unique_counts_all <- numeric(length(unique_years_all))

# Loop through each year and calculate cumulative unique species for all records
for (i in seq_along(unique_years_all)) {
  current_year <- unique_years_all[i]
  species_up_to_now <- unique(to_plot_all$scientificNameFull[to_plot_all$eventDate <= current_year])
  cumulative_unique_counts_all[i] <- length(species_up_to_now)
}

# Create a result data frame for all records
result_all <- data.frame(
  year = unique_years_all,
  cumulative_unique_species = cumulative_unique_counts_all,
  type = "Floristic Inventory"  # Changed name here
)

# Filter data for non-floristic records
to_plot_non_floristic <- herb_data[herb_data$taxon.rank %in% c("species", "subspecies", "variety") & 
                                     herb_data$is_floristic %in% c(NA, FALSE) &
                                     herb_data$stateProvince.new %in% "santa catarina", 
                                   c("eventDate", "scientificNameFull")]
to_plot_non_floristic <- to_plot_non_floristic[!is.na(to_plot_non_floristic$eventDate), ]

# Make eventDate only a year
# to_plot_non_floristic$eventDate <- as.integer(substr(to_plot_non_floristic$eventDate, 1, 4))

# Get unique species over the years for non-floristic records
unique_years_non_floristic <- unique(to_plot_non_floristic$eventDate)
cumulative_unique_counts_non_floristic <- numeric(length(unique_years_non_floristic))

# Loop through each year and calculate cumulative unique species for non-floristic records
for (i in seq_along(unique_years_non_floristic)) {
  current_year <- unique_years_non_floristic[i]
  species_up_to_now <- unique(to_plot_non_floristic$scientificNameFull[to_plot_non_floristic$eventDate <= current_year])
  cumulative_unique_counts_non_floristic[i] <- length(species_up_to_now)
}

# Create a result data frame for non-floristic records
result_non_floristic <- data.frame(
  year = unique_years_non_floristic,
  cumulative_unique_species = cumulative_unique_counts_non_floristic,
  type = "Others"  # Changed name here
)

# Combine results into one data frame
final_result <- rbind(result_all, result_non_floristic)



# Plotting ----------------------------------------------------------------

##### Or double y-axis
# Create combined plot with two y-axes
combined_plot <- ggplot() +
  
  # First plot: Registers by date (black and blue points)
  geom_point(data = count_other, aes(x = as.Date(Var1), y = Freq, color = "Others"), 
             size = 1, shape = 19, alpha = 0.25) +
  geom_point(data = count_floristic, aes(x = as.Date(Var1), y = Freq, color = "Floristic Inventory"), 
             size = 1.2, shape = 19) +
  
  # Add the second plot's lines (cumulative richness)
  geom_line(data = final_result, aes(x = as.Date(year), 
                                     y = cumulative_unique_species / 10, 
                                     color = type)) + 
  # Dividing `cumulative_unique_species` by 10 to scale it for plotting with the first y-axis.
  
  # Vertical lines (important dates)
  geom_vline(xintercept = as.Date(c("2007-11-06", "2020-02-20")), 
             color = "darkred", lwd = 0.5, linetype = "dashed") +
  
  # Labels and Titles
  labs(x = "\nYear (1943–2024)", 
       y = "N of registers per year\n", 
       color = NULL, 
       title = "N of registers and cumulative species richness per year in FURB Herbarium") +
  
  # Second y-axis for cumulative richness, scaled
  scale_y_continuous(sec.axis = sec_axis(~.*10, name = "\nCumulative species richness")) +
  
  # Customize color for cumulative richness lines and points
  scale_color_manual(values = c("Floristic Inventory" = "#6BAED6", "Others" = 'black')) +
  
  # Themes for the plot
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top",
    legend.justification = "center",
    legend.text = element_text(size = 13),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    axis.title.y.right = element_text(size = 15, angle = 90), # Secondary axis
    axis.text.y.right = element_text(size = 13)
  )

combined_plot
ggplot2::ggsave(filename = "cumrich.svg",
                plot = combined_plot,
                path = here::here("figures"),
                height = 8,
                width = 10,
                device = "svg")



# How much time does it take for floristic inventory register to be identified?
herb_flor <- herb_data[herb_data$is_floristic %in% TRUE, ]
herb_flor1 <- herb_flor

# How many registers were not identified yet?
table(herb_flor$scientificNameStatus) # 2591 at genus- and 432 at family-level: 3,023


# Adjusting for some cases...
herb_flor1$dateIdentified <- ifelse(
  herb_flor1$scientificNameStatus %in% c("indet", "family_as_genus") &
    herb_flor1$taxon.rank %in% c("family", "genus", NA, ""),
  as.character(herb_flor1$eventDate),  # Converter para character temporariamente
  as.character(herb_flor1$dateIdentified)  # Manter como character
)

sum(is.na(herb_flor1$dateIdentified)) # 425 sem data de ID


# setdiff(unique(a$family.new1), 
#         unique(herb_flor1$family.new1[!(herb_flor1$catalogNumber %in% a$catalogNumber)]))

# Make new columns for yearIdentified and monthIdentified after the adjustment above
herb_flor1$yearIdentified_new <- sapply(strsplit(herb_flor1$dateIdentified, "-"),
                                        function(x) x[1])
herb_flor1$monthIdentified_new <- sapply(strsplit(herb_flor1$dateIdentified, "-"),
                                         function(x) x[2])

# Remove what is still NA after the new dateIdentified
herb_flor1 <- herb_flor1[!is.na(herb_flor1$monthIdentified_new) &
                           !is.na(herb_flor1$yearIdentified_new) &
                           !grepl("NA", herb_flor1$monthIdentified_new), ]
# From 27883 now we have 27,100 

# Make new columns but now only with months
herb_flor1$dateIdentified_new <- as.Date(paste(herb_flor1$yearIdentified_new,
                                               herb_flor1$monthIdentified_new,
                                               "01",
                                               sep = "-"))
herb_flor1$eventDate_new <- as.Date(paste(herb_flor1$year,
                                          herb_flor1$month,
                                          "01",
                                          sep = "-"))

# From 27883 now we have 27,100 minus the 10,003 registers below
# 10,003 registers identified at the same month at the colection
(table(herb_flor1$eventDate_new == herb_flor1$dateIdentified_new)[2])
# Removing these since they were identified at the same day (most likely)
herb_flor1 <- herb_flor1[!(herb_flor1$eventDate_new == herb_flor1$dateIdentified_new), ]
# We are left with 17,097 registers for time-to-id analyses

# Diff time between these posterior identifications
herb_flor1$diff <- as.integer(difftime(time1 = herb_flor1$dateIdentified_new,
                            time2 = herb_flor1$eventDate_new,
                            units = "days"))/365

# If negative difftime values, remove
herb_flor1 <- herb_flor1[herb_flor1$diff > 0, ]

mean(herb_flor1$diff) # 1.91 years
median(herb_flor1$diff) # 0.41 years

# How about the top 10 most abundant families?
fam_count <- sort(table(herb_flor1$family.new), decreasing = TRUE)
top5_fam <- names(fam_count)[1:10]

# Filter the original dataframe to include only the top 10 families
herb_flor2 <- herb_flor1[herb_flor1$family.new %in% top5_fam, ]

# Custom palette
my_palette <- c("#9EDAE5", "#FFBB78", "#C5B0D5", "#FF9898", "#C49C94",
                "#F7B6D2", "#C7C7C7", "#DBDB8D", "#9EDAE5", "#98DF8A")

# Plot the top 10
ggplot(herb_flor2, 
       aes(x = forcats::fct_rev(family.new), y = diff, fill = family.new)) +
  geom_boxplot() +
  coord_flip() + 
  theme_bw(base_size = 14) +
  labs(x = "\nTop 10 most abundant families", 
       y = "\nTime needed to species identification (years)", 
       fill = "Family") +
  scale_fill_manual(values = my_palette) +
  guides(fill = 'none')

ggsave(filename = "id_time.svg", device = "svg", path = here::here("figures"),
       width = 8, height = 6)

#Values
a = aggregate(diff ~ family.new,
              data = herb_flor2,
              FUN = function(x) c(mean = round(mean(x), 2), 
                                  sd = round(sd(x), 2),
                                  max = round(max(x), 2),
                                  n = length(x)))
print(a)

file_path2 <- here::here("data", "derived-data")
file_name5 <- "abund_fam_table.csv"
write.csv(a, file.path(file_path2, file_name5))


# How many species that were posteriorly identified configure new species to
# floristic inventory?
post_id_spp <- unique(herb_flor1$scientificNameFull[herb_flor1$taxon.rank %in% c("species",
                                                                             "subspecies",
                                                                             "variety")])

pre_id_spp <- unique(herb_flor$scientificNameFull[herb_flor$taxon.rank %in% c("species",
                                                                              "subspecies",
                                                                              "variety") &
                                                    !(herb_flor$catalogNumber %in% herb_flor1$catalogNumber)])
length(setdiff(post_id_spp, pre_id_spp)) # 1346 species/subspecies/varieties

# Genus? ------------------------------------------------------------------
post_id_gen <- unique(herb_flor1$genus)
pre_id_gen <- unique(herb_flor$genus[!(herb_flor$catalogNumber %in% herb_flor1$catalogNumber)])
length(setdiff(post_id_gen, pre_id_gen)) # 226 genus

# Families? ---------------------------------------------------------------
post_id_fam <- unique(herb_flor1$family.new)
pre_id_fam <- unique(herb_flor$family.new[!(herb_flor$catalogNumber %in% herb_flor1$catalogNumber)])
length(setdiff(post_id_fam, pre_id_fam)) # 21 families
rm(list=ls())

