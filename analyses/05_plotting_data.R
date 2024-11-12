

# Plotting data -----------------------------------------------------------


# Read necessary data
file_path <- here::here("data", "derived-data")
file_name1 <- "inv_spp1.rds"
file_name2 <- "inv_spp2.rds" 
file_name3 <- "flor_spp1.rds" 
file_name4 <- "flor_spp2.rds" 
file_name5 <- "herb_data_lifeforms.rds"

inventory_spp1 <- readRDS(file.path(file_path, file_name1))
inventory_spp2 <- readRDS(file.path(file_path, file_name2))
floristic_spp1 <- readRDS(file.path(file_path, file_name3))
floristic_spp2 <- readRDS(file.path(file_path, file_name4))
herb_data <- readRDS(file.path(file_path, file_name5))

# Pie chart for life forms ------------------------------------------------
to_pie <- data.frame(table(herb_data$lifeForm))
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



# Venn Diagram ------------------------------------------------------------
VennDiagram::venn.diagram(x = list(unique(c(inventory_spp1, inventory_spp2)),
                                   unique(c(floristic_spp1, floristic_spp2))),
                          disable.logging = TRUE,
                          category.names = c(
                            paste0("Forest Inventory", " (", length(unique(c(inventory_spp1, inventory_spp2))), ")"),
                            paste0("Floristic Inventory", " (", length(unique(c(floristic_spp1, floristic_spp2))), ")")),
                          filename = "figures/venn.svg",
                          print.mode = c("raw", "percent"),
                          imagetype = "svg",
                          na = "remove",
                          height = 10,
                          width = 10,
                          units = "in",
                          cex = 2,
                          cat.cex = 2.5,
                          lwd = 1.5,
                          alpha = 0.75,
                          margin = 0.075,
                          cat.pos = c(180, 180),
                          # cat.dist = c(0.075, 0.075, 0.025),
                          col = "black",
                          fill = c("#BDD7E7", "#6BAED6"))



# ##### For Euler Venn Diagram, if wanted #####
# # Define the sets and their sizes
# sets <- list("1st Forest Inventory" = inventory_spp1,
#              "2nd Forest Inventory" = inventory_spp2,
#              "1st Floristic Inventory" = floristic_spp1,
#              "2nd Floristic Inventory" = floristic_spp2)
# 
# # Create a proportional Euler diagram
# fit <- eulerr::euler(sets)
# 
# # Plot the Euler diagram
# plot(fit,
#     quantities = TRUE,  # Show the numbers inside the circles
#     fills = c("#BDD7E7", "#6BAED6", "#3182BD", "#08519C"),  # Set colors
#     alpha = 0.5,
#     #labels = c(),
#     #djust_labels = list(c(0.2, 0.1), c(0.1, 0.2), c(-0.1, -0.2)),
#     main = "Proportional Euler Diagram")



# Species increment through Floristic Inventory ---------------------------

# Count the number of samples per year
count_floristic <- as.data.frame(table(herb_data$eventDate[herb_data$is_floristic %in% TRUE &
                                                             herb_data$taxon.rank %in% c("species",
                                                                                         "subspecies",
                                                                                         "variety")]))
count_other <- as.data.frame(table(herb_data$eventDate[herb_data$is_floristic %in% c(FALSE, NA) &
                                                         herb_data$taxon.rank %in% c("species",
                                                                                     "subspecies",
                                                                                     "variety")]))
count_all <- as.data.frame(table(herb_data$eventDate[herb_data$taxon.rank %in% c("species",
                                                                                 "subspecies",
                                                                                 "variety")]))

# Conversions
count_floristic$Var1 <- as.Date(count_floristic$Var1)
count_other$Var1 <- as.Date(count_other$Var1)
count_all$Var1 <- as.Date(count_all$Var1)



##### How did a Floristic Inventory increased richness in the FURB herbarium? ####

# Filter data for all records
to_plot_all <- herb_data[herb_data$taxon.rank %in% c("species", "subspecies", "variety"), 
                         c("eventDate", "scientificNameFull", "is_floristic")]
to_plot_all <- to_plot_all[!is.na(to_plot_all$eventDate), ]

# Make eventDate only a year
# to_plot_all$eventDate <- as.integer(substr(to_plot_all$eventDate, 1, 4))

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
                                     herb_data$is_floristic %in% c(NA, FALSE), 
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

# # Registers by date
# count_plot <- ggplot() +
#   geom_point(data = count_other, aes(x = Var1, y = Freq), 
#              color = rgb(0, 0, 0, 0.25), 
#              size = 1, shape = 19) +
#   geom_point(data = count_floristic, aes(x = Var1, y = Freq), 
#              color = "#6BAED6", 
#              size = 1.2, shape = 19) +
#   geom_vline(xintercept = as.Date(c("2007-11-06", "2020-02-20")), 
#              color = "darkred", lwd = 0.5, linetype = "dashed") +
#   labs(x = "\nDate", y = "Registers by date\n") +
#   theme_minimal() +
#   theme(panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         axis.title.x = element_text(size = 15),
#         axis.title.y = element_text(size = 15),
#         axis.text.x = element_text(size = 13),
#         axis.text.y = element_text(size = 13, margin = margin(l = 6.75))) 
# 
# 
# 
# # Cumulative richess by date
# cum_rich_plot <- ggplot(final_result, aes(x = year, y = cumulative_unique_species, color = type)) +
#   geom_line() +         # Add lines connecting the points
#   #geom_point() +        # Add points to the plot
#   geom_vline(xintercept = as.Date(c("2007-11-06", "2020-02-20")), 
#              color = "darkred", lwd = 0.5, linetype = "dashed") +
#   # geom_hline(yintercept = c(4373, 4059), color = "darkred",
#   # lwd = 0.5, linetype = "dashed") +
#   labs(title = "Cumulative richness throughout the years in FURB",
#        x = NULL,
#        y = "Cumulative richness\n") +
#   scale_color_manual(values = c("Floristic Inventory" = "#6BAED6", "Others" = 'black')) + # Customize colors
#   theme_minimal() +     # Optional: use a minimal theme
#   theme(legend.title = element_blank(),  # Remove legend title
#         legend.position = "top",         # Position legend at the top
#         legend.justification = "center",
#         legend.text = element_text(size = 13),
#         axis.title.x = element_blank(),
#         axis.text.x = element_blank(),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         axis.title.y = element_text(size = 15),
#         axis.text.y = element_text(size = 13,)) # Center the legend
# 
# # Combine plots and assign to a variable
# combined_plot <- gridExtra::grid.arrange(cum_rich_plot, 
#                                          count_plot,
#                                          ncol = 1)
# 
# ggplot2::ggsave(filename = "cumrich.svg",
#                 plot = combined_plot,
#                 path = here::here("figures"),
#                 height = 8,
#                 width = 10,
#                 device = "svg")


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
  labs(x = "\nDate", y = "Number of registers\n", 
       color = NULL, title = "Number of registers and cumulative richness per date") +
  
  # Second y-axis for cumulative richness, scaled
  scale_y_continuous(sec.axis = sec_axis(~.*10, name = "\nCumulative richness")) +
  
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
ggplot2::ggsave(filename = "cumrich1.svg",
                plot = combined_plot,
                path = here::here("figures"),
                height = 8,
                width = 10,
                device = "svg")



# Time to identification --------------------------------------------------

#Datas de coleta, lag até identificação
date_data <- herb_data
date_data[date_data == ""] <- NA
# How many were possible to use?
# table(herb_data$is_floristic)

#Keeping only registers with year and month of both sample and ID
date_data <- date_data[!is.na(date_data$yearIdentified) & 
                         !is.na(date_data$year) &
                         !is.na(date_data$month) &
                         !is.na(date_data$monthIdentified), ]


#  
# We are rounding up or down the times for simplification


# Create year-month columns for both collect and ID
date_data$collect_date <- as.Date(paste(date_data$year, date_data$month, "01", sep = "-"))
date_data$id_date <- as.Date(paste(date_data$yearIdentified, date_data$monthIdentified, "01", sep = "-"))

# Filtering only the FlorestaSC interval (round up/down for simplification)
# "2007-11-06", "2020-02-20" becomes "2007-11-01", "2020-03-01"
start_date <- as.Date("2007-11-01")
end_date <- as.Date("2020-03-01")

# Filter date_data where both collect_date and id_date are within the interval
date_data <- date_data[
  date_data$collect_date >= start_date & date_data$collect_date <= end_date & 
    date_data$id_date >= start_date & date_data$id_date <= end_date, ]

# Now we measure the differences in time to id
# In months
date_data$diff_sample_to_id_months <- as.numeric(difftime(date_data$id_date, 
                                                          date_data$collect_date, 
                                                          units = "days")) / 30
# In years
date_data$diff_sample_to_id_years <- (date_data$yearIdentified - as.numeric(date_data$year)) + 
  (date_data$monthIdentified - as.numeric(date_data$month)) / 12

# Some wrong dates where the ID was performed before the sample need to be removed
date_data$wtf <- date_data$diff_sample_to_id_months < 0
date_data <- date_data[date_data$wtf %in% FALSE, ]

# Average time to identification
mean(date_data$diff_sample_to_id_years) # 0.87 years
# Median time to identification
median(date_data$diff_sample_to_id_years) # 0.08 years

# Quantos indivíduos não foram identificados até hoje?
# sum(is.na(date_data$yearIdentified)) # 0 if we are considering floristic inventory

# #Quem são eles?
# herb_ni <- herb[is.na(herb$detyy), ]
# fam_count_ni <- sort(table(herb_ni$family), decreasing = TRUE)
# top5_fam_ni <- names(fam_count_ni)[1:5]
# 
# #Top 5 famílias não identificadas
# herb_ni_top5 <- herb_ni[herb_ni$family %in% top5_fam_ni, ]
# herb_ni_top5$today <- 2024
# herb_ni_top5$diff <- herb_ni_top5$today - herb_ni_top5$collyy
# 
# ggplot(herb_ni_top5, 
#        aes(x = family, y = diff, fill = family)) +
#   geom_boxplot() +
#   coord_flip() + 
#   theme_bw(base_size = 14) +
#   labs(x = "\nMost abundant NI families", 
#        y = "\nElapsed time since plant collection (years)", 
#        fill = "Family") +
#   #scale_y_reverse() +
#   scale_fill_manual(values = wesanderson::wes_palette(n = 5, name = "Moonrise3"))
# 
# ggsave("Figure2.png", device = "png", path = file_path,
#        width = 8, height = 6)
# 
# #Values
# herb_ni_top5 %>%
#   group_by(family) %>%
#   summarise(mean = mean(diff),
#             sd = sd(diff),
#             n = n())

# #Considerando famílias mais abundantes
# #Obter valores
# fam_count <- sort(table(date_data$family.new1), decreasing = TRUE)
# 
# #Selecionar top 5 famílias mais abundantes nos dados de herbário
# top_fam <- names(fam_count)[1:10]
# 
# # Filter the original dataframe to include only the top 5 species
# date_data2 <- date_data[date_data$family.new1 %in% top_fam, ]

ggplot(date_data,
       aes(x = is_floristic, y = diff_sample_to_id_months, fill = is_floristic)) +
  geom_boxplot() +
  theme_bw(base_size = 14) +
  labs(x = "Is floristic?", 
       y = "\nTime needed to identify (months)", 
       fill = "Is floristic?")

ggsave("Figure1.svg", device = "svg", path = file_path,
       width = 8, height = 6)

#Values
date_data2 %>%
  group_by(family) %>%
  summarise(mean = mean(diff),
            sd = sd(diff),
            n = n())