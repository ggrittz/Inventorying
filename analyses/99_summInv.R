# A function to summarize all information from the FlorestaSC project
# This function is pretty much hardcoded for our intent, but can be used for any
# other data set that includes the same structure. The idea is simply to summarize
# what is unique based on multiple column classes.
.summInv <- function(data, taxon, source, veg.type, filter) {

  results <- list()
  
  for (src in source) {
    for (ft in veg.type) {
      filtered_data <- data[data$source == src & data$FT == ft, ]
      unique_values <- unique(filtered_data[[taxon]][!filter(filtered_data[[taxon]])])
      unique_values <- unique_values[!is.na(unique_values)]
      results[[paste0(taxon, "_", src, "_", ft)]] <- unique_values
    }
  }
  
  return(results)
}