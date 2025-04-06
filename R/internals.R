#' @title Remove punctuation, special characters, numbers and spaces
#'
#' @description Simple function to prepare vectors of taxon names for
#'   increasing the chances of exact and fuzzy name matching
#'
#' @param x a vector of characters (i.e. names)
#'
#' @return the vector \code{x} without punctuation, special
#'   characters, numbers and spaces
#'
#' @author Renato A. F. de Lima
#'
#' @examples
#'
#' \dontrun{
#' taxa <- c("Genus \u00d7 epiteth (Jos\u00e9 AB.) C.")
#' print(taxa)
#' cleanName(taxa)
#' }
#'
#' @keywords internal
#'
#' @importFrom stringr str_to_lower
#'
.cleanName <- function(x) {
  
  x1 <- gsub(" ", "", x, fixed =  TRUE)
  x1 <- stringr::str_to_lower(x1)
  x1 <- gsub("[[:punct:]]", "", x1, perl =  TRUE)
  x1 <- rmLatin(x1)
  x1 <- gsub("\u00d7", "", x1, fixed = TRUE)
  x1 <- gsub("[0-9]", "", x1, perl = TRUE)
  
  return(x1)
}

#' A function to summarize all information from the FlorestaSC project
#' This function is pretty much hardcoded for our intent, but can be used for any
#' other data set that includes the same structure. The idea is simply to summarize
#' what is unique based on multiple column classes.
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
