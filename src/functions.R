truncate_years <- function(years_string) {
  # Convert the string to a numeric vector
  years <- as.numeric(unlist(strsplit(years_string, ",\\s*")))
  
  # Remove NA values and sort the years
  years <- sort(na.omit(years))
  
  if (length(years) == 0) {
    return("")
  }
  
  if (length(years) == 1) {
    return(as.character(years))
  }
  
  # Initialize an empty list to store the result
  result <- c()
  
  # Initialize start and end of the current range
  start <- years[1]
  end <- years[1]
  
  for (i in 2:length(years)) {
    if (years[i] == end + 1) {
      # Continue the range
      end <- years[i]
    } else {
      # End of current range
      if (end - start > 1) {
        result <- c(result, paste(start, end, sep = "-"))
      } else {
        result <- c(result, as.character(start), if (start != end) as.character(end))
      }
      # Start new range
      start <- years[i]
      end <- years[i]
    }
  }
  
  # Add the last range
  if (end - start > 1) {
    result <- c(result, paste(start, end, sep = "-"))
  } else {
    result <- c(result, as.character(start), if (start != end) as.character(end))
  }
  
  # Return the result as a single comma-separated string
  return(paste(result, collapse = ", "))
}

count_years <- function(years_string) {
  # Split the string by comma and optional spaces
  years_string <- gsub(", ", "; ", years_string)
  years <- strsplit(years_string, ";\\s*")
  
  # Count the number of elements in the resulting vector
  num_years <- unlist(map(years, length))
  
  return(num_years)
}
