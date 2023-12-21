HandleMissingValues.DataFrame <-
function(data, method = "drop", columns = NULL) {
  
  # Check the validity of arguments
  stopifnot(is.data.frame(data))
  stopifnot(is.character(method) && method %in% c("drop", "zero", "mean", "median", "mode", "interpolate"))
  stopifnot(is.null(columns) || (is.character(columns) && all(columns %in% names(data))))
  
  # If columns is specified, filter the columns to those in the list
  if (!is.null(columns)) {
    modifieddata <- data[, columns, drop = FALSE]
  } else {
    modifieddata <- data
  }
  
  if (method == "drop") {
    # Remove rows with missing values in selected columns
    data <- data[complete.cases(modifieddata), ]
  }
  
  else {
    # Apply the specified method to each selected column
    for (col in names(modifieddata)) {
      
      missingvalues <- sum(is.na(modifieddata[[col]]))
      
      if (missingvalues > 0) {
        if (method == "zero") {
          data[[col]][is.na(data[[col]])] <- 0
        } else if (method %in% c("mean", "median", "mode")) {
          imputevalue <- switch(method,
                                mean = mean(data[[col]], na.rm = TRUE),
                                median = median(data[[col]], na.rm = TRUE),
                                mode = {
                                  # Create a frequency table, find the value with the highest frequency
                                  tbl <- table(data[[col]], useNA = "ifany")
                                  mode_value <- as.numeric(names(tbl)[which.max(tbl)])
                                  # If NA is the most frequent value, use the second most frequent value
                                  if (is.na(mode_value)) {
                                    mode_value <- as.numeric(names(tbl)[which.max(tbl[-which.max(tbl)])])
                                  }
                                  mode_value
                                })
          data[[col]][is.na(data[[col]])] <- imputevalue
        } else if (method == "interpolate") {
          # Interpolate missing values in the column using linear interpolation
          interpolatedvalues <- approx(seq_along(data[[col]]), data[[col]], method = "linear", na.rm = TRUE)$y
          data[[col]] <- interpolatedvalues  # Replace the original column with the interpolated values
        }
      }
    }
  }
  return(data)
}
