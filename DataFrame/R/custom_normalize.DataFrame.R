custom_normalize.DataFrame <-
function(df, method = "standardize") {
  # Check if the input is a data frame
  stopifnot(is.data.frame(df))
  
  # Separate numeric and non-numeric columns
  numeric_columns <- sapply(df, is.numeric)
  df_numeric <- df[, numeric_columns]
  df_non_numeric <- df[, !numeric_columns]
  
  # Apply standardization/normalization method to numeric columns
  if (method == "standardize") {
    # Standardization (centering and scaling)
    df_numeric <- scale(df_numeric)
  }
  else if (method == "min_max") {
    # Min-Max Normalization
    df_numeric <- apply(df_numeric, 2, function(x) (x - min(x)) / (max(x) - min(x)))
  }
  else if (method == "log_transform") {
    # Log transformation
    df_numeric <- log1p(df_numeric)
  }
  else if (method == "robust") {
    # Robust standardization
    df_numeric <- apply(df_numeric, 2, function(x) (x - median(x)) / IQR(x))
  }
  else if (method == "z_score") {
    # Z-Score normalization
    df_numeric <- apply(df_numeric, 2, function(x) (x - mean(x)) / sd(x))
  }
  
  
  # Combine numeric and non-numeric columns back into a single data frame
  df <- cbind(df_non_numeric, df_numeric)
  
  # Return the processed table
  return(df)
}
