summary.DataFrame <-
function(df) {
  
  numeric_stats <- list()
  categorical_stats <- list()
  
  for (col in names(df)) {
    # Check if the column is numerical
    if (is.numeric(df[[col]])) {
      summary_stats <- c(
        Mean = mean(df[[col]], na.rm = TRUE),
        Median = median(df[[col]], na.rm = TRUE),
        SD = sd(df[[col]], na.rm = TRUE),
        Variance = var(df[[col]], na.rm = TRUE),
        Min = min(df[[col]], na.rm = TRUE),
        Max = max(df[[col]], na.rm = TRUE),
        Range = diff(range(df[[col]], na.rm = TRUE)),
        Quantile_25 = quantile(df[[col]], probs = 0.25, na.rm = TRUE),
        Quantile_75 = quantile(df[[col]], probs = 0.75, na.rm = TRUE),
        Missing = as.integer(sum(is.na(df[[col]])))
      )
      numeric_stats[[col]] <- summary_stats
    } else {
      # For categorical data, get frequency table
      categorical_stats[[col]] <- table(df[[col]])
    }
  }
  
  # Formatting the results for numeric data
  if (length(numeric_stats) > 0) {
    #numeric_stats_df <- data.frame(t(sapply(numeric_stats, unlist)))
    numeric_stats_df <- data.frame(do.call(rbind, numeric_stats))
    colnames(numeric_stats_df) <- names(summary_stats)
    #numeric_stats <- data.frame(numeric_stats)
    
    numeric_stats_df[,-ncol(numeric_stats_df)] <- round(numeric_stats_df[,-ncol(numeric_stats_df) ], 2)
  } else {
    numeric_stats_df <- NULL
  }
  
  
  list(
    Numeric = numeric_stats_df,
    Categorical = categorical_stats
  )
}
