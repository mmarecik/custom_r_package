astype.DataFrame <-
function(df, numeric.cols, factor.cols) {
  
  if(!missing(numeric.cols)) {
    # convert columns to numeric
    for(ncol in numeric.cols) {
      df[, ncol] <- as.numeric(df[, ncol])
    }
  }
  # convert columns to factors
  if(!missing(factor.cols)) {
    for(fcol in factor.cols) {
      df[, fcol] <- as.factor(df[, fcol])
    }
  }
  return(df)
}
