DataFrame <-
function(df, row.names, col.names, ...) {
  
  stopifnot(is.data.frame(df))
  # rename index
  if(!missing(row.names)) {
    rownames(df) <- row.names
  }
  # rename columns
  if(!missing(col.names)) {
    colnames(df) <- col.names
  }
  
  class(df) <- list('DataFrame', 'data.frame')
  return(df)
}
