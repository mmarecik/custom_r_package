encoding.DataFrame <-
function(df, col.names, type = "ordinal", drop = FALSE) {
  
  if (!is.list(col.names)) {
    stopifnot(is.character(col.names))
    col.names <- c(col.names)
  }
  
  # ordinal encoding
  if (type=="ordinal") {
    for(col in col.names) {
      df[, col] <- as.numeric(factor(df[[col]], levels = unique(df[[col]]), exclude = NULL))
    }
  }
  # one-hot encoding
  else if (type=="one-hot") {
    for(col in col.names) {
      
      if(!is.factor(df[[col]])) {
        df[, col] <- as.factor(df[, col])
      }
      # encoding
      encoded.df <- model.matrix(~0 + df[ , col])
      encoded.df.colnames <- sprintf(paste(col, '.%s', sep = "") , unique(df[ , col]))
      colnames(encoded.df) <- encoded.df.colnames
      
      col.pos <- match(col, colnames(df))
      new.colnames <- append(colnames(df), colnames(encoded.df), col.pos)
      new.colnames <- new.colnames[new.colnames != col]
      
      # concatenating data frames
      if (col.pos == ncol(df)){
        df <- cbind(df[,1:col.pos-1], encoded.df)
      } else {
        df <- cbind(df[,1:col.pos-1], cbind(encoded.df, df[,(col.pos+1):ncol(df)]))
      }
      colnames(df) <- new.colnames
      df <- data.frame(df)
      class(df) <- list('DataFrame', 'data.frame')
      
      # dropping one category
      stopifnot(drop %in% c(FALSE, TRUE, "last", "first", "binary"))
      if (drop == "last" || drop == TRUE ) {
        df <- df[ , !names(df) %in% c(colnames(encoded.df)[ncol(encoded.df)])]
      } else if (drop == "first") {
        df <- df[ , !names(df) %in% c(colnames(encoded.df)[1])]
      } else if (drop == "binary") {
        if (ncol(encoded.df) == 2) {
          df <- df[ , !names(df) %in% c(colnames(encoded.df)[2])]
        }
      }
      
    }
  }
  return(df)
}
