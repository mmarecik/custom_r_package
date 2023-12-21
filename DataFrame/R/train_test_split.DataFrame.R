train_test_split.DataFrame <-
function(df, test.size = 0.3, random.state = 47, stratify){
  
  set.seed(random.state)
  
  stopifnot(test.size >= 0 || test.size <= 1)
  train.size <- 1- test.size
  
  # stratified train test split
  if(!missing(stratify)) {
    # assert the column used for stratification is in dataframe
    stopifnot(stratify %in% colnames(df))
    # assert the column is categorical
    stopifnot(is.factor(df[[stratify]]))
    # assert the column takes more than 1 unique value
    stopifnot(length(unique(df[[stratify]])) > 1)
    
    if (!requireNamespace("caret", quietly = TRUE))
      install.packages("caret")
    
    indx <- caret::createDataPartition(df[, stratify], p = train.size, list = FALSE)
    
  } else { # not stratified train test split
    indx <- sample(1:nrow(df), size = (train.size)*nrow(df))
  }
  train_data <- df[indx, ]
  test_data <-  df[-indx, ]
  return(list(train = train_data, test = test_data))
}
