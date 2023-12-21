\name{DataFrame-package}
\alias{DataFrame-package}
\alias{DataFrame}
\docType{package}
\title{
\packageTitle{DataFrame}
}
\description{
\packageDescription{DataFrame}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{DataFrame}
\packageIndices{DataFrame}
~~ An overview of how to use the package, including the most important ~~
~~ functions ~~
}
\author{
\packageAuthor{DataFrame}

Maintainer: \packageMaintainer{DataFrame}
}
\references{
~~ Literature or other references for background information ~~
}
~~ Optionally other standard keywords, one per line, from file KEYWORDS in ~~
~~ the R documentation directory ~~
\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
# create artificial data frame
df <- data.frame(x = 1:5, y = c("c", "a", "b", "a", "c"), z = c("a", "a", "b", "a", "b"))
# change class of data frame to DataFrame
df <- DataFrame(df)

# dtypes conversion
numcols <- c("x")
fctcols <- c("y", "z")
df <- astype.DataFrame(df, numeric.cols = numcols, factor.cols = fctcols)
str(df)

#encoding
encoding.DataFrame(df, fctcols, type = "one-hot", drop = "binary")
encoding.DataFrame(df, fctcols, type = "ordinal", drop = FALSE)

train_test <- train_test_split.DataFrame(df, test.size = 0.3, random.state = 34, stratify="z")
train_test$train
train_test$test

train_test <- train_test_split.DataFrame(df, test.size = 0.3, random.state = 34)
train_test$train
train_test$test
}
