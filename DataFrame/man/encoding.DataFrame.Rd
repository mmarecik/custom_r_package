\name{encoding.DataFrame}
\alias{encoding.DataFrame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
~~encoding.DataFrame: convert to perform columns encoding~~
}
\description{
Function implements two types of encoding: ordinal and one-hot encoding.Ordinal encoding transform categorical features as an integer list. This results in a single column of integers (0 to n_categories) per feature. One-hot encoding encodes target labels with value between 1 and n_classes. In addition, user can specify if one of the dummy columns should be dropped.
}
\usage{
encoding.DataFrame(df, col.names, type = "ordinal", drop = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
Data frame to perform an encoding on.
}
  \item{col.names}{
List of columns which are meant to be encoded.
}
  \item{type}{
Type of encoding. Available options are: "ordinal" and "one-hot".
}
  \item{drop}{
Specifies if one of summy columns should be dropped. If TRUE or "last", the last category column is removed. If "first", the first category column is removed. If "binary", last column is removed only if encoded column is binary. Default FALSE.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Input data frame after encoding of specified columns.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# create artificial data frame
df <- data.frame(x = 1:3, y = c("c", "a", "b"), z = c("a", "a", "b"))
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
