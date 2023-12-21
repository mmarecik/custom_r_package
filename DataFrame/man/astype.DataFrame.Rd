\name{astype.DataFrame}
\alias{astype.DataFrame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
~~astype.DataFrame: convert columns data type~~
}
\description{
~~ Convenient wat to convert columns specified in a list to numeric or factor type. Function accepts two optional arguments, each argument must be a list. Lists specify columns which should be converted into numeric or factor dtype. If argument is missing, nothing changes. Recommended usage of keyword arguments as conversion is done in an order.  ~~
}
\usage{
astype.DataFrame(df, numeric.cols, factor.cols)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
Data frame that columns are to be converted.
}
  \item{numeric.cols}{
List of columns meant to be converted to numeric type.
}
  \item{factor.cols}{
List of columns meant to be converted to factor type.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Input dta frame after conversion of column types.
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
df <- data.frame(x = 1:3, y = 3:1, z = c("a", "a", "b"))
# change class of data frame to DataFrame
df <- DataFrame(df)
# dtypes conversion
numcols <- c("x", "y")
fctcols <- c("z")
astype.DataFrame(df, numeric.cols = numcols, factor.cols = fctcols)
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
