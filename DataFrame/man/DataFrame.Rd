\name{DataFrame}
\alias{DataFrame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creates DataFrame object from a given data frame.
}
\description{
Creates DataFrame object from a given data frame.
}
\usage{
DataFrame(df, row.names, col.names, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
Data frame to be converted to DataFrame.
}
  \item{row.names}{
List of new indeces. Optional.
}
  \item{col.names}{
List of new column names. Optional.
}
  \item{\dots}{
Anything.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
\dontrun{
# create artificial data frame
df <- data.frame(x = 1:3, y = 3:1, z = c("a", "a", "b"))
# change class of data frame to DataFrame
df <- DataFrame(df)
}
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
