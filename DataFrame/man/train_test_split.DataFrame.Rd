\name{train_test_split.DataFrame}
\alias{train_test_split.DataFrame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
~~train_test_split.DataFrame: performs train-test split ~~
}
\description{
Split data frame into random train and test subsets. If stratify argument is specified, function performs a stratified split.
}
\usage{
train_test_split.DataFrame(df, test.size = 0.3, random.state = 47, stratify)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
Data frame meant to be splitted into train and test set.
}
  \item{test.size}{
Share of the dataset used for testing. Must be a value between 0 and 1.
}
  \item{random.state}{
Integer number that controls randomization during splitting.
}
  \item{stratify}{
Column name which determines how to use a stratified split. Optional.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
List containing train-test split of inputs.
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
df <- data.frame(x = 1:5, y = c("c", "a", "b", "a", "c"), z = c("a", "a", "b", "a", "b"))
# change class of data frame to DataFrame
df <- DataFrame(df)

# dtypes conversion
numcols <- c("x")
fctcols <- c("y", "z")
df <- astype.DataFrame(df, numeric.cols = numcols, factor.cols = fctcols)
str(df)

train_test <- train_test_split.DataFrame(df, test.size = 0.3, random.state = 34, stratify="z")
train_test$train
train_test$test

train_test <- train_test_split.DataFrame(df, test.size = 0.3, random.state = 34)
train_test$train
train_test$test
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
