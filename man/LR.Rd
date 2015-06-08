\name{LR}
\alias{LR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
LR(x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, y) 
{
    sum_x <- sum(x)
    sum_y <- sum(y)
    mult_xy <- sum(x * y)
    n <- length(x)
    b <- (n * mult_xy - (sum_x * sum_y))/(n * sum(x^2) - (sum_x)^2)
    a <- (sum_y - b * sum_x)/n
    y_e <- a + b * x
    sse <- sum((y - y_e)^2)
    sst <- sum((y - mean(y))^2)
    R_squared <- 1 - sse/sst
    pdf("./Desktop/mygraph.pdf")
    plot(x, y, col = "red", xlab = substitute(x), ylab = substitute(y))
    lines(x, y_e, type = "l", col = "blue")
    dev.off()
    sprintf("Coefficient of determination, R^2=\%f", R_squared)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
