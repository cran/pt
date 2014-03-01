\name{power_pwf}
\alias{power_pwf}
\title{The power probability weighting function.}
\usage{
power_pwf(par, p)
}
\arguments{
  \item{par}{vector, contains the alpha and beta parameters
  for the pwf}

  \item{p}{numeric, the probability}
}
\description{
The power probability weighting function is given by

w(p) = beta * p^alpha,

where p is the probability constrained by

w(0) = 0, w(1) = 1, 0 < p < 1.
}
\references{
Stott, H. P. (2006). Cumulative prospect theory's
functional menagerie. Journal of Risk and Uncertainty,
32(2), 101-130.
}
\seealso{
{\code{\link{plotProbW}, \link{plotTwoParProbWFam}}}
}

