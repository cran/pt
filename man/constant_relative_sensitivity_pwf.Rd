\name{constant_relative_sensitivity_pwf}
\alias{constant_relative_sensitivity_pwf}
\title{The constant relative sensitivity probability weighting function.}
\usage{
constant_relative_sensitivity_pwf(par, p)
}
\arguments{
  \item{par}{vector, contains the alpha and beta parameters
  for the probability weighting function.}

  \item{p}{numeric, the probability}
}
\description{
Constant relative sensitivity probability weighting
function is given by

w(p) = beta^(1-alpha)*p^alpha,

where p is the probability constrained by

w(0) = 0, w(1) = 1, 0 < p < 1,

and the two parameters alpha and beta are constrained by

alpha > 0, 0 <= beta <= 1.
}
\references{
p. 52, Abdellaoui, M., L'Haridon, O., & Zank, H. (2010).
Separating curvature and elevation: A parametric
probability weighting function. Journal of Risk and
Uncertainty, 41(1), 39-65.
}
\seealso{
{\code{\link{plotProbW}, \link{plotTwoParProbWFam}}}
}

