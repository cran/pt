\name{neo_additive_pwf}
\alias{neo_additive_pwf}
\title{The neo-additive probability weighting function.}
\usage{
neo_additive_pwf(par, p)
}
\arguments{
  \item{par}{vector, contains the alpha and beta parameters
  for the pwf}

  \item{p}{numeric, the probability}
}
\description{
The neo-additive probability weighting function is given by

w(p) = beta + alpha * p,

where p is the probability constrained by

w(0) = 0, w(1) = 1, 0 < p < 1,

and the two parameters in the function alpha and beta are
constrained by

alpha >= 0,

beta >= 0, and

alpha + beta <= 1.
}
\references{
Eqn. 7.2.5, p. 208-209 Wakker, P. P. (2010). Prospect
theory: For risk and ambiguity. Cambridge, UK: Cambridge
University Press.
}
\seealso{
{\code{\link{plotProbW}, \link{plotTwoParProbWFam}}}
}

