\name{hyperbolic_logarithm_pwf}
\alias{hyperbolic_logarithm_pwf}
\title{The hyperbolic-logarithm probability weighting function.}
\usage{
hyperbolic_logarithm_pwf(par, p)
}
\arguments{
  \item{par}{vector, contains the alpha and beta parameters
  for the pwf}

  \item{p}{numeric, the probability}
}
\description{
The hyperbolic-logarithm probability weighting function is
given by

w(p) = (1 - alpha * log(p))^(-beta/alpha)

where p is the probability constrained by

w(0) = 0, w(1) = 1, 0 < p < 1,

and the two parameters alpha and beta are constrained by

alpha > 0, beta > 0.
}
\references{
Prelec, D. (1998). The probability weighting function.
Econometrica, 60(3), 497-528.

p. 176, Luce, R. D. (2001). Reduction invariance and
Prelec's weighting functions. Journal of Mathematical
Psychology, 45(1), 167-179.

Footnote 3, p. 105, Stott, H. P. (2006). Cumulative
prospect theory's functional menagerie. Journal of Risk and
Uncertainty, 32(2), 101-130.
}
\seealso{
{\code{\link{plotProbW}, \link{plotTwoParProbWFam}}}
}

