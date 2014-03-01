\name{compound_invariance_pwf}
\alias{compound_invariance_pwf}
\title{The compound invariance probability weighting function.}
\usage{
compound_invariance_pwf(par, p)
}
\arguments{
  \item{par}{vector, contains the alpha and beta parameters
  for the pwf}

  \item{p}{numeric, the probability}
}
\description{
The compound invariance probability weighting function is
given by

w(p) = (exp(-beta * (-log(x))^alpha)),

where p is the probability constrained by

w(0) = 0, w(1) = 1, 0 < p < 1,

and the two parameters alpha and beta are constrained by

alpha > 0, beta > 0.
}
\references{
Prelec, D. (1998). The probability weighting function.
Econometrica, 60(3), 497-528.

al-Nowaihi, A., & Dhami, S. (2006). A simple derivation of
Prelec's probability weighting function. Journal of
Mathematical Psychology, 50(6), 521-524.

p. 179, 207, Wakker, P. P. (2010). Prospect theory: For
risk and ambiguity. Cambridge, UK: Cambridge University
Press.
}
\seealso{
{\code{\link{plotProbW}, \link{plotTwoParProbWFam}}}
}

