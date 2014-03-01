\name{kt_pwf}
\alias{kt_pwf}
\title{The Tversky and Kahneman (1992) probability weighting function.}
\usage{
kt_pwf(par, p)
}
\arguments{
  \item{par}{vector, contains the alpha parameter for the
  pwf}

  \item{p}{numeric, the probability}
}
\description{
Tversky and Kahneman's (1992) probability weighting
function is given by

w(p) = p^alpha / ((p^alpha + (1 - p)^alpha)^(1/alpha))

where p is the probability constrained by

w(0) = 0, w(1) = 1, 0 < p < 1,

and alpha is the single parameter for the function. alpha
>= 0.28 as the function is not strictly increasing for
alpha < 0.28.
}
\references{
Tversky, A., & Kahneman, D. (1992). Advances in prospect
theory: Cumulative representation of uncertainty. Journal
of Risk and Uncertainty, 5(4), 297-323.

p. 206, Wakker, P. P. (2010). Prospect theory: For risk and
ambiguity. Cambridge, UK: Cambridge University Press.
}
\seealso{
{\code{\link{plotProbW}, \link{plotOneParProbWFam}}}
}

