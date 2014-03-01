\name{linear_in_log_odds_pwf}
\alias{linear_in_log_odds_pwf}
\title{The linear in log odds probability weighting function.}
\usage{
linear_in_log_odds_pwf(par, p)
}
\arguments{
  \item{par}{vector, contains the alpha and beta parameters
  for the pwf}

  \item{p}{numeric, the probability}
}
\description{
The linear in log odds probability weighting function is
given by

w(p) = beta * p^alpha / (beta * p^alpha + (1 - p)^alpha),

where p is the probability constrained by

w(0) = 0, w(1) = 1, 0 < p < 1.
}
\references{
p. 139, Gonzalez, R., & Wu, G. (1999). On the shape of the
probability weighting function. Cognitive Psychology, 38,
129-166.

p. 208, Wakker, P. P. (2010). Prospect theory: For risk and
ambiguity. Cambridge, UK: Cambridge University Press.
}
\seealso{
{\code{\link{plotProbW}, \link{plotTwoParProbWFam}}}
}

