\name{general_power_uf}
\alias{general_power_uf}
\title{The general power utility function.}
\usage{
general_power_uf(par, oc)
}
\arguments{
  \item{par}{vector, parameters alpha, beta and lambda for
  the utility function.}

  \item{oc}{numeric, the objective consequence}
}
\description{
The general power utility function is given by

U(oc) = beta * oc^alpha, if oc >= 0 and

U(oc) = -lambda * (delta * -oc)^gamma, if oc < 0.

U is the utility and oc is the objective consequence of a
gamble outcome. lambda is the loss aversion coefficient.
The Tversky & Kahneman (1992) assumption has also been
made, namely

U(-oc) = -lambda * U(oc) where oc >= 0.
}
\references{
p. 309 Tversky, A., & Kahneman, D. (1992). Advances in
prospect theory: Cumulative representation of uncertainty.
Journal of Risk and Uncertainty, 5(4), 297-323.

p. 466 Eqn. 2, 469, Birnbaum, M. H. (2008). New paradoxes
of risky decision making. Psychological Review, 115(2),
463-501.
}

