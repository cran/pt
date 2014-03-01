\name{exponential_uf}
\alias{exponential_uf}
\title{The exponential utility function.}
\usage{
exponential_uf(par, oc)
}
\arguments{
  \item{par}{vector, parameter alpha for the utility
  function.}

  \item{oc}{numeric, the objective consequence}
}
\description{
Exponential utility function is given by

U(oc) = 1 - exp(-alpha * oc), if alpha > 0,

U(oc) = oc, if alpha == 0, and

U(oc) = exp(-alpha * oc) - 1, if alpha < 0.

U is the utility and oc is the objective consequence of a
gamble outcome. lambda is the loss aversion coefficient.
alpha is an index of concavity. This function is defined on
the entire real line (see Wakker, p. 80).
}
\references{
p. 309 Tversky, A., & Kahneman, D. (1992). Advances in
prospect theory: Cumulative representation of uncertainty.
Journal of Risk and Uncertainty, 5(4), 297-323.

p. 466 Eqn. 2, 469, Birnbaum, M. H. (2008). New paradoxes
of risky decision making. Psychological Review, 115(2),
463-501.

p. 80 Wakker, P. P. (2008). Explaining the characteristics
of the power (CRRA) utility family. Health Economics,
17(12), 1329-1344.
}

