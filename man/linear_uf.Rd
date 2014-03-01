\name{linear_uf}
\alias{linear_uf}
\title{The linear utility function.}
\usage{
linear_uf(par, oc)
}
\arguments{
  \item{par}{vector, parameter lambda for the utility
  function.}

  \item{oc}{numeric, the objective consequence.}
}
\description{
The linear utility function is given by

U(oc) = oc, if oc >= 0 and

U(oc) = -lambda * (-oc), if oc < 0.

U is the utility and oc is the objective consequence of a
gamble outcome. lambda is the loss aversion coefficient.
The Tversky & Kahneman (1992) assumption has also been
made, namely

U(-oc) = -lambda * U(oc) where oc >= 0.
}
\references{
Tversky, A., & Kahneman, D. (1992). Advances in prospect
theory: Cumulative representation of uncertainty. Journal
of Risk and Uncertainty, 5(4), 297-323.

p. 466 Eqn. 2, 469, Birnbaum, M. H. (2008). New paradoxes
of risky decision making. Psychological Review, 115(2),
463-501.
}

