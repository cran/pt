\name{quadratic_uf}
\alias{quadratic_uf}
\title{The quadratic utility function.}
\usage{
quadratic_uf(par, oc)
}
\arguments{
  \item{par}{vector, parameters alpha, beta and lambda for
  the utility function.}

  \item{oc}{numeric, the objective consequence}
}
\description{
The quadratic utility function is given by

U(oc) = alpha * oc - oc^2, if oc >= 0 and

U(oc) = -lambda * (beta * (-oc) - (-oc)^2), if oc < 0.

U is the utility and oc is the objective consequence of a
gamble outcome. lambda is the loss aversion coefficient.
The Tversky & Kahneman (1992) assumption has also been
made, namely

U(-oc) = -lambda * U(oc) where oc >= 0.
}

