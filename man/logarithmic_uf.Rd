\name{logarithmic_uf}
\alias{logarithmic_uf}
\title{The logarithmic utility function.}
\usage{
logarithmic_uf(par, oc)
}
\arguments{
  \item{par}{vector, parameters alpha and lambda for the
  utility function.}

  \item{oc}{numeric, the objective consequence}
}
\description{
The logarithmic utility function is given by

U(oc) = log(alpha + oc), if oc >= 0 and

U(oc) = -lambda * (log(beta + (-oc)), if oc < 0.

U is the utility and oc is the objective consequence of a
gamble outcome. lambda is the loss aversion coefficient.
The Tversky & Kahneman (1992) assumption has also been
made, namely

U(-oc) = -lambda * U(oc) where oc >= 0.
}

