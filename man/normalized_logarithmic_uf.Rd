\name{normalized_logarithmic_uf}
\alias{normalized_logarithmic_uf}
\title{The normalized logarithmic utility function.}
\usage{
normalized_logarithmic_uf(par, oc)
}
\arguments{
  \item{par}{vector, parameters alpha, beta and lambda for
  the utility function.}

  \item{oc}{numeric, the objective consequence}
}
\description{
The normalized logarithmic utility function is given by

U(oc) = log(1-alpha*x), if oc >= 0 and

U(oc) = -lambda/beta * log(1-alpha*x), if oc < 0.

U is the utility and oc is the objective consequence of a
gamble outcome. lambda is the loss aversion coefficient.
The Tversky & Kahneman (1992) assumption has also been
made, namely

U(-oc) = -lambda * U(oc) where oc >= 0.
}
\references{
Scholten, M., & Read, D. (2014). Prospect theory and the
``forgotten" fourfold pattern of risk preferences. Journal
of Risk and Uncertainty, DOI 10.1007/s11166-014-9183-2.

Scholten, M., & Read, D. (2014). Prospect theory and the
``forgotten" fourfold pattern of risk preferences. Journal
of Risk and Uncertainty, DOI 10.1007/s11166-014-9183-2.
}

