\name{expo_power_uf}
\alias{expo_power_uf}
\title{The expo-power utility function.}
\usage{
expo_power_uf(par, oc)
}
\arguments{
  \item{par}{vector, parameters alpha, beta, gamma and
  lambda for the utility function.}

  \item{oc}{numeric, the objective consequence}
}
\description{
The expo-power utility function is given by

U(oc) = gamma - exp(-beta * oc^alpha), if oc >= 0 and

U(oc) = -lambda * gamma - exp(-beta * (-oc)^alpha), if oc <
0.

U is the utility and oc is the objective consequence of a
gamble outcome. lambda is the loss aversion coefficient.
The Tversky & Kahneman (1992) assumption has also been
made, namely

U(-oc) = -lambda * U(oc) where oc >= 0.

Parameter restrictions from Saha (1993) are:

gamma > 1,

alpha != 0,

beta != 0, and

alpha * beta > 1.
}
\references{
Saha, A. (1993). Expo-power utility: A 'flexible' form for
absolute and relative risk aversion. American Journal of
Agricultural Economics, 75(4), 905-913.

Peel, D. A., & Zhang, J. (2009). The expo-power value
function as a candidate for the work-horse specification in
parametric versions of cumulative prospect theory.
Economics Letters, 105(3), 326-329.
}

