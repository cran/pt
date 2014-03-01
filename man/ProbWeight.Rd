\name{ProbWeight}
\alias{ProbWeight}
\title{Create an instance of a ProbWeight class.}
\usage{
ProbWeight(fun, par)
}
\arguments{
  \item{fun}{text, the probability function string}

  \item{par}{vector, parameters for the probability
  function}
}
\description{
Creates an instance of the ProbWeight class.
}
\details{
This function creates an instance of a ProbWeight class.
The following functional forms are currently implemented:

linear

Tversky_Kahneman_1992 (requires 1 parameter > 0.28)

linear_in_log_odds (requires 2 parameters)

power (requires 2 parameters)

neo_additive (requires 2 parameters)

hyperbolic_logarithm (requires 2 parameters)

exponential_power (requires 2 parameters)

compound_invariance (requires 2 parameters)

constant_relative_sensitivity (requires 2 parameters)
}
\examples{
# This example creates a linear in log odds
# probability weighting function.

linear_in_log_odds_prob_weight <-
	ProbWeight(fun="linear_in_log_odds",
	par=c(alpha=0.61, beta=0.724))

# These examples create the probability weighting functions
# used by Tversky and Kahneman (1992).

tk_1992_positive_prob_weight <-
	ProbWeight(fun="Tversky_Kahneman_1992",
	par=c(alpha=0.61))

tk_1992_negative_prob_weight <-
	ProbWeight(fun="Tversky_Kahneman_1992",
	par=c(alpha=0.69))
}
\references{
Tversky, A., & Kahneman, D. (1992). Advances in prospect
theory: Cumulative representation of uncertainty. Journal
of Risk and Uncertainty, 5(4), 297-323.

Prelec, D. (1998). The probability weighting function.
Econometrica, 60(3), 497-528.

Wu, G., & Gonzalez, R. (1996). Curvature of the probability
weighting function. Management Science, 42(12), 1676-1690.

Wakker, P. P. (2010). Prospect theory: For risk and
ambiguity. Cambridge, UK: Cambridge University Press.

Stott, H. P. (2006). Cumulative prospect theory's
functional menagerie. Journal of Risk and Uncertainty,
32(2), 101-130.
}

