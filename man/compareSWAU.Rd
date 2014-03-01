\docType{methods}
\name{compareSWAU}
\alias{compareSWAU}
\alias{compareSWAU,Choices-method}
\title{Compare choices under Subjectively weighted average utility (SWAU).}
\usage{
compareSWAU(object, prob_weight, utility, digits)

\S4method{compareSWAU}{Choices}(object, prob_weight, utility, digits)
}
\arguments{
  \item{object}{Choices, an instance of a Choices class.}

  \item{prob_weight}{ProbWeight, an instance of a
  ProbWeight class.}

  \item{utility}{Utility, an instance of a Utility class.}

  \item{digits}{numeric, the number of digits to display in
  the output.}
}
\description{
Compare choices under Subjectively weighted average utility
(SWAU).
}
\examples{
# This example creates the two Allais common consequence paradox choices,
# and computes the SWAU for each gamble in the choices.

choice_ids <- c(1, 1, 1, 1, 2, 2, 2, 2)

gamble_ids <- c(1, 1, 1, 2, 1, 1, 2, 2)

outcome_ids <- c(1, 2, 3, 1, 1, 2, 1, 2)

objective_consequences <- c(2500, 2400, 0, 2400,
	2500, 0, 2400, 0)

probability_strings <- c("0.33", "0.66", "0.01", "1.0",
	"0.33", "0.67", "0.34", "0.66")

my_choices <- Choices(choice_ids=choice_ids,
	gamble_ids=gamble_ids,
	outcome_ids=outcome_ids,
	objective_consequences=objective_consequences,
	probability_strings=probability_strings)

my_choices

my_utility <- Utility(fun="power",
	par=c(alpha=0.4, beta=0.4, lambda=1))

my_pwf <-
ProbWeight(fun="linear_in_log_odds",
	par=c(alpha=0.4, beta=0.4))

compareSWAU(my_choices,
	prob_weight=my_pwf,
	utility=my_utility,
	digits=4)
}
\references{
Karmarkar, U. S. (1978). Subjectively weighted utility: A
descriptive extension of the expected utility model.
Organizational Behavior & Human Performance, 21(1), 61-72.

Karmarkar, U. S. (1979). Subjectively weighted utility and
the Allais Paradox. Organizational Behavior & Human
Performance, 24(1), 67-72.

Viscusi, W. K. (1989). Prospective reference theory: Toward
an explanation of the paradoxes. Journal of Risk and
Uncertainty, 2(3), 235-263.

Lattimore, P. K., Baker, J. R., & Witte, A. D. (1992). The
influence of probability on risky choice: A parametric
examination. Journal of Economic Behavior and Organization,
17(3), 377-400.

Birnbaum, M. H. (1999). The paradoxes of Allais, stochastic
dominance, and decision weights. In J. Shanteau, B. A.
Mellers & D. A. Schum (Eds.), Decision science and
technology: Reflections on the contributions of Ward
Edwards (pp. 27-52). Norwell, MA: Kluwer Academic
Publishers.
}

