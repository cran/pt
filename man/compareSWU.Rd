\docType{methods}
\name{compareSWU}
\alias{compareSWU}
\alias{compareSWU,Choices-method}
\title{Compare choice gambles under Edwards' (1954, 1962) Subjective Weighted Utility (SWU).}
\usage{
compareSWU(object, prob_weight, utility, digits)

\S4method{compareSWU}{Choices}(object, prob_weight, utility, digits)
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
Compare choice gambles under Edwards' (1954, 1962)
Subjective Weighted Utility (SWU).
}
\examples{
# This example creates the two Allais common consequence paradox choices,
# and computes the SWU for each gamble in the choices.

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

compareSWU(my_choices,
	prob_weight=my_pwf,
	utility=my_utility,
	digits=4)
}
\references{
Edwards, W. (1954). The theory of decision making.
Psychological Bulletin, 51(4), 380-417.

Edwards, W. (1962). Subjective probabilities inferred from
decisions. Psychological Review, 69(2), 109-135.

Birnbaum, M. H. (1999). The paradoxes of Allais, stochastic
dominance, and decision weights. In J. Shanteau, B. A.
Mellers & D. A. Schum (Eds.), Decision science and
technology: Reflections on the contributions of Ward
Edwards (pp. 27-52). Norwell, MA: Kluwer Academic
Publishers.
}

