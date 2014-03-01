\docType{methods}
\name{compareRDU}
\alias{compareRDU}
\alias{compareRDU,Choices-method}
\title{Compare choice gambles under Quiggin's (1993) Rank-dependent utility (RDU).}
\usage{
compareRDU(object, prob_weight, utility, digits)

\S4method{compareRDU}{Choices}(object, prob_weight, utility, digits)
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
Compare choice gambles under Quiggin's (1993)
Rank-dependent utility (RDU).
}
\examples{
# This example creates the two Allais common consequence paradox choices,
# and computes the RDU for each gamble in the choices.

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

tk_1992_utility <- Utility(fun="power",
	par=c(alpha=0.88, beta=0.88, lambda=2.25))

tk_1992_positive_probWeight <-
	ProbWeight(fun=
	"Tversky_Kahneman_1992",
	par=c(alpha=0.61))

compareRDU(my_choices,
	prob_weight=
	tk_1992_positive_probWeight,
	utility=tk_1992_utility,
	digits=4)
}
\references{
Quiggin, J. (1982). A theory of anticipated utility.
Journal of Economic Behavior & Organization, 3(4), 323-343.

Quiggin, J. (1985). Subjective utility, anticipated
utility, and the Allais paradox. Organizational Behavior
and Human Decision Processes, 35(1), 94-101.

Quiggin, J. (1993). Generalized expected utility theory:
The rank-dependent model. Boston, MA: Kluwer Academic
Publishers.
}

