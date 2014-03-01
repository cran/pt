\docType{methods}
\name{compareGDU}
\alias{compareGDU}
\alias{compareGDU,Choices-method}
\title{Compare choice gambles under Luce's (2000) (Lower) Gains-decompositions utility (GDU) theory.}
\usage{
compareGDU(object, prob_weight, utility, digits)

\S4method{compareGDU}{Choices}(object, prob_weight, utility, digits)
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
Compare choice gambles under Luce's (2000) (Lower)
Gains-decompositions utility (GDU) theory.
}
\examples{
# This example creates the two Allais common consequence paradox choices,
# and computes the GDU for each gamble in the choices.

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

my_pwf <-
	ProbWeight(fun="compound_invariance",
	par=c(alpha=0.542, beta=1.382))

my_utility <- Utility(fun="power",
	par=c(alpha=1, beta=1, lambda=1))

compareGDU(my_choices,
	prob_weight=my_pwf,
	utility=my_utility,
	digits=4)
}
\references{
Luce, R. D. (2000). Utility of gains and losses:
Measurement-theoretical and experimental approaches.
Mahwah, NJ: Lawrence Erlbaum Associates.
}

