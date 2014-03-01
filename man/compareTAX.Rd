\docType{methods}
\name{compareTAX}
\alias{compareTAX}
\alias{compareTAX,Choices-method}
\title{Compare choice gambles under Birnbaum's (2008) configural weight (special) TAX theory.}
\usage{
compareTAX(object, prob_weight, utility, delta, digits)

\S4method{compareTAX}{Choices}(object, prob_weight, utility, delta, digits)
}
\arguments{
  \item{object}{Choices, an instance of a Choices class.}

  \item{prob_weight}{ProbWeight, an instance of a
  ProbWeight class.}

  \item{utility}{Utility, an instance of a Utility class.}

  \item{delta}{numeric, the delta parameter in Birnbaum's
  TAX theory.}

  \item{digits}{numeric, the number of digits to display in
  the output.}
}
\description{
Compare choice gambles under Birnbaum's (2008) configural
weight (special) TAX theory.
}
\examples{
# This example creates the two Allais common consequence paradox choices,
# and computes the TAX for each gamble in the choices.

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

my_utility <- Utility(fun="linear",
	par=c(lambda=1))

power_probability_weighting <-
	ProbWeight(fun="power",
	par=c(alpha=0.7, beta=1))

compareTAX(my_choices,
	prob_weight=power_probability_weighting,
	utility=my_utility,
	delta=-1,
	digits=4)
}
\references{
Birnbaum, M. H. (2008). New paradoxes of risky decision
making. Psychological Review, 115(2), 463-501.
}

