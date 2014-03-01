\docType{methods}
\name{compareRAM}
\alias{compareRAM}
\alias{compareRAM,Choices-method}
\title{Compare choice gambles under Birnbaum's (2008) configural weight RAM theory.}
\usage{
compareRAM(object, branch_weight_list, prob_weight, utility, digits)

\S4method{compareRAM}{Choices}(object, branch_weight_list, prob_weight, utility,
  digits)
}
\arguments{
  \item{object}{Choices, an instance of a Choices class.}

  \item{branch_weight_list}{list, a list of branch
  weighting vectors.}

  \item{prob_weight}{ProbWeight, an instance of a
  ProbWeight class.}

  \item{utility}{Utility, an instance of a Utility class.}

  \item{digits}{numeric, the number of digits to display in
  the output.}
}
\description{
Compare choice gambles under Birnbaum's (2008) configural
weight RAM theory.
}
\examples{
# This example creates the two Allais common consequence paradox choices,
# and computes the RAM for each gamble in the choices.

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

# note that the maximum number of outcomes in the gambles is 3,
# so branch weights for 3 outcomes need to be provided.

branch_weight_list <- list(c(1),
	c(0.3738, 0.6262),
	c(0.16, 0.33, 0.51))

my_utility <- Utility(fun="linear",
	par=c(lambda=1))

power_probability_weighting <-
	ProbWeight(fun="power",
	par=c(alpha=0.7, beta=1))

compareRAM(my_choices,
	branch_weight_list=branch_weight_list,
	prob_weight=power_probability_weighting,
	utility=my_utility,
	digits=4)
}
\references{
Birnbaum, M. H. (2008). New paradoxes of risky decision
making. Psychological Review, 115(2), 463-501.
}

