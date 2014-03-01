\name{Choices}
\alias{Choices}
\title{Create a new instance of a Choices class.}
\usage{
Choices(choice_ids, gamble_ids, outcome_ids, objective_consequences, probability_strings)
}
\arguments{
  \item{choice_ids}{vector, contains the choice_id of each
  objective_consequence.}

  \item{gamble_ids}{vector, contains the gamble_id of each
  objective_consequence.}

  \item{outcome_ids}{vector, contains the outcome_id of
  each objective_consequence.}

  \item{objective_consequences}{vector, contains the
  objective consequences.}

  \item{probability_strings}{vector, contains the
  probability_string of each objective consequence.}
}
\description{
Create choices using five vectors.
}
\details{
This function creates a new instance of a Choices class.
The inputs are five vectors, representing the properties of
each outcome.
}
\examples{
choice_ids <- c(1, 1, 1, 1, 1, 1, 1, 1)

gamble_ids <- c(1, 1, 1, 1, 2, 2, 2, 2)

outcome_ids <- c(1, 2, 3, 4, 1, 2, 3, 4)

objective_consequences <- c(7, 7, 84, 90,
7, 10, 90, 90)

probability_strings <- c("0.1", "0.3", "0.3", "0.3",
"0.1", "0.3", "0.3", "0.3")

my_choices <- Choices(choice_ids=choice_ids,
	gamble_ids=gamble_ids,
	outcome_ids=outcome_ids,
	objective_consequences=objective_consequences,
	probability_strings=probability_strings)

my_choices
}

