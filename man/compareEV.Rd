\docType{methods}
\name{compareEV}
\alias{compareEV}
\alias{compareEV,Choices-method}
\title{Compare the expected value (EV) of choice gambles.}
\usage{
compareEV(object, digits)

\S4method{compareEV}{Choices}(object, digits)
}
\arguments{
  \item{object}{Choices, an instance of a Choices class.}

  \item{digits}{numeric, the number of digits to display in
  the output.}
}
\description{
Compare the expected value (EV) of choice gambles.
}
\examples{
# This example creates the two Allais common consequence paradox choices,
# and computes the EV for each gamble in the choices.

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

compareEV(my_choices, digits=4)
}
\references{
Montgomery, H., & Adelbratt, T. (1982). Gambling decisions
and information about expected value. Organizational
Behavior and Human Performance, 29(1), 39-57.

Lichtenstein, S., Slovic, P., & Zink, D. (1969). Effect of
instruction in expected value on optimality of gambling
decisions. Journal of Experimental Psychology, 79(2, Pt.1),
236-240.

Li, S. (2003). The role of Expected Value illustrated in
decision-making under risk: Single-play vs multiple-play.
Journal of Risk Research, 6(2), 113-124.

Colbert, G., Murray, D., & Nieschwietz, R. (2009). The use
of expected value in pricing judgments. Journal of Risk
Research, 12(2), 199-208.

Yates, J. F. (1990). Judgment and decision making.
Englewood Cliffs, NJ: Prentice Hall.
}

