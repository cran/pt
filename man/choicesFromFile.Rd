\name{choicesFromFile}
\alias{choicesFromFile}
\title{Create an instance of a Choices class using data from an external text file.}
\usage{
choicesFromFile(input_file, choice_id_header, gamble_id_header,
outcome_id_header, objective_consequence_header, probability_header, DELIMITER)
}
\arguments{
  \item{input_file}{text, the input_file.}

  \item{choice_id_header}{text, the column name of the
  choice_id variable.}

  \item{gamble_id_header}{text, the column name of the
  gamble_id variable.}

  \item{outcome_id_header}{text, the column name of the
  outcome_id variable.}

  \item{objective_consequence_header}{text, the column name
  of the objective_consequence variable.}

  \item{probability_header,}{the column name of the
  probability_string variable.}

  \item{DELIMITER}{text, the delimeter character separating
  the fields in the input file.}
}
\description{
Create an instance of a Choices class using data from an
external text file.
}
\details{
This function is used to create a new instance of a Choices
class from an external text file. This file has at least 5
columns, delimited by the DELIMITER character string. Each
row of the file contains an individual outcome. The last
line of the file needs to be a blank row. An example input
file describing the Allais constant ratio paradox looks
like this, with the DELIMITER being a "\\t".

choice_id gamble_id outcome_id probability
objective_consequence

1 1 1 1 3000

1 2 1 0.8 4000

1 2 2 0.2 0

2 1 1 0.25 3000

2 1 2 0.75 0

2 2 1 0.2 4000

2 2 2 0.8 0

------

Note that the last line is a blank row.
}
\examples{
# This example loads up the choices for the Allais constant ratio paradox, which
# are available as text files in the pt package.

my_input_file <- system.file("external", "allais_constant_ratio_paradox.txt", package="pt")

my_choices <- choicesFromFile(input_file=my_input_file,
	choice_id_header="choice_id",
	gamble_id_header="gamble_id",
	outcome_id_header="outcome_id",
	objective_consequence_header="objective_consequence",
	probability_header="probability",
	DELIMITER="\\t")

my_choices
}

