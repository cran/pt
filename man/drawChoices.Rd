\name{drawChoices}
\alias{drawChoices}
\title{Draw a one-stage decision tree.}
\usage{
drawChoices(choices, decision_square_x, decision_square_edge_length,
  circle_radius, y_split_gap, x_split_offset, probability_text_digits,
  y_probability_text_offset, y_value_text_offset, x_value_text_offset,
  probability_text_font_colour, probability_text_font_size,
  objective_consequence_text_font_colour, objective_consequence_text_font_size,
  label, label_font_colour, label_font_size, label_positions, line_positions,
  line_colours, line_styles, line_arrows, line_widths)
}
\arguments{
  \item{choices,}{an instance of class Choices}

  \item{decision_square_x}{numeric, the decision_square_x
  position}

  \item{decision_square_edge_length}{numeric, the
  decision_square_edge_length}

  \item{circle_radius}{numeric, the circle_radius}

  \item{y_split_gap}{numeric, the vertical gap between
  outcomes}

  \item{x_split_offset}{numeric, the horizontal gap between
  the decision square and decision circles}

  \item{probability_text_digits}{numeric, the digits to use
  for probability text}

  \item{y_probability_text_offset}{numeric, the y offset
  between an outcome line and the probability text}

  \item{y_value_text_offset}{numeric, the vertical offset
  between an outcome line and the objective_consequence
  text}

  \item{x_value_text_offset}{numeric, the horizontal offset
  between an outcome line and the objective_consequence
  text}

  \item{probability_text_font_colour}{character, the colour
  of the probability_text}

  \item{probability_text_font_size}{numeric, the font size
  of the probability_text}

  \item{objective_consequence_text_font_colour}{character,
  the colour of the objective_consequence_text}

  \item{objective_consequence_text_font_size}{numeric, the
  font size of the objective_consequence_text_font_size}

  \item{label}{vector, extra text labels}

  \item{label_font_colour}{vector, label_font_colour}

  \item{label_font_size}{vector, label_font_size}

  \item{label_positions}{list, a list of label coordinates
  of the form (x,y)}

  \item{line_positions}{list, a list of line coordinates of
  the form (x1,y1,x2,y2)}

  \item{line_colours}{vector, line_colours}

  \item{line_styles}{vector, line_styles}

  \item{line_arrows}{vector, line_arrows}

  \item{line_widths}{vector, line_widths}
}
\description{
Draws choices.
}
\examples{
# This example creates the Allais common consequence paradox choices, and
# draws them.

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

drawChoices(my_choices, decision_square_x=0.2, decision_square_edge_length=0.05,
	circle_radius=0.025, y_split_gap=0.1, x_split_offset=0.03,
	probability_text_digits=4, y_probability_text_offset=0.015,
	y_value_text_offset=0.005, x_value_text_offset=0.025,
	probability_text_font_colour="red", probability_text_font_size=11,
	objective_consequence_text_font_colour="blue",
	objective_consequence_text_font_size=11, label=c("A","B","C", "D"),
	label_font_colour=c("orange","magenta","green","blue"),
	label_font_size=c(11,11,11,11),
	label_positions=list(c(0.26,0.85),c(0.26,0.55),
		c(0.26,0.4),c(0.26,0.1)))
}

