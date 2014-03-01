\name{drawSimplex}
\alias{drawSimplex}
\title{Draw the probability simplex.}
\usage{
drawSimplex(x1, x2, x3, line_dot_density, draw_ev_flag, ev_colour, draw_pt_flag,
  alpha, beta, pt_colour, draw_utility_flag, utility, eu_colour, start_points,
  labels, label_positions, label_colours, label_font_sizes, label_font_faces,
  label_rotations, circle_radii, circle_outline_colours, circle_fill_colours,
  circle_positions, lines, line_widths, line_styles, line_colours, arrows,
  arrow_widths, arrow_styles, arrow_colours)
}
\arguments{
  \item{x1}{numeric, x1}

  \item{x2}{numeric, x2}

  \item{x3}{numeric, x3}

  \item{line_dot_density}{numeric, the number of dots to
  use when drawing lines}

  \item{draw_ev_flag}{logical, flag to tell whether to draw
  expected value indifference lines}

  \item{ev_colour,}{the colour of the expected value
  indifference lines}

  \item{draw_pt_flag}{logical, flag to tell whether to draw
  prospect theory indifference curves}

  \item{alpha}{numeric, the alpha parameter in the
  linear_in_log_odds pwf}

  \item{beta}{numeric, beta parameter in the
  linear_in_log_odds pwf}

  \item{pt_colour}{character, the colour of the prospect
  theory indifference curves}

  \item{draw_utility_flag}{logical, flag to tell whether to
  draw expected utility indifference lines}

  \item{utility}{Utility, utility}

  \item{eu_colour}{character, the colour of the expected
  utility indifference lines}

  \item{start_points}{list, start_points for the family of
  indifference lines or curves}

  \item{labels}{vector, a vector of text labels}

  \item{label_positions}{vector, a vector of
  label_positions}

  \item{label_colours}{vector, a vector of label_colours}

  \item{label_font_sizes}{vector, a vector of
  label_font_sizes}

  \item{label_font_faces}{vector, a vector of
  label_font_faces}

  \item{label_rotations}{vector, a vector of
  label_rotations}

  \item{circle_radii}{vector, a vector of circle_radii}

  \item{circle_outline_colours}{vector, a vector of
  circle_outline_colours}

  \item{circle_fill_colours}{vector, a vector of
  circle_fill_colours}

  \item{circle_positions}{vector, a vector of
  circle_positions}

  \item{lines}{vector, a vector of lines}

  \item{line_widths}{vector, a vector of line_widths}

  \item{line_styles}{vector, a vector of line_styles}

  \item{line_colours}{vector, a vector of line_colours}

  \item{arrows}{vector, a vector of arrows}

  \item{arrow_widths}{vector, a vector of arrow_widths}

  \item{arrow_styles}{vector, a vector of arrow_styles}

  \item{arrow_colours}{vector, a vector of arrow_colours}
}
\description{
Draws the probability simplex.
}
\details{
Iso-expected value lines, expected utility indifference
lines and prospect theory indifference curves (based on a
linear in log odds probability weighting function) can be
drawn.
}
\examples{
my_utility <- Utility(fun="power",
	par=c(alpha=0.88, beta=0.88, lambda=2.25))

drawSimplex(x1=0, x2=100, x3=200,
	line_dot_density=100,
	draw_ev_flag=TRUE, ev_colour="black",
	draw_pt_flag=TRUE, alpha=0.61, beta=0.724, pt_colour="red",
	draw_utility_flag=TRUE, utility=my_utility, eu_colour="purple",
	start_points=list(c(0.1,0.9),c(0.2,0.8),c(0.3,0.7),
	c(0.4,0.6),c(0.5,0.5),c(0.6,0.4),c(0.7,0.3),c(0.8,0.2),c(0.9,0.1)),
		labels=c("A","B","C","D","increasing preference"),
		label_positions=list(c(0.05,0.02),c(0.07,0.12),
		c(0.92,0.02),c(0.95,0.10),c(0.7,0.7)),
		label_colours=c("red","green","blue","orange","red"),
		label_font_sizes=c(12,12,12,12,16),
		label_font_faces=c("plain","plain","plain","plain","bold"),
		label_rotations=c(0,0,0,0,-45),
		circle_radii=c(0.005,0.005,0.005,0.005),
		circle_outline_colours=c("black","black","black","black"),
		circle_fill_colours=c("red","green","blue","orange"),
		circle_positions=list(c(0,0),c(0.01,0.1),c(0.89,0),c(0.9,0.1)),
		lines=list(c(0,0,0.01,0.1),c(0.89,0,0.9,0.1),
		c(0.01,0.1,0.9,0.1),c(0,0,0.89,0)),
		line_widths=c(1, 1, 1, 1),
		line_styles=c("dashed", "dashed", "dashed", "dashed"),
		line_colours=c("red","red","red","red"),
		arrows=list(c(0.8,0.5,0.5,0.8)),
		arrow_widths=c(2),
		arrow_styles=c("solid"),
		arrow_colours=c("red"))
}
\references{
Marschak, J. (1950). Rational behavior, uncertain
prospects, and measurable utility. Econometrica, 18(2),
111-141.

Machina, M. J. (1987). Choice under uncertainty: Problems
solved and unsolved. Journal of Economic Perspectives,
1(1), 121-154.
}

