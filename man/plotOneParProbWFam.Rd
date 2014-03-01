\name{plotOneParProbWFam}
\alias{plotOneParProbWFam}
\title{Plot a family of one parameter probability weighting functions.}
\usage{
plotOneParProbWFam(my_title, my_title_colour, my_title_font_size, my_x_label,
  my_y_label, pwf, par, draw_reference_line_flag, reference_line_colour,
  reference_line_style, my_labels, my_label_positions, font_scaling,
  arrow_positions)
}
\arguments{
  \item{my_title}{text, the title}

  \item{my_title_colour}{text, the title colour}

  \item{my_title_font_size}{numeric, the title font size}

  \item{my_x_label}{text, my_x_label}

  \item{my_y_label}{text, the my_y_label}

  \item{pwf}{function, the pwf}

  \item{par}{vector, the pwf_parameters}

  \item{draw_reference_line_flag}{logical,
  draw_reference_line_flag}

  \item{reference_line_colour}{text, reference_line_colour}

  \item{reference_line_style}{text, reference_line_style}

  \item{my_labels}{vector, labels}

  \item{my_label_positions}{vector, the coordinates for the
  labels}

  \item{font_scaling}{numeric, the scaling factor for the
  labels}

  \item{arrow_positions}{vector, the positions of arrow
  lines}
}
\description{
Plot a family of one parameter probability weighting
functions using base graphics.
}
\examples{
plotOneParProbWFam(my_title="Tversky & Kahneman (1992) family",
	my_title_colour="black", my_title_font_size=4,
	my_x_label = "p", my_y_label = "w(p)", pwf=kt_pwf,
	par=c(0.3, 0.61, 0.8, 1.0, 1.3),
	draw_reference_line_flag=TRUE, reference_line_colour="red",
	reference_line_style="dotted",
	my_labels=c(expression(paste(alpha == 0.3)),
	expression(paste(alpha == 0.61)),
	expression(paste(alpha == 0.8)),
	expression(paste(alpha == 1.0)),
	expression(paste(alpha == 1.3)),
	expression(paste(w(italic(p)) == frac(italic(p)^alpha,
	(italic(p)^alpha + (1-italic(p))^alpha)^(1/alpha))))),
	my_label_positions=list(c(0.9,0.15),c(0.7,0.45),c(0.15,0.5),
	c(0.31,0.62),c(0.5,0.7),c(0.42, 0.9)),
	font_scaling=1.0,
	arrow_positions = list(c(0.3,0.5,0.39,0.41),c(0.42,0.58,0.52,0.51),
		c(0.59,0.66,0.66,0.66)))
}
\references{
p. 207, Fig. 7.2.1 from Wakker, P. P. (2010). Prospect
theory: For risk and ambiguity. Cambridge, UK: Cambridge
University Press.
}

