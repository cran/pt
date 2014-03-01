\name{plotProbW}
\alias{plotProbW}
\title{Plot a probability weighting function.}
\usage{
plotProbW(my_title, my_title_colour, my_title_font_size, my_x_label, my_y_label,
  pwf, par, draw_reference_line_flag, reference_line_colour,
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
Plot a probability weighting function using base graphics.
}
\examples{
plotProbW(my_title=expression(paste("Kahneman & Tversky (1992), ",
	c==0.61)),
	my_title_colour="black", my_title_font_size=4,
	my_x_label = "p", my_y_label = "w(p)",
	pwf=kt_pwf, par=c(c=0.61),
	draw_reference_line_flag=TRUE, reference_line_colour="red",
	reference_line_style="dotted",
	my_labels=c(expression(paste(w(italic(p)) == frac(italic(p)^c,
	(italic(p)^c + (1-italic(p))^c)^(1/c))))),
	my_label_positions=list(c(0.4,0.8)),
	font_scaling=1.0)
}

