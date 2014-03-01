\name{plotUtility}
\alias{plotUtility}
\title{Plot a utility function.}
\usage{
plotUtility(my_title, my_title_colour, my_title_font_size, my_x_label, xmin,
  xmax, my_y_label, fun, par, fun_colour, draw_reference_line_flag,
  reference_line_colour, reference_line_style, my_labels, my_label_positions,
  my_label_colours, my_label_font_sizes)
}
\arguments{
  \item{my_title}{text, the title of the chart.}

  \item{my_title_colour}{text, the title colour.}

  \item{my_title_font_size}{numeric, the title font size.}

  \item{my_x_label}{text, the x-axis label.}

  \item{xmin}{numeric, the xmin on the x-axis.}

  \item{xmax}{numeric, the xmax on the x-axis.}

  \item{my_y_label}{text, the y-axis label.}

  \item{fun}{Utility, an instance of the Utility class.}

  \item{par}{vector, the parameters for the utility
  function.}

  \item{fun_colour}{text, the colour of the utility
  function line.}

  \item{draw_reference_line_flag}{logical, a boolean flag
  determining whether or not to draw a y=x reference line.}

  \item{reference_line_colour}{text, the reference line
  colour.}

  \item{reference_line_style}{numeric, the reference line
  style.}

  \item{my_labels}{vector, a vector of text labels to
  draw.}

  \item{my_label_positions}{list, a list of coordinates for
  the text labels.}

  \item{my_label_colours}{vector, stores the colours for
  each text label.}

  \item{my_label_font_sizes}{vector, stores the font size
  of each text label.}
}
\description{
Plot the utility function.
}
\examples{
plotUtility(my_x_label = "objective consequence",
	my_y_label = "subjective value",
	xmin = -10, xmax = 10,
	fun=power_uf,
	par=c(alpha = 0.88, beta = 0.88, lambda = 2.25),
	fun_colour = "purple",
	draw_reference_line_flag = TRUE,
	reference_line_colour = "red",
	reference_line_style = 1)
}

