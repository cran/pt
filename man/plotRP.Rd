\name{plotRP}
\alias{plotRP}
\title{Plot the risk premium.}
\usage{
plotRP(my_title, my_title_colour, my_title_font_size, my_x_label, xmin, xmax,
  my_y_label, my_color, fun, par, ev, eu, ce, my_labels, my_label_colors,
  my_label_positions, font_scaling)
}
\arguments{
  \item{my_title}{text, the title}

  \item{my_title_colour}{text, the title colour}

  \item{my_title_font_size}{numeric, the title font size}

  \item{my_x_label}{text, my_x_label}

  \item{xmin}{numeric, the xmin}

  \item{xmax}{numeric, the xmax}

  \item{my_y_label}{text, the my_y_label}

  \item{my_color}{text, the line color}

  \item{fun}{function, the utility function}

  \item{par}{vector, the uf_parameters}

  \item{ev}{numeric, the expected value}

  \item{eu}{numeric, the expected utility}

  \item{ce}{numeric, the certainty equivalent}

  \item{my_labels}{vector, text labels}

  \item{my_label_colors}{vector, colors of the text labels}

  \item{my_label_positions}{vector, positions of the text
  labels}

  \item{font_scaling}{numeric, the scaling of the text
  labels}
}
\description{
Plot the risk premium.
}
\examples{
choice_ids <- c(1, 1, 1, 2, 2, 2, 2)

gamble_ids <- c(1, 1, 2, 1, 1, 2, 2)

outcome_ids <- c(1, 1, 2, 1, 2, 1, 2)

objective_consequences <- c(4000, 0, 3000,
4000, 0, 3000, 0)

probability_strings <- c("0.8", "0.2", "1.0",
"0.2", "0.8", "0.25", "0.75")

my_choices <- Choices(choice_ids=choice_ids,
	gamble_ids=gamble_ids,
	outcome_ids=outcome_ids,
	objective_consequences=objective_consequences,
	probability_strings=probability_strings)

my_choices

my_utility <- Utility(fun="power",
par=c(alpha=0.88, beta=0.88, lambda=1))
eu_df <- compareEU(my_choices, utility=my_utility, digits=4)
eu_df

ev <- as.numeric(eu_df$ev[1])
eu <- as.numeric(eu_df$eu[1])
ce <- as.numeric(eu_df$ce[1])

plotRP(my_title = "risk premium",
	my_title_colour="black",
	my_title_font_size=4,
	my_x_label = "objective consequence",
	my_y_label = "subjective value",
	xmin = 2500, xmax = 3500,
	my_color="violet",
	fun=power_uf,
	par=c(alpha=0.88, beta=0.88, lambda=1),
	ev=ev, eu=eu, ce=ce,
	my_labels=c(expression(paste(U(x)==x^alpha, ",
	", x>=0)),
	expression(paste(plain()==-lambda * (-x)^beta, ", ", x<0)),
	"ev","eu","ce","rp"),
	my_label_colors=c("violet","violet","black","red","orange","blue"),
	my_label_positions=list(c(2700,1275),c(2740,1250),c(3250,1075),
	c(2800,1170),c(3050,1075),c(3150,1170)),
	font_scaling=1)
}

