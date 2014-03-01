\name{Utility}
\alias{Utility}
\title{Create an instance of a Utility class.}
\usage{
Utility(fun, par)
}
\arguments{
  \item{fun}{text, a string selecting the utility function}

  \item{par}{vector, parameters for the utility function}
}
\description{
This function creates an instance of a Utility class.
}
\details{
This function creates an instance of a Utility class
object. Two arguments need to be provided to create this
object. The first argument is a text string that defines
the functional form of a utility function stored by the
Utility class object. The second argument is a vector of
parameters needed for the selected utility function.

The following functional forms are currently implemented:

linear (requires 1 parameter)

power (requires 3 parameters)

exponential (requires 2 parameters)

normalized_exponential_uf (requires 3 parameters)

normalized_logarithmic_uf (requires 3 parameters)

normalized_power_uf (requires 3 parameters)

quadratic_uf (requires 3 parameters)

logarithmic_uf (requires 3 parameters)

expo_power_uf (requires 4 parameters)

general_linear_uf (requires 3 parameters)

general_power_uf (requires 5 parameters)
}
\examples{
# This example creates the power utility function with parameters
# used in the Tversky & Kahneman (1992) paper.

tk_1992_utility <- Utility(fun="power",
	par=c(alpha=0.88, beta=0.88, lambda=2.25))

# This example creates a linear utility function.

my_linear_utility <- Utility(fun="linear",
	par=c(lambda=1))
}
\references{
Tversky, A., & Kahneman, D. (1992). Advances in prospect
theory: Cumulative representation of uncertainty. Journal
of Risk and Uncertainty, 5(4), 297-323.

Wakker, P. P. (2010). Prospect theory: For risk and
ambiguity. Cambridge, UK: Cambridge University Press.

Stott, H. P. (2006). Cumulative prospect theory's
functional menagerie. Journal of Risk and Uncertainty,
32(2), 101-130.

Birnbaum, M. H. (2008). New paradoxes of risky decision
making. Psychological Review, 115(2), 463-501.
}

