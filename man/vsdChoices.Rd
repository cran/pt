\name{vsdChoices}
\alias{vsdChoices}
\title{Create choice situations that can elicit violations of (first-order) stochastic dominance in decision makers, using Birbaum's (1997) recipe.}
\usage{
vsdChoices(x, y, p, q, x_plus, y_minus, r)
}
\arguments{
  \item{x}{numeric, x is one of the objective consequences
  in the original binary gamble G0.}

  \item{y}{numeric, y is the other objective consequences
  in the original binary gamble G0.}

  \item{p}{text, p is a probability string associated with
  the objective consequence x.}

  \item{q}{text, q is a probability string associated with
  the objective consequence y.}

  \item{x_plus}{numeric, x_plus}

  \item{y_minus}{numeric, y_minus}

  \item{r}{numeric, r the g_minus probability offset}
}
\description{
Create choice situations that can elicit violations of
(first-order) stochastic dominance in decision makers,
using Birbaum's (1997) recipe.
}
\details{
Given a binary gamble G0, this function creates a pair of
three outcome gambles G+ and G- and a pair of four outcome
gambles GS+, GS- that can elicit vsd behaviour in decision
makers. e.g.

G0 = (96, 0.9; 12, 0.1)

G+ = (12, 0.05; 14, 0.05; 96, 0.9) and G- = (12, 0.1; 90,
0.05; 96, 0.85)

where G+ dominates G0 and G- is dominated by G0.

GS+ = (12, 0.05; 14, 0.05; 96, 0.05; 96, 0.85) and GS- =
(12, 0.05; 12, 0.05; 90, 0.05; 96, 0.85)
}
\examples{
my_choices_list <- vsdChoices(x=12, y=96, p="0.1", q="0.9", x_plus=14, y_minus=90, r="0.05")

original_choice <- my_choices_list[[1]]

original_choice

pair_of_three_outcome_choices <- my_choices_list[[2]]

pair_of_three_outcome_choices

pair_of_four_outcome_choices <- my_choices_list[[3]]

pair_of_four_outcome_choices
}
\references{
Figure 5, p. 475 from Birnbaum, M. H. (2008). New paradoxes
of risky decision making. Psychological Review, 115(2),
463-501.

Birnbaum, M. H. (1997). Violations of monotonicity in
judgment and decision making. In A. A. J. Marley (Ed.),
Choice, decision, and measurement: Essays in honor of R.
Duncan Luce (pp. 73-100). Mahwah, NJ: Erlbaum.
}

