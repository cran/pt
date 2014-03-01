\docType{package}
\name{pt-package}
\alias{pt}
\alias{pt-package}
\title{An R package for Prospect Theory}
\description{
A package for computational prospect theory (PT) and
comparing PT to other risky decision making theories.
Choice visualisation capabilities are also provided.
}
\details{
The pt package provides the ability to create, save and
visualise choices. The results for different risky decision
making theories can be calculated for these choice
situations.

The fastest way to get started is to either create choices
directly from the command line using the
{\code{\link{Choices}}} function or load in choices from
previously prepared external text files using the
{\code{\link{choicesFromFile}}} function.

Once the choices are in R, it is possible to visualise them
using {\code{\link{drawChoices}}}. Newly created choices
can be saved to text files using
{\code{\link{saveChoices}}}.

The predictions of various risky decision making theories
can then be run on choices using the following functions:

{\code{\link{compareEV}}} (for expected value)

{\code{\link{compareEU}}} (expected utility)

{\code{\link{compareRDU}}} (rank-dependent utility)

{\code{\link{comparePT}}} (prospect theory)

{\code{\link{compareSWU}}} (subjectively weighted utility)

{\code{\link{compareSWAU}}} (subjectively weighted average
utility)

{\code{\link{compareRAM}}} (rank-affected multiplicative
weights utility)

{\code{\link{compareTAX}}} ((special) transfer of attention
exchange utility)

{\code{\link{compareGDU}}} ((lower) gains decomposition
utility)

{\code{\link{comparePRT}}} (prospective reference theory
utility)

Visualisation functions include:

{\code{\link{drawChoices}}} (draws choices)

{\code{\link{drawSimplex}}} (draws the Marschak-Machina
unit probability simplex)

{\code{\link{plotProbW}}} (draws a single probability
weighting function)

{\code{\link{plotOneParProbWFam}}} (draws families of one
parameter probability weighting functions)

{\code{\link{plotTwoParProbWFam}}} (draws families of two
parameter probability weighting functions)

{\code{\link{plotRP}}} (draws the risk premium)

{\code{\link{plotUtility}}} (draws the utility function)
}
\author{
Gary Au \email{gary.au@unimelb.edu.au}

Maintainer: Gary Au \email{gary.au@unimelb.edu.au}
}
\keyword{package}

