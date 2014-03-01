\docType{class}
\name{Choices-class}
\alias{Choices-class}
\title{The Choices class.}
\description{
The Choices class contains choices for a decision maker.
Each choice contains gambles.
}
\note{
A function (also called Choices) has been defined to create
instances of this class from the command line. Another
function (called choicesFromFile) reads in data from
external text files to create instances of the class.
}
\section{Slots}{
  \describe{ \item{\code{choices}:}{Object of class
  \code{"vector"}, containing Gambles objects for decision
  makers to choose from.} }
}
\seealso{
{\code{\link{Choices}},\code{\link{choicesFromFile}}}
}

