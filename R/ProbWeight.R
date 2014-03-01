########################
#
# ProbWeight, a S4 class
#
########################	


#' The ProbWeight class.
#' 
#' The ProbWeight class stores both the form and parameter specification for a probability weighting function.
#' 
#' @section Slots:
#'  \describe{
#'    \item{\code{fun}:}{Object of class \code{"text"}, containing a text string that specifies the functional form of the probability weighting function.}
#'    \item{\code{par}:}{Object of class \code{"vector"}, containing the parameter specifications for the probability weighting function.}
#'  }
#'
#' @note A function (also called ProbWeight) has been defined to create an instance of this class.
#' @name ProbWeight-class
#' @aliases ProbWeight-class
#' @rdname ProbWeight-class
#' @seealso {\code{\link{ProbWeight}}}
#' @exportClass ProbWeight
setClass(
	Class = "ProbWeight",
	representation = representation
	(
		fun = "character",
		par = "vector"
	),
	# check for input consistency when creating new ProbWeight objects using "new" constructor
	validity = function(object)
	{
		# run the ProbWeight inspector
		if (get_probability_function(object) == "linear")
		{
		}		
		else if (get_probability_function(object) == "Tversky_Kahneman_1992")
		{
			if (get_number_of_parameters(object) != 1)
			{
				stop(paste(get_probability_function(object), " weighting function requires 1 parameter >= 0.28.\n", sep = ""))
			}
			else if (object@par[1] < 0.28)
			{
				stop(paste(get_probability_function(object), " weighting function requires 1 parameter >= 0.28 as the function is not strictly increasing for smaller values.\n", sep = ""))				
			}
		}
		else if (get_probability_function(object) == "linear_in_log_odds")
		{
			if (get_number_of_parameters(object) != 2)
			{
				stop(paste(get_probability_function(object), " weighting function requires 2 parameters.\n", sep = ""))
			}
		}
		else if (get_probability_function(object) == "power")
		{
			if (get_number_of_parameters(object) != 2)
			{
				stop(paste(get_probability_function(object), " weighting function requires 2 parameters.\n", sep = ""))
			}
		}
		else if (get_probability_function(object) == "neo_additive")
		{
			if (get_number_of_parameters(object) != 2)
			{
				stop(paste(get_probability_function(object), " weighting function requires 2 parameters.\n", sep = ""))
			}
			else
			{
				alpha <- object@par[1]
				beta <- object@par[2]
				
				if ((alpha < 0) | (beta < 0) | ((alpha + beta) > 1))
				{
					stop(paste("neo additive pwf requires: alpha >= 0, beta >= 0, alpha + beta <= 1.\n", sep = ""))					
				}
			}
		}
		else if (get_probability_function(object) == "hyperbolic_logarithm")
		{
			if (get_number_of_parameters(object) != 2)
			{
				stop(paste(get_probability_function(object), " weighting function requires 2 parameters.\n", sep = ""))
			}
			else
			{
				alpha <- object@par[1]
				beta <- object@par[2]
				
				if ((alpha < 0) | (beta < 0))
				{
					stop(paste("hyperbolic logarithm pwf requires: alpha > 0, beta > 0.", sep = ""))					
				}
			}			
		}
		else if (get_probability_function(object) == "exponential_power")
		{
			if (get_number_of_parameters(object) != 2)
			{
				stop(paste(get_probability_function(object), " weighting function requires 2 parameters.\n", sep = ""))
			}
			else
			{
				alpha <- object@par[1]
				beta <- object@par[2]
				
				if ((alpha == 0) | (beta < 0))
				{
					stop(paste("exponential power pwf requires: alpha != 0, beta > 0.", sep = ""))					
				}
			}
		}
		else if (get_probability_function(object) == "compound_invariance")
		{
			if (get_number_of_parameters(object) != 2)
			{
				stop(paste(get_probability_function(object), " weighting function requires 2 parameters.\n", sep = ""))
			}
			else
			{
				alpha <- object@par[1]
				beta <- object@par[2]
				
				if ((alpha < 0) | (beta < 0))
				{
					stop(paste("compound invariance pwf requires: alpha > 0, beta > 0.", sep = ""))					
				}
			}
		}
		else if (get_probability_function(object) == "constant_relative_sensitivity")
		{
			if (get_number_of_parameters(object) != 2)
			{
				stop(paste(get_probability_function(object), " weighting function requires 2 parameters.\n", sep = ""))
			}
			else
			{
				alpha <- object@par[1]
				beta <- object@par[2]
				
				if ((alpha < 0) | (beta < 0) | (beta > 1))
				{
					stop(paste("compound invariance pwf requires: alpha > 0, 0 <= beta <= 1.", sep = ""))					
				}
			}
		}	
		else
		{
			stop(paste(get_probability_function(object), " is an unimplemented probability weighting function\n", sep = ""))			
		}
		

		return (TRUE)
	}
)


#' @name ProbWeight
#' @title Create an instance of a ProbWeight class.
#' @rdname ProbWeight
#' @aliases ProbWeight
#' @description Creates an instance of the ProbWeight class.
#' @details This function creates an instance of a ProbWeight class.
#' The following functional forms are currently implemented:
#' 
#' linear
#'
#' Tversky_Kahneman_1992 (requires 1 parameter > 0.28)
#' 
#' linear_in_log_odds (requires 2 parameters)
#'
#' power (requires 2 parameters)
#' 
#' neo_additive (requires 2 parameters)
#' 
#' hyperbolic_logarithm (requires 2 parameters)
#' 
#' exponential_power (requires 2 parameters)
#' 
#' compound_invariance (requires 2 parameters)
#' 
#' constant_relative_sensitivity (requires 2 parameters)
#' 
#' @usage ProbWeight(fun, par)
#' @param fun text, the probability function string
#' @param par vector, parameters for the probability function
#' @references
#' 
#' Tversky, A., & Kahneman, D. (1992). Advances in prospect theory: Cumulative representation of uncertainty. Journal of Risk and Uncertainty, 5(4), 297-323.
#' 
#' Prelec, D. (1998). The probability weighting function. Econometrica, 60(3), 497-528.
#' 
#' Wu, G., & Gonzalez, R. (1996). Curvature of the probability weighting function. Management Science, 42(12), 1676-1690.
#' 
#' Wakker, P. P. (2010). Prospect theory: For risk and ambiguity. Cambridge, UK: Cambridge University Press.
#' 
#' Stott, H. P. (2006). Cumulative prospect theory's functional menagerie. Journal of Risk and Uncertainty, 32(2), 101-130.
#' 
#' @examples
#' 
#' # This example creates a linear in log odds 
#' # probability weighting function.
#' 
#' linear_in_log_odds_prob_weight <-
#'	ProbWeight(fun="linear_in_log_odds",
#'	par=c(alpha=0.61, beta=0.724))
#' 
#' # These examples create the probability weighting functions
#' # used by Tversky and Kahneman (1992).
#' 
#' tk_1992_positive_prob_weight <-
#'	ProbWeight(fun="Tversky_Kahneman_1992",
#'	par=c(alpha=0.61))
#'	
#' tk_1992_negative_prob_weight <-
#'	ProbWeight(fun="Tversky_Kahneman_1992",
#'	par=c(alpha=0.69))
#'
#' @export
ProbWeight <- function(fun, par)
{

	if (!missing(par))
	{
		new(Class = "ProbWeight",
			fun = fun,
			par = par
		)		
	}
	else
	{
		new(Class = "ProbWeight",
			fun = fun
		)			
	}

}

# declare a custom function to retrieve value
setGeneric(name = "get_probability_function",
	def = function(object)
	{
		standardGeneric("get_probability_function")
	}
)

# provide implementation of custom function to retrieve value
setMethod(f = "get_probability_function",
	signature = "ProbWeight",
	definition = function(object)
	{
		return (object@fun)
	}
)

# declare a custom function to retrieve the number of parameters associated with the pwf object
setGeneric(name = "get_number_of_parameters",
	def = function(object)
	{
		standardGeneric("get_number_of_parameters")
	}
)

# provide implementation of custom function
setMethod(f = "get_number_of_parameters",
	signature = "ProbWeight",
	definition = function(object)
	{
		return (length(object@par))
	}
)

# declare a custom function
setGeneric(name = "compute_prob_weight",
	def = function(object, ...)
	{
		standardGeneric("compute_prob_weight")
	}
)

# provide implementation of custom function
setMethod(f = "compute_prob_weight",
	signature = "ProbWeight",
	definition = function(object, probability)
	{
		
		if (get_probability_function(object) == "linear")
		{
			weight <- linear_pwf(p=probability)
		}
		else if (get_probability_function(object) == "Tversky_Kahneman_1992")
		{
			weight <- kt_pwf(par=object@par, p=probability)
		}
		else if (get_probability_function(object) == "linear_in_log_odds")
		{
			weight <- linear_in_log_odds_pwf(par=object@par, p=probability)
		}
		else if (get_probability_function(object) == "power")
		{
			weight <- power_pwf(par=object@par, p=probability)			
		}				
		else if (get_probability_function(object) == "neo_additive")
		{	
			weight <- neo_additive_pwf(par=object@par, p=probability)				
		}
		else if (get_probability_function(object) == "exponential_power")
		{
			weight <- exponential_power_pwf(par=object@par, p=probability)			
		}		
		else if (get_probability_function(object) == "hyperbolic_logarithm")
		{
			weight <- hyperbolic_logarithm_pwf(par=object@par, p=probability)
		}
		else if (get_probability_function(object) == "compound_invariance")
		{
			weight <- compound_invariance_pwf(par=object@par, p=probability)
		}
		else if (get_probability_function(object) == "constant_relative_sensitivity")
		{
			weight <- constant_relative_sensitivity_pwf(par=object@par, p=probability)
		}					
		
	
		return (weight)	
	}
) 