########################
#
# Utility, a S4 class
#
########################	

#' The Utility class.
#' 
#' The Utility class stores both the functional form and parameter specifications for a utility function.
#' 
#' @section Slots:
#'  \describe{
#'    \item{\code{fun}:}{Object of class \code{"text"}, containing a text string that specifies the functional form of the utility function.}
#'    \item{\code{par}:}{Object of class \code{"vector"}, containing the parameter specifications for the utility function.}
#'  }
#'
#' @note A wrapper function (also called Utility) can be used to create an instance of this class.
#' @seealso {\code{\link{Utility}}}
#' @name Utility-class
#' @aliases Utility-class
#' @rdname Utility-class
#' @exportClass Utility
setClass(Class = "Utility",
	representation = representation
	(
		fun = "character",
		par = "vector"
	),
	# check for input consistency when creating new Utility objects using "new" constructor
	validity = function(object)
	{
		if (get_utility_function(object) == "linear")
		{
			if (get_number_of_utility_parameters(object) != 1)
			{
				stop(paste(get_utility_function(object), " utility function requires 1 parameter.\n", sep = ""))
			}
		}
		else if (get_utility_function(object) == "power")
		{
			if (get_number_of_utility_parameters(object) != 3)
			{
				stop(paste(get_utility_function(object), " utility function requires 3 parameters.\n", sep = ""))
			}
		}
		else if (get_utility_function(object) == "exponential")
		{
			if (get_number_of_utility_parameters(object) != 2)
			{
				stop(paste(get_utility_function(object), " utility function requires 2 parameters.\n", sep = ""))
			}				
		}
		else if (get_utility_function(object) == "normalized_exponential")
		{
			if (get_number_of_utility_parameters(object) != 3)
			{
				stop(paste(get_utility_function(object), " utility function requires 3 parameters.\n", sep = ""))
			}				
		}
		else if (get_utility_function(object) == "normalized_logarithmic")
		{
			if (get_number_of_utility_parameters(object) != 3)
			{
				stop(paste(get_utility_function(object), " utility function requires 3 parameters.\n", sep = ""))
			}				
		}
		else if (get_utility_function(object) == "normalized_power")
		{
			if (get_number_of_utility_parameters(object) != 3)
			{
				stop(paste(get_utility_function(object), " utility function requires 3 parameters.\n", sep = ""))
			}				
		}
		else if (get_utility_function(object) == "quadratic")
		{
			if (get_number_of_utility_parameters(object) != 3)
			{
				stop(paste(get_utility_function(object), " utility function requires 3 parameters.\n", sep = ""))
			}				
		}		
		else if (get_utility_function(object) == "logarithmic")
		{
			if (get_number_of_utility_parameters(object) != 2)
			{
				stop(paste(get_utility_function(object), " utility function requires 2 parameters.\n", sep = ""))
			}
		}			
		else if (get_utility_function(object) == "expo_power")
		{
			if (get_number_of_utility_parameters(object) != 4)
			{
				stop(paste(get_utility_function(object), " utility function requires 4 parameters.\n", sep = ""))
			}				
		}		
		else if (get_utility_function(object) == "general_linear")
		{
			if (get_number_of_utility_parameters(object) != 3)
			{
				stop(paste(get_utility_function(object), " utility function requires 3 parameters.\n", sep = ""))
			}				
		}
		else if (get_utility_function(object) == "general_power")
		{
			if (get_number_of_utility_parameters(object) != 5)
			{
				stop(paste(get_utility_function(object), " utility function requires 5 parameters.\n", sep = ""))
			}				
		}		
		return (TRUE)
	}
)


#' This function creates an instance of a Utility class object.
#' Two arguments need to be provided to create this object. The
#' first argument is a text string that defines the functional
#' form of a utility function stored by the Utility class object. 
#' The second argument is a vector of parameters needed for the
#' selected utility function. 
#' 
#' @name Utility
#' @title Create an instance of a Utility class.
#' @rdname Utility
#' @aliases Utility
#' @description This function creates an instance of a Utility class.
#' @details The following functional forms are currently implemented:
#' 
#' linear (requires 1 parameter)
#' 
#' power (requires 3 parameters)
#' 
#' exponential (requires 2 parameters)
#' 
#' normalized_exponential_uf (requires 3 parameters)
#' 
#' normalized_logarithmic_uf (requires 3 parameters)
#' 
#' normalized_power_uf (requires 3 parameters)
#' 
#' quadratic_uf (requires 3 parameters)
#' 
#' logarithmic_uf (requires 3 parameters)
#' 
#' expo_power_uf (requires 4 parameters)
#' 
#' general_linear_uf (requires 3 parameters)
#' 
#' general_power_uf (requires 5 parameters)
#' 
#' @usage Utility(fun, par)
#' @param fun text, a string selecting the utility function 
#' @param par vector, parameters for the utility function
#' @examples
#' 
#' # This example creates the power utility function with parameters
#' # used in the Tversky & Kahneman (1992) paper.
#' 
#' tk_1992_utility <- Utility(fun="power",
#'	par=c(alpha=0.88, beta=0.88, lambda=2.25))
#'
#' # This example creates a linear utility function.
#' 
#' my_linear_utility <- Utility(fun="linear",
#' 	par=c(lambda=1))
#' 
#' @references
#' Tversky, A., & Kahneman, D. (1992). Advances in prospect theory: Cumulative representation of uncertainty. Journal of Risk and Uncertainty, 5(4), 297-323.
#' 
#' Wakker, P. P. (2010). Prospect theory: For risk and ambiguity. Cambridge, UK: Cambridge University Press.
#' 
#' Stott, H. P. (2006). Cumulative prospect theory's functional menagerie. Journal of Risk and Uncertainty, 32(2), 101-130.
#' 
#' Birnbaum, M. H. (2008). New paradoxes of risky decision making. Psychological Review, 115(2), 463-501.
#' 
#' @export
Utility <- function(fun, par)
{

	new(Class = "Utility",
		fun = fun,
		par = par
	)
}

# declare a custom function to retrieve the number of parameters associated with the utility object
setGeneric(name = "get_number_of_utility_parameters",
	def = function(object)
	{
		standardGeneric("get_number_of_utility_parameters")
	}
)

# provide implementation of custom function
setMethod(f = "get_number_of_utility_parameters",
	signature = "Utility",
	definition = function(object)
	{
		return (length(object@par))
	}
)

# declare a custom function to retrieve the utility function character text string
setGeneric(name = "get_utility_function",
	def = function(object)
	{
		standardGeneric("get_utility_function")
	}
)

# provide implementation of custom function
setMethod(f = "get_utility_function",
	signature = "Utility",
	definition = function(object)
	{
		return (object@fun)
	}
)

# declare a custom function to compute the utility, given a utility character string and associated parameters
setGeneric(name = "compute_utility",
	def = function(object, ...)
	{
		standardGeneric("compute_utility")
	}
)

# provide implementation of custom function
setMethod(f = "compute_utility",
	signature = "Utility",
	definition = function(object, objective_consequence)
	{
		
		if (get_utility_function(object) == "linear")
		{
			lambda <- object@par[1]			
			
			if (objective_consequence > 0)
			{		
				utility <- objective_consequence
			}
			else if (objective_consequence == 0)
			{
					utility <- 0				
			}			
			else if (objective_consequence < 0)
			{				
				utility <- -lambda * -objective_consequence			
			}
		}
		else if (get_utility_function(object) == "power")
		{
			# implements Wakker (2010), p.78, Eqn 3.5.1
			# and Birnbaum (2008), p.466
			# u(-x) = -lambda * u(x), x >= 0
			
			alpha <- object@par[1]
			beta <- object@par[2]			
			lambda <- object@par[3]
			
		
			if (objective_consequence > 0)
			{
				if (alpha > 0)
				{
					utility <- objective_consequence^alpha			
				}
				else if (alpha == 0)
				{
					utility <- log(objective_consequence)
				}
				else if (alpha < 0)
				{
					utility <- 1 - (1 + objective_consequence)^alpha
				}				

			}
			else if (objective_consequence == 0)
			{
					utility <- 0				
			}
			else if (objective_consequence < 0)
			{
				if (beta > 0)
				{
					utility <- -lambda * (-objective_consequence)^beta			
				}
				else if (beta == 0)
				{
					utility <- -lambda * log(-objective_consequence)
				}
				else if (beta < 0)
				{
					utility <- -lambda * (1.0 - (1.0 - objective_consequence)^beta)
				}				
			}
		}		
		else if (get_utility_function(object) == "exponential")
		{
			# implements Wakker (2010), p.80, Eqn 3.5.4			
			
			alpha <- object@par[1]	
			lambda <- object@par[2]	
			
			if (objective_consequence >= 0)
			{
				if (alpha > 0)
				{
					utility <- (1 - exp(-alpha * objective_consequence))					
				}
				else if (alpha == 0)
				{
					utility <- objective_consequence
				}
				else if (alpha < 0)
				{
					utility <- (exp(-alpha * objective_consequence) - 1)						
				}
			}
			else if (objective_consequence < 0)
			{
				if (alpha > 0)
				{
					utility <- lambda * (1 - exp(-alpha * objective_consequence))					
				}
				else if (alpha == 0)
				{
					utility <- lambda * objective_consequence
				}
				else if (alpha < 0)
				{
					utility <- lambda * (exp(-alpha * objective_consequence) - 1)						
				}				
			}
		
		}
		else if (get_utility_function(object) == "normalized_exponential")
		{
			
			alpha <- object@par[1]
			beta <- object@par[2]				
			lambda <- object@par[3]			
			
			if (objective_consequence >= 0)
			{		
				utility <- (1/alpha) * ( 1- exp(-alpha * objective_consequence))
			}
			else if (objective_consequence < 0)
			{				
				utility <- (-lambda/beta) * (1 - exp(-beta * (-objective_consequence)))
			}	
		}
		else if (get_utility_function(object) == "normalized_logarithmic")
		{
			alpha <- object@par[1]
			beta <- object@par[2]				
			lambda <- object@par[3]			
			
			if (objective_consequence >= 0)
			{		
				utility <- (1/alpha) * log( 1 + alpha * objective_consequence)
			}
			else if (objective_consequence < 0)
			{				
				utility <- (-lambda/beta) * log(1 + beta * (-objective_consequence))
			}				
		}
		else if (get_utility_function(object) == "normalized_power")
		{
			alpha <- object@par[1]
			beta <- object@par[2]				
			lambda <- object@par[3]			
			
			if (objective_consequence >= 0)
			{		
				utility <- (1/alpha) * log( 1 + alpha * objective_consequence)
			}
			else if (objective_consequence < 0)
			{				
				utility <- (-lambda/beta) * log(1 + beta * (-objective_consequence))
			}			
		}
		else if (get_utility_function(object) == "quadratic")
		{
			alpha <- object@par[1]
			beta <- object@par[2]
			lambda <- object@par[3]			
			
			if (objective_consequence >= 0)
			{			
				utility <- alpha * objective_consequence - objective_consequence^2
			}
			else if (objective_consequence < 0)
			{
				utility <- -lambda * (beta * (-objective_consequence) - (-objective_consequence)^2)
			}
		}
		else if (get_utility_function(object) == "logarithmic")
		{
			alpha <- object@par[1]
			beta <- object@par[2]
			lambda <- object@par[3]	
			
			if (objective_consequence >= 0)
			{			
				utility <- log(alpha + objective_consequence)
			}
			else if (objective_consequence < 0)
			{
				utility <- -lambda * (log(beta + (-objective_consequence)))
			}
		}		
		else if (get_utility_function(object) == "expo_power")
		{
			alpha <- object@par[1]
			beta <- object@par[2]
			gamma <- object@par[3]
			lambda <- object@par[4]			
			
			if (objective_consequence >= 0)
			{		
				utility <- gamma - exp(-beta * objective_consequence^alpha)
			}
			else if (objective_consequence < 0)
			{				
				utility <- -lambda * gamma - exp(-beta * (-objective_consequence)^alpha)
			}			
		}
		else if (get_utility_function(object) == "general_linear")
		{
			
			alpha <- object@par[1]
			beta <- object@par[2]			
			lambda <- object@par[3]			
			
			if (objective_consequence >= 0)
			{		
				utility <- alpha * objective_consequence
			}
			else if (objective_consequence < 0)
			{				
				utility <- -lambda * beta * (-objective_consequence)
			}	
		}
		else if (get_utility_function(object) == "general_power")
		{
			alpha <- object@par[1]				
			beta <- object@par[2]
			gamma <- object@par[3]
			delta <- object@par[4]			
			lambda <- object@par[5]			
			
			if (objective_consequence >= 0)
			{			
				utility <- (beta * objective_consequence^alpha)
			}
			else if (objective_consequence < 0)
			{
				utility <- -lambda * (delta * -objective_consequence)^gamma
			}
		}
		
		return (utility)	
	}
)

# declare a custom function
setGeneric(name = "compute_certainty_equivalent",
	def = function(object, ...)
	{
		standardGeneric("compute_certainty_equivalent")
	}
)

# provide implementation of custom function
setMethod(f = "compute_certainty_equivalent",
	signature = "Utility",
	definition = function(object, utility)
	{
	
		certainty_equivalent <- 0.0
		
		if (get_utility_function(object) == "linear")
		{
			lambda <- object@par[1]			
			
			if (utility > 0)
			{			
				certainty_equivalent <- utility
			}
			else if (utility == 0)
			{
				certainty_equivalent <- 0				
			}			
			else if (utility < 0)
			{
				certainty_equivalent <- (-utility / -lambda)
			}			
		}
		else if (get_utility_function(object) == "power")
		{
			alpha <- object@par[1]
			beta <- object@par[2]			
			lambda <- object@par[3]			
			
			if (utility > 0)
			{
				if (alpha > 0)
				{
					certainty_equivalent <- utility^(1 / alpha)			
				}
				else if (alpha == 0)
				{
					certainty_equivalent <- exp(utility)
				}
				else if (alpha < 0)
				{
					certainty_equivalent <- (1 - utility)^(1 / alpha) - 1.0
				}					
			}
			else if (utility == 0)
			{
				certainty_equivalent <- 0
			}
			else if (utility < 0)
			{
				if (beta > 0)
				{
					certainty_equivalent <- -1.0 * (-utility / lambda)^(1 / beta)			
				}
				else if (beta == 0)
				{
					certainty_equivalent <- -1.0 * exp(-utility / lambda)
				}
				else if (beta < 0)
				{
					certainty_equivalent <- (1 - (utility/lambda + 1)^(1 / beta))
				}			
			}
		}	
		else if (get_utility_function(object) == "exponential")
		{
			alpha <- object@par[1]			
			lambda <- object@par[2]	
			
			if (utility >= 0)
			{
				if (alpha > 0)
				{
					certainty_equivalent <- (-1 / alpha) * log(1 - utility)					
				}
				else if (alpha == 0)
				{
					certainty_equivalent <- utility
				}
				else if (alpha < 0)
				{
					certainty_equivalent <- (-1 / alpha) * log(utility + 1)					
				}
			}
			else
			{
				if (alpha > 0)
				{
					certainty_equivalent <- (-1 / alpha) * log(1 + utility/lambda)					
				}
				else if (alpha == 0)
				{
					certainty_equivalent <- -utility/lambda
				}
				else if (alpha < 0)
				{
					certainty_equivalent <- (-1 / alpha) * log(-utility/lambda + 1)					
				}
			}
		}
		else if (get_utility_function(object) == "normalized_exponential")
		{
			alpha <- object@par[1]
			beta <- object@par[2]
			lambda <- object@par[3]			
			
			if (utility >= 0)
			{		
				certainty_equivalent <- (-1/alpha) * log(1 - alpha * utility)
			}
			else if (utility < 0)
			{				
				certainty_equivalent <- (1/beta) * log(1 + (beta/lambda) * utility)	
			}					
		}
		else if (get_utility_function(object) == "normalized_logarithmic")
		{
			alpha <- object@par[1]
			beta <- object@par[2]
			lambda <- object@par[3]			
			
			if (utility >= 0)
			{		
				certainty_equivalent <- (1/alpha) * exp(alpha * utility - 1)
			}
			else if (utility < 0)
			{				
				certainty_equivalent <- (-1/beta) * (1 - exp((-beta/lambda) * utility))	
			}			
		}
		else if (get_utility_function(object) == "normalized_power")
		{
			alpha <- object@par[1]
			beta <- object@par[2]
			lambda <- object@par[3]			
			
			if (utility >= 0)
			{		
				certainty_equivalent <- ((1 + alpha) * utility)^(1 + alpha)
			}
			else if (utility < 0)
			{				
				certainty_equivalent <- -(-(1 + beta) * utility / lambda)^(1 + beta)	
			}			
		}
		else if (get_utility_function(object) == "quadratic")
		{
			alpha <- object@par[1]
			beta <- object@par[2]
			lambda <- object@par[3]			
			
			# take positive root of quadratic equation
			if (utility >= 0)
			{	
				certainty_equivalent <- (alpha + sqrt((-alpha)^2 - 4*utility))/2
			}
			else if (utility < 0)
			{				
				certainty_equivalent <- (lambda * beta + sqrt((-lambda*beta)^2 - 4*utility))/2	
			}	
		}
		else if (get_utility_function(object) == "logarithmic")
		{
			alpha <- object@par[1]
			beta <- object@par[2]
			lambda <- object@par[3]			
			
			if (utility >= 0)
			{	
				certainty_equivalent <- exp(utility) - alpha
			}
			else if (utility < 0)
			{				
				certainty_equivalent <- beta - exp(-utility/lambda)	
			}	
		}
		else if (get_utility_function(object) == "expo_power")
		{
			alpha <- object@par[1]
			beta <- object@par[2]
			gamma <- object@par[2]			
			lambda <- object@par[4]			
			
			if (utility >= 0)
			{		
				certainty_equivalent <- ((-1/beta) * log(gamma - utility))^(1/alpha)
			}
			else if (utility < 0)
			{				
				certainty_equivalent <- -((-1/beta)*log(gamma + utility/lambda))^(1/alpha)	
			}			
		}
		else if (get_utility_function(object) == "general_linear")
		{
			alpha <- object@par[1]
			beta <- object@par[2]
			lambda <- object@par[3]			
			
			if (utility >= 0)
			{		
				certainty_equivalent <- utility / alpha
			}
			else if (utility < 0)
			{				
				certainty_equivalent <- utility/(lambda * beta)			
			}			
		}	
		else if (get_utility_function(object) == "general_power")
		{
			if (utility >= 0)
			{
				alpha <- object@par[1]
				beta <- object@par[2]
				certainty_equivalent <- (utility / beta)^(1.0 / alpha)				
			}
			else if (utility < 0)
			{
				alpha <- object@par[1]				
				beta <- object@par[2]
				gamma <- object@par[3]
				delta <- object@par[4]				
				lambda <- object@par[5]
				certainty_equivalent <- -(-utility / (delta * lambda))^(1.0 / gamma)		
			}
		}
		
		return (certainty_equivalent)
	}
)