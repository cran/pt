########################
#
# routines to compute and plot utility functions and probability weighting functions
#
########################


########################
#
#
# probability weighting functions
#
#
########################	

#' @name linear_pwf
#' @title The linear probability weighting function.
#' @description Linear probability weighting function.
#' @param p numeric, the probability
#' @seealso {\code{\link{plotProbW}}}
#' @export
linear_pwf <- function(p)
{
	return (p)
}

#' @name kt_pwf
#' @title The Tversky and Kahneman (1992) probability weighting function.
#' @description Tversky and Kahneman's (1992) probability weighting function is given by
#' 
#' w(p) = p^alpha / ((p^alpha + (1 - p)^alpha)^(1/alpha))
#' 
#' where p is the probability constrained by
#' 
#' w(0) = 0, w(1) = 1, 0 < p < 1,
#' 
#' and alpha is the single parameter for the function.
#' alpha >= 0.28 as the function is not strictly increasing for alpha < 0.28.
#' 
#' @references
#' 
#' Tversky, A., & Kahneman, D. (1992). Advances in prospect theory: Cumulative representation of uncertainty. Journal of Risk and Uncertainty, 5(4), 297-323.
#' 
#' p. 206, Wakker, P. P. (2010). Prospect theory: For risk and ambiguity. Cambridge, UK: Cambridge University Press.
#' 
#' 
#' @param par vector, contains the alpha parameter for the pwf
#' @param p numeric, the probability
#' @seealso {\code{\link{plotProbW}, \link{plotOneParProbWFam}}}
#' @export
kt_pwf <- function(par, p)
{
	if (length(par) == 1)
	{
		alpha <- par[1]
	
		if (alpha >= 0.28)
		{
			return (p^alpha / ((p^alpha + (1 - p)^alpha)^(1/alpha)))
		}
		else
		{
			stop("alpha needs to satisfy: alpha >= 0.28.")			
		}		
	}
	else
	{
		stop("kt_pwf should have one parameter.")
	}
}

#' @name linear_in_log_odds_pwf
#' @title The linear in log odds probability weighting function.
#' @description The linear in log odds probability weighting function is given by
#' 
#' w(p) = beta * p^alpha / (beta * p^alpha + (1 - p)^alpha),
#' 
#' where p is the probability constrained by
#' 
#' w(0) = 0, w(1) = 1, 0 < p < 1.
#' 
#' @references
#' 
#' p. 139, Gonzalez, R., & Wu, G. (1999). On the shape of the probability weighting function. Cognitive Psychology, 38, 129-166.
#'
#' p. 208, Wakker, P. P. (2010). Prospect theory: For risk and ambiguity. Cambridge, UK: Cambridge University Press.
#' 
#' @param par vector, contains the alpha and beta parameters for the pwf
#' @param p numeric, the probability
#' @seealso {\code{\link{plotProbW}, \link{plotTwoParProbWFam}}}
#' @export
linear_in_log_odds_pwf <- function(par, p)
{
	if (length(par) == 2)
	{	
		alpha <- par[1]
		beta <- par[2]
		
		return (beta * p^alpha / (beta * p^alpha + (1 - p)^alpha))
	}
	else
	{
		stop("linear_in_log_odds_pwf should have two parameters.")
	}
}

#' @name power_pwf
#' @title The power probability weighting function.
#' @description The power probability weighting function is given by
#' 
#' w(p) = beta * p^alpha,
#' 
#' where p is the probability constrained by
#' 
#' w(0) = 0, w(1) = 1, 0 < p < 1.
#' 
#' @references
#' 
#' Stott, H. P. (2006). Cumulative prospect theory's functional menagerie. Journal of Risk and Uncertainty, 32(2), 101-130.
#' 
#' @param par vector, contains the alpha and beta parameters for the pwf
#' @param p numeric, the probability
#' @seealso {\code{\link{plotProbW}, \link{plotTwoParProbWFam}}}
#' @export
power_pwf <- function(par, p)
{
	if (length(par) == 2)
	{
		alpha <- par[1]	
		beta <- par[2]
		
		return (beta * p^alpha)		
	}
	else
	{
		stop("power_pwf should have two parameters.")
	}	
}

#' @name neo_additive_pwf
#' @title The neo-additive probability weighting function.
#' @description The neo-additive probability weighting function is given by
#' 
#' w(p) = beta + alpha * p,
#' 
#' where p is the probability constrained by
#' 
#' w(0) = 0, w(1) = 1, 0 < p < 1,
#' 
#' and the two parameters in the function alpha and beta are constrained by
#' 
#' alpha >= 0,
#' 
#' beta >= 0, and
#' 
#' alpha + beta <= 1.
#' 
#' @references
#' 
#' Eqn. 7.2.5, p. 208-209 Wakker, P. P. (2010). Prospect theory: For risk and ambiguity. Cambridge, UK: Cambridge University Press.
#' 
#' @param par vector, contains the alpha and beta parameters for the pwf
#' @param p numeric, the probability
#' @seealso {\code{\link{plotProbW}, \link{plotTwoParProbWFam}}}
#' @export
neo_additive_pwf <- function(par, p)
{	
	if (length(par) == 2)
	{	
		alpha <- par[1]
		beta <- par[2]
		
		if ((alpha >= 0) && (beta >= 0) && (alpha + beta <= 1.0))
		{
			return (beta + alpha*p)
		}
		else
		{
			stop("alpha and beta need to satisfy: alpha >= 0, beta >= 0, alpha + beta <= 1.0.")			
		}
	}
	else
	{
		stop("neo_additive_pwf should have two parameters.")
	}	
}

#' @name hyperbolic_logarithm_pwf
#' @title The hyperbolic-logarithm probability weighting function.
#' @description The hyperbolic-logarithm probability weighting function is given by
#' 
#' w(p) = (1 - alpha * log(p))^(-beta/alpha)
#' 
#' where p is the probability constrained by
#' 
#' w(0) = 0, w(1) = 1, 0 < p < 1,
#' 
#' and the two parameters alpha and beta are constrained by
#' 
#' alpha > 0, beta > 0.
#' 
#' @references
#' 
#' Prelec, D. (1998). The probability weighting function. Econometrica, 60(3), 497-528.
#' 
#' p. 176, Luce, R. D. (2001). Reduction invariance and Prelec's weighting functions. Journal of Mathematical Psychology, 45(1), 167-179.
#' 
#' Footnote 3, p. 105, Stott, H. P. (2006). Cumulative prospect theory's functional menagerie. Journal of Risk and Uncertainty, 32(2), 101-130.
#'
#' @param par vector, contains the alpha and beta parameters for the pwf
#' @param p numeric, the probability
#' @seealso {\code{\link{plotProbW}, \link{plotTwoParProbWFam}}}
#' @export
hyperbolic_logarithm_pwf <- function(par, p)
{
	if (length(par) == 2)
	{	
		alpha <- par[1]
		beta <- par[2]
		
		if ((alpha > 0) && (beta > 0))
		{
			return ((1 - alpha * log(p))^(-beta/alpha))			
		}
		else
		{
			stop("alpha and beta need to satisfy: alpha > 0, beta > 0.")			
		}
	}
	else
	{
		stop("hyperbolic_logarithm_pwf should have two parameters.")
	}		
}

#' @name exponential_power_pwf
#' @title The exponential-power probability weighting function.
#' @description The exponential-power probability weighting function is given by
#' 
#' w(p) = exp(-alpha/beta * (1-p^beta))
#' 
#' where p is the probability constrained by
#' 
#' w(0) = 0, w(1) = 1, 0 < p < 1,
#' 
#' and the two parameters alpha and beta are constrained by
#' 
#' alpha != 0, beta > 0.
#' 
#' @references
#' 
#' Prelec, D. (1998). The probability weighting function. Econometrica, 60(3), 497-528.
#' 
#' p. 176, Luce, R. D. (2001). Reduction invariance and Prelec's weighting functions. Journal of Mathematical Psychology, 45(1), 167-179.
#' 
#' Footnote 3, p. 105, Stott, H. P. (2006). Cumulative prospect theory's functional menagerie. Journal of Risk and Uncertainty, 32(2), 101-130.
#'
#' @param par vector, contains the alpha and beta parameters for the pwf
#' @param p numeric, the probability
#' @seealso {\code{\link{plotProbW}, \link{plotTwoParProbWFam}}}
#' @export
exponential_power_pwf <- function(par, p)
{
	if (length(par) == 2)
	{	
		alpha <- par[1]
		beta <- par[2]
		

		if ((alpha != 0) && (beta > 0))
		{
			return (exp(-alpha/beta * (1-p^beta)))		
		}
		else
		{
			stop("alpha and beta need to satisfy: alpha != 0, beta > 0.")			
		}		
	}
	else
	{
		stop("exponential_power_pwf should have two parameters.")
	}		
}

#' @name compound_invariance_pwf
#' @title The compound invariance probability weighting function.
#' @description The compound invariance probability weighting function is given by
#' 
#' w(p) = (exp(-beta * (-log(x))^alpha)),
#' 
#' where p is the probability constrained by
#' 
#' w(0) = 0, w(1) = 1, 0 < p < 1,
#' 
#' and the two parameters alpha and beta are constrained by
#' 
#' alpha > 0, beta > 0.
#' 
#' @references
#' 
#' Prelec, D. (1998). The probability weighting function. Econometrica, 60(3), 497-528.
#' 
#' al-Nowaihi, A., & Dhami, S. (2006). A simple derivation of Prelec's probability weighting function. Journal of Mathematical Psychology, 50(6), 521-524.
#' 
#' p. 179, 207, Wakker, P. P. (2010). Prospect theory: For risk and ambiguity. Cambridge, UK: Cambridge University Press.
#' 
#' @param par vector, contains the alpha and beta parameters for the pwf
#' @param p numeric, the probability
#' @seealso {\code{\link{plotProbW}, \link{plotTwoParProbWFam}}}
#' @export
compound_invariance_pwf <- function(par, p)
{

	if (length(par) == 2)
	{	
		alpha <- par[1]
		beta <- par[2]
		
		if ((alpha >= 0) && (beta >= 0))
		{
			return ((exp(-beta * (-log(p))^alpha)))		
		}
		else
		{
			stop("alpha and beta need to satisfy: alpha >= 0, beta >= 0.")			
		}		
	}
	else
	{
		stop("compound_invariance_pwf should have two parameters.")
	}	
}

#' @name constant_relative_sensitivity_pwf
#' @title The constant relative sensitivity probability weighting function.
#' @description Constant relative sensitivity probability weighting function is given by
#' 
#' w(p) = beta^(1-alpha)*p^alpha,
#' 
#' where p is the probability constrained by
#' 
#' w(0) = 0, w(1) = 1, 0 < p < 1,
#' 
#' and the two parameters alpha and beta are constrained by
#' 
#' alpha > 0, 0 <= beta <= 1.
#' 
#' @references
#' 
#' p. 52, Abdellaoui, M., L'Haridon, O., & Zank, H. (2010). Separating curvature and elevation: A parametric probability weighting function. Journal of Risk and Uncertainty, 41(1), 39-65.
#' 
#' @param par vector, contains the alpha and beta parameters for the probability weighting function.
#' @param p numeric, the probability
#' @seealso {\code{\link{plotProbW}, \link{plotTwoParProbWFam}}}
#' @export
constant_relative_sensitivity_pwf <- function(par, p)
{
	if (length(par) == 2)
	{	
		alpha <- par[1]
		beta <- par[2]
		
		if ((alpha > 0) && (beta >= 0) && (beta <= 1))
		{
			if (p <= beta)
			{
				return (beta^(1-alpha)*p^alpha)
			}
			else
			{
				return (1 - (1 - beta)^(1-alpha)*(1-p)^alpha)		
			}			
		}
		else
		{
			stop("alpha and beta need to satisfy: alpha >= 0, 0 <= beta <= 1.")			
		}
		

	}
	else
	{
		stop("constant_relative_sensitivity_pwf should have two parameters.")
	}	
}


########################
#
#
# utility functions
#
#
########################	

#' @name linear_uf
#' @title The linear utility function.
#' @description The linear utility function is given by
#' 
#' U(oc) = oc, if oc >= 0 and
#' 
#' U(oc) = -lambda * (-oc), if oc < 0.
#' 
#' U is the utility and oc is the objective consequence of a gamble outcome.
#' lambda is the loss aversion coefficient. The Tversky & Kahneman (1992) assumption has also been made, namely
#' 
#' U(-oc) = -lambda * U(oc) where oc >= 0.
#' 
#' @references
#' 
#' Tversky, A., & Kahneman, D. (1992). Advances in prospect theory: Cumulative representation of uncertainty. Journal of Risk and Uncertainty, 5(4), 297-323.
#' 
#' p. 466 Eqn. 2, 469, Birnbaum, M. H. (2008). New paradoxes of risky decision making. Psychological Review, 115(2), 463-501.
#' 
#' @param par vector, parameter lambda for the utility function.
#' @param oc numeric, the objective consequence.
#' @export
linear_uf <- function(par, oc)
{	
	if (length(par) == 1)
	{
		lambda <- par[1]
	
		value <- ifelse(oc < 0, -lambda * (-oc), oc)
		
		return (value)		
	}
	else
	{
		stop("linear_uf should have one parameter.")
	}	
}

#' @name power_uf
#' @title The power utility function.
#' @description The power utility function is given by
#' 
#' U(oc) = oc^alpha, if oc >= 0 and
#' 
#' U(oc) = -lambda * (-oc)^beta, if oc < 0.
#' 
#' U is the utility and oc is the objective consequence of a gamble outcome.
#' lambda is the loss aversion coefficient. The Tversky & Kahneman (1992) assumption has also been made, namely
#' 
#' U(-oc) = -lambda * U(oc) where oc >= 0.
#' 
#' @references
#' 
#' p. 309 Tversky, A., & Kahneman, D. (1992). Advances in prospect theory: Cumulative representation of uncertainty. Journal of Risk and Uncertainty, 5(4), 297-323.
#' 
#' p. 466 Eqn. 2, 469, Birnbaum, M. H. (2008). New paradoxes of risky decision making. Psychological Review, 115(2), 463-501.
#' 
#' p. 1336 Wakker, P. P. (2008). Explaining the characteristics of the power (CRRA) utility family. Health Economics, 17(12), 1329-1344.
#' 
#' @param par vector, parameters alpha, beta and lambda for the utility function.
#' @param oc numeric, the objective consequence
#' @export
power_uf <- function(par, oc)
{
	if (length(par) == 3)
	{
		alpha <- par[1]
		beta <- par[2]
		lambda <- par[3]
		
		if (alpha > 0)
		{
			if (beta > 0)
			{
				value <- ifelse(oc >= 0, oc^alpha, -lambda * (-oc)^beta)				
			}
			else if (beta == 0)
			{
				value <- ifelse(oc >= 0, oc^alpha, -lambda * log(-oc))				
			}
			else if (beta < 0)
			{
				value <- ifelse(oc >= 0, oc^alpha, -lambda * (1-(1-oc)^beta))
			}
		}
		else if (alpha == 0)
		{
			if (beta > 0)
			{			
				value <- ifelse(oc >= 0, log(oc), -lambda * (-oc)^beta)	
			}
			else if (beta == 0)
			{
				value <- ifelse(oc >= 0, log(oc), -lambda * log(-oc))
			}
			else if (beta < 0)
			{
				value <- ifelse(oc >= 0, log(oc), -lambda * (1-(1-oc)^beta))
			}
		}
		else if (alpha < 0)
		{
			if (beta > 0)
			{			
				value <- ifelse(oc >= 0, 1-(oc+1)^alpha, -lambda * (-oc)^beta)					
			}
			else if (beta == 0)
			{
				value <- ifelse(oc >= 0, 1-(oc+1)^alpha, -lambda * log(-oc))					
			}
			else if (beta < 0)
			{
				value <- ifelse(oc >= 0, 1-(oc+1)^alpha, -lambda * (1-(1-oc)^beta))					
			}
		}		

		return (value)
	}
	else
	{
		stop("power_uf should have three parameters.")
	}
}

#' @name exponential_uf
#' @title The exponential utility function.
#' @description Exponential utility function is given by
#' 
#' U(oc) = 1 - exp(-alpha * oc), if alpha > 0,
#' 
#' U(oc) = oc, if alpha == 0, and
#' 
#' U(oc) = exp(-alpha * oc) - 1, if alpha < 0.
#'
#' U is the utility and oc is the objective consequence of a gamble outcome.
#' lambda is the loss aversion coefficient. alpha is an index of concavity.
#' This function is defined on the entire real line (see Wakker, p. 80).
#' 
#' @references
#' 
#' p. 309 Tversky, A., & Kahneman, D. (1992). Advances in prospect theory: Cumulative representation of uncertainty. Journal of Risk and Uncertainty, 5(4), 297-323.
#' 
#' p. 466 Eqn. 2, 469, Birnbaum, M. H. (2008). New paradoxes of risky decision making. Psychological Review, 115(2), 463-501.
#' 
#' p. 80 Wakker, P. P. (2008). Explaining the characteristics of the power (CRRA) utility family. Health Economics, 17(12), 1329-1344.
#' 
#' @param par vector, parameter alpha for the utility function.
#' @param oc numeric, the objective consequence
#' @export
exponential_uf <- function(par, oc)
{	
	if (length(par) == 1)
	{
		alpha <- par[1]
		
		value <- ifelse(alpha > 0, (1 - exp(-alpha * oc)), ifelse(alpha < 0, exp(-alpha * oc) - 1, oc))
		
		return (value)
	}
	else
	{
		stop("exponential_uf should have one parameter.")
	}
}

#' @name normalized_exponential_uf
#' @title The normalized exponential utility function.
#' @description The normalized exponential utility function is given by
#' 
#' U(oc) = 1/alpha * (1 - exp(-alpha * oc)), if oc >= 0 and
#' 
#' U(oc) = -lambda/beta * (1-exp(-beta*(-oc))), if oc < 0.
#' 
#' U is the utility and oc is the objective consequence of a gamble outcome.
#' lambda is the loss aversion coefficient. The Tversky & Kahneman (1992) assumption has also been made, namely
#' 
#' U(-oc) = -lambda * U(oc) where oc >= 0.
#' 
#' @references
#' 
#' Scholten, M., & Read, D. (2014). Prospect theory and the ``forgotten" fourfold pattern of risk preferences. Journal of Risk and Uncertainty, DOI 10.1007/s11166-014-9183-2.
#' 
#' @param par vector, parameters alpha, beta and lambda for the utility function.
#' @param oc numeric, the objective consequence
#' @export
normalized_exponential_uf <- function(par, oc)
{	
	if (length(par) == 3)
	{
		alpha <- par[1]
		beta <- par[2]
		lambda <- par[3]
		
		value <- ifelse(oc < 0, -lambda/beta * (1-exp(-beta*(-oc))), 1/alpha * (1 - exp(-alpha * oc)))
		
		return (value)
	}
	else
	{
		stop("normalized_exponential_uf should have three parameters.")
	}
}

#' @name normalized_logarithmic_uf
#' @title The normalized logarithmic utility function.
#' @description The normalized logarithmic utility function is given by
#' 
#' U(oc) = log(1-alpha*x), if oc >= 0 and
#' 
#' U(oc) = -lambda/beta * log(1-alpha*x), if oc < 0.
#' 
#' U is the utility and oc is the objective consequence of a gamble outcome.
#' lambda is the loss aversion coefficient. The Tversky & Kahneman (1992) assumption has also been made, namely
#' 
#' U(-oc) = -lambda * U(oc) where oc >= 0.
#' 
#' @references
#' 
#' Scholten, M., & Read, D. (2014). Prospect theory and the ``forgotten" fourfold pattern of risk preferences. Journal of Risk and Uncertainty, DOI 10.1007/s11166-014-9183-2.
#' 
#' @param par vector, parameters alpha, beta and lambda for the utility function.
#' @param oc numeric, the objective consequence
#' @references
#' 
#' Scholten, M., & Read, D. (2014). Prospect theory and the ``forgotten" fourfold pattern of risk preferences. Journal of Risk and Uncertainty, DOI 10.1007/s11166-014-9183-2.
#' 
#' @export
normalized_logarithmic_uf <- function(par, oc)
{	
	if (length(par) == 3)
	{
		alpha <- par[1]
		beta <- par[2]
		lambda <- par[3]
		
		value <- ifelse(oc < 0, -lambda/beta * log(1-alpha*oc), log(1-alpha*oc))
		
		return (value)
	}
	else
	{
		stop("normalized_logarithmic_uf should have three parameters.")
	}
}

#' @name normalized_power_uf
#' @title The normalized power utility function.
#' @description The normalized power utility function is given by
#' 
#' U(oc) = ((1+alpha)*oc)^(1+alpha), if oc >= 0 and
#' 
#' U(oc) = -(-(1+beta)*oc/lambda)^(1+beta), if oc < 0.
#' 
#' U is the utility and oc is the objective consequence of a gamble outcome.
#' lambda is the loss aversion coefficient. The Tversky & Kahneman (1992) assumption has also been made, namely
#' 
#' U(-oc) = -lambda * U(oc) where oc >= 0.
#' 
#' @references
#' 
#' Scholten, M., & Read, D. (2014). Prospect theory and the ``forgotten" fourfold pattern of risk preferences. Journal of Risk and Uncertainty, DOI 10.1007/s11166-014-9183-2.
#' 
#' @param par vector, parameters alpha, beta and lambda for the utility function.
#' @param oc numeric, the objective consequence
#' @export
normalized_power_uf <- function(par, oc)
{	
	if (length(par) == 3)
	{
		alpha <- par[1]
		beta <- par[2]
		lambda <- par[3]
		
		value <- ifelse(oc < 0, -(-(1+beta)*oc/lambda)^(1+beta), ((1+alpha)*oc)^(1+alpha))
		
		return (value)
	}
	else
	{
		stop("normalized_power_uf should have three parameters.")
	}
}

#' @name quadratic_uf
#' @title The quadratic utility function.
#' @description The quadratic utility function is given by
#' 
#' U(oc) = alpha * oc - oc^2, if oc >= 0 and
#' 
#' U(oc) = -lambda * (beta * (-oc) - (-oc)^2), if oc < 0.
#' 
#' U is the utility and oc is the objective consequence of a gamble outcome.
#' lambda is the loss aversion coefficient. The Tversky & Kahneman (1992) assumption has also been made, namely
#' 
#' U(-oc) = -lambda * U(oc) where oc >= 0.
#' 
#' @param par vector, parameters alpha, beta and lambda for the utility function.
#' @param oc numeric, the objective consequence
#' @export
quadratic_uf <- function(par, oc)
{	
	if (length(par) == 3)
	{
		alpha <- par[1]
		beta <- par[2]
		lambda <- par[3]	
		
		value <- ifelse(oc < 0, -lambda * (beta * (-oc) - (-oc)^2), alpha * oc - oc^2)
		
		return (value)
	}
	else
	{
		stop("quadratic_uf should have three parameters.")
	}	
}

#' @name logarithmic_uf
#' @title The logarithmic utility function.
#' @description The logarithmic utility function is given by
#' 
#' U(oc) = log(alpha + oc), if oc >= 0 and
#' 
#' U(oc) = -lambda * (log(beta + (-oc)), if oc < 0.
#' 
#' U is the utility and oc is the objective consequence of a gamble outcome.
#' lambda is the loss aversion coefficient. The Tversky & Kahneman (1992) assumption has also been made, namely
#' 
#' U(-oc) = -lambda * U(oc) where oc >= 0.
#' 
#' @param par vector, parameters alpha and lambda for the utility function.
#' @param oc numeric, the objective consequence
#' @export
logarithmic_uf <- function(par, oc)
{	
	if (length(par) == 3)
	{
		alpha <- par[1]
		beta <- par[2]
		lambda <- par[3]	
		
		value <- ifelse(oc < 0, -lambda * (log(beta + (-oc))), log(alpha + oc))
		
		return (value)
	}
	else
	{
		stop("logarithmic_uf should have three parameters.")
	}		
}

#' @name expo_power_uf
#' @title The expo-power utility function.
#' @description The expo-power utility function is given by
#' 
#' U(oc) = gamma - exp(-beta * oc^alpha), if oc >= 0 and
#' 
#' U(oc) = -lambda * gamma - exp(-beta * (-oc)^alpha), if oc < 0.
#' 
#' U is the utility and oc is the objective consequence of a gamble outcome.
#' lambda is the loss aversion coefficient. The Tversky & Kahneman (1992) assumption has also been made, namely
#' 
#' U(-oc) = -lambda * U(oc) where oc >= 0.
#' 
#' Parameter restrictions from Saha (1993) are:
#' 
#' gamma > 1,
#' 
#' alpha != 0,
#' 
#' beta != 0, and
#' 
#' alpha * beta > 1.
#' 
#' @references
#' 
#' Saha, A. (1993). Expo-power utility: A 'flexible' form for absolute and relative risk aversion. American Journal of Agricultural Economics, 75(4), 905-913.
#' 
#' Peel, D. A., & Zhang, J. (2009). The expo-power value function as a candidate for the work-horse specification in parametric versions of cumulative prospect theory. Economics Letters, 105(3), 326-329.
#' 
#' @param par vector, parameters alpha, beta, gamma and lambda for the utility function.
#' @param oc numeric, the objective consequence
#' @export
expo_power_uf <- function(par, oc)
{
	if (length(par) == 4)
	{
		alpha <- par[1]
		beta <- par[2]
		gamma <- par[3]
		lambda <- par[4]
		
		value <- ifelse(oc < 0, -lambda * gamma - exp(-beta * (-oc)^alpha), gamma - exp(-beta * oc^alpha))
		
		return (value)
	}
	else
	{
		stop("expo_power_uf should have four parameters.")
	}
}

#' @name general_linear_uf
#' @title The general linear utility function.
#' @description The general linear utility function is given by
#' 
#' U(oc) = alpha * oc, if oc >= 0 and
#' 
#' U(oc) = -lambda * (beta * -oc), if oc < 0.
#' 
#' U is the utility and oc is the objective consequence of a gamble outcome.
#' lambda is the loss aversion coefficient. The Tversky & Kahneman (1992) assumption has also been made, namely
#' 
#' U(-oc) = -lambda * U(oc) where oc >= 0.
#' 
#' @references
#' 
#' Tversky, A., & Kahneman, D. (1992). Advances in prospect theory: Cumulative representation of uncertainty. Journal of Risk and Uncertainty, 5(4), 297-323.
#' 
#' p. 466 Eqn. 2, 469, Birnbaum, M. H. (2008). New paradoxes of risky decision making. Psychological Review, 115(2), 463-501.
#' 
#' @param par vector, parameter lambda for the utility function.
#' @param oc numeric, the objective consequence.
#' @export
general_linear_uf <- function(par, oc)
{	
	if (length(par) == 3)
	{
		alpha <- par[1]
		beta <- par[2]		
		lambda <- par[3]
	
		value <- ifelse(oc < 0, -lambda * (beta * -oc), alpha * oc)
		
		return (value)		
	}
	else
	{
		stop("general_linear_uf should have three parameters.")
	}	
}

#' @name general_power_uf
#' @title The general power utility function.
#' @description The general power utility function is given by
#' 
#' U(oc) = beta * oc^alpha, if oc >= 0 and
#' 
#' U(oc) = -lambda * (delta * -oc)^gamma, if oc < 0.
#' 
#' U is the utility and oc is the objective consequence of a gamble outcome.
#' lambda is the loss aversion coefficient. The Tversky & Kahneman (1992) assumption has also been made, namely
#' 
#' U(-oc) = -lambda * U(oc) where oc >= 0.
#'
#' @references
#' 
#' p. 309 Tversky, A., & Kahneman, D. (1992). Advances in prospect theory: Cumulative representation of uncertainty. Journal of Risk and Uncertainty, 5(4), 297-323.
#' 
#' p. 466 Eqn. 2, 469, Birnbaum, M. H. (2008). New paradoxes of risky decision making. Psychological Review, 115(2), 463-501.
#' 
#' @param par vector, parameters alpha, beta and lambda for the utility function.
#' @param oc numeric, the objective consequence
#' @export
general_power_uf <- function(par, oc)
{
	if (length(par) == 5)
	{
		alpha <- par[1]
		beta <- par[2]
		gamma <- par[3]
		delta <- par[4]		
		lambda <- par[5]

		value <- ifelse(oc < 0, -lambda * (delta * -oc)^gamma, beta * oc^alpha)
		
		return (value)
	}
	else
	{
		stop("general_power_uf should have five parameters.")
	}
}

########################
#
#
# plot functions
#
#
########################	

#' @name plotProbW
#' @title Plot a probability weighting function.
#' @description Plot a probability weighting function using base graphics.
#' @param my_title text, the title
#' @param my_title_colour text, the title colour
#' @param my_title_font_size numeric, the title font size
#' @param my_x_label text, my_x_label
#' @param my_y_label text, the my_y_label
#' @param pwf function, the pwf
#' @param par vector, the pwf_parameters
#' @param draw_reference_line_flag logical, draw_reference_line_flag
#' @param reference_line_colour text, reference_line_colour
#' @param reference_line_style text, reference_line_style
#' @param my_labels vector, labels
#' @param my_label_positions vector, the coordinates for the labels
#' @param font_scaling numeric, the scaling factor for the labels
#' @param arrow_positions vector, the positions of arrow lines
#' @examples
#' 
#' plotProbW(my_title=expression(paste("Kahneman & Tversky (1992), ",
#' 	c==0.61)),
#' 	my_title_colour="black", my_title_font_size=4,
#' 	my_x_label = "p", my_y_label = "w(p)",
#' 	pwf=kt_pwf, par=c(c=0.61),
#' 	draw_reference_line_flag=TRUE, reference_line_colour="red",
#' 	reference_line_style="dotted",
#' 	my_labels=c(expression(paste(w(italic(p)) == frac(italic(p)^c,
#' 	(italic(p)^c + (1-italic(p))^c)^(1/c))))),
#' 	my_label_positions=list(c(0.4,0.8)),
#' 	font_scaling=1.0)
#' 	
#' @export
plotProbW <- function(my_title, my_title_colour, my_title_font_size,
	my_x_label, my_y_label, 
	pwf, par,
	draw_reference_line_flag, reference_line_colour, reference_line_style, 
	my_labels, my_label_positions, font_scaling, arrow_positions)
{	
	
	x=NULL # put this here to avoid a R cmd check NOTE: no visible binding for global variable 'x'	
		
	FUN <- match.fun(pwf)

	# plot axes
	plot(x = 0:1,
		y = 0:1,
		lty = 1,
		xaxp = c(0, 1, 10),
		yaxp = c(0, 1, 10),		
		xaxs = "i",	#let the x and y axes intersect at the origin
		yaxs = "i",
		bty = "n",
		xlab = my_x_label, 
		ylab = my_y_label,
		pty = "s")		
	
	curve(FUN(par=par, x),
		from = 0, 
		to = 1, 
		n = 1000,
		xlim = c(0, 1), 
		ylim = c(0, 1),
		xaxs = "i",	# let the x and y axes intersect at the origin
		yaxs = "i",	
		xaxp = c(0, 1, 10),	# specify x axis intervals from 0 to 1 by 0.1
		yaxp = c(0, 1, 10),	# specify y axis intervals from 0 to 1 by 0.1	
		col = "Black",
		add = TRUE,
		asp = 1)	# set 1-1 aspect ratio	
	
	# draw grey axis
	abline(h = 0, v = 0, col = "black")

	# these need to come after the draw curve 
	
	if (draw_reference_line_flag == TRUE)
	{
		# draw a reference line	
		abline(a = 0, b = 1, lty = reference_line_style, col = reference_line_colour)
	}
		
	# draw text labels for selected lines to improve clarity
	
	if (!missing(my_labels))
	{
		for(index in 1:length(my_labels))
		{
			position <- my_label_positions[[index]]
			
			position_x <- position[1]
			position_y <- position[2]		
			
			text(x=position_x, y=position_y, labels = my_labels[index], col="black", cex = font_scaling)		
		}		
	}
	
	if (!missing(arrow_positions))
	{
		for(index in 1:length(arrow_positions))
		{
			position <- arrow_positions[[index]]
			
			start_position_x <- position[1]
			start_position_y <- position[2]		
			end_position_x <- position[3]
			end_position_y <- position[4]	
			
			grid::grid.move.to(x = start_position_x, y = start_position_y, default.units = "npc", name = NULL, draw = TRUE, vp = NULL)
			grid::grid.line.to(x = end_position_x, y = end_position_y, default.units = "npc",
				arrow=grid::arrow(angle = 30, length = grid::unit(0.01, "npc"),
      			ends = "last", type = "open"), name = NULL,
             		gp = grid::gpar(), draw = TRUE, vp = NULL)			
		}	
	}	

	if (!missing(my_title))
	{
		# title needs to come *after* the plot
		title(main = my_title, col.main = my_title_colour, font.main = my_title_font_size)
	}
	
	return (invisible())
}

#' @name plotOneParProbWFam
#' @title Plot a family of one parameter probability weighting functions.
#' @description Plot a family of one parameter probability weighting functions using base graphics.
#' @param my_title text, the title
#' @param my_title_colour text, the title colour
#' @param my_title_font_size numeric, the title font size
#' @param my_x_label text, my_x_label
#' @param my_y_label text, the my_y_label
#' @param pwf function, the pwf
#' @param par vector, the pwf_parameters
#' @param draw_reference_line_flag logical, draw_reference_line_flag
#' @param reference_line_colour text, reference_line_colour
#' @param reference_line_style text, reference_line_style
#' @param my_labels vector, labels
#' @param my_label_positions vector, the coordinates for the labels
#' @param font_scaling numeric, the scaling factor for the labels
#' @param arrow_positions vector, the positions of arrow lines
#' @examples
#' 
#'plotOneParProbWFam(my_title="Tversky & Kahneman (1992) family",
#'	my_title_colour="black", my_title_font_size=4,
#'	my_x_label = "p", my_y_label = "w(p)", pwf=kt_pwf,
#'	par=c(0.3, 0.61, 0.8, 1.0, 1.3),
#'	draw_reference_line_flag=TRUE, reference_line_colour="red",
#'	reference_line_style="dotted",
#'	my_labels=c(expression(paste(alpha == 0.3)),
#'	expression(paste(alpha == 0.61)),
#'	expression(paste(alpha == 0.8)),
#'	expression(paste(alpha == 1.0)),
#'	expression(paste(alpha == 1.3)),
#'	expression(paste(w(italic(p)) == frac(italic(p)^alpha,
#'	(italic(p)^alpha + (1-italic(p))^alpha)^(1/alpha))))),
#'	my_label_positions=list(c(0.9,0.15),c(0.7,0.45),c(0.15,0.5),
#'	c(0.31,0.62),c(0.5,0.7),c(0.42, 0.9)),
#'	font_scaling=1.0,
#'	arrow_positions = list(c(0.3,0.5,0.39,0.41),c(0.42,0.58,0.52,0.51),
#'		c(0.59,0.66,0.66,0.66)))
#' 	
#' @references
#' 	
#' 	p. 207, Fig. 7.2.1 from Wakker, P. P. (2010). Prospect theory: For risk and ambiguity. Cambridge, UK: Cambridge University Press.
#' 	
#' @export
plotOneParProbWFam <- function(my_title, my_title_colour, my_title_font_size,
	my_x_label, my_y_label, 
	pwf, par, 
	draw_reference_line_flag, reference_line_colour, reference_line_style, 
	my_labels, my_label_positions, font_scaling, arrow_positions)
{	
	
	x=NULL # put this here to avoid a R cmd check NOTE: no visible binding for global variable 'x'	
	
	FUN <- match.fun(pwf)		
	
	# plot axes
	plot(x = 0:1,
		y = 0:1,
		lty = 1,
		xaxp = c(0, 1, 10),
		yaxp = c(0, 1, 10),		
		xaxs = "i",	#let the x and y axes intersect at the origin
		yaxs = "i",
		bty = "n",
		xlab = my_x_label, 
		ylab = my_y_label,
		pty = "s")
	
	for(p1 in par)
	{
		params <- c(a=p1)
		
		curve(FUN(params, x),
			from = 0, 
			to = 1, 
			n = 1000,
			xlim = c(0, 1), 
			ylim = c(0, 1),
			xaxs = "i",	# let the x and y axes intersect at the origin
			yaxs = "i",	
			xaxp = c(0, 1, 10),	# specify x axis intervals from 0 to 1 by 0.1
			yaxp = c(0, 1, 10),	# specify y axis intervals from 0 to 1 by 0.1	
			col = "Black",
			add = TRUE,
			asp = 1)	# set 1-1 aspect ratio		
	}


	# these need to come after the draw curve 
	
	if (draw_reference_line_flag == TRUE)
	{
		# draw a reference line	
		abline(a = 0, b = 1, lty = reference_line_style, col = reference_line_colour)
	}

	
	# draw text labels for selected lines to improve clarity
	
	if (!missing(my_labels))
	{
		for(index in 1:length(my_labels))
		{
			position <- my_label_positions[[index]]
			
			position_x <- position[1]
			position_y <- position[2]		
			
			text(x=position_x, y=position_y, labels = my_labels[index], col="black", cex = font_scaling)		
		}		
	}
	
	if (!missing(arrow_positions))
	{
		for(index in 1:length(arrow_positions))
		{
			position <- arrow_positions[[index]]
			
			start_position_x <- position[1]
			start_position_y <- position[2]		
			end_position_x <- position[3]
			end_position_y <- position[4]	
			
			grid::grid.move.to(x = start_position_x, y = start_position_y, default.units = "npc", name = NULL, draw = TRUE, vp = NULL)
			grid::grid.line.to(x = end_position_x, y = end_position_y, default.units = "npc",
				arrow=grid::arrow(angle = 30, length = grid::unit(0.01, "npc"),
      			ends = "last", type = "open"), name = NULL,
             		gp = grid::gpar(), draw = TRUE, vp = NULL)			
		}	
	}
	
	if (!missing(my_title))
	{
		# title needs to come *after* the plot
		title(main = my_title, col.main = my_title_colour, font.main = my_title_font_size)
	}

	return (invisible())
}

#' @name plotTwoParProbWFam
#' @title Plot a family of two parameter probability weighting functions.
#' @description Plot a family of two parameter probability weighting functions using base graphics.
#' @param my_title text, the title
#' @param my_title_colour text, the title colour
#' @param my_title_font_size numeric, the title font size
#' @param my_x_label text, my_x_label
#' @param my_y_label text, the my_y_label
#' @param pwf function, the pwf
#' @param par vector, the pwf_parameters
#' @param draw_reference_line_flag logical, draw_reference_line_flag
#' @param reference_line_colour text, reference_line_colour
#' @param reference_line_style text, reference_line_style
#' @param my_labels vector, labels
#' @param my_label_positions vector, the coordinates for the labels
#' @param font_scaling numeric, the scaling factor for the labels
#' @param arrow_positions vector, the positions of arrow lines
#' @references
#' 
#' p. 140, Fig. 4 from Gonzalez, R., & Wu, G. (1999). On the shape of the probability weighting function. Cognitive Psychology, 38, 129-166.
#' 
#' @examples
#' 
#'plotTwoParProbWFam(my_title=expression(paste("linear in log odds,  ", 
#'	gamma == 0.6)),
#'	my_title_colour="black", my_title_font_size=4,
#'	my_x_label = "p", my_y_label = "w(p)", pwf=linear_in_log_odds_pwf, 
#'	par=list(a_list=c(0.6), b_list=seq(from=0.2, to=1.8, by=0.06)),  
#'	draw_reference_line_flag=TRUE, reference_line_colour="red", 
#'	reference_line_style="dotted", 
#'	my_labels=c(expression(paste(delta == 0.2)), 
#'		expression(paste(delta == 1.8)), 
#'		expression(paste(w(italic(p)) == frac(delta * italic(p)^gamma, 
#'			delta * italic(p)^gamma + (1-italic(p))^gamma)))), 
#'	my_label_positions=list(c(0.7,0.09),c(0.2,0.6),c(0.42, 0.9)), 
#'	font_scaling=1.0, 
#'	arrow_positions = list(c(0.28,0.56,0.35,0.53),c(0.7,0.23,0.75,0.35)))
#' 	
#' @export
plotTwoParProbWFam <- function(my_title, my_title_colour, my_title_font_size,
	my_x_label, my_y_label, 
	pwf, par, 
	draw_reference_line_flag, reference_line_colour, reference_line_style, 
	my_labels, my_label_positions, font_scaling, arrow_positions)
{	
	x=NULL # put this here to avoid a R cmd check NOTE: no visible binding for global variable 'x'	
	
	FUN <- match.fun(pwf)		
	
	# plot axes
	plot(x = 0:1,
		y = 0:1,
		lty = 1,
		xaxp = c(0, 1, 10),
		yaxp = c(0, 1, 10),		
		xaxs = "i",	#let the x and y axes intersect at the origin
		yaxs = "i",
		bty = "n",
		xlab = my_x_label, 
		ylab = my_y_label,
		pty = "s")
	
	p1_list <- par[[1]]
	p2_list <- par[[2]]
	
	for(p1 in p1_list)
	{

		for(p2 in p2_list)
		{
			params <- c(a=p1, b=p2)
		
			curve(FUN(params, x),
				from = 0, 
				to = 1, 
				n = 1000,
				xlim = c(0, 1), 
				ylim = c(0, 1),
				xaxs = "i",	# let the x and y axes intersect at the origin
				yaxs = "i",	
				xaxp = c(0, 1, 10),	# specify x axis intervals from 0 to 1 by 0.1
				yaxp = c(0, 1, 10),	# specify y axis intervals from 0 to 1 by 0.1	
				col = "Black",
				add = TRUE,
				asp = 1)	# set 1-1 aspect ratio		
		}
	}


	# these need to come after the draw curve 
	
	if (draw_reference_line_flag == TRUE)
	{
		# draw a reference line	
		abline(a = 0, b = 1, lty = reference_line_style, col = reference_line_colour)
	}

	
	# draw text labels for selected lines to improve clarity
	
	if (!missing(my_labels))
	{
		for(index in 1:length(my_labels))
		{
			position <- my_label_positions[[index]]
			
			position_x <- position[1]
			position_y <- position[2]		
			
			text(x=position_x, y=position_y, labels = my_labels[index], col="black", cex = font_scaling)		
		}		
	}
	
	if (!missing(arrow_positions))
	{
		for(index in 1:length(arrow_positions))
		{
			position <- arrow_positions[[index]]
			
			start_position_x <- position[1]
			start_position_y <- position[2]		
			end_position_x <- position[3]
			end_position_y <- position[4]	
			
			grid::grid.move.to(x = start_position_x, y = start_position_y, default.units = "npc", name = NULL, draw = TRUE, vp = NULL)
			grid::grid.line.to(x = end_position_x, y = end_position_y, default.units = "npc",
				arrow=grid::arrow(angle = 30, length = grid::unit(0.01, "npc"),
      			ends = "last", type = "open"), name = NULL,
             		gp = grid::gpar(), draw = TRUE, vp = NULL)			
		}	
	}
	
	if (!missing(my_title))
	{
		# title needs to come *after* the plot
		title(main = my_title, col.main = my_title_colour, font.main = my_title_font_size)
	}

	return (invisible())
}

#' @name plotUtility
#' @title Plot a utility function.
#' @description Plot the utility function.
#' @param my_title text, the title of the chart.
#' @param my_title_colour text, the title colour.
#' @param my_title_font_size numeric, the title font size.
#' @param my_x_label text, the x-axis label.
#' @param xmin numeric, the xmin on the x-axis.
#' @param xmax numeric, the xmax on the x-axis.
#' @param my_y_label text, the y-axis label.
#' @param fun Utility, an instance of the Utility class.
#' @param par vector, the parameters for the utility function.
#' @param fun_colour text, the colour of the utility function line.
#' @param draw_reference_line_flag logical, a boolean flag determining whether or not to draw a y=x reference line.
#' @param reference_line_colour text, the reference line colour.
#' @param reference_line_style numeric, the reference line style.
#' @param my_labels vector, a vector of text labels to draw.
#' @param my_label_positions list, a list of coordinates for the text labels.
#' @param my_label_colours vector, stores the colours for each text label.
#' @param my_label_font_sizes vector, stores the font size of each text label.
#' @examples
#' 
#' plotUtility(my_x_label = "objective consequence",
#' 	my_y_label = "subjective value",
#' 	xmin = -10, xmax = 10, 
#' 	fun=power_uf,
#' 	par=c(alpha = 0.88, beta = 0.88, lambda = 2.25),
#' 	fun_colour = "purple",
#' 	draw_reference_line_flag = TRUE,
#' 	reference_line_colour = "red",
#' 	reference_line_style = 1)
#' 
#' @export
plotUtility <- function(my_title, my_title_colour, my_title_font_size, 
	my_x_label, xmin, xmax, my_y_label, 
	fun, par, 
	fun_colour, 
	draw_reference_line_flag, reference_line_colour, reference_line_style, 
	my_labels, my_label_positions, my_label_colours, my_label_font_sizes)
{

	x=NULL # put this here to avoid a R cmd check NOTE: no visible binding for global variable 'x'	
		
	FUN <- match.fun(fun)

	curve(FUN(par, x),
		from = xmin,
		to = xmax,
		add = FALSE,
		col = fun_colour,		
		type = "l",
		xlab = my_x_label,
		ylab = my_y_label,
		xaxs = "i",	# let the x and y axes intersect at the origin
		yaxs = "i",		
		n = 2000)
	
	# draw grey axis
	abline(h = 0, v = 0, col = "black")
	
	if (draw_reference_line_flag == TRUE)
	{
		# draw a reference line	
		abline(a = 0, b = 1, lty = reference_line_style, col = reference_line_colour)
	}
	
	# draw text labels for selected lines to improve clarity
	
	if (!missing(my_labels))
	{
		for(index in 1:length(my_labels))
		{
			position <- my_label_positions[[index]]
			
			position_x <- position[1]
			position_y <- position[2]		
			
			text(x=position_x, y=position_y, labels = my_labels[index], col=my_label_colours[index], cex = my_label_font_sizes[index])		
		}		
	}	
	
	if (!missing(my_title))
	{
		# title needs to come *after* the plot
		title(main = my_title, col.main = my_title_colour, font.main = my_title_font_size)
	}
	
	return (invisible())
}


#' @name plotRP
#' @title Plot the risk premium.
#' @description Plot the risk premium.
#' @param my_title text, the title
#' @param my_title_colour text, the title colour
#' @param my_title_font_size numeric, the title font size
#' @param my_x_label text, my_x_label
#' @param xmin numeric, the xmin
#' @param xmax numeric, the xmax
#' @param my_y_label text, the my_y_label
#' @param my_color text, the line color
#' @param fun function, the utility function
#' @param par vector, the uf_parameters
#' @param ev numeric, the expected value
#' @param eu numeric, the expected utility
#' @param ce numeric, the certainty equivalent
#' @param my_labels vector, text labels
#' @param my_label_colors vector, colors of the text labels
#' @param my_label_positions vector, positions of the text labels
#' @param font_scaling numeric, the scaling of the text labels
#' @examples
#' 
#' choice_ids <- c(1, 1, 1, 2, 2, 2, 2)
#' 
#' gamble_ids <- c(1, 1, 2, 1, 1, 2, 2)
#' 
#' outcome_ids <- c(1, 1, 2, 1, 2, 1, 2)
#' 
#' objective_consequences <- c(4000, 0, 3000,
#' 4000, 0, 3000, 0)
#' 
#' probability_strings <- c("0.8", "0.2", "1.0",
#' "0.2", "0.8", "0.25", "0.75")
#' 
#' my_choices <- Choices(choice_ids=choice_ids,
#' 	gamble_ids=gamble_ids,
#' 	outcome_ids=outcome_ids,
#' 	objective_consequences=objective_consequences,
#' 	probability_strings=probability_strings)
#' 
#' my_choices
#' 
#' my_utility <- Utility(fun="power",
#' par=c(alpha=0.88, beta=0.88, lambda=1))
#' eu_df <- compareEU(my_choices, utility=my_utility, digits=4)
#' eu_df
#' 
#' ev <- as.numeric(eu_df$ev[1])
#' eu <- as.numeric(eu_df$eu[1])
#' ce <- as.numeric(eu_df$ce[1])
#' 
#' plotRP(my_title = "risk premium",
#' 	my_title_colour="black", 
#' 	my_title_font_size=4,
#' 	my_x_label = "objective consequence",
#' 	my_y_label = "subjective value", 
#' 	xmin = 2500, xmax = 3500,
#' 	my_color="violet",
#' 	fun=power_uf,
#' 	par=c(alpha=0.88, beta=0.88, lambda=1),
#' 	ev=ev, eu=eu, ce=ce,
#' 	my_labels=c(expression(paste(U(x)==x^alpha, ",
#' 	", x>=0)),
#' 	expression(paste(plain()==-lambda * (-x)^beta, ", ", x<0)),
#' 	"ev","eu","ce","rp"),
#' 	my_label_colors=c("violet","violet","black","red","orange","blue"),
#' 	my_label_positions=list(c(2700,1275),c(2740,1250),c(3250,1075),
#' 	c(2800,1170),c(3050,1075),c(3150,1170)),
#' 	font_scaling=1)
#' 
#' @export
plotRP <- function(my_title, my_title_colour, my_title_font_size, 
	my_x_label, xmin, xmax, my_y_label, my_color, 
	fun, par, ev, eu, ce, 
	my_labels, my_label_colors, my_label_positions, font_scaling)
{

	x=NULL # put this here to avoid a R cmd check NOTE: no visible binding for global variable 'x'	
		
	FUN <- match.fun(fun)

	curve(FUN(par, x),
		from = xmin,
		to = xmax,
		add = FALSE,
		col = my_color,		
		type = "l",
		xlab = my_x_label,
		ylab = my_y_label,
		xaxs = "i",	# let the x and y axes intersect at the origin
		yaxs = "i",		
		n = 2000)
	
	# draw ev vertical line	
	x0 <- ev
	y0 <- 0
	x1 <- ev	
	y1 <- FUN(par, x0)
	
	segments(x0, y0, x1, y1, lty="dashed")
	
	# draw eu horizontal line	
	x0 <- 0
	y0 <- eu
	x1 <- ce
	y1 <- eu
	
	segments(x0, y0, x1, y1, col="red", lty="dashed")	
	
	# draw ce vertical line	
	x0 <- ce
	y0 <- 0
	x1 <- ce
	y1 <- eu	
	segments(x0, y0, x1, y1, col="orange", lty="dashed")		

	# draw rp horizontal line
	x0 <- ce
	y0 <- eu
	x1 <- ev
	y1 <- eu	
	segments(x0, y0, x1, y1, col = "blue", lwd=2)	
	
	# draw grey axis
	abline(h = 0, v = 0, col = "black")
	
	
	if (!missing(my_labels))
	{
		for(index in 1:length(my_labels))
		{
			position <- my_label_positions[[index]]
			
			position_x <- position[1]
			position_y <- position[2]
			
			text_color <- my_label_colors[index]
			
			text(x=position_x, y=position_y, labels = my_labels[index], col = text_color, cex = font_scaling)	
			
		}		
	}
	
	if (!missing(my_title))
	{
		# title needs to come *after* the plot
		title(main = my_title, col.main = my_title_colour, font.main = my_title_font_size)
	}
	
	return (invisible())
}