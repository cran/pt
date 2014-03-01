########################
#
# Choices, a S4 class
#
########################	

#' The Choices class.
#' 
#' The Choices class contains choices for a decision maker. Each choice contains gambles.
#' 
#' @section Slots:
#'  \describe{
#'    \item{\code{choices}:}{Object of class \code{"vector"}, containing Gambles objects for decision makers to choose from.}
#'  }
#'
#' @note A function (also called Choices) has been defined to create instances of this class from the command line.
#' Another function (called choicesFromFile) reads in data from external text files to create instances of the class.
#' @name Choices-class
#' @aliases Choices-class
#' @rdname Choices-class
#' @seealso {\code{\link{Choices}},\code{\link{choicesFromFile}}}
#' @exportClass Choices
setClass(Class = "Choices",
	representation = representation
	(
		choices = "vector"
	),
	# check for input consistency when creating new Choices objects using the "new" constructor
	validity = function(object)
	{
		# this is the Choices inspector
		
		for (index1 in 1: length(choices))
		{
			my_gambless <- get_gambles(choices[index1])
			
			for (index2 in 1:length(my_gambless))
			{
				my_gamble <- my_gambless[index2]
				
				# make sure probabilities of all outcomes sum to <= 1	
				probability_sum = sum(sapply(my_gamble@outcomes, get_probability))

				if (probability_sum < 0 | probability_sum > 1)
				{
					stop(paste("gamble id: ", get_gamble_id(my_gamble), " sum of probabilities: ", probability_sum, " is outside valid range [0, 1].\n"));
				}
				else
				{			
				}	
			}
								
		}

		return (TRUE)
	}
)



########################
# show choice
########################	


setMethod(f = "show",
	signature = "Choices",
	definition = function(object)
	{
		df <- data.frame(row.names=NULL, stringsAsFactors=FALSE)	
		
		for (a in 1:length(object@choices))
		{	
			my_gambless <- get_gambles(object@choices[[a]])
			
			for (n in 1:length(my_gambless))
			{
				a_gamble <- my_gambless[[n]]	
				
				for (m in 1:get_number_of_outcomes(my_gambless[[n]]))
				{	
					an_outcome <- a_gamble@outcomes[[m]]
					
					gamble_id <- get_gamble_id(a_gamble)
					outcome_id <- get_outcome_id(an_outcome)
					probability_string <- get_probability_string(an_outcome)					
					objective_consequence <- format(get_objective_consequence(an_outcome), digits=16, scientific=FALSE)


					df <-rbind(df, data.frame("cid"=a, "gid"=gamble_id, "oid"=outcome_id, "pr"=probability_string, "oc"=objective_consequence))					
								
				}
			}
		}
		print (df)
	}
)



########################
# There are two ways to create an instance of a Choices class.
# The first way is by reading in choices data from an external text file. (choicesFromFile)
# The second way is to create a choice using a series of five vectors. (choices)
########################	

#' @name choicesFromFile
#' @title Create an instance of a Choices class using data from an external text file.
#' @aliases choicesFromFile
#' @description Create an instance of a Choices class using data from an external text file.
#' @details This function is used to create a new instance of a Choices class from an external text file.
#' This file has at least 5 columns, delimited by the DELIMITER character string.
#' Each row of the file contains an individual outcome. The last line of the file needs to be
#' a blank row. An example input file describing the Allais constant ratio paradox looks like this,
#' with the DELIMITER being a "\\t".
#' 
#' choice_id	gamble_id	outcome_id	probability	objective_consequence
#' 
#' 1	1	1	1	3000
#' 
#' 1	2	1	0.8	4000
#' 
#' 1	2	2	0.2	0
#' 
#' 2	1	1	0.25	3000
#' 
#' 2	1	2	0.75	0
#' 
#' 2	2	1	0.2	4000
#' 
#' 2	2	2	0.8	0
#' 
#' 
#' ------
#' 
#' Note that the last line is a blank row.
#'  
#' @usage choicesFromFile(input_file, choice_id_header, gamble_id_header, 
#' outcome_id_header, objective_consequence_header, probability_header, DELIMITER)
#' @param input_file text, the input_file.
#' @param choice_id_header text, the column name of the choice_id variable.
#' @param gamble_id_header text, the column name of the gamble_id variable.
#' @param outcome_id_header text, the column name of the outcome_id variable.
#' @param objective_consequence_header text, the column name of the objective_consequence variable.
#' @param probability_header, the column name of the probability_string variable.
#' @param DELIMITER text, the delimeter character separating the fields in the input file.
#' @examples
#' 
#' # This example loads up the choices for the Allais constant ratio paradox, which
#' # are available as text files in the pt package.
#' 
#' my_input_file <- system.file("external", "allais_constant_ratio_paradox.txt", package="pt")
#' 
#' my_choices <- choicesFromFile(input_file=my_input_file, 
#' 	choice_id_header="choice_id", 
#' 	gamble_id_header="gamble_id", 
#' 	outcome_id_header="outcome_id", 
#' 	objective_consequence_header="objective_consequence", 
#' 	probability_header="probability", 
#' 	DELIMITER="\t")
#' 	
#' my_choices
#' 
#' @export
choicesFromFile <- function(input_file, choice_id_header, gamble_id_header, outcome_id_header, objective_consequence_header, probability_header, DELIMITER)
{
	object <- new(Class = "Choices")
		
	object <- read_choices_data_file(object, input_file, choice_id_header, gamble_id_header, outcome_id_header, objective_consequence_header, probability_header, DELIMITER)	
	
	return (object)

}

#' @name Choices
#' @title Create a new instance of a Choices class.
#' @rdname Choices
#' @aliases Choices
#' @description Create choices using five vectors.
#' @details This function creates a new instance of a Choices class. The inputs are five vectors, representing
#' the properties of each outcome.
#' @usage Choices(choice_ids, gamble_ids, outcome_ids, objective_consequences, probability_strings)
#' @param choice_ids vector, contains the choice_id of each objective_consequence.
#' @param gamble_ids vector, contains the gamble_id of each objective_consequence.
#' @param outcome_ids vector, contains the outcome_id of each objective_consequence.
#' @param objective_consequences vector, contains the objective consequences.
#' @param probability_strings vector, contains the probability_string of each objective consequence.
#' @examples
#' choice_ids <- c(1, 1, 1, 1, 1, 1, 1, 1)
#' 
#' gamble_ids <- c(1, 1, 1, 1, 2, 2, 2, 2)
#' 
#' outcome_ids <- c(1, 2, 3, 4, 1, 2, 3, 4)
#' 
#' objective_consequences <- c(7, 7, 84, 90, 
#' 7, 10, 90, 90)
#' 
#' probability_strings <- c("0.1", "0.3", "0.3", "0.3", 
#' "0.1", "0.3", "0.3", "0.3")
#' 
#' my_choices <- Choices(choice_ids=choice_ids,
#' 	gamble_ids=gamble_ids, 
#' 	outcome_ids=outcome_ids, 
#' 	objective_consequences=objective_consequences, 
#' 	probability_strings=probability_strings)
#' 	
#' my_choices
#' 
#' @export
Choices <- function(choice_ids, gamble_ids, outcome_ids, objective_consequences, probability_strings)
{

	# perform validity checks on the input
	
	# firstly the choice_ids and all other vectors must be the same length
	
	choice_ids_length <- length(choice_ids)
	gamble_ids_length <- length(gamble_ids)
	outcome_ids_length <- length(outcome_ids)	
	objective_consequences_length <- length(objective_consequences)
	probability_strings_length <- length(probability_strings)
	

	if (choice_ids_length != gamble_ids_length)
	{
		stop(paste("choice_ids has length: ", choice_ids_length, " and gamble_ids has length: ", gamble_ids_length, "\n"));			
	}
	
	if (choice_ids_length != outcome_ids_length)
	{
		stop(paste("choice_ids has length: ", choice_ids_length, " and outcome_ids has length: ", outcome_ids_length, "\n"));			
	}	

	if (choice_ids_length != objective_consequences_length)
	{
		stop(paste("choice_ids has length: ", choice_ids_length, " and objective_consequences has length: ", objective_consequences_length, "\n"));			
	}
	
	if (choice_ids_length != probability_strings_length)
	{
		stop(paste("choice_ids has length: ", choice_ids_length, " and probability_strings has length: ", probability_strings_length, "\n"));			
	}	

	
	# create a gamble with no outcomes, but perform further checks on probability sums
	
	object <- new(Class = "Choices")

	cid_choices <- c()
	gamid_gambles <- c()
	outc_gambles <- c()
	prob_string_gambles <- c()
	obj_cons_gambles <- c()
	old_choice_id <- choice_ids[1]

	for (index in 1:length(choice_ids))
	{	
		
		# create a new choice
		if (choice_ids[index] != old_choice_id)
		{	
			my_gambles <- create_gambles_v3(gamid_gambles, outc_gambles, obj_cons_gambles, prob_string_gambles)
			object@choices <- append(object@choices, my_gambles)
			old_choice_id <- choice_ids[index]

			cid_choices <- c()
			gamid_gambles <- c()
			outc_gambles <- c()
			prob_string_gambles <- c()
			obj_cons_gambles <- c()
			
			choice_id <- choice_ids[index]
			gamble_id <- gamble_ids[index]
			outcome_id <- outcome_ids[index]	
			objective_consequence <- objective_consequences[index]
			probability_string <- probability_strings[index]
			
			cid_choices <- append(cid_choices, choice_id)			
			gamid_gambles <- append(gamid_gambles, gamble_id)
			outc_gambles <- append(outc_gambles, outcome_id)	
			obj_cons_gambles <- append(obj_cons_gambles, objective_consequence)
			prob_string_gambles <- append(prob_string_gambles, probability_string)			
		}
		else
		{
			choice_id <- choice_ids[index]			
			gamble_id <- gamble_ids[index]
			outcome_id <- outcome_ids[index]	
			objective_consequence <- objective_consequences[index]
			probability_string <- probability_strings[index]

			cid_choices <- append(cid_choices, choice_id)	
			gamid_gambles <- append(gamid_gambles, gamble_id)
			outc_gambles <- append(outc_gambles, outcome_id)	
			obj_cons_gambles <- append(obj_cons_gambles, objective_consequence)
			prob_string_gambles <- append(prob_string_gambles, probability_string)
			
			if (index == length(choice_ids))
			{
				my_gambles <- create_gambles_v3(gamid_gambles, outc_gambles, obj_cons_gambles, prob_string_gambles)
				object@choices <- append(object@choices, my_gambles)
			}
		}
	}

		
	return (object)	
}


########################
# read decision from external text file
########################	

# declare a custom function
setGeneric(name = "read_choices_data_file",
	def = function(object, ...)
	{
		standardGeneric("read_choices_data_file")
	}
)


setMethod(f = "read_choices_data_file",
	signature = "Choices",
	definition = function(object, input_file, choice_id_header, gamble_id_header, outcome_id_header, objective_consequence_header, probability_header, DELIMITER)
	{

		#read in outcomes from external file
		my_data_frame <- data.frame(read.table(file = input_file, header = TRUE, stringsAsFactors = FALSE, sep = DELIMITER))

		cid_choices <- c()
		gamid_gambles <- c()
		outc_gambles <- c()
		prob_string_gambles <- c()
		obj_cons_gambles <- c()
		old_choice_id <- eval(parse(text = my_data_frame[1, choice_id_header]))
				
		for (index in 1:nrow(my_data_frame))
		{
			choice_id <- eval(parse(text = my_data_frame[index, choice_id_header]))						
			gamble_id <- eval(parse(text = my_data_frame[index, gamble_id_header]))			
			outcome_id <- eval(parse(text = my_data_frame[index, outcome_id_header]))	
			objective_consequence <- my_data_frame[index, objective_consequence_header]			
			probability_string <- as.character(format(my_data_frame[index, probability_header], digits = 16, scientific=FALSE))

			# create a new choice
			if (choice_id != old_choice_id)
			{	
				my_gambles <- create_gambles_v3(gamid_gambles, outc_gambles, obj_cons_gambles, prob_string_gambles)
				object@choices <- append(object@choices, my_gambles)
				old_choice_id <- choice_id
				
				cid_choices <- c()
				gamid_gambles <- c()
				outc_gambles <- c()
				prob_string_gambles <- c()
				obj_cons_gambles <- c()
				
				cid_choices <- append(cid_choices, choice_id)
				gamid_gambles <- append(gamid_gambles, gamble_id)
				outc_gambles <- append(outc_gambles, outcome_id)	
				obj_cons_gambles <- append(obj_cons_gambles, objective_consequence)
				prob_string_gambles <- append(prob_string_gambles, probability_string)			
			}
			else
			{	
				cid_choices <- append(cid_choices, choice_id)				
				gamid_gambles <- append(gamid_gambles, gamble_id)
				outc_gambles <- append(outc_gambles, outcome_id)	
				obj_cons_gambles <- append(obj_cons_gambles, objective_consequence)
				prob_string_gambles <- append(prob_string_gambles, probability_string)
				
				if (index == nrow(my_data_frame))
				{
					my_gambles <- create_gambles_v3(gamid_gambles, outc_gambles, obj_cons_gambles, prob_string_gambles)
					object@choices <- append(object@choices, my_gambles)
				}
			}
		}
		
		return (object)
	}
)


########################
# support functions
########################	

########################
# get choices
########################	

# declare a custom function to return the choices
setGeneric(name = "get_choices",
	def = function(object, ...)
	{
		standardGeneric("get_choices")
	}
)

# provide implementation of custom function to return the choices
setMethod(f = "get_choices",
	signature = "Choices",
	definition = function(object)
	{	
		return (object@choices)
	}
)

########################
# computational functions for comparing theories
########################	

########################
# ev related functions
########################	

#' @name compareEV
#' @rdname compareEV
#' @exportMethod compareEV
setGeneric(name = "compareEV",
	def = function(object, digits)
	{
		standardGeneric("compareEV")
	}
)

#' @name compareEV
#' @title Compare the expected value (EV) of choice gambles.
#' @aliases compareEV,Choices-method
#' @rdname compareEV
#' @description Compare the expected value (EV) of choice gambles.
#' @param object Choices, an instance of a Choices class.
#' @param digits numeric, the number of digits to display in the output.
#' @references
#' 
#' Montgomery, H., & Adelbratt, T. (1982). Gambling decisions and information about expected value. Organizational Behavior and Human Performance, 29(1), 39-57.
#' 
#' Lichtenstein, S., Slovic, P., & Zink, D. (1969). Effect of instruction in expected value on optimality of gambling decisions. Journal of Experimental Psychology, 79(2, Pt.1), 236-240.
#' 
#' Li, S. (2003). The role of Expected Value illustrated in decision-making under risk: Single-play vs multiple-play. Journal of Risk Research, 6(2), 113-124.
#' 
#' Colbert, G., Murray, D., & Nieschwietz, R. (2009). The use of expected value in pricing judgments. Journal of Risk Research, 12(2), 199-208.
#' 
#' Yates, J. F. (1990). Judgment and decision making. Englewood Cliffs, NJ: Prentice Hall.
#' 
#' @examples
#' 
#' # This example creates the two Allais common consequence paradox choices, 
#' # and computes the EV for each gamble in the choices.
#' 
#' choice_ids <- c(1, 1, 1, 1, 2, 2, 2, 2)
#' 
#' gamble_ids <- c(1, 1, 1, 2, 1, 1, 2, 2)
#' 
#' outcome_ids <- c(1, 2, 3, 1, 1, 2, 1, 2)
#' 
#' objective_consequences <- c(2500, 2400, 0, 2400, 
#' 	2500, 0, 2400, 0)
#' 	
#' probability_strings <- c("0.33", "0.66", "0.01", "1.0", 
#' 	"0.33", "0.67", "0.34", "0.66")
#' 	
#' my_choices <- Choices(choice_ids=choice_ids,
#' 	gamble_ids=gamble_ids, 
#' 	outcome_ids=outcome_ids, 
#' 	objective_consequences=objective_consequences, 
#' 	probability_strings=probability_strings)
#' 	
#' my_choices
#' 
#' compareEV(my_choices, digits=4)
setMethod(f = "compareEV",
	signature = "Choices",
	definition = function(object, digits)
	{	
	
		summary_df <- data.frame()
		
		for (n in 1:length(object@choices))
		{	
			df <- compare_gambles_under_expected_value(object@choices[[n]], digits)
			df$cid <- n
			df <- df[c("cid", "gid", "ev")]
			summary_df <- rbind(summary_df, df)
		}		

		return (summary_df)

	}
)


########################
# EU related functions
########################	

#' @name compareEU
#' @rdname compareEU
#' @exportMethod compareEU
setGeneric(name = "compareEU",
	def = function(object, utility, digits)
	{
		standardGeneric("compareEU")
	}
)

#' @name compareEU
#' @title Compare the expected utility (EU) of choice gambles.
#' @aliases compareEU,Choices-method
#' @rdname compareEU
#' @description Compare the expected utility (EU) of choice gambles.
#' @param object Choices, an instance of a Choices class.
#' @param utility Utility, an instance of a Utility class.
#' @param digits numeric, the number of digits to display in the output.
#' @references
#' von Neumann, J., & Morgenstern, O. (1947). Theory of games and economic behavior (2nd ed.). Princeton, NJ: Princeton University Press.
#' 
#' Bernoulli, D. (1954). Exposition of a new theory on the measurement of risk. Econometrica, 22(1), 23-36.
#' 
#' Bernoulli, D. (1738). Specimen theoriae novae de mensura sortis. Commentarii Academiae Scientiarum Imperialis Petropolitanae, 5, 175-192.
#' 
#' @examples
#' 
#' # This example creates the two Allais common consequence paradox choices, 
#' # and computes the EU for each gamble in the choices.
#' 
#' choice_ids <- c(1, 1, 1, 1, 2, 2, 2, 2)
#' 
#' gamble_ids <- c(1, 1, 1, 2, 1, 1, 2, 2)
#' 
#' outcome_ids <- c(1, 2, 3, 1, 1, 2, 1, 2)
#' 
#' objective_consequences <- c(2500, 2400, 0, 2400, 
#' 	2500, 0, 2400, 0)
#' 	
#' probability_strings <- c("0.33", "0.66", "0.01", "1.0", 
#' 	"0.33", "0.67", "0.34", "0.66")
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
#' par=c(alpha=1.0, beta=1.0, lambda=1.0))
#' 
#' compareEU(my_choices, utility=my_utility, digits=4)
#' @export
setMethod(f = "compareEU",
	signature = "Choices",
	definition = function(object, utility, digits)
	{	
	
		summary_df <- data.frame()
		
		for (n in 1:length(object@choices))
		{	
			df <- compare_gambles_under_expected_utility(object@choices[[n]], utility, digits)
			df$cid <- n
			df <- df[c("cid", "gid", "ev", "eu", "ce", "rp")]
			summary_df <- rbind(summary_df, df)
		}

		return (summary_df)

	}
)


########################
# SWU related functions
########################	

#' @name compareSWU
#' @rdname compareSWU
#' @exportMethod compareSWU
setGeneric(name = "compareSWU",
	def = function(object, prob_weight, utility, digits)
	{
		standardGeneric("compareSWU")
	}
)

#' @name compareSWU
#' @title Compare choice gambles under Edwards' (1954, 1962) Subjective Weighted Utility (SWU).
#' @aliases compareSWU,Choices-method
#' @rdname compareSWU
#' @description Compare choice gambles under Edwards' (1954, 1962) Subjective Weighted Utility (SWU).
#' @param object Choices, an instance of a Choices class.
#' @param prob_weight ProbWeight, an instance of a ProbWeight class.
#' @param utility Utility, an instance of a Utility class.
#' @param digits numeric, the number of digits to display in the output.
#' @references
#' Edwards, W. (1954). The theory of decision making. Psychological Bulletin, 51(4), 380-417.
#' 
#' Edwards, W. (1962). Subjective probabilities inferred from decisions. Psychological Review, 69(2), 109-135.
#' 
#' Birnbaum, M. H. (1999). The paradoxes of Allais, stochastic dominance, and decision weights. In J. Shanteau, B. A. Mellers & D. A. Schum (Eds.), Decision science and technology: Reflections on the contributions of Ward Edwards (pp. 27-52). Norwell, MA: Kluwer Academic Publishers.
#' 
#' @examples
#' 
#' # This example creates the two Allais common consequence paradox choices, 
#' # and computes the SWU for each gamble in the choices.
#' 
#' choice_ids <- c(1, 1, 1, 1, 2, 2, 2, 2)
#' 
#' gamble_ids <- c(1, 1, 1, 2, 1, 1, 2, 2)
#' 
#' outcome_ids <- c(1, 2, 3, 1, 1, 2, 1, 2)
#' 
#' objective_consequences <- c(2500, 2400, 0, 2400, 
#' 	2500, 0, 2400, 0)
#' 	
#' probability_strings <- c("0.33", "0.66", "0.01", "1.0", 
#' 	"0.33", "0.67", "0.34", "0.66")
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
#' 	par=c(alpha=0.4, beta=0.4, lambda=1))
#'
#' my_pwf <-
#' ProbWeight(fun="linear_in_log_odds",
#' 	par=c(alpha=0.4, beta=0.4))
#'
#' compareSWU(my_choices,
#'	prob_weight=my_pwf,
#'	utility=my_utility,
#'	digits=4)
#' @export
setMethod(f = "compareSWU",
	signature = "Choices",
	definition = function(object, prob_weight, utility, digits)
	{	
	
		summary_df <- data.frame()
		
		for (n in 1:length(object@choices))
		{	
			df <- compare_gambles_under_swu(object@choices[[n]], prob_weight, utility, digits)
			df$cid <- n
			df <- df[c("cid", "gid", "ev", "swu", "ce", "rp")]
			summary_df <- rbind(summary_df, df)
		}
		
		return (summary_df)
	}
)

########################
# swau related functions
########################	

#' @name compareSWAU
#' @rdname compareSWAU
#' @exportMethod compareSWAU
setGeneric(name = "compareSWAU",
	def = function(object, prob_weight, utility, digits)
	{
		standardGeneric("compareSWAU")
	}
)

#' @name compareSWAU
#' @title Compare choices under Subjectively weighted average utility (SWAU).
#' @aliases compareSWAU,Choices-method
#' @rdname compareSWAU
#' @description Compare choices under Subjectively weighted average utility (SWAU).
#' @param object Choices, an instance of a Choices class.
#' @param prob_weight ProbWeight, an instance of a ProbWeight class.
#' @param utility Utility, an instance of a Utility class.
#' @param digits numeric, the number of digits to display in the output.
#' @references
#' Karmarkar, U. S. (1978). Subjectively weighted utility: A descriptive extension of the expected utility model. Organizational Behavior & Human Performance, 21(1), 61-72.
#' 
#' Karmarkar, U. S. (1979). Subjectively weighted utility and the Allais Paradox. Organizational Behavior & Human Performance, 24(1), 67-72.
#' 
#' Viscusi, W. K. (1989). Prospective reference theory: Toward an explanation of the paradoxes. Journal of Risk and Uncertainty, 2(3), 235-263.
#' 
#' Lattimore, P. K., Baker, J. R., & Witte, A. D. (1992). The influence of probability on risky choice: A parametric examination. Journal of Economic Behavior and Organization, 17(3), 377-400.
#' 
#' Birnbaum, M. H. (1999). The paradoxes of Allais, stochastic dominance, and decision weights. In J. Shanteau, B. A. Mellers & D. A. Schum (Eds.), Decision science and technology: Reflections on the contributions of Ward Edwards (pp. 27-52). Norwell, MA: Kluwer Academic Publishers.
#' 
#' @examples
#' 
#' # This example creates the two Allais common consequence paradox choices, 
#' # and computes the SWAU for each gamble in the choices.
#' 
#' choice_ids <- c(1, 1, 1, 1, 2, 2, 2, 2)
#' 
#' gamble_ids <- c(1, 1, 1, 2, 1, 1, 2, 2)
#' 
#' outcome_ids <- c(1, 2, 3, 1, 1, 2, 1, 2)
#' 
#' objective_consequences <- c(2500, 2400, 0, 2400, 
#' 	2500, 0, 2400, 0)
#' 	
#' probability_strings <- c("0.33", "0.66", "0.01", "1.0", 
#' 	"0.33", "0.67", "0.34", "0.66")
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
#' 	par=c(alpha=0.4, beta=0.4, lambda=1))
#'
#' my_pwf <-
#' ProbWeight(fun="linear_in_log_odds",
#' 	par=c(alpha=0.4, beta=0.4))
#'
#' compareSWAU(my_choices,
#'	prob_weight=my_pwf,
#'	utility=my_utility,
#'	digits=4)
#' @export
setMethod(f = "compareSWAU",
	signature = "Choices",
	definition = function(object, prob_weight, utility, digits)
	{	

		summary_df <- data.frame()
		
		for (n in 1:length(object@choices))
		{	
			df <- compare_gambles_under_swau(object@choices[[n]], prob_weight, utility, digits)
			df$cid <- n
			df <- df[c("cid", "gid", "ev", "swau", "ce", "rp")]
			summary_df <- rbind(summary_df, df)
		}
		
		return (summary_df)
	}
)


########################
# RDU related functions
########################	

#' @name compareRDU
#' @rdname compareRDU
#' @exportMethod compareRDU
setGeneric(name = "compareRDU",
	def = function(object, prob_weight, utility, digits)
	{
		standardGeneric("compareRDU")
	}
)

#' @name compareRDU
#' @title Compare choice gambles under Quiggin's (1993) Rank-dependent utility (RDU).
#' @aliases compareRDU,Choices-method
#' @rdname compareRDU
#' @description Compare choice gambles under Quiggin's (1993) Rank-dependent utility (RDU).
#' @param object Choices, an instance of a Choices class.
#' @param prob_weight ProbWeight, an instance of a ProbWeight class.
#' @param utility Utility, an instance of a Utility class.
#' @param digits numeric, the number of digits to display in the output.
#' @references
#' Quiggin, J. (1982). A theory of anticipated utility. Journal of Economic Behavior & Organization, 3(4), 323-343.
#' 
#' Quiggin, J. (1985). Subjective utility, anticipated utility, and the Allais paradox. Organizational Behavior and Human Decision Processes, 35(1), 94-101.
#' 
#' Quiggin, J. (1993). Generalized expected utility theory: The rank-dependent model. Boston, MA: Kluwer Academic Publishers.
#' 
#' @examples
#' 
#' # This example creates the two Allais common consequence paradox choices, 
#' # and computes the RDU for each gamble in the choices.
#' 
#' choice_ids <- c(1, 1, 1, 1, 2, 2, 2, 2)
#' 
#' gamble_ids <- c(1, 1, 1, 2, 1, 1, 2, 2)
#' 
#' outcome_ids <- c(1, 2, 3, 1, 1, 2, 1, 2)
#' 
#' objective_consequences <- c(2500, 2400, 0, 2400, 
#' 	2500, 0, 2400, 0)
#' 	
#' probability_strings <- c("0.33", "0.66", "0.01", "1.0", 
#' 	"0.33", "0.67", "0.34", "0.66")
#' 	
#' my_choices <- Choices(choice_ids=choice_ids,
#' 	gamble_ids=gamble_ids, 
#' 	outcome_ids=outcome_ids, 
#' 	objective_consequences=objective_consequences, 
#' 	probability_strings=probability_strings)
#' 	
#' my_choices
#' 
#' tk_1992_utility <- Utility(fun="power",
#' 	par=c(alpha=0.88, beta=0.88, lambda=2.25))
#' 	
#' tk_1992_positive_probWeight <-
#'	ProbWeight(fun=
#'	"Tversky_Kahneman_1992",
#'	par=c(alpha=0.61))
#'	
#' compareRDU(my_choices,
#'	prob_weight=
#'	tk_1992_positive_probWeight,
#'	utility=tk_1992_utility,
#'	digits=4)
#' @export
setMethod(f = "compareRDU",
	signature = "Choices",
	definition = function(object, prob_weight, utility, digits)
	{	
		
		summary_df <- data.frame()
		
		for (n in 1:length(object@choices))
		{	
			results_list <- compare_gambles_under_rdu(object@choices[[n]], prob_weight, utility, digits)
			
			for (m in 1:length(results_list))
			{
				df_list <- results_list[[m]]
				df <- df_list$summary
				df
				df$cid <- n
				df <- df[c("cid", "gid", "ev", "rdu", "ce", "rp")]	
				
				summary_df <- rbind(summary_df, df)
			}
		}
		
		return (summary_df)

	}
)

########################
# PT related functions
########################	

#' @name comparePT
#' @rdname comparePT
#' @exportMethod comparePT
setGeneric(name = "comparePT",
	def = function(object, prob_weight_for_positive_outcomes, prob_weight_for_negative_outcomes, utility, digits)
	{
		standardGeneric("comparePT")
	}
)

#' @name comparePT
#' @title Compare choice gambles under Tversky and Kahneman's (1992) (Cumulative) prospect theory (PT).
#' @aliases comparePT,Choices-method
#' @rdname comparePT
#' @description Compare choice gambles under Tversky and Kahneman's (1992) (Cumulative) prospect theory (PT).
#' @param object Choices, an instance of a Choices class.
#' @param prob_weight_for_positive_outcomes ProbWeight, an instance of a ProbWeight class.
#' @param prob_weight_for_negative_outcomes ProbWeight, an instance of a ProbWeight class.
#' @param utility Utility, an instance of a Utility class.
#' @param digits numeric, the number of digits to display in the output.
#' @references
#' Tversky, A., & Kahneman, D. (1992). Advances in prospect theory: Cumulative representation of uncertainty. Journal of Risk and Uncertainty, 5(4), 297-323.
#' 
#' Wakker, P. P. (2010). Prospect theory: For risk and ambiguity. Cambridge, UK: Cambridge University Press.
#'  
#' @examples
#' 
#' # This example creates the two Allais common consequence paradox choices, 
#' # and computes the PT for each gamble in the choices.
#' 
#' choice_ids <- c(1, 1, 1, 1, 2, 2, 2, 2)
#' 
#' gamble_ids <- c(1, 1, 1, 2, 1, 1, 2, 2)
#' 
#' outcome_ids <- c(1, 2, 3, 1, 1, 2, 1, 2)
#' 
#' objective_consequences <- c(2500, 2400, 0, 2400, 
#' 	2500, 0, 2400, 0)
#' 	
#' probability_strings <- c("0.33", "0.66", "0.01", "1.0", 
#' 	"0.33", "0.67", "0.34", "0.66")
#' 	
#' my_choices <- Choices(choice_ids=choice_ids,
#' 	gamble_ids=gamble_ids, 
#' 	outcome_ids=outcome_ids, 
#' 	objective_consequences=objective_consequences, 
#' 	probability_strings=probability_strings)
#' 	
#' my_choices
#' 
#' tk_1992_utility <- Utility(fun="power",
#'	par=c(alpha=0.88, beta=0.88, lambda=2.25))
#'	
#' tk_1992_positive_probWeight <-
#'	ProbWeight(fun=
#'	"Tversky_Kahneman_1992",
#'	par=c(alpha=0.61))
#'	
#' tk_1992_negative_probWeight <-
#'	ProbWeight(fun=
#'	"Tversky_Kahneman_1992",
#'	par=c(alpha=0.69))
#'	
#' comparePT(my_choices,
#'	prob_weight_for_positive_outcomes=
#'	tk_1992_positive_probWeight,
#'	prob_weight_for_negative_outcomes=
#'	tk_1992_negative_probWeight,
#'	utility=tk_1992_utility, 
#'	digits=4)
#' @export
setMethod(f = "comparePT",
	signature = "Choices",
	definition = function(object, 
		prob_weight_for_positive_outcomes, 
		prob_weight_for_negative_outcomes, 
		utility, 
		digits)
	{	
	
		summary_df <- data.frame(row.names=NULL, stringsAsFactors=FALSE)
		
		for (n in 1:length(object@choices))
		{	
			results_list <- compare_gambles_under_pt(object@choices[[n]], prob_weight_for_positive_outcomes, prob_weight_for_negative_outcomes, utility, digits)
			
			for (m in 1:length(results_list))
			{
				df_list <- results_list[[m]]
				df <- df_list$summary
				df$cid <- n
				df <- df[c("cid", "gid", "ev", "pt", "ce", "rp")]	
				
				summary_df <- rbind(summary_df, df)
			}

		}

		return (summary_df)

	}
)


########################
# TAX related functions
########################	

#' @name compareTAX
#' @rdname compareTAX
#' @exportMethod compareTAX
setGeneric(name = "compareTAX",
	def = function(object, prob_weight, utility, delta, digits)
	{
		standardGeneric("compareTAX")
	}
)

#' @name compareTAX
#' @title Compare choice gambles under Birnbaum's (2008) configural weight (special) TAX theory.
#' @aliases compareTAX,Choices-method
#' @rdname compareTAX
#' @description Compare choice gambles under Birnbaum's (2008) configural weight (special) TAX theory.
#' @param object Choices, an instance of a Choices class.
#' @param prob_weight ProbWeight, an instance of a ProbWeight class.
#' @param utility Utility, an instance of a Utility class.
#' @param delta numeric, the delta parameter in Birnbaum's TAX theory.
#' @param digits numeric, the number of digits to display in the output.
#' @references
#' Birnbaum, M. H. (2008). New paradoxes of risky decision making. Psychological Review, 115(2), 463-501.
#'
#' @examples
#' 
#' # This example creates the two Allais common consequence paradox choices, 
#' # and computes the TAX for each gamble in the choices.
#' 
#' choice_ids <- c(1, 1, 1, 1, 2, 2, 2, 2)
#' 
#' gamble_ids <- c(1, 1, 1, 2, 1, 1, 2, 2)
#' 
#' outcome_ids <- c(1, 2, 3, 1, 1, 2, 1, 2)
#' 
#' objective_consequences <- c(2500, 2400, 0, 2400, 
#' 	2500, 0, 2400, 0)
#' 	
#' probability_strings <- c("0.33", "0.66", "0.01", "1.0", 
#' 	"0.33", "0.67", "0.34", "0.66")
#' 	
#' my_choices <- Choices(choice_ids=choice_ids,
#' 	gamble_ids=gamble_ids, 
#' 	outcome_ids=outcome_ids, 
#' 	objective_consequences=objective_consequences, 
#' 	probability_strings=probability_strings)
#' 	
#' my_choices
#' 
#' my_utility <- Utility(fun="linear",
#'	par=c(lambda=1))
#'	
#' power_probability_weighting <-
#'	ProbWeight(fun="power",
#'	par=c(alpha=0.7, beta=1))
#'	
#' compareTAX(my_choices,
#'	prob_weight=power_probability_weighting,
#'	utility=my_utility, 
#'	delta=-1, 
#'	digits=4)
#' @export
setMethod(f = "compareTAX",
	signature = "Choices",
	definition = function(object, prob_weight, utility, delta, digits)
	{	
	
		summary_df <- data.frame()
		
		for (n in 1:length(object@choices))
		{	
			df <- compare_gambles_under_tax(object@choices[[n]], prob_weight, utility, delta, digits)
			df$cid <- n
			df <- df[c("cid", "gid", "ev", "tax", "ce", "rp")]
			summary_df <- rbind(summary_df, df)
		}

		return (summary_df)

	}
)

########################
# RAM related functions
########################	

#' @name compareRAM
#' @rdname compareRAM
#' @exportMethod compareRAM
setGeneric(name = "compareRAM",
	def = function(object, branch_weight_list, prob_weight, utility, digits)
	{
		standardGeneric("compareRAM")
	}
)

#' @name compareRAM
#' @title Compare choice gambles under Birnbaum's (2008) configural weight RAM theory.
#' @aliases compareRAM,Choices-method
#' @rdname compareRAM
#' @description Compare choice gambles under Birnbaum's (2008) configural weight RAM theory.
#' @param object Choices, an instance of a Choices class.
#' @param branch_weight_list list, a list of branch weighting vectors.
#' @param prob_weight ProbWeight, an instance of a ProbWeight class.
#' @param utility Utility, an instance of a Utility class.
#' @param digits numeric, the number of digits to display in the output.
#' @references
#' Birnbaum, M. H. (2008). New paradoxes of risky decision making. Psychological Review, 115(2), 463-501.
#'
#' @examples
#' 
#' # This example creates the two Allais common consequence paradox choices, 
#' # and computes the RAM for each gamble in the choices.
#' 
#' choice_ids <- c(1, 1, 1, 1, 2, 2, 2, 2)
#' 
#' gamble_ids <- c(1, 1, 1, 2, 1, 1, 2, 2)
#' 
#' outcome_ids <- c(1, 2, 3, 1, 1, 2, 1, 2)
#' 
#' objective_consequences <- c(2500, 2400, 0, 2400, 
#' 	2500, 0, 2400, 0)
#' 	
#' probability_strings <- c("0.33", "0.66", "0.01", "1.0", 
#' 	"0.33", "0.67", "0.34", "0.66")
#' 	
#' my_choices <- Choices(choice_ids=choice_ids,
#' 	gamble_ids=gamble_ids, 
#' 	outcome_ids=outcome_ids, 
#' 	objective_consequences=objective_consequences, 
#' 	probability_strings=probability_strings)
#' 	
#' my_choices
#' 
#' # note that the maximum number of outcomes in the gambles is 3,
#' # so branch weights for 3 outcomes need to be provided. 
#' 
#' branch_weight_list <- list(c(1),
#'	c(0.3738, 0.6262),
#'	c(0.16, 0.33, 0.51))
#'	
#' my_utility <- Utility(fun="linear",
#'	par=c(lambda=1))
#'	
#' power_probability_weighting <-
#'	ProbWeight(fun="power",
#'	par=c(alpha=0.7, beta=1))
#'	
#' compareRAM(my_choices,
#'	branch_weight_list=branch_weight_list,
#'	prob_weight=power_probability_weighting,
#'	utility=my_utility, 
#'	digits=4)
#' @export
setMethod(f = "compareRAM",
	signature = "Choices",
	definition = function(object, branch_weight_list, prob_weight, utility, digits)
	{	
		
		summary_df <- data.frame()
		
		for (n in 1:length(object@choices))
		{	
			df <- compare_gambles_under_ram(object@choices[[n]], branch_weight_list, prob_weight, utility, digits)
			df$cid <- n
			df <- df[c("cid", "gid", "ev", "ramu", "ce", "rp")]
			summary_df <- rbind(summary_df, df)
		}
		
		return (summary_df)

	}
)

########################
# GDU related functions
########################	

#' @name compareGDU
#' @rdname compareGDU
#' @exportMethod compareGDU
setGeneric(name = "compareGDU",
	def = function(object, prob_weight, utility, digits)
	{
		standardGeneric("compareGDU")
	}
)

#' @name compareGDU
#' @title Compare choice gambles under Luce's (2000) (Lower) Gains-decompositions utility (GDU) theory.
#' @aliases compareGDU,Choices-method
#' @rdname compareGDU
#' @description Compare choice gambles under Luce's (2000) (Lower) Gains-decompositions utility (GDU) theory.
#' @param object Choices, an instance of a Choices class.
#' @param prob_weight ProbWeight, an instance of a ProbWeight class.
#' @param utility Utility, an instance of a Utility class.
#' @param digits numeric, the number of digits to display in the output.
#' @references
#' Luce, R. D. (2000). Utility of gains and losses: Measurement-theoretical and experimental approaches. Mahwah, NJ: Lawrence Erlbaum Associates.
#'
#' @examples
#' 
#' # This example creates the two Allais common consequence paradox choices, 
#' # and computes the GDU for each gamble in the choices.
#' 
#' choice_ids <- c(1, 1, 1, 1, 2, 2, 2, 2)
#' 
#' gamble_ids <- c(1, 1, 1, 2, 1, 1, 2, 2)
#' 
#' outcome_ids <- c(1, 2, 3, 1, 1, 2, 1, 2)
#' 
#' objective_consequences <- c(2500, 2400, 0, 2400, 
#' 	2500, 0, 2400, 0)
#' 	
#' probability_strings <- c("0.33", "0.66", "0.01", "1.0", 
#' 	"0.33", "0.67", "0.34", "0.66")
#' 	
#' my_choices <- Choices(choice_ids=choice_ids,
#' 	gamble_ids=gamble_ids, 
#' 	outcome_ids=outcome_ids, 
#' 	objective_consequences=objective_consequences, 
#' 	probability_strings=probability_strings)
#' 	
#' my_choices
#' 
#' my_pwf <-
#'	ProbWeight(fun="compound_invariance",
#'	par=c(alpha=0.542, beta=1.382))
#'	
#' my_utility <- Utility(fun="power",
#'	par=c(alpha=1, beta=1, lambda=1))
#'	
#' compareGDU(my_choices,
#'	prob_weight=my_pwf,
#'	utility=my_utility,
#'	digits=4)
#' @export
setMethod(f = "compareGDU",
	signature = "Choices",
	definition = function(object, prob_weight, utility, digits)
	{	
	
		summary_df <- data.frame()
		
		for (n in 1:length(object@choices))
		{	
			df <- compare_gambles_under_gdu(object@choices[[n]], prob_weight, utility, digits)
			df$cid <- n
			df <- df[c("cid", "gid", "ev", "gdu", "ce", "rp")]
			summary_df <- rbind(summary_df, df)
		}
		
		return (summary_df)

	}
)


########################
# PRT related functions
########################	

#' @name comparePRT
#' @rdname comparePRT
#' @exportMethod comparePRT
setGeneric(name = "comparePRT",
	def = function(object, utility, gamma, digits)
	{
		standardGeneric("comparePRT")
	}
)

#' @name comparePRT
#' @title Compare choice gambles under Viscusi's (1989) Prospective reference theory (PRT).
#' @aliases comparePRT,Choices-method
#' @rdname comparePRT
#' @description Compare choice gambles under Viscusi's (1989) Prospective reference theory (PRT).
#' @param object Choices, an instance of a Choices class.
#' @param utility Utility, an instance of a Utility class.
#' @param gamma numeric, the gamma parameter in Viscusi's theory.
#' @param digits numeric, the number of digits to display in the output.
#' @references
#' Viscusi, W. K. (1989). Prospective reference theory: Toward an explanation of the paradoxes. Journal of Risk and Uncertainty, 2(3), 235-263.
#' 
#' @examples
#' 
#' # This example creates the two Allais common consequence paradox choices, 
#' # and computes the PRT for each gamble in the choices.
#' 
#' choice_ids <- c(1, 1, 1, 1, 2, 2, 2, 2)
#' 
#' gamble_ids <- c(1, 1, 1, 2, 1, 1, 2, 2)
#' 
#' outcome_ids <- c(1, 2, 3, 1, 1, 2, 1, 2)
#' 
#' objective_consequences <- c(2500, 2400, 0, 2400, 
#' 	2500, 0, 2400, 0)
#' 	
#' probability_strings <- c("0.33", "0.66", "0.01", "1.0", 
#' 	"0.33", "0.67", "0.34", "0.66")
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
#'	par=c(alpha=0.631, beta=0.631, lambda=1))
#'	
#' gamma <- 0.676
#'	
#' comparePRT(my_choices,
#'	utility=my_utility,
#'	gamma=gamma,
#'	digits=4)
#' @export
setMethod(f = "comparePRT",
	signature = "Choices",
	definition = function(object, utility, gamma, digits)
	{	
		
		summary_df <- data.frame()
		
		for (n in 1:length(object@choices))
		{	
			df <- compare_gambles_under_prt(object@choices[[n]], utility, gamma, digits)
			df$cid <- n
			df <- df[c("cid", "gid", "ev", "prtu", "ce", "rp")]
			summary_df <- rbind(summary_df, df)
		}
		
		return (summary_df)

	}
)

########################	
# I/O wrappers
########################	

#' @name saveChoices
#' @rdname saveChoices
#' @exportMethod saveChoices
setGeneric(name = "saveChoices",
	def = function(object, output_file, choice_id_header, 
		gamble_id_header, outcome_id_header, probability_header, 
		objective_consequence_header, DELIMITER)
	{
		standardGeneric("saveChoices")
	}
)

#' @name saveChoices
#' @title Saves a Choices object to an external text file.
#' @aliases saveChoices,Choices-method
#' @rdname saveChoices
#' @description Saves a Choices object to an external text file.
#' @param object Choices, an instance of a Choices class.
#' @param output_file text, the output file for saving my_choices.
#' @param choice_id_header text, the column name for the choice_id field in the output file.
#' @param gamble_id_header text, the column name for the gamble_id field in the output file.
#' @param outcome_id_header text, the column name for the outcome_id field in the output file.
#' @param probability_header text, the column name for the probability field in the output file.
#' @param objective_consequence_header text, the column name for the objective_consequence field in the output file.
#' @param DELIMITER text, the delimiter character used to separate the columns in the output file.
#' 
#' @examples
#' 
#' # This example creates the two Allais common consequence paradox choices, 
#' # and saves them to an external text file.
#' 
#' choice_ids <- c(1, 1, 1, 1, 2, 2, 2, 2)
#' 
#' gamble_ids <- c(1, 1, 1, 2, 1, 1, 2, 2)
#' 
#' outcome_ids <- c(1, 2, 3, 1, 1, 2, 1, 2)
#' 
#' objective_consequences <- c(2500, 2400, 0, 2400, 
#' 	2500, 0, 2400, 0)
#' 	
#' probability_strings <- c("0.33", "0.66", "0.01", "1.0", 
#' 	"0.33", "0.67", "0.34", "0.66")
#' 	
#' my_choices <- Choices(choice_ids=choice_ids,
#' 	gamble_ids=gamble_ids, 
#' 	outcome_ids=outcome_ids, 
#' 	objective_consequences=objective_consequences, 
#' 	probability_strings=probability_strings)
#' 	
#' my_choices
#' 
#' my_output_file <- paste(tempdir(), "\\", "saved_choices.txt", sep="")
#' 
#' saveChoices(my_choices, 
#'	output_file=my_output_file,
#'	choice_id_header="choice_id",
#'	gamble_id_header="gamble_id",
#'	outcome_id_header="outcome_id",
#'	probability_header="probability",
#'	objective_consequence_header="objective_consequence",
#'	DELIMITER="\\t")
#' 
#' # after finishing with the file, delete to keep the workspace tidy
#' unlink(my_output_file)
#' # remove the object from the global environment
#' rm(my_output_file)
#' 
#' @export
setMethod(f = "saveChoices",
	signature = "Choices",
	definition = function(object, 
		output_file, 
		choice_id_header, 
		gamble_id_header, 
		outcome_id_header, 
		probability_header, 
		objective_consequence_header, 
		DELIMITER)
	{	
		df <- data.frame(row.names = NULL, stringsAsFactors = FALSE)
		
		row_index <- 1
	
		# extract class data into data.frame		
		for (i in 1:length(object@choices))
		{
			choice_id <- i
			my_gambles <- object@choices[[i]]
			
			my_gambless <- get_gambles(my_gambles)
					
			for (j in 1:length(my_gambless))
			{
				my_gamble <- my_gambless[[j]]
				gamble_id <- get_gamble_id(my_gamble)
				
				outcomes <- get_outcomes(my_gamble)
				
				for (k in 1:length(outcomes))
				{
					outcome_id <- get_outcome_id(outcomes[[k]])
					objective_consequence <- get_objective_consequence(outcomes[[k]])
					probability_string <- get_probability_string(outcomes[[k]])	
					
					#[rows, cols]
					df[row_index, 1] <- choice_id
					df[row_index, 2] <- gamble_id
					df[row_index, 3] <- outcome_id
					df[row_index, 4] <- probability_string		
					df[row_index, 5] <- objective_consequence
					
					row_index <- row_index + 1
				}		
			}

		}
		
		names(df) <- c(choice_id_header, gamble_id_header, outcome_id_header, probability_header, objective_consequence_header)
			
		write.table(df,
			file = output_file,
			append = FALSE,
			quote = FALSE,
			sep = DELIMITER,
            	row.names = FALSE,
            	col.names = TRUE)
		
		return (invisible())
	}
)

#' @name vsdChoices
#' @title Create choice situations that can elicit violations of (first-order) stochastic dominance in decision makers, using Birbaum's (1997) recipe.
#' @description Create choice situations that can elicit violations of (first-order) stochastic dominance in decision makers, using Birbaum's (1997) recipe.
#' @details Given a binary gamble G0, this function creates a pair of three outcome gambles G+ and G-
#' and a pair of four outcome gambles GS+, GS- that can elicit vsd behaviour in decision makers. e.g.
#' 
#' G0 = (96, 0.9; 12, 0.1)
#' 
#' G+ = (12, 0.05; 14, 0.05; 96, 0.9) and G- = (12, 0.1; 90, 0.05; 96, 0.85)
#' 
#' where G+ dominates G0 and G- is dominated by G0.
#' 
#' GS+ = (12, 0.05; 14, 0.05; 96, 0.05; 96, 0.85) and GS- = (12, 0.05; 12, 0.05; 90, 0.05; 96, 0.85)
#' 
#' 
#' @examples
#' my_choices_list <- vsdChoices(x=12, y=96, p="0.1", q="0.9", x_plus=14, y_minus=90, r="0.05")
#' 
#' original_choice <- my_choices_list[[1]]
#' 
#' original_choice
#' 
#' pair_of_three_outcome_choices <- my_choices_list[[2]]
#' 
#' pair_of_three_outcome_choices
#' 
#' pair_of_four_outcome_choices <- my_choices_list[[3]]
#' 
#' pair_of_four_outcome_choices
#' 
#' @references
#' Figure 5, p. 475 from Birnbaum, M. H. (2008). New paradoxes of risky decision making. Psychological Review, 115(2), 463-501.
#' 
#' Birnbaum, M. H. (1997). Violations of monotonicity in judgment and decision making. In A. A. J. Marley (Ed.), Choice, decision, and measurement: Essays in honor of R. Duncan Luce (pp. 73-100). Mahwah, NJ: Erlbaum.
#' 
#' @usage
#' vsdChoices(x, y, p, q, x_plus, y_minus, r)
#' @param x numeric, x is one of the objective consequences in the original binary gamble G0.
#' @param y numeric, y is the other objective consequences in the original binary gamble G0.
#' @param p text, p is a probability string associated with the objective consequence x.
#' @param q text, q is a probability string associated with the objective consequence y.
#' @param x_plus numeric, x_plus
#' @param y_minus numeric, y_minus
#' @param r numeric, r the g_minus probability offset
#' @export
vsdChoices <- function(x, y, p, q, x_plus, y_minus, r)
{
	if ((x < 0) || (y < 0))
	{
		cat("y > x > 0\n")
		return (invisible())
	}
	else if (x > y)
	{
		cat("y > x > 0\n")
		return (invisible())		
	}
	
	np <- as.numeric(p)
	nq <- as.numeric(q)
	nr <- as.numeric(r)
	
	#g0
	choice_ids <- c(1, 1)	
	gamble_ids <- c(1, 1)
	outcome_ids <- c(1, 2)
	objective_consequences <- c(x, y)
	probability_strings <- c(as.character(np),
		as.character(nq))		
	g0 <- Choices(choice_ids=choice_ids,
		gamble_ids=gamble_ids, 		
		outcome_ids=outcome_ids, 
		objective_consequences=objective_consequences, 
		probability_strings=probability_strings)	
		
	#gplusminus
	choice_ids <- c(1, 1, 1, 1, 1, 1)
	gamble_ids <- c(1, 1, 1, 2, 2, 2)
	outcome_ids <- c(1, 2, 3, 1, 2, 3)
	objective_consequences <- c(x, x_plus, y, 
		x, y_minus, y)
	probability_strings <- c(		
		as.character(eval(parse(text="np-nr"))), 
		as.character(eval(parse(text="np-nr"))), 	
		as.character(nq), 
		
		as.character(np), 
		as.character(eval(parse(text="np-nr"))),		
		as.character(eval(parse(text="nq-nr"))))
	
	gplusminus <- Choices(choice_ids=choice_ids,
		gamble_ids=gamble_ids, 
		outcome_ids=outcome_ids, 
		objective_consequences=objective_consequences, 
		probability_strings=probability_strings)		
	
	#gsplusminus
	choice_ids <- c(1, 1, 1, 1, 1, 1, 1, 1)
	gamble_ids <- c(1, 1, 1, 1, 2, 2, 2, 2)
	outcome_ids <- c(1, 2, 3, 4, 1, 2, 3, 4)
	objective_consequences <- c(x, x_plus, y, y,
		x, x, y_minus, y)
		
	probability_strings <- c(		
		as.character(eval(parse(text="np-nr"))), 
		as.character(eval(parse(text="np-nr"))),
		as.character(eval(parse(text="np-nr"))), 		
		as.character(eval(parse(text="nq-nr"))), 		
	
		as.character(eval(parse(text="np-nr"))), 
		as.character(eval(parse(text="np-nr"))),
		as.character(eval(parse(text="np-nr"))), 		
		as.character(eval(parse(text="nq-nr"))))
	
	gsplusminus <- Choices(choice_ids=choice_ids,
		gamble_ids=gamble_ids, 
		outcome_ids=outcome_ids, 
		objective_consequences=objective_consequences, 
		probability_strings=probability_strings)
	
	
	my_list <- list("g0"=g0, "gplusminus"=gplusminus, "gsplusminus"=gsplusminus)

	return (my_list)	
}