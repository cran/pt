
setClass(
	Class = "Outcome",
	representation = representation
	(
		outcome_id = "numeric",
		position = "numeric",
		objective_consequence = "numeric",		
		probability = "numeric",
		# needed to save recurring floating point numbers to text files
		probability_string = "character",
		rank = "numeric",
		decision_weight = "numeric",
		subjective_value = "numeric",
		w = "numeric"
	),
	# check for input consistency when creating new Outcome objects using "new" constructor
	validity = function(object)
	{
		# inspector

		if(object@probability < 0 | object@probability > 1)
		{
			stop("probability falls outside range [0, 1].\n");
		}
		else
		{			
		}
		return (TRUE)
	}
)

########################	
# display functions
########################	


setMethod(f = "show",
	signature = "Outcome",
	definition = function(object)
	{
		cat("objective_consequence", "probability", "\n", sep = "\t")
		cat(object@objective_consequence, object@probability_string, "\n", sep = "\t")
	
	}
)



create_outcome <- function(outcome_id, 
	position, 
	objective_consequence, 
	probability_string, 
	rank, 
	decision_weight, 
	subjective_value, 
	w)
{
	new(Class = "Outcome",	
		outcome_id = outcome_id,
		position = position,			
		objective_consequence = objective_consequence,
		probability = eval(parse(text=probability_string)),
		probability_string = probability_string,
		rank = rank,
		decision_weight = decision_weight,
		subjective_value = subjective_value,
		w = w		
	)
  
}




########################
# outcome_id related functions
########################	

# declare a custom function to retrieve outcome_id
setGeneric(name = "get_outcome_id",
	def = function(object)
	{
		standardGeneric("get_outcome_id")
	}
)

# provide implementation of custom function to retrieve outcome_id
setMethod(f = "get_outcome_id",
	signature = "Outcome",
	definition = function(object)
	{
		return (object@outcome_id)
	}
)

# declare a custom function to assign outcome_id	
setGeneric(name = "set_outcome_id<-",
	def = function(object, value)
	{
		standardGeneric("set_outcome_id<-")
	}
)


setReplaceMethod(f = "set_outcome_id",
	signature = "Outcome",
	definition = function(object, value)
	{
		object@outcome_id <- value
		return (object)
	}
)	

########################
# position related functions
########################	

# declare a custom function to retrieve position
setGeneric(name = "get_position",
	def = function(object)
	{
		standardGeneric("get_position")
	}
)

# provide implementation of custom function to retrieve position
setMethod(f = "get_position",
	signature = "Outcome",
	definition = function(object)
	{
		return (object@position)
	}
)

# declare a custom function to assign position	
setGeneric(name = "set_position<-",
	def = function(object, value)
	{
		standardGeneric("set_position<-")
	}
)


setReplaceMethod(f = "set_position",
	signature = "Outcome",
	definition = function(object, value)
	{
		object@position <- value
		return (object)
	}
)	

########################
# objective_consequence related functions
########################

# declare a custom function to retrieve objective_consequence
setGeneric(name = "get_objective_consequence",
	def = function(object)
	{
		standardGeneric("get_objective_consequence")
	}
)


setMethod(f = "get_objective_consequence",
	signature = "Outcome",
	definition = function(object)
	{
		return (object@objective_consequence)
	}
)

# declare a custom function to assign objective consequence	
setGeneric(name = "set_objective_consequence<-",
	def = function(object, value)
	{
		standardGeneric("set_objective_consequence<-")
	}
)


setReplaceMethod(f = "set_objective_consequence",
	signature = "Outcome",
	definition = function(object, value)
	{
		object@objective_consequence <- value
		return (object)
	}
)	

########################
# probability related functions
########################	

# declare a custom function to retrieve probability_string
setGeneric(name = "get_probability_string",
	def = function(object)
	{
		standardGeneric("get_probability_string")
	}
)


setMethod(f = "get_probability_string",
	signature = "Outcome",
	definition = function(object)
	{
		return (object@probability_string)
	}
)

# declare a custom function to assign probability_string
setGeneric(name = "set_probability_string<-",
	def = function(object, value)
	{
		standardGeneric("set_probability_string<-")
	}
)


setReplaceMethod(f = "set_probability_string",
	signature = "Outcome",
	definition = function(object, value)
	{
		object@probability_string <- value
		object@probability <- eval(parse(text=value))
		# make sure set_probability is consistent, with probability in [0, 1]
		validObject(object)
		return (object)
	}
)	

# declare a custom function to retrieve probability
setGeneric(name = "get_probability",
	def = function(object)
	{
		standardGeneric("get_probability")
	}
)


setMethod(f = "get_probability",
	signature = "Outcome",
	definition = function(object)
	{
		return (object@probability)
	}
)

# declare a custom function to assign probability	
setGeneric(name = "set_probability<-",
	def = function(object, value)
	{
		standardGeneric("set_probability<-")
	}
)


setReplaceMethod(f = "set_probability",
	signature = "Outcome",
	definition = function(object, value)
	{
		object@probability <- value
		# make sure set_probability is consistent, with probability in [0, 1]
		validObject(object)
		return (object)
	}
)	

########################
# rank related functions
########################

# declare a custom function to retrieve rank
setGeneric(name = "get_rank",
	def = function(object)
	{
		standardGeneric("get_rank")
	}
)

# provide implementation of custom function to retrieve rank
setMethod(f = "get_rank",
	signature = "Outcome",
	definition = function(object)
	{
		return (object@rank)
	}
)

# declare a custom function to assign rank	
setGeneric(name = "set_rank<-",
	def = function(object, value)
	{
		standardGeneric("set_rank<-")
	}
)


setReplaceMethod(f = "set_rank",
	signature = "Outcome",
	definition = function(object, value)
	{
		object@rank <- value
		return (object)
	}
)	

########################
# decision weight related functions
########################

# declare a custom function to retrieve decision weight
setGeneric(name = "get_decision_weight",
	def = function(object)
	{
		standardGeneric("get_decision_weight")
	}
)


setMethod(f = "get_decision_weight",
	signature = "Outcome",
	definition = function(object)
	{
		return (object@decision_weight)
	}
)

# declare a custom function to assign decision weight	
setGeneric(name = "set_decision_weight<-",
	def = function(object, value)
	{
		standardGeneric("set_decision_weight<-")
	}
)


setReplaceMethod(f = "set_decision_weight",
	signature = "Outcome",
	definition = function(object, value)
	{
		object@decision_weight <- value
		return (object)
	}
)


########################
# subjective_value related functions
########################	

# declare a custom function to retrieve subjective value
setGeneric(name = "get_subjective_value",
	def = function(object)
	{
		standardGeneric("get_subjective_value")
	}
)

# provide implementation of custom function to retrieve value
setMethod(f = "get_subjective_value",
	signature = "Outcome",
	definition = function(object)
	{
		return (object@subjective_value)
	}
)

# declare a custom function to assign subjective_value	
setGeneric(name = "set_subjective_value<-",
	def = function(object, value)
	{
		standardGeneric("set_subjective_value<-")
	}
)


setReplaceMethod(f = "set_subjective_value",
	signature = "Outcome",
	definition = function(object, value)
	{
		object@subjective_value <- value
		return (object)
	}
)	

########################
# w related functions
########################

# declare a custom function to retrieve w
setGeneric(name = "get_w",
	def = function(object)
	{
		standardGeneric("get_w")
	}
)


setMethod(f = "get_w",
	signature = "Outcome",
	definition = function(object)
	{
		return (object@w)
	}
)

# declare a custom function to assign w	
setGeneric(name = "set_w<-",
	def = function(object, value)
	{
		standardGeneric("set_w<-")
	}
)


setReplaceMethod(f = "set_w",
	signature = "Outcome",
	definition = function(object, value)
	{
		object@w <- value
		return (object)
	}
)	