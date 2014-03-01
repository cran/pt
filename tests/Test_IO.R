library("pt")

########################	
# These routines test saving and loading choices to/from external text files
# using the R session's temporary directory. 
########################

choice_ids <- c(1, 1, 1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 1, 2, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 4, 1, 2, 3, 4)
objective_consequences <- c(7, 7, 84, 90, 7, 10, 90, 90)
probability_strings <- c("0.1", "0.3", "0.3", "0.3", "0.1", "0.3", "0.3", "0.3")
my_choices <- Choices(choice_ids=choice_ids,
	gamble_ids=gamble_ids, 
	outcome_ids=outcome_ids, 
	objective_consequences=objective_consequences, 
	probability_strings=probability_strings)
my_choices

my_output_file <- paste(tempdir(), "\\", "test_save_choices.txt", sep="")

saveChoices(my_choices, 
	output_file=my_output_file,
	choice_id_header="choice_id",
	gamble_id_header="gamble_id",
	outcome_id_header="outcome_id",
	probability_header="probability",
	objective_consequence_header="objective_consequence",
	DELIMITER="\t")

rm(my_choices)

my_choices <- choicesFromFile(input_file=my_output_file,
	choice_id_header="choice_id",
	gamble_id_header="gamble_id",
	outcome_id_header="outcome_id",
	objective_consequence_header="objective_consequence",	
	probability_header="probability",
	DELIMITER="\t")
my_choices

# delete the temporary file
unlink(my_output_file)
# remove the object from the global environment
rm(my_output_file)
