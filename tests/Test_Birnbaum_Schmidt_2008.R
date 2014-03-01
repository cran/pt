library("pt")

########################	
# Birnbaum, M. H., & Schmidt, U. (2008). An experimental investigation of violations of transitivity in choice under uncertainty. Journal of Risk and Uncertainty, 37(1), 77-91.
########################

########################	
#
# p.81
#
########################	
# A=(10, 0.4; 3, 0.3; 3, 0.3)
#   ~ 4.33 TAX (utility, not ce)
# B=(7.5, 0.4; 7.5, 0.3; 1, 0.3)
#   ~ 5.33 TAX (utility, not ce) ?an error? should be 4.362?
# C=(5, 0.4; 5, 0.3; 5, 0.3)
#   ~ 5.0 TAX (utility, not ce)
# B'=(7.5, 0.7; 1, 0.3)
#   ~ 3.79 TAX (utility, not ce)
# C'=(5, 0.7; 5, 0.3)
#   ~ 5.0 TAX (utility, not ce)
# A''=(10, 0.4; 3, 0.6)
#   ~ 5.01 TAX (utility, not ce)
# C''=(5, 0.4; 5, 0.6)
#   ~ 5.0 TAX (utility, not ce)
# A < B
# B' < C'
# C'' < A''
choice_ids <- c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3)
gamble_ids <- c(1, 1, 1, 2, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3, 1, 2, 1, 2, 1, 2, 1, 2)
objective_consequences <- c(10, 3, 3, 7.5, 7.5, 1, 
	7.5, 1, 5, 5,
	10, 3, 5, 5)
probability_strings <- 
	c("0.4", "0.3", "0.3", "0.4", "0.3", "0.3", 
		"0.7", "0.3", "0.7", "0.3",
		"0.4", "0.6", "0.4", "0.6")
my_choices <- Choices(choice_ids=choice_ids,
	gamble_ids=gamble_ids, 
	outcome_ids=outcome_ids, 
	objective_consequences=objective_consequences, 
	probability_strings=probability_strings)
my_choices

my_pwf <- 
	ProbWeight(fun="power",
		par=c(alpha=0.7, beta=1))
delta <- -1
my_utility <- Utility(fun="power", 
	par=c(alpha=1, beta=1, lambda=1))
compareTAX(my_choices, 
	prob_weight=my_pwf, 
	utility=my_utility,
	delta=delta,
	digits=4)

#   cid gid   ev   tax ce  rp
# 1   1   1  5.8 4.328 4.328  1.472
# 2   1   2 5.55 4.362 4.362  1.188
# 3   2   1 5.55 3.791 3.791  1.759
# 4   2   2    5     5     5      0
# 5   3   1  5.8 5.004 5.004 0.7956
# 6   3   2    5     5     5      0

# test gamble C
choice_ids <- c(1, 1, 1)
gamble_ids <- c(1, 1, 1)
outcome_ids <- c(1, 2, 3)
objective_consequences <- c(5, 5, 5)
probability_strings <- 
	c("0.4", "0.3", "0.3")
my_choices <- Choices(choice_ids=choice_ids,
	gamble_ids=gamble_ids, 
	outcome_ids=outcome_ids, 
	objective_consequences=objective_consequences, 
	probability_strings=probability_strings)
my_choices

my_pwf <- 
	ProbWeight(fun="power",
		par=c(alpha=0.7, beta=1))
delta <- -1
my_utility <- Utility(fun="power", 
	par=c(alpha=1, beta=1, lambda=1))
compareTAX(my_choices, 
	prob_weight=my_pwf, 
	utility=my_utility,
	delta=delta,
	digits=4)

#   cid gid ev tax ce rp
# 1   1   1  5   5     5     0