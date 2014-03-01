library("pt")

########################	
# Birnbaum, M. H. (2010). Testing lexicographic semiorders as models of decision making: Priority dominance, integration, interaction, and transitivity. Journal of Mathematical Psychology, 54(4), 363-386.
########################	

########################	
# Choice problem 11, Table 9, p.376
# R = (95, 0.01; 5, 0.99)
#   ~ 7.3 TAX
# S = (55, 0.01; 20, 0.99)
#   ~ 20.9 TAX
# S > R
choice_ids <- c(1, 1, 1, 1)
gamble_ids <- c(1, 1, 2, 2)
outcome_ids <- c(1, 2, 1, 2)
objective_consequences <- c(95, 5, 55, 20)
probability_strings <- 
	c("0.01", "0.99", "0.01", "0.99")
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

#   cid gid    ev   tax ce   rp
# 1   1   1   5.9 7.313 7.313  -1.413
# 2   1   2 20.35  20.9  20.9 -0.5494



########################	
# Choice problem 17, Table 9, p.376
# R = (95, 0.1; 5, 0.9)
#   ~ 15.6 TAX
# S = (55, 0.1; 20, 0.9)
#   ~ 24.1 TAX
# S > R
choice_ids <- c(1, 1, 1, 1)
gamble_ids <- c(1, 1, 2, 2)
outcome_ids <- c(1, 2, 1, 2)
objective_consequences <- c(95, 5, 55, 20)
probability_strings <- 
	c("0.1", "0.9", "0.1", "0.9")
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

#   cid gid   ev   tax ce   rp
# 1   1   1   14 15.61 15.61  -1.609
# 2   1   2 23.5 24.13 24.13 -0.6258



########################	
# Choice problem 21, Table 9, p.376
# R = (95, 0.5; 5, 0.5)
#   ~ 35.0 TAX
# S = (55, 0.5; 20, 0.5)
#   ~ 31.7 TAX
# R > S
choice_ids <- c(1, 1, 1, 1)
gamble_ids <- c(1, 1, 2, 2)
outcome_ids <- c(1, 2, 1, 2)
objective_consequences <- c(95, 5, 55, 20)
probability_strings <- 
	c("0.5", "0.5", "0.5", "0.5")
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

#   cid gid   ev   tax ce rp
# 1   1   1   50    35    35    15
# 2   1   2 37.5 31.67 31.67 5.833



########################	
# Choice problem 7, Table 9, p.376
# R = (95, 0.9; 5, 0.1)
#   ~ 54.4 TAX
# S = (55, 0.9; 20, 0.1)
#   ~ 39.2 TAX
# R > S
choice_ids <- c(1, 1, 1, 1)
gamble_ids <- c(1, 1, 2, 2)
outcome_ids <- c(1, 2, 1, 2)
objective_consequences <- c(95, 5, 55, 20)
probability_strings <- 
	c("0.9", "0.1", "0.9", "0.1")
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

#   cid gid   ev   tax ce rp
# 1   1   1   86 54.39 54.39 31.61
# 2   1   2 51.5 39.21 39.21 12.29



########################	
# Choice problem 3, Table 9, p.376
# R = (95, 0.99; 5, 0.01)
#   ~ 62.7 TAX
# S = (55, 0.99; 20, 0.01)
#   ~ 42.4 TAX
# R > S
choice_ids <- c(1, 1, 1, 1)
gamble_ids <- c(1, 1, 2, 2)
outcome_ids <- c(1, 2, 1, 2)
objective_consequences <- c(95, 5, 55, 20)
probability_strings <- 
	c("0.99", "0.01", "0.99", "0.01")
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

#   cid gid    ev   tax ce rp
# 1   1   1  94.1 62.69 62.69 31.41
# 2   1   2 54.65 42.43 42.43 12.22