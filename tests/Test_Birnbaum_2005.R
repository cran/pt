library("pt")

########################	
# Birnbaum, M. H. (2005). A comparison of five models that predict violations of first-order stochastic dominance in risky decision making. Journal of Risk and Uncertainty, 31(3), 263-287.
########################	

########################	
# RAM calculations p267
# F = (100, 0.5; 0, 0.5)
#   ~ 33.33
# G = (100, 0.05; 0, 0.95)
#   ~ 7.87
choice_ids <- c(1, 1, 1, 1)
gamble_ids <- c(1, 1, 2, 2)
outcome_ids <- c(1, 2, 1, 2)
objective_consequences <- c(100, 0, 100, 0)
probability_strings <- 
	c("1/2", "1/2", "0.05", "0.95")
my_choices <- Choices(choice_ids=choice_ids,
	gamble_ids=gamble_ids, 
	outcome_ids=outcome_ids, 
	objective_consequences=objective_consequences, 
	probability_strings=probability_strings)
my_choices

branch_weight_list <- list(
	c(1, 2))
my_pwf <- 
	ProbWeight(fun="power",
		par=c(alpha=0.6, beta=1))
my_utility <- Utility(fun="power", 
	par=c(alpha=1, beta=1, lambda=1))
compareRAM(my_choices, 
	branch_weight_list=branch_weight_list, 
	prob_weight=my_pwf, 
	utility=my_utility, 
	digits=4)

#   cid gid ev  ramu ce rp
# 1   1   1 50 33.33  33.33  16.67
# 2   1   2  5 7.872  7.872 -2.872


########################	
# RAM calculations p267
# G0 = (96, 0.9; 12, 0.1)
#   ~ 66.72
# G' = (96, 0.9; 12, 0.05; 12, 0.05)
#   ~ 56.62
# G'' = (96, 0.85; 96, 0.05; 12, 0.1)
#   ~ 64.22
choice_ids <- c(1, 1, 1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 2, 2, 2, 3, 3, 3)
outcome_ids <- c(1, 2, 1, 2, 3, 1, 2, 3)
objective_consequences <- c(96, 12, 96, 12, 12, 96, 96, 12)
probability_strings <- 
	c("0.9", "0.1", "0.9", "0.05", "0.05", "0.85", "0.05", "0.1")
my_choices <- Choices(choice_ids=choice_ids,
	gamble_ids=gamble_ids, 
	outcome_ids=outcome_ids, 
	objective_consequences=objective_consequences, 
	probability_strings=probability_strings)
my_choices

branch_weight_list <- list(
	c(1, 2),
	c(1, 2, 3))
my_pwf <- 
	ProbWeight(fun="power",
		par=c(alpha=0.6, beta=1))
my_utility <- Utility(fun="power", 
	par=c(alpha=1, beta=1, lambda=1))
compareRAM(my_choices, 
	branch_weight_list=branch_weight_list, 
	prob_weight=my_pwf, 
	utility=my_utility, 
	digits=4)

#   cid gid   ev  ramu ce rp
# 1   1   1 87.6 66.72  66.72  20.88
# 2   1   2 87.6 56.62  56.62  30.98
# 3   1   3 87.6 64.22  64.22  23.38


########################	
# RAM calculations p267
# G- = (96, 0.85; 90, 0.05; 12, 0.1)
#   ~ 63.23
# G+ = (96, 0.9; 14, 0.05; 12, 0.05)
#   ~ 56.99
# G- > G+
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(96, 90, 12, 96, 14, 12)
probability_strings <- 
	c("0.85", "0.05", "0.1", "0.9", "0.05", "0.05")
my_choices <- Choices(choice_ids=choice_ids,
	gamble_ids=gamble_ids, 
	outcome_ids=outcome_ids, 
	objective_consequences=objective_consequences, 
	probability_strings=probability_strings)
my_choices


branch_weight_list <- list(
	c(1, 2),
	c(1, 2, 3))
my_pwf <- 
	ProbWeight(fun="power",
		par=c(alpha=0.6, beta=1))
my_utility <- Utility(fun="power", 
	par=c(alpha=1, beta=1, lambda=1))
compareRAM(my_choices, 
	branch_weight_list=branch_weight_list, 
	prob_weight=my_pwf, 
	utility=my_utility, 
	digits=4)

#   cid gid   ev  ramu ce rp
# 1   1   1 87.3 63.23  63.23  24.07
# 2   1   2 87.7 56.99  56.99  30.71




########################	
# TAX calculations p268
# F = (100, 0.5; 0, 0.5)
#   ~ 33.33
# G = (100, 0.05; 0, 0.95)
#   ~ 7.53
choice_ids <- c(1, 1, 1, 1)
gamble_ids <- c(1, 1, 2, 2)
outcome_ids <- c(1, 2, 1, 2)
objective_consequences <- c(100, 0, 100, 0)
probability_strings <- 
	c("1/2", "1/2", "0.05", "0.95")
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

#   cid gid ev   tax ce  rp
# 1   1   1 50 33.33 33.33  16.67
# 2   1   2  5 7.529 7.529 -2.529


########################	
# TAX calculations p268
# G- = (96, 0.85; 90, 0.05; 12, 0.1)
#   ~ 63.10
# G+ = (96, 0.9; 14, 0.05; 12, 0.05)
#   ~ 45.77
# G- > G+
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(96, 90, 12, 96, 14, 12)
probability_strings <- 
	c("0.85", "0.05", "0.1", "0.9", "0.05", "0.05")
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
# 1   1   1 87.3  63.1  63.1  24.2
# 2   1   2 87.7 45.77 45.77 41.93


########################	
# GDU calculations p269
# F = (100, 0.5; 0, 0.5)
#   ~ 
# G = (100, 0.05; 0, 0.95)
#   ~ 
choice_ids <- c(1, 1, 1, 1)
gamble_ids <- c(1, 1, 2, 2)
outcome_ids <- c(1, 2, 1, 2)
objective_consequences <- c(100, 0, 100, 0)
probability_strings <- 
	c("1/2", "1/2", "0.05", "0.95")
my_choices <- Choices(choice_ids=choice_ids,
	gamble_ids=gamble_ids, 
	outcome_ids=outcome_ids, 
	objective_consequences=objective_consequences, 
	probability_strings=probability_strings)
my_choices

my_pwf <- 
	ProbWeight(fun="compound_invariance",
		par=c(alpha=0.542, beta=1.382))
my_utility <- Utility(fun="power", 
	par=c(alpha=1, beta=1, lambda=1))
compareGDU(my_choices, 
	prob_weight=my_pwf, 
	utility=my_utility,
	digits=4)

#   cid gid ev   gdu ce  rp
# 1   1   1 50 32.21 32.21  17.79
# 2   1   2  5 8.169 8.169 -3.169

########################	
# GDU calculations p269
# G- = (96, 0.85; 90, 0.05; 12, 0.1)
#   ~ 
# G+ = (96, 0.9; 14, 0.05; 12, 0.05)
#   ~ 
# G- > G+
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(96, 90, 12, 96, 14, 12)
probability_strings <- 
	c("0.85", "0.05", "0.1", "0.9", "0.05", "0.05")
my_choices <- Choices(choice_ids=choice_ids,
	gamble_ids=gamble_ids, 
	outcome_ids=outcome_ids, 
	objective_consequences=objective_consequences, 
	probability_strings=probability_strings)
my_choices

my_pwf <- 
	ProbWeight(fun="compound_invariance",
		par=c(alpha=0.542, beta=1.382))
my_utility <- Utility(fun="power", 
	par=c(alpha=1, beta=1, lambda=1))
compareGDU(my_choices, 
	prob_weight=my_pwf, 
	utility=my_utility,
	digits=4)

#   cid gid   ev   gdu ce  rp
# 1   1   1 87.3 111.5 111.5 -24.18
# 2   1   2 87.7 68.32 68.32  19.38


########################	
# PRT calculations p270
# F = (100, 0.5; 0, 0.5)
#   ~ 33.33 PRT
# G = (100, 0.05; 0, 0.95)
#   ~ 7.53 PRT
choice_ids <- c(1, 1, 1, 1)
gamble_ids <- c(1, 1, 2, 2)
outcome_ids <- c(1, 2, 1, 2)
objective_consequences <- c(100, 0, 100, 0)
probability_strings <- 
	c("1/2", "1/2", "0.05", "0.95")
my_choices <- Choices(choice_ids=choice_ids,
	gamble_ids=gamble_ids, 
	outcome_ids=outcome_ids, 
	objective_consequences=objective_consequences, 
	probability_strings=probability_strings)
my_choices

my_utility <- Utility(fun="power", 
	par=c(alpha=0.631, beta=0.631, lambda=1))
gamma <- 0.676
comparePRT(my_choices,
	utility=my_utility,
	gamma=gamma,
	digits=4)

#   cid gid ev  prtu ce rp
# 1   1   1 50 9.141  33.34  16.66
# 2   1   2  5 3.579  7.545 -2.545


########################	
# PRT calculations p270
# G- = (96, 0.85; 90, 0.05; 12, 0.1)
#   ~ 
# G+ = (96, 0.9; 14, 0.05; 12, 0.05)
#   ~ 
# G- > G+
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(96, 90, 12, 96, 14, 12)
probability_strings <- 
	c("0.85", "0.05", "0.1", "0.9", "0.05", "0.05")
my_choices <- Choices(choice_ids=choice_ids,
	gamble_ids=gamble_ids, 
	outcome_ids=outcome_ids, 
	objective_consequences=objective_consequences, 
	probability_strings=probability_strings)
my_choices

my_utility <- Utility(fun="power", 
	par=c(alpha=0.631, beta=0.631, lambda=1))
gamma <- 0.676
comparePRT(my_choices,
	utility=my_utility,
	gamma=gamma,
	digits=4)

#   cid gid   ev  prtu ce rp
# 1   1   1 87.3 15.43  76.43  10.87
# 2   1   2 87.7 14.19  66.96  20.74


########################	
# TAX calculations p282
# G3 = (100, 0.35; 0, 0.37; -95, 0.04; -97, 0.04, -100, 0.20)
#   ~ -55.8
# G4 = (100, 0.1; 99, 0.1; 96, 0.1; 0, 0.4; -100, 0.3)
#   ~ -14.7
# G4 > G3
choice_ids <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5)
objective_consequences <- c(100, 0, -95, -97, -100, 100, 99, 96, 0, -100)
probability_strings <- 
	c("0.35", "0.37", "0.04", "0.04", "0.2", "0.1", "0.1", "0.1", "0.4", "0.3")
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

#   cid gid   ev    tax  ce rp
# 1   1   1 7.32 -55.79 -55.79 63.11
# 2   1   2 -0.5 -14.68 -14.68 14.18