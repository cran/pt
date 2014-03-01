library("pt")

########################	
# Birnbaum, M. H., & Bahra, J. P. (2007). Gain-loss separability and coalescing in risky decision making. Management Science, 53(6), 1016-1028.
########################	


########################	
#
# Table 1, p.1017
# Test of gain-loss separability from Wu & Markle (2004) working paper.
#
########################	
# Choice problem 1, Table 1, p.1017
# F+ = (2000, 0.25; 800, 0.25; 0, 0.5)
#   ~ 497 TAX
#   ~ 601 PT (should be 604?)
# G+ = (1600, 0.25; 1200, 0.25; 0, 0.5)
#   ~ 552 TAX
#   ~ 551 PT
# G+ > F+ 
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(2000, 800, 0, 1600, 1200, 0)
probability_strings <- 
	c("0.25", "0.25", "0.5", "0.25", "0.25", "0.5")
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

#   cid gid  ev   tax ce rp
# 1   1   1 700 496.6 496.6 203.4
# 2   1   2 700 551.8 551.8 148.2


tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
tk_1992_positive_probWeight <- 
	ProbWeight(fun="Tversky_Kahneman_1992", 
		par=c(alpha=0.61))
tk_1992_negative_probWeight <- 
	ProbWeight(fun="Tversky_Kahneman_1992", 
		par=c(alpha=0.69))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=tk_1992_positive_probWeight,
	prob_weight_for_negative_outcomes=tk_1992_negative_probWeight,
	utility=tk_1992_utility, digits=4)

#   cid gid  ev    pt  ce  rp
# 1   1   1 700 280.2 604.2 95.85
# 2   1   2 700 258.5 551.3 148.7



########################	
# Choice problem 2, Table 1, p.1017
# F- = (0, 0.5; -800, 0.25; -1000, 0.25)
#   ~ -359 TAX
#   ~ -379 PT
# G- = (0, 0.5; -200, 0.25; -1600, 0.25)
#   ~ -276 TAX
#   ~ -437 PT
# G- > F- 
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(0, -800, -1000, 0, -200, -1600)
probability_strings <- 
	c("0.5", "0.25", "0.25", "0.5", "0.25", "0.25")
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

#   cid gid   ev    tax  ce  rp
# 1   1   1 -450 -358.7 -358.7 -91.33
# 2   1   2 -450 -275.9 -275.9 -174.1


tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
tk_1992_positive_probWeight <- 
	ProbWeight(fun="Tversky_Kahneman_1992", 
		par=c(alpha=0.61))
tk_1992_negative_probWeight <- 
	ProbWeight(fun="Tversky_Kahneman_1992", 
		par=c(alpha=0.69))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=tk_1992_positive_probWeight,
	prob_weight_for_negative_outcomes=tk_1992_negative_probWeight,
	utility=tk_1992_utility, digits=4)

#   cid gid   ev     pt   ce   rp
# 1   1   1 -450 -417.8 -378.6 -71.42
# 2   1   2 -450 -474.2 -437.2 -12.83



########################	
# Choice problem 3, Table 1, p.1017
# F = (2000, 0.25; 800, 0.25; -800, 0.25; -1000, 0.25)
#   ~ -280 TAX
#   ~ -107.2 PT
# G = (1600, 0.25; 1200, 0.25; -200, 0.25; -1600, 0.25)
#   ~ -300 TAX
#   ~ -178.6 PT
# F > G 
choice_ids <- c(1, 1, 1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 1, 2, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 4, 1, 2, 3, 4)
objective_consequences <- c(2000, 800, -800, -1000, 1600, 1200, -200, -1600)
probability_strings <- 
	c("0.25", "0.25", "0.25", "0.25", "0.25", "0.25", "0.25", "0.25")
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

#   cid gid  ev  tax ce rp
# 1   1   1 250 -280  -280   530
# 2   1   2 250 -300  -300   550


tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
tk_1992_positive_probWeight <- 
	ProbWeight(fun="Tversky_Kahneman_1992", 
		par=c(alpha=0.61))
tk_1992_negative_probWeight <- 
	ProbWeight(fun="Tversky_Kahneman_1992", 
		par=c(alpha=0.69))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=tk_1992_positive_probWeight,
	prob_weight_for_negative_outcomes=tk_1992_negative_probWeight,
	utility=tk_1992_utility, digits=4)

#   cid gid  ev     pt   ce  rp
# 1   1   1 250 -137.6 -107.2 357.2
# 2   1   2 250 -215.7 -178.6 428.6




########################	
#
# Table 2, p.1020
# New tests of violations of gain-loss separability 
#
########################	
# Choice problem 4, Table 2, p.1020
# F+ = (100, 0.25; 0, 0.25; 0, 0.5)
#   ~ 13.8 TAX
# G+ = (50, 0.25; 50, 0.25; 0, 0.5)
#   ~ 20.6 TAX
# G+ > F+
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(100, 0, 0, 50, 50, 0)
probability_strings <- 
	c("0.25", "0.25", "0.5", "0.25", "0.25", "0.5")
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

#   cid gid ev   tax ce rp
# 1   1   1 25 13.79 13.79 11.21
# 2   1   2 25 20.69 20.69 4.308



########################	
# Choice problem 5, Table 2, p.1020
# F- = (-0, 0.5; -50, 0.25; -50, 0.25)
#   ~ -20.6 TAX
# G- = (-0, 0.5; -0, 0.25; -100, 0.25)
#   ~ -13.8 TAX
# G- > F-
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(-0, -50, -50, -0, -0, -100)
probability_strings <- 
	c("0.5", "0.25", "0.25", "0.5", "0.25", "0.25")
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

#   cid gid  ev    tax  ce  rp
# 1   1   1 -25 -20.69 -20.69 -4.308
# 2   1   2 -25 -13.79 -13.79 -11.21



########################	
# Choice problem 6, Table 2, p.1020
# F = (100, 0.25; 0, 0.25; -50, 0.25; -50, 0.25)
#   ~ -25.0 TAX
# G = (50, 0.25; 50, 0.25; -0, 0.25; -100, 0.25)
#   ~ -25.0 TAX
# G ~ F
choice_ids <- c(1, 1, 1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 1, 2, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 4, 1, 2, 3, 4)
objective_consequences <- c(100, 0, -50, -50, 50, 50, -0, -100)
probability_strings <- 
	c("0.25", "0.25", "0.25", "0.25", "0.25", "0.25", "0.25", "0.25")
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
# 1   1   1  0 -25   -25    25
# 2   1   2  0 -25   -25    25



########################	
# Choice problem 7, Table 2, p.1020
# F' = (100, 0.25; 0, 0.25; -50, 0.5)
#   ~ -15.5 TAX
# G' = (50, 0.5; -0, 0.25; -100, 0.25)
#   ~ -34.5 TAX
# F' > G' 
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(100, 0, -50, 50, -0, -100)
probability_strings <- 
	c("0.25", "0.25", "0.5", "0.5", "0.25", "0.25")
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

#   cid gid ev    tax  ce rp
# 1   1   1  0 -15.51 -15.51 15.51
# 2   1   2  0 -34.49 -34.49 34.49




########################	
#
# Table 4, p.1021
# Tests of gain-loss separability and coalescing
#
########################	
# Choice problem 15, Table 4, p.1021
# F = (100, 0.25; 0, 0.25; 0, 0.5)
#   ~ 13.8 TAX
#   ~ 24.6 PT
# G = (50, 0.25; 50, 0.25; 0, 0.5)
#   ~ 20.6 TAX
#   ~ 18.7 PT
# G > F 
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(100, 0, 0, 50, 50, 0)
probability_strings <- 
	c("0.25", "0.25", "0.5", "0.25", "0.25", "0.5")
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

#   cid gid ev   tax ce rp
# 1   1   1 25 13.79 13.79 11.21
# 2   1   2 25 20.69 20.69 4.308


tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
tk_1992_positive_probWeight <- 
	ProbWeight(fun="Tversky_Kahneman_1992", 
		par=c(alpha=0.61))
tk_1992_negative_probWeight <- 
	ProbWeight(fun="Tversky_Kahneman_1992", 
		par=c(alpha=0.69))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=tk_1992_positive_probWeight,
	prob_weight_for_negative_outcomes=tk_1992_negative_probWeight,
	utility=tk_1992_utility, digits=4)

#   cid gid ev    pt  ce  rp
# 1   1   1 25 16.73 24.57 0.433
# 2   1   2 25 13.15 18.69 6.311



########################	
# Choice problem 13, Table 4, p.1021
# F = (-0, 0.5; -50, 0.25; -50, 0.25)
#   ~ -20.6 TAX
#   ~ -20.4 PT
# G = (-0, 0.5; -0, 0.25; -100, 0.25)
#   ~ -13.8 TAX
#   ~ -24.8 PT
# G > F 
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(-0, -50, -50, -0, -0, -100)
probability_strings <- 
	c("0.5", "0.25", "0.25", "0.5", "0.25", "0.25")
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

#   cid gid  ev    tax  ce  rp
# 1   1   1 -25 -20.69 -20.69 -4.308
# 2   1   2 -25 -13.79 -13.79 -11.21


tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
tk_1992_positive_probWeight <- 
	ProbWeight(fun="Tversky_Kahneman_1992", 
		par=c(alpha=0.61))
tk_1992_negative_probWeight <- 
	ProbWeight(fun="Tversky_Kahneman_1992", 
		par=c(alpha=0.69))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=tk_1992_positive_probWeight,
	prob_weight_for_negative_outcomes=tk_1992_negative_probWeight,
	utility=tk_1992_utility, digits=4)

#   cid gid  ev     pt   ce    rp
# 1   1   1 -25 -31.94 -20.38  -4.618
# 2   1   2 -25    -38 -24.83 -0.1664



########################	
# Choice problem 19, Table 4, p.1021
# F = (100, 0.25; 0, 0.25; -50, 0.25; -50, 0.25)
#   ~ -25.0 TAX
#   ~ -8.8 PT
# G = (50, 0.25; 50, 0.25; -0, 0.25; -100, 0.25)
#   ~ -25.0 TAX
#   ~ -15.3 PT
# G > F
choice_ids <- c(1, 1, 1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 1, 2, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 4, 1, 2, 3, 4)
objective_consequences <- c(100, 0, -50, -50, 50, 50, -0, -100)
probability_strings <- 
	c("0.25", "0.25", "0.25", "0.25", "0.25", "0.25", "0.25", "0.25")
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
# 1   1   1  0 -25   -25    25
# 2   1   2  0 -25   -25    25


tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
tk_1992_positive_probWeight <- 
	ProbWeight(fun="Tversky_Kahneman_1992", 
		par=c(alpha=0.61))
tk_1992_negative_probWeight <- 
	ProbWeight(fun="Tversky_Kahneman_1992", 
		par=c(alpha=0.69))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=tk_1992_positive_probWeight,
	prob_weight_for_negative_outcomes=tk_1992_negative_probWeight,
	utility=tk_1992_utility, digits=4)

#   cid gid ev     pt   ce  rp
# 1   1   1  0 -15.21 -8.771 8.771
# 2   1   2  0 -24.85 -15.33 15.33



########################	
# Choice problem 11, Table 4, p.1021
# F = (100, 0.25; 0, 0.25; -50, 0.5)
#   ~ -15.5 TAX
#   ~ -8.8 PT
# G = (50, 0.5; -0, 0.25; -100, 0.25)
#   ~ -34.5 TAX
#   ~ -15.3 PT
# F > G
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(100, 0, -50, 50, -0, -100)
probability_strings <- 
	c("0.25", "0.25", "0.5", "0.5", "0.25", "0.25")
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

#   cid gid ev    tax  ce rp
# 1   1   1  0 -15.51 -15.51 15.51
# 2   1   2  0 -34.49 -34.49 34.49


tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
tk_1992_positive_probWeight <- 
	ProbWeight(fun="Tversky_Kahneman_1992", 
		par=c(alpha=0.61))
tk_1992_negative_probWeight <- 
	ProbWeight(fun="Tversky_Kahneman_1992", 
		par=c(alpha=0.69))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=tk_1992_positive_probWeight,
	prob_weight_for_negative_outcomes=tk_1992_negative_probWeight,
	utility=tk_1992_utility, digits=4)

#   cid gid ev     pt   ce  rp
# 1   1   1  0 -15.21 -8.771 8.771
# 2   1   2  0 -24.85 -15.33 15.33



########################	
# Choice problem 17, Table 4, p.1021
# F = (100, 0.25; 0, 0.25; -0, 0.25; -100, 0.25)
#   ~ -30.0 TAX
#   ~ -12.8 PT
# G = (50, 0.25; 50, 0.25; -50, 0.25; -50, 0.25)
#   ~ -20.0 TAX
#   ~ -11.2 PT
# G > F
choice_ids <- c(1, 1, 1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 1, 2, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 4, 1, 2, 3, 4)
objective_consequences <- c(100, 0, -0, -100, 50, 50, -50, -50)
probability_strings <- 
	c("0.25", "0.25", "0.25", "0.25", "0.25", "0.25", "0.25", "0.25")
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
# 1   1   1  0 -30   -30    30
# 2   1   2  0 -20   -20    20


tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
tk_1992_positive_probWeight <- 
	ProbWeight(fun="Tversky_Kahneman_1992", 
		par=c(alpha=0.61))
tk_1992_negative_probWeight <- 
	ProbWeight(fun="Tversky_Kahneman_1992", 
		par=c(alpha=0.69))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=tk_1992_positive_probWeight,
	prob_weight_for_negative_outcomes=tk_1992_negative_probWeight,
	utility=tk_1992_utility, digits=4)

#   cid gid ev     pt   ce  rp
# 1   1   1  0 -21.27 -12.84 12.84
# 2   1   2  0 -18.79 -11.15 11.15



########################	
#
# Table 5, p.1022
# Additional tests of coalescing and gain-loss separability
#
########################	
# Choice problem 9, Table 5, p.1022
# F = (100, 0.25; 0, 0.75)
#   ~ 21.1 TAX
#   ~ 24.6 PT
# G = (50, 0.5; 0, 0.5)
#   ~ 16.7 TAX
#   ~ 18.7 PT
# F > G 
choice_ids <- c(1, 1, 1, 1)
gamble_ids <- c(1, 1, 2, 2)
outcome_ids <- c(1, 2, 1, 2)
objective_consequences <- c(100, 0, 50, 0)
probability_strings <- 
	c("0.25", "0.75", "0.5", "0.5")
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

#   cid gid ev   tax ce rp
# 1   1   1 25 21.11 21.11 3.887
# 2   1   2 25 16.67 16.67 8.333


tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
tk_1992_positive_probWeight <- 
	ProbWeight(fun="Tversky_Kahneman_1992", 
		par=c(alpha=0.61))
tk_1992_negative_probWeight <- 
	ProbWeight(fun="Tversky_Kahneman_1992", 
		par=c(alpha=0.69))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=tk_1992_positive_probWeight,
	prob_weight_for_negative_outcomes=tk_1992_negative_probWeight,
	utility=tk_1992_utility, digits=4)

#   cid gid ev    pt  ce  rp
# 1   1   1 25 16.73 24.57 0.433
# 2   1   2 25 13.15 18.69 6.311



########################	
# Choice problem 5(b), Table 5, p.1022 (coalesced version of problem 5, Table 2, p.1020)
# F = (-0, 0.5; -50, 0.5)
#   ~ -16.7 TAX
#   ~ -20.4 PT
# G = (-0, 0.75; -100, 0.25)
#   ~ -21.1 TAX
#   ~ -24.8 PT
# F > G 
choice_ids <- c(1, 1, 1, 1)
gamble_ids <- c(1, 1, 2, 2)
outcome_ids <- c(1, 2, 1, 2)
objective_consequences <- c(-0, -50, -0, -100)
probability_strings <- 
	c("0.5", "0.5", "0.75", "0.25")
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

#   cid gid  ev    tax  ce  rp
# 1   1   1 -25 -16.67 -16.67 -8.333
# 2   1   2 -25 -21.11 -21.11 -3.887


tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
tk_1992_positive_probWeight <- 
	ProbWeight(fun="Tversky_Kahneman_1992", 
		par=c(alpha=0.61))
tk_1992_negative_probWeight <- 
	ProbWeight(fun="Tversky_Kahneman_1992", 
		par=c(alpha=0.69))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=tk_1992_positive_probWeight,
	prob_weight_for_negative_outcomes=tk_1992_negative_probWeight,
	utility=tk_1992_utility, digits=4)

#   cid gid  ev     pt   ce    rp
# 1   1   1 -25 -31.94 -20.38  -4.618
# 2   1   2 -25    -38 -24.83 -0.1664







########################	
#
# Table 6, p.1022
# Tests of risk attitude, loss attitude, idempotence
#
########################	
# Choice problem 12, Table 6, p.1022
# R = (100, 0.5; 0, 0.5)
#   ~ 33.3 TAX
#   ~ 37.4 PT
# S = (50, 0.5; 50, 0.5)
#   ~ 50 TAX
#   ~ 50 PT
# S > R 
choice_ids <- c(1, 1, 1, 1)
gamble_ids <- c(1, 1, 2, 2)
outcome_ids <- c(1, 2, 1, 2)
objective_consequences <- c(100, 0, 50, 50)
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

#   cid gid ev   tax ce rp
# 1   1   1 50 33.33 33.33 16.67
# 2   1   2 50    50    50     0


tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
tk_1992_positive_probWeight <- 
	ProbWeight(fun="Tversky_Kahneman_1992", 
		par=c(alpha=0.61))
tk_1992_negative_probWeight <- 
	ProbWeight(fun="Tversky_Kahneman_1992", 
		par=c(alpha=0.69))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=tk_1992_positive_probWeight,
	prob_weight_for_negative_outcomes=tk_1992_negative_probWeight,
	utility=tk_1992_utility, digits=4)

#   cid gid ev    pt  ce                 rp
# 1   1   1 50 24.21 37.38                12.62
# 2   1   2 50 31.27    50 -0.00000000000001421



########################	
# Choice problem 16, Table 6, p.1022
# R = (100, 0.5; 0, 0.5)
#   ~ 33.3 TAX
#   ~ 37.4 PT
# S = (50, 1.0)
#   ~ 50 TAX
#   ~ 50 PT
# S > R
choice_ids <- c(1, 1, 1)
gamble_ids <- c(1, 1, 2)
outcome_ids <- c(1, 2, 1)
objective_consequences <- c(100, 0, 50)
probability_strings <- 
	c("0.5", "0.5", "1.0")
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

#   cid gid ev   tax ce rp
# 1   1   1 50 33.33 33.33 16.67
# 2   1   2 50    50    50     0


tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
tk_1992_positive_probWeight <- 
	ProbWeight(fun="Tversky_Kahneman_1992", 
		par=c(alpha=0.61))
tk_1992_negative_probWeight <- 
	ProbWeight(fun="Tversky_Kahneman_1992", 
		par=c(alpha=0.69))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=tk_1992_positive_probWeight,
	prob_weight_for_negative_outcomes=tk_1992_negative_probWeight,
	utility=tk_1992_utility, digits=4)

#   cid gid ev    pt  ce                 rp
# 1   1   1 50 24.21 37.38                12.62
# 2   1   2 50 31.27    50 -0.00000000000002132



########################	
# Choice problem 20, Table 6, p.1022
# R = (100, 0.5; 0, 0.5)
#   ~ 33.3 TAX
#   ~ 37.4 PT
# S = (45, 1.0)
#   ~ 45 TAX
#   ~ 45 PT
# S > R
choice_ids <- c(1, 1, 1)
gamble_ids <- c(1, 1, 2)
outcome_ids <- c(1, 2, 1)
objective_consequences <- c(100, 0, 45)
probability_strings <- 
	c("0.5", "0.5", "1.0")
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

#   cid gid ev   tax ce rp
# 1   1   1 50 33.33 33.33 16.67
# 2   1   2 45    45    45     0


tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
tk_1992_positive_probWeight <- 
	ProbWeight(fun="Tversky_Kahneman_1992", 
		par=c(alpha=0.61))
tk_1992_negative_probWeight <- 
	ProbWeight(fun="Tversky_Kahneman_1992", 
		par=c(alpha=0.69))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=tk_1992_positive_probWeight,
	prob_weight_for_negative_outcomes=tk_1992_negative_probWeight,
	utility=tk_1992_utility, digits=4)

#   cid gid ev    pt  ce                 rp
# 1   1   1 50 24.21 37.38                12.62
# 2   1   2 45  28.5    45 -0.00000000000001421



########################	
# Choice problem 6, Table 6, p.1022
# R = (-0, 0.5; -100, 0.5)
#   ~ -33.3 TAX
#   ~ -50 PT
# S = (-50, 0.5; -50, 0.5)
#   ~ -40.8 TAX
#   ~ -50 PT
# R > S
choice_ids <- c(1, 1, 1, 1)
gamble_ids <- c(1, 1, 2, 2)
outcome_ids <- c(1, 2, 1, 2)
objective_consequences <- c(-0, -100, -50, -50)
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

#   cid gid  ev    tax  ce  rp
# 1   1   1 -50 -33.33 -33.33 -16.67
# 2   1   2 -50    -50    -50      0


tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
tk_1992_positive_probWeight <- 
	ProbWeight(fun="Tversky_Kahneman_1992", 
		par=c(alpha=0.61))
tk_1992_negative_probWeight <- 
	ProbWeight(fun="Tversky_Kahneman_1992", 
		par=c(alpha=0.69))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=tk_1992_positive_probWeight,
	prob_weight_for_negative_outcomes=tk_1992_negative_probWeight,
	utility=tk_1992_utility, digits=4)

#   cid gid  ev     pt   ce                rp
# 1   1   1 -50 -58.78 -40.76              -9.236
# 2   1   2 -50 -70.35    -50 0.00000000000002842



########################	
# Choice problem 10, Table 6, p.1022
# R = (-0, 0.5; -100, 0.5)
#   ~ -33.3 TAX
#   ~ -50 PT
# S = (-50, 1.0)
#   ~ -40.8 TAX
#   ~ -50 PT
# R > S
choice_ids <- c(1, 1, 1)
gamble_ids <- c(1, 1, 2)
outcome_ids <- c(1, 2, 1)
objective_consequences <- c(-0, -100, -50)
probability_strings <- 
	c("0.5", "0.5", "1.0")
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

#   cid gid  ev    tax  ce  rp
# 1   1   1 -50 -33.33 -33.33 -16.67
# 2   1   2 -50    -50    -50      0


tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
tk_1992_positive_probWeight <- 
	ProbWeight(fun="Tversky_Kahneman_1992", 
		par=c(alpha=0.61))
tk_1992_negative_probWeight <- 
	ProbWeight(fun="Tversky_Kahneman_1992", 
		par=c(alpha=0.69))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=tk_1992_positive_probWeight,
	prob_weight_for_negative_outcomes=tk_1992_negative_probWeight,
	utility=tk_1992_utility, digits=4)

#   cid gid  ev     pt   ce                rp
# 1   1   1 -50 -58.78 -40.76              -9.236
# 2   1   2 -50 -70.35    -50 0.00000000000002842



########################	
# Choice problem 8, Table 6, p.1022
# test of loss aversion
# R = (100, 0.5; -100, 0.5)
#   ~ -33.3 TAX
#   ~ -22.3 PT
# S = (0, 0.5; -0, 0.5)
#   ~ 0 TAX
#   ~ 0 PT
# S > R
choice_ids <- c(1, 1, 1, 1)
gamble_ids <- c(1, 1, 2, 2)
outcome_ids <- c(1, 2, 1, 2)
objective_consequences <- c(100, -100, 0, -0)
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

#   cid gid ev    tax  ce rp
# 1   1   1  0 -33.33 -33.33 33.33
# 2   1   2  0      0      0     0


tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
tk_1992_positive_probWeight <- 
	ProbWeight(fun="Tversky_Kahneman_1992", 
		par=c(alpha=0.61))
tk_1992_negative_probWeight <- 
	ProbWeight(fun="Tversky_Kahneman_1992", 
		par=c(alpha=0.69))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=tk_1992_positive_probWeight,
	prob_weight_for_negative_outcomes=tk_1992_negative_probWeight,
	utility=tk_1992_utility, digits=4)

#   cid gid ev     pt  ce rp
# 1   1   1  0 -34.57 -22.3 22.3
# 2   1   2  0      0     0    0



########################	
# Choice problem 18, Table 6, p.1022
# tests of loss aversion
# R = (100, 0.5; -100, 0.5)
#   ~ -33.3 TAX
#   ~ -22.3 PT
# S = (0, 1.0)
#   ~ 0 TAX
#   ~ 0 PT
# R > S
# Note that choice problem 18b, Table 6, p.1022 has the same structure as choice problem 18 but is framed
# with a status quo reference point, resulting in S > R.
choice_ids <- c(1, 1, 1)
gamble_ids <- c(1, 1, 2)
outcome_ids <- c(1, 2, 1)
objective_consequences <- c(-0, -100, -50)
probability_strings <- 
	c("0.5", "0.5", "1.0")
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

#   cid gid  ev    tax  ce  rp
# 1   1   1 -50 -33.33 -33.33 -16.67
# 2   1   2 -50    -50    -50      0


tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
tk_1992_positive_probWeight <- 
	ProbWeight(fun="Tversky_Kahneman_1992", 
		par=c(alpha=0.61))
tk_1992_negative_probWeight <- 
	ProbWeight(fun="Tversky_Kahneman_1992", 
		par=c(alpha=0.69))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=tk_1992_positive_probWeight,
	prob_weight_for_negative_outcomes=tk_1992_negative_probWeight,
	utility=tk_1992_utility, digits=4)

#   cid gid  ev     pt   ce                rp
# 1   1   1 -50 -58.78 -40.76              -9.236
# 2   1   2 -50 -70.35    -50 0.00000000000002842



########################	
# Choice problem 21, Table 6, p.1022
# test of loss aversion (insurance scenario)
# R = (100, 0.5; -100, 0.5)
#   ~ -33.3 TAX
#   ~ -22.3 PT
# S = (-5, 1.0)
#   ~ -5 TAX
#   ~ -5 PT
# R > S
choice_ids <- c(1, 1, 1)
gamble_ids <- c(1, 1, 2)
outcome_ids <- c(1, 2, 1)
objective_consequences <- c(100, -100, -5)
probability_strings <- 
	c("0.5", "0.5", "1.0")
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

#   cid gid ev    tax  ce rp
# 1   1   1  0 -33.33 -33.33 33.33
# 2   1   2 -5     -5     -5     0


tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
tk_1992_positive_probWeight <- 
	ProbWeight(fun="Tversky_Kahneman_1992", 
		par=c(alpha=0.61))
tk_1992_negative_probWeight <- 
	ProbWeight(fun="Tversky_Kahneman_1992", 
		par=c(alpha=0.69))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=tk_1992_positive_probWeight,
	prob_weight_for_negative_outcomes=tk_1992_negative_probWeight,
	utility=tk_1992_utility, digits=4)

#   cid gid ev     pt  ce                  rp
# 1   1   1  0 -34.57 -22.3                  22.3
# 2   1   2 -5 -9.274    -5 0.0000000000000008882