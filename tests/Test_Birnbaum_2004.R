library("pt")

########################	
# Birnbaum, M. H. (2004). Causes of Allais common consequence paradoxes: An experimental dissection. Journal of Mathematical Psychology, 48(2), 87-106.
########################	



########################	
#
# Series A, Table 2, p.95
#
########################	
# Choice problem 6, Table 2, p.95
# R = (98, 0.1; 2, 0.9)
#   ~ 13.3 TAX
#   ~ 16.9 PT
# S = (40, 0.2; 2, 0.8)
#   ~ 9.0 TAX
#   ~ 10.7 PT
# R > S
choice_ids <- c(1, 1, 1, 1)
gamble_ids <- c(1, 1, 2, 2)
outcome_ids <- c(1, 2, 1, 2)
objective_consequences <- c(98, 2, 40, 2)
probability_strings <- 
	c("0.1", "0.9", "0.2", "0.8")
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
# 1   1   1 11.6 13.32 13.32 -1.716
# 2   1   2  9.6 8.962 8.962 0.6384


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

#   cid gid   ev    pt  ce   rp
# 1   1   1 11.6 12.03 16.89 -5.287
# 2   1   2  9.6  8.06 10.71 -1.114



########################	
# Choice problem 9, Table 2, p.95
# R = (98, 0.1; 2, 0.1; 2, 0.8)
#   ~ 9.6 TAX
#   ~ 16.9 PT
# S = (40, 0.1; 40, 0.1; 2, 0.8)
#   ~ 11.1 TAX
#   ~ 10.7 PT
# S > R
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(98, 2, 2, 40, 40, 2)
probability_strings <- 
	c("0.1", "0.1", "0.8", "0.1", "0.1", "0.8")
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
# 1   1   1 11.6 9.635 9.635  1.965
# 2   1   2  9.6 11.07 11.07 -1.466


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

#   cid gid   ev    pt  ce   rp
# 1   1   1 11.6 12.03 16.89 -5.287
# 2   1   2  9.6  8.06 10.71 -1.114



########################	
# Choice problem 12, Table 2, p.95
# R = (98, 0.1; 40, 0.8; 2, 0.1)
#   ~ 30.6 TAX
#   ~ 38.0 PT
# S = (40, 0.1; 40, 0.1; 2, 0.8)
#   ~ 40.0 TAX
#   ~ 40.0 PT
# S > R
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(98, 40, 2, 40, 40, 40)
probability_strings <- 
	c("0.1", "0.8", "0.1", "0.1", "0.8", "0.1")
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
# 1   1   1 42 30.58 30.58 11.42
# 2   1   2 40    40    40     0


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

#   cid gid ev    pt ce                 rp
# 1   1   1 42 24.56   38                3.995
# 2   1   2 40 25.69   40 -0.00000000000001421



########################	
# Choice problem 16, Table 2, p.95
# R = (98, 0.8; 98, 0.1; 2, 0.1)
#   ~ 62.6 TAX
#   ~ 67.6 PT
# S = (98, 0.8; 40, 0.1; 40, 0.1)
#   ~ 59.8 TAX
#   ~ 74.5 PT
# R > S
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(98, 98, 2, 98, 40, 40)
probability_strings <- 
	c("0.8", "0.1", "0.1", "0.8", "0.1", "0.1")
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
# 1   1   1 88.4 62.55 62.55 25.85
# 2   1   2 86.4 59.77 59.77 26.63


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

#   cid gid   ev    pt  ce  rp
# 1   1   1 88.4 40.76 67.59 20.81
# 2   1   2 86.4 44.42 74.52 11.88




########################	
# Choice problem 19, Table 2, p.95
# R = (98, 0.9; 2, 0.1)
#   ~ 54.7 TAX
#   ~ 67.6 PT
# S = (98, 0.8; 40, 0.2)
#   ~ 68.0 TAX
#   ~ 74.5 PT
# S > R
choice_ids <- c(1, 1, 1, 1)
gamble_ids <- c(1, 1, 2, 2)
outcome_ids <- c(1, 2, 1, 2)
objective_consequences <- c(98, 2, 98, 40)
probability_strings <- 
	c("0.9", "0.1", "0.8", "0.2")
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
# 1   1   1 88.4 54.68 54.68 33.72
# 2   1   2 86.4 68.04 68.04 18.36


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

#   cid gid   ev    pt  ce  rp
# 1   1   1 88.4 40.76 67.59 20.81
# 2   1   2 86.4 44.42 74.52 11.88



########################	
#
# Series B, Table 3, p.95
#
########################	
# Choice problem 10, Table 3, p.95
# S = (50, 0.15; 7, 0.85)
#   ~ 13.6 TAX
#   ~ 15.9 PT
# R = (100, 0.1; 7, 0.9)
#   ~ 18.0 TAX
#   ~ 22.1 PT
# S < R
choice_ids <- c(1, 1, 1, 1)
gamble_ids <- c(1, 1, 2, 2)
outcome_ids <- c(1, 2, 1, 2)
objective_consequences <- c(50, 7, 100, 7)
probability_strings <- 
	c("0.15", "0.85", "0.1", "0.9")
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
# 1   1   1 13.45 13.56 13.56 -0.1134
# 2   1   2  16.3 17.96 17.96  -1.663


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

#   cid gid    ev    pt  ce   rp
# 1   1   1 13.45 11.38 15.85 -2.404
# 2   1   2  16.3 15.23 22.08 -5.779



########################	
# Choice problem 17, Table 3, p.95
# S = (50, 0.1; 50, 0.05; 7, 0.85)
#   ~ 15.6 TAX
#   ~ 15.9 PT
# R = (100, 0.1; 7, 0.05; 7, 0.85)
#   ~ 14.6 TAX
#   ~ 22.1 PT
# R > S
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(50, 50, 7, 100, 7, 7)
probability_strings <- 
	c("0.1", "0.05", "0.85", "0.1", "0.05", "0.85")
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

#   cid gid    ev   tax ce  rp
# 1   1   1 13.45 15.56 15.56 -2.107
# 2   1   2  16.3 14.64 14.64  1.663


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

#   cid gid    ev    pt  ce   rp
# 1   1   1 13.45 11.38 15.85 -2.404
# 2   1   2  16.3 15.23 22.08 -5.779



########################	
# Choice problem 20, Table 3, p.95
# S = (50, 0.1; 50, 0.85; 50, 0.05)
#   ~ 50.0 TAX
#   ~ 50.0 PT
# R = (100, 0.1; 50, 0.85; 7, 0.05)
#   ~ 40.1 TAX
#   ~ 49.2 PT
# S > R
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(50, 50, 50, 100, 50, 7)
probability_strings <- 
	c("0.1", "0.85", "0.05", "0.1", "0.85", "0.05")
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

#   cid gid    ev  tax ce rp
# 1   1   1    50   50    50     0
# 2   1   2 52.85 40.1  40.1 12.75


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

#   cid gid    ev    pt  ce                 rp
# 1   1   1    50 31.27    50 -0.00000000000002132
# 2   1   2 52.85 30.84 49.23                3.621



########################	
# Choice problem 14, Table 3, p.95
# S = (100, 0.85; 50, 0.1; 50, 0.05)
#   ~ 68.4 TAX
#   ~ 82.2 PT
# R = (100, 0.85; 100, 0.1; 7, 0.05)
#   ~ 69.7 TAX
#   ~ 79.0 PT
# S < R
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(100, 50, 50, 100, 100, 7)
probability_strings <- 
	c("0.85", "0.1", "0.05", "0.85", "0.1", "0.05")
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
# 1   1   1  92.5 68.37 68.37 24.13
# 2   1   2 95.35  69.7  69.7 25.65


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

#   cid gid    ev    pt  ce  rp
# 1   1   1  92.5 48.44 82.23 10.27
# 2   1   2 95.35 46.79 79.05  16.3



########################	
# Choice problem 8, Table 3, p.95
# S = (100, 0.85; 50, 0.15)
#   ~ 75.7 TAX
#   ~ 82.2 PT
# R = (100, 0.95; 7, 0.05)
#   ~ 62.0 TAX
#   ~ 79.0 PT
# S > R
choice_ids <- c(1, 1, 1, 1)
gamble_ids <- c(1, 1, 2, 2)
outcome_ids <- c(1, 2, 1, 2)
objective_consequences <- c(100, 50, 100, 7)
probability_strings <- 
	c("0.85", "0.15", "0.95", "0.05")
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

#   cid gid    ev  tax ce rp
# 1   1   1  92.5 75.7  75.7  16.8
# 2   1   2 95.35   62    62 33.35


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

#   cid gid    ev    pt  ce  rp
# 1   1   1  92.5 48.44 82.23 10.27
# 2   1   2 95.35 46.79 79.05  16.3




########################	
#
# Table 4, p.96
# Violations of stochastic dominance and coalescing linked to event framing and event-splitting
# (SD violated if G- > G+)
#
########################	
# Choice problem 5, Table 4, p.96
# G+ = (96, 0.9; 14, 0.05; 12, 0.05)
#   ~ 45.8 TAX
#   ~ 70.3 PT
# G- = (96, 0.85; 90, 0.05; 12, 0.1)
#   ~ 63.1 TAX
#   ~ 69.7 PT
# G- > G+
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(96, 14, 12, 96, 90, 12)
probability_strings <- 
	c("0.9", "0.05", "0.05", "0.85", "0.05", "0.1")
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
# 1   1   1 87.7 45.77 45.77 41.93
# 2   1   2 87.3  63.1  63.1  24.2


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

#   cid gid   ev    pt  ce  rp
# 1   1   1 87.7 42.18 70.27 17.43
# 2   1   2 87.3  41.9 69.73 17.57



########################	
# Choice problem 11, Table 4, p.96
# G+ = (96, 0.85; 96, 0.05; 14, 0.05; 12, 0.05)
#   ~ 53.1 TAX
#   ~ 70.3 PT
# G- = (96, 0.85; 90, 0.05; 12, 0.05; 12, 0.05)
#   ~ 51.4 TAX
#   ~ 69.7 PT
# G+ > G-
choice_ids <- c(1, 1, 1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 1, 2, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 4, 1, 2, 3, 4)
objective_consequences <- c(96, 96, 14, 12, 96, 90, 12, 12)
probability_strings <- 
	c("0.85", "0.05", "0.05", "0.05", "0.85", "0.05", "0.05", "0.05")
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
# 1   1   1 87.7 53.06 53.06 34.64
# 2   1   2 87.3 51.38 51.38 35.92


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

#   cid gid   ev    pt  ce  rp
# 1   1   1 87.7 42.18 70.27 17.43
# 2   1   2 87.3  41.9 69.73 17.57



########################	
# Choice problem 15, Table 4, p.96
# G+ = (96, 0.9; 14, 0.05; 12, 0.05)
#   ~ 45.8 TAX
#   ~ 70.3 PT
# G- = (96, 0.85; 90, 0.05; 12, 0.1)
#   ~ 63.1 TAX
#   ~ 69.7 PT
# G- > G+
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(96, 14, 12, 96, 90, 12)
probability_strings <- 
	c("0.9", "0.05", "0.05", "0.85", "0.05", "0.1")
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
# 1   1   1 87.7 45.77 45.77 41.93
# 2   1   2 87.3  63.1  63.1  24.2


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

#   cid gid   ev    pt  ce  rp
# 1   1   1 87.7 42.18 70.27 17.43
# 2   1   2 87.3  41.9 69.73 17.57



########################	
# Choice problem 7, Table 4, p.96
# G+ = (99, 0.94; 8, 0.03; 6, 0.03)
#   ~ 46.0 TAX
#   ~ 76.2 PT
# G- = (99, 0.91; 96, 0.03; 6, 0.06)
#   ~ 66.6 TAX
#   ~ 75.9 PT
# G- > G+
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(99, 8, 6, 99, 96, 6)
probability_strings <- 
	c("0.94", "0.03", "0.03", "0.91", "0.03", "0.06")
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
# 1   1   1 93.48 45.96 45.96 47.52
# 2   1   2 93.33  66.6  66.6 26.73


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

#   cid gid    ev    pt  ce  rp
# 1   1   1 93.48 45.32 76.24 17.24
# 2   1   2 93.33 45.16 75.92 17.41



########################	
# Choice problem 13, Table 4, p.96
# G+ = (99, 0.91; 99, 0.03; 8, 0.03; 6, 0.03)
#   ~ 54.2 TAX
#   ~ 76.2 PT
# G- = (99, 0.91; 96, 0.03; 6, 0.03; 6, 0.03)
#   ~ 53.2 TAX
#   ~ 75.9 PT
# G+ > G-
choice_ids <- c(1, 1, 1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 1, 2, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 4, 1, 2, 3, 4)
objective_consequences <- c(99, 99, 8, 6, 99, 96, 6, 6)
probability_strings <- 
	c("0.91", "0.03", "0.03", "0.03", "0.91", "0.03", "0.03", "0.03")
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
# 1   1   1 93.48 54.23 54.23 39.25
# 2   1   2 93.33 53.17 53.17 40.16


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

#   cid gid    ev    pt  ce  rp
# 1   1   1 93.48 45.32 76.24 17.24
# 2   1   2 93.33 45.16 75.92 17.41



########################	
# Choice problem 18, Table 4, p.96
# G+ = (99, 0.94; 8, 0.03; 6, 0.03)
#   ~ 46.0 TAX
#   ~ 76.2 PT
# G- = (99, 0.91; 96, 0.03; 6, 0.06)
#   ~ 66.6 TAX
#   ~ 75.9 PT
# G- > G+
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(99, 8, 6, 99, 96, 6)
probability_strings <- 
	c("0.94", "0.03", "0.03", "0.91", "0.03", "0.06")
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
# 1   1   1 93.48 45.96 45.96 47.52
# 2   1   2 93.33  66.6  66.6 26.73


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

#   cid gid    ev    pt  ce  rp
# 1   1   1 93.48 45.32 76.24 17.24
# 2   1   2 93.33 45.16 75.92 17.41



