library("pt")

########################	
# Birnbaum, M. H. (2008). New paradoxes of risky decision making. Psychological Review, 115(2), 463-501.
########################	



########################	
#
# Coalescing tests: Choice problems 1.1-1.2
# Stochastic dominance tests: Choice problems 2,8.1-3.3,4
# Upper tail independence tests: Choice problems 5.1-5.2,6.1-6.2
#
########################	
# Choice problem 1.1, Table 1, p.474
# A = (100, 0.85; 50, 0.1; 50, 0.05)
#   ~ 68.4 TAX
#   ~ 82.2 PT
# B = (100, 0.85; 100, 0.1; 7, 0.05)
#   ~ 69.7 TAX
#   ~ 79.0 PT
# B > A
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
# Choice problem 1.2, Table 1, p.474
# A' = (100, 0.85; 50, 0.15)
#   ~ 75.7 TAX
#   ~ 82.2 PT
# B' = (100, 0.95; 7, 0.05)
#   ~ 62.0 TAX
#   ~ 79.0 PT
# A' > B'
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
# Choice problem 2,3.1, Table 1, p.474
# I = (96, 0.9; 14, 0.05; 12, 0.05)
#   ~ 45.8 TAX
#   ~ 70.3 PT
# J = (96, 0.85; 90, 0.05; 12, 0.1)
#   ~ 63.1 TAX
#   ~ 69.7 PT
# J > I
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
# Choice problem 3.2, Table 1, p.474
# M = (96, 0.85; 96, 0.05; 14, 0.05; 12, 0.05)
#   ~ 53.1 TAX
#   ~ 70.3 PT
# N = (96, 0.85; 90, 0.05; 12, 0.05; 12, 0.05)
#   ~ 51.4 TAX
#   ~ 69.7 PT
# M > N
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
# Choice problem 3.3, Table 1, p.474
# I' = (97, 0.9; 15, 0.05; 13, 0.05)
#   ~ 46.8 TAX
#   ~ 73.3 PT
# J' = (90, 0.85; 80, 0.05; 10, 0.1)
#   ~ 57.6 TAX
#   ~ 66.6 PT
# J' > I'
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(97, 15, 13, 90, 80, 10)
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
# 1   1   1 88.7 46.77 46.77 41.93
# 2   1   2 81.5 57.55 57.55 23.95

tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
linear_in_log_odds_probability_weighting <- 
	ProbWeight(fun="linear_in_log_odds", 
		par=c(alpha=0.61, beta=0.724))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=linear_in_log_odds_probability_weighting,
	prob_weight_for_negative_outcomes=linear_in_log_odds_probability_weighting,
	utility=tk_1992_utility, digits=4)

#   cid gid   ev    pt  ce  rp
# 1   1   1 88.7 43.78 73.31 15.39
# 2   1   2 81.5 40.23 66.59 14.91




########################	
# Choice problem 4, Table 1, p.474
# K = (96, 0.9; 14, 0.05; 12, 0.05)
#   ~ 45.8 TAX
#   ~ 72.3 PT
# L = (96, 0.25; 90, 0.05; 12, 0.7)
#   ~ 35.8 TAX
#   ~ 35.0 PT
# K > L
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(96, 14, 12, 96, 90, 12)
probability_strings <- 
	c("0.9", "0.05", "0.05", "0.25", "0.05", "0.7")
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
# 2   1   2 36.9  35.8  35.8 1.095

tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
linear_in_log_odds_probability_weighting <- 
	ProbWeight(fun="linear_in_log_odds", 
		par=c(alpha=0.61, beta=0.724))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=linear_in_log_odds_probability_weighting,
	prob_weight_for_negative_outcomes=linear_in_log_odds_probability_weighting,
	utility=tk_1992_utility, digits=4)

#   cid gid   ev    pt  ce  rp
# 1   1   1 87.7 43.24 72.27 15.43
# 2   1   2 36.9 22.87 35.04 1.863




########################	
# Choice problem 5.1, Table 1, p.474
# s = (92, 0.43; 68, 0.07; 0, 0.5)
#   ~ 32.3 TAX
#   ~ 33.4 PT
# t = (92, 0.48; 0, 0.52)
#   ~ 29.8 TAX
#   ~ 33.2 PT
# s > t
choice_ids <- c(1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2)
objective_consequences <- c(92, 68, 0, 92, 0)
probability_strings <- 
	c("0.43", "0.07", "0.5", "0.48", "0.52")
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
# 1   1   1 44.32 32.32 32.32    12
# 2   1   2 44.16 29.81 29.81 14.35

tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
linear_in_log_odds_probability_weighting <- 
	ProbWeight(fun="linear_in_log_odds", 
		par=c(alpha=0.61, beta=0.724))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=linear_in_log_odds_probability_weighting,
	prob_weight_for_negative_outcomes=linear_in_log_odds_probability_weighting,
	utility=tk_1992_utility, digits=4)

#   cid gid    ev    pt  ce  rp
# 1   1   1 44.32 21.94 33.43 10.89
# 2   1   2 44.16 21.82 33.23 10.93




########################	
# Choice problem 5.2, Table 1, p.474
# u = (97, 0.43; 68, 0.07; 0, 0.5)
#   ~ 33.4 TAX
#   ~ 35.1 PT
# v = (97, 0.43; 92, 0.05; 0, 0.52)
#   ~ 36.7 TAX
#   ~ 34.9 PT
# v > u
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(97, 68, 0, 97, 92, 0)
probability_strings <- 
	c("0.43", "0.07", "0.5", "0.43", "0.05", "0.52")
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
# 1   1   1 46.47 33.37 33.37  13.1
# 2   1   2 46.31 36.72 36.72 9.593

tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
linear_in_log_odds_probability_weighting <- 
	ProbWeight(fun="linear_in_log_odds", 
		par=c(alpha=0.61, beta=0.724))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=linear_in_log_odds_probability_weighting,
	prob_weight_for_negative_outcomes=linear_in_log_odds_probability_weighting,
	utility=tk_1992_utility, digits=4)

#   cid gid    ev    pt  ce  rp
# 1   1   1 46.47 22.91 35.11 11.36
# 2   1   2 46.31 22.79  34.9 11.41




########################	
# Choice problem 6.1, Table 1, p.474
# w = (110, 0.8; 44, 0.1; 40, 0.1)
#   ~ 65.0 TAX
#   ~ 83.5 PT
# x = (110, 0.8; 96, 0.1; 10, 0.1)
#   ~ 69.0 TAX
#   ~ 79.9 PT
# x > w
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(110, 44, 40, 110, 96, 10)
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
# 1   1   1 96.4 65.03 65.03 31.37
# 2   1   2 98.6 69.01 69.01 29.59

tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
linear_in_log_odds_probability_weighting <- 
	ProbWeight(fun="linear_in_log_odds", 
		par=c(alpha=0.61, beta=0.724))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=linear_in_log_odds_probability_weighting,
	prob_weight_for_negative_outcomes=linear_in_log_odds_probability_weighting,
	utility=tk_1992_utility, digits=4)

#   cid gid   ev    pt  ce  rp
# 1   1   1 96.4 49.09 83.48 12.92
# 2   1   2 98.6 47.22 79.88 18.72




########################	
# Choice problem 6.2, Table 1, p.474
# y = (96, 0.8; 44, 0.1; 40, 0.1)
#   ~ 60.3 TAX
#   ~ 75.0 PT
# z = (96, 0.9; 10, 0.1)
#   ~ 57.2 TAX
#   ~ 71.4 PT
# y > z
choice_ids <- c(1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2)
objective_consequences <- c(96, 44, 40, 96, 10)
probability_strings <- 
	c("0.8", "0.1", "0.1", "0.9", "0.1")
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
# 1   1   1 85.2 60.25 60.25 24.95
# 2   1   2 87.4  57.2  57.2  30.2

tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
linear_in_log_odds_probability_weighting <- 
	ProbWeight(fun="linear_in_log_odds", 
		par=c(alpha=0.61, beta=0.724))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=linear_in_log_odds_probability_weighting,
	prob_weight_for_negative_outcomes=linear_in_log_odds_probability_weighting,
	utility=tk_1992_utility, digits=4)

#   cid gid   ev    pt  ce  rp
# 1   1   1 85.2 44.65 74.96 10.24
# 2   1   2 87.4 42.79 71.41 15.99



########################	
#
# UCI tests: Choice problems 7.1-7.2
# LCI tests: Choice problems 8.1-8.2
# Allais common consequence tests: Choice problems 9.1-9.3
# Allais common ratio tests: Choice problems 10.1-10.5
#
########################	
# Choice problem 7.1, Table 2, p.480
# M = (110, 0.8; 44, 0.1; 40, 0.1)
#   ~ 65.0 TAX
#   ~ 83.5 PT
# N = (110, 0.8; 98, 0.1; 10, 0.1)
#   ~ 69.6 TAX
#   ~ 80.1 PT
# N > M
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(110, 44, 40, 110, 98, 10)
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
# 1   1   1 96.4 65.03 65.03 31.37
# 2   1   2 98.8 69.59 69.59 29.21


tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
linear_in_log_odds_probability_weighting <- 
	ProbWeight(fun="linear_in_log_odds", 
		par=c(alpha=0.61, beta=0.724))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=linear_in_log_odds_probability_weighting,
	prob_weight_for_negative_outcomes=linear_in_log_odds_probability_weighting,
	utility=tk_1992_utility, digits=4)

#   cid gid   ev    pt  ce  rp
# 1   1   1 96.4 49.09 83.48 12.92
# 2   1   2 98.8 47.33 80.09 18.71



########################	
# Choice problem 7.2, Table 2, p.480
# O = (98, 0.8; 40, 0.2)
#   ~ 68.0 TAX
#   ~ 75.7 PT
# P = (98, 0.9; 10, 0.1)
#   ~ 58.3 TAX
#   ~ 72.8 PT
# O > P
choice_ids <- c(1, 1, 1, 1)
gamble_ids <- c(1, 1, 2, 2)
outcome_ids <- c(1, 2, 1, 2)
objective_consequences <- c(98, 40, 98, 10)
probability_strings <- 
	c("0.8", "0.2", "0.9", "0.1")
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
# 1   1   1 86.4 68.04 68.04 18.36
# 2   1   2 89.2 58.29 58.29 30.91

tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
linear_in_log_odds_probability_weighting <- 
	ProbWeight(fun="linear_in_log_odds", 
		par=c(alpha=0.61, beta=0.724))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=linear_in_log_odds_probability_weighting,
	prob_weight_for_negative_outcomes=linear_in_log_odds_probability_weighting,
	utility=tk_1992_utility, digits=4)

#   cid gid   ev    pt  ce  rp
# 1   1   1 86.4 45.05 75.72 10.68
# 2   1   2 89.2 43.53 72.83 16.37



########################	
# Choice problem 8.1, Table 2, p.480
# Q = (96, 0.05; 12, 0.05; 3, 0.9)
#   ~ 8.8 TAX
#   ~ 11.6 PT
# R = (52, 0.05; 48, 0.05; 3, 0.9)
#   ~ 10.3 TAX
#   ~ 9.5 PT
# R > Q
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(96, 12, 3, 52, 48, 3)
probability_strings <- 
	c("0.05", "0.05", "0.9", "0.05", "0.05", "0.9")
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

#   cid gid  ev   tax ce   rp
# 1   1   1 8.1 8.804 8.804 -0.7037
# 2   1   2 7.7 10.27 10.27  -2.568


tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
linear_in_log_odds_probability_weighting <- 
	ProbWeight(fun="linear_in_log_odds", 
		par=c(alpha=0.61, beta=0.724))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=linear_in_log_odds_probability_weighting,
	prob_weight_for_negative_outcomes=linear_in_log_odds_probability_weighting,
	utility=tk_1992_utility, digits=4)

#   cid gid  ev    pt  ce   rp
# 1   1   1 8.1 8.628 11.58 -3.476
# 2   1   2 7.7 7.253 9.502 -1.802



########################	
# Choice problem 8.2, Table 2, p.480
# S = (96, 0.05; 12, 0.95)
#   ~ 18.3 TAX
#   ~ 19.9 PT
# T = (52, 0.1; 12, 0.9)
#   ~ 16.7 TAX
#   ~ 17.9 PT
# S > T
choice_ids <- c(1, 1, 1, 1)
gamble_ids <- c(1, 1, 2, 2)
outcome_ids <- c(1, 2, 1, 2)
objective_consequences <- c(96, 12, 52, 12)
probability_strings <- 
	c("0.05", "0.95", "0.1", "0.9")
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
# 1   1   1 16.2 18.32 18.32  -2.124
# 2   1   2   16 16.72 16.72 -0.7151

tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
linear_in_log_odds_probability_weighting <- 
	ProbWeight(fun="linear_in_log_odds", 
		par=c(alpha=0.61, beta=0.724))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=linear_in_log_odds_probability_weighting,
	prob_weight_for_negative_outcomes=linear_in_log_odds_probability_weighting,
	utility=tk_1992_utility, digits=4)

#   cid gid   ev    pt  ce   rp
# 1   1   1 16.2  13.9 19.91 -3.709
# 2   1   2   16 12.64 17.87  -1.87



########################	
# Choice problem 9.1, Table 2, p.480
# A = (1000000, 1.0)
#   ~ 1000k TAX
#   ~ 1000k TAX for u(x) = x^0.8 
#   ~ 1000k PT
# B = (2000000, 0.1; 1000000, 0.89; 2, 0.01)
#   ~ 810k TAX
#   ~ 742k TAX for u(x) = x^0.8 
#   ~ 1065k PT
# A > B
choice_ids <- c(1, 1, 1, 1)
gamble_ids <- c(1, 2, 2, 2)
outcome_ids <- c(1, 1, 2, 3)
objective_consequences <- c(1000000, 2000000, 1000000, 2)
probability_strings <- 
	c("1.0", "0.1", "0.89", "0.01")
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

#   cid gid      ev     tax   ce  rp
# 1   1   1 1000000 1000000 1000000      0
# 2   1   2 1090000  810212  810212 279788

my_pwf <- 
	ProbWeight(fun="power",
		par=c(alpha=0.7, beta=1))
delta <- -1
my_utility <- Utility(fun="power", 
	par=c(alpha=0.8, beta=0.8, lambda=1))
compareTAX(my_choices, 
	prob_weight=my_pwf, 
	utility=my_utility,
	delta=delta,
	digits=4)

#   cid gid      ev   tax   ce            rp
# 1   1   1 1000000 63096 1000000 -0.0000000008149
# 2   1   2 1090000 49718  742401           347599

tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
linear_in_log_odds_probability_weighting <- 
	ProbWeight(fun="linear_in_log_odds", 
		par=c(alpha=0.61, beta=0.724))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=linear_in_log_odds_probability_weighting,
	prob_weight_for_negative_outcomes=linear_in_log_odds_probability_weighting,
	utility=tk_1992_utility, digits=4)

# cid gid      ev     pt    ce            rp
# 1   1   1 1000000 190546 1000000 -0.000000001397
# 2   1   2 1090000 201336 1064590           25410



########################	
# Choice problem 9.2, Table 2, p.480
# C = (1000000, 0.11; 2, 0.89)
#   ~ 125k TAX
#   ~ 75k TAX for u(x) = x^0.8 
#   ~ 132k PT
# D = (2000000, 0.1; 2, 0.9)
#   ~ 236k TAX
#   ~ 138k TAX for u(x) = x^0.8 
#   ~ 248k PT
# D > C
choice_ids <- c(1, 1, 1, 1)
gamble_ids <- c(1, 1, 2, 2)
outcome_ids <- c(1, 2, 1, 2)
objective_consequences <- c(1000000, 2, 2000000, 2)
probability_strings <- 
	c("0.11", "0.89", "0.1", "0.9")
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

#   cid gid     ev    tax  ce  rp
# 1   1   1 110002 125288 125288 -15286
# 2   1   2 200002 235759 235759 -35757

my_pwf <- 
	ProbWeight(fun="power",
		par=c(alpha=0.7, beta=1))
delta <- -1
my_utility <- Utility(fun="power", 
	par=c(alpha=0.8, beta=0.8, lambda=1))
compareTAX(my_choices, 
	prob_weight=my_pwf, 
	utility=my_utility,
	delta=delta,
	digits=4)

#   cid gid     ev   tax  ce rp
# 1   1   1 110002  7907  74556 35445
# 2   1   2 200002 12951 138162 61840

tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
linear_in_log_odds_probability_weighting <- 
	ProbWeight(fun="linear_in_log_odds", 
		par=c(alpha=0.61, beta=0.724))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=linear_in_log_odds_probability_weighting,
	prob_weight_for_negative_outcomes=linear_in_log_odds_probability_weighting,
	utility=tk_1992_utility, digits=4)

#   cid gid     ev    pt   ce   rp
# 1   1   1 110002 32055 131926 -21924
# 2   1   2 200002 55872 248052 -48050



########################	
# Choice problem 9.3, Table 2, p.480
# X = (1000000, 0.1; 1000000, 0.01; 2, 0.89)
#   ~ 154k TAX
#   ~ 97k TAX for u(x) = x^0.8 
#   ~ 132k PT
# Y = (2000000, 0.1; 2, 0.01; 2, 0.89)
#   ~ 172k TAX
#   ~ 93k TAX for u(x) = x^0.8 
#   ~ 248k PT
# X > Y
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(1000000, 1000000, 2, 2000000, 2, 2)
probability_strings <- 
	c("0.1", "0.01", "0.89", "0.1", "0.01", "0.89")
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

#   cid gid     ev    tax  ce  rp
# 1   1   1 110002 154612 154612 -44610
# 2   1   2 200002 171859 171859  28143

my_pwf <- 
	ProbWeight(fun="power",
		par=c(alpha=0.7, beta=1))
delta <- -1
my_utility <- Utility(fun="power", 
	par=c(alpha=0.8, beta=0.8, lambda=1))
compareTAX(my_choices, 
	prob_weight=my_pwf, 
	utility=my_utility,
	delta=delta,
	digits=4)

#   cid gid     ev  tax ce  rp
# 1   1   1 110002 9757 96968  13034
# 2   1   2 200002 9441 93066 106935

tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
linear_in_log_odds_probability_weighting <- 
	ProbWeight(fun="linear_in_log_odds", 
		par=c(alpha=0.61, beta=0.724))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=linear_in_log_odds_probability_weighting,
	prob_weight_for_negative_outcomes=linear_in_log_odds_probability_weighting,
	utility=tk_1992_utility, digits=4)

#   cid gid     ev    pt   ce   rp
# 1   1   1 110002 32055 131926 -21924
# 2   1   2 200002 55872 248052 -48050



########################	
# Choice problem 10.1, Table 2, p.480
# E = (98, 0.1; 2, 0.9)
#   ~ 13.3 TAX
#   ~ 16.9 PT
# F = (40, 0.2; 2, 0.8)
#   ~ 9.0 TAX
#   ~ 10.7 PT
# E > F
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
# Choice problem 10.2, Table 2, p.480
# G = (98, 0.1; 2, 0.1; 2, 0.8)
#   ~ 9.6 TAX
#   ~ 16.9 PT
# H = (40, 0.1; 40, 0.1; 2, 0.8)
#   ~ 11.1 TAX
#   ~ 10.7 PT
# H > G
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
# Choice problem 10.3, Table 2, p.480
# I = (98, 0.1; 40, 0.8; 2, 0.1)
#   ~ 30.6 TAX
#   ~ 38.0 PT
# J = (40, 0.1; 40, 0.8; 40, 0.1)
#   ~ 40.0 TAX
#   ~ 40.0 PT
# J > I
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
# Choice problem 10.4, Table 2, p.480
# K = (98, 0.8; 98, 0.1; 2, 0.1)
#   ~ 62.6 TAX
#   ~ 67.6 PT
# L = (98, 0.8; 40, 0.1; 40, 0.1)
#   ~ 59.8 TAX
#   ~ 74.5 PT
# K > L
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
# Choice problem 10.5, Table 2, p.480
# M = (98, 0.9; 2, 0.1)
#   ~ 54.7 TAX
#   ~ 67.6 PT
# N = (98, 0.8; 40, 0.2)
#   ~ 68.0 TAX
#   ~ 74.5 PT
# N > M
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
# GLS tests: Choice problems 12.1-12.8
#
########################	
# Choice problem 12.1, Table 5, p.484
# A+ = (2000, 0.25; 800, 0.25; 0, 0.5)
#   ~ 497 TAX
#   ~ 601 PT
# B+ = (1600, 0.25; 1200, 0.25; 0, 0.5)
#   ~ 552 TAX
#   ~ 551 PT
# B+ > A+
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
# Choice problem 12.2, Table 5, p.484
# A- = (-0, 0.5; -800, 0.25; -1000, 0.25)
#   ~ -358 TAX
#   ~ -379 PT
# B- = (-0, 0.5; -200, 0.25; -1600, 0.25)
#   ~ -276 TAX
#   ~ -437 PT
# B- > A-
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(-0, -800, -1000, -0, -200, -1600)
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
# Choice problem 12.3, Table 5, p.484
# A = (2000, 0.25; 800, 0.25; -800, 0.25; -1000, 0.25)
#   ~ -280 TAX
#   ~ -107 PT
# B = (1600, 0.25; 1200, 0.25; -200, 0.25; -1600, 0.25)
#   ~ -300 TAX
#   ~ -179 PT
# A > B
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
# Choice problem 12.4, Table 5, p.484
# F+ = (100, 0.25; 0, 0.25; 0, 0.5)
#   ~ 14 TAX
#   ~ 25 PT
# G+ = (50, 0.25; 50, 0.25; 0, 0.5)
#   ~ 21 TAX
#   ~ 19 PT
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
# Choice problem 12.5, Table 5, p.484
# F- = (-0, 0.5; -50, 0.25; -50, 0.25)
#   ~ -21 TAX
#   ~ -20 PT
# G- = (-0, 0.5; -0, 0.25; -100, 0.25)
#   ~ -14 TAX
#   ~ -25 PT
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
# Choice problem 12.6, Table 5, p.484
# F = (100, 0.25; 0, 0.25; -50, 0.25; -50, 0.25)
#   ~ -25 TAX
#   ~ -9 PT
# G = (50, 0.25; 50, 0.25; -0, 0.25; -100, 0.25)
#   ~ -25 TAX
#   ~ -15 PT
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
# Choice problem 12.7, Table 5, p.484
# F'-' = (100, 0.25; 0, 0.25; -50, 0.5)
#   ~ -16 TAX
#   ~ -9 PT
# G'-' = (50, 0.5; -0, 0.25; -100, 0.25)
#   ~ -34 TAX
#   ~ -15 PT
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
# Choice problem 12.8, Table 5, p.484
# H = (100, 0.25; 0, 0.25; -50, 0.25; -50, 0.25)
#   ~ -30 TAX
#   ~ -13 PT
# I = (50, 0.25; 50, 0.25; -0, 0.25; -100, 0.25)
#   ~ -20 TAX
#   ~ -11 PT
# I > H 
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
# RBI tests: Choice problems 13.1, 13.2
# 4-DI tests: Choice problems 14.1, 14.2
#
########################	
# Choice problem 13.1, Table 6, p.486
# S = (44, 0.25; 40, 0.25; 5, 0.5)
#   ~ 20.0 TAX
#   ~ 19.7 PT
# R = (98, 0.25; 10, 0.25; 5, 0.5)
#   ~ 19.2 TAX
#   ~ 28.1 PT
# S > R
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(44, 40, 5, 98, 10, 5)
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

#   cid gid   ev   tax ce rp
# 1   1   1 23.5 20.04 20.04 3.463
# 2   1   2 29.5 19.21 19.21 10.29

tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
linear_in_log_odds_probability_weighting <- 
	ProbWeight(fun="linear_in_log_odds", 
		par=c(alpha=0.61, beta=0.724))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=linear_in_log_odds_probability_weighting,
	prob_weight_for_negative_outcomes=linear_in_log_odds_probability_weighting,
	utility=tk_1992_utility, digits=4)

#   cid gid   ev    pt  ce  rp
# 1   1   1 23.5 13.79 19.72  3.78
# 2   1   2 29.5 18.81 28.06 1.442



########################	
# Choice problem 13.2, Table 6, p.486
# S' = (111, 0.5; 44, 0.25; 40, 0.25)
#   ~ 57.2 TAX
#   ~ 69.5 PT
# R' = (111, 0.5; 98, 0.25; 10, 0.25)
#   ~ 60.7 TAX
#   ~ 64.3 PT
# R' > S'
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(111, 44, 40, 111, 98, 10)
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

#   cid gid   ev   tax ce rp
# 1   1   1 76.5 57.19 57.19 19.31
# 2   1   2 82.5  60.7  60.7  21.8

tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
linear_in_log_odds_probability_weighting <- 
	ProbWeight(fun="linear_in_log_odds", 
		par=c(alpha=0.61, beta=0.724))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=linear_in_log_odds_probability_weighting,
	prob_weight_for_negative_outcomes=linear_in_log_odds_probability_weighting,
	utility=tk_1992_utility, digits=4)

#   cid gid   ev    pt  ce  rp
# 1   1   1 76.5 41.77 69.48 7.022
# 2   1   2 82.5 39.01  64.3  18.2




########################	
# Choice problem 14.1, Table 6, p.486
# S = (110, 0.01; 49, 0.2; 45, 0.2; 4, 0.59)
#   ~ 21.7 TAX
#   ~ 20.9 PT
# R = (110, 0.01; 97, 0.2; 11, 0.2; 4, 0.59)
#   ~ 20.6 TAX
#   ~ 25.1 PT
# S > R
choice_ids <- c(1, 1, 1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 1, 2, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 4, 1, 2, 3, 4)
objective_consequences <- c(110, 49, 45, 4, 110, 97, 11, 4)
probability_strings <- 
	c("0.01", "0.2", "0.2", "0.59", "0.01", "0.2", "0.2", "0.59")
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
# 1   1   1 22.26  21.7  21.7 0.5595
# 2   1   2 25.06 20.56 20.56  4.501

tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
linear_in_log_odds_probability_weighting <- 
	ProbWeight(fun="linear_in_log_odds", 
		par=c(alpha=0.61, beta=0.724))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=linear_in_log_odds_probability_weighting,
	prob_weight_for_negative_outcomes=linear_in_log_odds_probability_weighting,
	utility=tk_1992_utility, digits=4)

#   cid gid    ev    pt  ce    rp
# 1   1   1 22.26 14.48 20.85   1.405
# 2   1   2 25.06  17.1 25.19 -0.1265




########################	
# Choice problem 14.2, Table 6, p.486
# S' = (110, 0.59; 49, 0.2; 45, 0.2; 4, 0.01)
#   ~ 49.8 TAX
#   ~ 71.9 PT
# R' = (110, 0.59; 97, 0.2; 11, 0.2; 4, 0.01)
#   ~ 50.0 TAX
#   ~ 67.2 PT
# R' > S'
choice_ids <- c(1, 1, 1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 1, 2, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 4, 1, 2, 3, 4)
objective_consequences <- c(110, 49, 45, 4, 110, 97, 11, 4)
probability_strings <- 
	c("0.59", "0.2", "0.2", "0.01", "0.59", "0.2", "0.2", "0.01")
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
# 1   1   1 83.74 49.85 49.85 33.89
# 2   1   2 86.54 50.03 50.03 36.51

tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
linear_in_log_odds_probability_weighting <- 
	ProbWeight(fun="linear_in_log_odds", 
		par=c(alpha=0.61, beta=0.724))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=linear_in_log_odds_probability_weighting,
	prob_weight_for_negative_outcomes=linear_in_log_odds_probability_weighting,
	utility=tk_1992_utility, digits=4)

#   cid gid    ev    pt  ce  rp
# 1   1   1 83.74 43.06 71.93 11.81
# 2   1   2 86.54 40.56  67.2 19.34





########################	
#
# 3-LDI tests: Choice problems 15.1, 15.2
# 3-2 LDI tests: Choice problems 16.1, 16.2
# 3-UDI tests: Choice problems 17.1, 17.2
#
########################	
# Choice problem 15.1, Table 7, p.488
# S = (58, 0.2; 56, 0.2; 2, 0.6)
#   ~ 21.7 TAX
#   ~ 19.9 PT
# R = (96, 0.2; 4, 0.2; 2, 0.6)
#   ~ 13.8 TAX
#   ~ 21.3 PT
# S > R
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(58, 56, 2, 96, 4, 2)
probability_strings <- 
	c("0.2", "0.2", "0.6", "0.2", "0.2", "0.6")
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
# 1   1   1   24 21.72 21.72 2.277
# 2   1   2 21.2 13.79 13.79 7.415

tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
linear_in_log_odds_probability_weighting <- 
	ProbWeight(fun="linear_in_log_odds", 
		par=c(alpha=0.61, beta=0.724))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=linear_in_log_odds_probability_weighting,
	prob_weight_for_negative_outcomes=linear_in_log_odds_probability_weighting,
	utility=tk_1992_utility, digits=4)

#   cid gid   ev    pt  ce    rp
# 1   1   1   24 13.91 19.92   4.083
# 2   1   2 21.2 14.76  21.3 -0.1039


########################	
# Choice problem 15.2, Table 7, p.488
# S2 = (58, 0.45; 56, 0.45; 2, 0.1)
#   ~ 36.9 TAX
#   ~ 41.0 PT
# R2 = (96, 0.45; 4, 0.45; 2, 0.1)
#   ~ 22.9 TAX
#   ~ 35.8 PT
# S2 > R2
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(58, 56, 2, 96, 4, 2)
probability_strings <- 
	c("0.45", "0.45", "0.1", "0.45", "0.45", "0.1")
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
# 1   1   1 51.5 36.91 36.91 14.59
# 2   1   2 45.2 22.86 22.86 22.34

tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
linear_in_log_odds_probability_weighting <- 
	ProbWeight(fun="linear_in_log_odds", 
		par=c(alpha=0.61, beta=0.724))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=linear_in_log_odds_probability_weighting,
	prob_weight_for_negative_outcomes=linear_in_log_odds_probability_weighting,
	utility=tk_1992_utility, digits=4)

#   cid gid   ev    pt  ce  rp
# 1   1   1 51.5 26.28 41.05 10.45
# 2   1   2 45.2 23.33 35.85 9.354


########################	
# Choice problem 16.1, Table 7, p.488
# S0 = (44, 0.5; 40, 0.5)
#   ~ 41.3 TAX
#   ~ 41.7 PT
# R0 = (96, 0.5; 4, 0.5)
#   ~ 34.7 TAX
#   ~ 39.3 PT
# S0 > R0
choice_ids <- c(1, 1, 1, 1)
gamble_ids <- c(1, 1, 2, 2)
outcome_ids <- c(1, 2, 1, 2)
objective_consequences <- c(44, 40, 96, 4)
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

#   cid gid ev   tax ce  rp
# 1   1   1 42 41.33 41.33 0.6667
# 2   1   2 50 34.67 34.67  15.33

tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
linear_in_log_odds_probability_weighting <- 
	ProbWeight(fun="linear_in_log_odds", 
		par=c(alpha=0.61, beta=0.724))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=linear_in_log_odds_probability_weighting,
	prob_weight_for_negative_outcomes=linear_in_log_odds_probability_weighting,
	utility=tk_1992_utility, digits=4)

#   cid gid ev    pt  ce   rp
# 1   1   1 42 26.64 41.67 0.3258
# 2   1   2 50 25.28 39.27  10.73



########################	
# Choice problem 16.2, Table 7, p.488
# S = (44, 0.48; 40, 0.48; 2, 0.04)
#   ~ 29.1 TAX
#   ~ 34.7 PT
# R = (96, 0.48; 4, 0.48; 2, 0.04)
#   ~ 24.5 TAX
#   ~ 37.7 PT
# S > R
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(44, 40, 2, 96, 4, 2)
probability_strings <- 
	c("0.48", "0.48", "0.04", "0.48", "0.48", "0.04")
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
# 1   1   1  40.4 29.12 29.12 11.28
# 2   1   2 48.08 24.52 24.52 23.56

tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
linear_in_log_odds_probability_weighting <- 
	ProbWeight(fun="linear_in_log_odds", 
		par=c(alpha=0.61, beta=0.724))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=linear_in_log_odds_probability_weighting,
	prob_weight_for_negative_outcomes=linear_in_log_odds_probability_weighting,
	utility=tk_1992_utility, digits=4)

#   cid gid    ev    pt  ce  rp
# 1   1   1  40.4 22.66 34.67 5.729
# 2   1   2 48.08  24.4 37.73 10.35



########################	
# Choice problem 17.1, Table 7, p.488
# S' = (100, 0.8; 44, 0.1; 40, 0.1)
#   ~ 61.6 TAX
#   ~ 77.4 PT
# R' = (100, 0.8; 96, 0.1; 4, 0.1)
#   ~ 63.4 TAX
#   ~ 71.7 PT
# R' > S'
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(100, 44, 40, 100, 96, 4)
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
# 1   1   1 88.4 61.62 61.62 26.78
# 2   1   2   90 63.39 63.39 26.61

tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
linear_in_log_odds_probability_weighting <- 
	ProbWeight(fun="linear_in_log_odds", 
		par=c(alpha=0.61, beta=0.724))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=linear_in_log_odds_probability_weighting,
	prob_weight_for_negative_outcomes=linear_in_log_odds_probability_weighting,
	utility=tk_1992_utility, digits=4)

#   cid gid   ev    pt  ce  rp
# 1   1   1 88.4 45.93  77.4    11
# 2   1   2   90 42.95 71.71 18.29



########################	
# Choice problem 17.2, Table 7, p.488
# S2' = (100, 0.1; 44, 0.45; 40, 0.45)
#   ~ 45.9 TAX
#   ~ 50.3 PT
# R2' = (100, 0.1; 96, 0.45; 4, 0.45)
#   ~ 43.9 TAX
#   ~ 42.6 PT
# S2' > R2'
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(100, 44, 40, 100, 96, 4)
probability_strings <- 
	c("0.1", "0.45", "0.45", "0.1", "0.45", "0.45")
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
# 1   1   1 47.8 45.88 45.88 1.918
# 2   1   2   55 43.92 43.92 11.08

tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
linear_in_log_odds_probability_weighting <- 
	ProbWeight(fun="linear_in_log_odds", 
		par=c(alpha=0.61, beta=0.724))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=linear_in_log_odds_probability_weighting,
	prob_weight_for_negative_outcomes=linear_in_log_odds_probability_weighting,
	utility=tk_1992_utility, digits=4)

#   cid gid   ev    pt  ce   rp
# 1   1   1 47.8 31.42 50.28 -2.479
# 2   1   2   55 27.17 42.62  12.38