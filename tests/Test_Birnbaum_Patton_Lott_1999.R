library("pt")

########################	
# Birnbaum, M. H., Patton, J. N., & Lott, M. K. (1999). Evidence against rank-dependent utility theories: Tests of cumulative independence, interval independence, stochastic dominance, and transitivity. Organizational Behavior and Human Decision Processes, 77(1), 44-83.
########################

########################	
#
# p.45
#
########################	
# A=(100, 0.5; 200, 0.5)
#   ~ 4.2 SWU (this is the utility, not the ce)
# B=(100, 0.99; 200, 0.01)
#   ~ 5.0 SWU (this is the utility, not the ce)
# C=(110, 0.5; 120, 0.5)
#   ~ 3.8 SWU (this is the utility, not the ce)
# D=(101, 0.01; 102 0.01; 103, 0.98)
#   ~ 4.9 SWU (this is the utility, not the ce)
# A < B
# C < D

choice_ids <- c(1, 1, 1, 1, 2, 2, 2, 2, 2)
gamble_ids <- c(1, 1, 2, 2, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 1, 2, 1, 2, 1, 2, 3)
objective_consequences <- c(100, 200, 100, 200, 110, 120, 101, 102, 103)
probability_strings <- 
	c("0.5", "0.5", "0.99", "0.01", "0.5", "0.5", "0.01", "0.01", "0.98")
my_choices <- Choices(choice_ids=choice_ids,
	gamble_ids=gamble_ids, 
	outcome_ids=outcome_ids, 
	objective_consequences=objective_consequences, 
	probability_strings=probability_strings)
my_choices

my_utility <- Utility(fun="power", 
	par=c(alpha=0.4, beta=0.4, lambda=2.25))
my_pwf <- 
	ProbWeight(fun="linear_in_log_odds", 
		par=c(alpha=0.4, beta=0.4))
compareSWU(my_choices,
	prob_weight=my_pwf,
	utility=my_utility, digits=4)

#   cid gid  ev   swu ce rp
# 1   1   1 150 4.181 35.75 114.2
# 2   1   2 101 5.012 56.24 44.76
# 3   2   1 115 3.812 28.37 86.63
# 4   2   2 103 4.941 54.26 48.71

########################	
#
# p.46,47,49
#
########################	
# S=(2, 0.5; 40, 0.25; 44, 0.25)
#   ~ 17.58 PT
# R=(2, 0.25; 10, 0.5; 98, 0.5)
#   ~ 25.83 PT
# S'=(40, 0.25; 44, 0.25; 108, 0.5)
#   ~ 68.29 PT
# R'-=(10, 0.25; 98, 0.25; 108, 0.5)
#   ~ 63.12 PT
# S < R
# S' > R'

choice_ids <- c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2)
gamble_ids <- c(1, 1, 1, 2, 2, 2, 1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3)
objective_consequences <- c(2, 40, 44, 2, 10, 98, 40, 44, 108, 10, 98, 108)
probability_strings <- 
	c("0.5", "0.25", "0.25", "0.5", "0.25", "0.25", "0.25", "0.25", "0.5", "0.25", "0.25", "0.5")
my_choices <- Choices(choice_ids=choice_ids,
	gamble_ids=gamble_ids, 
	outcome_ids=outcome_ids, 
	objective_consequences=objective_consequences, 
	probability_strings=probability_strings)
my_choices

tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
linear_in_log_odds_probability_weighting <- 
	ProbWeight(fun="linear_in_log_odds", 
		par=c(alpha=0.61, beta=0.724))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=linear_in_log_odds_probability_weighting,
	prob_weight_for_negative_outcomes=linear_in_log_odds_probability_weighting,
	utility=tk_1992_utility, digits=4)

#  gid od oc   pr     dw    sv     pt
#    1  3 44 0.25 0.2703 27.94  7.552
#    1  2 40 0.25 0.1497 25.69 11.397
#    1  1  2 0.50 0.5800  1.84 12.465
#  gid od oc   pr     dw     sv    pt
#    2  3 98 0.25 0.2703 56.530 15.28
#    2  2 10 0.25 0.1497  7.586 16.42
#    2  1  2 0.50 0.5800  1.840 17.48
#  gid od  oc   pr     dw    sv    pt
#    1  3 108 0.50 0.4200 61.58 25.86
#    1  2  44 0.25 0.1660 27.94 30.50
#    1  1  40 0.25 0.4141 25.69 41.14
#  gid od  oc   pr     dw     sv    pt
#    2  3 108 0.50 0.4200 61.576 25.86
#    2  2  98 0.25 0.1660 56.530 35.24
#    2  1  10 0.25 0.4141  7.586 38.38
#   cid gid ev    pt  ce  rp
# 1   1   1 22 12.46 17.58 4.417
# 2   1   2 28 17.48 25.83 2.174
# 3   2   1 75 41.14 68.29 6.713
# 4   2   2 81 38.38 63.12 17.88

########################	
#
# p.51,53
#
########################	
# E=(4, 0.59; 45, 0.2; 49, 0.2; 110, 0.01)
#   ~ 21.70 TAX
# F-=(4, 0.59; 11, 0.2; 97, 0.2; 110, 0.01)
#   ~ 20.85 PT ?an error? should be 20.56?
# E'=(4, 0.01; 45, 0.2; 49, 0.2; 110, 0.59)
#   ~ 49.85 PT
# F'-=(4, 0.01; 11, 0.2; 97, 0.2; 110, 0.59)
#   ~ 50.03 PT
# E > F
# F' > E'

choice_ids <- c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2)
gamble_ids <- c(1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4)
objective_consequences <- c(4, 45, 49, 110, 4, 11, 97, 110, 4, 45, 49, 110, 4, 11, 97, 110)
probability_strings <- 
	c("0.59", "0.2", "0.2", "0.01", "0.59", "0.2", "0.2", "0.01", "0.01", "0.2", "0.2", "0.59", "0.01", "0.2", "0.2", "0.59")
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
# 3   2   1 83.74 49.85 49.85  33.89
# 4   2   2 86.54 50.03 50.03  36.51



########################	
#
# p.55
#
########################	
# A=(0, 0.25; 0, 0.25; 100, 0.5)
#   ~ 44.82 TAX
# B=(0, 0.5; 100, 0.5)
#   ~ 50.0 TAX
# C=(0, 0.5; 100, 0.25; 100, 0.25)
#   ~ 55.18 TAX
# A < B < C

choice_ids <- c(1, 1, 1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 3, 3, 3)
outcome_ids <- c(1, 2, 3, 1, 2, 1, 2, 3)
objective_consequences <- c(0, 0, 100, 0, 100, 0, 100, 100)
probability_strings <- 
	c("0.25", "0.25", "0.5", "0.5", "0.5", "0.5", "0.25", "0.25")
my_choices <- Choices(choice_ids=choice_ids,
	gamble_ids=gamble_ids, 
	outcome_ids=outcome_ids, 
	objective_consequences=objective_consequences, 
	probability_strings=probability_strings)
my_choices

my_pwf <- 
	ProbWeight(fun="power",
		par=c(alpha=0.7, beta=1))
delta <- 0
my_utility <- Utility(fun="power", 
	par=c(alpha=1, beta=1, lambda=1))
compareTAX(my_choices, 
	prob_weight=my_pwf, 
	utility=my_utility,
	delta=delta,
	digits=4)

#   cid gid ev   tax ce rp
# 1   1   1 50 44.82 44.82  5.18
# 2   1   2 50    50    50     0
# 3   1   3 50 55.18 55.18 -5.18



########################	
#
# p.55
#
########################	
# A=(0, 0.25; 0, 0.25; 100, 0.5)
#   ~ 25 TAX
# B=(0, 0.5; 100, 0.5)
#   ~ 33.33 TAX
# C=(0, 0.5; 100, 0.25; 100, 0.25)
#   ~ 37.5 TAX
# A < B < C

choice_ids <- c(1, 1, 1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 3, 3, 3)
outcome_ids <- c(1, 2, 3, 1, 2, 1, 2, 3)
objective_consequences <- c(0, 0, 100, 0, 100, 0, 100, 100)
probability_strings <- 
	c("0.25", "0.25", "0.5", "0.5", "0.5", "0.5", "0.25", "0.25")
my_choices <- Choices(choice_ids=choice_ids,
	gamble_ids=gamble_ids, 
	outcome_ids=outcome_ids, 
	objective_consequences=objective_consequences, 
	probability_strings=probability_strings)
my_choices

my_pwf <- 
	ProbWeight(fun="power",
		par=c(alpha=1.0, beta=1))
delta <- -1
my_utility <- Utility(fun="power", 
	par=c(alpha=1, beta=1, lambda=1))
compareTAX(my_choices, 
	prob_weight=my_pwf, 
	utility=my_utility,
	delta=delta,
	digits=4)

#   cid gid ev   tax ce rp
# 1   1   1 50    25    25    25
# 2   1   2 50 33.33 33.33 16.67
# 3   1   3 50  37.5  37.5  12.5


########################	
#
# p.56-57
#
########################	
# G0=(12, 0.1; 96, 0.9)
#   ~ 58.10 TAX
# Gplus=(12, 0.05; 14, 0.05; 96, 0.9)
#   ~ 45.77 TAX
# Gminus=(12, 0.1; 90, 0.05; 96, 0.85)
#   ~ 63.10 TAX
# Gplus < G0 < Gminus

choice_ids <- c(1, 1, 1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 2, 2, 2, 3, 3, 3)
outcome_ids <- c(1, 2, 1, 2, 3, 1, 2, 3)
objective_consequences <- c(12, 96, 12, 14, 96, 12, 90, 96)
probability_strings <- 
	c("0.1", "0.9", "0.05", "0.05", "0.9", "0.1", "0.05", "0.85")
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
# 1   1   1 87.6  58.1  58.1  29.5
# 2   1   2 87.7 45.77 45.77 41.93
# 3   1   3 87.3  63.1  63.1  24.2


########################	
#
# p.58-59
#
########################	
# S=(2, 0.6; 40, 0.2; 44, 0.2)
#   ~ 16.19 TAX
# R=(2, 0.6; 10, 0.2; 98, 0.2)
#   ~ 15.47 TAX
# S''=(10, 0.6; 44, 0.4)
#   ~ 19.74 TAX
# R''=(10, 0.8; 98, 0.2)
#   ~ 26.12 TAX
# S'=(40, 0.2; 44, 0.2; 108, 0.6)
#   ~ 58.89 TAX
# R'=(10, 0.2; 98, 0.2; 108, 0.6)
#   ~ 62.72 TAX
# S'''=(40, 0.4; 98, 0.6)
#   ~ 62.06 TAX
# R'''=(10, 0.2; 98, 0.8)
#   ~ 52.55 TAX
# S > R
# S'' < R''
# S' < R'
# S''' > R'''
choice_ids <- c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4)
gamble_ids <- c(1, 1, 1, 2, 2, 2, 1, 1, 2, 2, 1, 1, 1, 2, 2, 2, 1, 1, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3, 1, 2, 1, 2, 1, 2, 3, 1, 2, 3, 1, 2, 1, 2)
objective_consequences <- c(2, 40, 44, 2, 10, 98, 
	10, 44, 10, 98, 
	40, 44, 108, 10, 98, 108, 
	40, 98, 10, 98)
probability_strings <- 
	c("0.6", "0.2", "0.2", "0.6", "0.2", "0.2", 
		"0.6", "0.4", "0.8", "0.2",
		"0.2", "0.2", "0.6", "0.2", "0.2", "0.6", 
		"0.4", "0.6", "0.2", "0.8")
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
# 1   1   1   18 16.19 16.19 1.809
# 2   1   2 22.8 15.47 15.47 7.331
# 3   2   1 23.6 19.74 19.74 3.864
# 4   2   2 27.6 26.12 26.12 1.478
# 5   3   1 81.6 58.89 58.89 22.71
# 6   3   2 86.4 62.72 62.72 23.68
# 7   4   1 74.8 62.06 62.06 12.74
# 8   4   2 80.4 52.55 52.55 27.85



########################	
#
# p.72-73
#
########################	
# Gplus=(12, 0.05; 14, 0.05; 96, 0.9)
#   ~ 45.77 TAX
# Gminus=(12, 0.1; 90, 0.05; 96, 0.85)
#   ~ 63.10 TAX
# GSplus=(12, 0.05; 14, 0.05; 96, 0.05; 96, 0.85)
#   ~ 53.06 TAX
# GSminus=(12, 0.05; 12, 0.05; 90, 0.05; 96, 0.85)
#   ~ 51.38 TAX
# Gplus < Gminus
# GSplus > GSminus

choice_ids <- c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2)
gamble_ids <- c(1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3, 1, 2, 3, 4, 1, 2, 3, 4)
objective_consequences <- c(12, 14, 96, 12, 90, 96, 
	12, 14, 96, 96, 12, 12, 90, 96)
probability_strings <- 
	c("0.05", "0.05", "0.9", "0.1", "0.05", "0.85", 
		"0.05", "0.05", "0.05", "0.85", "0.05", "0.05", "0.05", "0.85")
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
# 3   2   1 87.7 53.06 53.06 34.64
# 4   2   2 87.3 51.38 51.38 35.92