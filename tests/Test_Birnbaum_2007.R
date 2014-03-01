library("pt")

########################	
# Birnbaum, M. H. (2007). Tests of branch splitting and branch-splitting independence in Allais paradoxes with positive and mixed consequences. Organizational Behavior and Human Decision Processes, 102(2), 154-173.
########################	



########################	
#
# Table 2. Dissection of Allais paradox with large consequences (series A)
#
########################	
# Choice problem 6, Table 2, p.161
# S = (1000000, 0.11; 2, 0.89)
#   ~ 125k TAX
#   ~ 132k PT
# R = (2000000, 0.10; 2, 0.90)
#   ~ 236k TAX
#   ~ 248k PT
# R > S
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
# Choice problem 9, Table 2, p.161
# S = (1000000, 0.1; 1000000, 0.01; 2, 0.89)
#   ~ 155k TAX
#   ~ 132k PT
# R = (2000000, 0.10; 2, 0.01; 2, 0.89)
#   ~ 172k TAX
#   ~ 248k PT
# S > R
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
# Choice problem 12, Table 2, p.161
# S = (1000000, 1.0)
#   ~ 1000k TAX
#   ~ 1000k PT
# R = (1000000, 0.10; 2000000, 0.89; 2, 0.01)
#   ~ 810k TAX
#   ~ 1065k PT
# S > R
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


tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
linear_in_log_odds_probability_weighting <- 
	ProbWeight(fun="linear_in_log_odds", 
		par=c(alpha=0.61, beta=0.724))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=linear_in_log_odds_probability_weighting,
	prob_weight_for_negative_outcomes=linear_in_log_odds_probability_weighting,
	utility=tk_1992_utility, digits=4)

#   cid gid      ev     pt    ce            rp
# 1   1   1 1000000 190546 1000000 -0.000000001397
# 2   1   2 1090000 201336 1064590           25410


########################	
# Choice problem 16, Table 2, p.161
# S = (2000000, 0.89; 1000000, 0.1; 1000000, 0.01)
#   ~ 1400k TAX
#   ~ 1714k PT
# R = (2000000, 0.89; 2000000, 0.1; 2, 0.01)
#   ~ 1449k TAX
#   ~ 1825k PT
# S > R
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(2000000, 1000000, 1000000, 2000000, 2000000, 2)
probability_strings <- 
	c("0.89", "0.1", "0.01", "0.89", "0.1", "0.01")
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
# 1   1   1 1890000 1396927 1396927 493073
# 2   1   2 1980000 1448566 1448566 531434


tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
linear_in_log_odds_probability_weighting <- 
	ProbWeight(fun="linear_in_log_odds", 
		par=c(alpha=0.61, beta=0.724))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=linear_in_log_odds_probability_weighting,
	prob_weight_for_negative_outcomes=linear_in_log_odds_probability_weighting,
	utility=tk_1992_utility, digits=4)

#   cid gid      ev     pt    ce   rp
# 1   1   1 1890000 306095 1713673 176327
# 2   1   2 1980000 323580 1825338 154662


########################	
# Choice problem 19, Table 2, p.161
# S = (2000000, 0.89; 1000000, 0.11)
#   ~ 1541k TAX
#   ~ 1714k PT
# R = (2000000, 0.99; 2, 0.01)
#   ~ 1282k TAX
#   ~ 1825k PT
# S > R
choice_ids <- c(1, 1, 1, 1)
gamble_ids <- c(1, 1, 2, 2)
outcome_ids <- c(1, 2, 1, 2)
objective_consequences <- c(2000000, 1000000, 2000000, 2)
probability_strings <- 
	c("0.89", "0.11", "0.99", "0.01")
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
# 1   1   1 1890000 1541380 1541380 348620
# 2   1   2 1980000 1281939 1281939 698061


tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
linear_in_log_odds_probability_weighting <- 
	ProbWeight(fun="linear_in_log_odds", 
		par=c(alpha=0.61, beta=0.724))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=linear_in_log_odds_probability_weighting,
	prob_weight_for_negative_outcomes=linear_in_log_odds_probability_weighting,
	utility=tk_1992_utility, digits=4)

#   cid gid      ev     pt    ce   rp
# 1   1   1 1890000 306095 1713673 176327
# 2   1   2 1980000 323580 1825338 154662




########################	
#
# Table 3. Dissection of Allais paradox into branch independence and coalescing (series B)
#
########################	
# Choice problem 10, Table 3, p.161
# S = (500000, 0.15; 11, 0.85)
#   ~ 76k TAX
#   ~ 81k PT
# R = (1000000, 0.10; 11, 0.90)
#   ~ 118k TAX
#   ~ 124k PT
# R > S
choice_ids <- c(1, 1, 1, 1)
gamble_ids <- c(1, 1, 2, 2)
outcome_ids <- c(1, 2, 1, 2)
objective_consequences <- c(500000, 11, 1000000, 11)
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

#   cid gid     ev    tax  ce  rp
# 1   1   1  75009  76328  76328  -1318
# 2   1   2 100010 117888 117888 -17878


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
# 1   1   1  75009 20801  80706  -5696
# 2   1   2 100010 30365 124054 -24045


########################	
# Choice problem 17, Table 3, p.161
# S = (500000, 0.1; 500000, 0.05; 11, 0.85)
#   ~ 100k TAX
#   ~ 81k PT
# R = (1000000, 0.10; 11, 0.05; 11, 0.85)
#   ~ 82k TAX
#   ~ 124k PT
# S > R
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(500000, 500000, 11, 1000000, 11, 11)
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

#   cid gid     ev   tax ce  rp
# 1   1   1  75009 99514 99514 -24505
# 2   1   2 100010 82132 82132  17878


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
# 1   1   1  75009 20801  80706  -5696
# 2   1   2 100010 30365 124054 -24045


########################	
# Choice problem 20, Table 3, p.161
# S = (500000, 0.1; 500000, 0.85; 500000, 0.05)
#   ~ 500k TAX
#   ~ 500k PT
# R = (1000000, 0.10; 500000, 0.85; 11, 0.05)
#   ~ 378k TAX
#   ~ 470k PT
# S > R
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(500000, 500000, 500000, 1000000, 500000, 11)
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

#   cid gid     ev    tax  ce  rp
# 1   1   1 500000 500000 500000      0
# 2   1   2 525001 378151 378151 146850


tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
linear_in_log_odds_probability_weighting <- 
	ProbWeight(fun="linear_in_log_odds", 
		par=c(alpha=0.61, beta=0.724))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=linear_in_log_odds_probability_weighting,
	prob_weight_for_negative_outcomes=linear_in_log_odds_probability_weighting,
	utility=tk_1992_utility, digits=4)

#   cid gid     ev     pt   ce             rp
# 1   1   1 500000 103537 500000 -0.0000000006403
# 2   1   2 525001  98095 470246            54755


########################	
# Choice problem 14, Table 3, p.161
# S = (1000000, 0.85; 500000, 0.1; 500000, 0.05)
#   ~ 684k TAX
#   ~ 834k PT
# R = (1000000, 0.85; 1000000, 0.1; 11, 0.05)
#   ~ 674k TAX
#   ~ 791k PT
# S > R
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(1000000, 500000, 500000, 1000000, 1000000, 11)
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

#   cid gid     ev    tax  ce  rp
# 1   1   1 925000 683663 683663 241337
# 2   1   2 950001 674176 674176 275825


tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
linear_in_log_odds_probability_weighting <- 
	ProbWeight(fun="linear_in_log_odds", 
		par=c(alpha=0.61, beta=0.724))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=linear_in_log_odds_probability_weighting,
	prob_weight_for_negative_outcomes=linear_in_log_odds_probability_weighting,
	utility=tk_1992_utility, digits=4)

#   cid gid     ev     pt   ce   rp
# 1   1   1 925000 162349 833616  91384
# 2   1   2 950001 155017 790969 159031


########################	
# Choice problem 8, Table 3, p.161
# S = (500000, 0.15; 11, 0.85)
#   ~ 757k TAX
#   ~ 834k PT
# R = (1000000, 0.10; 11, 0.90)
#   ~ 591k TAX
#   ~ 791k PT
# S > R
choice_ids <- c(1, 1, 1, 1)
gamble_ids <- c(1, 1, 2, 2)
outcome_ids <- c(1, 2, 1, 2)
objective_consequences <- c(1000000, 500000, 1000000, 11)
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

#   cid gid     ev    tax  ce  rp
# 1   1   1 925000 757015 757015 167985
# 2   1   2 950001 591381 591381 358619


tk_1992_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))
linear_in_log_odds_probability_weighting <- 
	ProbWeight(fun="linear_in_log_odds", 
		par=c(alpha=0.61, beta=0.724))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=linear_in_log_odds_probability_weighting,
	prob_weight_for_negative_outcomes=linear_in_log_odds_probability_weighting,
	utility=tk_1992_utility, digits=4)

#   cid gid     ev     pt   ce   rp
# 1   1   1 925000 162349 833616  91384
# 2   1   2 950001 155017 790969 159031



########################	
#
# Table 7. Dissection of Allais paradox (series A)
#
########################	
# GDU uses parameterisations from Birnbaum, M. H. (2005). A comparison of five models that predict violations of first-order stochastic dominance in risky decision making. Journal of Risk and Uncertainty, 31(3), 263-287.
# p.269
# Choice problem 6, Table 7, p.165
# S = (40, 0.2; 2, 0.8)
#   ~ 9.0 TAX
#   ~ 8.4 GDU
# R = (98, 0.10; 2, 0.90)
#   ~ 13.3 TAX
#   ~ 12.9 GDU
# R > S
choice_ids <- c(1, 1, 1, 1)
gamble_ids <- c(1, 1, 2, 2)
outcome_ids <- c(1, 2, 1, 2)
objective_consequences <- c(40, 2, 98, 2)
probability_strings <- 
	c("0.2", "0.8", "0.1", "0.9")
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
# 1   1   1  9.6 8.962 8.962 0.6384
# 2   1   2 11.6 13.32 13.32 -1.716

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
# 1   1   1  9.6 8.353 8.353  1.247
# 2   1   2 11.6 12.94 12.94 -1.341



########################	
# Choice problem 11, Table 7, p.165
# GDU uses parameterisations from Birnbaum, M. H. (2005). A comparison of five models that predict violations of first-order stochastic dominance in risky decision making. Journal of Risk and Uncertainty, 31(3), 263-287.
# p.269
# S = (40, 0.1; 40, 0.1; 2, 0.8)
#   ~ 11.1 TAX
#   ~ 8.4 GDU
# R = (98, 0.10; 2, 0.90)
#   ~ 13.3 TAX
#   ~ 12.9 GDU
# R > S
choice_ids <- c(1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2)
objective_consequences <- c(40, 40, 2, 98, 2)
probability_strings <- 
	c("0.1", "0.1", "0.8", "0.1", "0.9")
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
# 1   1   1  9.6 11.07 11.07 -1.466
# 2   1   2 11.6 13.32 13.32 -1.716

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
# 1   1   1  9.6 8.353 8.353  1.247
# 2   1   2 11.6 12.94 12.94 -1.341


########################	
# Choice problem 21, Table 7, p.165
# GDU uses parameterisations from Birnbaum, M. H. (2005). A comparison of five models that predict violations of first-order stochastic dominance in risky decision making. Journal of Risk and Uncertainty, 31(3), 263-287.
# p.269
# S = (40, 0.2; 2, 0.8)
#   ~ 9.0 TAX
#   ~ 8.4 GDU
# R = (98, 0.10; 2, 0.10; 2, 0.80)
#   ~ 9.6 TAX
#   ~ 7.2 GDU ? an error, should be 9.681 ?
# R > S
choice_ids <- c(1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 1, 2, 3)
objective_consequences <- c(40, 2, 98, 2, 2)
probability_strings <- 
	c("0.2", "0.8", "0.1", "0.1", "0.8")
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
# 1   1   1  9.6 8.962 8.962 0.6384
# 2   1   2 11.6 9.635 9.635  1.965

my_pwf <- 
	ProbWeight(fun="compound_invariance",
		par=c(alpha=0.542, beta=1.382))
my_utility <- Utility(fun="power", 
	par=c(alpha=1, beta=1, lambda=1))
compareGDU(my_choices, 
	prob_weight=my_pwf, 
	utility=my_utility,
	digits=4)

#   cid gid   ev   gdu ce rp
# 1   1   1  9.6 8.353 8.353 1.247
# 2   1   2 11.6 9.681 9.681 1.919


########################	
# Choice problem 9, Table 7, p.165
# GDU uses parameterisations from Birnbaum, M. H. (2005). A comparison of five models that predict violations of first-order stochastic dominance in risky decision making. Journal of Risk and Uncertainty, 31(3), 263-287.
# p.269
# S = (40, 0.1; 40, 0.1; 2, 0.8)
#   ~ 11.1 TAX
#   ~ 8.4 GDU
# R = (98, 0.10; 2, 0.10; 2, 0.80)
#   ~ 9.6 TAX
#   ~ 7.2 GDU ? an error, should be 9.681 ?
# R > S
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(40, 40, 2, 98, 2, 2)
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
# 1   1   1  9.6 11.07 11.07 -1.466
# 2   1   2 11.6 9.635 9.635  1.965

my_pwf <- 
	ProbWeight(fun="compound_invariance",
		par=c(alpha=0.542, beta=1.382))
my_utility <- Utility(fun="power", 
	par=c(alpha=1, beta=1, lambda=1))
compareGDU(my_choices, 
	prob_weight=my_pwf, 
	utility=my_utility,
	digits=4)

#   cid gid   ev   gdu ce rp
# 1   1   1  9.6 8.353 8.353 1.247
# 2   1   2 11.6 9.681 9.681 1.919


########################	
# Choice problem 12, Table 7, p.165
# GDU uses parameterisations from Birnbaum, M. H. (2005). A comparison of five models that predict violations of first-order stochastic dominance in risky decision making. Journal of Risk and Uncertainty, 31(3), 263-287.
# p.269
# S = (40, 1.0)
#   ~ 40.0 TAX
#   ~ 40.0 GDU
# R = (98, 0.10; 40, 0.80; 2, 0.10)
#   ~ 30.6 TAX
#   ~ 31.9 GDU
# R > S
choice_ids <- c(1, 1, 1, 1)
gamble_ids <- c(1, 2, 2, 2)
outcome_ids <- c(1, 1, 2, 3)
objective_consequences <- c(40, 98, 40, 2)
probability_strings <- 
	c("1.0", "0.1", "0.8", "0.1")
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
# 1   1   1 40    40    40     0
# 2   1   2 42 30.58 30.58 11.42

my_pwf <- 
	ProbWeight(fun="compound_invariance",
		par=c(alpha=0.542, beta=1.382))
my_utility <- Utility(fun="power", 
	par=c(alpha=1, beta=1, lambda=1))
compareGDU(my_choices, 
	prob_weight=my_pwf, 
	utility=my_utility,
	digits=4)

#   cid gid ev   gdu ce rp
# 1   1   1 40    40    40     0
# 2   1   2 42 31.91 31.91 10.09


########################	
# Choice problem 16, Table 7, p.165
# GDU uses parameterisations from Birnbaum, M. H. (2005). A comparison of five models that predict violations of first-order stochastic dominance in risky decision making. Journal of Risk and Uncertainty, 31(3), 263-287.
# p.269
# S = (98, 0.8; 40, 0.1; 40, 0.1)
#   ~ 59.8 TAX
#   ~ 65.0 GDU
# R = (98, 0.8; 98, 0.1; 2, 0.1)
#   ~ 62.6 TAX
#   ~ 65.8 GDU
# R > S
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(98, 40, 40, 98, 98, 2)
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
# 1   1   1 86.4 59.77 59.77 26.63
# 2   1   2 88.4 62.55 62.55 25.85

my_pwf <- 
	ProbWeight(fun="compound_invariance",
		par=c(alpha=0.542, beta=1.382))
my_utility <- Utility(fun="power", 
	par=c(alpha=1, beta=1, lambda=1))
compareGDU(my_choices, 
	prob_weight=my_pwf, 
	utility=my_utility,
	digits=4)

#   cid gid   ev   gdu ce rp
# 1   1   1 86.4    65    65  21.4
# 2   1   2 88.4 65.83 65.83 22.57



########################	
# Choice problem 7, Table 7, p.165
# GDU uses parameterisations from Birnbaum, M. H. (2005). A comparison of five models that predict violations of first-order stochastic dominance in risky decision making. Journal of Risk and Uncertainty, 31(3), 263-287.
# p.269
# S = (98, 0.8; 40, 0.1; 40, 0.1)
#   ~ 59.8 TAX
#   ~ 65.0 GDU
# R = (98, 0.9; 2, 0.1)
#   ~ 54.7 TAX
#   ~ 65.8 GDU
# S > R
choice_ids <- c(1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2)
objective_consequences <- c(98, 40, 40, 98, 2)
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
# 1   1   1 86.4 59.77 59.77 26.63
# 2   1   2 88.4 54.68 54.68 33.72

my_pwf <- 
	ProbWeight(fun="compound_invariance",
		par=c(alpha=0.542, beta=1.382))
my_utility <- Utility(fun="power", 
	par=c(alpha=1, beta=1, lambda=1))
compareGDU(my_choices, 
	prob_weight=my_pwf, 
	utility=my_utility,
	digits=4)

#   cid gid   ev   gdu ce rp
# 1   1   1 86.4    65    65  21.4
# 2   1   2 88.4 65.83 65.83 22.57



########################	
# Choice problem 13, Table 7, p.165
# GDU uses parameterisations from Birnbaum, M. H. (2005). A comparison of five models that predict violations of first-order stochastic dominance in risky decision making. Journal of Risk and Uncertainty, 31(3), 263-287.
# p.269
# S = (98, 0.8; 40, 0.2)
#   ~ 68.0 TAX
#   ~ 71.4 GDU
# R = (98, 0.8; 98, 0.1; 2, 0.1)
#   ~ 62.6 TAX
#   ~ 65.8 GDU
# R > S
choice_ids <- c(1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 1, 2, 3)
objective_consequences <- c(98, 40, 98, 98, 2)
probability_strings <- 
	c("0.8", "0.2", "0.8", "0.1", "0.1")
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
# 2   1   2 88.4 62.55 62.55 25.85

my_pwf <- 
	ProbWeight(fun="compound_invariance",
		par=c(alpha=0.542, beta=1.382))
my_utility <- Utility(fun="power", 
	par=c(alpha=1, beta=1, lambda=1))
compareGDU(my_choices, 
	prob_weight=my_pwf, 
	utility=my_utility,
	digits=4)

#   cid gid   ev   gdu ce rp
# 1   1   1 86.4 71.42 71.42 14.98
# 2   1   2 88.4 65.83 65.83 22.57



########################	
# Choice problem 19, Table 7, p.165
# GDU uses parameterisations from Birnbaum, M. H. (2005). A comparison of five models that predict violations of first-order stochastic dominance in risky decision making. Journal of Risk and Uncertainty, 31(3), 263-287.
# p.269
# S = (98, 0.8; 40, 0.2)
#   ~ 68.0 TAX
#   ~ 71.4 GDU
# R = (98, 0.9; 2, 0.1)
#   ~ 54.7 TAX
#   ~ 65.8 GDU
# S > R
choice_ids <- c(1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 1, 2, 3)
objective_consequences <- c(98, 40, 98, 98, 2)
probability_strings <- 
	c("0.8", "0.2", "0.8", "0.1", "0.1")
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
# 2   1   2 88.4 62.55 62.55 25.85

my_pwf <- 
	ProbWeight(fun="compound_invariance",
		par=c(alpha=0.542, beta=1.382))
my_utility <- Utility(fun="power", 
	par=c(alpha=1, beta=1, lambda=1))
compareGDU(my_choices, 
	prob_weight=my_pwf, 
	utility=my_utility,
	digits=4)

#   cid gid   ev   gdu ce rp
# 1   1   1 86.4 71.42 71.42 14.98
# 2   1   2 88.4 65.83 65.83 22.57




########################	
#
# Table 8. Dissection of Allais paradox (series BA)
#
########################	
# GDU uses parameterisations from Birnbaum, M. H. (2005). A comparison of five models that predict violations of first-order stochastic dominance in risky decision making. Journal of Risk and Uncertainty, 31(3), 263-287.
# p.269
# Choice problem 10, Table 8, p.166
# S = (50, 0.15; 7, 0.85)
#   ~ 13.6 TAX
#   ~ 13.1 GDU
# R = (100, 0.10; 7, 0.90)
#   ~ 18.0 TAX
#   ~ 17.6 GDU
# R > S
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

my_pwf <- 
	ProbWeight(fun="compound_invariance",
		par=c(alpha=0.542, beta=1.382))
my_utility <- Utility(fun="power", 
	par=c(alpha=1, beta=1, lambda=1))
compareGDU(my_choices, 
	prob_weight=my_pwf, 
	utility=my_utility,
	digits=4)

#   cid gid    ev   gdu ce  rp
# 1   1   1 13.45 13.08 13.08 0.3652
# 2   1   2  16.3  17.6  17.6 -1.299



########################	
# GDU uses parameterisations from Birnbaum, M. H. (2005). A comparison of five models that predict violations of first-order stochastic dominance in risky decision making. Journal of Risk and Uncertainty, 31(3), 263-287.
# p.269
# Choice problem 15, Table 8, p.166
# S = (50, 0.1; 50, 0.05; 7, 0.85)
#   ~ 15.6 TAX
#   ~ 13.1 GDU
# R = (100, 0.10; 7, 0.90)
#   ~ 18.0 TAX
#   ~ 17.6 GDU
# R > S
choice_ids <- c(1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2)
objective_consequences <- c(50, 50, 7, 100, 7)
probability_strings <- 
	c("0.1", "0.05", "0.85", "0.1", "0.9")
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
# 2   1   2  16.3 17.96 17.96 -1.663

my_pwf <- 
	ProbWeight(fun="compound_invariance",
		par=c(alpha=0.542, beta=1.382))
my_utility <- Utility(fun="power", 
	par=c(alpha=1, beta=1, lambda=1))
compareGDU(my_choices, 
	prob_weight=my_pwf, 
	utility=my_utility,
	digits=4)

#   cid gid    ev   gdu ce  rp
# 1   1   1 13.45 13.08 13.08 0.3652
# 2   1   2  16.3  17.6  17.6 -1.299



########################	
# GDU uses parameterisations from Birnbaum, M. H. (2005). A comparison of five models that predict violations of first-order stochastic dominance in risky decision making. Journal of Risk and Uncertainty, 31(3), 263-287.
# p.269
# Choice problem 22, Table 8, p.166
# S = (50, 0.15; 7, 0.85)
#   ~ 13.6 TAX
#   ~ 13.1 GDU
# R = (100, 0.10; 7, 0.05; 7, 0.85)
#   ~ 14.6 TAX
#   ~ 12.6 GDU ? an error, should be 15.26?
# R > S
choice_ids <- c(1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 1, 2, 3)
objective_consequences <- c(50, 7, 100, 7, 7)
probability_strings <- 
	c("0.15", "0.85", "0.1", "0.05", "0.85")
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
# 2   1   2  16.3 14.64 14.64   1.663

my_pwf <- 
	ProbWeight(fun="compound_invariance",
		par=c(alpha=0.542, beta=1.382))
my_utility <- Utility(fun="power", 
	par=c(alpha=1, beta=1, lambda=1))
compareGDU(my_choices, 
	prob_weight=my_pwf, 
	utility=my_utility,
	digits=4)

#   cid gid    ev   gdu ce  rp
# 1   1   1 13.45 13.08 13.08 0.3652
# 2   1   2  16.3 15.26 15.26  1.045



########################	
# GDU uses parameterisations from Birnbaum, M. H. (2005). A comparison of five models that predict violations of first-order stochastic dominance in risky decision making. Journal of Risk and Uncertainty, 31(3), 263-287.
# p.269
# Choice problem 17, Table 8, p.166
# S = (50, 0.1; 50, 0.05; 7, 0.85)
#   ~ 15.6 TAX
#   ~ 13.1 GDU
# R = (100, 0.10; 7, 0.05; 7, 0.85)
#   ~ 14.6 TAX
#   ~ 12.6 GDU ? an error, should be 15.26?
# S > R
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

my_pwf <- 
	ProbWeight(fun="compound_invariance",
		par=c(alpha=0.542, beta=1.382))
my_utility <- Utility(fun="power", 
	par=c(alpha=1, beta=1, lambda=1))
compareGDU(my_choices, 
	prob_weight=my_pwf, 
	utility=my_utility,
	digits=4)

#   cid gid    ev   gdu ce  rp
# 1   1   1 13.45 13.08 13.08 0.3652
# 2   1   2  16.3 15.26 15.26  1.045



########################	
# GDU uses parameterisations from Birnbaum, M. H. (2005). A comparison of five models that predict violations of first-order stochastic dominance in risky decision making. Journal of Risk and Uncertainty, 31(3), 263-287.
# p.269
# Choice problem 20, Table 8, p.166
# S = (50, 1.0)
#   ~ 50.0 TAX
#   ~ 50.0 GDU
# R = (100, 0.10; 50, 0.85; 7, 0.05)
#   ~ 40.1 TAX
#   ~ 44.0 GDU
# R > S
choice_ids <- c(1, 1, 1, 1)
gamble_ids <- c(1, 2, 2, 2)
outcome_ids <- c(1, 1, 2, 3)
objective_consequences <- c(50, 100, 50, 7)
probability_strings <- 
	c("0.1", "0.1", "0.85", "0.05")
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
# 1   1   1     5   50    50   -45
# 2   1   2 52.85 40.1  40.1 12.75

my_pwf <- 
	ProbWeight(fun="compound_invariance",
		par=c(alpha=0.542, beta=1.382))
my_utility <- Utility(fun="power", 
	par=c(alpha=1, beta=1, lambda=1))
compareGDU(my_choices, 
	prob_weight=my_pwf, 
	utility=my_utility,
	digits=4)

#   cid gid    ev   gdu ce rp
# 1   1   1     5    50    50   -45
# 2   1   2 52.85 44.06 44.06 8.792


########################	
# GDU uses parameterisations from Birnbaum, M. H. (2005). A comparison of five models that predict violations of first-order stochastic dominance in risky decision making. Journal of Risk and Uncertainty, 31(3), 263-287.
# p.269
# Choice problem 14, Table 8, p.166
# S = (100, 0.85; 50, 0.1; 50, 0.05)
#   ~ 68.4 TAX
#   ~ 74.9 GDU
# R = (100, 0.85; 100, 0.10; 7, 0.05)
#   ~ 69.7 TAX
#   ~ 77.5 GDU
# S > R
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

my_pwf <- 
	ProbWeight(fun="compound_invariance",
		par=c(alpha=0.542, beta=1.382))
my_utility <- Utility(fun="power", 
	par=c(alpha=1, beta=1, lambda=1))
compareGDU(my_choices, 
	prob_weight=my_pwf, 
	utility=my_utility,
	digits=4)

#   cid gid    ev   gdu ce rp
# 1   1   1  92.5 74.91 74.91 17.59
# 2   1   2 95.35 77.55 77.55  17.8



########################	
# GDU uses parameterisations from Birnbaum, M. H. (2005). A comparison of five models that predict violations of first-order stochastic dominance in risky decision making. Journal of Risk and Uncertainty, 31(3), 263-287.
# p.269
# Choice problem 5, Table 8, p.166
# S = (100, 0.85; 50, 0.1; 50, 0.05)
#   ~ 68.4 TAX
#   ~ 74.9 GDU
# R = (100, 0.95; 7, 0.05)
#   ~ 62.0 TAX
#   ~ 77.5 GDU
# S > R
choice_ids <- c(1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2)
objective_consequences <- c(100, 50, 50, 100, 7)
probability_strings <- 
	c("0.85", "0.1", "0.05", "0.95", "0.05")
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
# 2   1   2 95.35    62    62 33.35

my_pwf <- 
	ProbWeight(fun="compound_invariance",
		par=c(alpha=0.542, beta=1.382))
my_utility <- Utility(fun="power", 
	par=c(alpha=1, beta=1, lambda=1))
compareGDU(my_choices, 
	prob_weight=my_pwf, 
	utility=my_utility,
	digits=4)

#   cid gid    ev   gdu ce rp
# 1   1   1  92.5 74.91 74.91 17.59
# 2   1   2 95.35 77.55 77.55  17.8



########################	
# GDU uses parameterisations from Birnbaum, M. H. (2005). A comparison of five models that predict violations of first-order stochastic dominance in risky decision making. Journal of Risk and Uncertainty, 31(3), 263-287.
# p.269
# Choice problem 18, Table 8, p.166
# S = (100, 0.85; 50, 0.15)
#   ~ 75.7 TAX
#   ~ 79.8 GDU
# R = (100, 0.85; 100, 0.1; 7, 0.05)
#   ~ 69.7 TAX
#   ~ 77.5 GDU
# S > R
choice_ids <- c(1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 1, 2, 3)
objective_consequences <- c(100, 50, 100, 100, 7)
probability_strings <- 
	c("0.85", "0.15", "0.85", "0.1", "0.05")
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
# 2   1   2 95.35 69.7  69.7 25.65

my_pwf <- 
	ProbWeight(fun="compound_invariance",
		par=c(alpha=0.542, beta=1.382))
my_utility <- Utility(fun="power", 
	par=c(alpha=1, beta=1, lambda=1))
compareGDU(my_choices, 
	prob_weight=my_pwf, 
	utility=my_utility,
	digits=4)

#   cid gid    ev   gdu ce rp
# 1   1   1  92.5 79.84 79.84 12.66
# 2   1   2 95.35 77.55 77.55  17.8


########################	
# GDU uses parameterisations from Birnbaum, M. H. (2005). A comparison of five models that predict violations of first-order stochastic dominance in risky decision making. Journal of Risk and Uncertainty, 31(3), 263-287.
# p.269
# Choice problem 8, Table 8, p.166
# S = (100, 0.85; 50, 0.15)
#   ~ 75.7 TAX
#   ~ 79.8 GDU
# R = (100, 0.95; 7, 0.05)
#   ~ 62.0 TAX
#   ~ 77.5 GDU
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

my_pwf <- 
	ProbWeight(fun="compound_invariance",
		par=c(alpha=0.542, beta=1.382))
my_utility <- Utility(fun="power", 
	par=c(alpha=1, beta=1, lambda=1))
compareGDU(my_choices, 
	prob_weight=my_pwf, 
	utility=my_utility,
	digits=4)

#   cid gid    ev   gdu ce rp
# 1   1   1  92.5 79.84 79.84 12.66
# 2   1   2 95.35 77.55 77.55  17.8



########################	
#
# Table 10. Test of EU plus entropy model
#
########################	
# Choice problem 9, Table 10, p.170
# S = (47, 0.5; 43, 0.15; 43, 0.15; 43, 0.1; 43, 0.1)
#   ~ 43.5 TAX
# R = (89, 0.5; 11, 0.15; 11, 0.15; 11, 0.1; 11, 0.1)
#   ~ 21.4 TAX
# S > R
choice_ids <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5)
objective_consequences <- c(47, 43, 43, 43, 43, 89, 11, 11, 11, 11)
probability_strings <- 
	c("0.5", "0.15", "0.15", "0.1", "0.1", "0.5", "0.15", "0.15", "0.1", "0.1")
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
# 1   1   1 45 43.53 43.53 1.469
# 2   1   2 50 21.36 21.36 28.64



########################	
# Choice problem 25, Table 10, p.170
# S = (47, 0.5; 43, 0.25; 43, 0.25)
#   ~ 43.9 TAX
# R = (89, 0.5; 11, 0.15; 11, 0.15; 11, 0.1; 11, 0.1)
#   ~ 28.5 TAX
# R > S
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(47, 43, 43, 89, 11, 11)
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

#   cid gid ev   tax ce rp
# 1   1   1 45  43.9  43.9 1.104
# 2   1   2 50 28.48 28.48 21.52



########################	
# Choice problem 7, Table 10, p.170
# S = (47, 0.5; 43, 0.5)
#   ~ 44.3 TAX
# R = (89, 0.5; 11, 0.5)
#   ~ 37.0 TAX
# R > S
choice_ids <- c(1, 1, 1, 1)
gamble_ids <- c(1, 1, 2, 2)
outcome_ids <- c(1, 2, 1, 2)
objective_consequences <- c(47, 43, 89, 11)
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
# 1   1   1 45 44.33 44.33 0.6667
# 2   1   2 50    37    37     13



########################	
# Choice problem 23, Table 10, p.170
# S = (47, 0.25; 47, 0.25; 43, 0.5)
#   ~ 44.7 TAX
# R = (89, 0.25; 89, 0.25; 11, 0.5)
#   ~ 43.3 TAX
# R > S
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(47, 47, 43, 89, 89, 11)
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

#   cid gid ev   tax ce  rp
# 1   1   1 45 44.66 44.66 0.3446
# 2   1   2 50 43.28 43.28   6.72



########################	
# Choice problem 13, Table 10, p.170
# S = (47, 0.1; 47, 0.1; 47, 0.15; 47, 0.15; 43, 0.5)
#   ~ 45.0 TAX
# R = (89, 0.1; 89, 0.1; 89, 0.15; 89, 0.15; 11, 0.5)
#   ~ 50.1 TAX
# R > S
choice_ids <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5)
objective_consequences <- c(47, 47, 47, 47, 43, 89, 89, 89, 89, 11)
probability_strings <- 
	c("0.1", "0.1", "0.15", "0.15", "0.5", "0.1", "0.1", "0.15", "0.15", "0.5")
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

#   cid gid ev  tax ce     rp
# 1   1   1 45   45    45 -0.004934
# 2   1   2 50 50.1  50.1  -0.09622