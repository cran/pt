library("pt")

########################	
# Wakker, P. P. (2003). The data of Levy and Levy (2002) "Prospect theory: Much ado about nothing?" actually support prospect theory. Management Science, 49(7), 979-981.
########################

########################
# workings to Table 1, p.980
########################
# Levy and Levy's (2002) Experiment 1, Task 1
# F = (-3000, 0.5; 4500, 0.5)
#   ~ -483 PT
# G = (-6000, 0.25; 3000, 0.75)
#   ~ -743 PT
# F > G
choice_ids <- c(1, 1, 1, 1)
gamble_ids <- c(1, 1, 2, 2)
outcome_ids <- c(1, 2, 1, 2)
objective_consequences <- c(-3000, 4500, -6000, 3000)
probability_strings <- 
	c("0.5", "0.5", "0.25", "0.75")
my_choices <- Choices(choice_ids=choice_ids,
	gamble_ids=gamble_ids, 
	outcome_ids=outcome_ids, 
	objective_consequences=objective_consequences, 
	probability_strings=probability_strings)
my_choices

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

#  gid od    oc  pr     dw    sv     pt
#    1  2  4500 0.5 0.4206  1640  689.8
#    1  1 -3000 0.5 0.4540 -2583 -482.6
#  gid od    oc   pr     dw    sv     pt
#    2  2  3000 0.75 0.5683  1148  652.3
#    2  1 -6000 0.25 0.2935 -4753 -742.8
#   cid gid  ev     pt ce rp
# 1   1   1 750 -482.6 -446 1196
# 2   1   2 750 -742.8 -728 1478


########################
# Levy and Levy's (2002) Experiment 2
# F = (-1600, 0.25; -200, 0.25; 1200, 0.25; 1600, 0.25)
#   ~ -216 PT
# G = (-1000, 0.25; -800, 0.25; 800, 0.25; 2000, 0.25)
#   ~ -138 PT
# G > F
choice_ids <- c(1, 1, 1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 1, 2, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 4, 1, 2, 3, 4)
objective_consequences <- c(-1600, -200, 1200, 1600, -1000, -800, 800, 2000)
probability_strings <- 
	c("0.25", "0.25", "0.25", "0.25", "0.25", "0.25", "0.25", "0.25")
my_choices <- Choices(choice_ids=choice_ids,
	gamble_ids=gamble_ids, 
	outcome_ids=outcome_ids, 
	objective_consequences=objective_consequences, 
	probability_strings=probability_strings)
my_choices

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

#  gid od    oc   pr     dw      sv     pt
#    1  4  1600 0.25 0.2907   660.1  191.9
#    1  3  1200 0.25 0.1299   512.5  258.5
#    1  2  -200 0.25 0.1605  -238.3  220.3
#    1  1 -1600 0.25 0.2935 -1485.3 -215.7
#  gid od    oc   pr     dw     sv     pt
#    2  4  2000 0.25 0.2907  803.4  233.6
#    2  3   800 0.25 0.1299  358.7  280.2
#    2  2  -800 0.25 0.1605 -807.1  150.7
#    2  1 -1000 0.25 0.2935 -982.2 -137.6
#   cid gid  ev     pt   ce  rp
# 1   1   1 250 -215.7 -178.6 428.6
# 2   1   2 250 -137.6 -107.2 357.2


########################
# Levy and Levy's (2002) Experiment 3, Task 3
# F = (-1500, 0.5; 4500, 0.5)
#   ~ 53 PT
# G = (-3000, 0.25; 3000, 0.75)
#   ~ -106 PT
# F > G
choice_ids <- c(1, 1, 1, 1)
gamble_ids <- c(1, 1, 2, 2)
outcome_ids <- c(1, 2, 1, 2)
objective_consequences <- c(-1500, 4500, -3000, 3000)
probability_strings <- 
	c("0.5", "0.5", "0.25", "0.75")
my_choices <- Choices(choice_ids=choice_ids,
	gamble_ids=gamble_ids, 
	outcome_ids=outcome_ids, 
	objective_consequences=objective_consequences, 
	probability_strings=probability_strings)
my_choices

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

#  gid od    oc  pr     dw    sv     pt
#    1  2  4500 0.5 0.4206  1640 689.82
#    1  1 -1500 0.5 0.4540 -1403  52.75
#  gid od    oc   pr     dw    sv     pt
#    2  2  3000 0.75 0.5683  1148  652.3
#    2  1 -3000 0.25 0.2935 -2583 -105.8
#   cid gid   ev     pt   ce rp
# 1   1   1 1500  52.75  90.59 1409
# 2   1   2 1500 -105.8 -79.47 1579