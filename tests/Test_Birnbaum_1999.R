library("pt")

########################	
# Birnbaum, M. H. (1999). The paradoxes of Allais, stochastic dominance, and decision weights. In J. Shanteau, B. A. Mellers & D. A. Schum (Eds.), Decision science and technology: Reflections on the contributions of Ward Edwards (pp. 27-52). Norwell, MA: Kluwer Academic Publishers.
########################	

########################	
# common ratio paradox
########################	

########################	
# Choice problem 1, p.28,33,36,48
# A = (3000, 1.0)
#   ~ 24.6 SWU(A) (p.33)
#   ~ 3000 SWAU (p.36)
#   ~ 3000 TAX (p.48)
# B = (4000, 0.8; 0, 0.2)
#   ~ 11.33 SWU(B) (p.33)
#   ~ 1566 SWAU (p.36)
#   ~ 1934 TAX (p.48)
# A > B
choice_ids <- c(1, 1, 1)
choice_ids <- c(1, 1, 1)
gamble_ids <- c(1, 2, 2)
outcome_ids <- c(1, 1, 2)
objective_consequences <- c(3000, 4000, 0)
probability_strings <- c("1.0", "0.8", "0.2")
my_choices <- Choices(choice_ids=choice_ids,
	gamble_ids=gamble_ids, 
	outcome_ids=outcome_ids, 
	objective_consequences=objective_consequences, 
	probability_strings=probability_strings)
my_choices

my_utility <- Utility(fun="power", 
	par=c(alpha=0.4, beta=0.4, lambda=1))
my_pwf <- 
	ProbWeight(fun="linear_in_log_odds", 
		par=c(alpha=0.4, beta=0.4))
compareSWU(my_choices, 
	prob_weight=my_pwf,	
	utility=my_utility,
	digits=4)

#   cid gid   ev   swu ce              rp
# 1   1   1 3000  24.6  3000 -0.000000000001819
# 2   1   2 3200 11.33 431.9               2768

my_utility <- Utility(fun="power", 
	par=c(alpha=0.4, beta=0.4, lambda=1))
my_pwf <- 
	ProbWeight(fun="linear_in_log_odds", 
		par=c(alpha=0.4, beta=0.4))
compareSWAU(my_choices, 
	prob_weight=my_pwf,	
	utility=my_utility,
	digits=4)

#   cid gid   ev  swau ce             rp
# 1   1   1 3000  24.6   3000 -0.000000000001819
# 2   1   2 3200 18.96   1566               1634

my_utility <- Utility(fun="linear", 
	par=c(lambda=1))
my_pwf <- 
	ProbWeight(fun="power", 
		par=c(alpha=0.7, beta=1.0))
compareTAX(my_choices, 
	prob_weight=my_pwf,	
	utility=my_utility,
	delta=-1, digits=4)

#   cid gid   ev  tax ce rp
# 1   1   1 3000 3000  3000     0
# 2   1   2 3200 1934  1934  1266



########################	
# Choice problem 2, p.28,33,36,48
# A' = (3000, 0.25; 0, 0.75)
#   ~ 5.04 SWU(A') (p.33)
#   ~ 215.2 SWAU (p.36)
#   ~ 633 TAX (p.48)
# B' = (4000, 0.2; 0, 0.8)
#   ~ 5.16 SWU(B') (p.33)
#   ~ 218.8 SWAU (p.36)
#   ~ 733 TAX (p.48)
# B' > A'
choice_ids <- c(1, 1, 1, 1)
gamble_ids <- c(1, 1, 2, 2)
outcome_ids <- c(1, 2, 1, 2)
objective_consequences <- c(3000, 0, 4000, 0)
probability_strings <- c("1/4", "3/4", "0.2", "0.8")
my_choices <- Choices(choice_ids=choice_ids,
	gamble_ids=gamble_ids, 
	outcome_ids=outcome_ids, 
	objective_consequences=objective_consequences, 
	probability_strings=probability_strings)
my_choices

my_utility <- Utility(fun="power", 
	par=c(alpha=0.4, beta=0.4, lambda=1))
my_pwf <- 
	ProbWeight(fun="linear_in_log_odds", 
		par=c(alpha=0.4, beta=0.4))
compareSWU(my_choices, 
	prob_weight=my_pwf,	
	utility=my_utility,
	digits=4)

#   cid gid  ev   swu ce rp
# 1   1   1 750  5.04 57.04   693
# 2   1   2 800 5.155 60.34 739.7

my_utility <- Utility(fun="power", 
	par=c(alpha=0.4, beta=0.4, lambda=1))
my_pwf <- 
	ProbWeight(fun="linear_in_log_odds", 
		par=c(alpha=0.4, beta=0.4))
compareSWAU(my_choices, 
	prob_weight=my_pwf,	
	utility=my_utility,
	digits=4)

#   cid gid  ev  swau ce rp
# 1   1   1 750 8.573  215.2  534.8
# 2   1   2 800  8.63  218.8  581.2

my_utility <- Utility(fun="linear", 
	par=c(lambda=1))
my_pwf <- 
	ProbWeight(fun="power", 
		par=c(alpha=0.7, beta=1.0))
compareTAX(my_choices, 
	prob_weight=my_pwf,	
	utility=my_utility,
	delta=-1, digits=4)

#   cid gid  ev   tax ce rp
# 1   1   1 750 633.4 633.4 116.6
# 2   1   2 800 732.8 732.8  67.2

########################	
# common consequence paradox
########################	

########################	
# Choice problem 3, p.29,33,36,48
# C = (500000, 1.0)
#   ~ 190.4 SWU(C) (p.33)
#   ~ 500000 SWAU (p.36)
#   ~ 500000 TAX (p.48)
# D = (1000000, 0.1; 500000, 0.89; 0, 0.01)
#   ~ 127.16 SWU(D) (p.33)
#   ~ 474156 SWAU (p.36)
#   ~ 405106 TAX (p.48)
# C > D
choice_ids <- c(1, 1, 1, 1)
gamble_ids <- c(1, 2, 2, 2)
outcome_ids <- c(1, 1, 2, 3)
objective_consequences <- c(500000, 1000000, 500000, 0)
probability_strings <- c("1.0", "0.1", "0.89", "0.01")
my_choices <- Choices(choice_ids=choice_ids,
	gamble_ids=gamble_ids, 
	outcome_ids=outcome_ids, 
	objective_consequences=objective_consequences, 
	probability_strings=probability_strings)
my_choices

my_utility <- Utility(fun="power", 
	par=c(alpha=0.4, beta=0.4, lambda=1))
my_pwf <- 
	ProbWeight(fun="linear_in_log_odds", 
		par=c(alpha=0.4, beta=0.4))
compareSWU(my_choices, 
	prob_weight=my_pwf,	
	utility=my_utility,
	digits=4)

#   cid gid     ev   swu  ce            rp
# 1   1   1 500000 190.4 500000 -0.0000000004075
# 2   1   2 545000 127.2 182326           362674

my_utility <- Utility(fun="power", 
	par=c(alpha=0.4, beta=0.4, lambda=1))
my_pwf <- 
	ProbWeight(fun="linear_in_log_odds", 
		par=c(alpha=0.4, beta=0.4))
compareSWAU(my_choices, 
	prob_weight=my_pwf,	
	utility=my_utility,
	digits=4)

#   cid gid     ev  swau ce           rp
# 1   1   1 500000 190.4 500000 -0.0000000004075
# 2   1   2 545000 186.4 474156            70844

my_utility <- Utility(fun="linear", 
	par=c(lambda=1))
my_pwf <- 
	ProbWeight(fun="power", 
		par=c(alpha=0.7, beta=1.0))
compareTAX(my_choices, 
	prob_weight=my_pwf,	
	utility=my_utility,
	delta=-1, digits=4)

#   cid gid     ev    tax  ce  rp
# 1   1   1 500000 500000 500000      0
# 2   1   2 545000 405106 405106 139894

########################	
# Choice problem 4, p.29,33,36,48
# C' = (500000, 0.11; 0, 0.89)
#   ~ 28.12 SWU(G) (p.34)
#   ~ 13432 SWAU (p.36)
#   ~ 62643 TAX (p.48)
# D' = (1000000, 0.1; 0, 0.9)
#   ~ 35.8 SWU(H) (p.34)
#   ~ 24011 SWAU (p.36)
#   ~ 117879 TAX (p.48)
# D' > C'
choice_ids <- c(1, 1, 1, 1)
gamble_ids <- c(1, 1, 2, 2)
outcome_ids <- c(1, 2, 1, 2)
objective_consequences <- c(500000, 0, 1000000, 0)
probability_strings <- c("0.11", "0.89", "0.1", "0.9")
my_choices <- Choices(choice_ids=choice_ids,
	gamble_ids=gamble_ids, 
	outcome_ids=outcome_ids, 
	objective_consequences=objective_consequences, 
	probability_strings=probability_strings)
my_choices

my_utility <- Utility(fun="power", 
	par=c(alpha=0.4, beta=0.4, lambda=1))
my_pwf <- 
	ProbWeight(fun="linear_in_log_odds", 
		par=c(alpha=0.4, beta=0.4))
compareSWU(my_choices, 
	prob_weight=my_pwf,	
	utility=my_utility,
	digits=4)

#   cid gid     ev   swu ce rp
# 1   1   1  55000 28.12  4194 50806
# 2   1   2 100000 35.78  7657 92343

my_utility <- Utility(fun="power", 
	par=c(alpha=0.4, beta=0.4, lambda=1))
my_pwf <- 
	ProbWeight(fun="linear_in_log_odds", 
		par=c(alpha=0.4, beta=0.4))
compareSWAU(my_choices, 
	prob_weight=my_pwf,	
	utility=my_utility,
	digits=4)

#   cid gid     ev  swau ce rp
# 1   1   1  55000  44.8  13432  41568
# 2   1   2 100000 56.51  24011  75989

my_utility <- Utility(fun="linear", 
	par=c(lambda=1))
my_pwf <- 
	ProbWeight(fun="power", 
		par=c(alpha=0.7, beta=1.0))
compareTAX(my_choices, 
	prob_weight=my_pwf,	
	utility=my_utility,
	delta=-1, digits=4)

#   cid gid     ev    tax  ce  rp
# 1   1   1  55000  62643  62643  -7643
# 2   1   2 100000 117879 117879 -17879

########################	
# stochastic dominance
########################	

########################	
# Choice problem 5, p.34,36,48
# E = (100, 0.5; 200, 0.5)
#   ~ 4.18 SWU(G) (p.34)
#   ~ 145 SWAU (p.36)
#   ~ 133 TAX (p.48)
# F = (100, 0.99; 200, 0.01)
#   ~ 5.01 SWU(H) (p.34)
#   ~ 106 SWAU (p.36)
#   ~ 103 TAX (p.48)
# E > F
choice_ids <- c(1, 1, 1, 1)
gamble_ids <- c(1, 1, 2, 2)
outcome_ids <- c(1, 2, 1, 2)
objective_consequences <- c(100, 200, 100, 200)
probability_strings <- c("1/2", "1/2", "0.99", "0.01")
my_choices <- Choices(choice_ids=choice_ids,
	gamble_ids=gamble_ids, 
	outcome_ids=outcome_ids, 
	objective_consequences=objective_consequences, 
	probability_strings=probability_strings)
my_choices

my_utility <- Utility(fun="power", 
	par=c(alpha=0.4, beta=0.4, lambda=1))
my_pwf <- 
	ProbWeight(fun="linear_in_log_odds", 
		par=c(alpha=0.4, beta=0.4))
compareSWU(my_choices, 
	prob_weight=my_pwf,	
	utility=my_utility,
	digits=4)

#   cid gid  ev   swu ce rp
# 1   1   1 150 4.181 35.75 114.2
# 2   1   2 101 5.012 56.24 44.76

my_utility <- Utility(fun="power", 
	par=c(alpha=0.4, beta=0.4, lambda=1))
my_pwf <- 
	ProbWeight(fun="linear_in_log_odds", 
		par=c(alpha=0.4, beta=0.4))
compareSWAU(my_choices, 
	prob_weight=my_pwf,	
	utility=my_utility,
	digits=4)

#   cid gid  ev  swau ce rp
# 1   1   1 150 7.318  144.8  5.151
# 2   1   2 101 6.465  106.3  -5.28

my_utility <- Utility(fun="linear", 
	par=c(lambda=1))
my_pwf <- 
	ProbWeight(fun="power", 
		par=c(alpha=0.7, beta=1.0))
compareTAX(my_choices, 
	prob_weight=my_pwf,	
	utility=my_utility,
	delta=-1, digits=4)

#   cid gid  ev   tax ce rp
# 1   1   1 150 133.3 133.3 16.67
# 2   1   2 101 102.6 102.6 -1.57


########################	
# Choice problem 6, p.34,36,48
# G = (110, 0.5; 120, 0.5)
#   ~ 3.81 SWU(G) (p.34)
#   ~ 115 SWAU (p.36)
#   ~ 113 TAX (p.48)
# H = (101, 0.01; 102, 0.01; 103, 0.98)
#   ~ 4.94 SWU(H) (p.34)
#   ~ 102 SWAU (p.36)
#   ~ 102 TAX (p.48)
# G > H
choice_ids <- c(1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 1, 2, 3)
objective_consequences <- c(110, 120, 101, 102, 103)
probability_strings <- c("1/2", "1/2", "0.01", "0.01", "0.98")
my_choices <- Choices(choice_ids=choice_ids,
	gamble_ids=gamble_ids, 
	outcome_ids=outcome_ids, 
	objective_consequences=objective_consequences, 
	probability_strings=probability_strings)
my_choices

my_utility <- Utility(fun="power", 
	par=c(alpha=0.4, beta=0.4, lambda=1))
my_pwf <- 
	ProbWeight(fun="linear_in_log_odds", 
		par=c(alpha=0.4, beta=0.4))
compareSWU(my_choices, 
	prob_weight=my_pwf,	
	utility=my_utility,
	digits=4)

#   cid gid  ev   swu ce rp
# 1   1   1 115 3.812 28.37 86.63
# 2   1   2 103 4.941 54.26 48.71

my_utility <- Utility(fun="power", 
	par=c(alpha=0.4, beta=0.4, lambda=1))
my_pwf <- 
	ProbWeight(fun="linear_in_log_odds", 
		par=c(alpha=0.4, beta=0.4))
compareSWAU(my_choices, 
	prob_weight=my_pwf,	
	utility=my_utility,
	digits=4)

#   cid gid  ev  swau ce  rp
# 1   1   1 115 6.671  114.9 0.06525
# 2   1   2 103 6.379  102.8  0.2028

my_utility <- Utility(fun="linear", 
	par=c(lambda=1))
my_pwf <- 
	ProbWeight(fun="power", 
		par=c(alpha=0.7, beta=1.0))
compareTAX(my_choices, 
	prob_weight=my_pwf,	
	utility=my_utility,
	delta=-1, digits=4)

#   cid gid  ev   tax ce  rp
# 1   1   1 115 113.3 113.3  1.667
# 2   1   2 103 102.2 102.2 0.7854



########################	
# violations of branch independence
########################	

########################	
# Choice problem 7, p.39,48
# S = (5, 1/3; 40, 1/3; 44, 1/3)
#   ~ 23.17 TAX 
# R = (5, 1/3; 10, 1/3; 98, 1/3)
#   ~ 22.16 TAX
# S > R
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(5, 40, 44, 5, 10, 98)
probability_strings <- c("1/3", "1/3", "1/3", "1/3", "1/3", "1/3")
my_choices <- Choices(choice_ids=choice_ids,
	gamble_ids=gamble_ids, 
	outcome_ids=outcome_ids, 
	objective_consequences=objective_consequences, 
	probability_strings=probability_strings)
my_choices

my_utility <- Utility(fun="linear", 
	par=c(lambda=1))
my_pwf <- 
	ProbWeight(fun="power", 
		par=c(alpha=0.7, beta=1.0))
compareTAX(my_choices, 
	prob_weight=my_pwf,	
	utility=my_utility,
	delta=-1, digits=4)

#   cid gid    ev   tax ce rp
# 1   1   1 29.67 23.17 23.17   6.5
# 2   1   2 37.67 22.17 22.17  15.5

########################	
# Choice problem 8, p.39,48
# S' = (40, 1/3; 44, 1/3; 107, 1/3)
#   ~ 52.49 TAX 
# R' = (10, 1/3; 98, 1/3; 107, 1/3)
#   ~ 55.51 TAX
# R' > S'
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(40, 44, 107, 10, 98, 107)
probability_strings <- c("1/3", "1/3", "1/3", "1/3", "1/3", "1/3")
my_choices <- Choices(choice_ids=choice_ids,
	gamble_ids=gamble_ids, 
	outcome_ids=outcome_ids, 
	objective_consequences=objective_consequences, 
	probability_strings=probability_strings)
my_choices

my_utility <- Utility(fun="linear", 
	par=c(lambda=1))
my_pwf <- 
	ProbWeight(fun="power", 
		par=c(alpha=0.7, beta=1.0))
compareTAX(my_choices, 
	prob_weight=my_pwf,	
	utility=my_utility,
	delta=-1, digits=4)

#   cid gid    ev  tax ce rp
# 1   1   1 63.67 52.5  52.5 11.17
# 2   1   2 71.67 55.5  55.5 16.17

########################	
# violations of lower cumulative independence
########################	

########################	
# Choice problem 9, p.43,48
# S = (3, 0.8; 48, 0.1; 52, 0.1)
#   ~ 14.05 TAX 
# R = (3, 0.8; 10, 0.1; 98, 0.1)
#   ~ 11.67 TAX
# S > R
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(3, 48, 52, 3, 10, 98)
probability_strings <- c("0.8", "0.1", "0.1", "0.8", "0.1", "0.1")
my_choices <- Choices(choice_ids=choice_ids,
	gamble_ids=gamble_ids, 
	outcome_ids=outcome_ids, 
	objective_consequences=objective_consequences, 
	probability_strings=probability_strings)
my_choices

my_utility <- Utility(fun="linear", 
	par=c(lambda=1))
my_pwf <- 
	ProbWeight(fun="power", 
		par=c(alpha=0.7, beta=1.0))
compareTAX(my_choices, 
	prob_weight=my_pwf,	
	utility=my_utility,
	delta=-1, digits=4)

#   cid gid   ev   tax ce  rp
# 1   1   1 12.4 14.05 14.05 -1.654
# 2   1   2 13.2 11.67 11.67  1.531

########################	
# Choice problem 10, p.43,48
# S'' = (10, 0.8; 52, 0.2)
#   ~ 17.69 TAX 
# R'' = (10, 0.9; 98, 0.1)
#   ~ 20.37 TAX
# R'' > S''
choice_ids <- c(1, 1, 1, 1)
gamble_ids <- c(1, 1, 2, 2)
outcome_ids <- c(1, 2, 1, 2)
objective_consequences <- c(10, 52, 10, 98)
probability_strings <- c("0.8", "0.2", "0.9", "0.1")
my_choices <- Choices(choice_ids=choice_ids,
	gamble_ids=gamble_ids, 
	outcome_ids=outcome_ids, 
	objective_consequences=objective_consequences, 
	probability_strings=probability_strings)
my_choices

my_utility <- Utility(fun="linear", 
	par=c(lambda=1))
my_pwf <- 
	ProbWeight(fun="power", 
		par=c(alpha=0.7, beta=1.0))
compareTAX(my_choices, 
	prob_weight=my_pwf,	
	utility=my_utility,
	delta=-1, digits=4)

#   cid gid   ev   tax ce  rp
# 1   1   1 18.4 17.69 17.69 0.7056
# 2   1   2 18.8 20.37 20.37 -1.573


########################	
# violations of upper cumulative independence
########################	

########################	
# Choice problem 11, p.43,48
# S' = (40, 0.1; 44, 0.1; 110, 0.8)
#   ~ 65.03 TAX 
# R' = (10, 0.1; 98, 0.1; 110, 0.8)
#   ~ 69.59 TAX
# R' > S'
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(40, 44, 110, 10, 98, 110)
probability_strings <- c("0.1", "0.1", "0.8", "0.1", "0.1", "0.8")
my_choices <- Choices(choice_ids=choice_ids,
	gamble_ids=gamble_ids, 
	outcome_ids=outcome_ids, 
	objective_consequences=objective_consequences, 
	probability_strings=probability_strings)
my_choices

my_utility <- Utility(fun="linear", 
	par=c(lambda=1))
my_pwf <- 
	ProbWeight(fun="power", 
		par=c(alpha=0.7, beta=1.0))
compareTAX(my_choices, 
	prob_weight=my_pwf,	
	utility=my_utility,
	delta=-1, digits=4)

#   cid gid   ev   tax ce rp
# 1   1   1 96.4 65.03 65.03 31.37
# 2   1   2 98.8 69.59 69.59 29.21

########################	
# Choice problem 12, p.43,48
# S''' = (40, 0.2; 98, 0.8)
#   ~ 68.04 TAX 
# R''' = (10, 0.1; 98, 0.9)
#   ~ 58.29 TAX
# S''' > R'''
choice_ids <- c(1, 1, 1, 1)
gamble_ids <- c(1, 1, 2, 2)
outcome_ids <- c(1, 2, 1, 2)
objective_consequences <- c(40, 98, 10, 98)
probability_strings <- c("0.2", "0.8", "0.1", "0.9")
my_choices <- Choices(choice_ids=choice_ids,
	gamble_ids=gamble_ids, 
	outcome_ids=outcome_ids, 
	objective_consequences=objective_consequences, 
	probability_strings=probability_strings)
my_choices

my_utility <- Utility(fun="linear", 
	par=c(lambda=1))
my_pwf <- 
	ProbWeight(fun="power", 
		par=c(alpha=0.7, beta=1.0))
compareTAX(my_choices, 
	prob_weight=my_pwf,	
	utility=my_utility,
	delta=-1, digits=4)

#   cid gid   ev   tax ce rp
# 1   1   1 86.4 68.04 68.04 18.36
# 2   1   2 89.2 58.29 58.29 30.91

########################	
# violations of stochastic dominance
########################	

########################	
# Choice problem 13, p.44,48
# G+ = (12, 0.05; 14, 0.05; 96, 0.9)
#   ~ 45.77 TAX 
# G- = (12, 0.1; 90, 0.05; 96, 0.85)
#   ~ 63.10 TAX
# G- > G+
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(12, 14, 96, 12, 90, 96)
probability_strings <- c("0.05", "0.05", "0.9", "0.1", "0.05", "0.85")
my_choices <- Choices(choice_ids=choice_ids,
	gamble_ids=gamble_ids, 
	outcome_ids=outcome_ids, 
	objective_consequences=objective_consequences, 
	probability_strings=probability_strings)
my_choices

my_utility <- Utility(fun="linear", 
	par=c(lambda=1))
my_pwf <- 
	ProbWeight(fun="power", 
		par=c(alpha=0.7, beta=1.0))
compareTAX(my_choices, 
	prob_weight=my_pwf,	
	utility=my_utility,
	delta=-1, digits=4)


#   cid gid   ev   tax ce rp
# 1   1   1 87.7 45.77 45.77 41.93
# 2   1   2 87.3  63.1  63.1  24.2

########################	
# coalescing & event splitting effects
########################	

########################	
# Choice problem 14, p.45,48
# S1 = (8, 0.7; 0, 0.3)
#   ~ 3.44 TAX 
# R1 = (24, 0.3; 0, 0.7)
#   ~ 5.69 TAX
# R1 > S1
choice_ids <- c(1, 1, 1, 1)
gamble_ids <- c(1, 1, 2, 2)
outcome_ids <- c(1, 2, 1, 2)
objective_consequences <- c(8, 0, 24, 0)
probability_strings <- c("0.7", "0.3", "0.3", "0.7")
my_choices <- Choices(choice_ids=choice_ids,
	gamble_ids=gamble_ids, 
	outcome_ids=outcome_ids, 
	objective_consequences=objective_consequences, 
	probability_strings=probability_strings)
my_choices

my_utility <- Utility(fun="linear", 
	par=c(lambda=1))
my_pwf <- 
	ProbWeight(fun="power", 
		par=c(alpha=0.7, beta=1.0))
compareTAX(my_choices, 
	prob_weight=my_pwf,	
	utility=my_utility,
	delta=-1, digits=4)

#   cid gid  ev   tax ce rp
# 1   1   1 5.6 3.435 3.435 2.165
# 2   1   2 7.2 5.695 5.695 1.505

########################	
# Choice problem 15, p.45,48-49
# S2 = (8, 0.3; 8, 0.4; 0, 0.3)
#   ~ 4.14 TAX 
# R2 = (24, 0.3; 0, 0.4; 0, 0.3)
#   ~ 3.72 TAX
# S2 > R2
choice_ids <- c(1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3)
objective_consequences <- c(8, 8, 0, 24, 0, 0)
probability_strings <- c("0.3", "0.4", "0.3", "0.3", "0.4", "0.3")
my_choices <- Choices(choice_ids=choice_ids,
	gamble_ids=gamble_ids, 
	outcome_ids=outcome_ids, 
	objective_consequences=objective_consequences, 
	probability_strings=probability_strings)
my_choices

my_utility <- Utility(fun="linear", 
	par=c(lambda=1))
my_pwf <- 
	ProbWeight(fun="power", 
		par=c(alpha=0.7, beta=1.0))
compareTAX(my_choices, 
	prob_weight=my_pwf,	
	utility=my_utility,
	delta=-1, digits=4)

#   cid gid  ev   tax ce rp
# 1   1   1 5.6 4.138 4.138 1.462
# 2   1   2 7.2 3.723 3.723 3.477

########################	
# violations of distribution independence
########################	

########################	
# Choice problem 16, p.46,49
# S = (4, 0.59; 45, 0.2; 49, 0.2; 110, 0.01)
#   ~ 21.70 TAX 
# R = (4, 0.59; 11, 0.2; 97, 0.2; 110, 0.01)
#   ~ 20.56 TAX
# S > R

choice_ids <- c(1, 1, 1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 1, 2, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 4, 1, 2, 3, 4)
objective_consequences <- c(4, 45, 49, 110, 4, 11, 97, 110)
probability_strings <- c("0.59", "0.2", "0.2", "0.01", "0.59", "0.2", "0.2", "0.01")
my_choices <- Choices(choice_ids=choice_ids,
	gamble_ids=gamble_ids, 
	outcome_ids=outcome_ids, 
	objective_consequences=objective_consequences, 
	probability_strings=probability_strings)
my_choices

my_utility <- Utility(fun="linear", 
	par=c(lambda=1))
my_pwf <- 
	ProbWeight(fun="power", 
		par=c(alpha=0.7, beta=1.0))
compareTAX(my_choices, 
	prob_weight=my_pwf,	
	utility=my_utility,
	delta=-1, digits=4)

#   cid gid    ev   tax ce  rp
# 1   1   1 22.26  21.7  21.7 0.5595
# 2   1   2 25.06 20.56 20.56  4.501


########################	
# Choice problem 17, p.46,49
# S' = (4, 0.01; 45, 0.2; 49, 0.2; 110, 0.59)
#   ~ 49.85 TAX 
# R' = (4, 0.01; 11, 0.2; 97, 0.2; 110, 0.59)
#   ~ 50.03 TAX
# R' > S'
choice_ids <- c(1, 1, 1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 1, 2, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 4, 1, 2, 3, 4)
objective_consequences <- c(4, 45, 49, 110, 4, 11, 97, 110)
probability_strings <- c("0.01", "0.2", "0.2", "0.59", "0.01", "0.2", "0.2", "0.59")
my_choices <- Choices(choice_ids=choice_ids,
	gamble_ids=gamble_ids, 
	outcome_ids=outcome_ids, 
	objective_consequences=objective_consequences, 
	probability_strings=probability_strings)
my_choices

my_utility <- Utility(fun="linear", 
	par=c(lambda=1))
my_pwf <- 
	ProbWeight(fun="power", 
		par=c(alpha=0.7, beta=1.0))
compareTAX(my_choices, 
	prob_weight=my_pwf,	
	utility=my_utility,
	delta=-1, digits=4)

#   cid gid    ev   tax ce rp
# 1   1   1 83.74 49.85 49.85 33.89
# 2   1   2 86.54 50.03 50.03 36.51
