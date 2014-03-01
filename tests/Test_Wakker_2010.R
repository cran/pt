library("pt")

########################	
# Wakker, P. P. (2010). Prospect theory: For risk and ambiguity. Cambridge, UK: Cambridge University Press.
########################

########################
# workings to Figure 2.4.1.a, p.51
# lower prospect has higher EU
########################

my_input_file <- system.file("external", "figure_2.4.1.a.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
general_power_utility <- Utility(fun="power", par=c(alpha=0.5, beta=0.5, lambda=1))
compareEU(my_choices, utility=general_power_utility, digits=4)

#   cid gid   ev    eu  ce   rp
# 1   1   1   70 8.345 69.64  0.359
# 2   1   2 82.5 9.057 82.02 0.4762


########################
# workings to Figure 2.4.1.b, p.51
# upper prospect has higher EU
########################

my_input_file <- system.file("external", "figure_2.4.1.b.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
general_power_utility <- Utility(fun="power", par=c(alpha=0.5, beta=0.5, lambda=1))
compareEU(my_choices, utility=general_power_utility, digits=4)

#   cid gid ev    eu  ce   rp
# 1   1   1 34 5.769 33.28 0.7206
# 2   1   2 32 5.584 31.18 0.8235


########################
# workings to Figure 2.4.1.c, p.51
# upper prospect has higher EU
########################

my_input_file <- system.file("external", "figure_2.4.1.c.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
general_power_utility <- Utility(fun="power", par=c(alpha=0.5, beta=0.5, lambda=1))
compareEU(my_choices, utility=general_power_utility, digits=4)

#   cid gid ev    eu ce                  rp
# 1   1   1 66 8.105 65.7                0.3015
# 2   1   2 50 7.071   50 -0.000000000000007105


########################
# workings to Figure 2.4.1.d, p.51
# upper prospect has higher EU
########################

my_input_file <- system.file("external", "figure_2.4.1.d.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
general_power_utility <- Utility(fun="power", par=c(alpha=0.5, beta=0.5, lambda=1))
compareEU(my_choices, utility=general_power_utility, digits=4)

#   cid gid   ev   eu  ce   rp
# 1   1   1 3.88 1.94 3.764 0.1164
# 2   1   2 4.96 1.24 1.538  3.422


########################
# workings to Figure 2.4.1.e, p.51
# upper prospect has higher EU
########################

my_input_file <- system.file("external", "figure_2.4.1.e.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
general_power_utility <- Utility(fun="power", par=c(alpha=0.5, beta=0.5, lambda=1))
compareEU(my_choices, utility=general_power_utility, digits=4)

#   cid gid       ev   eu     ce            rp
# 1   1   1 40000000 5657 32000000         8000000
# 2   1   2 10000000 3162 10000000 -0.000000001863


########################
# workings to Figure 2.4.1.f, p.51
# upper prospect has higher EU
########################

my_input_file <- system.file("external", "figure_2.4.1.f.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
general_power_utility <- Utility(fun="power", par=c(alpha=0.5, beta=0.5, lambda=1))
compareEU(my_choices, utility=general_power_utility, digits=4)

#   cid gid      ev    eu  ce    rp
# 1   1   1 2000000 282.8 80000 1920000
# 2   1   2  500000 158.1 25000  475000


########################
# workings to Figure 2.4.1.g, p.51
# upper prospect has higher EU
########################

my_input_file <- system.file("external", "figure_2.4.1.g.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
general_power_utility <- Utility(fun="power", par=c(alpha=0.5, beta=0.5, lambda=1))
compareEU(my_choices, utility=general_power_utility, digits=4)

#   cid gid       ev   eu     ce            rp
# 1   1   1 13900000 3522 12401201         1498799
# 2   1   2 10000000 3162 10000000 -0.000000001863


########################
# workings to Figure 2.4.1.h, p.51
# upper prospect has higher EU
########################

my_input_file <- system.file("external", "figure_2.4.1.h.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
general_power_utility <- Utility(fun="power", par=c(alpha=0.5, beta=0.5, lambda=1))
compareEU(my_choices, utility=general_power_utility, digits=4)

#   cid gid      ev    eu   ce    rp
# 1   1   1 5000000 707.1 500000 4500000
# 2   1   2 1100000 347.9 121000  979000


########################
# workings to Figure 2.4.2.a, p.52
# gamble has a higher EU than the sure thing
########################

my_input_file <- system.file("external", "figure_2.4.2.a.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
my_utility <- Utility(fun="power", par=c(alpha=0.5, beta=0.5, lambda=1))
compareEU(my_choices, utility=my_utility, digits=4)

#   cid gid   ev    eu  ce rp
# 1   1   1 91.2 9.308 86.64 4.56
# 2   1   2 91.2  9.55  91.2    0

########################
# workings to Figure 2.4.2.b
########################

my_input_file <- system.file("external", "figure_2.4.2.b.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
my_utility <- Utility(fun="power", par=c(alpha=0.5, beta=0.5, lambda=1))
compareEU(my_choices, my_utility, digits=4)

#   cid gid   ev    eu  ce                  rp
# 1   1   1 3.88  1.94 3.764                0.1164
# 2   1   2    3 1.732     3 0.0000000000000004441

########################
# solution to Exercise 2.5.2.a
########################

my_input_file <- system.file("external", "exercise_2.5.2.a.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
my_utility <- Utility(fun="power", par=c(alpha=0.5, beta=0.5, lambda=1))
compareEU(my_choices, my_utility, digits=4)

#   cid gid   ev  eu  ce rp
# 1   1   1 45.7 6.7 44.89 0.81

########################
# solution to Exercise 2.5.2.b
########################

my_input_file <- system.file("external", "exercise_2.5.2.b.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
my_utility <- Utility(fun="power", par=c(alpha=0.5, beta=0.5, lambda=1))
compareEU(my_choices, my_utility, digits=4)

#   cid gid   ev  eu  ce rp
# 1   1   1 61.5 7.5 56.25 5.25

########################
# workings to Figure 4.12.1.a
########################

# illustration of Allais paradox. people should choose both a1 and b1 or a2 and b2 to be consistent.
# but they don't.

my_input_file <- system.file("external", "figure_4.12.1.a1.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
my_utility <- Utility(fun="power", par=c(alpha=0.5, beta=0.5, lambda=1))
compareEU(my_choices, my_utility, digits=4)

#   cid gid   ev    eu  ce rp
# 1   1   1 3250 20.55 422.5 2828

my_input_file <- system.file("external", "figure_4.12.1.a2.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
my_utility <- Utility(fun="power", par=c(alpha=0.5, beta=0.5, lambda=1))
compareEU(my_choices, my_utility, digits=4)

#   cid gid   ev    eu ce rp
# 1   1   1 4500 16.43  270 4230

########################
# workings to Figure 4.12.1.b
########################

my_input_file <- system.file("external", "figure_4.12.1.b1.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
my_utility <- Utility(fun="power", par=c(alpha=0.5, beta=0.5, lambda=1))
compareEU(my_choices, my_utility, digits=4)

#   cid gid    ev    eu  ce               rp
# 1   1   1 25000 158.1 25000 -0.000000000003638

my_input_file <- system.file("external", "figure_4.12.1.b2.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
my_utility <- Utility(fun="power", par=c(alpha=0.5, beta=0.5, lambda=1))
compareEU(my_choices, my_utility, digits=4)

#   cid gid    ev  eu  ce rp
# 1   1   1 26250 154 23713 2537

########################
# solution to Exercise 5.4.3
########################

my_input_file <- system.file("external", "exercise_5.4.3.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
linear_utility <- Utility(fun="linear", par=c(lambda=1))
pow_prob_weight <- ProbWeight(fun="power", par=c(alpha=0.5, beta=1.0))
compareRDU(my_choices, pow_prob_weight, linear_utility, digits=4)

#  gid oid oc      pr     rnk       w      dw sv    rdu
#    1   1 80 0.16667 0.00000 0.00000 0.40825 80 32.660
#    1   2 30 0.50000 0.16667 0.40825 0.40825 30 44.907
#    1   3 20 0.33333 0.66667 0.81650 0.18350 20 48.577
#   cid gid ev   rdu ce  rp
# 1   1   1 35 48.58 48.58 -13.58

########################
# workings to Example 5.6.1
########################

my_input_file <- system.file("external", "example_5.6.1.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
linear_utility <- Utility(fun="linear", par=c(lambda=1))
pow_prob_weight <- ProbWeight("power", c(alpha=2.0, beta=1.0))
compareRDU(my_choices, pow_prob_weight, linear_utility, digits=4)

#  gid oid oc  pr rnk    w   dw sv  rdu
#    1   2 80 0.2 0.0 0.00 0.04 80  3.2
#    1   1 50 0.3 0.2 0.04 0.21 50 13.7
#    1   3 10 0.5 0.5 0.25 0.75 10 21.2
#   cid gid ev  rdu ce rp
# 1   1   1 36 21.2  21.2  14.8

########################
# solution to Exercise 5.6.1.a
########################

my_input_file <- system.file("external", "exercise_5.6.1.a.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
general_power_utility <- Utility(fun="general_power", par=c(alpha=0.5, beta=10, gamma=0.5, delta=10, lambda=1))
pow_prob_weight <- ProbWeight(fun="power", par=c(alpha=2.0, beta=1.0))
compareRDU(my_choices, pow_prob_weight, general_power_utility, digits=4)

#  gid oid oc      pr     rnk       w      dw sv     rdu
#    1   2 64 0.33333 0.00000 0.00000 0.11111 80  8.8889
#    1   1 49 0.16667 0.33333 0.11111 0.13889 70 18.6111
#    1   3  9 0.50000 0.50000 0.25000 0.75000 30 41.1111
#   cid gid ev   rdu ce rp
# 1   1   1 34 41.11  16.9  17.1

########################
# solution to Exercise 5.6.1.b
########################

my_input_file <- system.file("external", "exercise_5.6.1.b.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
general_power_utility <- Utility(fun="general_power", par=c(alpha=0.5, beta=10, gamma=0.5, delta=10, lambda=1))
pow_prob_weight <- ProbWeight(fun="power", par=c(alpha=2.0, beta=1.0))
compareRDU(my_choices, pow_prob_weight, general_power_utility, digits=4)

#  gid oid oc       pr      rnk         w        dw     sv      rdu
#    1   1 80 0.083333 0.000000 0.0000000 0.0069444 89.443  0.62113
#    1   2 60 0.250000 0.083333 0.0069444 0.1041667 77.460  8.68985
#    1   3 40 0.166667 0.333333 0.1111111 0.1388889 63.246 17.47395
#    1   4 20 0.500000 0.500000 0.2500000 0.7500000 44.721 51.01497
#   cid gid    ev   rdu ce rp
# 1   1   1 38.33 51.01 26.03 12.31

########################
# solution to Exercise 5.6.2
########################

my_input_file <- system.file("external", "exercise_5.6.2.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
linear_utility <- Utility(fun="linear", par=c(lambda=1))
pow_prob_weight <- ProbWeight(fun="power", par=c(alpha=2.0, beta=1.0))
compareRDU(my_choices, pow_prob_weight, linear_utility, digits=4)

#  gid oid oc      pr     rnk        w       dw sv     rdu
#    1   1 80 0.16667 0.00000 0.000000 0.027778 80  2.2222
#    1   2 30 0.50000 0.16667 0.027778 0.416667 30 14.7222
#    1   3 20 0.33333 0.66667 0.444444 0.555556 20 25.8333
#   cid gid ev   rdu ce rp
# 1   1   1 35 25.83 25.83 9.167

########################
# solution to Assignment 5.6.3
########################

my_input_file <- system.file("external", "assignment_5.6.3.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
general_power_utility <- Utility(fun="general_power", par=c(alpha=0.5, beta=10, gamma=0.5, delta=10, lambda=1))
pow_prob_weight <- ProbWeight(fun="power", par=c(alpha=2.0, beta=1.0))
compareRDU(my_choices, pow_prob_weight, general_power_utility, digits=4)

#  gid oid oc   pr  rnk      w     dw sv    rdu
#    1   1 81 0.25 0.00 0.0000 0.0625 90  5.625
#    1   2 25 0.50 0.25 0.0625 0.5000 50 30.625
#    1   3  0 0.25 0.75 0.5625 0.4375  0 30.625
#   cid gid    ev   rdu ce rp
# 1   1   1 32.75 30.62 9.379 23.37

########################
# workings to Figure 6.3.1.a
########################

my_input_file <- system.file("external", "figure_6.3.1.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
general_linear_utility <- Utility(fun="general_linear", par=c(alpha=1/100, beta=1/100, lambda=1))
pow_prob_weight <- ProbWeight(fun="power", par=c(alpha=2.0, beta=1.0))
compareRDU(my_choices, pow_prob_weight, general_linear_utility, digits=4)

#  gid oid oc   pr  rnk      w     dw  sv    rdu
#    1   1 80 0.25 0.00 0.0000 0.0625 0.8 0.0500
#    1   2 60 0.25 0.25 0.0625 0.1875 0.6 0.1625
#    1   3 40 0.25 0.50 0.2500 0.3125 0.4 0.2875
#    1   4 20 0.25 0.75 0.5625 0.4375 0.2 0.3750
#   cid gid ev   rdu ce rp
# 1   1   1 50 0.375  37.5  12.5

########################
# workings to Figure 6.3.1.b
########################

my_input_file <- system.file("external", "figure_6.3.1.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
general_linear_utility <- Utility(fun="general_linear", par=c(alpha=1/100, beta=1/100, lambda=1))
pow_prob_weight <- ProbWeight(fun="power", par=c(alpha=0.5, beta=1.0))
compareRDU(my_choices, pow_prob_weight, general_linear_utility, digits=4)

#  gid oid oc   pr  rnk       w      dw  sv     rdu
#    1   1 80 0.25 0.00 0.00000 0.50000 0.8 0.40000
#    1   2 60 0.25 0.25 0.50000 0.20711 0.6 0.52426
#    1   3 40 0.25 0.50 0.70711 0.15892 0.4 0.58783
#    1   4 20 0.25 0.75 0.86603 0.13397 0.2 0.61463
#   cid gid ev    rdu ce  rp
# 1   1   1 50 0.6146 61.46 -11.46

########################
# solution to Exercise 7.2.1
########################

my_input_file <- system.file("external", "exercise_7.2.1.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
general_linear_utility <- Utility(fun="general_linear", par=c(alpha=1/100, beta=1/100, lambda=1))
tk_1992_positive_probWeight <- ProbWeight(fun="Tversky_Kahneman_1992", par=c(c=0.5))
compareRDU(my_choices, tk_1992_positive_probWeight, general_linear_utility, digits=4)

#  gid oid oc  pr rnk       w      dw  sv     rdu
#    1   1 70 0.2 0.0 0.00000 0.24845 0.7 0.17392
#    1   2 50 0.6 0.2 0.24845 0.24845 0.5 0.29814
#    1   3 30 0.2 0.8 0.49690 0.50310 0.3 0.44907
#   cid gid ev    rdu ce rp
# 1   1   1 50 0.4491 44.91 5.093

########################
# workings to Exercise 7.2.3
########################

my_input_file <- system.file("external", "exercise_7.2.3.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
example_7_2_1_cara_utility <- Utility(fun="exponential", par=c(alpha=0.005, lambda=1))
tk_1992_positive_probWeight <- ProbWeight(fun="Tversky_Kahneman_1992", par=c(c=0.61))
compareRDU(my_choices, tk_1992_positive_probWeight, example_7_2_1_cara_utility, digits=4)

#  gid oid  oc  pr rnk       w      dw       sv      rdu
#    1   1  80 0.5 0.0 0.00000 0.42064  0.32968 0.138676
#    1   2 -20 0.5 0.5 0.42064 0.57936 -0.10517 0.077744
#   cid gid ev     rdu ce rp
# 1   1   1 30 0.07774 16.19 13.81

########################
# workings to Example 9.3.3
########################

# PT(U, w, lambda)
my_input_file <- system.file("external", "example_9.3.3.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
example_9_3_3_cara_utility_with_loss_aversion <- Utility(fun="exponential", par=c(alpha=0.005, lambda=2.25))
example_9_3_3_positive_probability_weighting <- ProbWeight(fun="Tversky_Kahneman_1992", par=c(c=0.61))
example_9_3_3_negative_probability_weighting <- ProbWeight(fun="Tversky_Kahneman_1992", par=c(c=0.61))
comparePT(my_choices, tk_1992_positive_probWeight, example_9_3_3_negative_probability_weighting, example_9_3_3_cara_utility_with_loss_aversion, digits=4)

#  gid od  oc  pr     dw      sv      pt
#    1  1  80 0.5 0.4206  0.3297 0.13868
#    1  2 -20 0.5 0.4206 -0.2366 0.03914
#   cid gid ev      pt  ce  rp
# 1   1   1 30 0.03914 7.985 22.01

# PT(w, lambda)
my_input_file <- system.file("external", "example_9.3.3.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
example_9_3_3_linear_basic_utility_with_loss_aversion <- Utility(fun="linear", par=c(lambda=2.25))
example_9_3_3_positive_probability_weighting <- ProbWeight(fun="Tversky_Kahneman_1992", par=c(c=0.61))
example_9_3_3_negative_probability_weighting <- ProbWeight(fun="Tversky_Kahneman_1992", par=c(c=0.61))
comparePT(my_choices, tk_1992_positive_probWeight, example_9_3_3_negative_probability_weighting, example_9_3_3_linear_basic_utility_with_loss_aversion, digits=4)

#  gid od  oc  pr     dw  sv    pt
#    1  1  80 0.5 0.4206  80 33.65
#    1  2 -20 0.5 0.4206 -45 14.72
#   cid gid ev    pt  ce  rp
# 1   1   1 30 14.72 14.72 15.28

# PT(U, lambda)
my_input_file <- system.file("external", "example_9.3.3.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
example_9_3_3_cara_utility_with_loss_aversion <- Utility(fun="exponential", par=c(alpha=0.005, lambda=2.25))
example_9_3_3_linear_probability_weighting <- ProbWeight(fun="Tversky_Kahneman_1992", par=c(c=1))
example_9_3_3_linear_probability_weighting <- ProbWeight(fun="Tversky_Kahneman_1992", par=c(c=1))
comparePT(my_choices, example_9_3_3_linear_probability_weighting, example_9_3_3_linear_probability_weighting, example_9_3_3_cara_utility_with_loss_aversion, digits=4)

#  gid od  oc  pr  dw      sv      pt
#    1  1  80 0.5 0.5  0.3297 0.16484
#    1  2 -20 0.5 0.5 -0.2366 0.04652
#   cid gid ev      pt  ce  rp
# 1   1   1 30 0.04652 9.528 20.47

# PT(lambda)
my_input_file <- system.file("external", "example_9.3.3.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
example_9_3_3_linear_basic_utility_with_loss_aversion <- Utility(fun="linear", par=c(lambda=2.25))
example_9_3_3_linear_probability_weighting <- ProbWeight(fun="Tversky_Kahneman_1992", par=c(c=1))
example_9_3_3_linear_probability_weighting <- ProbWeight(fun="Tversky_Kahneman_1992", par=c(c=1))
comparePT(my_choices, example_9_3_3_linear_probability_weighting, example_9_3_3_linear_probability_weighting, example_9_3_3_linear_basic_utility_with_loss_aversion, digits=4)

#  gid od  oc  pr  dw  sv   pt
#    1  1  80 0.5 0.5  80 40.0
#    1  2 -20 0.5 0.5 -45 17.5
#   cid gid ev   pt ce rp
# 1   1   1 30 17.5 17.5 12.5

#RDU (U, w)
my_input_file <- system.file("external", "example_9.3.3.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
example_9_3_3_cara_utility_with_no_loss_aversion <- Utility(fun="exponential", par=c(alpha=0.005, lambda=1))
example_9_3_3_probability_weighting <- ProbWeight(fun="Tversky_Kahneman_1992", par=c(c=0.61))
compareRDU(my_choices, example_9_3_3_probability_weighting, example_9_3_3_cara_utility_with_no_loss_aversion, digits=4)

#  gid oid  oc  pr rnk      w     dw      sv     rdu
#    1   1  80 0.5 0.0 0.0000 0.4206  0.3297 0.13868
#    1   2 -20 0.5 0.5 0.4206 0.5794 -0.1052 0.07774
#   cid gid ev     rdu ce rp
# 1   1   1 30 0.07774 16.19 13.81

#RDU (w)
my_input_file <- system.file("external", "example_9.3.3.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
example_9_3_3_linear_basic_utility_with_no_loss_aversion <- Utility(fun="linear", par=c(lambda=1))
example_9_3_3_probability_weighting <- ProbWeight(fun="Tversky_Kahneman_1992", par=c(c=0.61))
compareRDU(my_choices, example_9_3_3_probability_weighting, example_9_3_3_linear_basic_utility_with_no_loss_aversion, digits=4)

#  gid oid  oc  pr rnk      w     dw  sv   rdu
#    1   1  80 0.5 0.0 0.0000 0.4206  80 33.65
#    1   2 -20 0.5 0.5 0.4206 0.5794 -20 22.06
#   cid gid ev   rdu ce rp
# 1   1   1 30 22.06 22.06 7.936

# EU(U)
my_input_file <- system.file("external", "example_9.3.3.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
example_9_3_3_cara_utility_with_no_loss_aversion <- Utility(fun="exponential", par=c(alpha=0.005, lambda=1))
compareEU(my_choices, example_9_3_3_cara_utility_with_no_loss_aversion, digits=4)

#   cid gid ev     eu  ce  rp
# 1   1   1 30 0.1123 23.81 6.186

# EV
my_input_file <- system.file("external", "example_9.3.3.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
compareEV(my_choices, digits=4)

#   cid gid ev
# 1   1   1 30


########################
# solution to Exercise 9.3.4.a
########################

my_input_file <- system.file("external", "exercise_9.3.4.a.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
pow_prob_weight <- ProbWeight(fun="power", par=c(alpha=2, beta=1))
linear_probability_weighting <- ProbWeight(fun="linear", par=c(lambda=1))
exercise_9_3_4_utility <- Utility(fun="power", par=c(alpha=0.5, beta=0.5, lambda=2))
comparePT(my_choices, pow_prob_weight, linear_probability_weighting, exercise_9_3_4_utility, digits=4)

#  gid od oc  pr   dw sv    pt
#    1  1  9 0.1 0.01  3  0.03
#    1  2  4 0.4 0.24  2  0.51
#    1  3 -4 0.4 0.40 -4 -1.09
#    1  4 -9 0.1 0.10 -6 -1.69
#   cid gid ev    pt   ce  rp
# 1   1   1  0 -1.69 -0.714 0.714

########################
# solution to Exercise 9.3.4.b
########################

my_input_file <- system.file("external", "exercise_9.3.4.b.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
pow_prob_weight <- ProbWeight(fun="power", par=c(alpha=2, beta=1))
linear_probability_weighting <- ProbWeight(fun="linear", par=c(lambda=1))
exercise_9_3_4_utility <- Utility(fun="power", par=c(alpha=0.5, beta=0.5, lambda=2))
comparePT(my_choices, pow_prob_weight, linear_probability_weighting, exercise_9_3_4_utility, digits=4)

#  gid od  oc  pr   dw sv    pt
#    1  1  36 0.1 0.01  6  0.06
#    1  2  25 0.2 0.08  5  0.46
#    1  3  -9 0.3 0.30 -6 -1.34
#    1  4 -16 0.4 0.40 -8 -4.54
#   cid gid   ev    pt   ce  rp
# 1   1   1 -0.5 -4.54 -5.153 4.653

########################
# solution to Assignment 9.3.5.a
########################

my_input_file <- system.file("external", "assignment_9.3.5.a.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
tk_1992_positive_probWeight <- ProbWeight(fun="Tversky_Kahneman_1992", par=c(c=0.61))
tk_1992_negative_probWeight <- ProbWeight(fun="Tversky_Kahneman_1992", par=c(c=0.69))
tk_1992_utility_with_loss_aversion <- Utility(fun="power", par=c(alpha=0.88, beta=0.88, lambda=2.25))
comparePT(my_choices, tk_1992_positive_probWeight, tk_1992_negative_probWeight, tk_1992_utility_with_loss_aversion, digits=4)

#  gid od oc   pr     dw     sv      pt
#    1  1 12 0.15 0.2269  8.906  2.0209
#    1  2  7 0.35 0.1937  5.542  3.0946
#    1  3 -2 0.30 0.1970 -4.141  2.2790
#    1  4 -5 0.20 0.2570 -9.274 -0.1047
#   cid gid   ev      pt     ce  rp
# 1   1   1 2.65 -0.1047 -0.03064 2.681

########################
# solution to Assignment 9.3.5.b
########################

my_input_file <- system.file("external", "assignment_9.3.5.b.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
tk_1992_positive_probWeight <- ProbWeight(fun="Tversky_Kahneman_1992", par=c(c=0.61))
tk_1992_negative_probWeight <- ProbWeight(fun="Tversky_Kahneman_1992", par=c(c=0.69))
tk_1992_utility_with_loss_aversion <- Utility(fun="power", par=c(alpha=0.88, beta=0.88, lambda=2.25))
comparePT(my_choices, tk_1992_positive_probWeight, tk_1992_negative_probWeight, tk_1992_utility_with_loss_aversion, digits=4)

#  gid od  oc  pr      dw     sv     pt
#    1  1  36 0.7 0.53382  23.42 12.501
#    1  2  25 0.1 0.07362  16.99 13.752
#    1  3  -9 0.1 0.08688 -15.56 12.400
#    1  4 -16 0.1 0.17015 -25.81  8.008
#   cid gid   ev    pt  ce  rp
# 1   1   1 25.2 8.008 10.64 14.56

########################
# solution to Exercise 9.3.7.a
########################

my_input_file <- system.file("external", "exercise_9.3.7.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
tk_1992_positive_probWeight <- ProbWeight(fun="Tversky_Kahneman_1992", par=c(c=0.61))
tk_1992_negative_probWeight <- ProbWeight(fun="Tversky_Kahneman_1992", par=c(c=0.69))
tk_1992_utility_with_loss_aversion <- Utility(fun="power", par=c(alpha=0.88, beta=0.88, lambda=2.25))
comparePT(my_choices, tk_1992_positive_probWeight, tk_1992_negative_probWeight, tk_1992_utility_with_loss_aversion, digits=4)

#  gid od  oc   pr      dw     sv     pt
#    1  1  80 0.01 0.05527  47.28  2.613
#    1  2  60 0.35 0.29444  36.71 13.422
#    1  3  30 0.35 0.19070  19.95 17.226
#    1  4 -30 0.29 0.32091 -44.88  2.823
#   cid gid   ev    pt  ce  rp
# 1   1   1 23.6 2.823 3.253 20.35

########################
# solution to Exercise 9.3.7.b
########################

my_input_file <- system.file("external", "exercise_9.3.7.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
linear_probability_weighting <- ProbWeight(fun="Tversky_Kahneman_1992", par=c(c=1))
tk_1992_utility_with_no_loss_aversion <- Utility(fun="power", par=c(alpha=0.88, beta=0.88, lambda=1))
comparePT(my_choices, linear_probability_weighting, linear_probability_weighting, tk_1992_utility_with_no_loss_aversion, digits=4)

#  gid od  oc   pr   dw     sv      pt
#    1  1  80 0.01 0.01  47.28  0.4728
#    1  2  60 0.35 0.35  36.71 13.3210
#    1  3  30 0.35 0.35  19.95 20.3023
#    1  4 -30 0.29 0.29 -19.95 14.5178
#   cid gid   ev    pt  ce  rp
# 1   1   1 23.6 14.52 20.91 2.691

########################
# solution to Exercise 9.3.7.c
########################

my_input_file <- system.file("external", "exercise_9.3.7.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
tk_1992_probability_weighting <- ProbWeight(fun="Tversky_Kahneman_1992", par=c(c=0.61))
linear_utility_with_no_loss_aversion <- Utility(fun="power", par=c(alpha=1, beta=1, lambda=1))
compareRDU(my_choices, tk_1992_probability_weighting, linear_utility_with_no_loss_aversion, digits=4)

#  gid oid  oc   pr  rnk       w      dw  sv    rdu
#    1   1  80 0.01 0.00 0.00000 0.05527  80  4.421
#    1   2  60 0.35 0.01 0.05527 0.29444  60 22.088
#    1   3  30 0.35 0.36 0.34971 0.19070  30 27.809
#    1   4 -30 0.29 0.71 0.54041 0.45959 -30 14.021
#   cid gid   ev   rdu ce rp
# 1   1   1 23.6 14.02 14.02 9.579

########################
# solution to Exercise 9.3.7.d
########################

my_input_file <- system.file("external", "exercise_9.3.7.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
tk_1992_probability_weighting <- ProbWeight(fun="Tversky_Kahneman_1992", par=c(c=0.61))
linear_utility_with_no_loss_aversion <- Utility(fun="power", par=c(alpha=1, beta=1, lambda=1))
comparePT(my_choices, tk_1992_probability_weighting, tk_1992_probability_weighting, linear_utility_with_no_loss_aversion, digits=4)

#  gid od  oc   pr      dw  sv     pt
#    1  1  80 0.01 0.05527  80  4.421
#    1  2  60 0.35 0.29444  60 22.088
#    1  3  30 0.35 0.19070  30 27.809
#    1  4 -30 0.29 0.31298 -30 18.419
#   cid gid   ev    pt  ce  rp
# 1   1   1 23.6 18.42 18.42 5.181

########################
# solution to Exercise 9.3.7.e
########################

my_input_file <- system.file("external", "exercise_9.3.7.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
linear_probability_weighting <- ProbWeight(fun="Tversky_Kahneman_1992", par=c(c=1))
linear_utility_with_loss_aversion <- Utility(fun="power", par=c(alpha=1, beta=1, lambda=2.25))
comparePT(my_choices, linear_probability_weighting, linear_probability_weighting, linear_utility_with_loss_aversion, digits=4)

#  gid od  oc   pr   dw    sv    pt
#    1  1  80 0.01 0.01  80.0  0.80
#    1  2  60 0.35 0.35  60.0 21.80
#    1  3  30 0.35 0.35  30.0 32.30
#    1  4 -30 0.29 0.29 -67.5 12.72
#   cid gid   ev    pt  ce  rp
# 1   1   1 23.6 12.72 12.72 10.88

########################
# solution to Assignment 9.3.9.a
########################

# PT(U, w, lambda)
my_input_file <- system.file("external", "assignment_9.3.9.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
tk_1992_positive_probWeight <- ProbWeight(fun="Tversky_Kahneman_1992", par=c(c=0.69))
tk_1992_negative_probWeight <- ProbWeight(fun="Tversky_Kahneman_1992", par=c(c=0.69))
power_utility <- Utility(fun="power", par=c(alpha=0.88, beta=0.88, lambda=2))
comparePT(my_choices, tk_1992_positive_probWeight, tk_1992_negative_probWeight, power_utility, digits=4)

#  gid od  oc   pr     dw     sv    pt
#    1  1  60 0.33 0.3472  36.71 12.75
#    1  2  30 0.33 0.2117  19.95 16.97
#    1  3 -30 0.34 0.3537 -39.89  2.86
#   cid gid   ev   pt  ce rp
# 1   1   1 19.5 2.86 3.301 16.2

########################
# solution to Assignment 9.3.9.b
########################

# EU(U)
my_input_file <- system.file("external", "assignment_9.3.9.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
power_utility <- Utility(fun="power", par=c(alpha=0.88, beta=0.88, lambda=1))
compareEU(my_choices, power_utility, digits=4)

#   cid gid   ev    eu ce  rp
# 1   1   1 19.5 11.91 16.7 2.796

# replicating EU(U) result using more general PT framework
my_input_file <- system.file("external", "assignment_9.3.9.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
tk_1992_positive_probWeight <- ProbWeight(fun="Tversky_Kahneman_1992", par=c(c=1.0))
tk_1992_negative_probWeight <- ProbWeight(fun="Tversky_Kahneman_1992", par=c(c=1.0))
power_utility <- Utility(fun="power", par=c(alpha=0.88, beta=0.88, lambda=1))
comparePT(my_choices, tk_1992_positive_probWeight, tk_1992_negative_probWeight, power_utility, digits=4)

#  gid od  oc   pr   dw     sv    pt
#    1  1  60 0.33 0.33  36.71 12.11
#    1  2  30 0.33 0.33  19.95 18.70
#    1  3 -30 0.34 0.34 -19.95 11.91
#   cid gid   ev    pt ce  rp
# 1   1   1 19.5 11.91 16.7 2.796

########################
# solution to Assignment 9.3.9.c
########################

my_input_file <- system.file("external", "assignment_9.3.9.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
tk_1992_probability_weighting <- ProbWeight(fun="Tversky_Kahneman_1992", par=c(c=0.69))
linear_utility_with_no_loss_aversion <- Utility(fun="power", par=c(alpha=1, beta=1, lambda=1))
compareRDU(my_choices, tk_1992_probability_weighting, linear_utility_with_no_loss_aversion, digits=4)

#  gid oid  oc   pr  rnk      w     dw  sv   rdu
#    1   1  60 0.33 0.00 0.0000 0.3472  60 20.83
#    1   2  30 0.33 0.33 0.3472 0.2117  30 27.18
#    1   3 -30 0.34 0.66 0.5589 0.4411 -30 13.95
#   cid gid   ev   rdu ce rp
# 1   1   1 19.5 13.95 13.95 5.547

########################
# solution to Assignment 9.3.9.d
########################

my_input_file <- system.file("external", "assignment_9.3.9.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
tk_1992_positive_probWeight <- ProbWeight(fun="Tversky_Kahneman_1992", par=c(c=0.69))
tk_1992_negative_probWeight <- ProbWeight(fun="Tversky_Kahneman_1992", par=c(c=0.69))
linear_utility_with_no_loss_aversion <- Utility(fun="power", par=c(alpha=1, beta=1, lambda=1))
comparePT(my_choices, tk_1992_positive_probWeight, tk_1992_negative_probWeight, linear_utility_with_no_loss_aversion, digits=4)

#  gid od  oc   pr     dw  sv    pt
#    1  1  60 0.33 0.3472  60 20.83
#    1  2  30 0.33 0.2117  30 27.18
#    1  3 -30 0.34 0.3537 -30 16.57
#   cid gid   ev    pt  ce  rp
# 1   1   1 19.5 16.57 16.57 2.925

########################
# solution to Assignment 9.3.9.e
########################

my_input_file <- system.file("external", "assignment_9.3.9.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
tk_1992_positive_probWeight <- ProbWeight(fun="Tversky_Kahneman_1992", par=c(c=1.0))
tk_1992_negative_probWeight <- ProbWeight(fun="Tversky_Kahneman_1992", par=c(c=1.0))
linear_utility_with_loss_aversion <- Utility(fun="power", par=c(alpha=1, beta=1, lambda=2))
comparePT(my_choices, tk_1992_positive_probWeight, tk_1992_negative_probWeight, linear_utility_with_loss_aversion, digits=4)

#  gid od  oc   pr   dw  sv   pt
#    1  1  60 0.33 0.33  60 19.8
#    1  2  30 0.33 0.33  30 29.7
#    1  3 -30 0.34 0.34 -60  9.3
#   cid gid   ev  pt ce rp
# 1   1   1 19.5 9.3  9.3 10.2

########################
# solution to Assignment 9.3.11.a
########################

my_input_file <- system.file("external", "assignment_9.3.11.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
compareEV(my_choices, digits=4)

#   cid gid ev
# 1   1   1 30

########################
# solution to Assignment 9.3.11.b
########################

my_input_file <- system.file("external", "assignment_9.3.11.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
power_utility <- Utility(fun="power", par=c(alpha=0.5, beta=0.5, lambda=1))
compareEU(my_choices, power_utility, digits=4)

#   cid gid ev    eu ce rp
# 1   1   1 30 2.236    5   25

########################
# solution to Assignment 9.3.11.c
########################

my_input_file <- system.file("external", "assignment_9.3.11.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
pow_prob_weight <- ProbWeight(fun="power", par=c(alpha=2, beta=1))
linear_utility_with_no_loss_aversion <- Utility(fun="power", par=c(alpha=1, beta=1, lambda=1))
compareRDU(my_choices, pow_prob_weight, linear_utility_with_no_loss_aversion, digits=4)

#  gid oid  oc  pr rnk    w   dw  sv rdu
#    1   1  80 0.5 0.0 0.00 0.25  80  20
#    1   2 -20 0.5 0.5 0.25 0.75 -20   5
#   cid gid ev rdu ce rp
# 1   1   1 30   5     5    25

########################
# solution to Assignment 9.3.11.d
########################

my_input_file <- system.file("external", "assignment_9.3.11.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
tk_1992_utility_with_no_loss_aversion <- Utility(fun="power", par=c(alpha=0.5, beta=0.5, lambda=1))
pow_prob_weight <- ProbWeight(fun="power", par=c(alpha=2, beta=1))
compareRDU(my_choices, pow_prob_weight, tk_1992_utility_with_no_loss_aversion, digits=4)

#  gid oid  oc  pr rnk    w   dw     sv    rdu
#    1   1  80 0.5 0.0 0.00 0.25  8.944  2.236
#    1   2 -20 0.5 0.5 0.25 0.75 -4.472 -1.118
#   cid gid ev    rdu ce rp
# 1   1   1 30 -1.118 -1.25 31.25

########################
# solution to Assignment 9.3.11.e
########################

my_input_file <- system.file("external", "assignment_9.3.11.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
linear_probability_weighting <- ProbWeight(fun="Tversky_Kahneman_1992", par=c(c=1.0))
linear_utility_with_loss_aversion <- Utility(fun="power", par=c(alpha=1, beta=1, lambda=2.25))
comparePT(my_choices, linear_probability_weighting, linear_probability_weighting, linear_utility_with_loss_aversion, digits=4)

#  gid od  oc  pr  dw  sv   pt
#    1  1  80 0.5 0.5  80 40.0
#    1  2 -20 0.5 0.5 -45 17.5
#   cid gid ev   pt ce rp
# 1   1   1 30 17.5 17.5 12.5

########################
# solution to Assignment 9.3.11.f
########################

my_input_file <- system.file("external", "assignment_9.3.11.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
linear_probability_weighting <- ProbWeight(fun="Tversky_Kahneman_1992", par=c(c=1.0))
tk_1992_utility_with_loss_aversion <- Utility(fun="power", par=c(alpha=0.5, beta=0.5, lambda=2.25))
comparePT(my_choices, linear_probability_weighting, linear_probability_weighting, tk_1992_utility_with_loss_aversion, digits=4)

#  gid od  oc  pr  dw      sv     pt
#    1  1  80 0.5 0.5   8.944  4.472
#    1  2 -20 0.5 0.5 -10.062 -0.559
#   cid gid ev     pt     ce  rp
# 1   1   1 30 -0.559 -0.06173 30.06

########################
# solution to Assignment 9.3.11.g
########################

my_input_file <- system.file("external", "assignment_9.3.11.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
pow_prob_weight <- ProbWeight(fun="power", par=c(alpha=2, beta=1))
linear_utility_with_loss_aversion <- Utility(fun="power", par=c(alpha=1, beta=1, lambda=2.25))
comparePT(my_choices, pow_prob_weight, pow_prob_weight, linear_utility_with_loss_aversion, digits=4)

#  gid od  oc  pr   dw  sv    pt
#    1  1  80 0.5 0.25  80 20.00
#    1  2 -20 0.5 0.25 -45  8.75
#   cid gid ev   pt ce  rp
# 1   1   1 30 8.75 8.75 21.25

########################
# solution to Assignment 9.3.11.h
########################

my_input_file <- system.file("external", "assignment_9.3.11.txt", package="pt")
my_choices <- choicesFromFile(input_file=my_input_file, choice_id_header="choice_id", gamble_id_header="gamble_id", outcome_id_header="outcome_id", objective_consequence_header="objective_consequence", probability_header="probability", DELIMITER="\t")
my_choices
pow_prob_weight <- ProbWeight(fun="power", par=c(alpha=2, beta=1))
tk_1992_utility_with_loss_aversion <- Utility(fun="power", par=c(alpha=0.5, beta=0.5, lambda=2.25))
comparePT(my_choices, pow_prob_weight, pow_prob_weight, tk_1992_utility_with_loss_aversion, digits=4)

#  gid od  oc  pr   dw      sv      pt
#    1  1  80 0.5 0.25   8.944  2.2361
#    1  2 -20 0.5 0.25 -10.062 -0.2795
#   cid gid ev      pt     ce  rp
# 1   1   1 30 -0.2795 -0.01543 30.02
