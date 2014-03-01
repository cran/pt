
## ----fig.show='asis', fig.align='center', tidy=FALSE, out.width='0.8\\linewidth'----
library("pt")


## ----fig.show='asis', fig.align='center', tidy=FALSE, out.width='0.8\\linewidth'----
choice_ids <- c(1, 1, 1, 1, 2, 2, 2, 2)
gamble_ids <- c(1, 1, 1, 2, 1, 1, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 1, 2, 1, 2)
objective_consequences <- c(2500, 2400, 0, 2400, 
	2500, 0, 2400, 0)
probability_strings <- c("0.33", "0.66", "0.01", "1.0", 
	"0.33", "0.67", "0.34", "0.66")
my_choices <- Choices(choice_ids=choice_ids,
	gamble_ids=gamble_ids, 
	outcome_ids=outcome_ids, 
	objective_consequences=objective_consequences, 
	probability_strings=probability_strings)
my_choices

drawChoices(my_choices, 
	decision_square_x=0.2, decision_square_edge_length=0.05,
	circle_radius=0.025, y_split_gap=0.1, x_split_offset=0.03, 
	probability_text_digits=4, y_probability_text_offset=0.015, 
	y_value_text_offset=0.005, x_value_text_offset=0.025, 
	probability_text_font_colour="red", probability_text_font_size=11, 
	objective_consequence_text_font_colour="blue", 
	objective_consequence_text_font_size=11, label=c("A","B","C", "D"), 
	label_font_colour=c("orange","magenta","green","blue"), 
	label_font_size=c(11,11,11,11),
	label_positions=list(c(0.26,0.85),c(0.26,0.55),
		c(0.26,0.4),c(0.26,0.1)))


## ----fig.show='asis', fig.align='center', tidy=FALSE, out.width='0.8\\linewidth'----
compareEV(my_choices, digits=4)


## ----fig.show='asis', fig.align='center', tidy=FALSE, out.width='0.8\\linewidth'----
my_utility <- Utility(fun="power", par=c(alpha=0.63, beta=0.63, lambda=2.25))
compareEU(my_choices, utility=my_utility, digits=4)


## ----fig.show='asis', fig.align='center', tidy=FALSE, out.width='0.8\\linewidth'----
my_utility <- Utility(fun="power", par=c(alpha=1.2, beta=1.2, lambda=2.25))
compareEU(my_choices, utility=my_utility, digits=4)


## ----fig.show='asis', fig.align='center', tidy=FALSE, out.width='0.8\\linewidth'----
my_utility <- Utility(fun="power", par=c(alpha=1.0, beta=1.0, lambda=1.0))
compareEU(my_choices, utility=my_utility, digits=4)


## ----fig.show='asis', fig.align='center', tidy=FALSE, out.width='0.8\\linewidth'----
tk_1992_utility <- Utility(fun="power", par=c(alpha=0.88, beta=0.88, lambda=2.25))
linear_in_log_odds_prob_weight <- ProbWeight(fun="linear_in_log_odds", par=c(alpha=0.61, beta=0.724))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=linear_in_log_odds_prob_weight,
	prob_weight_for_negative_outcomes=linear_in_log_odds_prob_weight,
	utility=tk_1992_utility, digits=4)


## ----fig.show='asis', fig.align='center', tidy=FALSE, out.width='0.8\\linewidth'----
my_utility <- Utility(fun="power", par=c(alpha=0.88, beta=0.88, lambda=2.25))

drawSimplex(x1=0, x2=2400, x3=2500,
	line_dot_density=3000,
	draw_ev_flag=TRUE, ev_colour="black",
	draw_pt_flag=TRUE, alpha=0.61, beta=0.724, pt_colour="red", 
	draw_utility_flag=TRUE, utility=my_utility, eu_colour="purple",
	start_points=list(c(0.1,0.9),c(0.2,0.8),c(0.3,0.7),c(0.4,0.6),
		c(0.5,0.5),c(0.6,0.4),c(0.7,0.3),c(0.8,0.2),c(0.9,0.1)),
	labels=c("A","B","C","D","increasing preference"),
	label_positions=list(c(0.05,0.38),c(0.05,0.05),c(0.7,0.38),
		c(0.7,0.05),c(0.7,0.7)),	
	label_colours=c("orange","magenta","green","blue","red"),
	label_font_sizes=c(12,12,12,12,16),
	label_font_faces=c("plain","plain","plain","plain","bold"),	
	label_rotations=c(0,0,0,0,-45),
	circle_radii=c(0.01,0.01,0.01,0.01), 	
	circle_outline_colours=c("black","black","black","black"),
	circle_fill_colours=c("orange","purple","orange","purple"),	
	circle_positions=list(c(0.01,0.33),c(0,0),
		c(0.67,0.33),c(0.66,0)), 
	lines=list(c(0.01,0.33,0,0),c(0,0,0.66,0),
		c(0.66,0,0.67,0.33),c(0.67,0.33,0.01,0.33)),
	line_widths=c(1, 1, 1, 1),
	line_styles=c("dashed", "dashed", "dashed", "dashed"),	
	line_colours=c("blue","blue","blue","blue"),
	arrows=list(c(0.8,0.5,0.5,0.8)),
	arrow_widths=c(2), arrow_styles=c("solid"), arrow_colours=c("red"))


## ----fig.show='asis', fig.align='center', tidy=FALSE, out.width='0.8\\linewidth'----
my_utility <- Utility(fun="linear", par=c(lambda=1))
power_prob_weight <- ProbWeight(fun="power", par=c(alpha=0.7, beta=1))
compareTAX(my_choices, prob_weight=power_prob_weight, utility=my_utility, delta=-1, digits=4)


## ----fig.show='asis', fig.align='center', tidy=FALSE, out.width='0.8\\linewidth'----
choice_ids <- c(1, 1, 1, 1, 2, 2, 2, 2)
gamble_ids <- c(1, 2, 2, 2, 1, 1, 2, 2)
outcome_ids <- c(1, 1, 2, 3, 1, 2, 1, 2)
objective_consequences <- c(1000000, 2000000, 1000000, 2, 
	1000000, 2, 2000000, 2)
probability_strings <- c("1.0", "0.1", "0.89", "0.01", 
	"0.11", "0.89", "0.1", "0.9")
my_choices <- Choices(choice_ids=choice_ids,
	gamble_ids=gamble_ids, 
	outcome_ids=outcome_ids, 
	objective_consequences=objective_consequences, 
	probability_strings=probability_strings)
my_choices

drawChoices(my_choices, 
	decision_square_x=0.2, decision_square_edge_length=0.05,
	circle_radius=0.025, y_split_gap=0.1, x_split_offset=0.03, 
	probability_text_digits=4, y_probability_text_offset=0.015, 
	y_value_text_offset=0.005, x_value_text_offset=0.025, 
	probability_text_font_colour="red", probability_text_font_size=11, 
	objective_consequence_text_font_colour="blue", 
	objective_consequence_text_font_size=11, label=c("A","B","C", "D"), 
	label_font_colour=c("orange","magenta","green","blue"), 
	label_font_size=c(11,11,11,11),
	label_positions=list(c(0.26,0.95),c(0.26,0.65),
		c(0.26,0.4),c(0.26,0.1)))


## ----fig.show='asis', fig.align='center', tidy=FALSE, out.width='0.8\\linewidth'----
my_utility <- Utility(fun="power", par=c(alpha=0.88, beta=0.88, lambda=2.25))

drawSimplex(x1=2, x2=1000000, x3=2000000, 
	line_dot_density=1000,
	draw_ev_flag=TRUE, ev_colour="black",
	draw_pt_flag=TRUE, alpha=0.61, beta=0.724, pt_colour="red", 
	draw_utility_flag=TRUE, utility=my_utility, eu_colour="purple",
	start_points=list(c(0.1,0.9),c(0.2,0.8),c(0.3,0.7),c(0.4,0.6),
		c(0.5,0.5),c(0.6,0.4),c(0.7,0.3),c(0.8,0.2),c(0.9,0.1)),
	labels=c("A","B","C","D","increasing preference"),
	label_positions=list(c(0.05,0.02),c(0.05,0.12),c(0.92,0.02),
		c(0.95,0.10),c(0.7,0.7)),	
	label_colours=c("orange","purple","orange","purple","red"),
	label_font_sizes=c(12,12,12,12,16),
	label_font_faces=c("plain","plain","plain","plain","bold"),	
	label_rotations=c(0,0,0,0,-45),
	circle_radii=c(0.01,0.01,0.01,0.01), 	
	circle_outline_colours=c("black","black","black","black"),
	circle_fill_colours=c("orange","purple","orange","purple"),	
	circle_positions=list(c(0,0),c(0.01,0.1),c(0.89,0),c(0.9,0.1)), 
	lines=list(c(0,0,0.01,0.1),c(0.89,0,0.9,0.1),
		c(0.01,0.1,0.9,0.1),c(0,0,0.89,0)),
	line_widths=c(1, 1, 1, 1),
	line_styles=c("dashed", "dashed", "dashed", "dashed"),	
	line_colours=c("blue","blue","blue","blue"),
	arrows=list(c(0.8,0.5,0.5,0.8)),
	arrow_widths=c(2), arrow_styles=c("solid"), arrow_colours=c("red"))


## ----fig.show='asis', fig.align='center', tidy=FALSE, out.width='0.8\\linewidth'----
choice_ids <- c(1, 1, 1, 2, 2, 2, 2)
gamble_ids <- c(1, 1, 2, 1, 1, 2, 2)
outcome_ids <- c(1, 1, 2, 1, 2, 1, 2)
objective_consequences <- c(4000, 0, 3000,  
	4000, 0, 3000, 0)
probability_strings <- c("0.8", "0.2", "1.0",
	"0.2", "0.8", "0.25", "0.75")
my_choices <- Choices(choice_ids=choice_ids,
	gamble_ids=gamble_ids, 
	outcome_ids=outcome_ids, 
	objective_consequences=objective_consequences, 
	probability_strings=probability_strings)
my_choices

drawChoices(my_choices,
	decision_square_x=0.2, decision_square_edge_length=0.05,
	circle_radius=0.025, y_split_gap=0.1, x_split_offset=0.03, 
	probability_text_digits=4, y_probability_text_offset=0.015, 
	y_value_text_offset=0.005, x_value_text_offset=0.025, 
	probability_text_font_colour="red", probability_text_font_size=11, 
	objective_consequence_text_font_colour="blue", 
	objective_consequence_text_font_size=11, label=c("A","B","C", "D"), 
	label_font_colour=c("orange","magenta","green","blue"), 
	label_font_size=c(11,11,11,11),
	label_positions=list(c(0.26,0.85),c(0.26,0.6),
		c(0.26,0.4),c(0.26,0.1)))


## ----fig.show='asis', fig.align='center', tidy=FALSE, out.width='0.8\\linewidth'----
compareEV(my_choices, digits=4)


## ----fig.show='asis', fig.align='center', tidy=FALSE, out.width='0.8\\linewidth'----
my_utility <- Utility(fun="power", par=c(alpha=0.88, beta=0.88, lambda=1))
eu_df <- compareEU(my_choices, utility=my_utility, digits=4)
eu_df

ev <- as.numeric(eu_df$ev[1])
eu <- as.numeric(eu_df$eu[1])
ce <- as.numeric(eu_df$ce[1])
plotRP(my_title = "risk premium",
	my_title_colour="black", my_title_font_size=4,
	my_x_label = "objective consequence", 
	my_y_label = "subjective value", xmin = 2500, xmax = 3500, 
	my_color="violet", 
	fun=power_uf,
	par=c(alpha=0.88, beta=0.88, lambda=1),
	ev=ev, eu=eu, ce=ce, 
	my_labels=c(expression(paste(U(x)==x^alpha, ",     ", x>=0)), 
		expression(paste(plain()==-lambda * (-x)^beta, ", ", x<0)), 
		"ev","eu","ce","rp"), 
	my_label_colors=c("violet","violet","black","red","orange","blue"), 
	my_label_positions=list(c(2700,1275),c(2740,1250),c(3250,1075),
		c(2800,1170),c(3050,1075),c(3150,1170)), 
	font_scaling=1)


## ----fig.show='asis', fig.align='center', tidy=FALSE, out.width='0.8\\linewidth'----
my_utility <- Utility(fun="power", par=c(alpha=0.78, beta=0.78, lambda=1))
compareEU(my_choices, utility=my_utility, digits=4)


## ----fig.show='asis', fig.align='center', tidy=FALSE, out.width='0.8\\linewidth'----
my_utility <- Utility(fun="power", par=c(alpha=0.77, beta=0.78, lambda=1))
compareEU(my_choices, utility=my_utility, digits=4)


## ----fig.show='asis', fig.align='center', tidy=FALSE, out.width='0.8\\linewidth'----
my_utility <- Utility(fun="power", par=c(alpha=1, beta=1, lambda=1))
compareEU(my_choices, utility=my_utility, digits=4)


## ----fig.show='asis', fig.align='center', tidy=FALSE, out.width='0.8\\linewidth'----
tk_1992_utility <- Utility(fun="power", par=c(alpha=0.88, beta=0.88, lambda=2.25))
tk_1992_positive_prob_weight <- ProbWeight(fun="Tversky_Kahneman_1992", par=c(alpha=0.61))
compareRDU(my_choices, prob_weight=tk_1992_positive_prob_weight, utility=tk_1992_utility, digits=4)


## ----fig.show='asis', fig.align='center', tidy=FALSE, out.width='0.8\\linewidth'----
tk_1992_utility <- Utility(fun="power", par=c(alpha=0.88, beta=0.88, lambda=2.25))
tk_1992_positive_prob_weight <- ProbWeight(fun="Tversky_Kahneman_1992", par=c(alpha=0.61))
tk_1992_negative_prob_weight <- ProbWeight(fun="Tversky_Kahneman_1992", par=c(alpha=0.69))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=tk_1992_positive_prob_weight,
	prob_weight_for_negative_outcomes=tk_1992_negative_prob_weight,
	utility=tk_1992_utility, digits=4)


## ----fig.show='asis', fig.align='center', tidy=FALSE---------------------
plotProbW(my_title=expression(paste("Tversky & Kahneman (1992), ", 
	alpha==0.61)), 
	my_title_colour="black", my_title_font_size=4,
	my_x_label = "p", my_y_label = "w(p)", 
	pwf=kt_pwf, par=c(alpha=0.61),
	draw_reference_line_flag=TRUE, reference_line_colour="red", 
	reference_line_style="dotted", 
	my_labels=c(expression(paste(w(italic(p)) == frac(italic(p)^alpha, 
		(italic(p)^alpha + (1-italic(p))^alpha)^(1/alpha))))), 
	my_label_positions=list(c(0.4,0.8)), 
	font_scaling=1.0)


## ----fig.show='asis', fig.align='center', tidy=FALSE---------------------
plotOneParProbWFam(my_title="Tversky & Kahneman (1992) family",
	my_title_colour="black", my_title_font_size=4,
	my_x_label = "p", my_y_label = "w(p)", pwf=kt_pwf,
	par=c(0.3, 0.61, 0.8, 1.0, 1.3),
	draw_reference_line_flag=TRUE, reference_line_colour="red",
	reference_line_style="dotted",
	my_labels=c(expression(paste(alpha == 0.3)),
	expression(paste(alpha == 0.61)),
	expression(paste(alpha == 0.8)),
	expression(paste(alpha == 1.0)),
	expression(paste(alpha == 1.3)),
	expression(paste(w(italic(p)) == frac(italic(p)^alpha,
	(italic(p)^alpha + (1-italic(p))^alpha)^(1/alpha))))),
	my_label_positions=list(c(0.9,0.15),c(0.7,0.45),c(0.15,0.5),
	c(0.31,0.62),c(0.5,0.7),c(0.42, 0.9)),
	font_scaling=1.0,
	arrow_positions = list(c(0.3,0.5,0.39,0.41),c(0.42,0.58,0.52,0.51),
		c(0.59,0.66,0.66,0.66)))


## ----fig.show='asis', fig.align='center', tidy=FALSE---------------------
plotTwoParProbWFam(my_title=expression(paste("linear in log odds,  ", 
	gamma == 0.6)),
	my_title_colour="black", my_title_font_size=4,
	my_x_label = "p", my_y_label = "w(p)", pwf=linear_in_log_odds_pwf, 
	par=list(a_list=c(0.6), b_list=seq(from=0.2, to=1.8, by=0.06)),  
	draw_reference_line_flag=TRUE, reference_line_colour="red", 
	reference_line_style="dotted", 
	my_labels=c(expression(paste(delta == 0.2)), 
		expression(paste(delta == 1.8)), 
		expression(paste(w(italic(p)) == frac(delta * italic(p)^gamma, 
			delta * italic(p)^gamma + (1-italic(p))^gamma)))), 
	my_label_positions=list(c(0.7,0.09),c(0.2,0.6),c(0.42, 0.9)), 
	font_scaling=1.0, 
	arrow_positions = list(c(0.28,0.56,0.35,0.53),c(0.7,0.23,0.75,0.35)))


## ----fig.show='asis', fig.align='center', tidy=FALSE---------------------
plotTwoParProbWFam(my_title=expression(paste("linear in log odds,  ", 
	delta == 0.6)),
	my_title_colour="black", my_title_font_size=4,
	my_x_label = "p", my_y_label = "w(p)", pwf=linear_in_log_odds_pwf, 
	par=list(a_list=seq(from=0.2, to=1.8, by=0.06), b_list=c(0.6)), 
	draw_reference_line_flag=TRUE, reference_line_colour="red", 
	reference_line_style="dotted", 
	my_labels=c(expression(paste(gamma == 0.2)),
		expression(paste(gamma == 1.8)), 
		expression(paste(w(italic(p)) == frac(delta * italic(p)^gamma, 
			delta * italic(p)^gamma + (1-italic(p))^gamma)))), 
	my_label_positions=list(c(0.8,0.25),c(0.5,0.1),c(0.42, 0.9)), 
	font_scaling=1.0, arrow_positions = list(c(0.5,0.25,0.45,0.3),
		c(0.78,0.36,0.8,0.47)))


## ----fig.show='asis', fig.align='center', tidy=FALSE, out.width='0.8\\linewidth'----
tk_1992_utility <- Utility(fun="power", par=c(alpha=0.88, beta=0.88, lambda=2.25))
linear_in_log_odds_prob_weight <- ProbWeight(fun="linear_in_log_odds", par=c(alpha=0.61, beta=0.724))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=linear_in_log_odds_prob_weight,
	prob_weight_for_negative_outcomes=linear_in_log_odds_prob_weight,
	utility=tk_1992_utility, digits=4)


## ----fig.show='asis', fig.align='center', tidy=FALSE, out.width='0.8\\linewidth'----
my_utility <- Utility(fun="power", par=c(alpha=0.4, beta=0.4, lambda=1))
my_pwf <- ProbWeight(fun="linear_in_log_odds", par=c(alpha=0.4, beta=0.4))
compareSWU(my_choices, prob_weight=my_pwf, utility=my_utility, digits=4)		


## ----fig.show='asis', fig.align='center', tidy=FALSE, out.width='0.8\\linewidth'----
my_utility <- Utility(fun="power", par=c(alpha=0.4, beta=0.4, lambda=1))
my_pwf <- ProbWeight(fun="linear_in_log_odds", par=c(alpha=0.4, beta=0.4))
compareSWAU(my_choices, prob_weight=my_pwf, utility=my_utility, digits=4)		


## ----fig.show='asis', fig.align='center', tidy=FALSE, out.width='0.8\\linewidth'----
branch_weight_list <- list(c(1), c(0.3738, 0.6262))
my_utility <- Utility(fun="linear", par=c(lambda=1))
power_prob_weight <- ProbWeight(fun="power", par=c(alpha=0.7, beta=1))
compareRAM(my_choices, branch_weight_list=branch_weight_list,
	prob_weight=power_prob_weight, utility=my_utility, digits=4)


## ----fig.show='asis', fig.align='center', tidy=FALSE, out.width='0.8\\linewidth'----
my_pwf <- ProbWeight(fun="compound_invariance", par=c(alpha=0.542, beta=1.382))
my_utility <- Utility(fun="power", par=c(alpha=1, beta=1, lambda=1))
compareGDU(my_choices, prob_weight=my_pwf, utility=my_utility, digits=4)


## ----fig.show='asis', fig.align='center', tidy=FALSE, out.width='0.8\\linewidth'----
my_utility <- Utility(fun="power", par=c(alpha=0.631, beta=0.631, lambda=1))
comparePRT(my_choices, utility=my_utility, gamma=0.676, digits=4)


## ----fig.show='asis', fig.align='center', tidy=FALSE, out.width='0.8\\linewidth'----
my_utility <- Utility(fun="linear", par=c(lambda=1))
power_prob_weight <- ProbWeight(fun="power", par=c(alpha=0.7, beta=1))
compareTAX(my_choices, prob_weight=power_prob_weight, utility=my_utility, delta=-1, digits=4)		


## ----fig.show='asis', fig.align='center', tidy=FALSE, out.width='0.8\\linewidth'----
my_utility <- Utility(fun="power", par=c(alpha=0.88, beta=0.88, lambda=1))

drawSimplex(x1=0, x2=3000, x3=4000,
	line_dot_density=1000,
	draw_ev_flag=TRUE, ev_colour="black",
	draw_pt_flag=TRUE, alpha=0.61, beta=0.724, pt_colour="red", 
	draw_utility_flag=TRUE, utility=my_utility, eu_colour="purple",
	start_points=list(c(0.1,0.9),c(0.2,0.8),c(0.3,0.7),c(0.4,0.6),
		c(0.5,0.5),c(0.6,0.4),c(0.7,0.3),c(0.8,0.2),c(0.9,0.1)),
	labels=c("A","B","C","D","increasing preference"),
	label_positions=list(c(0.03,0.04),c(0.21,0.75),
		c(0.79,0.04),c(0.85,0.18),c(0.7,0.7)),	
	label_colours=c("orange","purple","orange","purple","red"),
	label_font_sizes=c(12,12,12,12,16),
	label_font_faces=c("plain","plain","plain","plain","bold"),	
	label_rotations=c(0,0,0,0,-45),
	circle_radii=c(0.01,0.01,0.01,0.01), 	
	circle_outline_colours=c("black","black","black","black"),
	circle_fill_colours=c("orange","purple","orange","purple"),	
	circle_positions=list(c(0,0),c(0.2,0.8),c(0.75,0),c(0.8,0.2)),
	lines=list(c(0,0,0.2,0.8),c(0.2,0.8,0.8,0.2),
		c(0.8,0.2,0.75,0),c(0.75,0,0,0)),
	line_widths=c(1, 1, 1, 1),
	line_styles=c("dashed", "dashed", "dashed", "dashed"),	
	line_colours=c("blue","blue","blue","blue"),
	arrows=list(c(0.8,0.5,0.5,0.8)),
	arrow_widths=c(2), arrow_styles=c("solid"), arrow_colours=c("red"))


## ----fig.show='asis', fig.align='center', tidy=FALSE, out.width='0.8\\linewidth'----
choice_ids <- c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4)
gamble_ids <- c(1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2)
outcome_ids <- c(1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1)
objective_consequences <- c(1000, 0, 500, -0, -1000, -500, 
	5000, 0, 5, -5000, -0, -5)
probability_strings <- c("1/2", "1/2", "1", "1/2", "1/2", 
	"1", "0.001", "0.999", "1", "0.001", "0.999", "1")
my_choices <- Choices(choice_ids=choice_ids,
	gamble_ids=gamble_ids, 
	outcome_ids=outcome_ids, 
	objective_consequences=objective_consequences, 
	probability_strings=probability_strings)
my_choices

drawChoices(my_choices, 
	decision_square_x=0.2, decision_square_edge_length=0.03,
	circle_radius=0.02, y_split_gap=0.07, x_split_offset=0.03, 
	probability_text_digits=4, y_probability_text_offset=0.015, 
	y_value_text_offset=0.005, x_value_text_offset=0.025, 
	probability_text_font_colour="red", probability_text_font_size=10, 
	objective_consequence_text_font_colour="blue", 
	objective_consequence_text_font_size=10, 
	label=c("A","B","C","D","E","F","G","H"), 
	label_font_colour=c("orange","magenta","green","blue",
		"purple","pink","grey","violet"), 
	label_font_size=c(11,11,11,11,11,11,11,11),
	label_positions=list(c(0.26,0.94),c(0.26,0.78),c(0.26,0.69),
		c(0.26,0.53),c(0.26,0.44),c(0.26,0.28),c(0.26,0.2),
		c(0.26,0.03)))


## ----fig.show='asis', fig.align='center', tidy=FALSE, out.width='0.8\\linewidth'----
tk_1992_utility <- Utility(fun="power", par=c(alpha=0.88, beta=0.88, lambda=2.25))
linear_in_log_odds_prob_weight <- ProbWeight(fun="linear_in_log_odds", par=c(alpha=0.61, beta=0.724))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=linear_in_log_odds_prob_weight,
	prob_weight_for_negative_outcomes=linear_in_log_odds_prob_weight,
	utility=tk_1992_utility, digits=4)


## ----fig.show='asis', fig.align='center', tidy=FALSE, out.width='0.8\\linewidth'----
choice_ids <- c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4)
gamble_ids <- c(1, 2, 2, 1, 2, 2, 1, 2, 2, 1, 2, 2)
outcome_ids <- c(1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2)
objective_consequences <- c(1, 0, 100, 10000, 0, 1000000, 
	-1, 0, -100, -10000, 0, -1000000)
probability_strings <- c("1", "0.99", "0.01", "1", "0.99", "0.01", 
	"1", "0.99", "0.01", "1", "0.99", "0.01")
my_choices <- Choices(choice_ids=choice_ids,
	gamble_ids=gamble_ids, 
	outcome_ids=outcome_ids, 
	objective_consequences=objective_consequences, 
	probability_strings=probability_strings)
my_choices

drawChoices(my_choices,
	decision_square_x=0.2, decision_square_edge_length=0.03,
	circle_radius=0.02, y_split_gap=0.07, x_split_offset=0.03, 
	probability_text_digits=4, y_probability_text_offset=0.015, 
	y_value_text_offset=0.005, x_value_text_offset=0.025, 
	probability_text_font_colour="red", probability_text_font_size=10, 
	objective_consequence_text_font_colour="blue", 
	objective_consequence_text_font_size=10, label=c("A","B","C","D","E","F","G","H"), 
	label_font_colour=c("orange","magenta","green","pink","orange","magenta","green","pink"), 
	label_font_size=c(11,11,11,11,11,11,11,11),
	label_positions=list(c(0.26,0.98),c(0.26,0.8),c(0.26,0.73),c(0.26,0.56),
		c(0.26,0.48),c(0.26,0.31),c(0.26,0.23),c(0.26,0.06)))


## ----fig.show='asis', fig.align='center', tidy=FALSE, out.width='0.8\\linewidth'----
norm_log_utility <- Utility(fun="normalized_logarithmic", par=c(alpha=0.032, beta=0.0031, lambda=2.25))
tk_1992_positive_prob_weight <- ProbWeight(fun="Tversky_Kahneman_1992", par=c(alpha=0.4496))
tk_1992_negative_prob_weight <- ProbWeight(fun="Tversky_Kahneman_1992", par=c(alpha=0.6704))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=tk_1992_positive_prob_weight,
	prob_weight_for_negative_outcomes=tk_1992_negative_prob_weight,
	utility=norm_log_utility, digits=4)


## ----fig.show='asis', fig.align='center', tidy=FALSE, out.width='0.8\\linewidth'----
choice_ids <- c(1, 1, 1)
gamble_ids <- c(1, 2, 2)
outcome_ids <- c(1, 1, 2)
objective_consequences <- c(0, -100, 100)
probability_strings <- c("1", "1/2", "1/2")
my_choices <- Choices(choice_ids=choice_ids,
	gamble_ids=gamble_ids, 
	outcome_ids=outcome_ids, 
	objective_consequences=objective_consequences, 
	probability_strings=probability_strings)
my_choices

drawChoices(my_choices,
	decision_square_x=0.2, decision_square_edge_length=0.05,
	circle_radius=0.025, y_split_gap=0.15, x_split_offset=0.03, 
	probability_text_digits=4, y_probability_text_offset=0.015, 
	y_value_text_offset=0.005, x_value_text_offset=0.025, 
	probability_text_font_colour="red", probability_text_font_size=10, 
	objective_consequence_text_font_colour="blue", 
	objective_consequence_text_font_size=10, label=c("A","B"), 
	label_font_colour=c("orange","magenta"), label_font_size=c(11,11),
	label_positions=list(c(0.26,0.7),c(0.26,0.36)))


## ----fig.show='asis', fig.align='center', tidy=FALSE, out.width='0.8\\linewidth'----
tk_1992_utility <- Utility(fun="power", par=c(alpha=0.88, beta=0.88, lambda=2.25))
linear_in_log_odds_prob_weight <- ProbWeight(fun="linear_in_log_odds", par=c(alpha=0.61, beta=0.724))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=linear_in_log_odds_prob_weight,
	prob_weight_for_negative_outcomes=linear_in_log_odds_prob_weight,
	utility=tk_1992_utility, digits=4) 


## ----fig.show='asis', fig.align='center', tidy=FALSE, out.width='0.8\\linewidth'----
my_utility <- Utility(fun="linear", par=c(lambda=1))
power_prob_weight <- ProbWeight(fun="power", par=c(alpha=0.7, beta=1))
compareTAX(my_choices, prob_weight=power_prob_weight, utility=my_utility, delta=-1, digits=4)


## ----fig.show='asis', fig.align='center', tidy=FALSE, out.width='0.8\\linewidth'----
plotUtility(my_x_label = "objective consequence", 
	my_y_label = "subjective value", 
	xmin = -10, xmax = 10, fun=power_uf, 
	par=c(alpha = 0.88, beta = 0.88, lambda = 2.25),
	fun_colour = "purple",
	draw_reference_line_flag = TRUE, 
	reference_line_colour = "red", 
	reference_line_style = 1)


## ----fig.show='asis', fig.align='center', tidy=FALSE, out.width='0.8\\linewidth'----
choice_ids <- c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3)
gamble_ids <- c(1, 1, 1, 2, 2, 2, 1, 1, 1, 2, 2, 2, 1, 1, 1, 2, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3)
objective_consequences <- c(100, 0, 0, 50, 50, 0, 
	-0, -50, -50, -0, -0, -100, 
	100, 0, -50, 50, -0, -100)
probability_strings <- c("0.25", "0.25", "0.5", "0.25", "0.25", "0.5", 
	"0.5", "0.25", "0.25", "0.5", "0.25", "0.25", 
	"0.25", "0.25", "0.5", "0.5", "0.25", "0.25")
my_choices <- Choices(choice_ids=choice_ids,
	gamble_ids=gamble_ids, 
	outcome_ids=outcome_ids, 
	objective_consequences=objective_consequences, 
	probability_strings=probability_strings)
my_choices

drawChoices(my_choices,
	decision_square_x=0.2, decision_square_edge_length=0.03,
	circle_radius=0.02, y_split_gap=0.04, x_split_offset=0.03, 
	probability_text_digits=4, y_probability_text_offset=0.015, 
	y_value_text_offset=0.005, x_value_text_offset=0.025, 
	probability_text_font_colour="red", probability_text_font_size=10, 
	objective_consequence_text_font_colour="blue", 
	objective_consequence_text_font_size=10, 
	label=c("A","B","C", "D","E","F"), 
	label_font_colour=c("orange","magenta","green",
		"blue","purple","pink"), 
	label_font_size=c(11,11,11,11,11,11),
	label_positions=list(c(0.26,0.93),c(0.26,0.74),
		c(0.26,0.6),c(0.26,0.4),c(0.26,0.28),c(0.26,0.05)))


## ----fig.show='asis', fig.align='center', tidy=FALSE, out.width='0.8\\linewidth'----
tk_1992_utility <- Utility(fun="power", par=c(alpha=0.88, beta=0.88, lambda=2.25))
tk_1992_positive_prob_weight <- ProbWeight(fun="Tversky_Kahneman_1992", par=c(alpha=0.61))
tk_1992_negative_prob_weight <- ProbWeight(fun="Tversky_Kahneman_1992", par=c(alpha=0.69))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=tk_1992_positive_prob_weight,
	prob_weight_for_negative_outcomes=tk_1992_negative_prob_weight,
	utility=tk_1992_utility, digits=4)


## ----fig.show='asis', fig.align='center', tidy=FALSE, out.width='0.8\\linewidth'----
my_utility <- Utility(fun="linear", par=c(lambda=1))
power_prob_weight <- ProbWeight(fun="power", par=c(alpha=0.7, beta=1))
compareTAX(my_choices, prob_weight=power_prob_weight, utility=my_utility, delta=-1, digits=4)


## ----fig.show='asis', fig.align='center', tidy=FALSE, out.width='0.8\\linewidth'----
choice_ids <- c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3)
gamble_ids <- c(1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2)
outcome_ids <- c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2) 
objective_consequences <- c(4200, -3000, 3000, -6000,
	4200, 0, 3000, 0, 
	0, -3000, 0, -6000)
probability_strings <- c("0.5", "0.5", "0.75", "0.25",
	"0.5", "0.5", "0.75", "0.25",
	"0.5", "0.5", "0.75", "0.25")
my_choices <- Choices(choice_ids=choice_ids,
	gamble_ids=gamble_ids, 
	outcome_ids=outcome_ids, 
	objective_consequences=objective_consequences, 
	probability_strings=probability_strings)
my_choices

drawChoices(my_choices,
	decision_square_x=0.2, decision_square_edge_length=0.03,
	circle_radius=0.02, y_split_gap=0.06, x_split_offset=0.03, 
	probability_text_digits=4, y_probability_text_offset=0.015, 
	y_value_text_offset=0.005, x_value_text_offset=0.025, 
	probability_text_font_colour="red", probability_text_font_size=10, 
	objective_consequence_text_font_colour="blue", 
	objective_consequence_text_font_size=10, 
	label=c("A","B","C", "D","E","F"), 
	label_font_colour=c("orange","magenta","green",
		"blue","purple","pink"), 
	label_font_size=c(11,11,11,11,11,11),
	label_positions=list(c(0.26,0.93),c(0.26,0.74),
		c(0.26,0.6),c(0.26,0.4),c(0.26,0.28),c(0.26,0.05)))


## ----fig.show='asis', fig.align='center', tidy=FALSE, out.width='0.8\\linewidth'----
tk_1992_utility <- Utility(fun="power", par=c(alpha=0.88, beta=0.88, lambda=2.25))
tk_1992_positive_prob_weight <- ProbWeight(fun="Tversky_Kahneman_1992", par=c(alpha=0.61))
tk_1992_negative_prob_weight <- ProbWeight(fun="Tversky_Kahneman_1992", par=c(alpha=0.69))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=tk_1992_positive_prob_weight,
	prob_weight_for_negative_outcomes=tk_1992_negative_prob_weight,
	utility=tk_1992_utility, digits=4)


## ----fig.show='asis', fig.align='center', tidy=FALSE, out.width='0.8\\linewidth'----
my_utility <- Utility(fun="linear", par=c(lambda=1))
power_prob_weight <- ProbWeight(fun="power", par=c(alpha=0.7, beta=1))
compareTAX(my_choices, prob_weight=power_prob_weight, utility=my_utility, delta=-1, digits=4)


## ----fig.show='asis', fig.align='center', tidy=FALSE, out.width='0.8\\linewidth'----
choice_ids <- c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2)
gamble_ids <- c(1, 1, 1, 2, 2, 2, 1, 1, 2, 2)
outcome_ids <- c(1, 2, 3, 1, 2, 3, 1, 2, 1, 2)
objective_consequences <- c(100, 50, 50, 
	100, 100, 7, 
	100, 50, 100, 7)
probability_strings <- c("0.85", "0.1", "0.05", 
	"0.85", "0.1", "0.05", 
	"0.85", "0.15", "0.95", "0.05")
my_choices <- Choices(choice_ids=choice_ids,
	gamble_ids=gamble_ids, 
	outcome_ids=outcome_ids, 
	objective_consequences=objective_consequences, 
	probability_strings=probability_strings)
my_choices

drawChoices(my_choices,
	decision_square_x=0.2, decision_square_edge_length=0.03,
	circle_radius=0.02, y_split_gap=0.07, x_split_offset=0.03, 
	probability_text_digits=4, y_probability_text_offset=0.015, 
	y_value_text_offset=0.005, x_value_text_offset=0.025, 
	probability_text_font_colour="red", probability_text_font_size=10, 
	objective_consequence_text_font_colour="blue", 
	objective_consequence_text_font_size=10, 
	label=c("A","B","C","D"), 
	label_font_colour=c("orange","magenta","green","blue"), 
	label_font_size=c(11,11,11,11),
	label_positions=list(c(0.26,0.9),c(0.26,0.6),
		c(0.26,0.37),c(0.26,0.13)))


## ----fig.show='asis', fig.align='center', tidy=FALSE, out.width='0.8\\linewidth'----
tk_1992_utility <- Utility(fun="power", par=c(alpha=0.88, beta=0.88, lambda=2.25))
tk_1992_positive_prob_weight <- ProbWeight(fun="Tversky_Kahneman_1992", par=c(alpha=0.61))
tk_1992_negative_prob_weight <- ProbWeight(fun="Tversky_Kahneman_1992", par=c(alpha=0.69))
comparePT(my_choices, 
	prob_weight_for_positive_outcomes=tk_1992_positive_prob_weight,
	prob_weight_for_negative_outcomes=tk_1992_negative_prob_weight,
	utility=tk_1992_utility, digits=4)


## ----fig.show='asis', fig.align='center', tidy=FALSE, out.width='0.8\\linewidth'----
my_utility <- Utility(fun="linear", par=c(lambda=2.25))
power_prob_weight <- ProbWeight(fun="power", par=c(alpha=0.7, beta=1))
compareTAX(my_choices, prob_weight=power_prob_weight, utility=my_utility, delta=-1, digits=4)


## ----fig.show='asis', fig.align='center', tidy=FALSE, out.width='0.8\\linewidth'----
my_list <- vsdChoices(x=12, y=96, p="0.1", q="0.9", x_plus=14, y_minus=90, r="0.05")
my_list

drawChoices(my_list[[2]],
	decision_square_x=0.2, decision_square_edge_length=0.05,
	circle_radius=0.025, y_split_gap=0.1, x_split_offset=0.03, 
	probability_text_digits=4, y_probability_text_offset=0.015, 
	y_value_text_offset=0.005, x_value_text_offset=0.025, 
	probability_text_font_colour="red", probability_text_font_size=11, 
	objective_consequence_text_font_colour="blue", 
	objective_consequence_text_font_size=11, label=c("A","B"), 
	label_font_colour=c("orange","magenta"), label_font_size=c(11,11),
	label_positions=list(c(0.26,0.7),c(0.26,0.3)))


## ----fig.show='asis', fig.align='center', tidy=FALSE, out.width='0.8\\linewidth'----
tk_1992_utility <- Utility(fun="power", par=c(alpha=0.88, beta=0.88, lambda=2.25))
linear_in_log_odds_prob_weight <- ProbWeight(fun="linear_in_log_odds", par=c(alpha=0.61, beta=0.724))
comparePT(my_list[[2]], 
	prob_weight_for_positive_outcomes=linear_in_log_odds_prob_weight,
	prob_weight_for_negative_outcomes=linear_in_log_odds_prob_weight,
	utility=tk_1992_utility, digits=4)


## ----fig.show='asis', fig.align='center', tidy=FALSE, out.width='0.8\\linewidth'----
my_utility <- Utility(fun="linear", par=c(lambda=1.0))
power_prob_weight <- ProbWeight(fun="power", par=c(alpha=0.7, beta=1))
compareTAX(my_list[[2]], prob_weight=power_prob_weight, utility=my_utility, delta=-1, digits=4)


