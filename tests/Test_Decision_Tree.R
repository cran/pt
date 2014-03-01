library("pt")

########################	
# These routines test drawing decision trees.
########################

choice_ids <- c(1, 1, 1, 1, 1, 1, 1, 1, 1)
gamble_ids <- c(1, 1, 1, 1, 2, 2, 2, 3, 3)
outcome_ids <- c(1, 2, 3, 4, 1, 2, 3, 1, 2)
objective_consequences <- c(10, 14, 21, 2, 40, 45, 30, 100, 200)
probability_strings <- 
	c("1/4", "1/4", "1/4", "1/4", "1/3", "1/3", "1/3", "1/2", "1/2")

my_choices <- Choices(choice_ids, gamble_ids, outcome_ids, 
	objective_consequences, probability_strings)
my_choices

drawChoices(my_choices,
	decision_square_x=0.2, decision_square_edge_length=0.05,
	circle_radius=0.025, y_split_gap=0.1, x_split_offset=0.03, 
	probability_text_digits=3, y_probability_text_offset=0.015, 
	y_value_text_offset=0.005, x_value_text_offset=0.025, 
	probability_text_font_colour="red", probability_text_font_size=11, 
	objective_consequence_text_font_colour="blue", 
	objective_consequence_text_font_size=11, label=c("A","B","C"), 
	label_font_colour=c("orange","magenta","green"), label_font_size=c(11,11,11),
	label_positions=list(c(0.26,0.7),c(0.26,0.45),c(0.26,0.1)))

########################	
# These routines test drawing the simplex.
########################

my_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=2.25))

drawSimplex(x1=0, x2=100, x3=200,
	line_dot_density=100,	
	draw_ev_flag=TRUE, ev_colour="black",
	draw_pt_flag=TRUE, alpha=0.61, beta=0.724, pt_colour="red", 
	draw_utility_flag=TRUE, utility=my_utility, eu_colour="purple",
	start_points=list(c(0.1,0.9),c(0.2,0.8),c(0.3,0.7),c(0.4,0.6),c(0.5,0.5),c(0.6,0.4),c(0.7,0.3),c(0.8,0.2),c(0.9,0.1)), 
		labels=c("A","B","C","D","increasing preference"),
		label_positions=list(c(0.05,0.02),c(0.07,0.12),c(0.92,0.02),c(0.95,0.10),c(0.7,0.7)),	
		label_colours=c("red","green","blue","orange","red"),
		label_font_sizes=c(12,12,12,12,16),
		label_font_faces=c("plain","plain","plain","plain","bold"),	
		label_rotations=c(0,0,0,0,-45),	
		circle_radii=c(0.005,0.005,0.005,0.005), 	
		circle_outline_colours=c("black","black","black","black"),
		circle_fill_colours=c("red","green","blue","orange"),	
		circle_positions=list(c(0,0),c(0.01,0.1),c(0.89,0),c(0.9,0.1)), 
		lines=list(c(0,0,0.01,0.1),c(0.89,0,0.9,0.1),c(0.01,0.1,0.9,0.1),c(0,0,0.89,0)),
		line_widths=c(1, 1, 1, 1),
		line_styles=c("dashed", "dashed", "dashed", "dashed"),	
		line_colours=c("red","red","red","red"),	
		arrows=list(c(0.8,0.5,0.5,0.8)),
		arrow_widths=c(2),
		arrow_styles=c("solid"),
		arrow_colours=c("red"))

# draw the simplex for the Allais common consequence paradox 
my_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=1))

drawSimplex(x1=0, x2=1000000, x3=5000000,
	line_dot_density=100,	
	draw_ev_flag=TRUE, ev_colour="black",
	draw_pt_flag=TRUE, alpha=0.61, beta=0.724, pt_colour="red", 
	draw_utility_flag=TRUE, utility=my_utility, eu_colour="purple",
	start_points=list(c(0.1,0.9),c(0.2,0.8),c(0.3,0.7),c(0.4,0.6),c(0.5,0.5),c(0.6,0.4),c(0.7,0.3),c(0.8,0.2),c(0.9,0.1)), 
		labels=c("A","B","C","D","increasing preference"),
		label_positions=list(c(0.05,0.02),c(0.07,0.12),c(0.92,0.02),c(0.95,0.10),c(0.7,0.7)),	
		label_colours=c("red","green","blue","orange","red"),
		label_font_sizes=c(12,12,12,12,16),
		label_font_faces=c("plain","plain","plain","plain","bold"),	
		label_rotations=c(0,0,0,0,-45),	
		circle_radii=c(0.005,0.005,0.005,0.005), 	
		circle_outline_colours=c("black","black","black","black"),
		circle_fill_colours=c("red","green","blue","orange"),	
		circle_positions=list(c(0,0),c(0.01,0.1),c(0.89,0),c(0.9,0.1)), 
		lines=list(c(0,0,0.01,0.1),c(0.89,0,0.9,0.1),c(0.01,0.1,0.9,0.1),c(0,0,0.89,0)),
		line_widths=c(1, 1, 1, 1),
		line_styles=c("dashed", "dashed", "dashed", "dashed"),	
		line_colours=c("red","red","red","red"),	
		arrows=list(c(0.8,0.5,0.5,0.8)),
		arrow_widths=c(2),
		arrow_styles=c("solid"),
		arrow_colours=c("red"))

# draw the simplex using the following outcomes (x1=0, x2=1, x3=2)
my_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=1))

drawSimplex(x1=0, x2=1, x3=2, 
	line_dot_density=100,	
	draw_ev_flag=TRUE, ev_colour="black",
	draw_pt_flag=TRUE, alpha=0.61, beta=0.724, pt_colour="red", 
	draw_utility_flag=TRUE, utility=my_utility, eu_colour="purple",
	start_points=list(c(0.1,0.9),c(0.2,0.8),c(0.3,0.7),c(0.4,0.6),c(0.5,0.5),c(0.6,0.4),c(0.7,0.3),c(0.8,0.2),c(0.9,0.1)), 
		labels=c("A","B","C","D","increasing preference"),
		label_positions=list(c(0.05,0.02),c(0.07,0.12),c(0.92,0.02),c(0.95,0.10),c(0.7,0.7)),	
		label_colours=c("red","green","blue","orange","red"),
		label_font_sizes=c(12,12,12,12,16),
		label_font_faces=c("plain","plain","plain","plain","bold"),	
		label_rotations=c(0,0,0,0,-45),
		circle_radii=c(0.005,0.005,0.005,0.005), 	
		circle_outline_colours=c("black","black","black","black"),
		circle_fill_colours=c("red","green","blue","orange"),	
		circle_positions=list(c(0,0),c(0.01,0.1),c(0.89,0),c(0.9,0.1)), 
		lines=list(c(0,0,0.01,0.1),c(0.89,0,0.9,0.1),c(0.01,0.1,0.9,0.1),c(0,0,0.89,0)),
		line_widths=c(1, 1, 1, 1),
		line_styles=c("dashed", "dashed", "dashed", "dashed"),	
		line_colours=c("red","red","red","red"),	
		arrows=list(c(0.8,0.5,0.5,0.8)),
		arrow_widths=c(2),
		arrow_styles=c("solid"),
		arrow_colours=c("red"))


# draw a minimal version of the above, omitting extra labelling 
my_utility <- Utility(fun="power", 
	par=c(alpha=0.88, beta=0.88, lambda=1))

drawSimplex(x1=0, x2=1, x3=2, 
	line_dot_density=100,	
	draw_ev_flag=TRUE, ev_colour="black",
	draw_pt_flag=TRUE, alpha=0.61, beta=0.724, pt_colour="red", 
	draw_utility_flag=TRUE, utility=my_utility, eu_colour="purple",
	start_points=list(c(0.1,0.9),c(0.2,0.8),c(0.3,0.7),c(0.4,0.6),c(0.5,0.5),c(0.6,0.4),c(0.7,0.3),c(0.8,0.2),c(0.9,0.1)))
