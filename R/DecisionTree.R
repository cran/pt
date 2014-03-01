#########################
#
# Decision_Tree.R
#
# Draw a decision tree for one-stage prospects using grid graphics.
#
########################

#' @name drawChoices
#' @title Draw a one-stage decision tree.
#' @description Draws choices.
#' @param choices, an instance of class Choices
#' @param decision_square_x numeric, the decision_square_x position
#' @param decision_square_edge_length numeric, the decision_square_edge_length
#' @param circle_radius numeric, the circle_radius
#' @param y_split_gap numeric, the vertical gap between outcomes
#' @param x_split_offset numeric, the horizontal gap between the decision square and decision circles
#' @param probability_text_digits numeric, the digits to use for probability text
#' @param y_probability_text_offset numeric, the y offset between an outcome line and the probability text
#' @param y_value_text_offset numeric, the vertical offset between an outcome line and the objective_consequence text
#' @param x_value_text_offset numeric, the horizontal offset between an outcome line and the objective_consequence text
#' @param probability_text_font_colour character, the colour of the probability_text
#' @param probability_text_font_size numeric, the font size of the probability_text
#' @param objective_consequence_text_font_colour character, the colour of the objective_consequence_text
#' @param objective_consequence_text_font_size numeric, the font size of the objective_consequence_text_font_size
#' @param label vector, extra text labels
#' @param label_font_colour vector, label_font_colour
#' @param label_font_size vector, label_font_size
#' @param label_positions list, a list of label coordinates of the form (x,y)
#' @param line_positions list, a list of line coordinates of the form (x1,y1,x2,y2)
#' @param line_colours vector, line_colours
#' @param line_styles vector, line_styles
#' @param line_arrows vector, line_arrows
#' @param line_widths vector, line_widths
#' @examples
#' 
#' # This example creates the Allais common consequence paradox choices, and
#' # draws them.
#' 
#' choice_ids <- c(1, 1, 1, 1, 2, 2, 2, 2)
#' gamble_ids <- c(1, 1, 1, 2, 1, 1, 2, 2)
#' outcome_ids <- c(1, 2, 3, 1, 1, 2, 1, 2)
#' objective_consequences <- c(2500, 2400, 0, 2400, 
#' 	2500, 0, 2400, 0)
#' probability_strings <- c("0.33", "0.66", "0.01", "1.0", 
#' 	"0.33", "0.67", "0.34", "0.66")
#' my_choices <- Choices(choice_ids=choice_ids,
#' 	gamble_ids=gamble_ids, 
#' 	outcome_ids=outcome_ids, 
#' 	objective_consequences=objective_consequences, 
#' 	probability_strings=probability_strings)
#' my_choices
#' 
#' drawChoices(my_choices, decision_square_x=0.2, decision_square_edge_length=0.05,
#' 	circle_radius=0.025, y_split_gap=0.1, x_split_offset=0.03, 
#' 	probability_text_digits=4, y_probability_text_offset=0.015, 
#' 	y_value_text_offset=0.005, x_value_text_offset=0.025, 
#' 	probability_text_font_colour="red", probability_text_font_size=11, 
#' 	objective_consequence_text_font_colour="blue", 
#' 	objective_consequence_text_font_size=11, label=c("A","B","C", "D"), 
#' 	label_font_colour=c("orange","magenta","green","blue"), 
#' 	label_font_size=c(11,11,11,11),
#' 	label_positions=list(c(0.26,0.85),c(0.26,0.55),
#' 		c(0.26,0.4),c(0.26,0.1)))
#' 		
#' @export
drawChoices <- function(choices, decision_square_x, decision_square_edge_length, 
	circle_radius, y_split_gap, x_split_offset, 
	probability_text_digits, 
	y_probability_text_offset, y_value_text_offset, x_value_text_offset, 
	probability_text_font_colour, probability_text_font_size, 
	objective_consequence_text_font_colour, objective_consequence_text_font_size, 
	label, label_font_colour, label_font_size, label_positions, 
	line_positions, line_colours, line_styles, line_arrows, line_widths)
{
	
	plot.new()

	gambless <- get_choices(choices)
	
	n_gambles <- length(gambless)
	
	square_gap <- 1/n_gambles
	
	decision_square_y <- 1 - square_gap/2
	
	for (choice_index in 1:length(gambless))
	{
		gambles <- gambless[[choice_index]]
	
		draw_multiple_gambles_internal(gambles, decision_square_x, decision_square_y, decision_square_edge_length, circle_radius, y_split_gap, x_split_offset, probability_text_digits, y_probability_text_offset, y_value_text_offset, x_value_text_offset, probability_text_font_colour, probability_text_font_size, objective_consequence_text_font_colour, objective_consequence_text_font_size, label, label_font_colour, label_font_size, label_positions, line_positions, line_colours, line_styles, line_arrows, line_widths)
	
		decision_square_y <- decision_square_y - square_gap	

	}
	

	return (invisible())
}


draw_multiple_gambles <- function(gambles,
	decision_square_x, decision_square_y, decision_square_edge_length, 
	circle_radius, y_max, y_split_gap, 
	x_split_offset, horizontal_line_length, probability_text_digits, 
	y_probability_text_offset, y_value_text_offset, x_value_text_offset, 
	probability_text_font_colour, probability_text_font_size,
	objective_consequence_text_font_colour, objective_consequence_text_font_size,
	label, label_font_colour, label_font_size, label_positions, 
	line_positions, line_colours, line_styles, line_arrows, line_widths)
{

	draw_multiple_gambles_internal(gambles, decision_square_x, decision_square_y, decision_square_edge_length, circle_radius, y_split_gap, x_split_offset, probability_text_digits, y_probability_text_offset, y_value_text_offset, x_value_text_offset, probability_text_font_colour, probability_text_font_size, objective_consequence_text_font_colour, objective_consequence_text_font_size, label, label_font_colour, label_font_size, label_positions, line_positions, line_colours, line_styles, line_arrows, line_widths)
	
	return (invisible())
}
  

draw_multiple_gambles_internal <- function(gambles, decision_square_x, decision_square_y, decision_square_edge_length, circle_radius, y_split_gap, x_split_offset, probability_text_digits, y_probability_text_offset, y_value_text_offset, x_value_text_offset, probability_text_font_colour, probability_text_font_size, objective_consequence_text_font_colour, objective_consequence_text_font_size, label, label_font_colour, label_font_size, label_positions, line_positions, line_colours, line_styles, line_arrows, line_widths)
{

	number_of_gambles <- get_number_of_gambles(gambles)
	number_of_gambles
	
	total_number_of_gamble_outcomes <- get_total_number_of_gamble_outcomes(gambles)

	total_y_range <- (y_split_gap * total_number_of_gamble_outcomes) - y_split_gap				
	total_y_range
	
	y_max <- decision_square_y + total_y_range / 2
	y_max
	

	# need to calculate y position of the circles

	y_circle_pos <- y_max
	rolling_outcome_total <- 0

	for (i in 1:number_of_gambles)
	{
		number_of_gamble_outcomes <- get_number_of_gamble_outcomes(gambles, i)
		
		rolling_outcome_total <- rolling_outcome_total + number_of_gamble_outcomes
		
		y_circle_pos <- y_circle_pos - y_split_gap * (number_of_gamble_outcomes - 1) / 2
		
		
		# draw lines from decision square to circles	
			
		horizontal_line_length <- circle_radius * 3
		
		grid::grid.move.to(decision_square_x, decision_square_y)
		grid::grid.line.to(decision_square_x + x_split_offset, y_circle_pos)
		grid::grid.line.to(decision_square_x + x_split_offset + horizontal_line_length, y_circle_pos)			
			
		
		# draw circles
		draw_single_gamble_internal(get_gamble(gambles, i), 
			decision_square_x + x_split_offset + horizontal_line_length, 
			y_circle_pos, 
			circle_radius,
 			y_max,
			y_split_gap, 
			x_split_offset, 
			horizontal_line_length,
			probability_text_digits,
			y_probability_text_offset,
			y_value_text_offset,
			x_value_text_offset,
			probability_text_font_colour,
			probability_text_font_size,
			objective_consequence_text_font_colour,
			objective_consequence_text_font_size)
		
		# shift the position down one unit to prepare for next gamble
		y_circle_pos <- y_max - (rolling_outcome_total - 1) * y_split_gap - y_split_gap
	
	}

	# draw decision square last

	grid::grid.rect(x = decision_square_x, y = decision_square_y, width = decision_square_edge_length, height = decision_square_edge_length, default.units="npc", name=NULL, gp=grid::gpar(fill="white"), draw=TRUE, vp=NULL)

	
	# draw extra text labels

	if (!missing("label"))
	{
		for (i in 1:length(label))
		{
			label_position <- label_positions[[i]]
			label_x <- label_position[1]
			label_y <- label_position[2]
			
			grid::grid.text(label[i], x = label_x, y = label_y, gp=grid::gpar(col=label_font_colour[i], fontsize=label_font_size[i]))
			
		}
	}
	
	# draw extra lines	
	
	if (!missing("line_positions"))
	{
		
		for (i in 1:length(line_positions))
		{
			line_position <- line_positions[[i]]
			x1 <- line_position[1]
			y1 <- line_position[2]
			x2 <- line_position[3]
			y2 <- line_position[4]
			
			grid::grid.move.to(x1, y1)
			
			if (line_arrows[i] == TRUE)
			{
			
				grid::grid.line.to(x2, y2, gp = grid::gpar(col = line_colours[i], lty=line_styles[i], lwd=line_widths[i]), arrow=grid::arrow(angle = 30, length = grid::unit(0.01, "npc"), ends = "last", type = "open"))
			}
			else
			{
				grid::grid.line.to(x2, y2, gp = grid::gpar(col = line_colours[i], lty=line_styles[i], lwd=line_widths[i]))				
			}
					
		}
	}
	
	return (invisible())
}


draw_single_gamble_internal <- function(gamble, centre_x, centre_y, circle_radius, y_max, y_split_gap, x_split_offset, horizontal_line_length, probability_text_digits, y_probability_text_offset, y_value_text_offset, x_value_text_offset, probability_text_font_colour, probability_text_font_size, objective_consequence_text_font_colour, objective_consequence_text_font_size)
{

	number_of_outcomes <- get_number_of_outcomes(gamble)

	if (number_of_outcomes == 1)
	{
		
		grid::grid.move.to(centre_x, centre_y)
		grid::grid.line.to(centre_x + x_split_offset + horizontal_line_length, centre_y)
		
		outcome_probability_string <- get_gamble_outcome_probability_string(gamble, 1)
		outcome_objective_consequence <- get_gamble_outcome_objective_consequence(gamble, 1)

		grid::grid.text(outcome_probability_string, x = centre_x + x_split_offset + horizontal_line_length / 2, y = centre_y + y_probability_text_offset, gp=grid::gpar(col=probability_text_font_colour, fontsize=probability_text_font_size))
		grid::grid.text(format(outcome_objective_consequence, scientific=FALSE), x = centre_x + x_split_offset + horizontal_line_length + x_value_text_offset, y = centre_y + y_value_text_offset, just = "left", gp=grid::gpar(col=objective_consequence_text_font_colour, fontsize=objective_consequence_text_font_size))
	}
	# an even number of outcomes
	else if (number_of_outcomes %% 2 == 0)
	{
		
		# draw upper outcomes		
		
		upper_number_of_outcomes <- ceiling(number_of_outcomes / 2)
		lower_number_of_outcomes <- upper_number_of_outcomes
		
	
		y_outcome_position <- y_split_gap / 2
	
		for (i in 0:(upper_number_of_outcomes-1))
		{
			grid::grid.move.to(centre_x, centre_y)
			grid::grid.line.to(centre_x + x_split_offset, centre_y + y_outcome_position + y_split_gap * i)
			grid::grid.line.to(centre_x + x_split_offset + horizontal_line_length, centre_y + y_outcome_position + y_split_gap * i)
			
			outcome_probability_string <- get_gamble_outcome_probability_string(gamble, i+1)
			outcome_objective_consequence <- get_gamble_outcome_objective_consequence(gamble, i+1)				

			grid::grid.text(outcome_probability_string, x = centre_x + x_split_offset + horizontal_line_length / 2, y = centre_y + y_outcome_position + y_split_gap * i + y_probability_text_offset, gp=grid::gpar(col=probability_text_font_colour, fontsize=probability_text_font_size))
			grid::grid.text(format(outcome_objective_consequence, scientific=FALSE), x = centre_x + x_split_offset + horizontal_line_length + x_value_text_offset, y = centre_y + y_outcome_position + y_split_gap * i + y_value_text_offset, just = "left", gp=grid::gpar(col=objective_consequence_text_font_colour, fontsize=objective_consequence_text_font_size))		
		}
		
	
		for (i in 0:(lower_number_of_outcomes-1))
		{
			grid::grid.move.to(centre_x, centre_y)
			grid::grid.line.to(centre_x + x_split_offset, centre_y - y_outcome_position - y_split_gap * i)
			grid::grid.line.to(centre_x + x_split_offset + horizontal_line_length, centre_y - y_outcome_position - y_split_gap * i)
			
			# note: the local loop index is from 1:n to do the lower outcomes, but we are still in the outer loop at position upper_number_of_outcomes + i 
			
			outcome_probability_string <- get_gamble_outcome_probability_string(gamble, upper_number_of_outcomes + i+1)		
			outcome_objective_consequence <- get_gamble_outcome_objective_consequence(gamble, upper_number_of_outcomes + i+1)				

			grid::grid.text(outcome_probability_string, x = centre_x + x_split_offset + horizontal_line_length / 2, y = centre_y - y_outcome_position - y_split_gap * i + y_probability_text_offset, gp=grid::gpar(col=probability_text_font_colour, fontsize=probability_text_font_size))
			grid::grid.text(format(outcome_objective_consequence, scientific=FALSE), x = centre_x + x_split_offset + horizontal_line_length + x_value_text_offset, y = centre_y - y_outcome_position - y_split_gap * i + y_value_text_offset, just = "left", gp=grid::gpar(col=objective_consequence_text_font_colour, fontsize=objective_consequence_text_font_size))		
		}
		
	}
	# an odd number of outcomes	
	else
	{
	
		# draw upper outcomes
		
		one_below_half_point <- floor(number_of_outcomes / 2)
		
		max_y <- y_split_gap * one_below_half_point
		
		outcome_y <- max_y
		
		for (i in 1:one_below_half_point)
		{
			grid::grid.move.to(centre_x, centre_y)
			grid::grid.line.to(centre_x + x_split_offset, centre_y + outcome_y)
			grid::grid.line.to(centre_x + x_split_offset + horizontal_line_length, centre_y + outcome_y)
			
			outcome_probability_string <- get_gamble_outcome_probability_string(gamble, i)		
			outcome_objective_consequence <- get_gamble_outcome_objective_consequence(gamble, i)				

			grid::grid.text(outcome_probability_string, x = centre_x + x_split_offset + horizontal_line_length / 2, y = centre_y + outcome_y + y_probability_text_offset, gp=grid::gpar(col=probability_text_font_colour, fontsize=probability_text_font_size))
			grid::grid.text(format(outcome_objective_consequence, scientific=FALSE), x = centre_x + x_split_offset + horizontal_line_length + x_value_text_offset, y = centre_y + outcome_y + y_value_text_offset, just = "left", gp=grid::gpar(col=objective_consequence_text_font_colour, fontsize=objective_consequence_text_font_size))		
			
			outcome_y <- outcome_y - y_split_gap
		}
		
		# draw the middle outcome
		
		grid::grid.move.to(centre_x, centre_y)
		grid::grid.line.to(centre_x + x_split_offset + horizontal_line_length, centre_y)
		
		outcome_probability_string <- get_gamble_outcome_probability_string(gamble, one_below_half_point + 1)	
		outcome_objective_consequence <- get_gamble_outcome_objective_consequence(gamble, one_below_half_point + 1)				

		grid::grid.text(outcome_probability_string, x = centre_x + x_split_offset + horizontal_line_length / 2, y = centre_y + y_probability_text_offset, gp=grid::gpar(col=probability_text_font_colour, fontsize=probability_text_font_size))
		grid::grid.text(format(outcome_objective_consequence, scientific=FALSE), centre_x + x_split_offset + horizontal_line_length + x_value_text_offset, y = centre_y + y_value_text_offset, just = "left", gp=grid::gpar(col=objective_consequence_text_font_colour, fontsize=objective_consequence_text_font_size))	
		
		
		# draw lower outcomes	
		
		outcome_y <- y_split_gap	
		
		for (i in (one_below_half_point + 2):number_of_outcomes)
		{
			grid::grid.move.to(centre_x, centre_y)
			grid::grid.line.to(centre_x + x_split_offset, centre_y - outcome_y)
			grid::grid.line.to(centre_x + x_split_offset + horizontal_line_length, centre_y - outcome_y)
			
			outcome_probability_string <- get_gamble_outcome_probability_string(gamble, i)		
			outcome_objective_consequence <- get_gamble_outcome_objective_consequence(gamble, i)				

			grid::grid.text(outcome_probability_string, x = centre_x + x_split_offset + horizontal_line_length / 2, y = centre_y - outcome_y + y_probability_text_offset, gp=grid::gpar(col=probability_text_font_colour, fontsize=probability_text_font_size))
			grid::grid.text(format(outcome_objective_consequence, scientific=FALSE), x = centre_x + x_split_offset + horizontal_line_length + x_value_text_offset, y = centre_y - outcome_y + y_value_text_offset, just = "left", gp=grid::gpar(col=objective_consequence_text_font_colour, fontsize=objective_consequence_text_font_size))
			
			outcome_y <- outcome_y + y_split_gap
			
		}
		
	}
	
	# draw a filled white circle as the last operation to cover the lines extending from the circle centre.
	
	grid::grid.circle(x = centre_x, y = centre_y, r = circle_radius, default.units="npc", name=NULL, gp=grid::gpar(fill="white"), draw=TRUE, vp=NULL)
	
	
	return (invisible())
}


draw_single_gamble <- function(gamble, centre_x, centre_y, circle_radius, 
	y_max, y_split_gap, x_split_offset, horizontal_line_length, 
	probability_text_digits, y_probability_text_offset, y_value_text_offset, 
	x_value_text_offset, probability_text_font_colour, probability_text_font_size, 
	objective_consequence_text_font_colour, objective_consequence_text_font_size)
{

	draw_single_gamble_internal(gamble, centre_x, centre_y, circle_radius, y_max, y_split_gap, x_split_offset, horizontal_line_length, probability_text_digits, y_probability_text_offset, y_value_text_offset, x_value_text_offset, probability_text_font_colour, probability_text_font_size, objective_consequence_text_font_colour, objective_consequence_text_font_size)
	
	return (invisible())
}

#' @name drawSimplex
#' @title Draw the probability simplex.
#' @description Draws the probability simplex.
#' @details Iso-expected value lines, expected utility indifference lines and prospect theory
#' indifference curves (based on a linear in log odds probability weighting function) can be drawn.
#' @references
#' Marschak, J. (1950). Rational behavior, uncertain prospects, and measurable utility. Econometrica, 18(2), 111-141.
#' 
#' Machina, M. J. (1987). Choice under uncertainty: Problems solved and unsolved. Journal of Economic Perspectives, 1(1), 121-154.
#' 
#' @param x1 numeric, x1
#' @param x2 numeric, x2
#' @param x3 numeric, x3
#' @param line_dot_density numeric, the number of dots to use when drawing lines
#' @param draw_ev_flag logical, flag to tell whether to draw expected value indifference lines
#' @param ev_colour, the colour of the expected value indifference lines
#' @param draw_pt_flag logical, flag to tell whether to draw prospect theory indifference curves
#' @param alpha numeric, the alpha parameter in the linear_in_log_odds pwf
#' @param beta numeric, beta parameter in the linear_in_log_odds pwf
#' @param pt_colour character, the colour of the prospect theory indifference curves
#' @param draw_utility_flag logical, flag to tell whether to draw expected utility indifference lines 
#' @param utility Utility, utility
#' @param eu_colour character, the colour of the expected utility indifference lines
#' @param start_points list, start_points for the family of indifference lines or curves
#' @param labels vector, a vector of text labels
#' @param label_positions vector, a vector of label_positions
#' @param label_colours vector, a vector of label_colours
#' @param label_font_sizes vector, a vector of label_font_sizes
#' @param label_font_faces vector, a vector of label_font_faces
#' @param label_rotations vector, a vector of label_rotations
#' @param circle_radii vector, a vector of circle_radii
#' @param circle_outline_colours vector, a vector of circle_outline_colours
#' @param circle_fill_colours vector, a vector of circle_fill_colours
#' @param circle_positions vector, a vector of circle_positions
#' @param lines vector, a vector of lines
#' @param line_widths vector, a vector of line_widths
#' @param line_styles vector, a vector of line_styles
#' @param line_colours vector, a vector of line_colours
#' @param arrows vector, a vector of arrows
#' @param arrow_widths vector, a vector of arrow_widths
#' @param arrow_styles vector, a vector of arrow_styles
#' @param arrow_colours vector, a vector of arrow_colours
#' @examples
#' 
#' my_utility <- Utility(fun="power", 
#'	par=c(alpha=0.88, beta=0.88, lambda=2.25))
#'
#'drawSimplex(x1=0, x2=100, x3=200,
#'	line_dot_density=100,	
#'	draw_ev_flag=TRUE, ev_colour="black",
#'	draw_pt_flag=TRUE, alpha=0.61, beta=0.724, pt_colour="red", 
#'	draw_utility_flag=TRUE, utility=my_utility, eu_colour="purple",
#'	start_points=list(c(0.1,0.9),c(0.2,0.8),c(0.3,0.7),
#'	c(0.4,0.6),c(0.5,0.5),c(0.6,0.4),c(0.7,0.3),c(0.8,0.2),c(0.9,0.1)), 
#'		labels=c("A","B","C","D","increasing preference"),
#'		label_positions=list(c(0.05,0.02),c(0.07,0.12),
#'		c(0.92,0.02),c(0.95,0.10),c(0.7,0.7)),	
#'		label_colours=c("red","green","blue","orange","red"),
#'		label_font_sizes=c(12,12,12,12,16),
#'		label_font_faces=c("plain","plain","plain","plain","bold"),	
#'		label_rotations=c(0,0,0,0,-45),	
#'		circle_radii=c(0.005,0.005,0.005,0.005), 	
#'		circle_outline_colours=c("black","black","black","black"),
#'		circle_fill_colours=c("red","green","blue","orange"),	
#'		circle_positions=list(c(0,0),c(0.01,0.1),c(0.89,0),c(0.9,0.1)), 
#'		lines=list(c(0,0,0.01,0.1),c(0.89,0,0.9,0.1),
#'		c(0.01,0.1,0.9,0.1),c(0,0,0.89,0)),
#'		line_widths=c(1, 1, 1, 1),
#'		line_styles=c("dashed", "dashed", "dashed", "dashed"),	
#'		line_colours=c("red","red","red","red"),	
#'		arrows=list(c(0.8,0.5,0.5,0.8)),
#'		arrow_widths=c(2),
#'		arrow_styles=c("solid"),
#'		arrow_colours=c("red"))
#'		
#' @export
drawSimplex <- function(x1, x2, x3,
	line_dot_density,
	draw_ev_flag, ev_colour,
	draw_pt_flag, alpha, beta, pt_colour,
	draw_utility_flag, utility, eu_colour,
	start_points, 
	labels, label_positions, label_colours, label_font_sizes, label_font_faces, label_rotations,
	circle_radii, circle_outline_colours, circle_fill_colours, circle_positions, 
	lines, line_widths, line_styles, line_colours, 
	arrows, arrow_widths, arrow_styles, arrow_colours)
{

	plot.new()	
	
	# draw grid and text

	# need to draw axes text labels before everything else, otherwise it will be overwritten.
	
	increment <-  0	
	
	for (prob in seq(0, 1, 0.1))
	{
		grid::grid.text(label=as.character(prob), x = grid::unit(0.1 + increment, "npc"), y = grid::unit(0.1 - 0.03, "npc"), just = "centre",
			default.units = "npc", gp=grid::gpar(col="black", fontsize=12), draw = TRUE)
		
		grid::grid.move.to(0.1 + increment, 0.1)
		grid::grid.line.to(0.1 + increment, 0.1 - 0.01)	
		
		grid::grid.text(label=as.character(prob), x = grid::unit(0.1 - 0.03, "npc"), y = grid::unit(0.1 + increment, "npc"), just = "centre",
			default.units = "npc", gp=grid::gpar(col="black", fontsize=12), draw = TRUE)		
	
		grid::grid.move.to(0.1, 0.1 + increment)
		grid::grid.line.to(0.1 - 0.01, 0.1 + increment)

		grid::grid.text(label=as.character(prob), x = grid::unit(0.9 + 0.02 - increment, "npc"), y = grid::unit(0.1 + increment + 0.03, "npc"), just = "left",
			default.units = "npc", gp=grid::gpar(col="black", fontsize=12), draw = TRUE)		
		
		grid::grid.move.to(0.9 - increment, 0.1 + increment)
		grid::grid.line.to(0.9 - increment + 0.01, 0.1 + increment + 0.01)	
		
		
		grid::grid.move.to(0.1, 0.1 + increment)
		grid::grid.line.to(0.9 - increment, 0.1 + increment, gp=grid::gpar(col="gray90", lty="dotted"))
		
		
		
		increment <- increment + 0.8/10
	}	
	
	grid::grid.text(label=expression(p[1]), x = grid::unit(0.1 + 0.8/2, "npc"), y = grid::unit(0.1 - 0.08, "npc"), just = "centre",
		default.units = "npc", gp=grid::gpar(col="black", fontsize=12), draw = TRUE)
	grid::grid.text(label=expression(p[2]==1), x = grid::unit(0.1 - 0.08, "npc"), y = grid::unit(0.1 - 0.04, "npc"), just = "left",
		default.units = "npc", gp=grid::gpar(col="black", fontsize=12), draw = TRUE)		
	grid::grid.text(label=expression(p[3]), x = grid::unit(0.1 - 0.08, "npc"), y = grid::unit(0.1 + 0.8/2, "npc"), just = "centre",
		default.units = "npc", gp=grid::gpar(col="black", fontsize=12), draw = TRUE)	
	
	x1_string <- format(x1, digits=15, scientific=FALSE)		
	f <- quote(x[1]==x1_string)
	g <- eval(substitute(substitute(expr, list(x1_string=x1_string)), list(expr= f)))	
	grid::grid.text(label=g, x = grid::unit(0.9, "npc"), y = grid::unit(0.1 - 0.08, "npc"), just = "centre",
		default.units = "npc", gp=grid::gpar(col="black", fontsize=12), draw = TRUE)
	
	x2_string <- format(x2, digits=15, scientific=FALSE)	
	f <- quote(x[2]==x2_string)
	g <- eval(substitute(substitute(expr, list(x2_string=x2_string)), list(expr= f)))	
	grid::grid.text(label=g, x = grid::unit(0.1 - 0.08, "npc"), y = grid::unit(0.1 - 0.08, "npc"), just = "left",
		default.units = "npc", gp=grid::gpar(col="black", fontsize=12), draw = TRUE)
	
	x3_string <- format(x3, digits=15, scientific=FALSE)
	f <- quote(x[3]==x3_string)
	g <- eval(substitute(substitute(expr, list(x3_string=x3_string)), list(expr= f)))		
	grid::grid.text(label=g, x = grid::unit(0.1 - 0.08, "npc"), y = grid::unit(0.97, "npc"), just = "left",
		default.units = "npc", gp=grid::gpar(col="black", fontsize=12), draw = TRUE)	
	
	
	# draw a triangle outline
	grid::grid.move.to(0.1, 0.1)
	grid::grid.line.to(0.1, 0.9)
	
	grid::grid.move.to(0.1, 0.1)
	grid::grid.line.to(0.9, 0.1)	
	
	grid::grid.move.to(0.1, 0.9)
	grid::grid.line.to(0.9, 0.1)
	

	
	# need to compute Ks for each start point	
	line_dot_increment <- 1/line_dot_density

	
	for (i in 1:length(start_points))
	{
		##################################
		# draw ev iso-expected value lines		
		
		sp <- start_points[[i]]
		p1 <- sp[1]
		p3 <- sp[2]		
				
		ux1 <- x1
		ux2 <- x2
		ux3 <- x3	
		
		#compute k		
		k <- ux1 * p1 + ux2 * (1-p1-p3) + ux3 * p3
		
		if (draw_ev_flag == TRUE)
		{		
			for (p1 in seq(0, 1, line_dot_increment))
			{	
				p3 <- (k - ux2)/(ux3-ux2) + (ux2-ux1) * p1 / (ux3-ux2)
				
				if ((p3 >= 0) && (p3 <= 1))
				{
					if (p1 + p3 < 1)
					{
						grid::grid.circle(x=p1*0.8+0.1, y=p3*0.8+0.1, r=0.0001, default.units="npc", gp=grid::gpar(col=ev_colour), draw=TRUE)
					}					
				}		
			}
		}		
		
		
		
		############################		
		# draw eu indifference lines
		
		if (draw_utility_flag == TRUE)
		{
			
			sp <- start_points[[i]]
			p1 <- sp[1]
			p3 <- sp[2]		
			
			
			ux1 <- compute_utility(utility, x1)
			ux2 <- compute_utility(utility, x2)
			ux3 <- compute_utility(utility, x3)			
			
			#compute k		
			k <- ux1*p1 + ux2*(1-p1-p3) + ux3*p3
			
			for (p1 in seq(0, 1, line_dot_increment))
			{	
				p3 <- (k - ux2)/(ux3-ux2) + (ux2-ux1) * p1 / (ux3-ux2)
				
				if ((p3 >= 0) && (p3 <= 1))
				{
					if (p1 + p3 < 1)
					{
						grid::grid.circle(x=p1*0.8+0.1, y=p3*0.8+0.1, r=0.0001, default.units="npc", gp=grid::gpar(col=eu_colour), draw=TRUE)
					}
					
				}
			}
			
		}
		
		
		
		###########################################################		
		# draw cpt indifference curves using linear in log odds pwf
		
		if (draw_pt_flag == TRUE)
		{
			
			sp <- start_points[[i]]
			p1 <- sp[1]
			p3 <- sp[2]		
			
			ux1 <- compute_utility(utility, x1)
			ux2 <- compute_utility(utility, x2)
			ux3 <- compute_utility(utility, x3)				
			
			#compute k
			
			wp1 <- beta * p1^alpha / (beta * p1^alpha + (1-p1)^alpha)
			w_1_minus_p3 <- beta * (1-p3)^alpha / (beta * (1-p3)^alpha + p3^alpha)		
			
			k <- wp1*ux1 + (w_1_minus_p3 - wp1)*ux2 + (1 - w_1_minus_p3)*ux3	
			
			
			for (p1 in seq(0, 1, line_dot_increment))
			{	
				wp1 <- beta * p1^alpha / (beta * p1^alpha + (1-p1)^alpha)					
				
				c <- (k-ux3)/(ux2-ux3) - wp1 * (ux1-ux2)/(ux2-ux3)			
				
				p3 <- ((c*beta)^(1/alpha) - beta^(1/alpha)) / ((c*beta)^(1/alpha) - beta^(1/alpha) - c^(1/alpha))
				
			
				if (is.nan(p3) == FALSE)
				{		
					if ((p3 >= 0) && (p3 <= 1))
					{
						if (p1 + p3 < 1)
						{
							grid::grid.circle(x=p1*0.8+0.1, y=p3*0.8+0.1, r=0.0001, default.units="npc", gp=grid::gpar(col=pt_colour), draw=TRUE)
						}
					}
				}
			}
		}
	}	
		
	###############################
	# draw labels
	
	if (!missing("labels"))
	{
		for (i in 1:length(labels))
		{
			my_label <- labels[i]
			label_position <- label_positions[[i]]
			p1 <- label_position[1]
			p3 <- label_position[2]		
			
			grid::grid.text(label=my_label, x = grid::unit(p1*0.8+0.1, "npc"), y = grid::unit(p3*0.8+0.1, "npc"), just = "centre",
				rot=label_rotations[i],
				default.units = "npc", 
				gp=grid::gpar(col=label_colours[i], fontsize=label_font_sizes[i], fontface=label_font_faces[i]), 
				draw = TRUE)		
		}	
	}
	
	###############################
	# draw circles
	
	if (!missing("circle_positions"))
	{
		for (i in 1:length(circle_positions))
		{
			circle_position <- circle_positions[[i]]
			p1 <- circle_position[1]
			p3 <- circle_position[2]		
			
			grid::grid.circle(x=p1*0.8+0.1, y=p3*0.8+0.1, r=circle_radii[i], default.units="npc", gp=grid::gpar(col=circle_outline_colours[i], fill=circle_fill_colours[i], alpha=0.5), draw=TRUE)
		}
	}
	
	###############################
	# draw lines
	
	if (!missing("lines"))
	{	
		for (i in 1:length(lines))
		{
			line <- lines[[i]]
			p1a <- line[1]
			p3a <- line[2]		
			p1b <- line[3]
			p3b <- line[4]	
			
			grid::grid.move.to(x=p1a*0.8+0.1, y=p3a*0.8+0.1)
			grid::grid.line.to(x=p1b*0.8+0.1, y=p3b*0.8+0.1, gp=grid::gpar(col=line_colours[i], lwd=line_widths[i], lty=line_styles[i]))
		}
	}
	
	###############################
	# draw arrows

	if (!missing("arrows"))
	{	
		for (i in 1:length(arrows))
		{
			arrow <- arrows[[i]]
			p1a <- arrow[1]
			p3a <- arrow[2]		
			p1b <- arrow[3]
			p3b <- arrow[4]	
			
			grid::grid.move.to(x=p1a*0.8+0.1, y=p3a*0.8+0.1)
			grid::grid.line.to(x=p1b*0.8+0.1, y=p3b*0.8+0.1, arrow=grid::arrow(angle = 30, length = grid::unit(0.01, "npc"), ends = "last", type = "open"), 
				gp=grid::gpar(col=arrow_colours[i], lwd=arrow_widths[i], lty=arrow_styles[i]))
		}
	}
	
	return (invisible())
}

  
