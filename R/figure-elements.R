# Figure elements

# Add x-axis scale: initial fraction strain A
#
add_scale_initial_fraction <- function(input_fig, name_A) {
	breaks <- seq(0, 1, by = 0.2)
	input_fig +
		ggplot2::aes(x = .data$initial_fraction_A) +
		ggplot2::scale_x_continuous(
			name = paste("Initial fraction", name_A),
			limits = c(0, 1),
			breaks = breaks,
			minor_breaks = breaks
		)
}

# Add y-axis scale: Within-group fitness ratio A/B
#
add_scale_fitness_ratio <- function(input_fig, name_A, name_B) {
	input_fig +
	ggplot2::aes(y = .data$fitness_ratio_A_B) +
	ggplot2::scale_y_log10(
		name = paste("Fitness ratio\n", name_A, "/",name_B),
		labels = scales::label_log(),
	)
	# ggplot2::geom_hline(yintercept = 1, color = "white", linewidth = 0.8)
}

# Add points for within-group plots
#
add_points_within_group <- function(input_fig) {
	my_color <- gray(0.1)
	my_fill <- gray(0.65)
	input_fig +
	ggplot2::geom_point(shape = 21, color = my_color, fill = my_fill) +
	ggplot2::geom_point(shape = 1, color = my_color)
}



