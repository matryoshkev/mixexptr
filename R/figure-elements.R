# Figure elements

# Theme
#   Size suited to figures in papers
#   Less clutter
theme_mixexptr <- function(...) {
	ggplot2::theme_grey(...) +
	ggplot2::theme(
		text                 = ggplot2::element_text(size = 9),
		legend.title         = ggplot2::element_blank(),
		legend.background    = ggplot2::element_blank(),
		legend.direction     = "horizontal",
		legend.justification = c(0.5, 0.15),
		legend.position      = c(0.5, 1),
		strip.text           = ggplot2::element_blank(),
		strip.background     = ggplot2::element_blank()
	)
}

# Add x-axis scale: initial fraction strain A
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

# Add y-axis scale: fitness (strain A, strain B, total group)
add_scale_fitness <- function(input_fig, show_intercept = TRUE) {
	output <- input_fig +
		ggplot2::aes(y = fitness) +
		ggplot2::scale_y_log10(
			name = "Wrightian fitness\n (final no. / initial no.)",
			labels = scales::label_log()
		)
	if (show_intercept) {
		output <- output +
			ggplot2::geom_hline(yintercept = 1, color = "white", linewidth = 1)
	}
	output
}

# Add y-axis scale: within-group fitness ratio A/B
add_scale_fitness_ratio <- function(
	input_fig, name_A, name_B, show_intercept = TRUE
) {
	output <- input_fig +
		ggplot2::aes(y = .data$fitness_ratio_A_B) +
		ggplot2::scale_y_log10(
			name = paste("Fitness ratio\n", name_A, "/",name_B),
			labels = scales::label_log(),
		)
	if (show_intercept) {
		output <- output +
			ggplot2::geom_hline(yintercept = 1, color = "white", linewidth = 1)
	}
	output
}

# Add points for within-group plots
add_points_within_group <- function(
	input_fig, color = gray(0.1), fill = gray(0.65)
) {
	input_fig +
	ggplot2::geom_point(shape = 21, color = color, fill = fill) +
	ggplot2::geom_point(shape = 1, color = color)
}

# Add points (generic)
add_points <- function(input_fig) {
	input_fig +
	ggplot2::geom_point(shape = 21) +
	ggplot2::geom_point(shape = 1)
}

# Color points by strain/total-group
add_scale_strain_color <- function(
	input_fig, values = c("tan4", "lightsteelblue4", gray(0.1))
) {
	input_fig +
	ggplot2::aes(color = .data$strain) +
	ggplot2::scale_color_manual(values = values)
}

# Fill points by strain/total-group
add_scale_strain_fill <- function(
	input_fig, values = c("tan", "lightsteelblue", gray(0.65))
) {
	input_fig +
	ggplot2::aes(fill = .data$strain) +
	ggplot2::scale_fill_manual(values = values)
}
