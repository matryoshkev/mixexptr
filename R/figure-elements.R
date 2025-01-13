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

# Add x-axis scale: initial ratio A/B (log10)
add_scale_initial_ratio <- function(
	input_fig, name_A, name_B, show_intercept = TRUE
) {
	breaks <- 10^c(-10:10)
	output <- input_fig +
		ggplot2::aes(x = .data$initial_ratio_A_B) +
		ggplot2::scale_x_log10(
			name = paste("Initial ratio\n", name_A, "/", name_B),
			breaks = breaks,
			minor_breaks = breaks,
			labels = scales::label_log()
		)
	if (show_intercept) {
		output <- output +
			ggplot2::geom_vline(xintercept = 1, color = "white", linewidth = 1)
	}
	output
}

# Add y-axis scale: fitness (strain A, strain B, total group)
scale_y_fitness <- function(..., show_intercept = TRUE) {
	scale <- list(
		ggplot2::aes(y = .data$fitness),
		ggplot2::scale_y_log10(
			name = "Wrightian fitness\n (final no. / initial no.)",
			labels = scales::label_log()
		)
	)
	if (show_intercept) {
		scale <- c(scale, list(
			ggplot2::geom_hline(yintercept = 1, color = "white", linewidth = 1)
		))
	}
	scale
}

# Add y-axis scale: within-group fitness ratio A/B
scale_y_fitness_ratio <- function(
	name_A, name_B, ..., show_intercept = TRUE
) {
	scale <- list(
		ggplot2::aes(y = .data$fitness_ratio_A_B),
		ggplot2::scale_y_log10(
			name = paste("Fitness ratio\n", name_A, "/",name_B),
			labels = scales::label_log()
		)
	)
	if (show_intercept) {
		scale <- c(scale, list(
			ggplot2::geom_hline(yintercept = 1, color = "white", linewidth = 1)
		))
	}
	scale
}

# Add points that are readable when overlapped
geom_point_mixexptr <- function(shape = 21, ...) {
	suppressWarnings(  # So ggplot2 doesn't warn if fill supplied
		list(
			ggplot2::geom_point(shape = shape, ...),
			ggplot2::geom_point(shape = shape, fill = NA, ...)
		)
	)
}

# Color points by strain/total-group
scale_color_strain <- function(
	values = c(color_strain_A(), color_strain_B(), color_group()),
	...
) {
	list(
		ggplot2::aes(color = .data$strain),
		ggplot2::scale_color_manual(values = values, ...)
	)
}
color_strain_A <- function() "tan4"
color_strain_B <- function() "lightsteelblue4"
color_group    <- function() gray(0)

# Fill points by strain/total-group
scale_fill_strain <- function(
	values = c(fill_strain_A(), fill_strain_B(), fill_group()),
	...
) {
	list(
		ggplot2::aes(fill = .data$strain),
		ggplot2::scale_fill_manual(values = values, ...)
	)
}
fill_strain_A  <- function() "tan"
fill_strain_B  <- function() "lightsteelblue"
fill_group     <- function() gray(0.65)
