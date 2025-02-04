# Figure elements

# Theme ========================================================================

# Default package theme (suited to figures in papers)
theme_mixexptr <- function() {
	ggplot2::theme_grey() +
	ggplot2::theme(
		text = ggplot2::element_text(size = 9)
	)
}

# Additional options for plot_mix_fitness() (less clutter)
theme_plot_mix_fitness <- function() {
	ggplot2::theme(
		legend.title         = ggplot2::element_blank(),
		legend.background    = ggplot2::element_blank(),
		legend.direction     = "horizontal",
		legend.justification = c(0.5, 0.15),
		legend.position      = c(0.5, 1),
		strip.text           = ggplot2::element_blank(),
		strip.background     = ggplot2::element_blank()
	)
}


# Axes =========================================================================

# Add x-axis scale: initial fraction strain A
scale_x_initial_fraction <- function(
	var_names,
	strain_names,
	...,
	xlab = NULL,
	xlim = NULL,
	breaks = ggplot2::waiver(),
	minor_breaks = ggplot2::waiver()
) {
	if (is.null(xlab)) {
		xlab <- paste("Initial fraction", strain_names$A)
	}
	if (is.null(xlim)) {
		xlim <- c(0, 1)
		breaks <- seq(0, 1, by = 0.2)
		minor_breaks <- NULL
	}
	list(
		ggplot2::aes(x = .data[[var_names$initial_fraction_A]]),
		ggplot2::scale_x_continuous(
			name = xlab,
			limits = xlim,
			breaks = breaks,
			minor_breaks = minor_breaks
		)
	)
}

# Add x-axis scale: initial ratio A/B (log10)
scale_x_initial_ratio <- function(
	var_names,
	strain_names,
	...,
	xlab = NULL,
	xlim = NULL,
	breaks = ggplot2::waiver(),
	minor_breaks = ggplot2::waiver()
) {
	if (is.null(xlab)) {
		xlab <- paste("Initial ratio", strain_names$A, "/", strain_names$B)
	}
	if (is.null(xlim)) {
		breaks <- 10^c(-12:12)
		minor_breaks <- NULL
	}
	scale <- list(
		ggplot2::aes(x = .data[[var_names$initial_ratio_A_B]]),
		ggplot2::scale_x_log10(
			name = xlab,
			limits = xlim,
			breaks = breaks,
			minor_breaks = minor_breaks,
			labels = scales::label_log()
		)
	)
	scale
}

# Add y-axis scale: fitness (strain A, strain B, total group)
scale_y_fitness <- function(var_names, ..., ylab = NULL, ylim = NULL) {
	var_names <- as.list(var_names)
	if (is.null(ylab)) {
		ylab <- "Wrightian fitness\n (final no. / initial no.)"
	}
	scale <- list(
		ggplot2::aes(y = .data[[var_names$fitness]]),
		ggplot2::scale_y_log10(
			name = ylab,
			limits = ylim,
			labels = scales::label_log()
		)
	)
	scale
}

# Add y-axis scale: total-group fitness
scale_y_fitness_total <- function(var_names, ..., ylab = NULL, ylim = NULL) {
	var_names <- as.list(var_names)
	if (is.null(ylab)) {
		ylab <- "Total group fitness\n(final no. / initial no.)"
	}
	scale <- list(
		ggplot2::aes(y = .data[[var_names$fitness_total]]),
		ggplot2::scale_y_log10(
			name = ylab,
			limits = ylim,
			labels = scales::label_log()
		)
	)
	scale
}

# Add y-axis scale: within-group fitness ratio A/B
scale_y_fitness_ratio <- function(
	var_names, strain_names, ..., ylab = NULL, ylim = NULL
) {
	var_names <- as.list(var_names)
	if (is.null(ylab)) {
		ylab <- paste("Fitness ratio\n", strain_names$A, "/", strain_names$B)
	}
	scale <- list(
		ggplot2::aes(y = .data[[var_names$fitness_ratio_A_B]]),
		ggplot2::scale_y_log10(
			name = ylab,
			limits = ylim,
			labels = scales::label_log())
		)
	scale
}


# Other aesthetics =============================================================

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
color_group    <- function() "black"

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
fill_strain_A <- function() "tan"
fill_strain_B <- function() "lightsteelblue"
fill_group    <- function() "gray65"

# Default fill scale for mixed groups. Replaced if user specifies fill.
scale_fill_group <- function() {
	list(
		ggplot2::aes(fill = TRUE),
		ggplot2::scale_fill_manual(values = fill_group(), guide = "none")
	)
}

# Default shape scale for points. Replaced if user specifies shape.
# scale_shape_mixexptr <- function() {
# 	list(
# 		ggplot2::aes(shape = TRUE),
# 		ggplot2::scale_shape_manual(values = 21, guide = "none")
# 	)
# }


# Geom =========================================================================

# Default points that are more readable when overlapped
geom_point_mixexptr <- function(shape = 21, ..., na.rm = TRUE) {
	list(
		ggplot2::geom_point(shape = shape, ..., na.rm = na.rm),
		ggplot2::geom_point(shape = shape, fill = NA, ..., na.rm = na.rm)
	)
}

