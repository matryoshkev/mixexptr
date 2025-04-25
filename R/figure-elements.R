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
			breaks = breaks_log10,
			minor_breaks = minor_breaks,
			labels = labels_log10
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
			breaks = breaks_log10,
			labels = labels_log10
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
			breaks = breaks_log10,
			labels = labels_log10
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
			breaks = breaks_log10,
			labels = labels_log10
		)
	)
	scale
}

# Breaks for log10 axes
#   Limits assumed to always include 1, minimum 10-fold range
breaks_log10 <- function(limits) {
	limits_range <- suppressWarnings(log10(range(limits, na.rm = TRUE)))
	span <- limits_range[2] - limits_range[1]
	breaks <- case_when(
		span < 1.47 ~ c(0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50),
		span < 3 ~ c(0.003, 0.01, 0.03, 0.1, 0.3, 1, 3, 10, 30, 100, 300),
		span < 6 ~ 10^seq(-5, 5, by = 1),
		span < 9 ~ 10^seq(-10, 10, by = 2),
		span < 12 ~ 10^seq(-15, 15, by = 3),
		TRUE ~ 10^seq(-20, 20, by = 4)
	)
	breaks
}

# Labels for log10 axes
labels_log10 <- function(breaks) {
		if (max(abs(log10(breaks)), na.rm = TRUE) >= 3) {
			# 10^n notation
			labels <- paste0(10, "^", log10(breaks))
			labels[labels == "10^0"] <- "1"
			labels <- ggplot2:::parse_safe(labels)
		} else {
			# Clean decimal/integer
			labels <- scales::number(breaks, drop0trailing = TRUE)
		}
		labels
}

# Minor breaks for log10 axes
# minor_breaks_log10 <- function() {}

# Calculate limits for log10 axes from data
limits_log10 <- function(values) {
  values <- values[is.finite(values) & values > 0]
  values <- c(values, 1)  # Always include 1
	log10_range <- log10(range(values))
 	midpoint <- mean(log10_range)
	span <- log10_range[2] - log10_range[1]
	span <- max(span, 1)  # Minimum 10-fold range
	span <- span * 1.1  # 5% expansion to either side
	min <- 10^(midpoint - span/2)
	max <- 10^(midpoint + span/2)
	c(min, max)
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

