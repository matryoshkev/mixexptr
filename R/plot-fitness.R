# Plotting functions

#' Plot strain and multilevel fitness
#'
#' Show diagnostic overview of fitness effects in data. Draws a combined plot
#' of strain, total group, and relative within-group fitness against mix
#' frequency.
#'
#' @param data Wide-format data frame of fitness values. Each row must contain
#'   data for two microbes in the same population. Accepts data frame
#'   extensions like `tibble`.
#' @param var_names Named character vector identifying fitness and mixing
#'   variables in `data`. See Details. If `NULL`, defaults to column names
#'   returned by `calculate_mix_fitness()`.
#' @param mix_scale Character string or vector choosing mixing scale(s) to show
#'   on x axis. If `"fraction"`, uses `initial_fraction_A`. If `"ratio"`, uses
#'   `initial_ratio_A_B` (on \eqn{\log_{10}} scale). Defaults to show both
#'   scales.
#'
#' @details
#' Expects Wrightian fitness measures like those returned by
#' `calculate_mix_fitness()`. `var_names` must be a named vector with the
#' following elements (shown here with default values):
#' ```
#' var_names = c(
#'   name_A = "name_A",
#'   name_B = "name_B",
#'   initial_fraction_A = "initial_fraction_A",
#'   initial_ratio_A_B = "initial_ratio_A_B",
#'   fitness_A = "fitness_A",
#'   fitness_B = "fitness_B",
#'   fitness_total = "fitness_total",
#'   fitness_ratio_A_B = "fitness_ratio_A_B"
#' )
#' ```
#'
#' @export
#'
plot_mix_fitness <- function(
	data,
	var_names = NULL,
	mix_scale = c("fraction", "ratio")
) {
	# Use default variable names if not supplied
	if (is.null(var_names)) {var_names <- fitness_vars_default()}

	# Choose mix scale(s)
	mix_scale <- rlang::arg_match(
		mix_scale, c("fraction", "ratio"), multiple = TRUE
	)

	if (("fraction" %in% mix_scale) & ("ratio" %in% mix_scale)) {
		# Show both mix scales
		figA <- plot_fitness_strain_total(data, var_names, "fraction")
		figB <- plot_within_group_fitness(data, var_names, "fraction")
		figC <- plot_fitness_strain_total(data, var_names, "ratio")
		figD <- plot_within_group_fitness(data, var_names, "ratio")
		fig_output <- figA + figB + figC + figD
	} else {
		# Show one mix scale
		figA <- plot_fitness_strain_total(data, var_names, mix_scale)
		figB <- plot_within_group_fitness(data, var_names, mix_scale)
		fig_output <- figA + figB
	}

	# Size plots for page-width figure
	# (units affect plotting area, not total size)
	fig_output <- fig_output + patchwork::plot_layout(
		widths = grid::unit(c(3.2, 1.6), "inches"),
		heights = grid::unit(1.4, "inches")
	)

	fig_output
}


# Functions that will eventually be exported ===================================

# Plot relative within-group fitness (fitness_A/fitness_B)
plot_within_group_fitness <- function(
	data,
	var_names = fitness_vars_default(),
	mix_scale = "fraction"
) {
	var_names <- as.list(var_names)
	name_A <- data[[var_names$name_A]][[1]]
	name_B <- data[[var_names$name_B]][[1]]

	# Format data for plot
	data_for_plot <- format_to_plot_fitness_ratio(data, var_names, mix_scale)

	# Construct plot
	fig_output <-
		ggplot2::ggplot(data_for_plot) +
		theme_mixexptr() +
		switch(
			mix_scale,
			fraction = scale_x_initial_fraction(name_A = name_A),
			ratio = scale_x_initial_ratio(name_A = name_A, name_B = name_B)
		) +
		scale_y_fitness_ratio(name_A = name_A, name_B = name_B) +
		geom_point_mixexptr(color = color_group(), fill = fill_group()) +
		ggplot2::ggtitle("")  # Space for legend, align height

	# Return ggplot object
	fig_output
}


# Helper functions =============================================================

# Default names for fitness and mixing variables
fitness_vars_default <- function() {
	c(
		name_A = "name_A",
		name_B = "name_B",
		initial_fraction_A = "initial_fraction_A",
		initial_ratio_A_B = "initial_ratio_A_B",
		fitness_A = "fitness_A",
		fitness_B = "fitness_B",
		fitness_total = "fitness_total",
		fitness_ratio_A_B = "fitness_ratio_A_B"
	)
}

# Plot strain and total-group fitness
plot_fitness_strain_total <- function(
	data,
	var_names = fitness_vars_default(),
	mix_scale = "fraction"
) {
	var_names <- as.list(var_names)
	name_A <- data[[var_names$name_A]][[1]]
	name_B <- data[[var_names$name_B]][[1]]

	# Format to plot fitness
	data_for_plot <- format_to_plot_fitness(data, var_names, mix_scale)

	# Construct plot
	fig_output <-
		ggplot2::ggplot(data_for_plot) +
		theme_mixexptr() +
		switch(
			mix_scale,
			fraction = scale_x_initial_fraction(name_A = name_A),
			ratio = scale_x_initial_ratio(name_A = name_A, name_B = name_B)
		) +
		scale_y_fitness() +
		geom_point_mixexptr() +
		scale_color_strain() +
		scale_fill_strain() +
		ggplot2::ggtitle("") +  # Space for legend, align height
		ggplot2::facet_wrap(~ my_facet, nrow = 1)

	# Return ggplot object
	fig_output
}

# Format data to plot strain and/or total-group fitness
format_to_plot_fitness <- function(
	data,
	var_names = fitness_vars_default(),
	mix_scale
) {
	var_names <- as.list(var_names)
	name_A <- data[[var_names$name_A]][[1]]
	name_B <- data[[var_names$name_B]][[1]]
	name_total <- "Total group"

	output <- as.data.frame(data)  # So reshape() doesn't choke on tibbles
	output$initial_fraction_A <- output[[var_names$initial_fraction_A]]
	output$initial_ratio_A_B  <- output[[var_names$initial_ratio_A_B]]
	output$fitness_A          <- output[[var_names$fitness_A]]
	output$fitness_B          <- output[[var_names$fitness_B]]
	output$fitness_total      <- output[[var_names$fitness_total]]
	output <- subset(
		output,
		select = c(
			"initial_fraction_A", "initial_ratio_A_B",
			"fitness_A", "fitness_B", "fitness_total"
		)
	)
	output <- stats::reshape(
		output,
		direction = "long",
		varying = c("fitness_A", "fitness_B", "fitness_total"),
		v.names = c("fitness"),
		times = c(name_A, name_B, name_total),
		timevar = "strain"
	)
	output$strain <- factor(
		output$strain,
		levels = c(name_A, name_B, name_total)
	)
	output$my_facet <- output$strain == name_total

	# Drop rows with no value for fitness
	output <- output[!is.na(output$fitness), ]
	if (mix_scale == "ratio") {
		# Drop rows where mixing ratio undefined on log scale
		output <- output[is.finite(output$initial_ratio_A_B), ]
		output <- output[output$initial_ratio_A_B > 0, ]
	}

	output
}

# Format data to plot within-group relative fitness
format_to_plot_fitness_ratio <- function(data, var_names, mix_scale) {
	output <- as.data.frame(data)
	output$initial_fraction_A <- data[[var_names$initial_fraction_A]]
	output$initial_ratio_A_B  <- data[[var_names$initial_ratio_A_B]]
	output$fitness_ratio_A_B  <- data[[var_names$fitness_ratio_A_B]]

	# Drop rows with no value for fitness ratio
	output <- output[!is.na(output$fitness_ratio_A_B), ]
	if (mix_scale == "ratio") {
		# Drop rows where mixing ratio undefined on log scale
		output <- output[is.finite(output$initial_ratio_A_B), ]
		output <- output[output$initial_ratio_A_B > 0, ]
	}

	output
}
