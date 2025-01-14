# Plotting functions

#' Plot strain and multilevel fitness
#'
#' Draw a combined plot of strain, total group, and relative within-group
#' fitness against mix frequency. Useful as a quick diagnostic for fitness
#' effects in data.
#'
#' @param data Data frame of fitness values. Each row must contain data for two
#'   microbes in the same population. Accepts data frame extensions like
#'   `tibble`.
#' @param var_names Named character vector identifying which fitness measures
#'   are contained in `data` variables. If `NULL`, defaults to column names
#'   returned by `calculate_mix_fitness().` See Details.
#' @param mix_scale Determines x-axis scale. `"fraction"` uses
#'   `initial_fraction_A`. `"ratio"` uses `initial_ratio_A_B` (on
#'   \eqn{\log_{10}} scale).
#'
#' @details
#' `var_names` must be a named vector with the following elements (shown with
#' default values):
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
	mix_scale = "fraction"
) {
	if (is.null(var_names)) {
		var_names <- fitness_vars_default()
	}
	figA <- plot_fitness_strain_total(data, var_names, mix_scale)
	figB <- plot_within_group_fitness(data, var_names, mix_scale)
	fig_output <-
		figA + figB +
		patchwork::plot_layout(
			widths = grid::unit(c(3.2, 1.6), "inches"),
			heights = grid::unit(1.4, "inches")
			# Units affect plotting area, not total size
		)
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
	data_for_plot <- format_to_plot_fitness(data, var_names)
	if (mix_scale == "ratio") {
		data_for_plot <- subset(data_for_plot,
			is.finite(initial_ratio_A_B) & initial_ratio_A_B > 0
		)
	}

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

# Reshape data to plot strain and/or total-group fitness
format_to_plot_fitness <- function(
	data,
	var_names = fitness_vars_default()
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
	output <- subset(output, !is.na(fitness))
	output$strain <- factor(
		output$strain,
		levels = c(name_A, name_B, name_total)
	)
	output$my_facet <- output$strain == name_total

	output
}

# Plot relative within-group fitness (fitness_A/fitness_B)
#   Will eventually be user-facing
plot_within_group_fitness <- function(
	data,
	var_names = fitness_vars_default(),
	mix_scale = "fraction"
) {
	var_names <- as.list(var_names)
	name_A <- data[[var_names$name_A]][[1]]
	name_B <- data[[var_names$name_B]][[1]]

	# Format to plot fitness ratio
	data_for_plot <- as.data.frame(data)
	data_for_plot$initial_fraction_A <- data[[var_names$initial_fraction_A]]
	data_for_plot$initial_ratio_A_B  <- data[[var_names$initial_ratio_A_B]]
	data_for_plot$fitness_ratio_A_B  <- data[[var_names$fitness_ratio_A_B]]
	data_for_plot <- subset(data_for_plot, !is.na(fitness_ratio_A_B))
	if (mix_scale == "ratio") {
		data_for_plot <- subset(data_for_plot,
			is.finite(initial_ratio_A_B) & initial_ratio_A_B > 0
		)
	}

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
