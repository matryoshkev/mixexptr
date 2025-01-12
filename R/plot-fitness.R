# Plotting functions

#' Plot strain and multilevel fitness
#'
#' Draw a combined plot of strain, total group, and relative within-group
#' fitness against mix frequency.
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
		var_names <- c(
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
	figA <- plot_fitness_strain_total(data, var_names, mix_scale)
	figB <- plot_within_group_fitness(data, var_names, mix_scale)
	fig_output <- combine_figures(figA, figB, widths = c(14, 8))
	grid::grid.draw(fig_output)
	return(invisible(fig_output))
}


# Helper functions =============================================================

# Plot strain and total-group fitness
plot_fitness_strain_total <- function(
	data,
	var_names = c(
		name_A = "name_A",
		name_B = "name_B",
		initial_fraction_A = "initial_fraction_A",
		initial_ratio_A_B = "initial_ratio_A_B",
		fitness_A = "fitness_A",
		fitness_B = "fitness_B",
		fitness_total = "fitness_total",
		fitness_ratio_A_B = "fitness_ratio_A_B"
	),
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
		ggplot2::ggtitle("")  # Space for legend, align height with other plots

	if (mix_scale == "fraction") {
		fig_output <- add_scale_initial_fraction(fig_output, name_A)
	} else if (mix_scale == "ratio") {
		fig_output <- add_scale_initial_ratio(fig_output, name_A, name_B)
	}

	fig_output <- add_scale_fitness(fig_output)
	fig_output <- add_points(fig_output)
	fig_output <- add_scale_strain_color(fig_output)
	fig_output <- add_scale_strain_fill(fig_output)
	fig_output <- fig_output + ggplot2::facet_wrap(~ my_facet, nrow = 1)

	# Return ggplot object
	fig_output
}

# Reshape data to plot strain and/or total-group fitness
format_to_plot_fitness <- function(
	data,
	var_names = c(
		name_A = "name_A",
		name_B = "name_B",
		initial_fraction_A = "initial_fraction_A",
		initial_ratio_A_B = "initial_ratio_A_B",
		fitness_A = "fitness_A",
		fitness_B = "fitness_B",
		fitness_total = "fitness_total",
		fitness_ratio_A_B = "fitness_ratio_A_B"
	)
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
	var_names = c(
		name_A = "name_A",
		name_B = "name_B",
		initial_fraction_A = "initial_fraction_A",
		initial_ratio_A_B = "initial_ratio_A_B",
		fitness_A = "fitness_A",
		fitness_B = "fitness_B",
		fitness_total = "fitness_total",
		fitness_ratio_A_B = "fitness_ratio_A_B"
	),
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
		ggplot2::ggtitle("")  # Space for legend, align height with other plots
	if (mix_scale == "fraction") {
		fig_output <- add_scale_initial_fraction(fig_output, name_A)
	} else if (mix_scale == "ratio") {
		fig_output <- add_scale_initial_ratio(fig_output, name_A, name_B)
	}
	# fig_output <- add_scale_initial_fraction(fig_output, name_A)
	fig_output <- add_scale_fitness_ratio(fig_output, name_A, name_B)
	fig_output <- add_points_within_group(fig_output)

	# Return ggplot object
	fig_output
}

# Combine subfigures
#   Deprecated: Maybe try `patchwork` package
combine_figures <- function(fig1, fig2, widths = c(1, 1)) {
	width_1 <- widths[[1]]
	width_2 <- widths[[2]]
	# grDevices::pdf(file = NULL)
		# So ggplotGrob() doesn't create blank plot window
		# see https://github.com/tidyverse/ggplot2/issues/809
	fig1 <- ggplot2::ggplotGrob(fig1)
	fig2 <- ggplot2::ggplotGrob(fig2)
	fig_output <- gtable::gtable(
		widths  = grid::unit(rep(1, width_1 + width_2), "null"),
		heights = grid::unit(rep(1, 1), "null")
	)
	fig_output <- gtable::gtable_add_grob(
		fig_output,
		grobs = list(fig1, fig2),
		l = c(1, width_1 + 1), # Left extents
		r = c(width_1, width_1 + width_2),  # Right extents
		t = c(1, 1), # Top extents
		b = c(1, 1)  # Bottom extents
	)
	# grDevices::dev.off()  # end of workaround
	fig_output
}
