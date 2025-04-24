# Plot fitness measures ========================================================

#' Plot strain and multilevel fitness
#'
#' `plot_mix_fitness()` shows a diagnostic overview of various fitness effects in
#' a dataset. Draws a combined plot of strain, total group, and relative
#' within-group fitness against mix frequency.
#'
#' @param data Data frame of fitness values. Wide format: each row must contain
#'   data for two microbes in the same population. Accepts data frame
#'   extensions like `tibble`.
#' @param var_names Named character vector identifying fitness and mixing
#'   variables in `data`. If `NULL`, defaults to column names returned by
#'   [calculate_mix_fitness()]. See Details.
#' @param mix_scale Determines mixing scale for x axis. When `"fraction"`, uses
#'   initial frequency of strain A (proportion of total) from the
#'   `initial_fraction_A` variable. When `"ratio"`, uses ratio of strain A
#'   to strain B (on \eqn{\log_{10}} scale) from the `initial_ratio_A_B`
#'   variable. Defaults to show both.
#'
#' @details
#' Expects Wrightian fitness measures like those returned by
#' [calculate_mix_fitness()].
#'
#' `var_names` must be a named vector or list that includes the following
#' elements (shown here with default values):
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
#' @seealso [calculate_mix_fitness()]
#'
#' @examples
#' library(patchwork)
#'
#' # Using data from smith et al (2010)
#' fitness_smith_2010 <- calculate_mix_fitness(
#'   data_smith_2010,
#'   var_names = c(
#'     initial_number_A = "initial_cells_evolved",
#'     initial_number_B = "initial_cells_ancestral",
#'     final_number_A = "final_spores_evolved",
#'     final_number_B = "final_spores_ancestral",
#'     name_A = "GVB206.3",
#'     name_B = "GJV10"
#'   )
#' )
#' plot_mix_fitness(fitness_smith_2010)
#'
#' @export
#'
plot_mix_fitness <- function(
	data, var_names = NULL, mix_scale = c("fraction", "ratio")
) {
	# Variable names
	if (is.null(var_names)) {var_names <- fitness_vars_default()}

	# Scale options
	mix_scale <- rlang::arg_match(
		mix_scale, c("fraction", "ratio"), multiple = TRUE
	)
	ylim <- get_ylim_mix_fitness(data, var_names)

	# Make subplots
	if (("fraction" %in% mix_scale) & ("ratio" %in% mix_scale)) {
		# Show both mix scales
		figA <- plot_fitness_strain_total(
			data, var_names, mix_scale = "fraction", ylim = ylim$fitness
		)
		figB <- plot_within_group_fitness(
			data, var_names, mix_scale = "fraction", ylim = ylim$fitness_ratio
		)
		figC <- plot_fitness_strain_total(
			data, var_names, mix_scale = "ratio", ylim = ylim$fitness
		)
		figD <- plot_within_group_fitness(
			data, var_names, mix_scale = "ratio", ylim = ylim$fitness_ratio
		)
		fig_output <- figA + figB + figC + figD
	} else {
		# Show one mix scale
		figA <- plot_fitness_strain_total(
			data, var_names, mix_scale, ylim = ylim$fitness
		)
		figB <- plot_within_group_fitness(
			data, var_names, mix_scale, ylim = ylim$fitness_ratio
		)
		fig_output <- figA + figB
	}

	# Size plots for page-width figure
	fig_output <- fig_output + patchwork::plot_layout(
		widths = grid::unit(c(3.2, 1.6), "inches"),
		heights = grid::unit(1.4, "inches")
		# Units affect plotting area, not total size
	)

	fig_output
}

#' Plot strain fitness
#'
#' `plot_strain_fitness()` draws a plot of absolute fitness for each of two
#' microbe strains as a function of their initial frequency
#'
#' @param data Data frame of fitness values and mix frequencies. Wide format:
#'   each row must contain data for two microbes in the same population.
#'   Accepts data frame extensions like `tibble`.
#' @param var_names Named character vector identifying fitness and mixing
#'   variables in `data`. If `NULL`, defaults to column names returned by
#'   [calculate_mix_fitness()]. See Details.
#' @param mix_scale Determines mixing scale for x axis. When `"fraction"`
#'   (the default), uses initial frequency of strain A (proportion of total)
#'   from `initial_fraction_A` variable. When `"ratio"`, uses ratio of strain A
#'   to strain B (on \eqn{\log_{10}} scale) from `initial_ratio_A_B` variable.
#' @param xlab,ylab Optional string to replace default axis label
#' @param xlim,ylim Optional axis limits to replace default
#'
#' @details
#' Expects Wrightian fitness measures like those returned by
#' [calculate_mix_fitness()].
#'
#' `var_names` must be a named vector or list that includes the following
#' elements (shown here with default values):
#' ```
#' var_names = c(
#'   name_A = "name_A",
#'   name_B = "name_B",
#'   initial_fraction_A = "initial_fraction_A",
#'   initial_ratio_A_B = "initial_ratio_A_B",
#'   fitness_A = "fitness_A",
#'   fitness_B = "fitness_B"
#' )
#' ```
#'
#' @return `ggplot` object that can be modified further
#'
#' @seealso [plot_total_group_fitness()], [plot_within_group_fitness()]
#'
#' @examples
#' # Using data from smith et al (2010)
#' fitness_smith_2010 <- calculate_mix_fitness(
#'   data_smith_2010,
#'   var_names = c(
#'     initial_number_A = "initial_cells_evolved",
#'     initial_number_B = "initial_cells_ancestral",
#'     final_number_A = "final_spores_evolved",
#'     final_number_B = "final_spores_ancestral",
#'     name_A = "GVB206.3",
#'     name_B = "GJV10"
#'   )
#' )
#' plot_strain_fitness(fitness_smith_2010)
#'
#' @export
#'
plot_strain_fitness <- function(
	data,
	var_names = NULL,
	mix_scale = "fraction",
	xlab = NULL,
	ylab = NULL,
	xlim = c(NA, NA),
	ylim = c(NA, NA)
) {
	# Variable names
	if (is.null(var_names)) {var_names <- fitness_vars_default()}
	var_names <- as.list(var_names)
	var_names$fitness <- "fitness"
	strain_names <- get_strain_names(data, var_names)

	# Scale options
	mix_scale <- rlang::arg_match(mix_scale, c("fraction", "ratio"))
	if (missing(xlim)) { xlim <- NULL }
	if (missing(ylim)) { ylim <- NULL }

	# Long-format data
	data_to_plot <- stats::reshape(
		as.data.frame(data),  # reshape() chokes on tibbles
		direction = "long",
		varying = c(var_names$fitness_A, var_names$fitness_B),
		v.names = "fitness",
		timevar = "strain",
		times = c(strain_names$A, strain_names$B)
	)

	if (is.null(ylim)) { ylim <- limits_log10(data_to_plot$fitness) }

	# Construct plot
	fig_output <-
		ggplot2::ggplot(data_to_plot) +
		theme_mixexptr() +
		switch(
			mix_scale,
			fraction = scale_x_initial_fraction(
				var_names, strain_names, xlab = xlab, xlim = xlim
			),
			ratio = scale_x_initial_ratio(
				var_names, strain_names, xlab = xlab, xlim = xlim
			)
		) +
		scale_y_fitness(var_names, ylab = ylab, ylim = ylim) +
		geom_point_mixexptr() +
		scale_fill_strain() +
		scale_color_strain() +
		ggplot2::ggtitle("")  # Space for legend, align height

	# Expand limits to include log-intercepts
	if (is.null(xlim) & mix_scale == "ratio")
		fig_output <- fig_output + ggplot2::expand_limits(x = 1)
	if (is.null(ylim))
		fig_output <- fig_output + ggplot2::expand_limits(y = 1)

	# Return ggplot object
	fig_output
}

#' Plot total group fitness
#'
#' `plot_total_group_fitness()` draws a plot of total-group fitness as a
#' function of initial strain frequency
#'
#' @inheritParams plot_strain_fitness
#' @param data Data frame of fitness values and mix frequencies.
#'   Accepts data frame extensions like `tibble`.
#'
#' @details
#' Expects Wrightian fitness measures like those returned by
#' [calculate_mix_fitness()].
#'
#' `var_names` must be a named vector or list that includes the following
#' elements (shown here with default values):
#' ```
#' var_names = c(
#'   name_A = "name_A",
#'   name_B = "name_B",
#'   initial_fraction_A = "initial_fraction_A",
#'   initial_ratio_A_B = "initial_ratio_A_B",
#'   fitness_total = "fitness_total"
#' )
#' ```
#'
#' @return `ggplot` object that can be modified further
#'
#' @seealso [plot_within_group_fitness()], [plot_strain_fitness()]
#'
#' @examples
#' # Using data from smith et al (2010)
#' fitness_smith_2010 <- calculate_mix_fitness(
#'   data_smith_2010,
#'   var_names = c(
#'     initial_number_A = "initial_cells_evolved",
#'     initial_number_B = "initial_cells_ancestral",
#'     final_number_A = "final_spores_evolved",
#'     final_number_B = "final_spores_ancestral",
#'     name_A = "GVB206.3",
#'     name_B = "GJV10"
#'   )
#' )
#' plot_total_group_fitness(fitness_smith_2010)
#'
#' @export
#'
plot_total_group_fitness <- function(
	data,
	var_names = NULL,
	mix_scale = "fraction",
	xlab = NULL,
	ylab = NULL,
	xlim = c(NA, NA),
	ylim = c(NA, NA)
) {
	# Variable names
	if (is.null(var_names)) {var_names <- fitness_vars_default()}
	var_names <- as.list(var_names)
	strain_names <- get_strain_names(data, var_names)

	# Scale options
	mix_scale <- rlang::arg_match(mix_scale, c("fraction", "ratio"))
	if (missing(xlim)) { xlim <- NULL }
	if (missing(ylim)) { ylim <- NULL }
	if (is.null(ylim)) { ylim <- limits_log10(data[[var_names$fitness_total]]) }

	# Construct plot
	fig_output <-
		ggplot2::ggplot(data) +
		theme_mixexptr() +
		switch(
			mix_scale,
			fraction = scale_x_initial_fraction(
				var_names, strain_names, xlab = xlab, xlim = xlim
			),
			ratio = scale_x_initial_ratio(
				var_names, strain_names, xlab = xlab, xlim = xlim
			)
		) +
		scale_y_fitness_total(var_names, ylab = ylab, ylim = ylim) +
		geom_point_mixexptr() +
		scale_fill_group() +
		ggplot2::ggtitle("")  # Space for legend, align height

	# Expand limits to include log-intercepts
	if (is.null(xlim) & mix_scale == "ratio") {
		fig_output <- fig_output + ggplot2::expand_limits(x = 1)
	}
	if (is.null(ylim)) {
		fig_output <- fig_output + ggplot2::expand_limits(y = 1)
	}

	# Return ggplot object
	fig_output
}

#' Plot within-group fitness ratio
#'
#' `plot_within_group_fitness()` draws a plot of the relative within-group
#' fitness of strains A and B as a function of their initial frequency
#'
#' @inheritParams plot_strain_fitness
#' @param data Data frame of fitness values and mix frequencies.
#'   Accepts data frame extensions like `tibble`.
#'
#' @details
#' Expects Wrightian fitness measures like those returned by
#' [calculate_mix_fitness()]. Relative within-group fitness is measured as
#' the ratio of strain A fitness to strain B fitness.
#'
#' `var_names` must be a named vector or list that includes the following
#' elements (shown here with default values):
#' ```
#' var_names = c(
#'   name_A = "name_A",
#'   name_B = "name_B",
#'   initial_fraction_A = "initial_fraction_A",
#'   initial_ratio_A_B = "initial_ratio_A_B",
#'   fitness_ratio_A_B = "fitness_ratio_A_B"
#' )
#' ```
#'
#' @return `ggplot` object that can be modified further
#'
#' @seealso [plot_total_group_fitness()], [plot_strain_fitness()]
#'
#' @examples
#' # Using data from smith et al (2010)
#' fitness_smith_2010 <- calculate_mix_fitness(
#'   data_smith_2010,
#'   var_names = c(
#'     initial_number_A = "initial_cells_evolved",
#'     initial_number_B = "initial_cells_ancestral",
#'     final_number_A = "final_spores_evolved",
#'     final_number_B = "final_spores_ancestral",
#'     name_A = "GVB206.3",
#'     name_B = "GJV10"
#'   )
#' )
#' plot_within_group_fitness(fitness_smith_2010, mix_scale = "ratio")
#'
#' @export
#'
plot_within_group_fitness <- function(
	data,
	var_names = NULL,
	mix_scale = "fraction",
	xlab = NULL,
	ylab = NULL,
	xlim = c(NA, NA),
	ylim = c(NA, NA)
) {
	# Variable names
	if (is.null(var_names)) {var_names <- fitness_vars_default()}
	var_names <- as.list(var_names)
	strain_names <- get_strain_names(data, var_names)

	# Scale options
	mix_scale <- rlang::arg_match(mix_scale, c("fraction", "ratio"))
	if (missing(xlim)) { xlim <- NULL }
	if (missing(ylim)) { ylim <- NULL }
	if (is.null(ylim)) {
		ylim <- limits_log10(data[[var_names$fitness_ratio_A_B]])
	}

	# Construct plot
	fig_output <-
		ggplot2::ggplot(data) +
		theme_mixexptr() +
		switch(
			mix_scale,
			fraction = scale_x_initial_fraction(
				var_names, strain_names, xlab = xlab, xlim = xlim
			),
			ratio = scale_x_initial_ratio(
				var_names, strain_names, xlab = xlab, xlim = xlim
			)
		) +
		scale_y_fitness_ratio(
			var_names, strain_names, ylab = ylab, ylim = ylim
		) +
		geom_point_mixexptr() +
		scale_fill_group() +
		ggplot2::ggtitle("")  # Space for legend, align height

	# Expand limits to include log-intercepts
	if (is.null(xlim) & mix_scale == "ratio") {
		fig_output <- fig_output + ggplot2::expand_limits(x = 1)
	}
	if (is.null(ylim)) {
		fig_output <- fig_output + ggplot2::expand_limits(y = 1)
	}

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
		fitness = "fitness",
		fitness_A = "fitness_A",
		fitness_B = "fitness_B",
		fitness_total = "fitness_total",
		fitness_ratio_A_B = "fitness_ratio_A_B"
	)
}

# Plot strain and total-group fitness (for plot_mix_fitness())
plot_fitness_strain_total <- function(
	data,
	var_names = fitness_vars_default(),
	mix_scale = "fraction",
	ylim = NULL
) {
	# Variable names
	var_names <- as.list(var_names)
	var_names$fitness <- "fitness"
	strain_names <- get_strain_names(data, var_names)
	name_total <- "Total group"

	# Scale options
	mix_scale <- rlang::arg_match(mix_scale, c("fraction", "ratio"))

	# Long-format data
	# data_for_plot <- format_to_plot_fitness(data, var_names, mix_scale)
	data_for_plot <- stats::reshape(
		as.data.frame(data),
		direction = "long",
		varying = c(
			var_names$fitness_A, var_names$fitness_B, var_names$fitness_total
		),
		v.names = "fitness",
		timevar = "strain",
		times = c(strain_names$A, strain_names$B, name_total)
	)
	data_for_plot$strain <- factor(
		data_for_plot$strain,
		levels = c(strain_names$A, strain_names$B, name_total)
	)
	data_for_plot$my_facet <- data_for_plot$strain == name_total

	# Construct plot
	fig_output <-
		ggplot2::ggplot(data_for_plot) +
		theme_mixexptr() +
		theme_plot_mix_fitness() +
		switch(
			mix_scale,
			fraction = scale_x_initial_fraction(var_names, strain_names),
			ratio = scale_x_initial_ratio(var_names, strain_names)
		) +
		scale_y_fitness(var_names, ylim = ylim) +
		geom_point_mixexptr() +
		scale_color_strain() +
		scale_fill_strain() +
		ggplot2::ggtitle("") +  # Space for legend, align height
		ggplot2::facet_wrap(~ my_facet, nrow = 1)

	# Expand limits to include log-intercepts
	fig_output <- fig_output +
		ggplot2::expand_limits(x = 1) +
		ggplot2::expand_limits(y = 1)

	# Return ggplot object
	fig_output
}

# Get strain names from fitness vars or data
get_strain_names <- function(data, var_names) {
	name_A <- var_names[["name_A"]]
	name_B <- var_names[["name_B"]]
	if (utils::hasName(data, name_A)) { name_A <- data[[name_A]][[1]] }
	if (utils::hasName(data, name_B)) { name_B <- data[[name_B]][[1]] }
	list(A = name_A, B = name_B)
}

# Get y-axis limits for fitness & fitness_ratio
#   so log10(fitness) and log10(fitness_ratio) are visually comparable
get_ylim_mix_fitness <- function(data, var_names) {
	var_names <- as.list(var_names)

	# Range: strain & total-group fitness
	fitness <- c(
		data[[var_names$fitness_A]],
		data[[var_names$fitness_B]],
		data[[var_names$fitness_total]],
		1
	)
	fitness <- fitness[is.finite(fitness) & fitness > 0]
	span_fitness <- log10(range(fitness)[2] / range(fitness)[1])

	# Range: Within-group fitness ratio
	fitness_ratio <- c(data[[var_names$fitness_ratio_A_B]], 1)
	fitness_ratio <- fitness_ratio[is.finite(fitness_ratio) & fitness_ratio > 0]
	span_fitness_ratio <- log10(range(fitness_ratio)[2] / range(fitness_ratio)[1])

	# Shared range (10-fold minimum)
	span_shared <- max(span_fitness, span_fitness_ratio, 1)
	span_shared <- span_shared + span_shared * 0.1

	# Limits
	midpoint_fitness <- mean(log10(range(fitness)))
	midpoint_fitness_ratio <- mean(log10(range(fitness_ratio)))
	ylim_fitness <- c(
		10^(midpoint_fitness - span_shared/2),
		10^(midpoint_fitness + span_shared/2)
	)
	ylim_fitness_ratio <- c(
		10^(midpoint_fitness_ratio - span_shared/2),
		10^(midpoint_fitness_ratio + span_shared/2)
	)

	list(fitness = ylim_fitness, fitness_ratio = ylim_fitness_ratio)
}

# Format data to plot strain and/or total-group fitness
# format_to_plot_fitness <- function(
# 	data,
# 	var_names = fitness_vars_default(),
# 	mix_scale
# ) {
# 	var_names <- as.list(var_names)
# 	# name_A <- data[[var_names$name_A]][[1]]
# 	# name_B <- data[[var_names$name_B]][[1]]
# 	name_A <- var_names$name_A[[1]]
# 	name_B <- var_names$name_B[[1]]
# 	name_total <- "Total group"
#
# 	output <- as.data.frame(data)  # So reshape() doesn't choke on tibbles
# 	output$initial_fraction_A <- output[[var_names$initial_fraction_A]]
# 	output$initial_ratio_A_B  <- output[[var_names$initial_ratio_A_B]]
# 	output$fitness_A          <- output[[var_names$fitness_A]]
# 	output$fitness_B          <- output[[var_names$fitness_B]]
# 	output$fitness_total      <- output[[var_names$fitness_total]]
# 	output <- subset(
# 		output,
# 		select = c(
# 			"initial_fraction_A", "initial_ratio_A_B",
# 			"fitness_A", "fitness_B", "fitness_total"
# 		)
# 	)
# 	output <- stats::reshape(
# 		output,
# 		direction = "long",
# 		varying = c("fitness_A", "fitness_B", "fitness_total"),
# 		v.names = c("fitness"),
# 		times = c(name_A, name_B, name_total),
# 		timevar = "strain"
# 	)
# 	output$strain <- factor(
# 		output$strain,
# 		levels = c(name_A, name_B, name_total)
# 	)
# 	output$my_facet <- output$strain == name_total
#
# 	# Drop rows with no value for fitness
# 	if (mix_scale == "ratio") {
# 		# Drop rows where mixing ratio undefined on log scale
# 		output <- output[is.finite(output$initial_fraction_A), ]
# 		output <- output[output$initial_ratio_A_B > 0, ]
# 		# output <- output[is.finite(output[[var_initial_ratio]]), ]
# 		# output <- output[output[[var_initial_ratio]] > 0, ]
# 	}
#
# 	output
# }

# Format data to plot within-group fitness ratio
# format_to_plot_fitness_ratio <- function(data, var_names, mix_scale) {
# 	output <- data
#
# 	if (mix_scale == "ratio") {
# 		# Drop rows where mixing ratio undefined on log scale
# 		var_initial_ratio <- var_names$initial_ratio_A_B
# 		output <- output[is.finite(output[[var_initial_ratio]]), ]
# 		output <- output[output[[var_initial_ratio]] > 0, ]
# 	}
#
# 	output
# }
