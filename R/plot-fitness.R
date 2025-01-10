# Plotting functions

plot_fitness <- function() {}


# Helper functions =============================================================

# Plot strain and total-group fitness
#
plot_fitness_strain_group <- function() {}

# Reshape data to plot strain and/or total-group fitness
#
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
	output$initial_ratio_A    <- output[[var_names$initial_ratio_A]]
	output$fitness_A          <- output[[var_names$fitness_A]]
	output$fitness_B          <- output[[var_names$fitness_B]]
	output$fitness_total      <- output[[var_names$fitness_total]]
	output <- subset(
		output,
		select = c(
			"initial_fraction_A", "initial_ratio_A",
			"fitness_A", "fitness_B", "fitness_total"
		)
	)
	output <- stats::reshape(
		output,
		direction = "long",
		varying = c("fitness_A", "fitness_B", "fitness_total"),
		v.names = c("fitness"),
		times = c(name_A, name_A, name_total),
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
#
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
	data_for_plot$initial_ratio_A_B <- data[[var_names$initial_ratio_A_B]]
	data_for_plot$fitness_ratio_A_B <- data[[var_names$fitness_ratio_A_B]]
	data_for_plot <- subset(data_for_plot, !is.na(fitness_ratio_A_B))

	# Construct plot
	fig_output <- ggplot2::ggplot(data_for_plot)
	fig_output <- add_scale_initial_fraction(fig_output, name_A)
	fig_output <- add_scale_fitness_ratio(fig_output, name_A, name_B)
	fig_output <- add_points_within_group(fig_output)

	# Return ggplot object
	fig_output
}

