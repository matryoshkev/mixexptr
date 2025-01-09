#' Calculate fitness measures
#'
#' `calculate_mix_fitness()` calculates several measures of microbial fitness
#' given a data frame with the observed abundances of two microbes before and
#' after the experiment.
#'
#' Describe Wrightian fitness. Describe fitness outcomes.
#' Brief motivation for Wrightian: robust across species and types of
#' interaction.
#' Include explicit math if possible.
#'
#' `population_vars` requires ...
#'
#' @param data Data frame or data frame extension (e.g. tibble)
#' @param population_vars Named character vector or list of columns in `data`
#'   that describe microbe abundance before and after experiment. See Details.
#' @param strain_names Character vector of names for microbes in experiment
#' @param keep Optional character vector of columns in `data` to keep
#'   in output (e.g. treatment variables, experimental block)
#'
#' @return Object of same type as `data` with the following columns:
#' @export
#'
calculate_mix_fitness <- function(
	data,
	population_vars,
	strain_names,
	keep = NULL
) {
	output <- data

	# Calculate initial and final population states
	output <- output |>
		set_initial_population(data, population_vars) |>
		set_final_population(data, population_vars)

	# Calculate fitness measures
	output <- within(output, {
		fitness_A         <- final_number_A / initial_number_A
		fitness_B         <- final_number_B / initial_number_B
		fitness_total     <- final_number_total / initial_number_total
		fitness_ratio_A_B <- fitness_A / fitness_B
	})

	# Replace NaN from single-strain expts etc
	output[sapply(output, is.nan)] <- NA

	# Warn about fitness zeroes

	# Label which is A and which is B
	output$name_A <- strain_names[[1]]
	output$name_B <- strain_names[[2]]

	# Return data frame with calculated values
	subset(output, select = c(
		"name_A", "name_B", keep,
		"initial_fraction_A", "initial_ratio_A_B",
		"fitness_A", "fitness_B", "fitness_total", "fitness_ratio_A_B"
	))
}

# Helper functions =============================================================

# Calculate initial population state
#   Using separate data, output to avoid column name conflicts
#   Possible input combos:
#     number_A,     number_B
#     number_total, fraction_A
#     number_total, fraction_B
#     number_total, number_A
#     number_total, number_B
set_initial_population <- function(output, data, vars) {
	vars <- as.list(vars)

	# TODO: Warn if invalid data

	if (
		# Data are number A & number B
		!is.null(vars$initial_number_A) &
		!is.null(vars$initial_number_B)
	) {
		output$initial_number_A <- data[[vars$initial_number_A]]
		output$initial_number_B <- data[[vars$initial_number_B]]
		output <- within(output, {
			initial_number_total <- initial_number_A + initial_number_B
			initial_fraction_A <- initial_number_A / initial_number_total
			initial_ratio_A_B <- initial_number_A / initial_number_B
		})
	# } else if () {
	} else {
		stop("Cannot calculate initial population from data")
	}

	output
}

# Calculate final population state
#   Using separate data, output to avoid column name conflicts
#   Possible input combos:
#     number_A,     number_B
#     number_total, fraction_A
#     number_total, fraction_B
#     number_total, number_A
#     number_total, number_B
set_final_population <- function(output, data, vars) {
	vars <- as.list(vars)

	# TODO: Warn if invalid data

	if (
		# Data are number A & number B
		!is.null(vars$final_number_A) &
		!is.null(vars$final_number_B)
	) {
		output$final_number_A <- data[[vars$final_number_A]]
		output$final_number_B <- data[[vars$final_number_B]]
		output <- within(output, {
			final_number_total <- final_number_A + final_number_B
		})
	# } else if () {
	} else {
		stop("Cannot calculate final population from data")
	}

	output
}

# Set strain names
#   Can be character vector e.g. c("Strain A", "Strain B")
#   Or can be columns in data (useful if data compares different strains)
# set_strain_names <- function(data, strain_names) {}

