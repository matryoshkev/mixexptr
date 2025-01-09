#' Calculate fitness measures
#'
#' Calculates several measures of microbial fitness given a data frame with the
#' initial and final abundances of two microbes.
#'
#' Describe Wrightian fitness. Describe fitness outcomes. Brief motivation for
#' Wrightian: robust across species and types of interaction. Include explicit
#' math if possible.
#'
#' `population_vars` requires ... Possible input combos
#'
#' @param data Data frame or data frame extension (e.g. tibble)
#' @param population_vars Named character vector of columns in `data`
#'   that describe microbe abundance before and after experiment. See Details.
#' @param strain_names Character vector of names for microbes in
#'   experiment
#' @param keep Optional character vector of columns in `data` to keep in output
#'   (e.g. treatment variables, experimental block)
#'
#' @return Object of same type as `data` with the following columns:
#' @export
#'
calculate_fitness <- function(
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
	if (any(c(output$fitness_A == 0, output$fitness_B == 0), na.rm = TRUE)) {
		warning(
			"Some fitness values are zero. Undefined on log scale.",
			call. = FALSE
		)
	}

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
set_initial_population <- function(output, data, vars) {
	vars <- as.list(vars)

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
	} else if (
		# Data are number total & fraction A
		!is.null(vars$initial_number_total) &
		!is.null(vars$initial_fraction_A)
	) {
		output$initial_number_total <- data[[vars$initial_number_total]]
		output$initial_fraction_A <- data[[vars$initial_fraction_A]]
		output <- within(output, {
			initial_number_A <- initial_number_total * initial_fraction_A
			initial_number_B <- initial_number_total * (1-initial_fraction_A)
			initial_ratio_A_B <- initial_fraction_A / (1-initial_fraction_A)
		})
	} else if (
		# Data are number total & fraction B
		!is.null(vars$initial_number_total) &
		!is.null(vars$initial_fraction_B)
	) {
		output$initial_number_total <- data[[vars$initial_number_total]]
		output$initial_fraction_B <- data[[vars$initial_fraction_B]]
		output <- within(output, {
			initial_number_A <- initial_number_total * (1-initial_fraction_B)
			initial_number_B <- initial_number_total * initial_fraction_B
			initial_ratio_A_B <- initial_number_A / initial_number_B
		})
	} else if (
		# Data are number total & number A
		!is.null(vars$initial_number_total) &
		!is.null(vars$initial_number_A)
	) {
		output$initial_number_total <- data[[vars$initial_number_total]]
		output$initial_number_A <- data[[vars$initial_number_A]]
		output <- within(output, {
			initial_number_B <- initial_number_total - initial_number_A
			initial_fraction_A <- initial_number_A / initial_number_total
			initial_ratio_A_B <- initial_number_A / initial_number_B
		})
	} else if (
		# Data are number total & number B
		!is.null(vars$initial_number_total) &
		!is.null(vars$initial_number_B)
	) {
		output$initial_number_total <- data[[vars$initial_number_total]]
		output$initial_number_B <- data[[vars$initial_number_B]]
		output <- within(output, {
			initial_number_A <- initial_number_total - initial_number_B
			initial_fraction_A <- initial_number_A / initial_number_total
			initial_ratio_A_B <- initial_number_A / initial_number_B
		})
	} else {
		stop("Cannot calculate initial population from data")
	}

	# Warn if nonbiological population state
	for (var_name in c("initial_number_A", "initial_number_B")) {
		if (any(output[[var_name]] < 0, na.rm = TRUE)) {
			warning(
				"Some ", var_name, " values < 0: Not biologically meaningful",
				call. = FALSE
			)
		}
	}

	output
}

# Calculate final population state
#   Using separate data, output to avoid column name conflicts
set_final_population <- function(output, data, vars) {
	vars <- as.list(vars)

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
	} else if (
		# Data are number total & fraction A
		!is.null(vars$final_number_total) &
		!is.null(vars$final_fraction_A)
	) {
		output$final_number_total <- data[[vars$final_number_total]]
		output <- within(output, {
			final_number_A <- final_number_total * final_fraction_A
			final_number_B <- final_number_total * (1-final_fraction_A)
		})
	} else if (
		# Data are number total & fraction B
		!is.null(vars$final_number_total) &
		!is.null(vars$final_fraction_B)
	) {
		output$final_number_total <- data[[vars$final_number_total]]
		output <- within(output, {
			final_number_A <- final_number_total * (1-final_fraction_B)
			final_number_B <- final_number_total * final_fraction_B
		})
	} else if (
		# Data are number total & number A
		!is.null(vars$final_number_total) &
		!is.null(vars$final_number_A)
	) {
		output$final_number_total <- data[[vars$final_number_total]]
		output$final_number_A <- data[[vars$final_number_A]]
		output <- within(output, {
			final_number_B <- final_number_total - final_number_A
		})
	} else if (
		# Data are number total & number B
		!is.null(vars$final_number_total) &
		!is.null(vars$final_number_B)
	) {
		output$final_number_total <- data[[vars$final_number_total]]
		output$final_number_B <- data[[vars$final_number_B]]
		output <- within(output, {
			final_number_A <- final_number_total - final_number_B
		})
	} else {
		stop("Cannot calculate final population from data")
	}

	# Warn if nonbiological population state
	for (var_name in c("final_number_A", "final_number_B")) {
		if (any(output[[var_name]] < 0, na.rm = TRUE)) {
			warning(
				"Some ", var_name, " values < 0: Not biologically meaningful",
				call. = FALSE
			)
		}
	}

	output
}

# TODO: Set strain names
#   Can be character vector e.g. c("Strain A", "Strain B")
#   Or can be columns in data (useful if data compares different strains)
# set_strain_names <- function(data, strain_names) {}

