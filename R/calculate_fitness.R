#' Calculate microbial fitness
#'
#' `calculate_mix_fitness()` calculates several measures of microbial fitness
#' given a data frame with the observed abundances of two microbes before and
#' after the experiment.
#'
#' @param data Data frame or data frame extension (e.g. tibble)
#' @param population_vars Named vector or list.
#' @param strain_names Named vector or list.
#' @param keep Optional named vector or list of column names in `data` to keep
#'   in output frame (e.g. treatment variables).
#'
#' @return Object of same type as `data`.
#' @export
#'
#' @examples
calculate_mix_fitness <- function(
	data,
	population_vars,
	strain_names,
	keep = NULL
) {
	output <- data
	population_vars <- as.list(population_vars)
	strain_names <- as.list(strain_names)

	# Label which is A and which is B

	# Get initial and final population states

	# Calculate fitness and initial frequency

	# Replace NaN from single-strain expts etc
	output[sapply(output, is.nan)] <- NA

	# Warn about fitness zeroes

	# Return frame with calculated values added
	output
}

# Helper functions =============================================================

