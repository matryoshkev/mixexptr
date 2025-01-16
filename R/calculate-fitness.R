#' Calculate fitness measures
#'
#' Calculate several measures of microbial fitness from a data frame of the
#' initial and final abundances of two microbes.
#'
#' @param data Data frame of initial and final abundance values. Each row must
#'   contain data for two microbes in the same population. Accepts data frame
#'   extensions like `tibble`. See Details.
#' @param var_names Named character vector of columns in `data` that
#'   describe initial and final abundances. See Details.
#' @param keep Optional character vector of columns in `data` to keep in output
#'   (e.g. treatment variables, experimental block)
#'
#' @details
#' `var_names` must identify variables in `data` that are sufficient to identify
#' initial and final abundance of both strains. For initial abundance, this must
#' be two of:
#' * `initial_number_A`
#' * `initial_number_B`
#' * `initial_number_total`
#' * `initial_fraction_A`
#' * `initial_fraction_B`
#'
#' For final abundance, this must be two of:
#' * `final_number_A`
#' * `final_number_B`
#' * `final_number_total`
#' * `final_fraction_A`
#' * `final_fraction_B`
#'
#' Values of `number` vars in `data` can be counts or densities, but `initial`
#' and `final` must have same units.
#'
#' `var_names` must also contain `name_A` and `name_B` naming the
#' microbes in `data.` If these values are column names in `data`, `data` values
#' will be used in the output frame. Otherwise the output frame will use the
#' string values as names.
#'
#' @return
#' A data frame of same type as `data` with the following columns:
#' \item{`name_A`}{Name of strain A}
#' \item{`name_B`}{Name of strain B}
#' \item{...}{Other columns specified by `keep`}
#' \item{`initial_fraction_A`}{Initial frequency of strain A. Fraction of all
#' cells or virions.}
#' \item{`initial_ratio_A_B`}{Initial ratio of strain A to strain B
#' frequencies}
#' \item{`fitness_A`}{Fitness of strain A}
#' \item{`fitness_B`}{Fitness of strain B}
#' \item{`fitness_total`}{Total-group fitness. Sum of both strains.}
#' \item{`fitness_ratio_A_B`}{Within-group relative fitness measured as
#' `fitness_A` / `fitness_B`}
#'
#' All `fitness` values are unscaled Wrightian fitness measured over the
#' entire time period between the initial and final abundances. If \eqn{n_i}
#' and \eqn{n'_i} are the initial and final abundances of microbe \eqn{i}, then
#' its Wrightian fitness is \deqn{w_i = n'_i / n_i} If the absolute abundance of
#' a microbe increases 100-fold, its fitness will be \eqn{w = 100}. If it
#' decreases to 10% of its initial abundance, its fitness will be \eqn{w = 0.1}.
#' These fitness measures are robust across microbial species and types of
#' interaction, make fitness effects quantitatively comparable across systems,
#' and can be meaningfully incorporated into theoretical models of microbial
#' social evolution. They are best visualized and analyzed over \eqn{\log_{10}}
#' scales.
#'
#' @references
#'
#' smith j and Inglis RF (2021) Evaluating kin and group selection as tools for
#' quantitative analysis of microbial data. Proceedings B 288:20201657.
#' <https://doi.org/10.1098/rspb.2020.1657>
#'
#' @examples
#' # Data with cell counts for each strain
#' calculate_mix_fitness(
#'   data_smith_2010,
#'   var_names = c(
#'     initial_number_A = "initial_cells_evolved",
#'     initial_number_B = "initial_cells_ancestral",
#'     final_number_A   = "final_spores_evolved",
#'     final_number_B   = "final_spores_ancestral",
#'     name_A = "Evolved GVB206.3",
#'     name_B = "Ancestral GJV10"
#'   ),
#'   keep = "exptl_block"
#' )
#'
#' # Data with total density and strain frequency
#' calculate_mix_fitness(
#'   data_Yurtsev_2013,
#'   var_names = c(
#'     initial_number_total = "OD_initial",
#'     initial_fraction_A = "fraction_resistant_initial",
#'     final_number_total = "OD_final",
#'     final_fraction_A = "fraction_resistant_final",
#'     name_A = "AmpR",
#'     name_B = "AmpS"
#'   ),
#'   keep = c("ampicillin", "dilution", "replicate")
#' )
#' # Warns of nonbiological values in data: some resistant fractions < 0
#' # Artifact of subtracting background during flow cytometry?
#'
#' @export
#'
calculate_mix_fitness <- function(data, var_names, keep = NULL) {
	output <- data

	# Calculate initial and final population states
	output <- set_population(output, "initial", data, var_names)
	output <- set_population(output, "final", data, var_names)

	# Calculate fitness measures
	output$fitness_A <- output$final_number_A / output$initial_number_A
	output$fitness_B <- output$final_number_B / output$initial_number_B
	output$fitness_total <-
		output$final_number_total / output$initial_number_total
	output$fitness_ratio_A_B <- output$fitness_A / output$fitness_B

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
	output <- set_strain_names(output, data, var_names)

	# Return data frame with calculated values
	subset(output, select = c(
		"name_A", "name_B", keep,
		"initial_fraction_A", "initial_ratio_A_B",
		"fitness_A", "fitness_B", "fitness_total", "fitness_ratio_A_B"
	))
}

# Helper functions =============================================================

# Calculate initial/final population state
set_population <- function(output, time_point, data, var_names) {
	time_point <- match.arg(time_point, c("initial", "final"))

	# Names for output data frame
	name_number_A     <- paste0(time_point, "_number_A")
	name_number_B     <- paste0(time_point, "_number_B")
	name_number_total <- paste0(time_point, "_number_total")
	name_fraction_A   <- paste0(time_point, "_fraction_A")
	name_fraction_B   <- paste0(time_point, "_fraction_B")
	name_ratio_A_B    <- paste0(time_point, "_ratio_A_B")

	# Variable names in data
	var_names <- as.list(var_names)
	var_number_A     <- var_names[[name_number_A]]
	var_number_B     <- var_names[[name_number_B]]
	var_number_total <- var_names[[name_number_total]]
	var_fraction_A   <- var_names[[name_fraction_A]]
	var_fraction_B   <- var_names[[name_fraction_B]]

	if (!is.null(var_number_A) & !is.null(var_number_B)) {
		# Data are number A & number B
		validate_count_data(data, var_number_A)
		validate_count_data(data, var_number_B)
		number_A     <- data[[var_number_A]]
		number_B     <- data[[var_number_B]]
		number_total <- number_A + number_B
		fraction_A   <- number_A / number_total
	} else if (!is.null(var_number_total) & !is.null(var_fraction_A)) {
		# Data are number total & fraction A
		validate_count_data(data, var_number_total)
		validate_fraction_data(data, var_fraction_A)
		number_total <- data[[var_number_total]]
		fraction_A   <- data[[var_fraction_A]]
		number_A     <- number_total * fraction_A
		number_B     <- number_total * (1-fraction_A)
	} else if (!is.null(var_number_total) & !is.null(var_fraction_B)) {
		# Data are number total & fraction B
		validate_count_data(data, var_number_total)
		validate_fraction_data(data, var_fraction_B)
		number_total <- data[[var_number_total]]
		fraction_A   <- 1 - data[[var_fraction_B]]
		number_A     <- number_total * fraction_A
		number_B     <- number_total * (1-fraction_A)
	} else if (!is.null(var_number_total) & !is.null(var_number_A)) {
		# Data are number total & number A
		validate_count_data(data, var_number_total)
		validate_count_data(data, var_number_A)
		validate_difference_data(data, var_number_A, var_number_total)
		number_total <- data[[var_number_total]]
		number_A     <- data[[var_number_A]]
		number_B     <- number_total - number_A
		fraction_A   <- number_A / number_total
	} else if (!is.null(var_number_total) & !is.null(var_number_B)) {
		# Data are number total & number B
		validate_count_data(data, var_number_total)
		validate_count_data(data, var_number_B)
		validate_difference_data(data, var_number_B, var_number_total)
		number_total <- data[[var_number_total]]
		number_B     <- data[[var_number_B]]
		number_A     <- number_total - number_B
		fraction_A   <- number_A / number_total
	}

	output[[name_number_A]]     <- number_A
	output[[name_number_B]]     <- number_B
	output[[name_number_total]] <- number_total
	if (time_point == "initial") {
		output[[name_fraction_A]] <- fraction_A
		output[[name_ratio_A_B]]  <- number_A / number_B
	}
	output
}

# Validate number data
validate_count_data <- function(data, var_name) {
	if (any(data[[var_name]] < 0, na.rm = TRUE)) {
		warning(
			"Some ", var_name, " values < 0: Not biologically meaningful",
			call. = FALSE
		)
	}
}

# Validate fraction data
validate_fraction_data <- function(data, var_name) {
	if (any(c(data[[var_name]] < 0, data[[var_name]] > 1), na.rm = TRUE)) {
		warning(
			"Some ", var_name, " values not in range [0, 1]",
			": Not biologically meaningful",
			call. = FALSE
		)
	}
}

# Validate data difference between strain and total number
validate_difference_data <- function(data, var_name_strain, var_name_total) {
	if (any(c(data[[var_name_strain]] > data[[var_name_total]]), na.rm = TRUE)) {
		warning(
			"Some ", var_name_strain, " values > ", var_name_total,
			": Not biologically meaningful",
			call. = FALSE
		)
	}
}

# Set strain names in target frame using vars in input frame
#   If input var not present use string
set_strain_names <- function(target, input, var_names) {
	var_names <- as.list(var_names)
	var_name_A <- var_names$name_A
	var_name_B <- var_names$name_B

	# Error if names missing
	if (is.null(var_name_A)) stop("var_names must contain name_A")
	if (is.null(var_name_B)) stop("var_names must contain name_B")

	# If input var not present use string
	name_A <- input[[var_name_A]]
	name_B <- input[[var_name_B]]
	target$name_A <- if (is.null(name_A)) var_name_A else name_A
	target$name_B <- if (is.null(name_B)) var_name_B else name_B

	target
}

