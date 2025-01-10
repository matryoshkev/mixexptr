#'Calculate fitness measures
#'
#'Calculates several measures of microbial fitness given a data frame with the
#'initial and final abundances of two microbes.
#'
#'`data` columns named in `population_vars` must be sufficient to identify
#'initial and final abundance of both strains. For initial abundance, vector
#'names should be two of `initial_number_A`, `initial_number_B`,
#'`initial_number_total`, `initial_fraction_A`, or `initial_fraction_B.` For
#'final abundance, vector names should be two of `final_number_A`,
#'`final_number_B`, `final_number_total`, `final_fraction_A`, or
#'`final_fraction_B`. Values of `number` vars can be counts or densities, but
#'initial and final must have same units.
#'
#'@param data Data frame or data frame extension (e.g. tibble). Each row
#'  contains data for two microbes in the same population.
#'@param population_vars Named character vector of columns in `data` that
#'  describe initial and final abundances. See Details.
#'@param strain_names Character vector of microbe names. Strain A first, strain
#'  B second.
#'@param keep Optional character vector of columns in `data` to keep in output
#'  (e.g. treatment variables, experimental block)
#'
#'@return Data frame of same type as `data` with the following columns:
#'  \item{`name_A`}{Name of strain A}
#'
#'  \item{`name_B`}{Name of strain B}
#'
#'  \item{...}{Other columns specified by `keep`}
#'
#'  \item{`initial_fraction_A`}{Initial frequency of strain A. Fraction of all
#'  cells or virions.}
#'
#'  \item{`initial_ratio_A_B`}{Initial ratio of strain A to strain B
#'  frequencies}
#'
#'  \item{`fitness_A`}{Fitness of strain A}
#'
#'  \item{`fitness_B`}{Fitness of strain B}
#'
#'  \item{`fitness_total`}{Total-group fitness. Sum of both strains.}
#'
#'  \item{`fitness_ratio_A_B`}{Within-group relative fitness measured as
#'  `fitness_A` / `fitness_B`}
#'
#'  All `fitness` values are unscaled Wrightian fitness measured over the entire
#'  time period between the initial and final abundances. If \eqn{n_i} and
#'  \eqn{n'_i} are the initial and final abundances of microbe \eqn{i}, then its
#'  Wrightian fitness is \deqn{w_i = n'_i / n_i} If the absolute abundance of a
#'  microbe increases 100-fold, its fitness will be \eqn{w = 100}. If it
#'  decreases to 10% of its initial abundance, its fitness will be \eqn{w =
#'  0.1}. These fitness measures are robust across microbial species and types
#'  of interaction, make fitness effects quantitatively comparable across
#'  systems, and can be meaningfully incorporated into theoretical models of
#'  microbial social evolution. They are best visualized and analyzed over
#'  \eqn{\log_{10}} scales.
#'
#'@references
#'
#'smith j and Inglis RF (2021) Evaluating kin and group selection as tools for
#'quantitative analysis of microbial data. Proceedings B 288:20201657.
#'<https://doi.org/10.1098/rspb.2020.1657>
#'
#'@examples
#' # Data with cell counts for each strain
#' calculate_fitness(
#'   data_smith_2010,
#'   population_vars = c(
#'     initial_number_A = "initial_cells_evolved",
#'     initial_number_B = "initial_cells_ancestral",
#'     final_number_A   = "final_spores_evolved",
#'     final_number_B   = "final_spores_ancestral"
#'   ),
#'   strain_names = c("GVB206.3", "GJV10"),
#'   keep = "exptl_block"
#' )
#'
#' # Data with total density and strain frequency
#'
#'@export
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

	# TODO: Warn if nonbiological input

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
	# TODO: Replace with warning if nonbiological input

	output
}

# Calculate final population state
#   Using separate data, output to avoid column name conflicts
set_final_population <- function(output, data, vars) {
	vars <- as.list(vars)

	# TODO: Warn if nonbiological input

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
	# TODO: Replace with warning if nonbiological input

	output
}

# TODO: Set strain names
#   Can be character vector e.g. c("Strain A", "Strain B")
#   Or can be columns in data (useful if data compares different strains)
# set_strain_names <- function(data, strain_names) {}

