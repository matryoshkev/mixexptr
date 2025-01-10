# Example usage of mixexptr

library(dplyr)

data_smith_2010 %>%
	# tibble() %>%
	calculate_fitness(
		population_vars = c(
			initial_number_A = "initial_cells_evolved",
			initial_number_B = "initial_cells_ancestral",
			final_number_A = "final_spores_evolved",
			final_number_B = "final_spores_ancestral"
		),
		strain_names = c("GVB206.3", "GJV10"),
		keep = "exptl_block"
	)

data_Yurtsev_2013 %>%
	# tibble() %>%
	calculate_fitness(
		population_vars = c(
			initial_number_total = "OD_initial",
			initial_fraction_A = "fraction_resistant_initial",
			final_number_total = "OD_final",
			final_fraction_A = "fraction_resistant_final"
		),
		strain_names = c("AmpR", "AmpS"),
		keep = c("ampicillin", "dilution", "replicate")
	)
