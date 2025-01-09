# Example usage of mixexptr

library(dplyr)

names(data_smith_2010)

data_smith_2010 |>
	tibble() |>
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

# my_pop_vars <- c(
# 	initial_number_A = "initial_cells_evolved",
# 	initial_number_B = "initial_cells_ancestral",
# 	final_number_A = "final_spores_evolved",
# 	final_number_B = "final_spores_ancestral"
# )

# data_smith2010 |>
# 	set_initial_population(data_smith2010, my_pop_vars)
# 	set_final_population(data_smith2010, my_pop_vars)
