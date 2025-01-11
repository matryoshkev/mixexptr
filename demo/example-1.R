# Example usage of mixexptr
library(dplyr)

# data_smith_2010 --------------------------------------------------------------

fitness_smith_2010 <-
	data_smith_2010 %>%
	tibble() %>%
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
fitness_smith_2010

# Testing
#
# fitness_smith_2010 %>% format_to_plot_fitness()
#
# dev.new(width = 2.25, height = 2.25, units = "in")
# fitness_smith_2010 %>% plot_within_group_fitness()
#
dev.new(width = 4.5, height = 2.25, units = "in")
tmp <- fitness_smith_2010 %>% plot_fitness_strain_total()

dev.new(width = 6.25, height = 2.25, units = "in")
# tmp <- fitness_smith_2010 %>% plot_fitness()
fitness_smith_2010 %>% plot_fitness()
fitness_smith_2010 %>% plot_fitness(mix_scale = "ratio")


# data_Yurtsev_2013 ------------------------------------------------------------

fitness_Yurstev_2013 <-
	data_Yurtsev_2013 %>%
	tibble() %>%
	subset(
		# Drop rows with nonbiological values
		!is.na(fraction_resistant_initial) &
		!is.na(fraction_resistant_final) &
		fraction_resistant_initial > 0 &
		fraction_resistant_final > 0 &
		fraction_resistant_initial < 1 &
		fraction_resistant_final < 1
	) %>%
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
fitness_Yurstev_2013

# Testing
#
# fitness_Yurstev_2013 %>% format_to_plot_fitness()
#
# dev.new(width = 2.25, height = 2.25, units = "in")
# fitness_Yurstev_2013 %>%
# 	filter(dilution == 100 & ampicillin == 100) %>%
# 	plot_within_group_fitness()
# 	# ggplot2::aes(color = factor(ampicillin))
#
# dev.new(width = 4.5, height = 2.25, units = "in")
# fitness_Yurstev_2013 %>%
# 	filter(dilution == 100 & ampicillin == 100) %>%
# 	plot_fitness_strain_total()

dev.new(width = 6.25, height = 2.25, units = "in")
tmp <-
	fitness_Yurstev_2013 %>%
	filter(dilution == 100 & ampicillin == 100) %>%
	plot_fitness()
plot(tmp)

fitness_Yurstev_2013 %>%
	filter(dilution == 100 & ampicillin == 100) %>%
	plot_fitness(mix_scale = "ratio")
