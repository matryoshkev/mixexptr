# Example usage of mixexptr: Madwick et al (2018) data
# Shows advanced mixexptr features and potential data issues

library(dplyr)    # Data handling that makes code more readable
library(ggplot2)  # To customize mixexptr plots


# Calculate fitness ------------------------------------------------------------

# Data observations are total cell counts and genotype frequency
head(data_Madgwick_2018)
var_names_Madgwick <- c(
	initial_number_total = "input_cells_total",
	initial_fraction_A = "input_freq_i",
	final_number_total = "spores_total",
	final_fraction_A = "output_freq_i",
	name_A = "strain_i",
  name_B = "strain_j"
)

# Calculate fitness measures
fitness_Madgwick <-
	data_Madgwick_2018 %>%
	tibble() %>%
	calculate_mix_fitness(var_names = var_names_Madgwick)


# Diagnostic plot of fitness effects -------------------------------------------

# Focus on one strain pair: NC105.1 + NC63.2
fitness_NC105_NC63 <-
	fitness_Madgwick %>%
	filter(name_A == "NC105.1" & name_B == "NC63.2")

fitness_NC105_NC63 %>% plot_mix_fitness()
# Within-group relative fitness more linear over log-ratio scale


# Plot specific fitness measures -------------------------------------------

dev.new(width = 5, height = 3)
fitness_Madgwick %>%
	filter(name_A == "NC105.1" & name_B == "NC63.2") %>%
	plot_strain_fitness() +
	ggplot2::facet_wrap(~ strain)

dev.new(width = 3, height = 2.5)
fitness_Madgwick %>%
	filter(name_A == "NC105.1" & name_B == "NC63.2") %>%
	plot_total_group_fitness()
	# ggplot2::geom_smooth(method = "loess")

dev.new(width = 3, height = 2.5)
fitness_NC105_NC63 %>% plot_within_group_fitness()

# mgcv::gam(
# 	fitness_total ~ s(initial_fraction_A, k = 3),
# 	data = fitness_NC105_NC63,
# 	method = "REML"
# )


# Tmp for dev -------------------------------------------

