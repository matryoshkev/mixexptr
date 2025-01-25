# Example usage of mixexptr: Yurtsev et al (2013) data
# Shows advanced mixexptr features and potential data issues

library(dplyr)    # Data handling that makes code more readable
library(ggplot2)  # To customize mixexptr plots


# Curate data and calculate fitness --------------------------------------------

# Data observations are total cell density (OD 600) and fraction resistant
head(data_Yurtsev_2013)
var_names_Yurtsev <- c(
	initial_number_total = "OD_initial",
	initial_fraction_A = "fraction_resistant_initial",
	final_number_total = "OD_final",
	final_fraction_A = "fraction_resistant_final",
	name_A = "AmpR",
  name_B = "AmpS"
)

# Data includes experimental treatments: ampicillin concentration and
# dilution factor
data_Yurtsev_2013 %>% select(ampicillin, dilution) %>% distinct()

# Focus on data used in Fig. 3AB of the paper
data_Fig3AB <-
	data_Yurtsev_2013 %>%
	tibble() %>%
	filter(
		ampicillin %in% c(0, 15, 50, 100, 200) &
		dilution %in% c(100, 200)
	)

# Calculate fitness measures and include treatment variables
data_Fig3AB %>%
	calculate_mix_fitness(
		var_names = var_names_Yurtsev,
		keep = c("ampicillin", "dilution")
	)
# Warning messages:
# 1: Some fraction_resistant_initial values not in range [0, 1]:
#    Not biologically meaningful
# 2: Some fraction_resistant_final values not in range [0, 1]:
#    Not biologically meaningful

# Some strain frequencies < 0 due to noise correction of flow cytometry data
data_Fig3AB %>%
	filter(fraction_resistant_initial < 0 | fraction_resistant_final < 0) %>%
	select(fraction_resistant_initial, fraction_resistant_final) %>%
	distinct()

# Calculate fitness from biologically meaningful data
fitness_Fig3AB <-
	data_Fig3AB %>%
	filter(fraction_resistant_initial > 0 & fraction_resistant_final > 0) %>%
	calculate_mix_fitness(
		var_names = var_names_Yurtsev,
		keep = c("ampicillin", "dilution", "replicate", "culture_id")
	)


# Diagnostic plot of fitness effects -------------------------------------------

fitness_Fig3AB %>% plot_mix_fitness()
# Shows combined data for all treatment levels: too busy!

# Focus on one combination of treatments
fitness_Fig3AB %>%
	filter(ampicillin == 100 & dilution == 100) %>%
	plot_mix_fitness()
# Total group fitness mostly unchanged by mixing
# Most of the action is within group, over linear mixing scale

# If we want to include fitness results in archived data package,
# we can use better variable names and clean up
fitness_Fig3AB <- fitness_Fig3AB %>%
	rename(
		initial_freq_AmpR = "initial_fraction_A",
		initial_ratio_AmpRS = "initial_ratio_A_B",
		fitness_AmpR = "fitness_A",
		fitness_AmpS = "fitness_B",
		fitness_ratio_AmpRS = "fitness_ratio_A_B"
	) %>%
	select(
		ampicillin, dilution, replicate, culture_id,
		initial_freq_AmpR, initial_ratio_AmpRS,
		fitness_AmpR, fitness_AmpS, fitness_total, fitness_ratio_AmpRS
	)
# Including all fitness outcomes increases data re-usability


# Within-group fitness ---------------------------------------------------------

# We can use mixexptr plot functions with our preferred variable names
# as long as we tell them what's what
fitness_names_Yurtsev <- c(
	initial_fraction_A = "initial_freq_AmpR",
	initial_ratio_A_B = "initial_ratio_AmpRS",
	fitness_ratio_A_B = "fitness_ratio_AmpRS",
	fitness_total = "fitness_total",
	name_A = "AmpR",
	name_B = "AmpS"
)

# Plot within-group fitness ratio, including treatment effects
# dev.new(width = 2.5, height = 2.25, units = "in")
# dev.new(width = 5, height = 2.5, units = "in")
fitness_Fig3AB %>%
	plot_within_group_fitness(var_names = fitness_names_Yurtsev) +
	facet_wrap(~ dilution, labeller = "label_both") +
	aes(fill = factor(ampicillin)) +
	scale_fill_brewer(name = "Ampicillin\n(\u03BCg/mL)", palette = "YlOrRd")

# Can use theme options of your choice
fitness_Fig3AB %>%
	plot_within_group_fitness(var_names = fitness_names_Yurtsev) +
	facet_wrap(~ dilution, labeller = "label_both") +
	aes(fill = factor(ampicillin)) +
	scale_fill_brewer(name = "Ampicillin\n(\u03BCg/mL)", palette = "YlOrRd") +
	theme_bw()

# Total-group fitness ---------------------------------------------------------

# NOTE: This section mainly for dev

fitness_Fig3AB %>%
	plot_total_group_fitness(var_names = fitness_names_Yurtsev) +
	facet_wrap(~ dilution, labeller = "label_both") +
	aes(fill = factor(ampicillin)) +
	scale_fill_brewer(name = "Ampicillin\n(\u03BCg/mL)", palette = "YlOrRd")


# Strain fitness ---------------------------------------------------------------

# NOTE: This section mainly for dev

fitness_names_Yurtsev <- c(
	fitness_names_Yurtsev,
	fitness_A = "fitness_AmpR",
	fitness_B = "fitness_AmpS"
)

fitness_Fig3AB %>%
	filter(ampicillin == 100 ) %>%
	plot_strain_fitness(var_names = fitness_names_Yurtsev) +
	facet_wrap(~ dilution, labeller = "label_both")

fitness_Fig3AB %>%
	filter(ampicillin == 100 & dilution == 100) %>%
	plot_fitness_strain_total(var_names = fitness_names_Yurtsev)

dev.new()
fitness_Fig3AB %>%
	filter(ampicillin == 100 & dilution == 100) %>%
	plot_mix_fitness(var_names = fitness_names_Yurtsev)

