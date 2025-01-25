# Example usage of mixexptr: smith et al (2010) data

library(dplyr)
# library(ggplot2)


# Calculate fitness ------------------------------------------------------------

# Data is initial and final cell counts
head(data_smith_2010)

# Calculate fitness measures
fitness_smith_2010 <-
	data_smith_2010 %>%
	tibble() %>%
	calculate_mix_fitness(
		var_names = c(
			initial_number_A = "initial_cells_evolved",
			initial_number_B = "initial_cells_ancestral",
			final_number_A = "final_spores_evolved",
			final_number_B = "final_spores_ancestral",
			name_A = "GVB206.3",
			name_B = "GJV10"
		),
		keep = "exptl_block"
	)
fitness_smith_2010


# Diagnostic plot of fitness effects -------------------------------------------

# dev.new(width = 6.25, height = 4.5, units = "in")
plot_mix_fitness(fitness_smith_2010)

# Multilevel fitness appears most informative/convenient
#   Total group over mixing fraction
#   Within-group over mixing ratio


# Within-group fitness ---------------------------------------------------------

# Plot within-group fitness
# dev.new(width = 2.5, height = 2.25, units = "in")
fig_within_group <-
	fitness_smith_2010 %>%
	plot_within_group_fitness(mix_scale = "ratio")
fig_within_group

# Fit statistical model
fitted_within_group <- lm(
	log10(fitness_ratio_A_B) ~ log10(initial_ratio_A_B),
	data = fitness_smith_2010
)

# Parameter estimates and confidence intervals
summary(fitted_within_group)
confint(fitted_within_group)

# Add fitted model to within-group figure
predicted_within_group <- tibble(initial_ratio_A_B = 10^seq(-2, 2, by = 0.2))
predicted_within_group <- predicted_within_group %>%
	mutate(
		fitness_ratio_A_B =
			predict(fitted_within_group, newdata = predicted_within_group),
		fitness_ratio_A_B = 10^fitness_ratio_A_B
	)
fig_within_group <- fig_within_group + geom_line(data = predicted_within_group)
fig_within_group


# Total-group fitness ----------------------------------------------------------

# Plot total-group fitness
# dev.new(width = 2.5, height = 2.25)
fig_total_group <- fitness_smith_2010 %>% plot_total_group_fitness()
fig_total_group

# Fit statistical model
fitted_total_group <- lm(
	log10(fitness_total) ~ poly(initial_fraction_A, 2),
	data = fitness_smith_2010
)

# Parameter estimates and confidence intervals
summary(fitted_total_group)
confint(fitted_total_group)

# Add fitted model to total-group figure
predicted_total_group <- tibble(initial_fraction_A = seq(0,1, by = 0.02))
predicted_total_group <- predicted_total_group %>%
	mutate(
		fitness_total =
			predict(fitted_total_group, newdata = predicted_total_group),
		fitness_total = 10^fitness_total
	)
fig_total_group <- fig_total_group + geom_line(data = predicted_total_group)


# Make combined figure using `patchwork` package -------------------------------

dev.new(width = 4.5, height = 2.25, units = "in")
fig_smith_2010 <- fig_total_group + fig_within_group
fig_smith_2010

