# Prepare data_Madgwick_2018

# TODO: Include corrected frequencies (see paper methods)?

library(readr)
library(dplyr)
library(usethis)

data_Madgwick_2018 <-
	read_csv("data-raw/Supplementary Table 1_Investment data.csv", na = ".") %>%
	rename(
		strain_i = "Strain i",
		strain_j = "Strain j",
		input_freq_i = "p(i)",
		output_freq_i = "p(i)'",
		spores_total = "TG(ij)",
		spores_i_alone = "Ti(c)",
		spores_j_alone = "Tj(c)"
	) %>%
	mutate(input_cells_total = 1e7) %>%
	select(
		strain_i, strain_j, replicate,
		input_freq_i, input_cells_total,
		output_freq_i, spores_total,
		spores_i_alone, spores_j_alone
	)

# Strain i alone
data_i_alone <-
	data_Madgwick_2018 %>%
	select(strain_i, strain_j, replicate, input_cells_total, spores_i_alone) %>%
	distinct() %>%
	mutate(input_freq_i = 1, output_freq_i = 1) %>%
	rename(spores_total = spores_i_alone)

# Strain j alone
data_j_alone <-
	data_Madgwick_2018 %>%
	select(strain_i, strain_j, replicate, input_cells_total, spores_j_alone) %>%
	distinct() %>%
	mutate(input_freq_i = 0, output_freq_i = 0) %>%
	rename(spores_total = spores_j_alone)

# Combine mixed and single-strain data
data_Madgwick_2018 <-
	data_Madgwick_2018 %>%
	select(
		strain_i, strain_j, replicate,
		input_freq_i, input_cells_total,
		output_freq_i, spores_total
	) %>%
	bind_rows(data_i_alone, data_j_alone) %>%
	arrange(strain_i, strain_j, replicate, input_freq_i) %>%
	as.data.frame()

data_Madgwick_2018

usethis::use_data(data_Madgwick_2018, overwrite = TRUE)
