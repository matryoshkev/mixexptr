# Prepare data_smith2010

library(dplyr, warn.conflicts = FALSE)
library(usethis)

data_smith2010 <-
	read.delim("data-raw/data-smith2010.tsv", comment.char = "#") |>
	tibble() |>
	mutate(
		final_spores_evolved = colonies_rif_1 * dilution_rif_1,
		final_spores_ancestral = colonies_kan_1 * dilution_kan_1,
	) |>
	select(exptl_block, starts_with("initial"), starts_with("final"))

usethis::use_data(data_smith2010, overwrite = TRUE)
