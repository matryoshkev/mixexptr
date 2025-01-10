# Prepare data_Yurtsev_2013
# Source: Yurtsev EA, Chao HX, Datta MS, Artemova T, Gore J, 2013 Bacterial cheating
# drives the population dynamics of cooperative antibiotic resistance plasmids.
# Molecular Systems Biology 9:683. doi:10.1038/msb.2013.39
# Url: https://bitbucket.org/eugene_yurtsev/bacterialcheatingproject

library(dplyr)

data_Yurtsev_2013 <-
	read.csv("data-raw/difference_maps_ampicillin_no_inhibitor.csv") |>
	rename(
		OD_initial = ODi,
		OD_final = ODf,
		fraction_resistant_initial = fi,
		fraction_resistant_final = ff
	) |>
	select(
		ampicillin, dilution,
		replicate, culture_id,
		OD_initial, fraction_resistant_initial,
		OD_final, fraction_resistant_final
	)

usethis::use_data(data_Yurtsev_2013, overwrite = TRUE)
