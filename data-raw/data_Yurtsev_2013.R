# Prepare data_Yurtsev_2013

library(dplyr)
library(usethis)

data_Yurtsev_2013 <-
	read.csv("data-raw/difference_maps_ampicillin_no_inhibitor.csv") %>%
	tibble() %>%
	rename(
		OD_initial = ODi,
		OD_final = ODf,
		fraction_resistant_initial = fi,
		fraction_resistant_final = ff
	) %>%
	select(
		ampicillin, dilution,
		replicate, culture_id,
		OD_initial, fraction_resistant_initial,
		OD_final, fraction_resistant_final
	) %>%
	as.data.frame()

usethis::use_data(data_Yurtsev_2013, overwrite = TRUE)
