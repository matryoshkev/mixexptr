# Figure elements

# Add x-axis scale: initial fraction strain A
#
add_scale_initial_fraction_A <- function(input_fig, name_A) {
	breaks <- seq(0, 1, by = 0.2)
	input_fig +
		ggplot2::aes(x = .data$initial_fraction_A) +
		ggplot2::scale_x_continuous(
			name = paste("Initial fraction", name_A),
			limits = c(0, 1),
			breaks = breaks,
			minor_breaks = breaks
		)
}

