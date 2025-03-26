#' Data from smith, Van Dyken, and Zee (2010): Experimentally-evolved
#' _Myxococcus_ cheaters
#'
#' This data describes the sporulation success of _Myxococcus xanthus_
#' bacteria in mixed-genotype fruiting bodies. GVB206.3 is an experimentally
#' evolved genotype. GJV10 is an ancestral genotoype.
#'
#' @format
#' `data_smith_2010` is a data frame with 28 rows and 5 columns:
#' \describe{
#'   \item{exptl_block}{Experimental block. Date experiment started (YYYY-MM-DD).}
#'   \item{initial_cells_evolved}{Initial number of GVB206.3 cells (colony-forming units per plate)}
#'   \item{initial_cells_ancestral}{Initial number of GJV10 cells (colony-forming units per plate)}
#'   \item{final_spores_evolved}{Final number of GVB206.3 spores (colony-forming units per plate)}
#'   \item{final_spores_ancestral}{Final number of GJV10 spores (colony-forming units per plate)}
#' }
#'
#' @references
#' smith j, Van Dyken JD, and Zee PC (2010) A generalization of Hamilton's rule
#' for the evolution of microbial cooperation. Science 328:1700-1703.
#' <https://doi.org/10.1126/science.1189675>
#'
"data_smith_2010"

#' Data from Yurtsev et al. (2013): _E. coli_ cheating of resistance to
#' beta-lactam antibiotics
#'
#' This data set describes the growth of bacterial populations over a single
#' cycle in a variety of environments (different ampicillin concentrations)
#' from a variety of initial starting conditions (changing both the total
#' initial cell density and the relative abundance of resistant and sensitive
#' bacteria).
#
#' @format
#' `data_Yurtsev_2013` is a data frame with 2304 rows and 8 columns:
#' \describe{
#'   \item{ampicillin}{Antibiotic concentration in micrograms per mL}
#'   \item{dilution}{The amount by which the culture was diluted}
#'   \item{culture_id}{Provided for convenience, but **not unique**, use ampicillin concentrations and replicate to identify cultures uniquely}
#'   \item{replicate}{Integer to indicate from which experiment the data came from}
#'   \item{OD_initial}{Initial population density (measured in units of optical density), population density at beginning of growth cycle measurement corrected for non-linear effects and background}
#'   \item{OD_final}{Final population density (measured in units of optical density), population density at end of growth cycle measurement corrected for non-linear effects and background}
#'   \item{fraction_resistant_initial}{Initial fraction of resistant cells (measured using flow cytometry), fraction of resistant cells at the beginning of the growth cycle}
#'   \item{fraction_resistant_final}{Final fraction of resistant cells (measured using flow cytometry), fraction of resistant cells at end of growth cycle}
#' }
#'
#' @references
#' Yurtsev EA, Chao HX, Datta MS, Artemova T, and Gore J (2013) Bacterial
#' cheating drives the population dynamics of cooperative antibiotic resistance
#' plasmids. Molecular Systems Biology 9:683.
#' <https://doi.org/10.1038/msb.2013.39>
#'
#' @source
#' <https://bitbucket.org/eugene_yurtsev/bacterialcheatingproject>
#'
"data_Yurtsev_2013"

#' Data from Madgwick et al. (2018): Cooperation and cheating among
#' _Dictyostelium_ amoebae
#'
#' This data describes the sporulation success of _Dictyostelium discoideum_
#' amoebae in mixed-genotype fruiting bodies.
#'
#' @format
#' `data_Madgwick_2018` is a data frame with 1226 rows and 7 columns:
#' \describe{
#'   \item{strain_i}{Name of strain i}
#'   \item{strain_j}{Name of strain j}
#'   \item{replicate}{Experimental block}
#'   \item{input_freq_i}{input frequency of strain i (determined by experiment)}
#'   \item{input_cells_total}{total number of cells plated for development (determined by experiment)}
#'   \item{output_freq_i}{output frequency of strain i (measured using flow cytometry or microscopy)}
#'   \item{spores_total}{total number of spores produced (measured using hemocytometer)}
#' }
#'
#' @references
#' Madgwick PG, Stewart B, Belcher LJ, Thompson CRL, and Wolf JB (2018)
#' Strategic investment explains patterns of cooperation and cheating in a
#' microbe. Proceedings of the National Academy of Science USA 115:
#' E4823-E4832. <https://doi.org/10.1073/pnas.1716087115>
#'
"data_Madgwick_2018"
