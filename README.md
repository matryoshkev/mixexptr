
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mixexptr - Analyze microbial mix experiments in R

<!-- badges: start -->
<!-- badges: end -->

A common experimental design when studying microbial interactions is to
mix together two different microbes (different genotypes, for example)
then measure how their fitness and behavior depends on mix frequency.
How do they behave differently together compared to on their own?

mixexptr is an R package for analyzing microbial mix experiments.
mixexptr provides tools to calculate and visualize fitness measures that
help researchers get the most out of their data and quantitatively
compare results across systems.

## Further reading

-   smith j and Inglis RF (2021) Evaluating kin and group selection as
    tools for quantitative analysis of microbial data. Proceedings B
    288:20201657. <https://doi.org/10.1098/rspb.2020.1657>

<!--
## Installation

You can install the development version of mixexptr from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("matryoshkev/mixexptr")
```

## Example

This is a basic example which shows you how to solve a common problem:


``` r
library(mixexptr)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`? You can include R chunks like so:


``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don't forget to commit and push the resulting figure files, so they display on GitHub and CRAN.
-->
