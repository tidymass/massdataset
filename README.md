<!-- README.md is generated from README.Rmd. Please edit that file -->

# massdataset <img src="man/figures/massdataset_logo.png" align="right" alt="" width="120" />

[![](https://www.r-pkg.org/badges/version/massdataset?color=green)](https://cran.r-project.org/package=massdataset)
[![](https://img.shields.io/github/languages/code-size/tidymass/massdataset.svg)](https://github.com/tidymass/massdataset)
[![Dependencies](https://tinyverse.netlify.com/badge/massdataset)](https://cran.r-project.org/package=massdataset)
[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

`massdataset` is a part of [tidymass](https://www.tidymass.org/).

------

# About

`massdataset` provides the `mass_dataset` class for organizing rectangular **metabolomics datasets** in a standard structure. It also provides core processing utilities for working with `mass_dataset` objects. The class is designed to interoperate with the broader [`tidymass`](https://www.tidymass.org/) ecosystem.

<img src="man/figures/Figure.png" align="middle" alt="" width = "100%"/>

Merging separate data objects is useful for manually-imported data objects, especially for metabolomics data analysis. It's significant to keep unified format before analysis. While the first category of merging functions is useful for direct manipulations of the data for analytical purposes, `massdataset` is a convenience/support tool to help get your data into the right format.

# Installation

You can install the development version of `massdataset` from [GitHub](https://github.com/tidymass/massdataset).

``` r
if(!require(remotes)){
install.packages("remotes")
}
remotes::install_github("tidymass/massdataset")
```

More information can be found [here](https://www.tidymass.org/massdataset/articles/massdataset_install.html).

# Get started

Please see the `Help documents`.

# Need help?

If you have any questions about `massdataset`, please don’t hesitate to
email me (<xiaotao.shen@outlook.com>) or reach out via the social media links below.

<i class="fa fa-weixin"></i>
WeChat: shenxt1990

<i class="fa fa-envelope"></i> <xiaotao.shen@outlook.com>

<i class="fa fa-twitter"></i>
[Twitter](https://twitter.com/JasperShen1990)

# Citation

If you use `massdataset` in your publications, please cite this paper:

Shen, X., Yan, H., Wang, C. et al. TidyMass an object-oriented reproducible analysis framework for LC–MS data. Nat Commun 13, 4365 (2022). 

[Weblink](https://www.nature.com/articles/s41467-022-32155-w)

Thanks very much!
