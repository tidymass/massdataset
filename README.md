<!-- README.md is generated from README.Rmd. Please edit that file -->

# massdataset <img src="man/figures/massdataset_logo.png" align="right" alt="" width="120" />

[![](https://www.r-pkg.org/badges/version/massdataset?color=green)](https://cran.r-project.org/package=massdataset)
[![](https://img.shields.io/github/languages/code-size/tidymass/massdataset.svg)](https://github.com/tidymass/massdataset)
[![Dependencies](https://tinyverse.netlify.com/badge/massdataset)](https://cran.r-project.org/package=massdataset)
[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

`massdataset` is a part of [tidymass](https://www.tidymass.org/).

------

# About

`massdataset` provide the `mass_dataset` class which is specifically developed and designed to organize the rectangular **metabolomics data sets** into a standard structure. `massdataset` package also provide a lot of base processing functions to process and operate the `mass_dataset` class. In additional, the `mass_dataset` class can be processed by all the packages from [`tidymass`](https://tidymass.github.io/tidymass/).

<img src="man/figures/Figure.png" align="middle" alt="" width = "100%"/>

Merging separate data objects is useful for manually-imported data objects, especially for metabolomics data analysis. It's significant to keep unified format before analysis. While the first category of merging functions is useful for direct manipulations of the data for analytical purposes, `massdataset` is a convenience/support tool to help get your data into the right format.

# Installation

You can install `massdataset` from
[Github](https://github.com/tidymass/massdataset).

``` r
if(!require(devtools)){
install.packages("devtools")
}
devtools::install_github("tidymass/massdataset")
```

More installation information can be found [here](https://tidymass.github.io/massdataset/articles/massdataset_install.html).

# Get started

Please see the "Help documents".

# Need help?

If you have any questions about `massdataset`, please don’t hesitate to
email me (<shenxt@stanford.edu>) or reach out me via the social medias below.

<i class="fa fa-weixin"></i>
[shenxt1990](https://www.shenxt.info/files/wechat_QR.jpg)

<i class="fa fa-envelope"></i> <shenxt@stanford.edu>

<i class="fa fa-twitter"></i>
[Twitter](https://twitter.com/JasperShen1990)

<i class="fa fa-map-marker-alt"></i> [M339, Alway Buidling, Cooper Lane,
Palo Alto, CA
94304](https://www.google.com/maps/place/Alway+Building/@37.4322345,-122.1770883,17z/data=!3m1!4b1!4m5!3m4!1s0x808fa4d335c3be37:0x9057931f3b312c29!8m2!3d37.4322345!4d-122.1748996)

# Citation

If you use `masscleaner` in you publication, please cite this publication:

TidyMass: An Object-oriented Reproducible Analysis Framework for LC-MS Data.

Xiaotao Shen, Hong Yan, Chuchu Wang, Peng Gao, Caroline H. Johnson, Michael P. Snyder.

[Web Link](https://www.biorxiv.org/content/10.1101/2022.03.15.484499v1).

Thanks very much!
