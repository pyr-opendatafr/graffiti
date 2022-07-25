graffiti R package
================

## Overview

*Experimental*

This package contains an R-Shiny Dashboard Displaying Data From INSEE BDM database.
This dashboard contains a library of pre-made plots and a plot generator.
Graffiti app is a contribution to reproducible research and public data transparency.
Under the hood, graffiti uses [insee](https://github.com/pyr-opendatafr/R-Insee-Data) R package.

Have a look at the app : https://graffiti.lab.sspcloud.fr/

## Installation, Loading an Use

``` r
# Get the development version from GitHub
# install.packages("devtools")
# devtools::install_github("pyr-opendatafr/R-Insee-Data")
# devtools::install_github("pyr-opendatafr/graffiti")

library(graffiti)

# Use
graffiti()

```

## Dashboard

![](inst/assets/demo.png)


## Contributing

All contributions are welcome !!

## Deployment environment

The graffiti app is built with a dedicated docker image available in a separate [repo](https://github.com/pyr-opendatafr/graffiti-env).
Then, it is deployed thanks to [Onyxia datascience platform](https://onyxia.lab.sspcloud.fr/home)

## Support

Feel free to open an issue with any question about this package using <https://github.com/pyr-opendatafr/graffiti/issues> Github repository.
