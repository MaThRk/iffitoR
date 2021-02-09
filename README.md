# A little collection of funtions to get data from the tables exported from the iffi-database

The package (without the vignettes) can be installed with:

`remotes::install_github("https://github.com/RobinKohrs/iffitoR")`

There is some problem with building the vignettes locally with `devtools`. A solution should be

1. Cloning this repo
  `git clone "https://github.com/RobinKohrs/iffitoR"`

2. Build the package locally
  `R CMD build iffitoR`
  
3. Install it
  `R CMD INSTALL iffitoR`

When working under linux one will not be able to use the `RODBC`-functions. There is not solution at the moment is to use the data that comes with the package and to not build the vignettes.

## A simple vignette with some descriptions

[Vignettes](https://robinkohrs.github.io/iffitoR/docs/index.html)

***

![](man/figures/anim.gif)
