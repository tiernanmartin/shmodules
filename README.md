
<!-- README.md is generated from README.Rmd. Please edit that file -->
shmodules
=========

This package is a collection of functions that act as "building blocks" for shiny modules. The package is primarily for internal use.

### Installation

``` r
devtools::install_github("tiernanmartin/shmodules")
```

### Quick demos

While this package is made up of many functions, the intent is for them to be used in concert to create [shiny modules](http://shiny.rstudio.com/articles/modules.html), which are added to apps.

Examples of these modules can be accessed with the `shmodules::launchExample()` function, as demonstrated below:

``` r
#Run this code in the console
shmodules::launchExample(name = 'linkedScatter')
```
