
<!-- README.md is generated from README.Rmd. Please edit that file -->
shmodules
=========

This package is a collection of functions that act as "building blocks" for shiny modules. The package is primarily for internal use.

### Installation

``` r
devtools::install_github("tiernanmartin/shmodules")
```

### Quick demo

Using `fluidDashboardBody` in place of `shinydashboard::dashboardBody` will add a javascript "trigger" to the HTML output. While this is not a module, it is often desireable for the body contents to adapt their sizing when the sidebar panel is collapsed.

The HTML output looks like this:

``` html
<div class="content-wrapper">
  <section class="content">
    <script>$(".sidebar-toggle").on("click", function() { $(this).trigger("shown"); });</script>
  </section>
</div>
```
