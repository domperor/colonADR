
# colonADR

<!-- badges: start -->
<!-- badges: end -->

This package calculate APC (adenoma per colonoscopy), ADR (adenoma detection rate), polyp per colonoscopy (PPC) and polyp detection rate (PDR) from JED-style database. You can find test dataset in "/tests/testthat/sample_CF_data.csv". **For Shiny app, see ["/inst/shiny"](./inst/shiny).**


## Installation

You can install the development version of colonADR like so:

``` r
remotes::install_github("domperor/colonADR")
library(colonADR)
```

## Example

This is a basic example:

``` r
library(colonADR)
data<-(c("腺腫|腺腫疑い|憩室", "憩室|痔核", "特記所見なし", "家族性大腸腺腫症", "進行大腸癌|ポリープ", "polyp|adenoma"))
x<-apcadr(data)

# prints "APCstrict:0.4, APCloose:0.6, ADRstrict:0.4, ADRloose:0.4, PPCstrict:0.8, PPCloose:1, PDRstrict:0.6, PDRloose:0.6"
# Then you can access each value with x$APCstrict, x$APCloose (or x$APC), x$ADRstrict, x$ADRloose (or x$ADR), x$PPCstrict, x$PPCloose (or x$PPC), x$PDRstrict and x$PDRloose (or x$PDR).
```

## History

2025.12.18 ver 0.0.1

2026.06.11 ver 0.0.3

2026.10.15 ver 0.0.4 English support

2026.11.11 ver 0.0.5 strict/loose support
