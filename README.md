
# colonADR

<!-- badges: start -->
<!-- badges: end -->

This package calculate APC (adenoma per colonoscopy), ADR (adenoma detection rate), polyp per colonoscopy (PPC), polyp detection rate (PDR), advanced colorectal neoplasm detection rate (ACN DR), and advanced colorectal neoplasm per colonoscopy (ACN per colonoscopy) from JED-style database. You can find test dataset in "tests/testthat/sample_CF_data.csv".


## Installation

You can install the development version of apcadrJed like so:

``` r
remotes::install_github("domperor/colonADR")
library(colonADR)
```

## Example

This is a basic example:

``` r
library(apcadrJed)
data<-(c("腺腫|腺腫|憩室", "憩室|痔核", "特記所見なし", "家族性大腸腺腫症", "進行大腸癌|ポリープ", "polyp|adenoma"))
x<-apcadr(data)

# prints "APC:0.6, ADR:0.4, PPC:1, PDR:0.66667, ACN DR:0.2, ACN per colonoscopy:0.2"
# Then you can access each value with x$APC, x$ADR, x$PPC, x$PDR, x$ACNDR and x$ACNPC.
```

## History

2025.12.18 ver 0.0.1
