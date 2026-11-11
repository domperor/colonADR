# Extracted from test-colonADR_main.R:5

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "colonADR", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
data<-read.csv("sample_CF_data.csv")
x<-apcadr(data$統計_診断)
expect_equal(as.numeric(format(x$APC, digits = 5)), 0.59596)
expect_equal(as.numeric(format(x$ADR, digits = 5)), 0.39394)
